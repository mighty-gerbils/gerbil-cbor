(import
  :std/sugar
  :std/misc/list
  ;:std/misc/vector
  :std/srfi/43
  :std/contract
  :std/io
  :std/misc/alist
  :std/srfi/19
  :std/misc/bytes
  :std/error
  :std/text/utf8
  :std/srfi/1
  "util")

(export (prefix-out encoder cbor-) current-hook object->cbor)

(def (object->cbor obj)
  (using (writer (open-buffered-writer #f) :- BufferedWriter)
    (encoder writer obj)
    (get-buffer-output-u8vector writer)))

(defrule (match-encoder writer item (predicate encode) ... rest)
  (match item
    ((? predicate) (encode writer item)) ... rest))

(def current-hook #f)

(def (encoder buf item)
  (using (buf : BufferedWriter)
    (match-encoder buf item
      (number? write-number)
      (boolean? write-bool)
      (void? write-null)
      ((cut eq? <> 'undefined) write-undefined)
      (hash-table? write-hashmap)
      (u8vector? write-u8vector)
      (vector? write-vector)
      (string? write-string)
      (cbor-tag? write-tag)
      ; These all have O(n) complexity
      (alist? write-alist)
      (proper-list? write-list)
      (else ((current-hook) buf item)))))


(def (default-hook writer item)
  (using (writer :- BufferedWriter)
    (match item
           ((? date?)
            (encoder (make-cbor-tag 0 (date->string item))))
           ; TODO: other types
           (else (error "Don't know how to serialize item: " item)))))

(set! current-hook (make-parameter default-hook))

(def MAXu8 255)
(def MAXu16 65535)
(def MAXu32 4294967295)

(def (smallest-int-container num)
  (cond
    ((fx<= num MAXu8)
     1)
    ((fx<= num MAXu16)
     2)
    ((fx<= num MAXu32)
     4)
    (else 8)))

(defrule (match-for-int-size writer major-type item (size type-arg) ...)
         (match (smallest-int-container item)
                (size (fx+
                        (BufferedWriter-write-u8 writer (data-item major-type type-arg))
                        (BufferedWriter-write writer (uint->u8vector item big size)))) ...))

(def (write-positive-uint writer major-type item)
  (using ((writer :- BufferedWriter)
          (item :~ fixnum?))
    (if (fx< item 24)
      (fx+ (writer.write-u8 (data-item major-type item)))
      (match-for-int-size writer major-type item
        (1 24)
        (2 25)
        (4 26)
        (8 27)))))

(def (write-number writer item)
     (using (writer :- BufferedWriter)
            (cond
              ((and (integer? item) (positive? item) (fixnum? item))
               (write-positive-uint writer 0 item))
              ((and (integer? item) (negative? item))
               (write-positive-uint writer 1 (abs (1+ item))))
              ((flonum? item)
               (let (buf (make-u8vector 8 0))
                 (u8vector-double-set! buf 0 item big)
                 ; we do not currently support writing single-precision floats
                 (fx+ (writer.write-u8 (data-item 7 27))
                    (writer.write buf))))
              (else
                (BUG write-number "This function should not be called with non-numbers" item)))))

(def (write-list writer item)
     (using
       ((writer :- BufferedWriter)
        ; this is probably O(n)
        (item :~ ##proper-list?))
       (fx+ (writer.write-u8 (data-item 4 31))
            (fold (lambda (item v)
                    (fx+ (encoder writer item)
                         v)) 0 item)
            ; terminate the indefinite sequence
            (writer.write-u8 (data-item 7 31)))))

(def (write-u8vector writer item)
  (using
    ((writer :- BufferedWriter)
     (item :~ u8vector?))
    (fx+ (write-positive-uint writer 2 (u8vector-length item))
         (writer.write item))))

; Wrap a vector in a cbor tag so we can decode it back as a vector
(def (wrap-vector writer item)
  ; Tag 41 is for homogeneous array types: https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml
  (write-tag-as (make-cbor-tag 41 item) write-vector))

(def (write-vector writer item)
  (using ((writer :- BufferedWriter)
          (item :~ vector?))
    (fx+ (write-positive-uint writer 4 (vector-length item))
         (vector-fold (lambda (_ v element)
                        (fx+ (encoder writer element)
                             v))
                      0 item))))

(def (write-hashmap writer item)
  (using ((writer :- BufferedWriter)
          (item :~ hash-table?))
    (fx+ (write-positive-uint writer 5 (hash-length item))
         (hash-fold
           (lambda (key value v)
             (fx+ (encoder writer key)
                  (encoder writer value)
                  v))
           0
           item))))

(def (write-alist writer item)
  (using ((writer :- BufferedWriter)
          (item :~ alist?))
    (fx+ (fold (lambda (pair v)
                 (fx+ (encoder writer (car pair))
                      (if (list? (cdr pair))
                        (encoder writer (cadr pair))
                        (encoder writer (cdr pair)))
                      v))
               0 item)
         (writer.write-u8 (data-item 7 31)))))

(def (write-string writer item)
  (using ((writer :- BufferedWriter)
          (item :~ string?))
    (fx+ (write-positive-uint writer 3 (string-length item))
         (writer.write (string->utf8 item)))))

(def (write-bool writer item)
  (using ((writer :- BufferedWriter)
          (item :~ boolean?))
    (writer.write-u8 (data-item 7 (if item 21 20)))))

(def (write-void writer item)
  (using ((writer :- BufferedWriter)
          (item :~ void?))
    (writer.write-u8 (data-item 7 23))))

(def (write-null writer item)
  (using ((writer :- BufferedWriter)
          (item :~ void?))
    (writer.write-u8 (data-item 7 22))))

(def (write-undefined writer _)
  (using (writer :- BufferedWriter)
    (writer.write-u8 (data-item 7 23))))

(def (write-tag-as writer item inner-encoder)
  (using (item : cbor-tag)
    (fx+ (write-positive-uint writer 6 item.tag)
         (inner-encoder writer item.value))))

(def (write-tag writer item)
  (write-tag-as writer item encoder))
