; This file contains
(import :std/sugar
        :std/contract
        :std/iter
        :std/format
        :std/misc/bytes
        :std/text/utf8
        :std/error
        "util"
        (for-syntax "util")
        :std/io)
(export (prefix-out decoder cbor-) current-tag-handler max-indefinite-item cbor->object)

(def max-indefinite-item (make-parameter 1024))

(def +unmarshal+ (make-vector 256 #f))

(def (cbor->object u8v)
  (using ((u8v :~ u8vector?)
          (reader (open-buffered-reader u8v) :- BufferedReader))
    (decoder reader)))

(def (decoder reader)
     (using (reader : BufferedReader)
            ; read the first item
            (let* ((item (reader.read-u8!))
                   (decode-method (vector-ref +unmarshal+ item)))
              (decode-method item reader))))

; The default tag handler strips the tag from the underlying value and returns it
(def (default-tag-handler item)
  (using (item :- cbor-tag)
    item.value))

(def current-tag-handler (make-parameter default-tag-handler))

(def (extract-raw-arg item buf)
     (using ((item :~ fixnum?)
             (buf :- BufferedReader))
            (fxand 31 item)))


(defrule (register major-type arg method)
         (vector-set! +unmarshal+ (data-item major-type arg) method))

(def (register-range major-type start stop method)
     (using ((major-type :~ fixnum?)
             (start :~ fixnum?)
             (stop :~ fixnum?))
            (for (i (in-range start (fx+ 1 stop)))
                 (register major-type i method))))

(def (malformed-message item _)
  (error "Malformed message with initial byte " item))

(def (read-u8 _ buf)
  (using (buf :- BufferedReader)
    (buf.read-u8!)))
(def (read-u16 _ buf)
     (using (buf :- BufferedReader)
            (buf.read-u16)))
(def (read-u32 _ buf)
     (using (buf :- BufferedReader)
            (buf.read-u32)))
(def (read-u64 _ buf)
     (using (buf :- BufferedReader)
            (buf.read-u64)))

; Reads a list from the buffered and decodes it recursively
(def (read-list item buf f)
  (let f ((count (f item buf))
          (item (decoder buf)))
    (if (fx= 1 count)
      ; properly terminate the list
      (cons item '())
      (cons item (f (1- count)
                    (decoder buf))))))

(def (read-indefinite-list item buf (count 0))
     (when (fx> count (max-indefinite-item))
       (error "Exceeded max indefinite item allocation of " (max-indefinite-item)))
     (let (item (decoder buf))
       (if (eq? item 'BREAK)
         '()
         (cons item (read-indefinite-list item buf (1+ count))))))

; only the value associated with the *last* instance of a key is returned. That is,
; if there are duplicates, we overwrite any existing keys.
(def (read-map item buf f (table (make-hash-table)))
     (let f ((count (1- (f item buf)))
             (key (decoder buf))
             (value (decoder buf)))
       (begin
         (hash-put! table key value)
         (if (positive? count)
           (f (1- count)
              (decoder buf)
              (decoder buf))
           table))))

(def (read-indefinite-map item buf (table (make-hash-table)) (count 0))
     (when (fx> count (max-indefinite-item))
       (error "Exceeded max indefinite item allocation of " (max-indefinite-item)))
     (let (key (decoder buf))
       (if (not (eq? key 'BREAK))
         (begin
           (hash-put! table key (decoder buf))
           (read-indefinite-map item buf table (1+ count)))
         table)))

; TODO: do this without copying the buffer
(def (read-utf8-string item buf f)
  (utf8->string (read-bytes item buf f)))

(def (read-bytes item buf f)
     (using (buf :- BufferedReader)
            (let* ((count (f item buf))
                   (bytebuffer (make-u8vector count))
                   (readcount (Reader-read buf bytebuffer)))
              bytebuffer)))

; TODO: indefinite-length bytes
(def (read-indefinite-bytes item buf)
     (using (buf :- BufferedReader)
            (u8vector-concatenate
              (let f ((item (buf.read-u8!))
                      (count 0))
                (when (fx> count (max-indefinite-item))
                  (error "Exceeded max indefinite item allocation of " (max-indefinite-item)))
                (cond
                  ((fx= item (data-item/const 2 31))
                   (if (fx> count 0)
                     (error "Found indefinite-length byte string while already decoding indefinite-length byte string. Malformed message." item)
                     (f (buf.read-u8!)
                        count)))
                  ((fx= item (data-item/const 7 31))
                   '())
                  ; byte chunk, to be concatenated
                  (((in-range? (data-item/const 2 0) (data-item/const 2 27)) item)
                   (cons ((vector-ref +unmarshal+ item) item buf)
                         (f (buf.read-u8!)
                            (1+ count))))
                  (else (error "Invalid data item while reading indefinite-length
                               byte string" item)))))))

(def (read-indefinite-text item buf)
     (using (buf :- BufferedReader)
            (string-join
              (let f ((item (buf.read-u8!))
                      (count 0))
                (when (fx> count (max-indefinite-item))
                  (error "Exceeded max indefinite item allocation of " (max-indefinite-item)))
                (cond
                  ((fx= item (data-item/const 3 31))
                   (if (fx> count 0)
                     (error "Found indefinite-length text string while already decoding indefinite-length text string. Malformed message." item)
                     (f (buf.read-u8!)
                        count)))
                  ((fx= item (data-item/const 7 31))
                   '())
                  ; byte chunk, to be concatenated
                  (((in-range? (data-item/const 3 0) (data-item/const 3 27)) item)
                   ; TODO: don't rely on the default decoder for this
                   (cons ((vector-ref +unmarshal+ item) item buf)
                         (f (buf.read-u8!)
                            (1+ count))))
                  (else (error "Invalid data item while reading indefinite-length
                               byte string" item)))) " ")))


(def (read-negative item buf f)
  (fx- -1 (f item buf)))

(def (read-f32 item buf)
  (using (buf :- BufferedReader)
         (let* ((bytebuffer (make-u8vector 4))
                (readcount (buf.read bytebuffer)))
           (u8vector-float-ref bytebuffer 0 big))))

(def (read-f64 item buf)
  (using (buf :- BufferedReader)
         (let* ((bytebuffer (make-u8vector 8))
                (readcount (buf.read bytebuffer)))
           (u8vector-double-ref bytebuffer 0 big))))

; Converts a u16 in IEEE 754 FP16 format to a float (single precision)
(def (u16->float half)
     (let* ((exponent (fxand (fxarithmetic-shift-right half 10) #x1f))
            (mantissa (fxand half #x3ff))
            (val (cond
                   ((fxzero? exponent)
                    (fl* mantissa (fx- (fxarithmetic-shift-right 1 20))))
                   ((not (fx= exponent 31))
                    (fl* (fx+ mantissa 1024) (expt 2 (fx- exponent 24))))
                   ((fxzero? mantissa)
                    (+inf.0))
                   (else
                     (-inf.0)))))
       (if (fxbit-set? 15 half)
         (- val)
         (val))))

(def (read-f16 item buf)
     (u16->float (read-u16 item buf)))

(def (read-tag item buf f)
  (let* ((tag-num (f item buf))
         (val (decoder buf)))
    ((current-tag-handler) (make-cbor-tag tag-num val))))

(def (read-simple item buf f)
  (match (f item buf)
    ((? (in-range? 0 19))
     (error "Unassigned simple value in range 0 to 19"))
    (20 #f)
    (21 #t)
    ; null
    (22 (void))
    ; undefined
    (23 'undefined)
    ((? (in-range? 24 31))
     (error "Simple values in range 23 31 are reserved and unimplemented."))
    ((? (in-range? 32 255))
     (error "Simple values in range 32 255 are unassigned."))
    (else (error "wtf!?"))))

(defrule (des f r)
  (lambda (item buf)
    (f item buf r)))

; Set up the jump table for decoding
(begin
  ; positive integers
  (register-range 0 0 23 extract-raw-arg)
  (register 0 24 read-u8)
  (register 0 25 read-u16)
  (register 0 26 read-u32)
  (register 0 27 read-u64)
  (register-range 0 28 31 malformed-message)
  ; negative integers
  (register-range 1 0 23 (lambda (item buf) (fx- -1 (extract-raw-arg item buf))))
  (register 1 24 (des read-negative read-u8))
  (register 1 25 (des read-negative read-u16))
  (register 1 26 (des read-negative read-u32))
  ; fixnums can only occupy up to 62-bits, not the full 64 bit register, so we use the
  ; slower `-`. In general, it should only be slower for values that *must* fit into a
  ; 64-bit register because we pack ints into the smallest container
  (register 1 27 (lambda (item buf) (- -1 (read-u64 item buf))))
  (register-range 1 28 31 malformed-message)
  ; byte strings
  (register-range 2 0 23 (des read-bytes extract-raw-arg))
  (register 2 24 (des read-bytes read-u8))
  (register 2 25 (des read-bytes read-u16))
  (register 2 26 (des read-bytes read-u32))
  (register 2 27 (des read-bytes read-u64))
  ; TODO: this actually should indicate an indefinite length byte string
  (register-range 2 28 31 read-indefinite-bytes)
  ; utf-8 strings
  (register-range 3 0 23 (des read-utf8-string extract-raw-arg))
  (register 3 24 (des read-utf8-string read-u8))
  (register 3 25 (des read-utf8-string read-u16))
  (register 3 26 (des read-utf8-string read-u32))
  (register 3 27 (des read-utf8-string read-u64))
  ; TODO: this actually should indicate an indefinite length text string
  (register-range 3 28 31 read-indefinite-text)
  ; lists
  (register-range 4 0 23 (des read-list extract-raw-arg))
  (register 4 24 (des read-list read-u8))
  (register 4 25 (des read-list read-u16))
  (register 4 26 (des read-list read-u32))
  (register 4 27 (des read-list read-u64))
  (register-range 4 28 31 read-indefinite-list)
  ; map
  (register-range 5 0 23 (des read-map extract-raw-arg))
  (register 5 24 (des read-map read-u8))
  (register 5 25 (des read-map read-u16))
  (register 5 26 (des read-map read-u32))
  (register 5 27 (des read-map read-u64))
  (register-range 5 28 31 read-indefinite-map)
  ; tagged data items
  ; TODO: handle tags
  (register-range 6 0 23 (des read-tag extract-raw-arg))
  (register 6 24 (des read-tag read-u8))
  (register 6 25 (des read-tag read-u16))
  (register 6 26 (des read-tag read-u32))
  (register 6 27 (des read-tag read-u64))
  (register-range 6 28 31 malformed-message)
  ; floating point and others...
  ; TODO: hanlde floats and other data items
  (register-range 7 0 23 (des read-simple extract-raw-arg))
  (register 7 24 (des read-simple read-u8))
  (register 7 25 read-f16)
  (register 7 26 read-f32)
  (register 7 27 read-f64)
  (register-range 7 28 30 malformed-message)
  ; The special end terminator for indefinite-length data types
  (register 7 31 (lambda (_ _) 'BREAK)))


