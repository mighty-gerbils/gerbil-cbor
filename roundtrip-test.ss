(import
  :std/contract
  :std/io
  :std/test
  :std/sugar
  "lib")
(export #t)


(def (simple-encode-decode arg)
     (using (writer (open-buffered-writer #f) :- BufferedWriter)
       (let* ((written (cbor-encoder writer arg))
              (buffer (get-buffer-output-u8vector writer))
              (reader (open-buffered-reader buffer)))
         ; check that the length of the 
         (check (u8vector-length buffer) => written)
         (cbor-decoder reader))))

(defrules roundtrip-check ()
  ((_ arg pred)
   (check (simple-encode-decode arg) ? pred))
  ((_ arg)
   (check (simple-encode-decode arg) => arg)))

(defstruct mycustomstruct (myfield otherfield)
  final: #t)

(def (custom-hook writer item)
     (if (mycustomstruct? item)
       (using (item : mycustomstruct)
              ; wrap the item's fields in a cbor tag and encode it as normal
              (cbor-encoder writer (make-cbor-tag 555555 [item.myfield item.otherfield])))
       (error "Unknown type to encode" item)))

(def (custom-tag-handler item)
  (using (item : cbor-tag)
    (match item
      ((cbor-tag 555555 i) (make-mycustomstruct (car i) (cadr i))))))

(def cbor-roundtrip-test
     (test-suite "cbor/roundtrip"
                 (test-case "simple int" (roundtrip-check 3))
                 (test-case "u8 int" (roundtrip-check 123))
                 (test-case "u16 int" (roundtrip-check 50000))
                 (test-case "u32 int" (roundtrip-check 800000))
                 (test-case "u64 int" (roundtrip-check 1844674407370955161))
                 (test-case "simple negative int" (roundtrip-check -3))
                 (test-case "negative u8 int" (roundtrip-check -123))
                 (test-case "negative u16 int" (roundtrip-check -50000))
                 (test-case "negative u32 int" (roundtrip-check -800000))
                 (test-case "negative u64 int" (roundtrip-check -1844674407370955161))
                 (test-case "void" (roundtrip-check (void)  void?))
                 (test-case "undefined" (roundtrip-check 'undefined))
                 (test-case "false" (roundtrip-check #f))
                 (test-case "true" (roundtrip-check #t))
                 (test-case "string" (roundtrip-check "hello world!"))
                 (test-case "list" (roundtrip-check [1 2 3 4]))
                 (test-case "hash-table" (roundtrip-check (list->hash-table [["my" . "ht"]])))
                 (test-case "vector" (check (simple-encode-decode (vector 1 2 3)) => [1 2 3]))
                 (test-case "alist" (simple-encode-decode [["key" . "value"]]))
                 (test-case "cbor tag with default hook" (check (simple-encode-decode (make-cbor-tag 123 "hello"))
                                                                => "hello"))
                 (test-case "custom type encode hook"
                            (parameterize ((current-hook custom-hook)
                                           (current-tag-handler custom-tag-handler))
                              (simple-encode-decode (make-mycustomstruct 12345 #f))))
                 (test-case "object->cbor => cbor->object"
                            (check (cbor->object (object->cbor [1 2 3])) => [1 2 3]))))
