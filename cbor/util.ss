; This module contains utilities for encoding/decoding CBOR messages
(export data-item data-item/const (struct-out cbor-tag))
(module I
  (export #t)
  (def (data-item major-type arg)
    (let* ((out (fx+ (fxarithmetic-shift-left major-type 5) arg))
           (bitlength (##fxlength out)))
      (if (fx> bitlength 8)
        (error "major type or arg too large. Bit count: " bitlength)
        out))))
(import I (for-syntax I))
; Constant/expand-time calculation of a data-item tag
(defsyntax (data-item/const stx)
  (syntax-case stx ()
    ((_ major-type arg)
     (and (stx-fixnum? #'major-type) (stx-fixnum? #'arg))
     (with-syntax* (((values major-tag)
                     (data-item (syntax->datum #'major-type)
                                (syntax->datum #'arg))))
                   major-tag))))

(defstruct cbor-tag (tag value)
  final: #t)
