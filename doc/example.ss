(import
  :std/contract
  :std/io
  :std/error
  "../gerbil-cbor/lib")

(defstruct point (x y) final: #t equal: #t)
; As of this writing, this is an unassigned tag in the
; [IANA CBOR Tag Registry](https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml)
(def POINT 60003)
(def tag (make-cbor-tag POINT 123))

(def mypoint (make-point 123 45.6))
(def writer (open-buffered-writer #f))
(def (tag-hook writer item)
  ; the writer is *always* a `BufferedWriter`, otherwise the `encoder` application below
  ; will fail before we get to this point.
  (using (writer :- BufferedWriter)
    (match item
      ((? point?)
       (using ((item :- point)
               (tag (make-cbor-tag POINT [item.x item.y]) :- cbor-tag))
         ; encode point as a list of it's x, y values
         (encoder writer tag)))
      (else
        (error "Don't know how to encode item" item)))))
; use the current-hook parameter to encode our `point`.
(parameterize ((current-hook tag-hook))
  (encoder writer mypoint))

(def buffer (open-buffered-reader (get-buffer-output-u8vector writer)))
; Decoder example
(def (tag-handler item)
  (using (item :- cbor-tag)
    (match item.tag
      (POINT
        (make-point (car item.value) (cadr item.value)))
      (else
        (error "Do not know how to decode item." item)))))

(def newpoint
     (parameterize ((current-tag-handler tag-handler))
       (decoder buffer)))
(using ((newpoint : point)
        (mypoint : point))
       (displayln "Equal? " (equal? mypoint newpoint)))


