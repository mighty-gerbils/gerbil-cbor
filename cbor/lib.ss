(import
  "util"
  "encoder"
  "decoder")

(export
  ; encoder
  cbor-encoder current-hook object->cbor
  ; decoder
  cbor-decoder current-tag-handler max-indefinite-item cbor->object
  ; util
  (struct-out cbor-tag))
