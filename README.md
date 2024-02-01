# Gerbil CBOR

Gerbil Scheme implementation of CBOR/[RFC
8949](https://www.rfc-editor.org/rfc/rfc8949.html)

## Features

- De(serialize) all native CBOR types
- Custom encoder/decoder callbacks

## I just want to see the function signatures

```scheme
(encoder buffered-writer obj) ; => #!void
(decoder buffered-reader) ; => obj
(object->cbor obj) ; => u8vector
(cbor->object u8v) ; => object
```

See [example](./doc/example.ss) for a more complete example of usage

## The rest of the docs

[encoder](./doc/encoder.md)

[decoder](./doc/decoder.md)
