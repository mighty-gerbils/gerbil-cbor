# CBOR Decoder

The `cbor-decoder` for CBOR decoding all primative CBOR types into plain Scheme objects by
default, but allows parameterizing and extending the decoder using the
`current-tag-handler` parameter.


## `cbor-decoder`

```scheme
(decoder buffer) ; => objects
```

The `decoder` form takes in a `BufferedReader` and attempts to read from it, decoding
until either the end of the CBOR item is reached or a malformed message is encountered.

## `cbor->object`

```scheme
(def (cbor->object u8v)) ; => scheme object
```

`cbor-object` is the same as `cbor-decoder` but accepts a `u8vector` instead of a
`BufferedReader` as input.

## `max-indefinite-item`

This parameter controls how many items will be decoded when an indefinite list or map is
decoded. The default is `1024`.

**Note:** This is the number of items, not the *size* of the decoded items.

## `cbor-tag`

```scheme
(defstruct cbor-tag (tag item))
```

`cbor-tag` is a struct used to tie a semantic meaning to an identifier and a
corresponding value. When decoding, the `tag` field may be used to identify a particular
way to interpret the data within the `item` field. The `item` field should always be a
decoded plain Scheme object such as a `list?`, `vector?`, or `number?`.

## `current-tag-handler`

The `current-tag-handler` is a parameter that points to a lambda that accepts a
`cbor-tag` as its only argument and is expected to return an object, type, etc. that
meets the semantics associated with the `cbor-tag.tag`. The default tag handler simply
strips the `cbor-tag` wrapper and returnes `cbor-tag.value` directly with no special
meaning or transformations applied.

### Tips for decoding your custom types

See the [decoder docs](decoder.md) for more information.

In the below example, `buffer` is a `u8vector` taken with
`(open-buffered-reader (get-buffer-output-u8vector writer))`
from the decoder example.

See [example.ss](example.ss) for a complete executable example.

```scheme

(import
  :std/contract
  :std/io
  :std/error
  "gerbil-cbor/lib")

(defstruct point (x y) final: #t)
; As of this writing, this is an unassigned tag in the
; [IANA CBOR Tag Registry](https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml)
(def POINT 60003)

(def (tag-handler item)
  (using (item :- cbor-tag)
    (match item.tag
      (POINT
        (make-point (car item.value) (cadr item.value)))
      (else
        (error "Do not know how to decode item." item)))))

(def newpoint
     (parameterize ((current-tag-handler tag-handler))
       (cbor-decoder buffer)))
(using ((newpoint : point)
        (mypoint : point))
       (displayln "Equal? " (and
                              (equal? mypoint.x newpoint.x)
                              (equal? mypoint.y newpoint.y))))
```

The above example shows how you can use tags to identify a particular custom type and
decode and transform a raw Scheme value into your type.

It's also worth noting that this style of types is *completely optional*, you may use
plain scheme objects if you do not wish to use stronger typing in your communication.
