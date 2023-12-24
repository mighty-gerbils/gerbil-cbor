(import :std/test
        :std/io
        :std/format
        :std/text/utf8
        :std/contract
        "decoder"
        "util")
(export cbor-decoder-test)


(def cbor-decoder-test
     (test-suite "cbor/decoder"
                 (test-case "decode simple int"
                            (def testsmallint (open-buffered-reader #u8(16)))
                            (check (cbor-decoder testsmallint) => 16))
                 (test-case "decode u8"
                            (check (cbor-decoder (open-buffered-reader #u8(24 32))) => 32))
                 (test-case "decode u16"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#x19 #x75 #x30)))
                                   => 30000))
                 (test-case "decode u32"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#x1a #x0 #xc #x35 #x0)))
                                   => 800000))
                 (test-case "decode u64"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#x1b #xff #xff #xff #xff #xff #xff #xff #xff)))
                                   => 18446744073709551615))
                 (test-case "decode negative simple"
                            (check (cbor-decoder (open-buffered-reader #u8(#x21))) => -2))
                 (test-case "decode negative u8"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#x38 #x1f))) => -32))
                 (test-case "decode negative u16"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#x39 #x75 #x2f))) => -30000))
                 (test-case "decode negative u32"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#x3a #x00 #x0c #x34 #xff))) => -800000))
                 (test-case "decode negative u64"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#x3b #x63 #x5 #x1f #xf8 #x62 #x1c #x5e #x64))) => -7135144336296795749))
                 (test-case "decode simple list"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#x83 #x1 #x2 #x3))) => [1 2 3]))
                 (test-case "decode recursive list"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#x83 #x1 #x82 #x2 #x3 #x4)))
                                   => [1 [2 3] 4]))
                 (test-case "decode text"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#x6c #x48 #x65 #x6c #x6c
                                                               #x6f #x20 #x77 #x6f #x72 #x6c #x64 #x21)))
                                   => "Hello world!"))
                 (test-case "decode bytes"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#x43 #x30 #x31 #x32)))
                                   => #u8(#x30 #x31 #x32)))
                 (test-case "decode map"
                            (let* ((output (cbor-decoder
                                             (open-buffered-reader #u8(#xa3 #x64 #x6b #x65 #x79
                                                                       #x31 #x66 #x76 #x61 #x6c
                                                                       #x75 #x65 #x31 #x02 #x03
                                                                       #x83 #x01 #x02 #x03 #xa1
                                                                       #x64 #x6b #x65 #x79 #x32
                                                                       #x04))))
                                   (aslist (hash->list output)))
                              (check (assq 2 aslist) => [2 . 3])
                              (check (assoc "key1" aslist) => ["key1" . "value1"])
                              ; we could check more, but I'm lazy
                              (check (cdr (assoc [1 2 3] (hash->list output))) ? hash-table?)))
                 (test-case "decode double/fl64"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#xfb #x40 #xa #x66 #x66 #x66 #x66 #x66 #x66)))
                                   => 3.3))
                 (test-case "decode tagged item default"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#xd8 #x7b #x19 #x1 #xc8)))
                                   => 456))
                 (test-case "decode tagged item custom"
                            (parameterize ((current-tag-handler (lambda (item)
                                                                  (using (item :- cbor-tag)
                                                                         [item.tag . item.value]))))
                              (check (cbor-decoder
                                       (open-buffered-reader #u8(#xd8 #x7b #x19 #x1
                                                                 #xc8))) => [123 . 456])))
                 (test-case "decode simple boolean true"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#xf5))) => #t))
                 (test-case "decode simple boolean false"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#xf4))) => #f))
                 (test-case "decode simple void"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#xf6))) ? void?))
                 (test-case "decode simple undefined"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#xf7))) => 'undefined))
                 (test-case "decode indefinite length byte string"
                            (check (cbor-decoder
                                     (open-buffered-reader #u8(#x5f
                                                               #x44 #xaa #xbb #xcc #xdd
                                                               #x43 #xee #xff #x99
                                                               #xff))) => #u8(#xaa #xbb #xcc #xdd #xee #xff #x99)))))
