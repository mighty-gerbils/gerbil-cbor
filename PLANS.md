# Planned Features

- Custom type tags for Lisp-ey goodness
	- I plan to submit them to the IANA registry
- CBOR Sequences ([RFC 8742](https://www.rfc-editor.org/rfc/rfc8742.html)) with
  streaming via coroutines/generators
- Vector/typed-array handling ([RFC 8746](https://www.rfc-editor.org/rfc/rfc8746.html))
- Tags for Date and Time ([RFC 8943](https://www.rfc-editor.org/rfc/rfc8943.html))
- CDDL parser-generator ([RFC 8610](https://datatracker.ietf.org/doc/html/rfc8610))
	- This may be implemented in Gerbil or in another language. At the very least I plan
	 to implement a generator backend for Gerbil structures

It is unlikely that I will implement [RFC 8152](https://tools.ietf.org/html/rfc8152) or
[RFC 8392](https://tools.ietf.org/html/rfc8392), though contributions will be welcome.
