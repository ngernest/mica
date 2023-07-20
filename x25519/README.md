# Example: X25519 (Work in Progress, for internal use only )

The two subfolders contain two different implementations of **X25519**, the [elliptic-curve Diffie-Hellman](https://en.wikipedia.org/wiki/Elliptic-curve_Diffie–Hellman) (ECDH)
key exchange protocol using [Curve25519](https://en.wikipedia.org/wiki/Curve25519). 

The code in the `rfc7748` subfolder is by [Markus Rudy](https://github.com/burgerdev) 
and is taken from the repo [burgerdev/ocaml-rfc7748](https://github.com/burgerdev/ocaml-rfc7748/tree/master). 
- This implementation is done purely in OCaml. 

The code in the `Callipyge` repo is by [Romain Calascibetta](https://blog.osau.re/index.html) and is taken from the repo [oklm-wsh/Callipyge](https://github.com/oklm-wsh/Callipyge). 
- This implementation uses OCaml's C FFI.

The file `ecdh.ml` contains the module signature for the ECDH protocol. 


TODOs:
- Rewrite the code in the two subfolders as modules implementing the `ECDH` interface
- Note: will need to rewrite the Callipyge code so that it is purely functional 
- Get Mica to parse the two modules
- Figure out how to build this subfolder 
- May need a different Opam switch due to the use of the C FFI 