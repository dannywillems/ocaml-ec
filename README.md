# Mec - Mini Elliptic Curve library

**DO NOT USE IN PRODUCTION**


**Mec** provides elliptic curve and finite field based cryptographic primitives and protocols.

The library characteristics are:
- written purely in OCaml. No C or assembly code
- implementations are modular, focusing on parameters readability for
  the different primitive instantiations
- can be used to generate test vectors
- can be used to create new primitive instantiations
- can be compiled in JavaScript to be run in web-browsers, if the performance is not important
- provides tools to generate secure parameters, like for Poseidon hash function
- can be used to prototype primitives or protocols

Do not use this library if:
- performance is important. Mec is not meant to be efficient, and maybe will never be.
- you want to compare the performance between different cryptographic
  primitives. As performance is not important in the implementation, it won't make
  sense to compare.

Here some suggestions of libraries you can use to get better performances:
- [ocaml-bls12-381](https://gitlab.com/dannywillems/ocaml-bls12-381) provides the instance BLS12-381 and also Poseidon128 as implemented here
- [hacl-star](https://github.com/project-everest/hacl-star/) provides different elliptic curves.


## Structure

- curve (`Mec.Curve`) focuses on implementing elliptic curves like Pallas,
  Vesta, Secp256k1, Secp256r1, Ed26619, etc. Computations can happen in
  different types of coordinates: affine, projective, jacobian, etc and
  conversions are provided.

- hash (`Mec.Hash`) focuses on implementing EC or field based hash functions
  like Poseidon, Sinsemilla, Pedersen. We tend to add instances found in
  projects like Dusk, Zcash and others.
  
- signature (`Mec.Signature`) focuses on implementing EC or field signature schemes.

- protocols (`Mec.Protocols`) implements different protocols using the
  cryptographic primitives. At the moment, this section is poor.


## Install and use

There is no plan to release a version of Mec right now as it is not meant to be
used in production. There will maybe be releases from time to time. If not, it
is recommended to pin to a certain commit, or master.

```shell
opam pin add mec.dev git+https://gitlab.com/dannywillems/ocaml-ec\#master --no-action
opam install mec.dev
```

To use in your project, simply add `mec` in your dependencies.
