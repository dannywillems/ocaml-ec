## Poseidon Hash utils

### Generate the number of rounds

```
dune exec ./security_parameters.exe security-level alpha width field-size
```

Based on the attacks given in [this paper](https://eprint.iacr.org/eprint-bin/getfile.pl?entry=2019/458&version=20201216:132935&file=458.pdf).

Some values for 128 bits with x^5 (given as R_F, R_P):


| Field             | Width = 3 | Width = 5 |
|-------------------|-----------|-----------|
| BLS12-381 Fr      | (8, 55)   | (8, 55)   |
| Pallas base field | (8, 55)   | (8, 55)   |
