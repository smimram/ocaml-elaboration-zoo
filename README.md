# The elaboration zoo, in OCaml

This is mostly a translation of András Kovács' [elaboration zoo](https://github.com/AndrasKovacs/elaboration-zoo), that I made for learning elaboration in dependent type theory.

In order to be efficient one should

- perform normalization by evaluation (in order not to have to deal with substitution and α-conversion)
- use de Bruijn indices (using variable names is too slow in practice)

This is [Coquand algorithm](https://www.sciencedirect.com/science/article/pii/0167642395000216).
