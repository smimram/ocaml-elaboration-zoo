# The elaboration zoo, in OCaml

This is mostly a translation of [András Kovács](https://andraskovacs.github.io/)' [elaboration zoo](https://github.com/AndrasKovacs/elaboration-zoo), that I made for learning elaboration in dependent type theory.

The morale is that, in order to be efficient (both in terms of development speed and execution), one should

- perform normalization by evaluation (in order not to have to deal with substitution and α-conversion),
- use de Bruijn indices (using variable names is too slow in practice).

The resulting algorithm is known as [Coquand algorithm](https://www.sciencedirect.com/science/article/pii/0167642395000216).

Following Kovács, we provide various implementations, which are more and more elaborate and realistic.

1. _Evaluation of λ-calculus_ using [normalization by evaluation](https://en.wikipedia.org/wiki/Normalisation_by_evaluation) (NBE).

   - [01-eval-HOAS-names](01-eval-HOAS-names). We keep variable names and abstractions are evaluated to OCaml abstractions, i.e. values are
   
     ```ocaml
     type t =
       | Var of string
       | App of t * t
       | Abs of string * (t -> t)
     ```
      
     If evaluating λ-terms is all you wanted, and you wanted to do it quickly, this is the way to proceed. There are two limitations of this implementation however.
     
     1. Closures are taken care of by OCaml, which means that we do not have any control over those. In order to implement metavariables, we will need that, which is easily taken care of (see next point).
     2. We use variable names all the way. This means that in order to look for the value of a variable in an environment we need to look for each element and compare a string until we find the right one, and this is quite inefficient. All we needed is to store at which position in the environment we can find the value, which is the idea of de Buijn indices.
     
   - [01-eval-closures-names](01-eval-closures-names). Abstractions are evaluated to formal abstractions with closures where variables are named with strings, i.e. values are
   
     ```ocaml
     type t =
       | Var of string
       | App of t * t
       | Abs of environment * string * Term.t

     and environment = (string * t) list
     ```
   
     This is the most direct way to proceed (we use NBE so that α-conversion is handled by OCaml and we keep variable names so that we can easily debug our implementation), but it is inefficient because we have to compare strings in order to look for the value of a variable in an environment.
    - [01-eval-closures-debruijn](01-eval-closures-debruijn). Same as above, but before evaluating λ-terms, we convert all variables to de Bruijn indices which makes evaluation much more efficient.

2. _Typechecking of a dependently-typed λ-calculus_. We add types to our language. In particular, functions are typed with Π-types.

   - [02-typecheck-closures-debruijn](02-typecheck-closures-debruijn)

3. _Metavariables_. Often, we would like to have the typechecker come up with some values automatically for us. In this case, we put a metavariable and hope that it will be filled later on. For instance, given the identity

    ```
    id : (A : Type) → A → A
    ```
    
    we want to be able to write
    
    ```
    id _ 3
    ```
    
    and have the typechecker guess that `_` (the metavariable) has to be `ℕ`.

    - [03-holes](03-holes)
