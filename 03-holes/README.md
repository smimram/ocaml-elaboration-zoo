# Typechecking a dependent type theory with holes

The holes are replaced by so-called "metavariables" which are then "guessed" (or
_elaborated_), by unification, to actual terms. Since metavariables can depend
on the context and we don't want to carry that around, we simply make them
closed functions, which are formally applied to the arguments they can depend
upon. They can then be stored in a dedicated environment, isolated from the
usual one. For simplicity, we only allow dependencies on λ-abstracted variables,
but not definitions. Kovács example is

```
let id : (A : U) -> A -> A = λ A x. x;
let id2 : (A : U) -> A -> A = λ A x. id _ x;
U
```

and it gets solved in the following steps.

1. We replace the hole by a metavariable `?α` which can depend on the abstracted
   variables `A` and `x`. In order to handle this, we apply the metavariable to
   those variables so that it will correspond to a closed abstraction. This
   means that we replace `_` by `?α A x` where `?α` is a fresh metavariable.
2. During typechecking we generate constraints on metavariables. For instance,
   in the above, we see that `x`, which has type `A` must be of the type given
   by the first argument of the identity, which means that we should have the
   constraint
   
   ```
   ?α A x = A
   ```
   
3. We then solve the constraints we have by _pattern unification_ which is a
   decidable restricted form of higher-order unification (which is undecidable
   in general). Here, we only allow constraints of the form
   
   ```
   ?α x₀ x₁ ... xₙ = u
   ```

   such that the following side conditions are satisfied:
   
   - the `xᵢ` are pairwise distinct variables,
   - the free variables of `u` are among the `xᵢ`,
   - `?α` does not occur in `u` (unless we have `?α = ?α`, which can simply be
     dropped).
   
   When this is the case, we must have
   
   ```
   ?α = λ x₀ ... xₙ . u
   ```
   
   (and of course, me must further check that multiple solutions to the same
   `?α` agree). Here, we immediately replace the metavariable by its solution
   (more precisely, we update a reference in the metavariable, which was
   formerly pointing to nothing [`None`]).

In the last case, there is a subtlety: when abstracting, we must follow the de
Bruijn discipline. Therefore we must α-convert the variables in `u` so that this
is the case. This means that the value for `?α` will not be

```
?α = λ x₀ x₁ ... xₙ . u
```

but rather

```
λ.λ...λ. u[0/xₙ,1/xₙ₋₁,...,n/x₀]
```
