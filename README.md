# Geometric Algebra Embedded Domain Specific Language

This is a simple domain specific language, embedded within Haskell,
for doing computations within 3D vectorspace geometric algebra.

The language supports the following operations / constants:
- Basis Vectors, Bivectors, and Trivectors:
  - `e1`, `e2`, `e3`
  - `e12` = `-e21`, `e23` = `-e32`, `e31` = `-e13`
  - `e123` = `i`
- Binary Operations:
  - Geometric product `*`
  - Inner (dot) product `dot`
  - Outer (wedge) product `wedge` = `/\`
- Multivector Functions:
  - `exp`, the taylor series for e^x.
  - `norm` for normalizing vectors.
  - `bnorm` for normalizing bivectors.
  - `brev` for reversing bivectors.
  - `binv` for inverting bivectors.
- Rotation helper functions:
  - `rotor <plane> <angle>` for generating a rotor.
  - `rotate <plane> <angle> <vector>` for rotating a vector within a plane.
- Evaluation:
  - `varName = var "varName"` declaring variables to be used in expressions.
  - `eval [("varName", value),...] $ <expr>` providing variables with values then simplifying.
- Plotting
  - `t`, a variable provided that is used in parametric plots.
  - `plot [("name",val),...] $ <expr>`, like eval, but assumes you are using `t` in your expression, and creates a parametric plot from t = 0 to t = 1.
- Constants
  - `pi = 0.5*tau`
  - `tau = 2*pi`

Note that some functions, such as the dot and wege products only
expect vectors as inputs, and will misbehave if they are given
inputs of different grades. This is something that could be
improved upon in the future, but for now, keep it in mind.

The plotting functionality assumes you have gnuplot installed.

## Examples

Run `cabal repl` to get started.

Writing the vector [1,7,5]:

```haskell
e1+7*e2+5*e3
```

Storing multivectors, then taking the dot product:

```haskell
a = e1+7*e2+5*e3
b = 2*e1+3*e2
dot a b
```

And again, with the wedge product:

```haskell
a = e1+7*e2+5*e3
b = 2*e1+3*e2
a /\ b
```

Multiply 3 things:
```haskell
(2*e1+0.4*e2+0.1*e3)*(5.7+e123)*(e12+0.3*e3)
```

Demonstrate that the wedge is anticommutative on the basis vectors:
```haskell
e1 /\ e2 + e2 /\ e1
```

Variables:
```haskell
x = var "x"
y = var "y"

foo = (x*e1+y*e2)*(e123 + (y-x)*e3)

eval [("x", 5),("y", e3)] foo
```

Plot a rotation using rotors in gnuplot:
```haskell
b = bnorm ((e1+e2) /\ (e2+e3))
plot [] $ rotate b (t*tau) e1
```
Here we are using the wedge to create a bivector with
a certain orientation, then normalizing it so we can
use the `rotate` function. The input bivector must
be normalized so that we can create a rotor with it.
See the references at the top of GA.hs for more details.

If the plot doesn't show up at first, give it some time.
The plot often has to process in the background even
after the command has finished running.

## Code
All of the code for this project is in `src/GA.hs`.
I have put header comments to separate it into sections.
The top half of the file includes the AST and
will help a user of the EDSL understand better how to use it.
The bottom half has more trick and tedious stuff, like
normalizing the expressions, and pretty-printing.

## Effort

The thing that took the most effor for me was figuring out
how to represent a single multivector.
I started by trying to store a multivector as a record
with exactly 8 double values, but that quickly became
annoying to work with and didn't represent what
the user typed in very well.

Eventually, I landed on the system I have now, which
is more like a deep EDSL, where the AST matches
very closely what the user inputs, and it is converted
to a secondary normalized form when simplification is needed.

An interesting trick is that I do this conversion to a secondary
normalized form in the implementation of Show for
the primary AST, then pretty-print the normalized form.
This would not typically be the best thing to do code wise,
but it allows me to do additional work and cleanup on the AST
without requiring the user to call some additional "simplify"
function.

Once I had the data structures in place, things got a lot easier.
The tricky part after that was normalization. This seemed
very intimidating at first, but was not an issue after
I realized it needed to happen in separate basis, scalar, and
like-term stages.
