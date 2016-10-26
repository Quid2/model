Derivation of data type models from Haskell data types.

 ### Installation

It is not yet on [hackage](https://hackage.haskell.org/) but you can use it in your [stack](https://docs.haskellstack.org/en/stable/README/) projects by adding in the `stack.yaml` file, under the `packages` section:

````
- location:
   git: https://github.com/tittoassini/model
   commit: b05a56a993213271e3b13d28a5e8bb90c9d8576f
  extra-dep: true
````

 ### Compatibility

Tested with [ghc](https://www.haskell.org/ghc/) 7.10.3 and 8.0.1.

 ### Known Bugs and Infelicities

* Works incorrectly with data types with more than 5 type variables.
