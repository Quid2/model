
Derivation of data type models from Haskell data types.

### Installation

It is not yet on [hackage](https://hackage.haskell.org/) but you can still use it in your [stack](https://docs.haskellstack.org/en/stable/README/) projects by adding a reference to its github location under the 'packages' section:

````
packages:
- location:
    git: https://github.com/tittoassini/model
    commit: 0e1fe4f
````


### Compatibility

Tested with [ghc](https://www.haskell.org/ghc/) 7.10.3 and 8.0.1.

### Known Bugs and Infelicities

* Works incorrectly with data types with more than 5 type variables.
