[![Build Status](https://travis-ci.org/tittoassini/model.svg?branch=master)](https://travis-ci.org/tittoassini/model)

With `model` you can easily derive models of Haskell data types.

Let's see some code.

We need a couple of GHC extensions:

> {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

Import the library:

> import Data.Model

To derive a model of a data type we need to make it an instance of the `Generic` and `Model` classes.

For data types without parameters, we can do it directly in the `deriving` clause of the definition:

> data Direction = North | South | Center | East | West deriving (Show,Generic,Model)

For data types with parameters we currently need a separate instance declaration for `Model`:

> data Couple a b = Couple a b deriving (Show,Generic)

> instance (Model a,Model b) => Model (Couple a b)

Instances for a few common types (Bool,Maybe,Either..) are already predefined.

We use `typeModel` to get the model for the given type plus its full environment, that's to say the models of all the data types referred to, directly or indirectly by the data type.

We pass the type using a Proxy.

> e1 = typeModel (Proxy:: Proxy (Couple Direction Bool))

That's a lot of information, let's show it in a prettier and more compact way:

> e2 = pPrint $ typeModel (Proxy:: Proxy (Couple Direction Bool))

Data types with symbolic names are also supported:

> instance (Model a) => Model [a]

> e3 = pPrint $ typeModel (Proxy:: Proxy [Bool])

 ### Installation

Get the latest stable version from [hackage](https://hackage.haskell.org/package/model).

<!--
It is not yet on [hackage](https://hackage.haskell.org/) but you can use it in your [stack](https://docs.haskellstack.org/en/stable/README/) projects by adding in the `stack.yaml` file, under the `packages` section:

````
- location:
   git: https://github.com/tittoassini/model
   commit: b05a56a993213271e3b13d28a5e8bb90c9d8576f
  extra-dep: true
````
-->

 ### Compatibility

Tested with [ghc](https://www.haskell.org/ghc/) 7.10.3 and 8.0.2.

 ### Known Bugs and Infelicities

* No support for variables of higher kind.

  For example, we cannot define a `Model` instance for `Higher`:

  `data Higher f a = Higher (f a) deriving Generic`

  as `f` has kind `*->*`:

* Parametric data types cannot derive `Model` in the `deriving` clause and need to define an instance separately

  For example:

  `data Couple a b = Couple a b Bool deriving (Generic,Model)`

  won't work, we need a separate instance:

  `instance (Model a,Model b) => Model (Couple a b)`

* Works incorrectly with data types with more than 9 type variables.
