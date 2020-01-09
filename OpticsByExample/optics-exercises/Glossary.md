# Glossary

## Lenses

### Structure

This is the data we want to apply to the lense. It could be a record or tree etc with well-typed sections. These well-typed sections become the path pieces we use to "read into" the structure.

### Path

A path is a description of how to "read into" a structure. That description
could be simply by index eg. "get the first thing" or "get the second thing" or it could be a series of types such that we get the Address, then the Street in sequence.

### Focus

The focus is the result of applying a Path to a Structure.

Question: Given a path into a structure is it possible to *not* find a focus?
* Argument that this is impossible:
  * I think the whole point of this is that the path and structure are related by actions such that one can surely define a path into a structure that doesn't exist but types on the action would prevent this from compiling. For example, `view _8 (1, 2)` should surely not compile right?

        Prelude Control.Lens> view _8 (1, 2)

        <interactive>:34:1: error:
            • Non type-variable argument
                in the constraint: Field8 (a, b) (a, b) t t
              (Use FlexibleContexts to permit this)
            • When checking the inferred type
                it :: forall a b t. (Field8 (a, b) (a, b) t t, Num a, Num b) => t

    I think the type protection in _8 is actually what is helping us, note below where it requires a Control.Lens.Tuple.N7. I two-tuple wouldn't fit that.

        Prelude Control.Lens> :i _8
        class Field8 s t a b | s -> a, t -> b, s b -> t, t a -> s where
          _8 :: Lens s t a b
          default _8 :: (GHC.Generics.Generic s, GHC.Generics.Generic t,
                        Control.Lens.Tuple.GIxed
                          Control.Lens.Tuple.N7
                          (GHC.Generics.Rep s)
                          (GHC.Generics.Rep t)
                          a
                          b) =>
                        Lens s t a b
            -- Defined in ‘Control.Lens.Tuple’

### Action