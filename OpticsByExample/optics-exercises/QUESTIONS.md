# Questions

## HIE problems

- Can't find symbols in different files

Things that do work
- Can find symbols in the same file (eg. F12)
- Can see intellisense
- In intellisense, can click link into (local?) docs(!)


## You can make a "liar lens" - what's the point of that?

    *Main Lib Control.Lens> print purplePearl
    Ship {_name = "Purple Pearl", _numCrew = 74}
    *Main Lib Control.Lens> x = lens (\s -> "foo") (\ship newName -> ship { _name = newName })
    *Main Lib Control.Lens> view x pur
    pure         purplePearl
    *Main Lib Control.Lens> view x purplePearl
    "foo"

## Given a path into a structure is it possible to *not* find a focus?
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

    If you get the types for `(,)` and `(,,)` you can see they are different in that 'Control.Lens.Tuple' defines `Field1` through `Field3` for `(,,)` while `(,)` gets only the first two. I think `Control.Lens.Tuple.N3` is private because I can't get info about it and I doubt the type would mean much to me anyway.

  * Counter-point: Check this out!

        Prelude Control.Lens> set (_Left) "new" (Left "old")
        Left "new"
        Prelude Control.Lens> set (_Left) "new" (Right "old")
        Right "old"

    Meaning given a path into a structure, the path may not apply to the structure meaning we do not apply the action:

        Prelude Control.Lens> view (_Left) (Left "foo")
        "foo"
        Prelude Control.Lens> view (_Right) (Left "foo")
        ()

    Or we do, but we return an action-defined return value, like what *is* there or unit.