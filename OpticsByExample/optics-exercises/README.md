# optics-exercises

To get `view (_1 . _2) ((1, 2), 3)` to work in `stack repl` you first need to `import Control.Lens` in the repl.

class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _1 :: Lens s t a b
  default _1 :: (GHC.Generics.Generic s, GHC.Generics.Generic t,
                 Control.Lens.Tuple.GIxed
                   Control.Lens.Tuple.N0
                   (GHC.Generics.Rep s)
                   (GHC.Generics.Rep t)
                   a
                   b) =>
                Lens s t a b
  	-- Defined in ‘Control.Lens.Tuple’

type Lens s t a b =
  forall (f :: * -> *). Functor f => (a -> f b) -> s -> f t
  	-- Defined in ‘Control.Lens.Type’

Pelude Control.Lens> set (_2 . _Left) "new" (False, Left "old")
(False,Left "new")
Prelude Control.Lens> set _Left "new" (Left "old")
Left "new"
Prelude Control.Lens> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
Prelude Control.Lens>