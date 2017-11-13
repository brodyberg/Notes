module PipesPlayground where

import Pipes

name :: Proxy p => () -> Producer p String IO ()
name () = runIdentityP $ do
  lift $ putStr "Ho ho ho! What is your name? "
  lift getLine >>= respond