module Control.Applicative exposing (apply, liftA2, liftA3)


apply : (p -> (a -> b)) -> (p -> a) -> (p -> b)
apply f g x =
    f x (g x)


liftA2 : (a -> b -> c) -> (p -> a) -> (p -> b) -> (p -> c)
liftA2 f aa ab x =
    f (aa x) (ab x)


liftA3 : (a -> b -> c -> d) -> (p -> a) -> (p -> b) -> (p -> c) -> (p -> d)
liftA3 f aa ab ac =
    liftA2 f aa ab <*> ac
