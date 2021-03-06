module ChapterThree where

import Prelude hiding (Monad, (>>=), return)
import ChapterTwo(Sheep, mother, father)

-- aka fuzzy warm thing
class Monad m where 
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

-- Maybe fits as a Monad perfectly
instance Monad Maybe where
    Nothing  >>= f = Nothing
    (Just x) >>= f = f x
    return         = Just

maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = (return s) >>= mother >>= father

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = (return s) >>= father >>= mother >>= mother

-- we can also use do notation to build readable and complicated sequences
mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do m <- mother s
                                  gf <- father m
                                  father gf
-- mothersPaternalGrandfather s = do { m <- mother s; gf <- father m; father gf }

-- mothersPaternalGrandfather s = mother s >>= (\m ->
--                               father m >>= (\gf ->
--                                father gf))
