module ChapterFive where

import           ChapterTwo    (Sheep, father, mother)
import           Control.Monad
import           Data.Maybe

-- exercise 1
maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = (return s) >>= mother >>= father >>= father

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = (return s) >>= father >>= mother >>= mother >>= mother

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = (return s) >>= mother >>= father >>= father >>= father

-- exercise 2
parent :: Sheep -> Maybe Sheep
parent s = mother s `mplus` mother s

grandparent :: Sheep -> Maybe Sheep
grandparent s = do p <- parent s
                   parent p

-- exercise 3
parentList :: Sheep -> [Sheep]
parentList s = maybeToList (mother s) `mplus` maybeToList (father s)

grandParentList :: Sheep -> [Sheep]
grandParentList s = do p <- parentList s
                       parentList p

-- exercise 4
parentMonadPlus :: (MonadPlus m) => Sheep -> m Sheep
parentMonadPlus s = maybeToMonadTransformer (father s) `mplus` maybeToMonadTransformer (mother s)

grandParentMonadPlus :: (MonadPlus m) => Sheep -> m Sheep
grandParentMonadPlus s = do p <- parentMonadPlus s
                            parentMonadPlus p

maybeToMonadTransformer (Just s) = return s 
maybeToMonadTransformer Nothing = mzero