module ChapterFive where

import ChapterTwo(Sheep, mother, father)

-- exercise 1
maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = (return s) >>= mother >>= father >>= father

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = (return s) >>= father >>= mother >>= mother >>= mother

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = (return s) >>= mother >>= father >>= father >>= father

-- exercise 2
parent :: Sheep -> Maybe Sheep
parent = undefined

grandparent :: Sheep -> Maybe Sheep
grandparent = undefined