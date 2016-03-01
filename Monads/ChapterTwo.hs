module ChapterTwo where

data Sheep

father :: Sheep -> Maybe Sheep
father = undefined

mother :: Sheep -> Maybe Sheep
mother = undefined

-- ugh
maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = case (mother s) of
                            Nothing -> Nothing
                            Just m -> father m
    
-- ugh * ugh!
mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = case (mother s) of
                                Nothing -> Nothing
                                Just m -> case (father m) of
                                    Nothing -> Nothing
                                    Just gf -> father gf

-- we are beginning something!
-- comb is just a combinator for sequencing operations that return Maybe... maybe
comb :: Maybe a -> (a -> Maybe b) -> Maybe b
comb Nothing _ = Nothing
comb (Just x) f = f x

-- now we can use `comb` (infix) to build complicated sequences
mothersPaternalGrandfatherWithComb :: Sheep -> Maybe Sheep
mothersPaternalGrandfatherWithComb s = (Just s) `comb` mother `comb` father `comb` father
-- ^ super cool
-- now with scary bind operators which are equal to our `comb`
mothersPaternalGrandfatherWithBindOperator :: Sheep -> Maybe Sheep
mothersPaternalGrandfatherWithBindOperator s = return s >>= mother >>= father >>= father