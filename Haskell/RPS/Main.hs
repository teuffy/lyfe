{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List
import           Data.Map.Strict
import           Data.Maybe
import           Data.Random.Extras
import           Data.Random.Source.DevRandom
import           Data.RVar

data Hand = Rock | Scissors | Paper deriving (Eq, Show)
instance Ord Hand where
    (>) Rock Scissors = True
    (>) Scissors Paper = True
    (>) Paper Rock = True
    (>) _ _ = False
possibleHands :: [Hand]
possibleHands = [Rock, Paper, Scissors]

randomHand :: IO Hand
randomHand = runRVar handChoice DevRandom
    where
        handChoice = choice(possibleHands)

playAgainst :: Hand -> Hand -> String
playAgainst userHand compHand
    | userHand > compHand = "You have won"
    | userHand == compHand = "Draw!"
    | otherwise = "You have lost!"

getHandFromChar :: Char -> Hand
getHandFromChar input
    | input == 'r' = Rock
    | input == 's' = Scissors
    | input == 'p' = Paper

writeHandToDB :: Hand -> IO ()
writeHandToDB hand
   | hand == Rock = writeToDefaultDB "r"
   | hand == Paper = writeToDefaultDB "p"
   | hand == Scissors = writeToDefaultDB "s"
    where
        writeToDefaultDB :: String -> IO ()
        writeToDefaultDB = appendFile "db"

type Cache = [Maybe Char]
updateLastThreePlays :: Cache -> Char -> Cache
updateLastThreePlays (c0 : c1 : [_]) nc = (Just nc) : c0 : [c1]
updateLastThreePlays _ _ = [Nothing, Nothing, Nothing] -- empty cache

getBeatingHand :: Hand -> Hand
getBeatingHand inHand
    | inHand == Rock = Paper
    | inHand == Paper = Scissors
    | inHand == Scissors = Rock
    | otherwise = undefined

groupIt :: String -> String -> Map Char Int
groupIt cache databaseContent =
    go cache databaseContent empty
    where
        go :: String -> String -> Map Char Int -> Map Char Int
        go key content accMap
            | contentLength < 2 || keyLength >= contentLength = accMap
            | (reverse key) `isPrefixOf` content = go key (tail content) (insertWith (+) (content !! keyLength) 1 accMap)
            | otherwise = go key (tail content) accMap
            where
                keyLength = length key
                contentLength = length content

checkIt :: Map Char Int -> IO Hand
checkIt superMap = do
    go Nothing $ toList superMap
    where
        go :: Maybe (Char, Int) -> [(Char, Int)] -> IO Hand
        go _ [] = randomHand
        go Nothing [(c, _)] = return $ getBeatingHand $ getHandFromChar c
        go (Just (c0, n0)) [(c1, n1)]
            | n0 > n1 = go Nothing [(c0, n0)]
            | n0 < n1 = go Nothing [(c1, n1)]
            | otherwise = do
                (c, n) <- runRVar equivalentHands DevRandom
                go Nothing [(c, n)]
                    where
                        equivalentHands = choice([(c0, n0), (c1, n1)])
        go (Just (c0, n0)) ((c1, n1) : cns)
            | n0 > n1 = go (Just (c0, n0)) cns
            | n0 < n1 = go (Just (c1, n1)) cns
            | otherwise =  do
                (c, n) <- runRVar equivalentHands DevRandom
                go (Just (c, n)) cns
                    where
                        equivalentHands = choice([(c0, n0), (c1, n1)])
        go Nothing ((c, n) : cns) = go (Just (c, n)) cns

calculateCompHand :: Cache -> IO Hand
calculateCompHand cs = do
    content <- readFile "db"
    checkIt $ groupIt cacheValue content
        where
        cacheValue = catMaybes cs

play :: Cache -> IO ()
play cache = do
    compHand <- calculateCompHand cache
    putStrLn "Choose (r)ock, (p)aper or (s)cissors"
    userHand <- getLine
    writeHandToDB $ getHandFromChar $ head userHand
    print $ compHand
    print $ getHandFromChar (head userHand) `playAgainst` compHand
    play $ updateLastThreePlays cache  $ head userHand

main :: IO ()
main = play [Nothing, Nothing, Nothing]