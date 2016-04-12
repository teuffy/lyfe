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

hand :: RVar Hand
hand = choice(possibleHands)

computerHand :: IO Hand
computerHand = runRVar hand DevRandom

playAgainst :: Hand -> Hand -> String
playAgainst userHand compHand
    | userHand > compHand = "You have won"
    | userHand == compHand = "Draw!"
    | otherwise = "You have lost!"

getHand :: String -> Hand
getHand input
    | input == "r" = Rock
    | input == "s" = Scissors
    | input == "p" = Paper

writeToDB :: Hand -> IO ()
writeToDB hand
   | hand == Rock = writeToDefaultDB "r"
   | hand == Paper = writeToDefaultDB "p"
   | hand == Scissors = writeToDefaultDB "s"
    where
     writeToDefaultDB :: String -> IO ()
     writeToDefaultDB = appendFile "db"
type Cache = [Maybe Char]
superCache :: Cache -> Char -> Cache
superCache (c0 : c1 : [_]) nc = (Just nc) : c0 : [c1]
superCache _ _ = undefined

getBeatingHand :: Hand -> Hand
getBeatingHand hand
    | hand == Rock = Paper
    | hand == Paper = Scissors
    | hand == Scissors = Rock

groupIt :: String -> String -> Map Char Int -> Map Char Int

-- TODO: game beggining
groupIt key content  accMap
    | length content >= 4 && key `isInfixOf` content = groupIt key (tail content) (insertWith (+) (content !! 3) 1 accMap)
    | length content < 4 = accMap
    | length key < 3 = accMap
    | otherwise = groupIt key (tail content) accMap

checkIt :: Map Char Int -> IO Hand
checkIt superMap = do
    go Nothing $ toList superMap
    where
        go :: Maybe (Char, Int) -> [(Char, Int)] -> IO Hand
        go _ [] = runRVar hand DevRandom
        go Nothing [(c, _)] = return $ getBeatingHand $ getHand [c]
        go (Just (c0, n0)) [(c1, n1)]
            | n0 > n1 = go Nothing [(c0, n0)]
            | otherwise = go Nothing [(c1, n1)]
        go (Just (c0, n0)) ((c1, n1) : cns)
            | n0 > n1 = go (Just (c0, n0)) cns
            | otherwise = go (Just (c1, n1)) cns
        go Nothing ((c, n) : cns) = go (Just (c, n)) cns

calculateCompHand :: Cache -> IO Hand
calculateCompHand cs = do
    content <- readFile "db"
    print content
    checkIt $ groupIt stringCache content empty
        where
        stringCache = catMaybes cs

play :: Cache -> IO ()
play cache = do
    putStrLn "Choose rock, paper or scissors"
    userHand <- getLine
    print $ "1"
    compHand <- calculateCompHand cache
    print $ "2"
    writeToDB $ getHand userHand
    print $ "3"
    print $ compHand
    print $ getHand userHand `playAgainst` compHand
    play $ superCache cache $ head userHand

main :: IO ()
main = play [Nothing, Nothing, Nothing]
