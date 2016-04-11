{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe
import           Data.Random.Extras
import           Data.Random.Source.DevRandom
import           Data.RVar
import qualified Data.Text as T
import Data.List
import Data.Map.Strict

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
    | input == "rock" = Rock
    | input == "scissors" = Scissors
    | input == "paper" = Paper

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

groupIt :: String -> String -> Map Char Int -> Map Char Int
groupIt key content accMap
    | key `isInfixOf` content = groupIt key (tail content) (insertWith (+) (content !! 3) 1 accMap)
    | otherwise = groupIt key (tail content) accMap

-- checkIt :: Map Char Int -> Hand
-- checkIt superMap = 
--    foldlWithKey f 


calculateCompHand :: Cache -> IO Hand
calculateCompHand cs = do
    content <- readFile "db"
    undefined
        where
        sum = catMaybes cs
-- runRVar hand DevRandom
-- calculateCompHand (Just a, _, _) = do
--  content <- readFile "db"
--  undefined


play :: Cache -> IO ()
play cache = do
    putStrLn "Choose rock, paper or scissors"
    userHand <- getLine
    compHand <- runRVar hand DevRandom
    writeToDB $ getHand userHand
    print $ compHand
    print $ getHand userHand `playAgainst` compHand
    play $ superCache cache $ head userHand

main :: IO ()
main = play [Nothing, Nothing, Nothing]
