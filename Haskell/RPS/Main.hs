module Main where

import Data.Random.Extras
import Data.Random.Source.DevRandom
import Data.RVar

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

main :: IO ()
main = do
    putStrLn "Choose rock, paper or scissors"
    userHand <- getLine
    compHand <- runRVar hand DevRandom
    putStrLn("You have chosen: " ++ userHand ++ " and computer ")
    print $ compHand
    print $ getHand userHand `playAgainst` compHand
