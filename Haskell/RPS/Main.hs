{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List
import           Data.Map.Strict
import           Data.Maybe
import           Data.Random.Extras
import           Data.Random.Source.DevRandom
import           Data.RVar

data Hand = Rock | Scissors | Paper deriving (Ord, Eq, Show)

possibleHands :: [Hand]
possibleHands = [Rock, Paper, Scissors]

randomHand :: IO Hand
randomHand = runRVar handChoice DevRandom
    where
        handChoice = choice(possibleHands)

playAgainst :: Hand -> Hand -> String
playAgainst userHand compHand 
    | playAgainst' userHand compHand == 1 = "You have won"
    | playAgainst' userHand compHand == 0 = "Draw!"
    | otherwise = "You have lost!"

playAgainst' :: Hand -> Hand -> Int
playAgainst' hand otherHand
    | hand == otherHand = 0
playAgainst' Rock otherHand
    | otherHand == Paper = -1
    | otherHand == Scissors = 1
playAgainst' Paper otherHand
    | otherHand == Scissors = -1
    | otherHand == Rock = 1
playAgainst' Scissors otherHand
    | otherHand == Rock = -1
    | otherHand == Paper = 1

deserializeHand :: Char -> Hand
deserializeHand inputChar
    | inputChar == 'r' = Rock
    | inputChar == 's' = Scissors
    | inputChar == 'p' = Paper
    | otherwise = error "Non-existing serialized Hand passed"

serializeHand :: Hand -> Char
serializeHand inputHand
    | inputHand == Rock = 'r'
    | inputHand == Scissors = 's'
    | inputHand == Paper = 'p'

getDatabaseContent :: IO String
getDatabaseContent = readFile "db"

writeHandToDB :: Hand -> IO ()
writeHandToDB h = writeCharToDefaultDB $ serializeHand h
    where
        writeCharToDefaultDB :: Char -> IO ()
        writeCharToDefaultDB c = appendFile "db" [c]

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

getHandOccurencesBasedOnCache :: String -> IO (Map Hand Int)
getHandOccurencesBasedOnCache cache = do
    content <- getDatabaseContent
    return $ go cache content empty
    where
        go :: String -> String -> Map Hand Int -> Map Hand Int
        go key content accMap
            | contentLength < 2 || keyLength >= contentLength = accMap
            | (reverse key) `isPrefixOf` content = go key (tail content) (insertWith (+) (deserializeHand (content !! keyLength)) 1 accMap)
            | otherwise = go key (tail content) accMap
            where
                keyLength = length key
                contentLength = length content

chooseBestHand :: Map Hand Int -> IO Hand
chooseBestHand nextHandOccurences = do
    go Nothing $ toList nextHandOccurences
    where
        selectRandomHandFromEqOccurences :: Hand -> Hand -> IO Hand
        selectRandomHandFromEqOccurences h1 h2 = runRVar (choice [h1, h2]) DevRandom   
        go :: Maybe (Hand, Int) -> [(Hand, Int)] -> IO Hand
        go _ [] = randomHand
        go Nothing [(h, _)] = return $ getBeatingHand h 
        go (Just (h0, n0)) [(h1, n1)]
            | n0 > n1 = go Nothing [(h0, n0)]
            | n0 < n1 = go Nothing [(h1, n1)]
            | otherwise = do
                h <- selectRandomHandFromEqOccurences h0 h1
                go Nothing [(h, n0)]
        go (Just (h0, n0)) ((h1, n1) : cns)
            | n0 > n1 = go (Just (h0, n0)) cns
            | n0 < n1 = go (Just (h1, n1)) cns
            | otherwise =  do
                h <- selectRandomHandFromEqOccurences h0 h1
                go (Just (h, n0)) cns         
        go Nothing ((h, n) : cns) = go (Just (h, n)) cns
           

calculateAIHand :: Cache -> IO Hand
calculateAIHand cs = do
    nextHandOccurences <- getHandOccurencesBasedOnCache cacheValue
    chooseBestHand nextHandOccurences
        where
        cacheValue = catMaybes cs

play :: Cache -> IO ()
play cache = do
-- calculateAIPlay
-- getAndValidateUserHand
-- play
    compHand <- calculateAIHand cache
    putStrLn "Choose (r)ock, (p)aper or (s)cissors"
    userHand <- getLine
    writeHandToDB $ deserializeHand $ head userHand
    print $ compHand
    print $ deserializeHand (head userHand) `playAgainst` compHand
    play $ updateLastThreePlays cache  $ head userHand

main :: IO ()
main = play [Nothing, Nothing, Nothing]
