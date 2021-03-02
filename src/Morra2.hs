module Morra2 where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Maybe
import Data.Char
import Data.List.Extra
import System.Random
import Data.Bifunctor as BI
import Control.Monad
import Text.Read (readMaybe)


data Choice = Odds | Evens deriving Show
newtype GameState = GameState (Int,Int) 

instance Show GameState where
    show (GameState (p,c)) = "P: "++ show p ++ " C: " ++ show c

game :: IO ()
game = do 
    newState <- execStateT session $ GameState (0,0)
    print newState 

session :: StateT GameState IO () 
session = do
    (GameState scores) <- get   
    choice <- liftIO playerChoice 
    number <- liftIO playerNumber
    computerNumber <- liftIO computerNumber 
    let sum = computerNumber + number
        playerIsWinner = playerWinner choice sum  
    put . GameState $ if playerIsWinner 
                            then BI.first (+1) scores
                            else fmap (+1) scores 

playerWinner :: Choice -> Int -> Bool
playerWinner Odds = odd
playerWinner Evens = even 

playerNumber :: IO Int 
playerNumber = do
    print "insert your number:" 
    n <- getLine 
    case readMaybe n of  
        Nothing -> playerNumber 
        Just x -> x <$ print ("P: " ++ show x)

computerNumber :: IO Int 
computerNumber = do
        n <- randomRIO (0,9) 
        print ("C: " ++ show n) 
        pure n

playerChoice :: IO Choice 
playerChoice = do
    print "select Odds or Evens" 
    choice <- validateChoice <$> getLine
    case choice of 
        Nothing -> playerChoice
        Just c -> do
            print ("Player is " ++ show c ++ ", Computer is " ++ show (reverseChoice c))
            pure c

reverseChoice :: Choice -> Choice
reverseChoice Odds = Evens
reverseChoice Evens = Odds 

validateChoice :: String -> Maybe Choice
validateChoice s 
        | Odds `eqIgnoreCase` s = Just Odds
        | Evens `eqIgnoreCase` s = Just Evens
        | otherwise = Nothing 
            where eqIgnoreCase choice s = allSame $ (toLower <$>) <$> [show choice, s]