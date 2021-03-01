{-# LANGUAGE FlexibleInstances #-}
module Morra where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Char
import Data.Maybe
import Data.Map
import System.Random
import Text.Read (readMaybe)

data Choice = Evens
            | Odds 
            deriving (Show)

validateChoice :: String -> Maybe Choice
validateChoice s 
            | lowerS == (toLower <$> show Evens) = Just Evens
            | lowerS == (toLower <$> show Odds) = Just Odds 
            | otherwise = Nothing 
            where lowerS = toLower <$> s

newtype GameState = GameState { players :: Map String Int }

instance Show GameState where
    show (GameState m) = show m

app :: IO ()
app = do
    results <- do 
        let gameState = GameState (fromList [("C", 0),("P", 0)])
        execStateT game gameState 
    print $ show results

instance Show (StateT GameState IO ()) where
    show (StateT f) = "" 

-- newtype StateT s m a = StateT {runStateT s -> m (a,s)}
game :: StateT GameState IO ()
game = do
    (GameState players) <- get :: (StateT GameState IO GameState) 
    choice <- liftIO askChoice 
    computerNumber <- liftIO computerNumberChoice 
    number <- liftIO numberChoice
    liftIO $ print $ "C: " ++ show computerNumber
    let sum = computerNumber + number 
        win = winner choice sum 
    liftIO $ if even sum then print "pari"
                         else print "dispari"
    put $ GameState $ adjust (+1) (if win then "P" else "C") players 

winner :: Choice -> (Int -> Bool)
winner Odds = odd 
winner Evens = even  

    --runStateT game $ GameState (fromList [("C", 0),("P",0)]) 
numberChoice :: IO Int
numberChoice = do
    print "segli il tuo numero:" 
    mNumber <- getLine
    case readMaybe mNumber of 
        Nothing -> do
            print "non hai scritto un numero, riprova"
            numberChoice
        Just a -> pure a

computerNumberChoice :: IO Int
computerNumberChoice = randomRIO (0, 9) 

askChoice :: IO Choice
askChoice = do 
    print "scegli odds or evens:"
    mChoice <- validateChoice <$> getLine
    case mChoice of
         Nothing -> print "sbagliato, riprova" >> askChoice
         Just x -> do
             print $ "hai scelto " ++ show x
             pure x

