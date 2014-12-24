{-# LANGUAGE DeriveFunctor #-}

module Parse where

import Control.Applicative
import Control.Arrow

data Parse t a =
  Parse {
    runParse :: [t] -> Maybe (a, [t])
    } deriving (Functor)

instance Applicative (Parse t) where
  pure a = Parse $ \x -> Just (a, x)
  Parse f <*> Parse p = Parse $ \s -> case (f s) of
    Just (g, s') -> (fmap . first) g $ p s' 
    Nothing -> Nothing

instance Monad (Parse t) where
  return a = Parse $ \ x -> Just (a, x)
  Parse f >>= b = Parse $ \s -> case f s  of
    Just (a, s') -> runParse (b a) s'
    Nothing -> Nothing

instance Alternative (Parse t) where
  empty = Parse $ \s -> Nothing
  Parse p <|> Parse q = Parse $ \s -> p s <|> q s
