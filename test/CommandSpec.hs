{-# LANGUAGE OverloadedStrings #-}

module CommandSpec where

import Bot (wiggle)
import Command
import Control.Monad.Free (Free(..))
import Data.Functor.Identity (Identity(..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Effect (Effect, EffectF(..))
import Network.HTTP.Simple (httpLBS, parseRequest)
import Reaction (runReaction)
import Test.HUnit

commandWithGermanUmlauts :: Test
commandWithGermanUmlauts =
  TestLabel "Parse Command with German Umlauts" $
  TestCase $
  assertEqual
    "Unexpected parse result"
    (Just Command {commandName = "russify", commandArgs = "äöü"})
    (textAsCommand "!russify äöü")

commandWithRussians :: Test
commandWithRussians =
  TestLabel "Parse Command with Russians" $
  TestCase $
  assertEqual
    "Unexpected parse result"
    (Just Command {commandName = "russify", commandArgs = "водка"})
    (textAsCommand "!russify водка")

textAsPipeSpec :: Test
textAsPipeSpec =
  TestLabel "Parse Command Pipe" $
  TestCase $
  assertEqual
    "Unexpected parse result"
    [Command "rq" "", Command "russify" ""]
    (textAsPipe "!rq | !russify")

textAsPipeSingleCommandSpec :: Test
textAsPipeSingleCommandSpec =
  TestLabel "Parse Command Pipe with single command" $
  TestCase $
  assertEqual "Unexpected parse result" [Command "rq" ""] (textAsPipe "!rq")

wiggleSpec :: Test
wiggleSpec =
  TestCase
    (do counterIO <- newIORef (0 :: Int)
        _ <- runEffectIO counterIO $ runReaction wiggle (Identity "Username")
        actual <- readIORef counterIO
        assertEqual "There should be exactly two invocations" 2 actual
        return ())
  where
    runEffectIO :: IORef Int -> Effect a -> IO a
    runEffectIO _ (Pure a) = return a
    runEffectIO counter (Free (HttpRequest _ f)) = do
      modifyIORef' counter (+ 1)
      request <- parseRequest "https://httpstat.us/404"
      response <- httpLBS request
      let result = f response
      runEffectIO counter result
    runEffectIO counter (Free (Timeout _ e s)) = do
      runEffectIO counter e
      runEffectIO counter s
    runEffectIO _ _ = error "not implemented"
