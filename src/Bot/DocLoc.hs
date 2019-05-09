{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.DocLoc
  ( githubLinkLocationStr
  , githubLinkLocation
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Haskell.TH
import Text.InterpolatedString.QM

githubLinkLocationStr :: Q Exp
githubLinkLocationStr = LitE . StringL <$> githubLinkLocation

gitHead :: IO String
gitHead = do
  headContent <- T.strip <$> TIO.readFile ".git/HEAD"
  if T.take 5 headContent == "ref: "
    then T.unpack . T.strip <$>
         TIO.readFile (T.unpack $ T.append ".git/" $ T.drop 5 headContent)
    else return $ T.unpack headContent

githubLinkLocation :: Q String
githubLinkLocation = do
  filename <- loc_filename <$> location
  line <- fst . loc_start <$> location
  rev <- runIO gitHead
  return
    [qms|https://github.com/tsoding/HyperNerd/blob/{rev}/{filename}#L{line}|]
