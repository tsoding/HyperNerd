{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.DocLoc
  ( githubLinkLocationStr
  , githubLinkLocation
  , gitHeadStr
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Haskell.TH
import Text.InterpolatedString.QM

githubLinkLocationStr :: Q Exp
githubLinkLocationStr = LitE . StringL <$> githubLinkLocation

gitHeadStr :: Q Exp
gitHeadStr = LitE . StringL <$> gitHead

gitHead :: Q String
gitHead =
  runIO $ do
    headContent <- T.strip <$> TIO.readFile ".git/HEAD"
    if T.take 5 headContent == "ref: "
      then T.unpack . T.strip <$>
           TIO.readFile (T.unpack $ T.append ".git/" $ T.drop 5 headContent)
      else return $ T.unpack headContent

githubLinkLocation :: Q String
githubLinkLocation = do
  filename <- loc_filename <$> location
  line <- fst . loc_start <$> location
  rev <- gitHead
  return
    [qms|https://github.com/tsoding/HyperNerd/blob/{rev}/{filename}#L{line}|]
