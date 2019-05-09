{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.DocLoc where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Haskell.TH
import Text.InterpolatedString.QM

githubLinkLocationStr :: Q Exp
githubLinkLocationStr = LitE . StringL <$> githubLinkLocation

githubLinkLocation :: Q String
githubLinkLocation = do
  filename <- loc_filename <$> location
  line <- fst . loc_start <$> location
  rev <-
    runIO $ do
      refFilename <-
        T.unpack . T.append ".git/" . T.drop 5 . T.strip <$>
        TIO.readFile ".git/HEAD"
      T.unpack . T.strip <$> TIO.readFile refFilename
  return
    [qms|https://github.com/tsoding/HyperNerd/blob/{rev}/{filename}#L{line}|]
