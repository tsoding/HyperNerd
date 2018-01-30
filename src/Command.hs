module Command where

import qualified Data.Text as T

data Command a = Command { commandName :: T.Text
                         , commandArgs :: a
                         }

-- TODO: implement Command.textAsCommand
textAsCommand :: T.Text -> Maybe (Command T.Text)
textAsCommand _ = Nothing
