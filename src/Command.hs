module Command where

import qualified Data.Text as T
import Text.Regex

data Command a = Command { commandName :: T.Text
                         , commandArgs :: a
                         } deriving (Eq, Show)

textAsCommand :: T.Text -> Maybe (Command T.Text)
textAsCommand text =
    do [name, args] <- matchRegex (mkRegex "^!(\\w+)\\s*(.*)$") $ T.unpack text
       return $ Command { commandName = T.pack name
                        , commandArgs = T.pack args }
