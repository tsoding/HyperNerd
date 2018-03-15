module Bot.Russify where

import           Bot.Replies
import qualified Data.Text as T
import           Effect
import           Russify
import           Text.Printf

russifyCommand :: T.Text -> T.Text -> Effect ()
russifyCommand sender westernSpyMsg =
    replyToUser sender
    $ T.pack
    $ printf "%s KKomrade"
    $ russify westernSpyMsg
