module Bot.CustomCommand ( addCustomCommand
                         , deleteCustomCommand
                         , dispatchCustomCommand
                         ) where

import           Command
import qualified Data.Text as T
import           Effect
import           Events

addCustomCommand :: CommandHandler (T.Text, T.Text)
addCustomCommand _ _ = return ()

deleteCustomCommand :: CommandHandler T.Text
deleteCustomCommand _ _ = return ()

dispatchCustomCommand :: Sender -> Command T.Text -> Effect ()
dispatchCustomCommand _ _ = return ()
