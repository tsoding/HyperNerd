module Bot where

import qualified Data.Text as T

type Bot s = Event -> Effect s

data Event = Join

data Effect s = None
              | Say T.Text

bot :: Bot s
bot Join = Say $ T.pack "Hi!"
