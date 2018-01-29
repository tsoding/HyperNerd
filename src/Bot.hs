module Bot where

import qualified Data.Text as T

data Event = Join

data Effect s = None
              | Say T.Text

bot :: Event -> Effect s
bot Join = Say $ T.pack "Hi!"
