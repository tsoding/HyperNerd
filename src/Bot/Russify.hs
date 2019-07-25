{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Russify
  ( russifyCommand
  , derussifyCommand
  ) where

import Bot.Replies
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.FileEmbed
import qualified Data.Map.Lazy as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Reaction
import Transport

russifyCommand :: Reaction Message T.Text
russifyCommand = cmapR (transliterate mazarusha) sayMessage

derussifyCommand :: Reaction Message T.Text
derussifyCommand = cmapR (transliterate demazarusha) sayMessage

mazarusha :: M.Map T.Text T.Text
mazarusha =
  fromMaybe M.empty $
  decode $
  B.fromStrict $ encodeUtf8 $(embedStringFile "./resources/mazarusha.json")

demazarusha :: M.Map T.Text T.Text
demazarusha =
  M.fromList
    [ ("а", "a")
    , ("б", "b")
    , ("в", "v")
    , ("г", "g")
    , ("д", "d")
    , ("е", "ye")
    , ("ё", "yo")
    , ("ж", "j")
    , ("з", "z")
    , ("и", "ee")
    , ("й", "y")
    , ("к", "k")
    , ("л", "l")
    , ("м", "m")
    , ("н", "n")
    , ("о", "o")
    , ("п", "p")
    , ("р", "r")
    , ("с", "s")
    , ("т", "t")
    , ("у", "oo")
    , ("ф", "f")
    , ("х", "kh")
    , ("ц", "ts")
    , ("ч", "ch")
    , ("ш", "sh")
    , ("щ", "sch")
    , ("ъ", "") -- hard sign
    , ("ы", "y")
    , ("ь", "") -- soft sign
    , ("э", "eh")
    , ("ю", "yu")
    , ("я", "ya")
    , ("А", "A")
    , ("Б", "B")
    , ("В", "V")
    , ("Г", "G")
    , ("Д", "D")
    , ("Е", "Ye")
    , ("Ё", "Yo")
    , ("Ж", "J")
    , ("З", "Z")
    , ("И", "Ee")
    , ("Й", "Y")
    , ("К", "K")
    , ("Л", "L")
    , ("М", "M")
    , ("Н", "N")
    , ("О", "O")
    , ("П", "P")
    , ("Р", "R")
    , ("С", "S")
    , ("Т", "T")
    , ("У", "Oo")
    , ("Ф", "F")
    , ("Х", "Kh")
    , ("Ц", "Ts")
    , ("Ч", "Ch")
    , ("Ш", "Sh")
    , ("Щ", "Sch")
    , ("Ъ", "") -- Hard sign
    , ("Ы", "Y")
    , ("Ь", "") -- Soft sign
    , ("Э", "Eh")
    , ("Ю", "Yu")
    , ("Я", "Ya")
    ]

transliterateChar :: M.Map T.Text T.Text -> Char -> T.Text
transliterateChar table x = fromMaybe (T.pack [x]) $ M.lookup (T.pack [x]) table

transliterate :: M.Map T.Text T.Text -> T.Text -> T.Text
transliterate = T.concatMap . transliterateChar
