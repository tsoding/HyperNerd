{-# LANGUAGE OverloadedStrings #-}
module OrgModeTest (spec) where

import OrgMode
import Test.HUnit

spec :: Test
spec =
  TestLabel "Rendering OrgMode table" $
  TestCase $
  assertEqual
    ""
    ("|hello|world|foo|\n\
     \|-\n\
     \|1|2|3|\n\
     \|1|2|3|\n\
     \|1|2||\n\
     \|1|||\n\
     \||||\n\
     \|1|2|3|\n\
     \|\\|~|||\n") $
  renderTable
    ["hello", "world", "foo"]
    [ ["1", "2", "3"]
    , ["1", "2", "3"]
    , ["1", "2"]
    , ["1"]
    , []
    , ["1", "2", "3", "4", "5"]
    , ["|~"]
    ]
