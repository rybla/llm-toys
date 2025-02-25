module Example.NaturalRobot.Compile where

import Prelude

import Compile.Common (Compile, base_href)

compile :: Compile
compile = do
  let localpath = "Example/NaturalRobot/"
  let index_href = base_href <> localpath <> "index.html"

  pure { label: "Example.NaturalRobot", href: index_href }
