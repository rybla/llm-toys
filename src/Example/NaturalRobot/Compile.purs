module Example.NaturalRobot.Compile where

import Prelude

import Compile.Common (Compile, base_href, dist_path, writeTextFile)
import Data.Array as Array
import Data.String as String
import Node.ChildProcess as CP

compile :: Compile
compile = do
  let localpath = "Example/NaturalRobot/"
  let app_module_name = "Example.NaturalRobot.Compile"

  let index_href = base_href <> localpath <> "index.html"
  let index_path = dist_path <> localpath <> "index.html"
  let main_path = dist_path <> localpath <> "main.js"

  writeTextFile index_path index_html

  CP.execSync
    ( [ "bun spago bundle"
      , "--bundle-type app"
      , "--module " <> app_module_name
      , " --outfile " <> main_path
      ] # Array.intercalate " "
    ) # void

  pure { label: "Example.NaturalRobot", href: index_href }

index_html :: String
index_html = String.trim
  """
<!DOCTYPE html>
<html lang="en">

<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>llm-toys | Discrete Adventure | Engine1</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Merienda:wght@300..900&display=swap" rel="stylesheet">

<style></style>

<script src="main.js"></script>
</head>

<body>

</body>

</html>
"""

