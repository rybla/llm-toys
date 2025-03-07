module Example.MutableWorld.Compile where

import Prelude

import Compile.Common (Compile, base_href, dist_path, writeTextFile)
import Data.Foldable (fold)
import Data.Traversable (traverse)
import Example.MutableWorld.Common (Engine)
import Example.MutableWorld.Engine1 as Engine1
import Node.ChildProcess as CP
import Utility (format)

compile :: Compile
compile = do
  [ { label: "Example.MutableWorld.Engine1"
    , localpath: "Example/MutableWorld/Engine1/"
    , app_module_name: "Example.MutableWorld.Engine1"
    , engine: Engine1.engine
    }
  ]
    # traverse compile_engine
    # map fold

compile_engine :: { label :: String, localpath :: String, app_module_name :: String, engine :: Engine } -> Compile
compile_engine { label, localpath, app_module_name, engine } = do
  let index_href = base_href <> localpath <> "index.html"
  let index_path = dist_path <> localpath <> "index.html"
  let main_path = dist_path <> localpath <> "main.js"

  writeTextFile index_path $ index_html engine

  void $ CP.execSync $
    "bun --platform=node spago bundle --bundle-type app --module {{app_module_name}} --outfile {{main_path}}"
      # format { app_module_name, main_path }

  pure [ { label, href: index_href } ]

index_html :: Engine -> String
index_html engine =
  format
    { title: "MutableWorld | " <> engine.name
    }
    """
<!DOCTYPE html>
<html lang="en">

<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>llm-toys | {{title}}</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Merienda:wght@300..900&display=swap" rel="stylesheet">

<style>
html, body {
  margin: 0;
  padding: 0;
  font-family: Merienda;
}

.App {
  padding: 1em;
  display: flex;
  flex-direction: column;
  gap: 1em;
}

.Transcript {
  height: 20em;
  overflow-y: scroll;
  
  padding: 1em;
  box-shadow: 0 0 0 1px black inset;

  display: flex;
  flex-direction: column;
  gap: 0.5em;
}

.Msg {
  box-shadow: 0 0 0 1px black inset;

  display: flex;
  flex-direction: row;
}

.Msg>div {
  padding: 0.5em;
}

.Msg>div:nth-child(1) {
  flex-grow: 0;
  flex-shrink: 0;
  
  width: 8em;
  
  background-color: black;
  color: white;
}

.Msg>div:nth-child(2) {
  flex-grow: 1;
  flex-shrink: 1;

  background-color: transparent;
  color: black;

  whitespace: pre-wrap;
}

.World {  
  height: 20em;
  overflow-y: scroll;

  padding: 1em;
  box-shadow: 0 0 0 1px black inset;

  white-space: pre-wrap;
}

.Prompts {
  height: 20em;
  overflow-y: scroll;

  padding: 1em;
  box-shadow: 0 0 0 1px black inset;

  display: flex;
  flex-direction: column;
  gap: 0.5em;
}

.PromptButton {
  padding: 0.5em;
  text-align: left;
  font-family: Merienda;
}
</style>

<script src="main.js"></script>
</head>

<body>

</body>

</html>
"""

