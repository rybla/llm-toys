module Example.NaturalRobot.Compile where

import Prelude

import Compile.Common (Compile, base_href, dist_path, writeTextFile)
import Data.String as String
import Node.ChildProcess as CP
import Utility (format)

compile :: Compile
compile = do
  let localpath = "Example/NaturalRobot/"
  let app_module_name = "Example.NaturalRobot.App"

  let index_href = base_href <> localpath <> "index.html"
  let index_path = dist_path <> localpath <> "index.html"
  let main_path = dist_path <> localpath <> "main.js"

  writeTextFile index_path index_html

  void $ CP.execSync $
    "bun --platform=node spago bundle --bundle-type app --module {{app_module_name}} --outfile {{main_path}}"
      # format { app_module_name, main_path }

  pure { label: "Example.NaturalRobot", href: index_href }

index_html :: String
index_html = String.trim
  $ format
      { grid_unit: "20"
      , field_units: "20"
      , transition_duration: "200"
      }
  $
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

<style>
html,
body {
  margin: 0;
  padding: 0;
  font-family: "Merienda", serif;
}

.App {
  padding: 1em;
  display: flex;
  flex-direction: column;
  gap: 1em;
}

.Field {
  width: calc({{grid_unit}}px * {{field_units}});
  height: calc({{grid_unit}}px * {{field_units}});
  padding: {{grid_unit}}px;
  box-shadow: 0 0 0 1px black inset;
}

.Robot {
  position: relative;
  width: 0;
  height: 0;

  transition-property: all;
  transition-duration: {{transition_duration}}ms;
  transition-timing-function: linear;

  overflow: visible;
}

.Robot>div {
  position: relative;
  width: {{grid_unit}}px;
  left: calc(-{{grid_unit}}px / 2);
  height: {{grid_unit}}px;
  top: calc(-{{grid_unit}}px / 2);

  background-color: blue;
}

.Console {
  display: flex;
  flex-direction: column;
  gap: 1em;
}

.Transcript {
  height: 10em;
  overflow-y: scroll;

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
  width: 10em;
  background-color: black;
  color: white;
}

.Msg>div:nth-child(2) {
  background-color: transparent;
  color: black;
}


</style>

<script src="main.js"></script>
</head>

<body>

</body>

</html>
"""

