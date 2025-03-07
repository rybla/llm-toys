module Example.DiscreteAdventure.Engine1.Compile where

import Prelude

import Compile.Common (Compile, base_href, dist_path, writeTextFile)
import Data.String as String
import Node.ChildProcess as CP
import Utility (format)

compile :: Compile
compile = do
  let label = "Example.DiscreteAdventure.Engine1"
  let localpath = "Example/DiscreteAdventure/Engine1/"
  let app_module_name = "Example.DiscreteAdventure.Engine1.App"

  let index_href = base_href <> localpath <> "index.html"
  let index_path = dist_path <> localpath <> "index.html"
  let main_path = dist_path <> localpath <> "main.js"

  writeTextFile index_path index_html

  void $ CP.execSync $
    "bun --platform=node spago bundle --bundle-type app --module {{app_module_name}} --outfile {{main_path}}"
      # format { app_module_name, main_path }

  pure [ { label, href: index_href } ]

index_html ∷ String
index_html =
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

.column-item {
  padding: 0.5em;
  border-radius: 0.5em;
}

.story-item {}

.story-item.generating {
  background: linear-gradient(to right, rgba(255, 0, 0, 0.3), rgba(255, 166, 0, 0.3), rgba(255, 255, 0, 0.3), rgba(0, 128, 0, 0.3), rgba(0, 0, 255, 0.3), rgba(76, 0, 130, 0.3), rgba(238, 130, 238, 0.3), rgba(255, 0, 0, 0.3));
  background-size: 200% 100%;
  animation: strobe 4s linear infinite;
}

.story-item.error {
  background-color: rgba(255, 0, 0, 0.3);
}

.story-item.choice {
  background-color: rgba(0, 255, 208, 0.3);
}

.story-item.description {
  background-color: rgba(208, 0, 255, 0.062);
}

.menu-item {}

.menu-item.generating {
  background: linear-gradient(to right, rgba(255, 0, 0, 0.3), rgba(255, 166, 0, 0.3), rgba(255, 255, 0, 0.3), rgba(0, 128, 0, 0.3), rgba(0, 0, 255, 0.3), rgba(76, 0, 130, 0.3), rgba(238, 130, 238, 0.3), rgba(255, 0, 0, 0.3));
  background-size: 200% 100%;
  animation: strobe 4s linear infinite;
}

.menu-item.waiting_for_story {
  background: linear-gradient(to right, rgba(255, 0, 0, 0.3), rgba(255, 166, 0, 0.3), rgba(255, 255, 0, 0.3), rgba(0, 128, 0, 0.3), rgba(0, 0, 255, 0.3), rgba(76, 0, 130, 0.3), rgba(238, 130, 238, 0.3), rgba(255, 0, 0, 0.3));
  background-size: 200% 100%;
  animation: strobe 4s linear infinite;
}

.menu-item.error {
  background-color: rgba(255, 0, 0, 0.3);
}

.menu-item.choice {
  background-color: rgba(0, 255, 208, 0.3);
  cursor: pointer;
}

.menu-item.choice,
.menu-item.choice:hover {
  transition-duration: 0.5s;
  transition-property: box-shadow;
}

.menu-item.choice:hover {
  box-shadow:
    0.2em 0.2em 0 0 black,
    -0.2em -0.2em 0 0 black;
}

/* Define the strobe animation keyframes */
@keyframes strobe {
  0% {
    background-position-x: 0%;
  }

  100% {
    background-position-x: 200%;
  }
}
</style>

<link rel="stylesheet" href="main.css">
<script src="main.js"></script>
</head>

<body>

</body>

</html>
""" # String.trim

