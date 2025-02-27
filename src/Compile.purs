module Compile where

import Prelude

import Compile.Common (dist_path, writeTextFile)
import Data.Array as Array
import Data.Map as Map
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Example.DiscreteAdventure.Engine1.Compile as Example.DiscreteAdventure.Engine1.Compile
import Utility (replaceFormatVars)

main :: Effect Unit
main = do
  let index_path = dist_path <> "index.html"

  labels_hrefs <- sequence
    [ Example.DiscreteAdventure.Engine1.Compile.compile
    ]

  writeTextFile index_path $
    index_html
      ( labels_hrefs
          # map
              ( \{ label, href } -> String.trim $
                  replaceFormatVars (Map.fromFoldable [ "label" /\ label, "href" /\ href ])
                    """<li><a href="{{href}}">{{label}}</a></li>"""

              )
          # Array.intercalate "\n    "
      )

index_html ∷ String → String
index_html labels_hrefs_listItems = String.trim $
  replaceFormatVars (Map.fromFoldable [ "labels_hrefs_listItems" /\ labels_hrefs_listItems ])
    """
<!DOCTYPE html>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>llm-toys</title>
</head>

<body>
  <ul>
    {{labels_hrefs_listItems}}
  </ul>
</body>

</html>
"""

