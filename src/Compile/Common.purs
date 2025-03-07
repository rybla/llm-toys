module Compile.Common where

import Prelude

import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (permsAll)
import Node.FS.Sync (mkdir')
import Node.FS.Sync as FS
import Node.Path (dirname)

type Compile = Effect (Array { label :: String, href :: String })

base_href = "/llm-toys/"
dist_path = "docs/"

writeTextFile path text = do
  mkdir' (dirname path) { mode: permsAll, recursive: true }
  FS.writeTextFile UTF8 path text
