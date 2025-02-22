module Ai.Llm.Config where

import Ai.Llm (GenerateConfig)

foreign import config
  :: { "openai" ::
         { "gpt-4-turbo" :: GenerateConfig
         , "gpt-4o" :: GenerateConfig
         , "gpt-4o-mini" :: GenerateConfig
         , "o1" :: GenerateConfig
         , "o1-mini" :: GenerateConfig
         , "o3-mini" :: GenerateConfig
         }
     , "ollama" ::
         { "deepseek-r1:14b" :: GenerateConfig
         , "llama3.2:3b" :: GenerateConfig
         , "gemma2:9b" :: GenerateConfig
         }
     }

