module Example.DiscreteAdventure.Engine1 where

import Prelude

import Ai.Llm.Config as Ai.Llm.Config
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Example.DiscreteAdventure (Engine, main_component)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver as HVD
import Utility (paragraph, paragraphs, replaceFormatVars)

type World =
  { player_name :: String
  }

engine :: Engine World
engine =
  { config: Ai.Llm.Config.config."openai"."gpt-4-turbo"
  , initial_world:
      { player_name: "Glorbax the Unready" }
  , renderWorld: \world ->
      HH.div
        []
        [ HH.text $ show world ]
  , initial_transcript:
      []
  , initial_choices:
      [ { description: "Begin the story."
        , short_description: "Begin the story."
        , update:
            { description: "Identity"
            , apply: identity
            }
        }
      ]
  , promptStoryEvent: \world choice ->
      { system: paragraphs
          [ """
You are a game master for a role-playing game with the player.
The way this game works is that the player will describe what they want to do in the game, and you will reply with the description of how the player exactly goes about doing the thing they want to do, and also the immediate consequences of that in the game world.
Your written descriptions of what's happening should be from the point of view of the players's character.
"""
          , templateWorldNotes world
          ]
      , user: choice.description
      }
  }

templateWorldNotes :: World -> String
templateWorldNotes world =
  replaceFormatVars
    ( Map.fromFoldable
        [ "player_name" /\ world.player_name ]
    ) $ paragraph
    """
The following are notes about the current state of the game world:
- The player's name is {{player_name}}
"""

--------------------------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI main_component { engine } =<< HA.awaitBody)

