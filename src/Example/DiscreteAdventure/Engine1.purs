module Example.DiscreteAdventure.Engine1 where

import Prelude

import Ai.Llm.Config as Ai.Llm.Config
import Control.Monad.Writer (tell)
import Data.Map as Map
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Example.DiscreteAdventure (Engine, main_component)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver as HVD
import Utility (css, paragraph, paragraphs, replaceFormatVars)

type World =
  { player_name :: String
  , player_description :: String
  , player_strengths :: Array String
  , player_weaknesses :: Array String
  }

engine :: Engine World
engine =
  { config: Ai.Llm.Config.config."openai"."gpt-4o"
  , initial_world:
      { player_name: "Glorbax"
      , player_description: "Glorbax, a pilot in the private security force AetherSec, navigates the neon-drenched skies with a blend of military precision and reckless abandon. His expertise in piloting sleek, armored craft makes him a legend in high-speed pursuits, often turning the tide in critical missions for the corporation. Though his thrill-seeking nature frequently tests the boundaries of protocol, his unmatched strengths render him indispensable to the force."
      , player_strengths: [ "Vehicle Operation", "Navigation", "Mechanic Skills", "Combat Driving", "Sensor Operation" ]
      , player_weaknesses: [ "Overconfidence in Flight", "Cybernetic Dependency", "Haunted by a Past Failure", "Substance Abuse", "Corporate Loyalty Blindspot", "Chronic Injury", "Technological Overreliance", "Fear of Ground Combat" ]
      }
  , renderWorld: \world ->
      HH.div
        [ css do
            tell [ "padding: 1em" ]
            tell [ "display: flex", "flex-direction: column", "gap: 1em" ]
        ]
        [ HH.div
            [ css do tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
            [ HH.div [ css do tell [ "font-weight: bold" ] ] [ HH.text "Player" ]
            , HH.div
                [ css do
                    tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ]
                    tell [ "padding-left: 1em" ]
                ]
                let
                  item title value = HH.div []
                    [ HH.span [ css do tell [ "font-weight: bold" ] ] [ HH.text $ title <> ": " ]
                    , HH.text value
                    ]
                in
                  [ item "Name" $ world.player_name
                  , item "Description" $ world.player_description
                  , item "Strengths" $ String.joinWith ", " world.player_strengths
                  , item "Weaknesses" $ String.joinWith ", " world.player_weaknesses
                  ]
            ]

        ]
  , initial_transcript:
      [ { description: "The neon haze of New Amsterdam shimmered in the rain as Glorbax stepped from the mag-rail station, the air thick with ozone and fried synth-meat. Towers loomed, their facades flickering with ads—cryptosecurities, neural augments, off-world visas—all selling escape from a city that never let go. Glorbax adjusted their damp coat and scanned the crowd: chrome-fingered fixers, AI bootleggers, drones humming like fireflies. Somewhere in District 17, a stolen prototype, a missing scientist, a whispered name awaited—the kind of job that paid well, if it didn’t get them killed first."
        , choice:
            { short_description: "I exit the mag-rail station of New Amsterdam."
            , description: "I exit the mag-rail station of New Amsterdam."
            , update: { apply: identity, description: "identity" }
            }
        }
      ]
  , initial_choices:
      [ { short_description: "Follow the lead."
        , description: "A cryptic message pings on Glorbax’s neural HUD: Red Lantern, back room, come alone. The Red Lantern is a seedy bar deep in District 17, a place where deals are made and bodies disappear. Glorbax could head there now, hoping to get ahead of whoever else might be chasing the same target."
        , update: { description: "identity", apply: identity }
        }
      , { short_description: "Gather intel."
        , description: "Before diving into the unknown, Glorbax could tap into their network. A local hacker named Spindle owes them a favor—perhaps a quick meeting in a back alley or a secure data drop could reveal more about the stolen prototype and its significance before they make their next move."
        , update: { description: "identity", apply: identity }
        }
      , { short_description: "Lay low and observe."
        , description: "The city never stops talking. Glorbax could melt into the crowd, eavesdropping in street markets and corporate cafés, watching who moves where and who’s looking nervous. Sometimes, the best advantage isn’t rushing in—it’s waiting for someone else to slip up first."
        , update: { description: "identity", apply: identity }
        }
      ]
  , prompt_StoryEvent: \world choice -> do
      pure
        { system: paragraphs
            [ """
You are a game master for a role-playing game with the player.
The way this game works is that the player will describe what they want to do in the game, and you will reply with the description of how the player exactly goes about doing the thing they want to do, and also the immediate consequences of that in the game world.
Each description you write be a short paragraph -- around 3-4 sentences long.
Make sure your paragraphs don't get too long.
Your written descriptions of what's happening should be from the point of view of the player's character.
"""
            , templateWorldNotes world
            ]
        , user: choice.description
        }
  , prompt_StoryChoices: \world events -> do
      pure
        { system: paragraphs
            [ """
You are a game master for a role-playing game with the player.
The user will provide the transcript of the story so far that describes the game session.
You should respond, in a structured format, with 3-5 different choices that the player gets to choose from of what to do immediately next in the game given their current situation.
The choices should be very distinct and reflect different types of attitudes that the player could take towards their current situation.
For example, you could include an aggressive choice, an emotional choice, a strategic choice, a weird choice, a choice to proceed with the current objective, a choice to pursue alternative objectives, or other kinds of choices that make sense given the player's current circumstances.
"""
            , templateWorldNotes world
            ]
        , user: paragraphs $ events # map _.description
        }
  }

templateWorldNotes :: World -> String
templateWorldNotes world =
  replaceFormatVars
    ( Map.fromFoldable
        [ Tuple "player_name" world.player_name
        , Tuple "player_description" world.player_description
        , Tuple "player_strengths" $ String.joinWith ", " world.player_strengths
        , Tuple "player_weaknesses" $ String.joinWith ", " world.player_weaknesses
        ]
    ) $ paragraph
    """
The following are notes about the current state of the game world:
- The player:
  - The player's name is {{player_name}}
  - {{player_name}} is described as follows: {{player_description}}
  - {{player_name}} has the following strengths: {{player_strengths}}
  - {{player_name}} has the following weaknesses: {{player_weaknesses}}
"""

--------------------------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI main_component { engine } =<< HA.awaitBody)

