module Example.MutableWorld.App where

import Prelude

import Ai2.Llm (Config, generate, generate_structure, mkSystemMsg, mkUserMsg)
import Ai2.Widget.Provider as Provider
import Control.Monad.State (get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Argonaut.Decode (fromJsonString)
import Data.Array (filter, foldMap, intercalate, length, take)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens ((%=), (.=))
import Data.List (Pattern)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Aff as Aff
import Example.MutableWorld.Common (Engine)
import Example.MutableWorld.World (World, WorldUpdate, applyWorldUpdate, describeWorld, renderWorld, renderWorldUpdate)
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (PlainHTML, fromPlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Utility (copyToClipboard, readFromClipboard)
import Halogen.VDom.Driver as HVD
import Halogen.Widget as Widget
import Type.Prelude (Proxy(..))
import Utility (css, format, prop)
import Web.Event.Event as Event
import Web.HTML.HTMLTextAreaElement as HTMLTextAreaElement
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

--------------------------------------------------------------------------------

make_main :: Engine -> Effect Unit
make_main engine = HA.runHalogenAff (HVD.runUI main_component engine =<< HA.awaitBody)

--------------------------------------------------------------------------------

type Input = Engine

type State =
  { config :: Maybe Config
  , engine :: Engine
  , processing :: Boolean
  , world :: World
  , story :: Array { prompt :: String, content :: String }
  , transcript :: Array { label :: String, content :: PlainHTML }
  }

data Action
  = SetConfig Config
  | SubmitPrompt PromptSource String
  | InputKeyDown PromptSource KeyboardEvent
  | ImportWorld
  | ExportWorld
  | ImportStory
  | ExportStory
  | ExportStoryMd

data PromptSource
  = UpdateWorld_PromptSource
  | PromptStory_PromptSource

main_component :: forall query output. H.Component query Input output Aff
main_component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState engine =
    { engine
    , config: Nothing
    , processing: false
    , transcript: []
    , story: []
    , world:
        { characters: Map.empty
        , locations: Map.empty
        }
    }

  eval = H.mkEval H.defaultEval { handleAction = handleAction }

  getConfig = get >>= \{ config } -> config # flip maybe pure do
    throwError $ Aff.error "config has not been set"

  handleAction (SetConfig config) = do
    prop @"config" .= pure config
    pure unit

  handleAction (SubmitPrompt PromptStory_PromptSource userPrompt_) = do
    config <- getConfig

    get >>= \{ processing } ->
      when processing do
        throwError $ Aff.error $ "already processing! dont submit another prompt yet"

    prop @"processing" .= true

    -- generate next paragraph of story
    story_content <- do
      systemMsg <- do
        pure $ mkSystemMsg $ String.trim
          """
You are a helpful fiction writing assistant, and are currently collaborating with the user to write a novel.

The way this collaboration works is that if the user has written a portion of their story already, they will show you the latest few paragraphs in their story.
You should read these paragraphs carefully to get an idea of what's going on right now in their story.

The user will also provide a description of the current state of the world.
Note that this description is fairly comprehensive -- it may include many details about the world that are not immediately relevant to what's going on right now in their story.
But, also note that even less-immediate details will be very useful to have in the back of your mind when considering what direction things should go in the short term, in order for the course of action to eventually lead to resolving other far-away situations in the world.
The story takes place in this world, so make sure to use locations and characters by name, have named characters speak dialogue, and take into account their descriptions, statuses, and other properties.

Finally, the user will provide a high-level suggestion for what they think should happen next in their story.
Consider this suggestion, and find an interpretation that makes the most sense to make their story interesting and make progress in developing the plot and the worldbuilding.

You should reply with 1-3 paragraphs that continue the user's story, picking up right where the user left off (if they've written anything in their story already).
  """

      let userPrompt = userPrompt_ # String.trim
      promptMsg <- do
        { world, story } <- get
        pure $ mkUserMsg $ String.trim $
          """
{{story_so_far}}

So right now in the story, this is the current state of the world:

{{world}}

This is my suggestion for what should happen next in the story:

    {{prompt}}  
  """
            # format
                { story_so_far:
                    if length story == 0 then
                      "I haven't written any of the story yet. So you are helping me write the very first paragraph."
                    else
                      intercalate "\n\n"
                        [ "The following paragraphs are the most recent portion of the story I'm currently writing."
                        , intercalate "\n\n" $
                            story # take 2 # map (\{ content } -> "    " <> content)
                        ]
                , world: describeWorld world
                , prompt: userPrompt
                }

      state_backup <- get
      prop @"transcript" %=
        ( _ `Array.snoc`
            { label: "Story / Next / User Prompt"
            , content: HH.text userPrompt
            }
        )

      result <- do
        err_msg <- lift $ generate { config, messages: [ systemMsg, promptMsg ] }
        case err_msg of
          Left err -> do
            put state_backup
            throwError $ Aff.error $ "error when generating: " <> err
          Right msg -> pure msg

      let story_content = String.trim result.content

      prop @"transcript" %=
        ( _ `Array.snoc`
            { label: "Story / Next / Model"
            , content: HH.text story_content
            }
        )

      prop @"story" %= (_ `Array.snoc` { prompt: userPrompt, content: story_content })
      pure story_content

    -- derive and apply changes to world
    do
      let
        systemMsg = mkSystemMsg $ String.trim
          """
You are a helpful assistant for writing story-related content.
The user is currently writing a story.
They will present to you the current state of the world, and the most recent paragraph of their story which may imply some changes to the world state.
It is critically important to keep the world state updated to match the latest events in their story, so the user needs your help to update the world state.
The updates you produce should exactly reflect whatever changes to the world state are implied by the single story paragraph provided by the user.
You will always output in a structured form with an array of updates to apply simultaneously to the world.
  """

      let userPrompt = userPrompt_ # String.trim
      promptMsg <- do
        { world } <- get
        let
          prompt =
            """
The current state of the world:

{{world}}

The next paragraph of my story, which may imply some changes to the world state:

    {{story_content}}
    """
              # format
                  { world: describeWorld world
                  , story_content
                  }
              # String.trim
        pure $ mkUserMsg prompt

      state_backup <- get
      prop @"transcript" %=
        ( _ `Array.snoc`
            { label: "Story / Update World / User Prompt"
            , content: HH.text userPrompt
            }
        )

      result <- do
        err_msg <- lift $
          generate_structure @(updates :: Array WorldUpdate)
            { config, name: "updates", messages: [ systemMsg, promptMsg ] }
        case err_msg of
          Left err -> do
            put state_backup
            throwError $ Aff.error $ "error when generating: " <> err
          Right msg -> pure msg

      prop @"transcript" %=
        ( _ `Array.snoc`
            { label: "Story / Update World / Model Response"
            -- , content: result.updates # map show # intercalate "\n"
            , content:
                HH.div [ css do tell [ "display: flex", "flex-direction: column", "gap: 0.5em", "flex-wrap: wrap" ] ] $
                  result.updates # map
                    ( HH.div
                        [ css do
                            tell [ "padding: 0.5em" ]
                            -- tell [ "box-shadow: 0 0 0 1px black" ]
                            tell [ "background-color: color-mix(in hsl, black, transparent 80%)" ]
                        ] <<< pure <<< renderWorldUpdate
                    )

            }
        )
      prop @"world" %= \world -> foldr applyWorldUpdate world result.updates

    prop @"processing" .= false

  handleAction (SubmitPrompt UpdateWorld_PromptSource userPrompt_) = do
    config <- getConfig

    get >>= \{ processing } ->
      when processing do
        throwError $ Aff.error $ "already processing! dont submit another prompt yet"

    prop @"processing" .= true

    let
      systemMsg = mkSystemMsg $ String.trim
        """
You are a helpful assistant for writing story-related content.
You are interacting with a fictional world in collaboration with the user.
The world may start off empty, of pre-filled with some existing content from the user.
The user will give you instructions for how to update the world, by creating new content to put into the world or modifying existing content.
The idea is that these changes will reflect a story progressing in the fictional world.
You will always output in a structured form with an array of updates to apply simultaneously to the world.
Make sure to always keep the user's specific instructions in mind, but also feel free to take creative liberties and extrapolate interesting details in order to make the updates reflect an interesting sequence of events for a story!
Have fun with it!
"""

    let userPrompt = userPrompt_ # String.trim
    promptMsg <- do
      { world } <- get
      let
        prompt =
          """
The current state of the world:

{{world}}

My instructions for how to update the world state:

    {{prompt}}
  """
            # format
                { world: describeWorld world
                , prompt: userPrompt
                }
            # String.trim
      pure $ mkUserMsg prompt

    state_backup <- get
    prop @"transcript" %=
      ( _ `Array.snoc`
          { label: "Manually modify world / User Prompt"
          , content: HH.text userPrompt
          }
      )

    result <- do
      err_msg <- lift $
        generate_structure @(updates :: Array WorldUpdate)
          { config
          , name: "updates"
          , messages:
              [ systemMsg
              , promptMsg
              ]
          }
      case err_msg of
        Left err -> do
          put state_backup
          throwError $ Aff.error $ "error when generating: " <> err
        Right msg -> pure msg

    prop @"transcript" %=
      ( _ `Array.snoc`
          { label: "Manually modify world / Model Response"
          -- , content: result.updates # map show # intercalate "\n"
          , content:
              HH.div [ css do tell [ "display: flex", "flex-direction: column", "gap: 0.5em", "flex-wrap: wrap" ] ] $
                result.updates # map
                  ( HH.div
                      [ css do
                          tell [ "padding: 0.5em" ]
                          -- tell [ "box-shadow: 0 0 0 1px black" ]
                          tell [ "background-color: color-mix(in hsl, black, transparent 80%)" ]
                      ] <<< pure <<< renderWorldUpdate
                  )
          }
      )
    prop @"world" %= \world -> foldr applyWorldUpdate world result.updates

    prop @"processing" .= false

  handleAction ExportWorld = do
    { world } <- get
    copyToClipboard (stringifyWithIndent 4 $ encodeJson world) # liftAff # void

  handleAction ImportWorld = do
    readFromClipboard # liftAff >>= case _ of
      Left _ -> pure unit
      Right s -> case fromJsonString s of
        Left _ -> pure unit
        Right world -> prop @"world" .= world

  handleAction ExportStory = do
    { story } <- get
    copyToClipboard (stringifyWithIndent 4 $ encodeJson story) # liftAff # void

  handleAction ExportStoryMd = do
    { story } <- get
    void $ liftAff $ copyToClipboard $ intercalate "\n\n" $
      story
        # foldMap \{ content, prompt } ->
            [ "> " <> prompt
            , content
            ]

  handleAction ImportStory = do
    readFromClipboard # liftAff >>= case _ of
      Left _ -> pure unit
      Right s -> case fromJsonString s of
        Left _ -> pure unit
        Right story -> prop @"story" .= story

  handleAction (InputKeyDown source event) = do
    let
      key = event # KeyboardEvent.key
      cmd = (event # KeyboardEvent.ctrlKey) || (event # KeyboardEvent.metaKey)

    when (key == "Enter" && cmd) do
      el <- event
        # KeyboardEvent.toEvent
        # Event.target
        # maybe (throwError $ Aff.error "impossible") \target -> target
            # HTMLTextAreaElement.fromEventTarget
            # maybe (throwError $ Aff.error "impossible") pure
      v <- el # HTMLTextAreaElement.value # liftEffect
      HTMLTextAreaElement.setValue "" el # liftEffect
      handleAction (SubmitPrompt source v)

  render state =
    let
      transcript_processing_slotId = show (length state.transcript)
      transcript_bottom_slotId = "transcript_bottom_" <> show (length state.transcript + (if state.processing then 1 else 0))

      story_bottom_slotId = "story_bottom_" <> show (length state.story)

      transcript =
        HHK.div [ HP.classes [ H.ClassName "Transcript" ] ] $ fold $
          [ state.transcript # mapWithIndex \i { label, content } ->
              Tuple (show i) $
                HH.div [ HP.classes [ H.ClassName "Msg" ] ]
                  [ HH.div [] [ HH.text label ]
                  , HH.div [] [ content # HH.fromPlainHTML ]
                  ]
          , if not state.processing then []
            else [ Tuple transcript_processing_slotId $ HH.div [] [ HH.text "processing..." ] ]
          , [ Tuple transcript_bottom_slotId $ HH.slot_ (Proxy @"ScrollToMe") transcript_bottom_slotId Widget.scrollToMe unit ]
          ]
      story =
        HHK.div [ HP.classes [ H.ClassName "Story" ] ] $ fold $
          [ state.story # mapWithIndex \i { prompt, content } ->
              Tuple (show i) $ HH.div [ HP.classes [ H.ClassName "StoryItem" ] ] $ fold $
                [ [ HH.div [ HP.classes [ H.ClassName "StoryItemPrompt" ] ] [ HH.text prompt ] ]
                , content # String.split (Pattern "\n") # map String.trim # filter (not <<< String.null) # map \s ->
                    HH.div [ HP.classes [ H.ClassName "StoryItemContent" ] ] [ HH.text s ]
                ]
          , [ Tuple story_bottom_slotId $ HH.slot_ (Proxy @"ScrollToMe") story_bottom_slotId Widget.scrollToMe unit ]
          ]
      world =
        HH.div [ HP.classes [ H.ClassName "World" ] ]
          -- [ HH.text $ describeWorld state.world ]
          [ renderWorld state.world # fromPlainHTML ]
      toolbar =
        HH.div [ HP.classes [ H.ClassName "Toolbar" ] ]
          [ HH.button
              [ HE.onClick $ const ExportWorld ]
              [ HH.text "export world" ]
          , HH.button
              [ HE.onClick $ const ImportWorld ]
              [ HH.text "import world" ]
          , HH.button
              [ HE.onClick $ const ExportStory ]
              [ HH.text "export story (json)" ]
          , HH.button
              [ HE.onClick $ const ExportStoryMd ]
              [ HH.text "export story (md)" ]
          , HH.button
              [ HE.onClick $ const ImportStory ]
              [ HH.text "import story" ]
          ]
      prompting =
        HH.div [ HP.classes [ H.ClassName "Prompts" ] ]
          [ HH.div [ HP.classes [ H.ClassName "PromptSectionTitle" ] ]
              [ HH.text "Manually modify world:" ]
          , HH.textarea
              [ HE.onKeyDown $ InputKeyDown UpdateWorld_PromptSource
              , HP.value "Create some locations and characters for a medieval fantasy world. Be creative!"
              ]
          , HH.div [ HP.classes [ H.ClassName "PromptSectionTitle" ] ]
              [ HH.text "Prompt next portion of story:" ]
          , HH.textarea
              [ HE.onKeyDown $ InputKeyDown PromptStory_PromptSource
              , HP.value "A secret cabal of mages gather in the Shadowhold Citadel. They are outcasts from Acanion city. They are trying to cast a curse on the famed Elara the Bard in order to spark chaos so that they can take over the city."
              ]
          ]
      provider = HH.slot (Proxy @"provider") unit Provider.component { providerCategory: "Main", providers: Provider.providers_with_structured_output } SetConfig

      row kids =
        HH.div [ HP.classes [ H.ClassName "AppRow" ] ]
          kids
      panel title kid =
        HH.div [ HP.classes [ H.ClassName "AppPanel" ] ]
          [ HH.div [] [ HH.text title ]
          , HH.div [] [ kid ]
          ]
      panel_small title kid =
        HH.div [ HP.classes [ H.ClassName "AppPanel", H.ClassName "small" ] ]
          [ HH.div [] [ HH.text title ]
          , HH.div [] [ kid ]
          ]
    in
      HH.div
        [ HP.classes [ H.ClassName "App" ] ]
        [ panel_small "Toolbar" toolbar
        , row
            [ panel "World" world
            , panel "Story" story
            ]
        , HH.div [ HP.classes [ H.ClassName "AppRow" ] ]
            [ panel "Prompting" prompting
            , panel "Transcript" transcript
            ]
        , provider
        ]

