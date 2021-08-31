module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Either (Either(..))
import Data.Int as Int

import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)

import Formless as F
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (connect, Connected)
import Halogen.Store.Select (selectEq)
import Halogen.Store.Monad (class MonadStore, runStoreT, updateStore)
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event, preventDefault)

-- render in main

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  app <- runStoreT initialStore reduce formComponent
  runUI app unit body

-- main component

data Action = HandleForm Player | Receive (Connected Store Unit)

-- use extended Form

formComponent :: forall q o m. MonadAff m => MonadStore StoreAction Store m => H.Component q Unit o m
formComponent = connect (selectEq identity) $ H.mkComponent
  { initialState: \{ context } -> context
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, receive = Just <<< Receive }
  }
  where
  handleAction = case _ of
    HandleForm player -> logShow (player :: Player)

    Receive store ->
      H.put store.context

  render { name } =
    HH.div_
      [ HH.text $ "Name: " <> name
      , HH.slot F._formless unit (F.component input spec) { name } HandleForm]

-----
-- GLOBAL STATE
-----

type Store = { name :: String }

initialStore :: Store
initialStore = { name: "" }

type StoreAction = Store -> Store

reduce :: Store -> StoreAction -> Store
reduce store k = k store


-- formless types

type Player = { name :: String }

newtype PlayerForm (r :: Row Type -> Type) f = PlayerForm
  (r
  --          error    input  output
    ( name :: f Void   String String)
  )

derive instance newtypePlayerForm :: Newtype (PlayerForm r f) _

-- initial values for fielddeep fuchsias
input :: forall m. Monad m => { name :: String } -> F.Input' PlayerForm m
input { name } =
  { initialInputs: Nothing -- same as: Just (F.wrapInputFields { name: "", age: "" })
  , validators: PlayerForm
      { name: F.noValidation }
  }

data FormAction = FormSubmit Event

-- extend Formless component

spec :: forall query m
      . MonadStore StoreAction Store m
     => MonadAff m
     => F.Spec PlayerForm () query FormAction () { name :: String } Player m
spec = F.defaultSpec {
    render = render, handleAction = handleAction, handleEvent = handleEvent
  }
  where
  handleAction (FormSubmit event) = do
    H.liftEffect $ preventDefault event
    { form } <- H.get
    let submittedName = F.getInput _name form
    updateStore \store -> store { name = submittedName }
    F.handleAction handleAction handleEvent F.submit

  handleEvent = F.raiseResult

  render { form } =
    HH.form
      [ HE.onSubmit $ F.injAction <<< FormSubmit ]
      [ HH.input
          [ HP.value $ F.getInput _name form
          , HP.placeholder ""
          , HE.onValueInput $ F.set _name
          ]
      , HH.input
          [ HP.value "Submit"
          , HP.type_ HP.InputSubmit
          ]
      ]

_name = Proxy :: Proxy "name"