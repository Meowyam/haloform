module Dog where

import Prelude (class Monad, Void, const, unit, ($))

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))


-- types

type Dog = { name :: String }

newtype DogForm (r :: Row Type -> Type) f = DogForm
  (r
  --          error    input  output
    ( name :: f Void   String String)
  )

derive instance newtypeDogForm :: Newtype (DogForm r f) _

data Action = HandleForm Dog

-- initial values for fielddeep fuchsias
input :: forall m. Monad m => F.Input' DogForm m
input =
  { initialInputs: Nothing -- same as: Just (F.wrapInputFields { name: "", age: "" })
  , validators: DogForm
      { name: F.noValidation }
  }

-- extend Formless component

spec :: forall input m. Monad m => F.Spec' DogForm Dog input m
spec = F.defaultSpec { render = render
                     , handleEvent = F.raiseResult}
  where
  render { form } =
    HH.form_
      [ HH.input
          [ HP.value $ F.getInput _name form
          , HE.onValueInput $ F.set _name
          ]
      , HH.button
          [ HE.onClick \_ -> F.submit ]
          [ HH.text "Submit" ]
      ]
    where
    _name = Proxy :: Proxy "name"

-- use extended Form

formComponent :: forall q i o m. MonadAff m => H.Component q i o m
formComponent = H.mkComponent
  { initialState: const unit
  , render: const render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction (HandleForm dog) = logShow (dog :: Dog)

  render = HH.slot F._formless unit (F.component (const input) spec) unit HandleForm