module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Dog as Dog

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Dog.formComponent unit body