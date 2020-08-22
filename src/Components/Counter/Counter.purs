module Components.Counter where

import Prelude

import Data.Interpolate (i)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import React.Basic.DOM (button, div_, p_, text)
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useState')
import React.Basic.Hooks as Hooks

type Props = 
  { label :: String
  , onClick :: Int -> Effect Unit
  , counterType :: CounterType
  }

data CounterType = Increment | Decrement

counterTypeToString :: CounterType -> String
counterTypeToString = case _ of
  Increment -> "incrementer"
  Decrement -> "decrementer"

counterTypeFromString :: String -> Maybe CounterType
counterTypeFromString = case _ of
  "incrementer" -> Just Increment
  "decrementer" -> Just Decrement
  _ -> Nothing

mkCounter :: Component Props
mkCounter = component "Counter" \props -> Hooks.do 
    count /\ setCount <- useState' 0

    pure do
      div_
        [ p_ [text $ i "You clicked " count " times"]
        , button
            { onClick: handler_ do 
                let next = step count props.counterType
                setCount next
                props.onClick next
            , children: [ text props.label ]   
            } 
        ]

step :: Int -> CounterType -> Int
step n counterType = case counterType of
                      Increment -> n + 1
                      Decrement -> n - 1
