module Components.Interop where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Components.Counter (CounterType(..), Props, mkCounter, counterTypeFromString)
import React.Basic (JSX)

type JSProps = 
    { label :: Nullable String
    , onClick :: Nullable (EffectFn1 Int Unit)
    , counterType :: Nullable String
    }

jsPropsToProps :: JSProps -> Props
jsPropsToProps props = 
    { label: fromMaybe "Click away!" $ toMaybe props.label
    , onClick: fromMaybe mempty $ map runEffectFn1 $ toMaybe props.onClick
    , counterType: fromMaybe Increment $ counterTypeFromString =<< toMaybe props.counterType
    }

jsCounter :: JSProps -> JSX
jsCounter = unsafePerformEffect mkCounter <<< jsPropsToProps
