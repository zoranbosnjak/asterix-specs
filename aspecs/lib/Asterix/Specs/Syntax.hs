
module Asterix.Specs.Syntax where

import           Data.Text (Text)
import           Data.Text.Lazy.Builder (Builder)

import           Asterix.Specs.Types

type Decoder = FilePath -> Text -> Either String Asterix
type Encoder = Asterix -> Builder
