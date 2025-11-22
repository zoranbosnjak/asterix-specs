-- | Toplevel Asterix.Specs module.

module Asterix.Specs
( module Asterix.Specs
, module Asterix.Specs.Types
, module Asterix.Specs.Syntax
, module Asterix.Specs.Validation
) where

import           Asterix.Specs.Syntax
import qualified Asterix.Specs.Syntax.Ast
import qualified Asterix.Specs.Syntax.Internal
import qualified Asterix.Specs.Syntax.Json
import           Asterix.Specs.Types
import           Asterix.Specs.Validation

syntaxes :: [(String, Coder)]
syntaxes =
    [ ("internal", Asterix.Specs.Syntax.Internal.coder)
    , ("ast", Asterix.Specs.Syntax.Ast.coder)
    , ("json", Asterix.Specs.Syntax.Json.coder)
    ]

