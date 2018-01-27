module Codec.Beam
  ( -- * Generate BEAM code
    encode
    -- * Syntax
  , Op, X(..), Y(..), F(..), Nil(..), Label(..), Import(..), Literal(..), Lambda(..)
  , Destination, destination, Pair, pair, Field, field
    -- * Argument constraints
  , Register, RegisterF, Source, SourceF
  ) where



-- | Create code for a BEAM module!
encode
  :: BS.ByteString          -- ^ module name
  -> [(BS.ByteString, Int)] -- ^ functions @(name, arity)@ that should be public
  -> [Op]                   -- ^ instructions
  -> BS.ByteString          -- ^ return encoded BEAM
encode name toExpose ops =
  Env
    { _moduleName = Atom name
    , _labelCount = 0
    , _functionCount = 0
    , _atomTable = Table.singleton name 1
    , _literalTable = Table.empty
    , _lambdaTable = []
    , _importTable = Table.empty
    , _exportNextLabel = Nothing
    , _toExport = []
    , _code = mempty
    }
