module Codec.Beam
  ( Instruction(..), Term(..)
  , encode
  ) where

import qualified Control.Monad.State as State
import Control.Applicative ((<*))

import Data.Binary.Put (runPut, putWord32be)
import Data.Bits (shiftL, (.&.))
import Data.ByteString.Lazy (ByteString)
import Data.Map ((!))
import Data.Monoid ((<>))
import Data.Word (Word8, Word32)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Map as Map


{-| Create structurally correct BEAM code.
 -}


data Instruction
  = Label
  | FuncInfo ByteString Int
  | Return
  | Move Term Term


data Term
  = Integer Int
  | Atom ByteString
  | X Int


encode :: ByteString -> [Instruction] -> ByteString
encode name instructions =
  let
    (code, Env atomTable labelCount functionCount) =
      State.runState
        (mapM (generate name) instructions)
        (Env (Map.singleton name 1) 0 0)

    sections =
      mconcat
        [ "Atom" <> prefixLength (encodeAtoms atomTable)
        , "Code" <> prefixLength (encodeCode labelCount functionCount code)
        , "LocT" <> prefixLength (pack32 0)
        , "StrT" <> prefixLength (pack32 0)
        , "ImpT" <> prefixLength (pack32 0)
        , "ExpT" <> prefixLength (pack32 0)
        ]
  in
    "FOR1" <> pack32 (BS.length sections) <> "BEAM" <> sections


instructionSetId :: Word32
instructionSetId =
  0


maxOpCode :: Word32
maxOpCode =
  159



-- HELPERS


encodeAtoms :: Map.Map ByteString Int -> ByteString
encodeAtoms table =
  pack32 (length list) <> concatM fromName list

  where
    fromName name =
      pack8 (BS.length name) <> name

    list =
      map fst
        $ List.sortOn snd
        $ Map.toList table



encodeCode :: Word32 -> Word32 -> [ByteString] -> ByteString
encodeCode labelCount functionCount instructions =
  let
    header =
      mconcat
        [ pack32 instructionSetId
        , pack32 maxOpCode
        , pack32 labelCount
        , pack32 functionCount
        ]
  in
    prefixLength header <> mconcat instructions



-- GENERATE


data Env
  = Env
      { _atoms :: Map.Map ByteString Int
      , _labelCount :: Word32
      , _functionCount :: Word32
      }


generate :: ByteString -> Instruction -> State.State Env ByteString
generate moduleName i =
  case i of
    Label ->
      op 1 [] <* incLabelCount

    FuncInfo name arity ->
      op 2 [Atom moduleName, Atom name, Integer arity] <* incLabelCount

    Return ->
      op 19 []

    Move from to ->
      op 64 [from, to]

  where
    op code args =
      do  wordLists <-
            mapM term args

          return $ pack32 code <> BS.pack (mconcat wordLists)


term :: Term -> State.State Env [Word8]
term t =
  case t of
    Integer value ->
      return (encodeNumber 1 value)

    Atom name ->
      encodeNumber 2 <$> getAtom name

    X reg ->
      return (encodeNumber 3 reg)


incLabelCount :: State.State Env ()
incLabelCount =
  do  old <-
        State.gets _labelCount

      State.modify $
        \e -> e { _labelCount = old + 1 }


incFunctionCount :: State.State Env ()
incFunctionCount =
  do  old <-
        State.gets _functionCount

      State.modify $
        \e -> e { _functionCount = old + 1 }


getAtom :: ByteString -> State.State Env Int
getAtom name =
  do  old <-
        State.gets _atoms

      case Map.lookup name old of
        Just id ->
          return id

        Nothing ->
          do  let id =
                    Map.size old + 1

              State.modify $
                \e -> e { _atoms = Map.insert name id old }

              return id



-- BYTES


encodeNumber :: Word8 -> Int -> [Word8]
encodeNumber tag n =
  if n < 16 then
    [ shiftL (fromIntegral n) 1 .&. tag ]

  else
    error "TODO"


prefixLength :: ByteString -> ByteString
prefixLength bytes =
  pack32 (BS.length bytes) <> align bytes


pack8 :: Integral n => n -> ByteString
pack8 =
  BS.singleton . fromIntegral


pack32 :: Integral n => n -> ByteString
pack32 n =
  runPut (putWord32be (fromIntegral n :: Word32))


align :: ByteString -> ByteString
align bytes =
  bytes <> BS.replicate (BS.length bytes `mod` 4) 0



-- HELPERS


concatM :: Monoid m => (a -> m) -> [a] -> m
concatM f =
  mconcat . map f
