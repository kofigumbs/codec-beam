{-# LANGUAGE FlexibleInstances #-}
module BShow where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)


class BShow a where
  bshow :: a -> ByteString


instance BShow ByteString where
  bshow = id


instance BShow String where
  bshow = encodeUtf8 . pack


instance BShow Int where
  bshow = encodeUtf8 . pack . show


instance BShow Bool where
  bshow True = "yes"
  bshow False = "nah"
