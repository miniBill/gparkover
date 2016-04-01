This module's code is taken almost verbatim from Data.Binary.Get.

\begin{code}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Get.Extra
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC.
--
\end{code}
\begin{code}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.Binary.Get.Extra where

import ClassyPrelude

import Data.Binary.Get
import Data.Int
\end{code}
\begin{code}
-- | Read an Int8
getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8
{-# INLINE getInt8 #-}
\end{code}
\begin{code}
-- | Read an Int16 in little endian format
getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16le
{-# INLINE getInt16le #-}
\end{code}
\begin{code}
-- | Read an Int64 in little endian format
getInt64le :: Get Int64
getInt64le = fromIntegral <$> getWord64le
{-# INLINE getInt64le #-}
\end{code}