\begin{code}
{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, ScopedTypeVariables #-}
module Seeker where

import ClassyPrelude

import Data.ByteString (hGet)
import Data.Bits
import Data.Int
import Data.Word (Word16)
import System.IO

import Utils
\end{code}
\begin{code}
newtype Seeker a = Seeker { runSeeker :: Handle -> IO a } deriving Functor

instance Applicative Seeker where
    pure = return
    (<*>) = ap

instance Monad Seeker where
    return = Seeker . const . return
    x >>= f = Seeker (\h -> do
        y <- runSeeker x h
        runSeeker (f y) h)
\end{code}
\begin{code}
skip :: Integral a => a -> Seeker ()
skip = seek RelativeSeek
\end{code}
\begin{code}
seek :: Integral a => SeekMode -> a -> Seeker ()
seek k i = Seeker (\h -> hSeek h k (toInteger i))
\end{code}
\begin{code}
getAddress :: Seeker Integer
getAddress = Seeker hTell
\end{code}
\begin{code}
getFileSize :: Seeker Integer
getFileSize = Seeker hFileSize
\end{code}
\begin{code}
getByteString :: Int -> Seeker ByteString
getByteString len = Seeker (\h -> do
    got <- hGet h len
    if length got == len
        then return got
        else error $ "EOF reached while reading" ++ show len ++ "bytes")
\end{code}
\begin{code}
getWords :: (Bits a, Num a) => Int -> Seeker a
getWords len = foldr foldNum 0 <$> getByteString len
\end{code}
\begin{code}
foldNum :: (Bits a, Num a) => Word8 -> a -> a
foldNum n o = (o `shiftL` 8) .|. (fromIntegral n)
\end{code}
\begin{code}
getWord8 :: Seeker Word8
getWord8 = getWords 1
\end{code}
\begin{code}
getWord16le :: Seeker Word16
getWord16le = getWords 2
\end{code}
\begin{code}
getWord32le :: Seeker Word32
getWord32le = getWords 4
\end{code}
\begin{code}
getWord64le :: Seeker Word64
getWord64le = getWords 8
\end{code}
\begin{code}
search :: ByteString -> Seeker ()
search bs = do
    ps <- getByteString (length bs)
    if bs == ps
        then return ()
        else do
            skip (1 - length bs)
            search bs
\end{code}
\begin{code}
searchDupes :: Integer -> Integer -> Seeker [Integer]
searchDupes displacement max = undefined
\end{code}

The following code is taken almost verbatim from Data.Binary.Get.

\begin{code}
-----------------------------------------------------------------------------
-- |
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC.
--
\end{code}
\begin{code}
-- | Read an Int8
getInt8 :: Seeker Int8
getInt8 = fromIntegral <$> getWord8
{-# INLINE getInt8 #-}
\end{code}
\begin{code}
-- | Read an Int16 in little endian format
getInt16le :: Seeker Int16
getInt16le = fromIntegral <$> getWord16le
{-# INLINE getInt16le #-}
\end{code}
\begin{code}
-- | Read an Int32 in little endian format
getInt32le :: Seeker Int32
getInt32le = fromIntegral <$> getWord32le
{-# INLINE getInt32le #-}
\end{code}
\begin{code}
-- | Read an Int64 in little endian format
getInt64le :: Seeker Int64
getInt64le = fromIntegral <$> getWord64le
{-# INLINE getInt64le #-}
\end{code}