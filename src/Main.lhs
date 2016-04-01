\begin{code}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import ClassyPrelude

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get hiding (getBytes)

import NTFS


main :: IO ()
main = do
    contents <- B.readFile "broken.img"
    let parser = do
        b <- parseBootSector
        m <- parseMft (ebpbClustersPerMftRecord . bsExtendedBpb $ b) ()
        return (b, m)
    let (bs, mft) = runGet parser contents
    print bs
    print mft
\end{code}