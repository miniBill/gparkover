\begin{code}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import ClassyPrelude

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get hiding (getBytes)

import NTFS

parser :: Get (BootSector, Mft)
parser = do
    b <- parseBootSector
    m <- parseMft (bsClustersPerMftRecord b) (bsMftLogicalClusterNumber b)
    return (b, m)

main :: IO ()
main = do
    contents <- B.readFile "broken.img"
    let (bs, mft) = runGet parser contents
    print bs
    print mft
\end{code}