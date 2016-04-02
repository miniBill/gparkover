\begin{code}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import ClassyPrelude

import System.IO hiding (print)

import NTFS
import Seeker
\end{code}
\begin{code}
parser :: Seeker (BootSector, Mft)
parser = do
    b <- parseBootSector
    m <- parseMft (bsClustersPerMftRecord b) (bsMftLogicalClusterNumber b)
    return (b, m)
\end{code}
\begin{code}
main :: IO ()
main = do
    (bs, mft) <- withBinaryFile "broken.img" ReadMode (runSeeker parser)
    print bs
    print mft
\end{code}