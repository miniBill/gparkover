\begin{code}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import ClassyPrelude

import System.IO hiding (print)

import NTFS
import Seeker
import Utils
\end{code}
\begin{code}
parser :: Seeker (BootSector, Mft, BootSector)
parser = do
    rawHead <- getByteString 512
    seek AbsoluteSeek 0
    b <- parseBootSector
    seek AbsoluteSeek $ (toInteger $ bsMftLogicalClusterNumber b) * bsBytesPerCluster b
    m <- parseMft $ bsBytesPerMftRecord b
    let footOffset = toInteger $ bsTotalSize b
    traceM $ "MBR copy should be at " ++ showSize footOffset
    seek AbsoluteSeek footOffset
    search rawHead
    skip (-512)
    realFootOffset <- getAddress
    traceM $ "Actually, it was at " ++ showSize realFootOffset
    let displacement = realFootOffset - footOffset
    traceM $ "So we moved " ++ showSize displacement
    b' <- parseBootSector
    seek AbsoluteSeek 0
    duplicates <- searchDupes displacement realFootOffset
    return (b, m, b')
\end{code}
\begin{code}
main :: IO ()
main = do
    (bs, mft, bs') <- withBinaryFile "broken.img" ReadMode (runSeeker parser)
    print bs
    print mft
    print bs'
\end{code}