\begin{code}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import ClassyPrelude

import System.IO hiding (print, putStrLn)

import NTFS
import Seeker
import Utils
\end{code}
\begin{code}
parser :: Seeker (BootSector, Mft, BootSector, Integer)
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
    traceM $ "So we moved " ++ showSize displacement ++ ", that is " ++ show displacement ++ " bytes"
    b' <- parseBootSector
    return (b, m, b', displacement)
\end{code}
\begin{code}
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "You must specify the input file!"
        (file:_) -> do
            (bs, mft, bs', d) <- withBinaryFile (unpack file) ReadMode (runSeeker parser)
            print bs
            print mft
            print bs'
            putStrLn $ "Run this: ./seeking " ++ file ++ " " ++ tshow d
\end{code}