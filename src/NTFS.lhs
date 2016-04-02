\begin{code}
{-# LANGUAGE NoImplicitPrelude #-}
module NTFS where

import ClassyPrelude hiding (decodeUtf8)

import Data.Bits
import Data.Int
import Data.Text.Encoding
import Data.Word

import Seeker
\end{code}
\begin{code}
data Bpb = Bpb {
    bpbBytesPerSector :: Int16,
    bpbSectorsPerCluster :: Int8 }
    deriving Show
\end{code}
\begin{code}
data ExtendedBpb = ExtendedBpb {
    ebpbTotalSectors :: Int64,
    ebpbMftLogicalClusterNumber :: Int64,
    ebpbMftMirrorLogicalClusterNumber :: Int64,
    ebpbClustersPerMftRecord :: Integer }
    deriving Show
\end{code}
\begin{code}
data BootSector = BootSector {
    bsOemId :: Text,
    bsBytesPerSector :: Int16,
    bsSectorsPerCluster :: Int8,
    bsTotalSectors :: Int64,
    bsMftLogicalClusterNumber :: Int64,
    bsMftMirrorLogicalClusterNumber :: Int64,
    bsClustersPerMftRecord :: Integer }
\end{code}
\begin{code}
instance Show BootSector where
    show b = concat [
        "BootSector {\n",
        line "OEM ID" (unpack . bsOemId),
        line "Bytes per sector" bsBytesPerSector,
        line "Sectors per cluster" bsSectorsPerCluster,
        line "Total sectors" bsTotalSectors,
        line "MFT cluster #" bsMftLogicalClusterNumber,
        line "MFT mirror cluster #" bsMftMirrorLogicalClusterNumber,
        line "Clusters per MFT record" bsClustersPerMftRecord,
        line "Total size" (\b -> showSize totalSize ++ "+ 512 B for the boot sector"),
        "}"] where
        line :: Show a => String -> (BootSector -> a) -> String
        line h f = "\t" ++ h ++ ":" ++ replicate (24 - length h) ' ' ++ show (f b) ++ "\n"
        totalSize = bytesPerSector b * totalSectors b        
        bytesPerSector = toInteger . bsBytesPerSector
        totalSectors   = toInteger . bsTotalSectors
\end{code}
\begin{code}
iterate :: (a -> a) -> a -> [a]
iterate f x =  x : iterate f (f x)
\end{code}
\begin{code}
showSize :: Integer -> String
showSize x = result where
    split = map (`mod` 1024) $ iterate (`div` 1024) x
    zipped = reverse $ zip ["", "K", "M", "G", "T"] split
    result = foldMap printPiece zipped
    printPiece (s,i) = if i > 0
        then show i ++ s ++ "B "
        else ""
\end{code}
\begin{code}
parseBootSector :: Seeker BootSector
parseBootSector = do
    skip 3 -- jump instruction, ignored
    oemId <- decodeUtf8 <$> getByteString 8
    bpb <- parseBpb
    extendedBpb <- parseExtendedBpb
    skip 426 -- bootstrap code, ignored
    assertBytes "Signature" [0x55, 0xAA]
    return $ BootSector {
        bsOemId = oemId,
        bsBytesPerSector = bpbBytesPerSector bpb,
        bsSectorsPerCluster = bpbSectorsPerCluster bpb,
        bsTotalSectors = ebpbTotalSectors extendedBpb,
        bsMftLogicalClusterNumber = ebpbMftLogicalClusterNumber extendedBpb,
        bsMftMirrorLogicalClusterNumber = ebpbMftMirrorLogicalClusterNumber extendedBpb,
        bsClustersPerMftRecord = ebpbClustersPerMftRecord extendedBpb}

assertBytes :: String -> [Word8] -> Seeker ()
assertBytes err xs = do
    let ps = pack xs
    bs <- getByteString $ length ps
    if ps == bs
        then return ()
        else do
            addr <- getAddress
            return ()
            --fail $
            --    "Assert failed (" ++ err ++ "): got " ++ show bs ++
            --    " while expecting " ++ show ps ++ " at address " ++ show addr
\end{code}
\begin{code}
parseBpb :: Seeker Bpb
parseBpb = do
    bytesPerSector <- getInt16le
    sectorsPerCluster <- getInt8
    assertZeroes 2 -- reserved sectors, must be zero
    assertZeroes 5
    assertBytes "Media descriptor, 0xF8 for HDD" [0xF8]
    assertZeroes 2
    skip 8
    assertZeroes 4
    return $ Bpb {
        bpbBytesPerSector = bytesPerSector,
        bpbSectorsPerCluster = sectorsPerCluster }
\end{code}
\begin{code}
assertZeroes :: Int -> Seeker ()
assertZeroes n = assertBytes "Zeroes" $ replicate n 0
\end{code}
\begin{code}
parseExtendedBpb :: Seeker ExtendedBpb
parseExtendedBpb = do
    skip 4
    totalSectors <- getInt64le
    mftClusterNumber <- getInt64le
    mftMirrorClusterNumber <- getInt64le
    rawClustersPerMftRecord <- (fromIntegral :: (Int8 -> Integer)) <$> getInt8
    let clustersPerMftRecord = if rawClustersPerMftRecord >= 0
        then rawClustersPerMftRecord
        else 2 ^ abs rawClustersPerMftRecord
    skip 3
    rawClustersPerIndexBuffer <- (fromIntegral :: (Int8 -> Integer)) <$> getInt8
    let clustersPerIndexBuffer = if rawClustersPerIndexBuffer >= 0
        then rawClustersPerIndexBuffer
        else 2 ^ abs rawClustersPerIndexBuffer
    skip 3
    skip 8 -- volume serial number, ignored
    skip 4
    return ExtendedBpb {
        ebpbTotalSectors = totalSectors,
        ebpbMftLogicalClusterNumber = mftClusterNumber,
        ebpbMftMirrorLogicalClusterNumber = mftMirrorClusterNumber,
        ebpbClustersPerMftRecord = clustersPerMftRecord }
\end{code}
\begin{code}
data Mft = Mft deriving Show
\end{code}
\begin{code}
parseMft :: Integer -> Int64 -> Seeker Mft
parseMft clustersPerRecord logicalClusterNumber = do
    return Mft
\end{code}