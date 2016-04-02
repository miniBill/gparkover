\begin{code}
{-# LANGUAGE NoImplicitPrelude #-}
module NTFS where

import ClassyPrelude hiding (decodeUtf8)

import Data.Bits
import Data.Int
import Data.Text.Encoding
import Data.Word
import Numeric (showHex)
import System.IO

import Seeker
import Utils
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
    ebpbBytesPerMftRecord :: Integer }
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
    bsBytesPerMftRecord :: Integer }
\end{code}
\begin{code}
bsBytesPerCluster :: BootSector -> Integer
bsBytesPerCluster b = toInteger (bsSectorsPerCluster b) * toInteger (bsBytesPerSector b)
\end{code}
\begin{code}
bsTotalSize :: BootSector -> Integer
bsTotalSize b = toInteger (bsBytesPerSector b) * toInteger (bsTotalSectors b)
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
        line "Bytes per MFT record" bsBytesPerMftRecord,
        line "Total size" (\b -> showSize (bsTotalSize b + 512)),
        "}"] where
        line :: Show a => String -> (BootSector -> a) -> String
        line h f = "\t" ++ h ++ ":" ++ replicate (24 - length h) ' ' ++ show (f b) ++ "\n"
\end{code}
\begin{code}
iterate :: (a -> a) -> a -> [a]
iterate f x =  x : iterate f (f x)
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
        bsBytesPerMftRecord = ebpbBytesPerMftRecord extendedBpb}

assertBytes :: String -> [Word8] -> Seeker ()
assertBytes err xs = do
    let ps = pack xs
    bs <- getByteString $ length ps
    if ps == bs
        then return ()
        else do
            addr <- getAddress
            trace ("Assert failed (" ++ err ++ "): got " ++ show bs ++
                " while expecting " ++ show ps ++ " at address " ++ showHex addr "") $
                return ()
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
    rawBytesPerMftRecord <- (fromIntegral :: (Int8 -> Integer)) <$> getInt8
    bytesPerMftRecord <- if rawBytesPerMftRecord >= 0
        then fail "Cluster number as a size of mft records is not supported"
        else return $ 2 ^ abs rawBytesPerMftRecord
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
        ebpbBytesPerMftRecord = bytesPerMftRecord }
\end{code}
\begin{code}
data Mft = Mft {
    mftRecords :: [MftRecord] }

instance Show Mft where
    show m = "Mft {\n" ++ concatMap (\r -> "\t" ++ show r ++ "\n") (mftRecords m) ++ "}"

data MftRecord = MftRecord {
    recAttributes :: [MftAttribute] }
    deriving Show

data MftAttribute = StandardInformation | Filename Text deriving Show
\end{code}
\begin{code}
parseMft :: Integer -> Seeker Mft
parseMft bytesPerRecord = do
    records <- forM [1,2] (const $ parseMftRecord bytesPerRecord)
    return $ Mft {
        mftRecords = records }
\end{code}
\begin{code}
parseMftRecord :: Integer -> Seeker MftRecord
parseMftRecord bytesPerRecord = do
    start <- getAddress
    assertBytes "FILE header" [0x46, 0x49, 0x4C, 0x45]
    
    seek AbsoluteSeek (start + 0x14)
    offsetToFirstAttr <- getInt16le
    
    seek AbsoluteSeek (start + toInteger offsetToFirstAttr)
    attributes <- forM [1,2] (const parseAttribute)
    
    seek AbsoluteSeek (start + bytesPerRecord)
    return $ MftRecord {
        recAttributes = attributes }

parseAttribute :: Seeker MftAttribute
parseAttribute = do
    start <- getAddress
    typeIdentifier <- getInt32le
    length <- getInt32le
    nonResident <- getInt8
    when (nonResident /= 0) $ fail "Non-resident attributes unsupported"
    result <- case typeIdentifier of
        0x10 -> do
            return StandardInformation
        0x30 -> do
            seek AbsoluteSeek (start + 0x10)
            sizeOfContent <- getInt64le
            
            seek AbsoluteSeek (start + 0x14)
            offsetToContent <- getInt16le
            
            seek AbsoluteSeek (start + toInteger offsetToContent)
            
            skip 0x40
            filenameLength <- getInt8
            skip 1
            filename <- getByteString $ fromIntegral filenameLength * 2
            return $ Filename (decodeUtf16LE filename)
        _ -> fail $ "Unknown attribute"
    seek AbsoluteSeek (start + toInteger length)
    return result
\end{code}