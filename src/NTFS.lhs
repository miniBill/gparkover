\begin{code}
{-# LANGUAGE NoImplicitPrelude #-}
module NTFS where

import ClassyPrelude hiding (decodeUtf8)

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get hiding (getBytes)
import Data.Bits
import Data.Int
import Data.Text.Encoding
import Data.Word

import Data.Binary.Get.Extra
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
    bsBpb :: Bpb,
    bsExtendedBpb :: ExtendedBpb }
\end{code}
\begin{code}
instance Show BootSector where
    show b =
        "BootSector {\n" ++
        "\tOEM ID: " ++ unpack (bsOemId b) ++ "\n" ++
        "\tBPB: " ++ show (bsBpb b) ++ "\n" ++
        "\tExtended BPB: " ++ show (bsExtendedBpb b) ++ "\n" ++
        "\tTotal size: " ++ showSize totalSize ++ " + 512 B for the boot sector\n" ++
        "}" where
        totalSize = bytesPerSector b * totalSectors b        
        bytesPerSector = toInteger . bpbBytesPerSector . bsBpb
        totalSectors   = toInteger . ebpbTotalSectors  . bsExtendedBpb
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
parseBootSector :: Get BootSector
parseBootSector = do
    skip 3 -- jump instruction, ignored
    oemId <- decodeUtf8 <$> getByteString 8
    bpb <- parseBpb
    extendedBpb <- parseExtendedBpb
    skip 426 -- bootstrap code, ignored
    assertBytes [0x55, 0xAA] -- signature
    return $ BootSector {
        bsOemId = oemId,
        bsBpb = bpb,
        bsExtendedBpb = extendedBpb }

assertBytes :: [Word8] -> Get ()
assertBytes xs = do
    let ps = pack xs
    bs <- getByteString $ length ps
    if ps == bs
        then return ()
        else fail $
            "Assert failed: got " ++ show bs ++
            " while expecting " ++ show ps
\end{code}
\begin{code}
parseBpb :: Get Bpb
parseBpb = do
    bytesPerSector <- getInt16le
    sectorsPerCluster <- getInt8
    assertZeroes 2 -- reserved sectors, must be zero
    assertZeroes 5
    assertBytes [0xF8] -- media descriptor, F8 is HDD
    assertZeroes 2
    skip 8
    assertZeroes 4
    return $ Bpb {
        bpbBytesPerSector = bytesPerSector,
        bpbSectorsPerCluster = sectorsPerCluster }
\end{code}
\begin{code}
assertZeroes :: Int -> Get ()
assertZeroes n = assertBytes $ replicate n 0
\end{code}
\begin{code}
parseExtendedBpb :: Get ExtendedBpb
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
parseMft :: Get Mft
parseMft = return Mft
\end{code}