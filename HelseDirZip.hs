module HelseDirZip (helsedirzip, multilabels, 
    Labels(..), PostLabel(..), DiskLabel(..),
    PostLabelMulti(..), PostLabelStub(..)
    ) where

import Codec.Archive.Zip
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (isPrefixOf, sort, sortBy, intercalate)
import qualified Crypto.Hash.MD5 as MD5


data Labels = Labels {
  num  :: Int,          -- ID
  post :: PostLabel,
  disk :: DiskLabel,
  cont :: BL.ByteString -- filinnhold
}

data PostLabel = PostLabel {
  plName :: Text,
  plStub :: PostLabelStub
}

data PostLabelMulti = PostLabelMulti {
  pmDoc :: [(Int,Text)],
  pmStub :: PostLabelStub
} deriving (Eq)

data PostLabelStub = PostLabelStub {
  psUnit :: Text,
  psAdr  :: Text,
  psPostNo :: Text,
  psPostAdr :: Text,
  psMD5 :: ByteString -- hash of: unit adr postno postadr
} deriving (Eq)

data DiskLabel = DiskLabel {
  dlName :: Text,
  dlPeriod :: Text
}

tLF   = T.pack "\n"
tCRLF = T.pack "\r\n"
tSP   = T.pack " "

instance Ord Labels where
  (Labels n1 _ _ _) `compare` (Labels n2 _ _ _) = n1 `compare` n2
 
instance Eq Labels where
  (Labels n1 _ _ _) == (Labels n2 _ _ _) = n1 == n2

instance Show PostLabel where
    show p   = (T.unpack (T.concat [plName p, tCRLF])) ++ show (plStub p)
   
instance Show PostLabelMulti where
    show p   = intercalate "\r\n" (map (\(i,n) -> show i ++ " " ++ T.unpack n) (pmDoc p)) ++ show (pmStub p)

instance Show PostLabelStub where
    show p   = T.unpack $ T.concat [psUnit p, tLF, psAdr p, tLF, psPostNo p, tSP, psPostAdr p, tLF]

instance Show DiskLabel where
    show p   = T.unpack $ T.concat [dlName p, tLF, dlPeriod p, tLF]

instance Show Labels where
    show p   = T.unpack (T.concat [T.pack "ID ", T.pack (show (num p)), tCRLF]) ++ show (disk p) ++ show (post p)


helsedirzip :: Bool -> BL.ByteString -> [Labels]
helsedirzip test zipfile = sort $ Map.elems $ Map.mapWithKey (merge disk cont) valids
  where arch = toArchive zipfile
        post = Map.fromList $ map entry2Post $ filter (\e -> "ETIK" `isPrefixOf` (eRelativePath e)) (zEntries arch)
        disk = Map.fromList $ map entry2Disk $ filter (\e -> "LABE" `isPrefixOf` (eRelativePath e)) (zEntries arch)
        cont
          | test = Map.fromList $ map (\(i,_) -> (i,BL.empty)) (Map.toList disk)
          | otherwise = Map.fromList $ map entry2Data $ filter (\e -> "LEGE" `isPrefixOf` (eRelativePath e)) (zEntries arch)
        valids = Map.filterWithKey (\k _ -> Map.member k disk && Map.member k cont) post


multilabels :: [Labels] -> [PostLabelMulti]
multilabels l = sortBy (\n1 n2 -> fst (head (pmDoc n1)) `compare` fst (head (pmDoc n2))) $ map (\(n,s) -> PostLabelMulti n s) $ Map.elems tree
  where tree = foldl (addit) Map.empty l :: Map ByteString ([(Int,Text)],PostLabelStub)
        addit tree la = Map.insertWith (addone) (md5 la) (newval la) tree
        md5 la = psMD5 (plStub (post la))
        newval la = ([(num la,(plName (post la)))],(plStub (post la)))
        addone (nu,_) (nu0,adr0) = (nu0++nu,adr0) :: ([(Int,Text)],PostLabelStub) -- siden labels er sortert på ID så kommer legene i stigende rekkefølge uten eksplisitt sortering


-- Setter sammen de tre datakildene ETIK, LABE, LEGE til en struktur
merge :: Map Int DiskLabel -> Map Int BL.ByteString -> Int -> PostLabel -> Labels
merge disk cont key post = Labels key post ((Map.!) disk key) ((Map.!) cont key)

entry2Post :: Entry -> (Int,PostLabel)
entry2Post entry = (num entry, toPostLabel (fromEntry entry))
  where num ent = read $ take 4 (drop 4 (eRelativePath ent)) :: Int
  
entry2Disk :: Entry -> (Int,DiskLabel)
entry2Disk entry = (num entry, toDiskLabel (fromEntry entry))
  where num ent = read $ take 4 (drop 4 (eRelativePath ent)) :: Int

entry2Data :: Entry -> (Int,BL.ByteString)
entry2Data entry = (num entry, fromEntry entry)
  where num ent = read $ take 4 (drop 4 (eRelativePath ent)) :: Int

toPostLabel :: BL.ByteString -> PostLabel
toPostLabel bs = PostLabel name (PostLabelStub comp adr1 postno postst md5)
  where str = T.pack $ map (\d -> (V.!) codec (fromIntegral d)) (BL.unpack bs)
        name = T.strip $ T.take 26 str
        comp = T.strip $ T.take 30 (T.drop 26 str)
        adr1 = T.strip $ T.take 30 (T.drop (26+30) str)
        postno = T.strip $ T.take 30 (T.drop (26+30+30) str)
        postst = T.strip $ T.take 26 (T.drop (26+30+30+30) str)
        md5 = MD5.hash $ B.concat (BL.toChunks (BL.drop 26 bs))

toDiskLabel :: BL.ByteString -> DiskLabel
toDiskLabel bs = DiskLabel name period
  where str = T.pack $ map (\d -> (V.!) codec (fromIntegral d)) (BL.unpack bs)
        name = T.strip $ T.take 26 str
        period = T.take 16 (T.drop 26 str)

-- Norsk (eldre) extended ASCII codec (f.eks med Ø som Yen-symbol)
codec :: Vector Char
codec = V.fromList ("         \t\n  \r                   !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~ CüéâäàåcêëèïîìÄÅÉæÆôöòûùÿÖÜc£Ø  áíóúñÑ" ++ (replicate 100 ' '))
