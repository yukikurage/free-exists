module SrcTree where

import Prelude

import Data.Array (uncons)
import Data.Map (Map)
import Data.Set (Set)

newtype FileMeta = FileMeta
  { fileName :: String
  }

newtype DirMeta = DirMeta
  { dirName :: String
  , dirFiles :: Map String FileMeta
  , dirDirs :: Map String DirMeta
  }

data SrcTree = SrcTree
  { srcTreeFiles :: Map String FileMeta
  , srcTreeDirs :: Map String DirMeta
  }
