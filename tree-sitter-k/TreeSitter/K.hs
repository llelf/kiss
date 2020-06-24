{-# LANGUAGE ForeignFunctionInterface #-}
module TreeSitter.K where
import TreeSitter.Language; import Foreign.Ptr; import Paths_tree_sitter_k
foreign import ccall unsafe "h/src/parser.c tree_sitter_k" tree_sitter_k::Ptr Language
getNodeTypesPath::IO FilePath; getNodeTypesPath=getDataFileName"h/src/node-types.json"
getTestCorpusDir::IO FilePath; getTestCorpusDir=getDataFileName"corpus"
