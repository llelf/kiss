{-#language DataKinds,DeriveAnyClass,DeriveGeneric,DerivingStrategies,DuplicateRecordFields,GeneralizedNewtypeDeriving,TemplateHaskell#-}
module AST where
import AST.GenerateSyntax; import Language.Haskell.TH.Syntax(runIO); import qualified TS.K

runIO TS.K.getNodeTypesPath >>= astDeclarationsForLanguage TS.K.tree_sitter_k
