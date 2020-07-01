{-#language DataKinds,DeriveAnyClass,DeriveGeneric,DerivingStrategies,DuplicateRecordFields,GeneralizedNewtypeDeriving,TemplateHaskell#-}
module AST where
import AST.GenerateSyntax; import Language.Haskell.TH.Syntax(runIO); import qualified TreeSitter.K

runIO TreeSitter.K.getNodeTypesPath >>= astDeclarationsForLanguage TreeSitter.K.tree_sitter_k
