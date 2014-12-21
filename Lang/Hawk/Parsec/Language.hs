-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Language
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (uses non-portable module Text.Parsec.Token)
--
-- A helper module that defines some language definitions that can be used
-- to instantiate a token parser (see "Text.Parsec.Token").
-- 
-- Added here to fix type checking errors with our modified Token module,
-- removed unnecessary language declarations.
-- 21/12/2014, Dmitry Matveev <me@dmitrymatveev.co.uk>
-----------------------------------------------------------------------------

module Lang.Hawk.Parsec.Language
    ( emptyDef
    , LanguageDef
    , GenLanguageDef
    ) where

import Text.Parsec
import Lang.Hawk.Parsec.Token

-- TODO: This seems wrong
-- < This is the most minimal token definition. It is recommended to use
-- this definition as the basis for other definitions. @emptyDef@ has
-- no reserved names or operators, is case sensitive and doesn't accept
-- comments, identifiers or operators.

emptyDef   :: LanguageDef st
emptyDef    = LanguageDef
               { commentStart   = ""
               , commentEnd     = ""
               , commentLine    = ""
               , nestedComments = True
               , identStart     = letter <|> char '_'
               , identLetter    = alphaNum <|> oneOf "_'"
               , opStart        = opLetter emptyDef
               , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , reservedOpNames= []
               , reservedNames  = []
               , caseSensitive  = True
               }
