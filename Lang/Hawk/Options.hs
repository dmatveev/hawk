module Lang.Hawk.Options (HawkConfig(..), HawkProg(..), parseHawkArgs) where

import qualified Text.Parsec as P
import Options.Applicative

import Lang.Hawk.Value
import qualified Lang.Hawk.AST as A (Expression(..))
import Lang.Hawk.Grammar (setVar)

data HawkProg = HawkString String
              | HawkFile String
                deriving (Show)

data HawkConfig = HawkConfig
                { awkFS      :: Maybe String
                , awkProgram :: HawkProg
                , awkVars    :: [HawkVar]
                , awkFiles   :: [String]
                , awkDebug   :: Bool
                }
                deriving (Show)

type HawkVar = (A.Expression, Value)

varReader :: Monad m => String -> m HawkVar 
varReader s = case P.parse setVar "" s of
   Left e                  -> fail $ show e
   Right (p,(A.Const lit)) -> return (p, toValue lit)

hawkConfig :: Parser HawkConfig
hawkConfig = HawkConfig <$> hawkFS <*> hawkProg <*> hawkVars <*> hawkInput <*> hawkDbg

hawkFS :: Parser (Maybe String)
hawkFS = optional $ strOption
              (short 'F'
              <> metavar "fs"
              <> help "Set default field separator (FS)")

hawkProg :: Parser HawkProg
hawkProg = (HawkString <$> argument str
              (metavar "'prog'"
               <> help "AWK program source string"))
       <|> (HawkFile <$> strOption
              (short 'f'
              <> metavar "progfile"
              <> help "AWK program source file"))

hawkVars :: Parser [HawkVar]
hawkVars = many $ option (str >>= varReader)
            (short 'v'
             <> metavar "var=value"
             <> help "Set AWK variable 'var' to 'value'")


hawkInput :: Parser [String]
hawkInput = many (argument str
            ( metavar "file ..."
            <> help "Input file(s) to process"))

hawkDbg :: Parser Bool
hawkDbg = switch
            (long "debug"
            <> help "Dump Hawk compiled \"bytecode\" without executing")

parseHawkArgs :: IO HawkConfig
parseHawkArgs = execParser $ info (helper <*> hawkConfig) fullDesc
