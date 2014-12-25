module Lang.Hawk.Options (HawkConfig(..), HawkProg(..), parseHawkArgs) where

import Options.Applicative

data HawkProg = HawkString String
              | HawkFile String
                deriving (Show)

data HawkConfig = HawkConfig
                { awkFS      :: Maybe String
                , awkProgram :: HawkProg
                , awkVars    :: [String]
                , awkFiles   :: [String]
                }
                deriving (Show)

hawkConfig :: Parser HawkConfig
hawkConfig = HawkConfig <$> hawkFS <*> hawkProg <*> hawkVars <*> hawkInput

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

hawkVars :: Parser [String]
hawkVars = many $ strOption
            (short 'v'
             <> metavar "var=value"
             <> help "Set AWK variable 'var' to 'value'")


hawkInput :: Parser [String]
hawkInput = arguments str
            ( metavar "file ..."
            <> help "Input file(s) to process")

parseHawkArgs :: IO HawkConfig
parseHawkArgs = execParser $ info (helper <*> hawkConfig) fullDesc
