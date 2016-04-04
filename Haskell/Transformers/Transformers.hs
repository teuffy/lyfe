{-# LANGUAGE FlexibleContexts #-}
import System.IO
import Control.Monad.Reader
import Control.Monad.Trans.Reader (Reader)
import Control.Applicative
import System.Environment
import Control.Monad.State

data AppConfig = AppConfig {
  logfile :: FilePath,
  version :: String,
  maxMessageLength :: Int
} deriving (Show, Read)

initLogFile :: String -> AppConfig -> IO Handle
initLogFile preamble config = do
  handle <- openFile (logfile config) WriteMode
  hPutStrLn handle (preamble ++ ", version" ++ version config)
  return handle

validateMessage :: String -> AppConfig -> Either String ()
validateMessage msg config =
  if (length msg > maxMessageLength config)
  then Left ("Message too long: " ++ msg)
  else Right ()

type ConfigReader a = AppConfig -> a

initLogFileTS :: String -> ConfigReader(IO Handle)
initLogFileTS = initLogFile

validateMessageTS :: String -> ConfigReader(Either String ())
validateMessageTS = validateMessage

-- validate our prompt before using it to open the logfile
validateAndInitLogTS :: String -> ConfigReader (IO (Maybe Handle))
validateAndInitLogTS prompt config = 
  case validateMessage prompt config of
    Left err -> putStrLn("Invalid prompt: " ++ err) >> return Nothing
    Right () -> Just <$> initLogFile prompt config

--often in Haskell type synonym is a datatype waiting to be born.
newtype CReader a = CReader { runCR :: AppConfig -> a }

initLogFileCR :: String -> CReader (IO Handle)
initLogFileCR preamble = CReader $ \c -> initLogFile preamble c

validateMessageCR :: String -> CReader (Either String ())
validateMessageCR message = CReader $ \c -> validateMessage message c

validateAndInitLogCR :: String -> CReader(IO (Maybe Handle))
validateAndInitLogCR msg = CReader $ \c ->
  case runCR (validateMessageCR msg) c of
    Left err -> putStrLn "Invalid init message" >> return Nothing
    Right () -> Just <$> runCR (initLogFileCR msg) c

runCRWithConfig :: AppConfig -> IO Handle
runCRWithConfig config = do
  let result = runCR (validateAndInitLogCR "Hello CR") config
  mh <- result
  case mh of Nothing -> error "Log file init failed"
             Just h -> return h

instance Functor CReader where
  fmap f cr = CReader $ \c -> f (runCR cr c)

askConfig :: CReader AppConfig
askConfig = CReader id

validateMessageF :: String -> CReader(Either String())
validateMessageF m = fmap (validateMessage m) askConfig

initLogFileF :: String -> CReader (IO Handle)
initLogFileF p = fmap (initLogFile p) askConfig

validateAndInitLogF1 :: String -> CReader (IO (Maybe Handle))
validateAndInitLogF1 p = fmap doInit (validateMessageF p)
    where doInit :: Either String () -> (IO (Maybe Handle))
          doInit (Left err) = putStrLn ("Invalid prompt: " ++ p) >> return Nothing
	      -- no posibility to use InitLogFile F here with fmap

-- Introducing monads!
instance Monad CReader where
--return :: a -> CReader a
  return = CReader . const
-- >>= :: CReader a -> (a -> CReader b) -> CReader b
a >>= f = CReader $ \c -> runCR (f ((runCR a) c)) c
