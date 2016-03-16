import Control.Monad.Writer
import Data.List (sort)

sortAndLogEntry :: String -> Writer [(String, String)] String  
sortAndLogEntry e = writer (sort e, [(e, sort e)])  
  
addEntries :: [String] -> Writer [(String, String)] String  
addEntries [] = error "emtpy list!" 
addEntries [x] =  do
    e <- sortAndLogEntry x
    return (e) 
addEntries (x : xs) =  do
    _ <- sortAndLogEntry x
    e <- addEntries xs
    return (e)

sampleRun :: (String, [(String, String)])
sampleRun = runWriter $ addEntries ["test1", "test2", "test3"] 
