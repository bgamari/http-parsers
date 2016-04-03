import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import System.FilePath.Glob
import HTTP.Parse
import System.FilePath

testParse :: Show a => Parser a -> FilePath -> IO ()
testParse parser file = do
    mr <- parseOnly parser <$> BS.readFile file
    -- e <- readFile (file <.> "expected")
    case mr of
        Left err -> fail $ "Error parsing "++file++": "++err
        Right r
          --- | show r /= e -> fail "Test output mismatch"
          | otherwise   -> return ()

testRequest :: FilePath -> IO ()
testRequest = testParse request

main :: IO ()
main = do
    glob "requests/*.req" >>= mapM_ testRequest
