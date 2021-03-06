import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import Network.HTTP.Parse
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Text.Show.Pretty

main :: IO ()
main = do
    tt <- sequence
        [ testGroup "requests" <$> testDirectory request "test/requests" ".req"
        , testGroup "responses" <$> testDirectory request "test/responses" ".resp"
        ]
    defaultMain $ testGroup "parsing" tt


testDirectory :: Show a => Parser a -> FilePath -> String -> IO [TestTree]
testDirectory parser dir ext = do
    files <- findByExtension [ext] dir
    let file f =
            let golden = f <.> "expected"
                out = f <.> "out"
                create = do
                    res <- parseOnly parser <$> BS.readFile f
                    writeBinaryFile out (ppShow res)
                -- TODO: Fix Show HttpVersion instance to produce
                -- valid Haskell
            in goldenVsFile f golden out create
    return $ map file files
