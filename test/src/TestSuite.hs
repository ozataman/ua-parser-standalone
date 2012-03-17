{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Criterion.Main                 as C
import           Data.Aeson
import           Data.ByteString.Char8          (ByteString)
import           Data.DeriveTH
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Yaml
import           System.Environment
import           Test.Framework                 (defaultMain, testGroup)
import           Test.Framework                 as T
import           Test.Framework.Providers.HUnit as T
import           Test.HUnit                     hiding (Test, path)
import           Text.Regex.PCRE.Light
-------------------------------------------------------------------------------
import           Web.UAParser.Core
-------------------------------------------------------------------------------



$(derives [makeNFData] [''UAResult])




main = benchMain
  -- arg <- getArgs
  -- case arg of
  --   ["bench"] -> benchMain
  --   _ -> testMain



                              ------------------
                              -- Benchmarking --
                              ------------------


-------------------------------------------------------------------------------
benchMain = do
  conf <- loadConfig "../resources/user_agent_parser.yaml"
  cases <- loadTests "resources/test_user_agent_parser.yaml"
  cases2 <- loadTests "resources/firefox_user_agent_strings.yaml"
  let allC = cases ++ cases2
      ua = bench "UA Parsing" $ nf (map (parseUA conf . uatcString)) allC
  print $ show (length allC) ++ " strings being parsed."
  C.defaultMain [ua]
        


                                 -------------
                                 -- Testing --
                                 -------------

          
testMain = T.defaultMain tests
    where 
      tests = 
        [ uaTests 
        , osTests ]
        

                               ----------------
                               -- UA Testing --
                               ----------------


-------------------------------------------------------------------------------
uaTests = buildTest $ do
  conf <- loadConfig "../resources/user_agent_parser.yaml"
  cases <- loadTests "resources/test_user_agent_parser.yaml"
  cases2 <- loadTests "resources/firefox_user_agent_strings.yaml"
  let allC = cases ++ cases2
  return $ testGroup "UA Parsing Tests" $ map (testUAParser conf) allC
  


-------------------------------------------------------------------------------
testUAParser :: UAConfig -> UserAgentTestCase -> Test
testUAParser config UATC{..} = testCase tn $ do
  case parsed of
    Nothing -> assertFailure "Can't produce UAResult"
    Just UAResult{..} -> do
     assertEqual "family is same" uatcFamily uarFamily
     -- assertEqual "v1 is the same" uatcV1 uarV1
     -- assertEqual "v2 is the same" uatcV2 uarV2
     -- assertEqual "v3 is the same" uatcV3 uarV3
  where 
    parsed = parseUA config uatcString
    tn = T.unpack $ T.intercalate "/" ["UA Test: ", uatcFamily, m uatcV1, m uatcV2, m uatcV3]
    m x = maybe "-" id x


                               ----------------
                               -- OS Testing --
                               ----------------



-------------------------------------------------------------------------------
osTests = buildTest $ do
  conf <- loadConfig "../resources/user_agent_parser.yaml"
  cases <- loadTests "resources/test_user_agent_parser_os.yaml"
  return $ testGroup "OS Parsing Tests" $ map (testOSParser conf) cases
  


-------------------------------------------------------------------------------
testOSParser :: UAConfig -> OSTestCase -> Test
testOSParser config OSTC{..} = testCase tn $ do
  case parsed of
    Nothing -> assertFailure "Can't produce OSResult"
    Just OSResult{..} -> do
     assertEqual "family is same" ostcFamily osrFamily
     -- assertEqual "v1 is the same" uatcV1 uarV1
     -- assertEqual "v2 is the same" uatcV2 uarV2
     -- assertEqual "v3 is the same" uatcV3 uarV3
  where 
    parsed = parseOS config ostcString
    tn = T.unpack $ T.intercalate "/" 
         ["OS Test: ", ostcFamily, m ostcV1, m ostcV2, m ostcV3, m ostcV4]
    m x = maybe "-" id x





-------------------------------------------------------------------------------
loadTests :: FromJSON a => FilePath -> IO a
loadTests fp = do
  xs <- decodeFile fp
  case xs of
    Nothing -> error "Can't load test file"
    Just xs' -> return xs'
  

-------------------------------------------------------------------------------
data UserAgentTestCase = UATC {
      uatcString :: ByteString
    , uatcFamily :: Text
    , uatcV1 :: Maybe Text
    , uatcV2 :: Maybe Text
    , uatcV3 :: Maybe Text
    } deriving (Show)
    

-------------------------------------------------------------------------------
instance FromJSON UserAgentTestCase where
    parseJSON (Object v) = 
      UATC <$> v .: "user_agent_string"
           <*> (v .: "family" <|> return "")
           <*> (v .:? "v1" <|> return Nothing)
           <*> (v .:? "v2" <|> return Nothing)
           <*> (v .:? "v3" <|> return Nothing)


-------------------------------------------------------------------------------
data OSTestCase = OSTC {
      ostcString :: ByteString
    , ostcFamily :: Text
    , ostcV1 :: Maybe Text
    , ostcV2 :: Maybe Text
    , ostcV3 :: Maybe Text
    , ostcV4 :: Maybe Text
    } deriving (Show)


-------------------------------------------------------------------------------
instance FromJSON OSTestCase where
    parseJSON (Object v) = 
      OSTC <$> (v .: "user_agent_string" <|> return "")
           <*> (v .: "os" <|> return "")
           <*> (v .:? "os_v1" <|> return Nothing)
           <*> (v .:? "os_v2" <|> return Nothing)
           <*> (v .:? "os_v3" <|> return Nothing)
           <*> (v .:? "os_v4" <|> return Nothing)
      
    
