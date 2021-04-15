import Control.Monad
import Data.Foldable
import Development.Shake
import Development.Shake.FilePath
import System.Console.GetOpt
import System.Directory


------------
-- CONFIG --
------------

dockerImage :: String
dockerImage = "runtimeverificationinc/kframework-k:ubuntu-focal-5.0.21"


-----------
-- FLAGS --
-----------

data Flags
  = Depth String  -- ^ should be an Int, but we're only passing it as-is anyway
  deriving Show
flags = [Option "" ["depth"] (ReqArg (Right . Depth) "N") "Stop after this number of rewrite steps."]


-----------------------
-- PATH TRANSFORMERS --
-----------------------

-- |
-- >>> rulesFolderToRulesProof "src/operational-semantics/small-steps"
-- "src/operational-semantics/small-steps/lambda-kompiled/timestamp"
rulesFolderToRulesProof :: FilePath -> FilePath
rulesFolderToRulesProof rulesFolder = rulesFolder </> "lambda-kompiled" </> "timestamp"

-- |
-- >>> rulesProofToRulesFolder "src/operational-semantics/small-steps/lambda-kompiled/timestamp"
-- "src/operational-semantics/small-steps"
rulesProofToRulesFolder :: FilePath -> FilePath
rulesProofToRulesFolder = takeDirectory . takeDirectory

-- |
-- >>> rulesFolderToRulesFile "src/operational-semantics/small-steps"
-- "src/operational-semantics/small-steps/lambda.k"
rulesFolderToRulesFile :: FilePath -> FilePath
rulesFolderToRulesFile rulesFolder = rulesFolder </> "lambda.k"

-- |
-- >>> rulesFileToRulesFolder "src/operational-semantics/small-steps/lambda.k"
-- "src/operational-semantics/small-steps"
rulesFileToRulesFolder :: FilePath -> FilePath
rulesFileToRulesFolder = takeDirectory

-- |
-- >>> actualResultToRulesFolder "src/operational-semantics/small-steps/foo.actual"
-- "src/operational-semantics/small-steps"
actualResultToRulesFolder :: FilePath -> FilePath
actualResultToRulesFolder = takeDirectory

-- |
-- >>> actualResultToExampleFile "src/operational-semantics/small-steps/foo.actual"
-- "src/examples/foo.lambda"
actualResultToExampleFile :: FilePath -> FilePath
actualResultToExampleFile actualFile = "src" </> "examples" </> takeBaseName actualFile <.> "lambda"

-- |
-- >>> actualResultToDepthFile "src/operational-semantics/small-steps/foo.actual"
-- "src/examples/foo.depth"
actualResultToDepthFile :: FilePath -> FilePath
actualResultToDepthFile actualFile = actualFile -<.> "depth"

-- |
-- >>> passedProofToExpectedFile "src/operational-semantics/small-steps/foo.passed"
-- "src/operational-semantics/small-steps/foo.actual"
passedProofToActualFile :: FilePath -> FilePath
passedProofToActualFile passedFile = passedFile -<.> "actual"

-- |
-- >>> passedProofToExpectedFile "src/operational-semantics/small-steps/foo.passed"
-- "src/operational-semantics/small-steps/foo.expected"
passedProofToExpectedFile :: FilePath -> FilePath
passedProofToExpectedFile passedFile = passedFile -<.> "expected"

-- |
-- >>> passedProofToExpectedFile "src/operational-semantics/small-steps/foo.expected"
-- "src/operational-semantics/small-steps/foo.passed"
expectedFileToPassedProof :: FilePath -> FilePath
expectedFileToPassedProof passedFile = passedFile -<.> "passed"

-- |
-- >>> dockerizePath "src/foo/bar/baz.txt"
-- "foo/bar/baz.txt"
dockerizePath :: FilePath -> FilePath
dockerizePath = dropDirectory1


-----------
-- UTILS --
-----------

readSingleLineFile :: FilePath -> IO String
readSingleLineFile filePath = do
  [x] <- lines <$> readFile filePath
  pure x


----------
-- MAIN --
----------

main :: IO ()
main = do
  -------------
  -- ACTIONS --
  -------------

  pwd <- getCurrentDirectory
  let dockerFlags = ["--workdir=/root", "--mount", "type=bind,source=" <> pwd </> "src,target=/root"]
  let dockerCmd cmd = command [] "docker" $ ["run"] <> dockerFlags <> [dockerImage] <> cmd
  let dockerCmd_ cmd = command_ [] "docker" $ ["run"] <> dockerFlags <> [dockerImage] <> cmd

  let krun rulesFolder exampleFile maxDepth = do
        let rulesProof = rulesFolderToRulesProof rulesFolder
        need [rulesProof, exampleFile]
        Stdout out <- dockerCmd [ "krun"
                                , "--directory", dockerizePath rulesFolder
                                , "--depth", maxDepth
                                , dockerizePath exampleFile
                                ]
        pure out


  -------------
  -- TARGETS --
  -------------

  rulesFiles <- getDirectoryFilesIO "" [rulesFolderToRulesFile "src//"]
  let rulesFolders = map rulesFileToRulesFolder rulesFiles
  let rulesProofs = map rulesFolderToRulesProof rulesFolders
  let syntaxFile = "src/syntax/lambda-syntax.k"

  expectedFiles <- getDirectoryFilesIO "" ["src//*.expected"]
  let passedProofs = map expectedFileToPassedProof expectedFiles
  let actualFiles = map passedProofToActualFile passedProofs


  ------------------
  -- SHAKE CONFIG --
  ------------------

  version <- getHashedShakeVersion ["Shakefile.hs"]
  shakeArgsWith shakeOptions {shakeVersion = version} flags $ \flags targets -> pure $ Just $ do
    let defaultTarget = "tests"
    want $ if null targets then [defaultTarget] else targets


    ------------------
    -- MISC TARGETS --
    ------------------

    phony "repl" $ do
      -- '-it' commands must be run from a real terminal
      putInfo "Run the following from your terminal:"
      putInfo $ unwords $ ["docker", "run"] <> dockerFlags <> ["-it", dockerImage, "bash"]


    -------------------
    -- BUILD TARGETS --
    -------------------

    phony "rules" $ do
      need rulesProofs

    for_ rulesFolders $ \rulesFolder -> do
      phony (dockerizePath rulesFolder) $ do
        need [rulesFolderToRulesProof rulesFolder]

    rulesFolderToRulesProof "src//" %> \rulesProof -> do
      let rulesFolder = rulesProofToRulesFolder rulesProof
      let rulesFile = rulesFolderToRulesFile rulesFolder
      need [syntaxFile, rulesFile]
      dockerCmd_ ["kompile", "--backend", "llvm", dockerizePath rulesFile]

    for_ actualFiles $ \actualResult -> do
      phony (dropExtensions . dockerizePath $ actualResult) $ do
        let exampleFile = actualResultToExampleFile actualResult
        let rulesFolder = actualResultToRulesFolder actualResult
        case flags of
          [Depth maxDepth] -> do
            out <- krun rulesFolder exampleFile maxDepth
            liftIO $ putStr out
          [] -> do
            need [actualResult]
            cmd_ "cat" actualResult
          _ -> error $ "unexpected flag combination: " <> show flags

    "src//*.actual" %> \actualResult -> do
      let exampleFile = actualResultToExampleFile actualResult
      let rulesFolder = actualResultToRulesFolder actualResult
      let depthFile = actualResultToDepthFile actualResult
      need [depthFile]
      maxDepth <- liftIO $ readSingleLineFile depthFile
      out <- krun rulesFolder exampleFile maxDepth
      liftIO $ writeFile actualResult out


    ------------------
    -- TEST TARGETS --
    ------------------

    phony "tests" $ do
      need passedProofs
      putInfo "*** TESTS PASSED ***"

    "src//*.passed" %> \passedProof -> do
      let expectedFile = passedProofToExpectedFile passedProof
      let actualFile = passedProofToActualFile passedProof
      need [expectedFile, actualFile]
      command_ [] "diff" ["-urN", expectedFile, actualFile]
      liftIO $ writeFile passedProof ""


    ---------------------
    -- CLEANUP TARGETS --
    ---------------------

    phony "clean" $ do
        liftIO $ removeFiles "src" ["//*.actual", "//*.passed"]

    phony "clobber" $ do
      need ["clean"]
      liftIO $ removeFiles "src" ["//lambda-kompiled"]
