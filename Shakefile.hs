import Control.Monad
import Data.Foldable
import Development.Shake
import Development.Shake.FilePath
import System.Directory


dockerImage :: String
dockerImage = "runtimeverificationinc/kframework-k:ubuntu-bionic-master"


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


main :: IO ()
main = do
  pwd <- getCurrentDirectory
  let dockerFlags = ["--workdir=/root", "--mount", "type=bind,source=" <> pwd </> "src,target=/root"]
  let dockerCmd cmd = command [] "docker" $ ["run"] <> dockerFlags <> [dockerImage] <> cmd
  let dockerCmd_ = void . dockerCmd_

  rulesFiles <- getDirectoryFilesIO "" [rulesFolderToRulesFile "src//"]
  let rulesFolders = map rulesFileToRulesFolder rulesFiles
  let rulesProofs = map rulesFolderToRulesProof rulesFolders

  expectedFiles <- getDirectoryFilesIO "" ["src//*.expected"]
  let passedProofs = map expectedFileToPassedProof expectedFiles
  let actualFiles = map passedProofToActualFile passedProofs

  shakeArgs shakeOptions $ do
    want ["tests"]

    phony "repl" $ do
      -- '-it' commands must be run from a real terminal
      putInfo "Run the following from your terminal:"
      putInfo $ unwords $ ["docker", "run"] <> dockerFlags <> ["-it", dockerImage, "bash"]


    phony "rules" $ do
      need rulesProofs

    for_ rulesFolders $ \rulesFolder -> do
      phony (dockerizePath rulesFolder) $ do
        need [rulesFolderToRulesProof rulesFolder]

    rulesFolderToRulesProof "src//" %> \rulesProof -> do
      let rulesFolder = rulesProofToRulesFolder rulesProof
      let rulesFile = rulesFolderToRulesFile rulesFolder
      need [rulesFile]
      dockerCmd_ ["kompile", "--backend", "java", dockerizePath rulesFile]

    "src//*.actual" %> \actualResult -> do
      let exampleFile = actualResultToExampleFile actualResult
      let rulesFolder = actualResultToRulesFolder actualResult
      let rulesProof = rulesFolderToRulesProof rulesFolder
      need [rulesProof]
      Stdout out <- dockerCmd ["krun", "--directory", dockerizePath rulesFolder, dockerizePath exampleFile]
      liftIO $ writeFile actualResult out


    phony "tests" $ do
      need passedProofs
      putInfo "*** TESTS PASSED ***"

    for_ actualFiles $ \actualFile -> do
      phony (dropExtensions . dockerizePath $ actualFile) $ do
        need [actualFile]
        cmd_ "cat" actualFile

    "src//*.passed" %> \passedProof -> do
      let expectedFile = passedProofToExpectedFile passedProof
      let actualFile = passedProofToActualFile passedProof
      need [expectedFile, actualFile]
      command_ [] "diff" ["-urN", expectedFile, actualFile]
      liftIO $ writeFile passedProof ""


    phony "clean" $ do
        liftIO $ removeFiles "src" ["//*.actual", "//*.passed"]

    phony "clobber" $ do
      need ["clean"]
      liftIO $ removeFiles "src" ["//lambda-kompiled"]
