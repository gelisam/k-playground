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
-- >>> dockerizePath "src/foo/bar/baz.txt"
-- "foo/bar/baz.txt"
dockerizePath :: FilePath -> FilePath
dockerizePath = dropDirectory1


main :: IO ()
main = do
  pwd <- getCurrentDirectory
  let dockerFlags = ["--workdir=/root", "--mount", "type=bind,source=" <> pwd </> "src,target=/root"]
  let dockerCmd_ cmd = command_ [] "docker" $ ["run"] <> dockerFlags <> [dockerImage] <> cmd

  rulesFiles <- getDirectoryFilesIO "" [rulesFolderToRulesFile "src//"]
  let rulesFolders = map rulesFileToRulesFolder rulesFiles
  let rulesProofs = map rulesFolderToRulesProof rulesFolders

  shakeArgs shakeOptions $ do
    want ["rules"]

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
