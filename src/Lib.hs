{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.Text as T
import Git.CmdLine
import Git.Types
import Shelly
import System.Directory

data BoshConnection = BoshConnection { boshHost :: Text, bosPort :: Int, boshCACert :: Text, boshClient :: Text, boshClientSecret :: Text }

deployBosh :: IO BoshConnection
deployBosh = do
  repo <- cloneBoshDeployment
  undefined

boshDeploymentRepoOptions = defaultRepositoryOptions { repoPath = "/tmp/bosh-deployment-repo"
                                                     , repoWorkingDir = Just "/tmp/bosh-deployment-wd" }

cloneBoshDeployment :: IO CliRepo
cloneBoshDeployment = cloneRepository "/tmp/bosh-deployment-dest" boshDeploymentRepoOptions "https://github.com/cloudfoundry/bosh-deployment"

execGit :: CliRepo -> [Text] -> IO Text
execGit repo args = do
  shelly $ print_stdout False $ errExit True $ run "git" $ gitStdOpts repo ++ args

cloneRepository :: Text -> RepositoryOptions -> Text -> IO CliRepo
cloneRepository destination repoOptions url =  do
  _ <- deleteStuff destination repoOptions
  repo <- openCliRepository repoOptions
  _ <- execGit repo [ "clone"
                    , "--separate-git-dir", T.pack (repoPath repoOptions)
                    , url, destination]
  return repo

deleteStuff :: Text -> RepositoryOptions -> IO ()
deleteStuff dest repoOptions = do
  _ <- removePathForcibly (T.unpack dest)
  _ <- removePathForcibly (repoPath repoOptions)
  maybe (return ()) removePathForcibly (repoWorkingDir repoOptions)
