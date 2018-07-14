{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.Maybe
import Data.Text as T
import Git.CmdLine
import Git.Types
import Shelly
import System.Directory

data BoshConnection = BoshConnection { boshHost :: Text, bosPort :: Int, boshCACert :: Text, boshClient :: Text, boshClientSecret :: Text }

deployBosh :: IO BoshConnection
deployBosh = do
  repo <- cloneBoshDeployment
  _ <- installBosh $ fromMaybe (cliRepoPath repo) (cliWorkingDir repo)
  undefined

boshDeploymentRepoOptions = defaultRepositoryOptions { repoPath = "/tmp/bosh-deployment-repo"
                                                     , repoWorkingDir = Just "/tmp/bosh-deployment-wd" }

cloneBoshDeployment :: IO CliRepo
cloneBoshDeployment = cloneRepository "/tmp/bosh-deployment-dest" boshDeploymentRepoOptions "https://github.com/cloudfoundry/bosh-deployment"

execGit :: CliRepo -> [Text] -> IO Text
execGit repo args =
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
  mapM_ removePathForcibly (repoWorkingDir repoOptions)


installBosh :: Text -> IO (Text, BoshConnection)
installBosh boshDeploymentPath = do
  home <- getHomeDirectory
  let envDir = T.pack home `T.append` "/.know"
  _ <- shelly $ verbosely $ errExit True $
    run_ "bosh" $ "create-env" : createEnvArgs envDir  boshDeploymentPath
  undefined

createEnvArgs :: Text -> Text -> [Text]
createEnvArgs envDir boshDeploymentPath = ymlPath "bosh.yml"
                                         : "-n"
                                         : ("--state=" `T.append` envDir `T.append` "/state.json")
                                         : ("--vars-store=" `T.append` envDir `T.append` "/creds.yml")
                                         : "--var=director_name=bosh-lite"
                                         : "--var=internal_ip=192.168.50.6"
                                         : "--var=internal_gw=192.168.50.1"
                                         : "--var=internal_cidr=192.168.50.0/24"
                                         : "--var=outbound_network_name=NatNetwork"
                                         : opsFiles
  where opsFileArg opsFile = "--ops-file=" `T.append` ymlPath opsFile
        ymlPath opsFile = boshDeploymentPath `T.append` "/" `T.append` opsFile
        opsFiles = Prelude.map opsFileArg [ "virtualbox/cpi.yml"
                                          , "virtualbox/outbound-network.yml"
                                          , "bosh-lite.yml"
                                          , "bosh-lite-runc.yml"
                                          , "uaa.yml"
                                          , "credhub.yml"
                                          , "jumpbox-user.yml"]
