{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import System.Environment
import Data.List
import Data.List.Utils
import System.Process
import Control.Lens
-- import Turtle
-- import Prelude hiding (FilePath)

data Config =  Config { srcDir :: String 
                      , editor :: String
                      , buildFileDir :: String
                      } deriving (Show)

hardCodedConfig = Config { srcDir = "/home/dara/projects/haskpkg-srcDir"
                         , editor = "/usr/bin/nvim" 
                         , buildFileDir = "/home/dara/projects/haskpkg-buildFiles"}


main :: IO ()
main = do
    args <- getArgs
    let config = hardCodedConfig
    let taskTree = envArgsParse args config
    runTaskTree config taskTree
    return ()

data Task = UpdateAll
           | ListCloned 
           | Invalid 
           | Clone String 
           | RmSrc String
           | CreateBuildFile String
           | Build String
           | Finish
           | None
           | Task :&: Task
           | Task :&&: Task
           deriving (Show, Read)


testRunCL x = runTaskTree hardCodedConfig $ envArgsParse x hardCodedConfig
testRunTT x = runTaskTree hardCodedConfig x

envArgsParse [] cfg = Finish -- putStrLn "Finished parsing arguments"-- doTask Finish
envArgsParse (arg:args) cfg = 
    if (arg `elem` ["u", "update", "up"]) then
        UpdateAll :&&: envArgsParse args cfg
    else if (arg == "raw") then
        read (intercalate " " args)
    else if (arg `elem` ["listCloned", "lc"]) then
        ListCloned :&&: envArgsParse args cfg
    else if (arg == "clone") then 
        parse2argTask arg args Clone cfg
    else if (arg == "rmsrc") then
        parse2argTask arg args RmSrc cfg
    else if (arg == "makeBuildFile") then
        parse2argTask arg args CreateBuildFile cfg
    else if (arg == "build") then
        parse2argTask arg args Build cfg
    else 
        Invalid :&: (envArgsParse args cfg)

parse2argTask arg args task cfg = (maybe Invalid (\str -> task str) (args ^? ix 0)) :&&: (envArgsParse (tail args) cfg)

runTaskTree cfg (t1 :&&: t2) = do runTaskTree cfg t1
                                  runTaskTree cfg t2
runTaskTree cfg (t1 :&: t2) = error "not implemented"
runTaskTree cfg x = doTask x cfg

helpMsg = "This is the help message \n\
          \Please get some help"
        
doTask Invalid cfg = putStrLn helpMsg
doTask UpdateAll cfg = putStrLn "I'm going to update you when you update me"
doTask ListCloned cfg = do
    putStrLn ("Cloned Repos ")
    cloned <- getCloned cfg
    putStrLn $ intercalate " " cloned
    -- callCommand ("cd "++(srcDir cfg)++" && ls")
doTask (Clone str) cfg = case (isGitUrl str) of
                        True -> do
                            gitClone str cfg
                        False ->  doTask Invalid cfg
doTask (RmSrc str) cfg = do 
    putStrLn ("removing "++str)
    callCommand ("cd "++(srcDir cfg)++" && rm -rf ./"++str)
doTask (CreateBuildFile str) cfg = do
    putStrLn ("Entering you into a shell, when you 'exit', the commands you ran will be saved to a file for you to edit")
    callCommand ("cd "++(srcDir cfg)++"/"++str++" && { HISTFILE=./.buildHistory sh ; echo \"#Build file end $(date)\" >> ./.buildHistory; "++(editor cfg)++" ./.buildHistory; cp ./.buildHistory "++(buildFileDir cfg)++"/"++str++".build; }")
doTask (Build str) cfg = do
    putStrLn ("Building "++str)
    callCommand ("cd "++(srcDir cfg)++"/"++str++" && sh "++(buildFileDir cfg)++"/"++str++".build")
doTask Finish cfg = putStrLn "Finished"

-- data BFReplState = BFReplStart | BFReplGoing 
-- mkBuildFileREPL cfg BFReplStart str _ = mkBuildFileREPL cfg BFReplGoing str (createProcess (proc "ls" []){ cwd = Just ((srcDir cfg)++"/str")})

isGitUrl str = endswith ".git" str
gitClone url cfg =  do
    putStrLn ("cloning you now "++url)
    callCommand ("pwd")
    callCommand ("cd "++(srcDir cfg)++"&& git clone "++url)

getCloned cfg = do
    cloned <- readCreateProcess ((shell "ls -1"){cwd = Just (srcDir cfg)} ) ""
    return (split "\n" (take (length (cloned) -1) cloned)) -- drop "" entry from ls


a .>> b = b a
