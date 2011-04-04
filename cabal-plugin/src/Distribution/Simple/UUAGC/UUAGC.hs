{-# LANGUAGE CPP #-}

module Distribution.Simple.UUAGC.UUAGC(uuagcUserHook,
                                       uuagc
                                      ) where
import Distribution.Simple.BuildPaths (autogenModulesDir)
import Debug.Trace
import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.Simple.Setup
import Distribution.PackageDescription hiding (Flag)
import Distribution.Simple.UUAGC.AbsSyn( AGFileOption(..)
                                         , AGFileOptions
                                         , AGOptionsClass(..)
                                         , UUAGCOption(..)
                                         , UUAGCOptions
                                         , defaultUUAGCOptions
                                         , fromUUAGCOtoArgs
                                         , fromUUAGCOstoArgs
                                         , lookupFileOptions
                                         , fileClasses
                                         )
import Distribution.Simple.UUAGC.Parser
import Distribution.Verbosity
import System.Process( CreateProcess(..), createProcess, CmdSpec(..)
                     , StdStream(..), runProcess, waitForProcess
                     , proc)
import System.Directory(getModificationTime
                       ,doesFileExist
                       ,removeFile)
import System.FilePath(pathSeparators,
                       (</>),
                       takeFileName,
                       normalise,
                       joinPath,
                       dropFileName,
                       addExtension,
                       dropExtension)

import System.Exit (ExitCode(..))
import System.IO( openFile, IOMode(..),
                  hFileSize,
                  hSetFileSize,
                  hClose,
                  hGetContents,
                  hFlush,
                  Handle(..), stderr, hPutStr, hPutStrLn)
import System(exitFailure)
import Control.Exception (throwIO)
import Control.Monad (liftM, when, guard, forM_, forM)
import Control.Arrow ((&&&), second)
import Data.Maybe (maybeToList)
import Data.Either (partitionEithers)
import Data.List (nub)

-- | 'uuagc' returns the name of the uuagc compiler
uuagcn = "uuagc"

-- | 'defUUAGCOptions' returns the default names of the uuagc options
defUUAGCOptions = "uuagc_options"

-- | File used to store de classes defined in the cabal file.
agClassesFile = ".ag_file_options"

-- | The prefix used for the cabal file optionsw
agModule = "x-agmodule"

-- | The prefix used for the cabal file options used for defining classes
agClass  = "x-agclass"

uuagcUserHook :: UserHooks
uuagcUserHook = simpleUserHooks {hookedPreProcessors = ("ag", uuagc):("lag",uuagc):knownSuffixHandlers
                                ,buildHook = uuagcBuildHook
                                ,postBuild = uuagcPostBuild
                                }

originalPreBuild  = preBuild simpleUserHooks
originalBuildHook = buildHook simpleUserHooks

processContent :: Handle -> IO [String]
processContent = liftM words . hGetContents

putErrorInfo :: Handle -> IO ()
putErrorInfo h = hGetContents h >>= hPutStr stderr

addSearch :: String -> [String] -> [String]
addSearch sp fl = let sf = [head pathSeparators]
                      path = if sp == ""
                             then '.' : sf
                             else sp ++ sf
                  in [normalise (joinPath [sp,f]) | f  <- fl]

throwFailure :: IO ()
throwFailure = throwIO $ ExitFailure 1

-- The tmp build directory really depends on the type of project.
-- In the case executables it uses the name of the generated file for
-- the output directory.
withBuildTmpDir
  :: PackageDescription
     -> LocalBuildInfo
     -> (FilePath -> IO ())
     -> IO ()
withBuildTmpDir pkgDescr lbi f = do
#if MIN_VERSION_Cabal(1,8,0)
            withLib pkgDescr $ \ _ -> f $ buildDir lbi
#else
            withLib pkgDescr () $ \ _ -> f $ buildDir lbi
#endif
            withExe pkgDescr $ \ theExe ->
                    f $ buildDir lbi </> exeName theExe </> exeName theExe ++ "-tmp"

-- Creates the output file given the main preprocessed file and the buildtmp folder
tmpFile :: FilePath -> FilePath -> FilePath
tmpFile buildTmp = (buildTmp </>)
                   . (`addExtension` "hs")
                   . dropExtension
                   . takeFileName

-- | 'updateAGFile' search into the uuagc options file for a list of all
-- AG Files and theirs file dependencies in order to see if the latters
-- are more updated that the formers, and if this is the case to
-- update the AG File
updateAGFile :: PackageDescription 
             -> LocalBuildInfo 
             -> (FilePath, String)
             -> IO ()
updateAGFile pkgDescr lbi (f, sp) = do
  fileOpts <- readFileOptions
  let opts = case lookup f fileOpts of
               Nothing -> []
               Just x -> x
      modeOpts = filter isModeOption opts
      isModeOption UHaskellSyntax = True
      isModeOption ULCKeyWords    = True
      isModeOption UDoubleColons  = True
      isModeOption _              = False
  (_, Just ppOutput, Just ppError, ph) <- newProcess modeOpts
  ec <- waitForProcess ph
  case ec of
    ExitSuccess ->
      do fls <- processContent ppOutput
         let flsC = addSearch sp fls
         when ((not.null) flsC) $ do
            flsmt <- mapM getModificationTime flsC
            let maxModified = maximum flsmt
                removeTmpFile f = do
                                  exists <- doesFileExist f
                                  when exists $ do
                                      fmt <- getModificationTime f
                                      when (maxModified > fmt) $ removeFile f
            withBuildTmpDir pkgDescr lbi $ removeTmpFile . (`tmpFile` f)
    (ExitFailure exc) ->
      do putErrorInfo ppOutput
         putErrorInfo ppError
         throwFailure
  where newProcess mopts = createProcess $ (proc uuagcn (fromUUAGCOstoArgs mopts ++ ["--genfiledeps"
                                                                                    ,"--=" ++ intercalate ":" [sp]
                                                                                    ,f
                                                                                    ]
                                                        )
                                           )
                                    { std_in  = Inherit
                                    , std_out = CreatePipe
                                    , std_err = CreatePipe
                                    }

getAGFileOptions :: [(String, String)] -> IO AGFileOptions
getAGFileOptions extra = do
  usesOptionsFile <- doesFileExist defUUAGCOptions
  if usesOptionsFile
       then do r <- parserAG' defUUAGCOptions
               case r of
                 Left e -> print e >> exitFailure
                 Right a -> return a
       else mapM (parseOptionAG . snd)
            $ filter ((== agModule) . fst) extra

getAGClasses :: [(String, String)] -> IO [AGOptionsClass]
getAGClasses = mapM (parseClassAG . snd) . filter ((== agClass) . fst)

writeFileOptions :: [(String, [UUAGCOption])] -> IO ()
writeFileOptions opts  = do
  hClasses <- openFile agClassesFile WriteMode
  hPutStr hClasses $ show opts
  hFlush  hClasses
  hClose  hClasses

readFileOptions :: IO [(String, [UUAGCOption])]
readFileOptions = do
  hClasses <- openFile agClassesFile ReadMode
  sClasses <- hGetContents hClasses
  classes <- readIO sClasses :: IO [(String, [UUAGCOption])]
  hClose hClasses
  return $ classes

getOptionsFromClass :: [(String, [UUAGCOption])] -> AGFileOption -> ([String], [UUAGCOption])
getOptionsFromClass classes fOpt =
    second (nub . concat . ((opts fOpt):))
    . partitionEithers $ do
                       fClass <- fileClasses fOpt
                       case fClass `lookup` classes of
                         Just x  -> return $ Right x
                         Nothing -> return $ Left $ "Warning: The class "
                                                   ++ show fClass
                                                   ++ " is not defined."

uuagcBuildHook
  :: PackageDescription
     -> LocalBuildInfo
     -> UserHooks
     -> BuildFlags
     -> IO ()
uuagcBuildHook pd lbi uh bf = do
  let lib    = library pd
      exes   = executables pd
      bis    = map libBuildInfo (maybeToList lib) ++ map buildInfo exes
  classes <- map (className &&& opts') `fmap` (getAGClasses . customFieldsPD $ pd)
  options <- getAGFileOptions (bis >>= customFieldsBI)
  fileOptions <- forM options (\ opt ->
      let (notFound, opts) = getOptionsFromClass classes $ opt
      in do case buildVerbosity bf of
              Flag v | v >= verbose -> putStrLn ("options for " ++ filename opt ++ ": " ++ show opts)
              _ -> return () 
            forM_ notFound (hPutStrLn stderr) >> return (normalise . filename $ opt, opts))
  writeFileOptions fileOptions
  let agflSP = map (id &&& dropFileName) $ nub $ getAGFileList options
  mapM_ (updateAGFile pd lbi) agflSP
  originalBuildHook pd lbi uh bf

uuagcPostBuild _ _ _ _ = do
               exists <- doesFileExist agClassesFile
               when exists $ removeFile agClassesFile

getAGFileList :: AGFileOptions -> [FilePath]
getAGFileList = map (normalise . filename)

uuagc :: BuildInfo
        -> LocalBuildInfo
        -> PreProcessor
uuagc build local  =
   PreProcessor {
     platformIndependent = True,
     runPreProcessor = mkSimplePreProcessor $ \ inFile outFile verbosity ->
                       do info verbosity $ concat [inFile, " has been preprocessed into ", outFile]
                          print $ "processing: " ++ inFile ++ " generating: " ++ outFile
--                          opts <- getAGFileOptions $ customFieldsBI build
                          fileOpts <- readFileOptions
                          let opts = case lookup inFile fileOpts of
                                       Nothing -> []
                                       Just x -> x
                              search  = dropFileName inFile
                              options = fromUUAGCOstoArgs opts
                                        ++ ["-P" ++ search, "--output=" ++ outFile, inFile]
                          (_,_,_,ph) <- createProcess (proc uuagcn options)
                          eCode <- waitForProcess ph
                          case eCode of
                            ExitSuccess   -> return ()
                            ExitFailure _ -> throwFailure
                }

