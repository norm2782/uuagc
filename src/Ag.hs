
module Main where

import System.Console.GetOpt
import System
import qualified UU.DData.Map as Map(elems, lookup, delete)
import List (sortBy,partition,isSuffixOf)
import UU.Pretty
import Scanner
import Parser
import Transform (sem_AG, wrap_AG, Syn_AG(..),Inh_AG(..))
import DefaultRules (sem_Grammar, wrap_Grammar, Syn_Grammar(..),Inh_Grammar(..))
import PrintCode (sem_Program, wrap_Program, Syn_Program(..),Inh_Program(..))

import PrintErrorMessages (sem_Error, wrap_Error, Syn_Error(..),Inh_Error(..) )
import Options
import qualified GenerateCode as Gen(sem_Grammar, wrap_Grammar, Syn_Grammar(..),Inh_Grammar(..))
import UU.DData.Seq as Seq((<>),toList)
import CommonTypes
version = strip txt
  where strip = takeWhile (/= '$') . drop 1 . dropWhile (/= ':')
        txt   = "Attribute Grammar compiler / HUT project. Version @FULLVERSION@."

{- MAIN -}
main        :: IO ()
main        =  do
                 args <- getArgs
                 progName <- getProgName
                 let usageheader = "Usage info:\n " ++ progName ++ " options file ...\n\nList of options:"
                     (flags,files,errs) = getOptions args
                     doit | showVersion flags = putStrLn ("version: " ++ version)
                          | null files || showHelp flags || (not.null) errs
                                              = putStr $ usageInfo usageheader options
                          | otherwise         = let fnames = zip files (outputFiles flags++repeat "")
                                                in mapM_(\(x,out) -> compile (searchPath  flags) x out flags) fnames 
                 mapM putStrLn errs
                 doit 

compile     :: [FilePath] -> String -> String -> Options -> IO ()
compile searchPath input output flags
            =  do
                 let inputfile  = inputFile input
                 let outputfile = if null output then outputFile input
                                                 else output
                 (ag,parseErrors) <- parseAG searchPath inputfile
                 let Syn_AG{ blocks_Syn_AG = blocks
                           , errors_Syn_AG = err_des
                           , trans_Syn_AG  = desugar
                           } = wrap_AG (sem_AG ag) Inh_AG{options_Inh_AG = flags}
                 
                     Syn_Grammar
                           { out_Syn_Grammar = transformed
                           , errors_Syn_Grammar = err_trans
                           } = wrap_Grammar (sem_Grammar desugar) Inh_Grammar{options_Inh_Grammar = flags}


                     Gen.Syn_Grammar
                           { Gen.prog_Syn_Grammar   = prog
                           , Gen.errors_Syn_Grammar = err_gen
                           , Gen.smdynlists_Syn_Grammar = (smimports, smdynlists)
                           } = Gen.wrap_Grammar (Gen.sem_Grammar transformed) Gen.Inh_Grammar{Gen.options_Inh_Grammar = flags}
                      
                     (semErrors,semWarnings) = 
                          let error e = case wrap_Error (sem_Error e)  Inh_Error{verbose_Inh_Error = verbose flags} 
                                           of Syn_Error{pp_Syn_Error = doc, isWarning_Syn_Error = isWarning } -> (doc,isWarning)
                              (warns,errs) = partition snd (map error (Seq.toList (err_des<>err_trans<>err_gen)))
                          in (map fst errs, map fst warns)
                                     
                                      
                                      
                     Syn_Program{ output_Syn_Program = code }
                        = wrap_Program (sem_Program prog) Inh_Program{ nest_Inh_Program = nest flags
                                                                    , width_Inh_Program = 450 }
                 
                 
                 
                 let takeErrors :: a -> Int -> [a] -> [a]
                     takeErrors more n xs = foldr (\x f n -> if n==0 then [more] else x:f(n-1)) (const []) xs n
                     handleErrors | (not.null) parseErrors =
                                       do let ms = takeErrors  "..., and more parse errors" 5 (map show parseErrors)
                                          mapM putStrLn ms
                                          return False
                                  | (not.null) semErrors   =
                                       do let ms = takeErrors (text "..., and more errors" ) 5 semErrors
                                          mapM (\m -> do render m 75  ; putStrLn "") ms
                                          return False
                                  | otherwise =      
                                       do let ms = takeErrors (text "..., and more warnings" ) 5 semWarnings
                                          mapM (\m -> do render m 75  ; putStrLn "") ms
                                          return True
                 success <- handleErrors 
                 if success then
                   do let (optpragmas,blocks') = case Map.lookup (identifier "optpragmas") blocks of
                                                  Nothing -> ([],blocks)
                                                  Just b  -> ([b],Map.delete (identifier "optpragmas") blocks)
                      writeFile outputfile
                       . unlines . concat $ optpragmas
                      appendFile outputfile "-- do not edit; automatically generated by UU.AG\n"
                      case (moduleName flags) of 
                          Name nm -> generateHeader outputfile nm 
                          Default -> generateHeader outputfile (defaultModuleName input) 
                          NoName  -> return ()
                      let textblocks = case Map.lookup (identifier "imports") blocks' of
                                        Nothing -> Map.elems blocks'
                                        Just b  -> b : Map.elems (Map.delete (identifier "imports") blocks')
                      appendFile outputfile
                       . unlines . concat $ smimports:textblocks
                      appendFileLn outputfile code
                      appendFileLn outputfile (unlines smdynlists)                
                      --putStrLn ("\n" ++ outputfile ++ " generated")
                      return ()
                  else exitFailure



generateHeader :: String -> String -> IO ()
generateHeader fname moduleName =  do
                         appendFile fname $ "module " ++ moduleName ++ " where\n"

appendFileLn :: String -> String -> IO ()
appendFileLn fname txt = do
                     appendFile fname txt
                     appendFile fname "\n"

inputFile :: String -> String
inputFile name = if ".ag" `isSuffixOf` name || ".lag" `isSuffixOf` name
                  then name
                  else name ++ ".ag"

outputFile :: String -> String
outputFile name = defaultModuleName name ++ ".hs"

defaultModuleName :: String -> String
defaultModuleName name = if ".ag" `isSuffixOf` name
                    then (take (length name - 3) name )
                    else if ".lag" `isSuffixOf` name
                            then (take (length name - 4) name )
                            else name
