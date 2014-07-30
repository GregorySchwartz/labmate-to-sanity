-- labmate-to-sanity
-- By Gregory W. Schwartz

-- Takes a csv file and return a new csv file in correct formatting

-- Built-in
import Data.List

-- Cabal
import Options.Applicative
import qualified Data.List.Split as Split

-- Command line arguments
data Options = Options { inputSep    :: String
                       , inputNewSep :: String
                       , input       :: String
                       , output      :: String
                       }

-- Command line options
options :: Parser Options
options = Options
      <$> strOption
          ( long "input-sep"
         <> short 'd'
         <> metavar "STRING"
         <> value ","
         <> help "The delimiter for the input file" )
      <*> strOption
          ( long "input-new-sep"
         <> short 'D'
         <> metavar "STRING"
         <> value ","
         <> help "The new delimiter for the output file" )
      <*> strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> value ""
         <> help "The input csv file" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> value "output.csv"
         <> help "The output csv file" )

-- | Removes empty lines
lineCompress :: String -> String
lineCompress []        = []
lineCompress ('\n':xs) = '\n' : (lineCompress $ dropWhile (== '\n') xs)
lineCompress (x:xs)    = x : (lineCompress xs)

-- | Return list of separated lines
lineSep :: String -> String -> [[String]]
lineSep sep = map (Split.splitOn sep) . lines

-- | Return uniform amount of fields based on first line
uniformField :: [[String]] -> [[String]]
uniformField xs = map growOrShrink xs
  where
    growOrShrink x
        | length x > standard = take standard x
        | length x < standard = x ++ (take (standard - length x) .  repeat $ "")
        | otherwise           = x
    standard = length . head $ xs

-- | Return a string of the csv list for saving to a file
printCSV :: String -> [[String]] -> String
printCSV sep = unlines . map (intercalate sep)

labmateToSanity :: Options -> IO ()
labmateToSanity opts = do
    contentsCarriages <- readFile . input $ opts
    -- Get rid of carriages
    let sep               = if (inputSep opts == "\\t")
                                then "\t"
                                else inputSep opts
        newSep            = if (inputNewSep opts == "\\t")
                                then "\t"
                                else inputNewSep opts
        contents          = lineCompress
                          . map (\x -> if (x == '\r') then '\n' else x)
                          $ contentsCarriages
        fieldList         = lineSep sep contents
        uniformFieldList  = uniformField fieldList
        formattedContents = printCSV newSep uniformFieldList

    -- Save results
    writeFile (output opts) formattedContents

main :: IO ()
main = execParser opts >>= labmateToSanity
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Convert a dirty csv file to sane csv file"
     <> header "labmate-to-sanity, Gregory W. Schwartz" )
