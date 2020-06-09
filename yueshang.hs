import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

printEn input = concatmap tkpara (lines input)
                where tkpara myline = case take 3 myline of 
                                      "/@ " -> tkline (drop 3 myline) ++ " "
                                      "/p)" -> "\n"
                                      _     -> "" 
                      
                      isBra word = case word of 
                                   '[' -> True
                                   ']' -> True
                                   _   -> False
                      braBreak = break isBra
                      safeTail mylist = if null mylist then "" else tail mylist
                      tkline myline = fst (braBreak myline) ++ safeTail(snd (braBreak (safeTail (snd (braBreak myline) )))) 
                

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
        myFunction = printEn
