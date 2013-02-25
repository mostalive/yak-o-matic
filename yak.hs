module Main(main) where
import System.Environment


import YakCli

usage = "Usage: yak-o-matic [OPTIONS]\n" ++
        "A utility for shaving a yak efficiently\n" ++
        " - Output a list of JSon structures representing the evolution\n"++
        "   of node count in clusters from a graphviz file\n" ++
        " - Alternatively, can use an org-mode file as input, using TODO\n" ++
        "   keywords as clusters\n"

main :: IO ()
main =
    getArgs              >>=
    makeOptions usage    >>=
    outputCfdData        >>=
    mapM_ (putStrLn.show)
