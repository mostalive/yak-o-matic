import Data.GraphViz.Types
import Data.GraphViz.Types.Graph
import Data.GraphViz.Commands.IO

iograph = (readDotFile "planning.dot" :: IO (DotGraph String)) 

smarthost g = successorsOf g "MailSmartHost"

main = do 
    g <- iograph
    return smarthost g
