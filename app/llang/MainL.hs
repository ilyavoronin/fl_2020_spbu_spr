import System.Environment
import LEval (parseAndEvalProg, Configuration (..))
import Combinators

main = do
    (f:input) <- getArgs
    s    <- readFile f
    case (parseAndEvalProg s (fmap read input)) of
    	Just res -> print (output res)
    	otherwise -> print "Something wrong"