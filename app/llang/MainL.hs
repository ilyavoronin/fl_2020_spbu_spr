import System.Environment
import LLang (parseL, getLast, initialConf, eval, output)
import Combinators

main = do
    (f:input) <- getArgs
    s    <- readFile f
    let prog = runParser parseL s
    let (Just res) = eval (getLast prog) (initialConf (fmap read input))
    print (output res)