import System.Environment
import LLang
import Combinators

getLast :: Result a b LAst -> LAst
getLast (Success _ last) = last
getLast _                = Seq []

main = do
    (f:input) <- getArgs
    s    <- readFile f
    let prog = runParser parseL s
    let (Just res) = eval (getLast prog) (initialConf (fmap read input))
    print (output res)