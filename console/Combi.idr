module Combi

import Lightyear.Combinators
import Lightyear.Core

export
skipMany : Monad m => ParserT str m a -> ParserT str m ()
skipMany p = do many p
                return ()

export
skipMany1 : Monad m => ParserT str m a -> ParserT str m ()
skipMany1 p = do p
                 skipMany p

export
many1 : Monad m => ParserT str m a -> ParserT str m (List a)
many1 p = return $ !p :: !(many p)

sepBy : Monad m => (p : ParserT str m a)
                -> (s : ParserT str m b)
                -> ParserT str m (List a)
sepBy p s = (p `sepBy1` s) <|> pure List.Nil

export
endBy : Monad m => (p : ParserT str m a)
                -> (s : ParserT str m b)
                -> ParserT str m (List a)
endBy p s = many (do x <- p; s; return x)
