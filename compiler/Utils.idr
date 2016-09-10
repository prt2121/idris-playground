module Utils

import LispVal
import Data.Vect

%access public export

mapLeft : (a -> c) -> Either a b -> Either c b
mapLeft f = mirror . (map f) . mirror

toVect2Maybe : List LispVal -> Maybe (Vect 2 LispVal)
toVect2Maybe [x, y] = Just (fromList [x, y])
toVect2Maybe _      = Nothing

toVect2 : List LispVal -> Either LispError (Vect 2 LispVal)
toVect2 ls = maybeToEither (NumArgs 2 ls) (toVect2Maybe ls)
