module Utils

%access public export

mapLeft : (a -> c) -> Either a b -> Either c b
mapLeft f = mirror . (map f) . mirror
