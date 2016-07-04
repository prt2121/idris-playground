module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

size : DataStore -> Nat
size (MkData size items) = size

getItems : (store : DataStore) -> Vect (size store) String
getItems (MkData size items) = items

-- Hint: Use Strings.isInfixOf
filterStr : String -> Vect n String -> (p ** Vect p String)
filterStr s xs = filter (\str => s `isInfixOf` str) xs

filterStr' : String -> List String -> List String
filterStr' s xs = filter (\str => s `isInfixOf` str) xs

search : DataStore -> String -> Maybe (String, DataStore)
search (MkData Z items) str = Nothing
search store@(MkData (S k) items) str = Just (unwords (filterStr' str (toList items)) ++ "\n", store)

-- Just (unwords (toList (filterStr str items)), store)

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) y =
              MkData _ (addToData items) where
                addToData : Vect old String -> Vect (S old) String
                addToData [] = [y]
                addToData (x :: xs) = x :: addToData xs

sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs t input =
  let val = cast input in
            if val < 0
            then Nothing
            else let newVal = t + val in Just ("Subtotal: " ++ show newVal ++ "\n", newVal)

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                            False => Nothing
                            True => Just (Get (cast val))
parseCommand "search" str = Just (Search str)
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd $ ltrim args -- remove leading spaces with the ltrim

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = getItems store in
                             case integerToFin pos (size store) of
                                  Nothing => Just ("Out of range\n", store)
                                  Just id => Just (index id store_items ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
            = case parse inp of
                    Nothing => Just ("Invalid command\n", store)
                    Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                    Just (Get pos) => getEntry pos store
                    Just Size => Just (show (size store) ++ "\n", store)
                    Just (Search str) => search store str
                    Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput

-- *datastore> :exec
-- Command: add LOL
-- ID 0
-- Command: ad haha
-- Invalid command
-- Command: add haha
-- ID 1
-- Command: get 1
-- haha
-- Command: get 0
-- LOL

-- replWith : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
-- replWith initial data, a prompt, and a function

-- main : IO ()
-- main = replWith 0 "Value: " sumInputs
-- replWith : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()

-- *datastore> :exec
-- Command: add heyPrat
-- ID 0
-- Command: add hello
-- ID 1
-- Command: search ell
-- hello
-- Command: search he
-- heyPrat hello
-- Command: quit
