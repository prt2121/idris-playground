module Chap05

printLength : IO ()
printLength = getLine >>= \line => let len = length line in
                                   putStrLn $ show len

--  ~/W/i/starting git:master λ →  ~/idris-install/.cabal-sandbox/bin/idris chap05.idr                                                                                    ⬆ ✖ ◼
--       ____    __     _
--      /  _/___/ /____(_)____
--      / // __  / ___/ / ___/     Version 0.12
--    _/ // /_/ / /  / (__  )      http://www.idris-lang.org/
--   /___/\__,_/_/  /_/____/       Type :? for help
 --
--  Idris is free software with ABSOLUTELY NO WARRANTY.
--  For details type :warranty.
--  Type checking ./chap05.idr
--  *chap05> :exec printLength
--  haha
--  4
--  *chap05> :q
--  Bye bye
