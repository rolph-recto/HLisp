str = " [set random [fun [lo hi] [__PRIM__random lo hi]]] \
\  \
\ [set + [fun [x y] [__PRIM__+ x y]]] \
\ \
\ [set - [fun [x y] [__PRIM__- x y]]] \
\ \
\ [set * [fun [x y] [__PRIM__* x y]]] \
\  \
\ [set / [fun [x y] [__PRIM__/ x y]]] \
\  \
\ [set < [fun [x y] [__PRIM__< x y]]] \
\  \
\ [set > [fun [x y] [__PRIM__> x y]]] \
\  \
\ [set == [fun [x y] [__PRIM__== x y]]] \
\  \
\ [set head [fun [x] [__PRIM__head x]]] \
\  \
\ [set tail [fun [x] [__PRIM__tail x]]] \
\  \
\ [set nil? [fun [x] [__PRIM__nil? x]]] \
\  \
\ [set cons [fun [h t] [__PRIM__cons h t]]] \
\  \
\ [set ! [fun [l n] [__PRIM__! l n]]] \
\  \
\ [set and [fun [x y] [if x [if y true false] false]]] \
\  \
\ [set or [fun [x y]  [if x true [if y true false]]]] \
\  \
\ [set not [fun [x] [if x false true]]] \
\  \
\ [set any [fun [f lst] \
\   [foldl [fun [x acc] [or [f x] acc]] false lst] \
\ ]] \
\  \
\ [set all [fun [f lst] \
\   [foldl [fun [x acc] [and [f x] acc]] true lst] \
\ ]] \
\  \
\ [set >= [fun [x y] [or [> x y] [== x y]]]] \
\  \
\ [set <= [fun [x y] [or [< x y] [== x y]]]] \
\  \
\ ; primitive higher-order functions \
\ [set map [fun [f lst] \
\   [if [nil? lst] \
\     ~[] \
\     [cons [f [head lst]] [map f [tail lst]]] \
\   ] \
\ ]] \
\  \
\ [set filter [fun [f lst] \
\   [if [nil? lst] \
\     ~[] \
\     [let [hd [head lst]] [tl [tail lst]] \
\       [if [f hd] [cons hd [filter f tl]] [filter f tl]] \
\     ] \
\   ] \
\ ]] \
\  \
\ [set foldr [fun [f acc lst] \
\   [if [nil? lst] \
\     acc \
\     [f [head lst] [foldr f acc [tail lst]]] \
\   ] \
\ ]] \
\  \
\ [set foldl [fun [f acc lst] \
\   [if [nil? lst] \
\     acc \
\     [foldl f [f acc [head lst]] [tail lst]] \
\   ] \
\ ]] \
\  \
\ ; list functions \
\ [set length [fun [lst] \
\   [foldl [fun [x acc] [+ 1 acc]] 0 lst] \
\ ]] \
\  \
\ [set range [fun [lo hi] \
\   [if [== lo hi] \
\     ~[] \
\     [cons lo [range [+ lo 1] hi]] \
\   ] \
\ ]] \
\  \
\ [set append [fun [x y] \
\   [if [nil? x] \
\     y \
\     [cons [head x] [append [tail x] y]] \
\   ] \
\ ]] \
\  \
\ [set cycle [fun [n i lst] [ \
\   [if [< n 1] \
\     ~[] \
\     [if [< i [length lst]] \
\       [cons [! lst i] [cycle [- n 1] [+ i 1] lst]] \
\       [cons [! lst 0] [cycle [- n 1] 1 lst]] \
\     ] \
\   ] \
\ ]]]"

main = do
  putStrLn str
