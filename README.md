# λ calculus interpreter
A simple lambda-calculus interpreter, with properly implemented mute variables.
## instructions
build with dune :
```
dune build ./lambda.exe
dune exec -- ./lambda.exe [optionnal files to load, e.g. std.lambda]
(λ.x.λy.x) y x
--> y
```
declare with colon :
```
M : λf.f f
--> M : λf.f f
M λx.x
--> λx.x
```
λx.y.z is short-hand for λx.λy.z :
```
false : λx.y.y
--> false : λx.λy.y
```
λ can be replaced with / :
```
/x.y.z
--> λx.λy.z
```
use quotes for sentences :
```
"This makes for handy comments !"
--> "This makes for handy comments !"
false "they work just like variables" x
--> x
```
