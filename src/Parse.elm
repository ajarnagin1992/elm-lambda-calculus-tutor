module Parse exposing (parseExp, printExp, Exp(..))

import Parser exposing (..)
import Parser.Expression exposing (..)
import Parser.Extras exposing (..)
import Set
import TreeDiagram exposing (Tree)


type Exp = BVar Name Int | FVar Name | Lambda Name Exp | App Exp Exp | Expl (List Exp) | Empty
type alias Name = String

-- Functions to bind variables in lambda expressions:

bindVars : Exp -> Exp
bindVars expr = case expr of
    Lambda x y -> Lambda x (bindVars (bindVarHelp y x 0))
    App x y -> App (bindVars x) (bindVars y)
    _ -> expr

bindVarHelp : Exp -> Name -> Int -> Exp
bindVarHelp expr str c = case expr of
    FVar x -> case (x == str) of
        True -> BVar x c
        False -> expr
    Lambda x y -> Lambda x (bindVarHelp y str (c + 1))
    App x y -> App (bindVarHelp x str c) (bindVarHelp y str c)
    _-> expr

-- Exposed functions related to the parser.

parseExp : String -> Exp
parseExp s = bindVars (switchapp (listconv (returnExp (run (expcontent term) s))))

printExp : Exp -> String
printExp w = case w of
    FVar x -> "FVar " ++ x
    BVar x y -> "BVar " ++ x
    Lambda x y -> "λ" ++ x ++ ".(" ++ printExp y ++  ")"
    App x y -> "App (" ++ printExp x ++ ") (" ++ printExp y ++ ") " 
    _ -> "Empty"

-- Non-exposed functions related to the parser:

returnErr : Result (List DeadEnd) Exp -> (List DeadEnd)
returnErr x = case x of
    Ok y -> []
    Err y -> y

returnExp : Result (List DeadEnd) Exp -> Exp
returnExp x = case x of
    Ok y -> y
    Err y -> Empty

app : (List Exp) -> Exp
app l = case l of
    [] -> Empty
    [x] -> x
    x :: xs -> List.foldl App x xs

exp : Parser Exp
exp = oneOf [ parens <| lazy(\_ -> term)
            , lazy (\_ -> lambda)
            , var
            ]

expcontent : Parser a -> Parser a
expcontent p = p 
        |. end

idvar : Parser Name
idvar = 
    variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c
        , reserved = Set.fromList ["\\", " "]
        }

lambda : Parser Exp
lambda =
    succeed Lambda
        |. spaces
        |. symbol "\\"
        |. spaces
        |= idvar
        |. spaces
        |. symbol "."
        |. spaces
        |= lazy (\_ -> term)
        |. spaces

listconv : Exp -> Exp
listconv x = case x of 
    Expl y -> listconv (app y)
    Lambda y z -> Lambda y (listconv z)
    App y z -> App (listconv y) (listconv z)
    Empty -> Empty
    _ -> x

switchapp : Exp -> Exp
switchapp x = case x of
    Lambda y z -> Lambda y (switchapp z)
    App y z -> App (switchapp z) (switchapp y)
    _ -> x


term : Parser Exp
term = succeed Expl
        |= many (lazy <| \_ -> exp)

var : Parser Exp
var = 
    succeed FVar 
        |. spaces
        |= idvar
        |. spaces
