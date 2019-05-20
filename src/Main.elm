import Browser
import DrawTree exposing(..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Parse exposing (..)
import String exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import TreeDiagram exposing (..)
import TreeDiagram.Svg exposing (..)

main =
  Browser.sandbox { init = init, update = update, view = view }

-- Model

type alias Model = { content : String , 
  expr : Exp ,
  etree : Tree (String, String, Int),
  fvar : Bool ,
  bvar : Bool }

init : Model
init = {content = "" , expr = (App 
  (Lambda "x" (App (BVar "x" 1) (BVar "x" 1))) (Lambda "x" (App (BVar "x" 1) (BVar "x" 1))))
  , etree = exprtoTree (App (Lambda "x" (App (BVar "x" 1) (BVar "x" 1))) 
    (Lambda "x" (App (BVar "x" 1) (BVar "x" 1)))), fvar = False, bvar = False}


-- Update


type Msg
  = Change String | Genpt | Showfvar | Showbvar | Apply

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }
    Genpt -> 
      { model | expr = parseExp model.content , etree = exprtoTree (parseExp model.content) }
    Showfvar -> 
      { model | fvar = not model.fvar }
    Showbvar ->
      { model | bvar = not model.bvar }
    Apply ->
      { model | expr =  applyExp model.expr, etree = exprtoTree (applyExp model.expr)}



-- View

view : Model -> Html Msg
view model = 
  div [Html.Attributes.class ("maindiv")]
  [
    -- div [Html.Attributes.class ("topbar"), Html.Attributes.]
    --   [
    --     Html.text ("λ-Calculus Parse Tree Generator")
    --   ],
    div []
      [ fieldset []
      [
          input [ placeholder "Input λ expression here!", value model.content, onInput Change ] []
        , button [ Html.Events.onClick Genpt ] [ Html.text "Generate Parse Tree" ] 
        , button [ Html.Events.onClick Apply ] [ Html.text "Apply Base Expression" ]
        , div []
          [
          checkbox Showfvar "Show free variables (orange)."
        , checkbox Showbvar "Show bound variables (blue)."
          ]
        ]
      ],
    div []
      [ drawTree model.etree (model.fvar, model.bvar) ]      
  ]

-- Helper functions

applyExp : Exp -> Exp
applyExp expr = case expr of
  App (Lambda x y) z -> applyExpAux x y z
  App z (Lambda x y) -> applyExpAux x y z
  _ -> expr
  
applyExpAux : String -> Exp -> Exp -> Exp
applyExpAux s mainexp applicator = 
  case mainexp of
    Lambda a b -> if a == s then
                    Lambda a b
                  else
                    Lambda a (applyExpAux s b applicator)
    App a b -> App (applyExpAux s a applicator) (applyExpAux s b applicator)
    BVar a b -> if a == s then
                  applicator
                else
                 BVar a b
    _ -> mainexp

exprtoTree : Exp -> Tree (String, String, Int)
exprtoTree expr = 
    case expr of
        FVar x -> TreeDiagram.node ("FVar", x, -1) []
        BVar x y -> TreeDiagram.node ("BVar", x, y) []
        Lambda x y  -> TreeDiagram.node ("Lambda", x, -1) [ exprtoTree y  ]
        App x y -> TreeDiagram.node ("App", "", -1) [exprtoTree x , exprtoTree y ]
        _ -> TreeDiagram.node ("Empty", "", -1) []

checkbox : msg -> String -> Html msg
checkbox msg name =
  label []
    [ input [ Html.Attributes.type_ "checkbox", Html.Events.onClick msg ] []
    , Html.text name
    ]