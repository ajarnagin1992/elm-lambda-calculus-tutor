module DrawTree exposing (..)

import Browser
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
         
drawNode : (String, String, Int) -> Svg msg
drawNode (m, n, o) = case (m, n, o) of
    ("Empty", "", _) -> g[][]
    ("App", "", _) -> g []
        [ circle [r ("10"), fill ("black"), stroke ("black"), strokeWidth("3")] []
        , Svg.text_ [x ("20"), y ("4"), fill ("black"), fontSize ("18"), fontFamily("Open Sans")] [Svg.text (m)] ]
    ("BVar", p, b) -> g [Svg.Attributes.id ("pos" ++ fromInt b)]
        [ circle [r ("10"), fill ("white"), stroke ("black"), strokeWidth("3")] []
        , text_ [x ("20"), y ("4"), fill ("black"), fontSize ("18"), fontFamily("Open Sans")] [Svg.text (n)] ]
    ("FVar", p, _) -> g []
        [ circle [r ("10"), fill ("white"), stroke ("black"), strokeWidth("3")] []
        , text_ [x ("20"), y ("4"), fill ("black"), fontSize ("18"), fontFamily("Open Sans")] [Svg.text (n)] ]
    ("Lambda", p, _) -> g []
        [ circle [r ("10"), fill ("black"), stroke ("black"), strokeWidth("3")] []
        , text_ [x ("20"), y ("4"), fill ("black"), fontSize ("18"), fontFamily("Open Sans")] [Svg.text ("λ " ++ n)] ]
    _ -> g []
        [ circle [r ("10"), fill ("black"), stroke ("black"), strokeWidth("3")] []
        , text_ [x ("20"), y ("4"), fill ("black"), fontSize ("18"), fontFamily("Open Sans")] [Svg.text (n)] ]

drawNodeBoun : (String, String, Int) -> Svg msg
drawNodeBoun (m, n, o) = case (m, n, o) of
    ("BVar", p, b) -> g [Svg.Attributes.id ("pos" ++ fromInt b)]
        [ circle [r ("10"), fill ("blue"), stroke ("black"), strokeWidth("3")] []
        , text_ [x ("20"), y ("4"), fill ("black"), fontSize ("18"), fontFamily("Open Sans")] [Svg.text (n)] ]
    _ -> drawNode (m, n, o)

drawNodeFree : (String, String, Int) -> Svg msg
drawNodeFree (m, n, o) = case (m, n, o) of
    ("FVar", p, _) -> g []
        [ circle [r ("10"), fill ("orange"), stroke ("black"), strokeWidth("3")] []
        , text_ [x ("20"), y ("4"), fill ("black"), fontSize ("18"), fontFamily("Open Sans")] [Svg.text (n)] ]
    _ -> drawNode (m, n, o)

drawNodeFreeBoun : (String, String, Int) -> Svg msg
drawNodeFreeBoun (m, n, o) = case (m, n, o) of
    ("FVar", p, _) -> g []
        [ circle [r ("10"), fill ("orange"), stroke ("black"), strokeWidth("3")] []
        , text_ [x ("20"), y ("4"), fill ("black"), fontSize ("18"), fontFamily("Open Sans")] [Svg.text (n)] ]
    ("BVar", p, b) -> g [Svg.Attributes.id ("pos" ++ fromInt b)]
        [ circle [r ("10"), fill ("blue"), stroke ("black"), strokeWidth("3")] []
        , text_ [x ("20"), y ("4"), fill ("black"), fontSize ("18"), fontFamily("Open Sans")] [Svg.text (n)] ]
    _ -> drawNode (m, n, o)

drawEdge : (Float, Float) -> Svg msg
drawEdge (x, y) = line
    [x1 ("0"), y1 ("0"), x2 (fromFloat x), y2 (fromFloat y), stroke ("black"), strokeWidth ("3")]
    []

drawTree : Tree (String, String, Int) -> (Bool, Bool) -> Html msg
drawTree t (b1, b2) =
    let
        choosedrawNode = case (b1, b2) of
            (True, True) -> drawNodeFreeBoun
            (True, False) -> drawNodeFree
            (False, True) -> drawNodeBoun
            (False, False) -> drawNode

    in
    draw 
        { defaultTreeLayout | padding = 100, siblingDistance = 150 }
        choosedrawNode
        drawEdge 
        t