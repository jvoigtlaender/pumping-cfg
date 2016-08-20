module Main (..) where

import List
import List exposing (..)
import Text exposing (..)
import Time exposing (..)
import Maybe
import Maybe exposing (..)
import Color exposing (..)
import Signal
import Signal exposing (..)
import Drag exposing (..)
import Graphics.Input exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


lineStyle =
    { defaultLine | width = 2, cap = Round, join = Smooth }


dimensions =
    collage 610 315


prevButton =
    button (message buttons.address Prev) "Vorheriges"


nextButton =
    button (message buttons.address Next) "Weiter"


snapButton =
    button (message buttons.address Snap) "Snap"


redoButton =
    button (message buttons.address Redo) "Redo"


type Extrusion
    = Extrusion ( Float, Float ) Float


type Part
    = Top (Maybe String) (Maybe String) String String Extrusion Float Extrusion
    | Bottom (Maybe String) String Extrusion


type alias Parts =
    List ( ( Float, Float ), Part )


type State
    = Tree Parts (List Form)
    | TreeAndHidden ( Parts, Maybe Part )


type Update
    = Prev
    | Redo
    | Next
    | None
    | Move ( Int, ( Int, Int ) )
    | Duplicate Int
    | Snap


clicked =
    mailbox 0


hovering =
    mailbox Nothing


buttons =
    mailbox Redo


type alias Entry =
    { initial : State, render : State -> ( List Element, Element ), update : Update -> State -> State }


extr1 =
    Extrusion ( -90, -120 ) 50


tree1 =
    [ ( ( -50, 140 ), Bottom (Just "S") "" extr1 ) ]


extr2 =
    Extrusion ( -40, -60 ) 40


part1 =
    Top (Just "S") (Just "A") "u" "y" extr1 (-15) extr2


( part2, part3 ) =
    fromMiddle (Just "A") extr2 5 (-20)


initialStateTree0 : State
initialStateTree0 =
    Tree tree1 []


initialStateTree1 : State
initialStateTree1 =
    Tree tree1
        <| List.map (move ( -60, 80 ))
            [ toForm (plainText "A")
            , traced lineStyle (path [ ( -5, -8 ), ( -10, -17 ) ])
            , move ( -12, -25 ) <| toForm (plainText "B")
            , traced lineStyle (path [ ( 5, -8 ), ( 10, -17 ) ])
            , move ( 12, -25 ) <| toForm (plainText "C")
            ]


initialStateTree2 : State
initialStateTree2 =
    Tree tree1
        <| List.map (move ( -50, 100 ))
            [ traced lineStyle (path [ ( 0, 30 ), ( 0, 7 ) ])
            , toForm (plainText "A")
            , traced lineStyle (path [ ( -3, -8 ), ( -10, -27 ) ])
            , move ( -12, -35 ) <| toForm (plainText "A")
            , traced lineStyle (path [ ( -7, -43 ), ( 3, -52 ) ])
            , move ( 5, -60 ) <| toForm (plainText "A")
            , traced lineStyle (path [ ( 2, -68 ), ( -5, -78 ) ])
            ]


initialStateTree3 : State
initialStateTree3 =
    let
        ( t, x1, y1, x5, x3, y2 ) =
            case part1 of
                Top _ (Just t) _ _ (Extrusion ( x1, y1 ) x5) x3 (Extrusion ( _, y2 ) _) ->
                    ( t, x1, y1, x5, x3, y2 )

                _ ->
                    Debug.crash "IMPOSSIBLE!"

        dx =
            (x1 + x5) / 2

        dy =
            y1 / 2

        mdxy =
            ( -dx, -dy )

        x3' =
            x3 - dx

        xy3' =
            ( x3', dy - y2 )

        ( x1_, y1_, x5_, x3_, y2_ ) =
            case part2 of
                Top _ _ _ _ (Extrusion ( x1_, y1_ ) x5_) x3_ (Extrusion ( _, y2_ ) _) ->
                    ( x1_, y1_, x5_, x3_, y2_ )

                Bottom _ _ _ ->
                    Debug.crash "IMPOSSIBLE!"

        dx_ =
            (x1_ + x5_) / 2

        dy_ =
            y1_ / 2

        mdxy_ =
            ( -dx_, -dy_ )

        x3'_ =
            x3_ - dx_

        xy3'_ =
            ( x3'_, dy_ - y2_ )
    in
        Tree tree1
            <| List.map (move ( -50 + dx, 140 + dy ))
                [ traced lineStyle (path [ add mdxy tm, add xy3' bm ])
                , move xy3' <| toForm (plainText t)
                , move xy3' <| move ( dx_, dy_ ) <| traced lineStyle (path [ add mdxy_ tm, add xy3'_ bm ])
                , move xy3' <| move ( dx_, dy_ ) <| move xy3'_ <| toForm (plainText t)
                ]


renderTree : State -> ( List Element, Element )
renderTree state =
    case state of
        Tree parts forms ->
            ( [], dimensions (forms ++ fromParts parts) )

        TreeAndHidden _ ->
            Debug.crash "IMPOSSIBLE!"


updateTree : Update -> State -> State
updateTree action state =
    case state of
        Tree parts forms ->
            case action of
                Move ( i, ( dx, dy ) ) ->
                    Tree (moveBy ( i, ( dx, dy ) ) parts)
                        (List.map (move ( toFloat dx, -(toFloat dy) )) forms)

                _ ->
                    Tree parts forms

        TreeAndHidden _ ->
            Debug.crash "IMPOSSIBLE!"


initialStatePumping : State
initialStatePumping =
    TreeAndHidden ( ( ( -50, 140 ), part1 ) :: List.map (\p -> ( ( 0, 0 ), p )) [ part2, part3 ], Nothing )


renderPumping : State -> ( List Element, Element )
renderPumping state =
    case state of
        TreeAndHidden ( parts, _ ) ->
            ( [ snapButton, redoButton ], dimensions (fromParts parts) )

        Tree _ _ ->
            Debug.crash "IMPOSSIBLE!"


updatePumping : Update -> State -> State
updatePumping action state =
    case state of
        TreeAndHidden ( parts, m ) ->
            TreeAndHidden
                <| case action of
                    None ->
                        ( parts, m )

                    Move by ->
                        ( moveBy by parts, m )

                    Duplicate i ->
                        let
                            go i ps =
                                case ps of
                                    p :: ps ->
                                        if i > 0 then
                                            p :: (go (i - 1) ps)
                                        else if isEmpty ps then
                                            [ p ]
                                        else
                                            p
                                                :: (case p of
                                                        ( _, p ) ->
                                                            ( ( 0, 0 ), p )
                                                   )
                                                :: ps

                                    [] ->
                                        Debug.crash "IMPOSSIBLE!"
                        in
                            if i > 0 then
                                ( go i parts, m )
                            else
                                case ( m, parts ) of
                                    ( Nothing, p :: ( _, p' ) :: ps ) ->
                                        ( p :: (take 1 (reverse ps)), Just p' )

                                    ( Just p', p :: ps ) ->
                                        ( p :: ( ( 0, 0 ), p' ) :: ps, Nothing )

                                    _ ->
                                        Debug.crash "IMPOSSIBLE!"

                    Snap ->
                        ( indexedMap
                            (\i p ->
                                if i > 0 then
                                    ( ( 0, 0 ), snd p )
                                else
                                    p
                            )
                            parts
                        , m
                        )

                    _ ->
                        Debug.crash "IMPOSSIBLE!"

        Tree _ _ ->
            Debug.crash "IMPOSSIBLE!"


scenes : List Entry
scenes =
    [ Entry initialStateTree0 renderTree updateTree
    , Entry initialStateTree1 renderTree updateTree
    , Entry initialStateTree2 renderTree updateTree
    , Entry initialStateTree3 renderTree updateTree
    , Entry initialStatePumping renderPumping updatePumping
    ]


main =
    Signal.map
        (\( current, t, prev, rest ) ->
            let
                ( btns, el ) =
                    t.render current
            in
                flow up
                    [ el
                    , flow right
                        ((if isEmpty prev then
                            []
                          else
                            [ prevButton ]
                         )
                            ++ (if isEmpty rest then
                                    []
                                else
                                    [ nextButton ]
                               )
                            ++ btns
                        )
                    ]
        )
        <| foldp
            (\a ( current, t, prev, rest ) ->
                case a of
                    Prev ->
                        case prev of
                            ( c, t' ) :: ps ->
                                ( c, t', ps, ( current, t ) :: rest )

                            [] ->
                                Debug.crash "IMPOSSIBLE!"

                    Next ->
                        case rest of
                            ( c, t' ) :: ps ->
                                ( c, t', ( current, t ) :: prev, ps )

                            [] ->
                                Debug.crash "IMPOSSIBLE!"

                    Redo ->
                        ( t.initial, t, prev, rest )

                    _ ->
                        ( t.update a current, t, prev, rest )
            )
            (case
                List.map (\t -> ( t.initial, t )) scenes
             of
                ( initial, t ) :: rest ->
                    ( initial, t, [], rest )

                [] ->
                    Debug.crash "IMPOSSIBLE!"
            )
            (mergeMany
                [ Signal.map
                    (\m ->
                        case m of
                            Just ( i, MoveBy ( dx, dy ) ) ->
                                Move ( i, ( dx, dy ) )

                            _ ->
                                None
                    )
                    <| trackMany Nothing hovering.signal
                , Signal.map Duplicate (dblClicks 300)
                , buttons.signal
                ]
            )


tm =
    ( 0, -7 )


bm =
    ( 0, 7 )


draw i part =
    let
        tl =
            ( -5, -6 )

        tr =
            ( 5, -6 )

        maybeAdd xy dxy m =
            if isNothing m then
                xy
            else
                add dxy xy

        package ( ( xl, xr, yb, dxy ), forms ) =
            move dxy
                <| toForm
                <| clickable (message clicked.address i)
                <| hoverable
                    (\b ->
                        message hovering.address
                            (if b then
                                Just i
                             else
                                Nothing
                            )
                    )
                <| (collage (3 + ceiling (xr - xl))
                        (26 + ceiling (-yb))
                        (forms [ alpha 0 (filled black (rect (4 + xr - xl) (27 - yb))) ])
                   )
    in
        package
            <| case part of
                Top s t s1 s2 (Extrusion ( x1, y1 ) x5) x3 (Extrusion ( x2, y2 ) x4) ->
                    let
                        dx =
                            (x1 + x5) / 2

                        dy =
                            y1 / 2

                        mdxy =
                            ( -dx, -dy )

                        x3' =
                            x3 - dx

                        xy3' =
                            ( x3', dy - y2 )

                        x' =
                            dx - x5

                        x'' =
                            x2 + x3'

                        x''' =
                            x4 + x3'
                    in
                        ( ( x1, x5, y1, ( dx, dy ) )
                        , \foil ->
                            withDefault [] (Maybe.map (\s -> [ move mdxy <| toForm (plainText s) ]) s)
                                ++ traced lineStyle
                                    (path
                                        [ maybeAdd mdxy tl s
                                        , ( x', dy )
                                        , ( x'', dy )
                                        , maybeAdd xy3' tl t
                                        ]
                                    )
                                :: withDefault [] (Maybe.map (\t -> [ move xy3' (toForm (plainText t)) ]) t)
                                ++ traced lineStyle
                                    (path
                                        [ maybeAdd xy3' tr t
                                        , ( x''', dy )
                                        , ( -x', dy )
                                        , maybeAdd mdxy tr s
                                        ]
                                    )
                                :: traced lineStyle
                                    (path
                                        [ maybeAdd mdxy tm s
                                        , maybeAdd xy3' bm t
                                        ]
                                    )
                                :: move ( (x'' + x') / 2, dy - 5 ) (toForm (plainText s1))
                                :: move ( (x''' - x') / 2, dy - 5 ) (toForm (plainText s2))
                                :: foil
                        )

                Bottom t s (Extrusion ( x2, y2 ) x4) ->
                    let
                        dx =
                            (x2 + x4) / 2

                        dy =
                            y2 / 2

                        mdxy =
                            ( -dx, -dy )

                        x' =
                            x2 - dx
                    in
                        ( ( x2, x4, y2, ( dx, dy ) )
                        , \foil ->
                            withDefault [] (Maybe.map (\t -> [ move mdxy <| toForm (plainText t) ]) t)
                                ++ traced lineStyle
                                    (path
                                        [ maybeAdd mdxy tl t
                                        , ( x', dy )
                                        , ( -x', dy )
                                        , maybeAdd mdxy tr t
                                        ]
                                    )
                                :: move ( 0, dy - 5 ) (toForm (plainText s))
                                :: foil
                        )



-----


plainText string =
    leftAligned (fromString string)


add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


fromMiddle s extr x3 y2 =
    case extr of
        Extrusion ( x1, y1 ) x5 ->
            let
                x2 =
                    x1 * y2 / y1

                x4 =
                    x5 * y2 / y1

                extr' =
                    Extrusion ( x2, y2 ) x4
            in
                ( Top s s "v" "x" extr x3 extr', Bottom s "w" extr' )


fromParts parts =
    let
        ( fs1, fs2 ) =
            go 0 False parts
    in
        fs1 ++ fs2


moveBy : ( Int, ( Int, Int ) ) -> Parts -> Parts
moveBy ( i, ( dx, dy ) ) =
    indexedMap
        (\j xyp ->
            if i == j then
                ( add (fst xyp) ( toFloat dx, -(toFloat dy) ), snd xyp )
            else
                xyp
        )


go i connect list =
    case list of
        ( ( x, y ), part ) :: rest ->
            let
                ( fs1, fs2 ) =
                    if isEmpty rest then
                        ( [], [] )
                    else
                        let
                            f =
                                List.map (move (add ( x, y ) (docking part)))
                        in
                            case go (i + 1) True rest of
                                ( xs, ys ) ->
                                    ( f xs, f ys )
            in
                ( (if connect && abs x + abs y >= 10 then
                    [ traced { lineStyle | dashing = [ 1, 5 ] } (path [ ( 0, -5 ), ( x, y + 5 ) ]) ]
                   else
                    []
                  )
                    ++ fs1
                , move ( x, y ) (draw i part) :: fs2
                )

        [] ->
            Debug.crash "IMPOSSIBLE!"


docking : Part -> ( Float, Float )
docking part =
    case part of
        Top _ _ _ _ (Extrusion ( x1, y1 ) x5) x3 (Extrusion ( x2, y2 ) x4) ->
            ( x3, y1 - y2 )

        Bottom _ _ _ ->
            Debug.crash "IMPOSSIBLE!"


dblClicks d =
    -- clicked.signal -- on Internet Explorer
    let
        step ( t', id ) ( s, id2 ) =
            case s of
                Nothing ->
                    ( Just t', id )

                Just t ->
                    if t' - t < d && id == id2 then
                        ( Nothing, id )
                    else
                        ( Just t', id )
    in
        Signal.map snd (Signal.filter (fst >> isNothing) ( Nothing, 0 ) <| foldp step ( Nothing, 0 ) (timestamp clicked.signal))


isNothing t =
    case t of
        Nothing ->
            True

        _ ->
            False
