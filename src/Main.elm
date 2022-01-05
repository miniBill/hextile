port module Main exposing (Grid, Model, Msg, Point, main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy
import Json.Decode
import List.Extra
import Random exposing (Seed)
import Random.List
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Events
import Time
import VirtualDom


port resetPickedDirectory : () -> Cmd msg


port pickedDirectoryReset : (() -> msg) -> Sub msg


port saveFile : { number : Int, content : String } -> Cmd msg


port saveSucceeded : (Int -> msg) -> Sub msg


port saveFailed : (Int -> msg) -> Sub msg


scale : number
scale =
    100


type alias Flags =
    { hasShowDirectoryPicker : Bool }


type alias Model =
    { size : Int
    , grid : Maybe Grid
    , animate : Bool
    , animateEvery : String
    , seed : Seed
    , saving : Saving
    , viewOptions : ViewOptions
    , saveLength : String
    }


type alias ViewOptions =
    { showLabels : Bool
    , showDots : Bool
    }


type Saving
    = SavingNot
    | SavingRunning Int Grid
    | SavingError
    | SavingSuccess


type Msg
    = Smaller
    | Bigger
    | Reset
    | GotSeed Seed
    | Rotat Link Link Link
    | RandomStep
    | Animate Bool
    | AnimateEvery String
    | ShowLabels Bool
    | ShowDots Bool
    | Save
    | PickedDirectoryReset
    | SaveSucceded Int
    | SaveFailed Int
    | SaveLength String


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init { hasShowDirectoryPicker } =
    reinit
        { size = 8
        , animateEvery = "16"
        , hasShowDirectoryPicker = hasShowDirectoryPicker
        , saving = SavingNot
        , saveLength = "100"
        , viewOptions =
            { showLabels = False
            , showDots = True
            }
        }


reinit :
    { a
        | size : Int
        , animateEvery : String
        , viewOptions : ViewOptions
        , saving : Saving
        , saveLength : String
    }
    ->
        ( Model
        , Cmd Msg
        )
reinit { size, animateEvery, viewOptions, saving, saveLength } =
    ( { grid = buildGrid <| buildSkeleton size
      , size = size
      , animate = False
      , animateEvery = animateEvery
      , viewOptions = viewOptions
      , seed = Random.initialSeed 0
      , saving = saving
      , saveLength = saveLength
      }
    , Random.generate GotSeed Random.independentSeed
    )


view : Model -> Html Msg
view model =
    let
        buttons =
            viewButtons model

        gridView =
            case model.grid of
                Nothing ->
                    Html.text "Error building grid"

                Just grid ->
                    Html.Lazy.lazy2 viewGrid model.viewOptions grid
    in
    case model.saving of
        SavingRunning _ _ ->
            Html.text "Saving..."

        _ ->
            Html.div []
                (buttons ++ [ gridView ])


viewButtons : Model -> List (Html Msg)
viewButtons { animate, animateEvery, viewOptions, saveLength } =
    [ Html.button [ Html.Events.onClick Smaller ] [ Html.text "Smaller" ]
    , Html.button [ Html.Events.onClick Reset ] [ Html.text "Reset" ]
    , Html.button [ Html.Events.onClick Bigger ] [ Html.text "Bigger" ]
    , Html.button [ Html.Events.onClick RandomStep ] [ Html.text "Random Step" ]
    , Html.button [ Html.Events.onClick (Animate <| not animate) ]
        [ Html.text <|
            if animate then
                "Stop animation"

            else
                "Animate"
        ]
    , Html.text "every"
    , Html.input
        [ Html.Attributes.value animateEvery
        , Html.Events.onInput AnimateEvery
        , Html.Attributes.type_ "number"
        , Html.Attributes.style "width" "50px"
        , if String.toFloat animateEvery == Nothing then
            Html.Attributes.style "background" "#fcc"

          else
            Html.Attributes.classList []
        ]
        []
    , Html.text "ms"
    , Html.button [ Html.Events.onClick (ShowLabels <| not viewOptions.showLabels) ]
        [ Html.text <|
            if viewOptions.showLabels then
                "Hide labels"

            else
                "Show labels"
        ]
    , Html.button [ Html.Events.onClick (ShowDots <| not viewOptions.showDots) ]
        [ Html.text <|
            if viewOptions.showDots then
                "Hide dots"

            else
                "Show dots"
        ]
    , Html.text "Save"
    , Html.input
        [ Html.Attributes.value saveLength
        , Html.Events.onInput SaveLength
        , Html.Attributes.type_ "number"
        , Html.Attributes.style "width" "60px"
        , if String.toInt saveLength == Nothing then
            Html.Attributes.style "background" "#fcc"

          else
            Html.Attributes.classList []
        ]
        []
    , Html.text "animation frames"
    , Html.button
        [ Html.Events.onClick Save ]
        [ Html.text "Save" ]
    ]
        |> List.intersperse (Html.text " ")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ viewOptions } as model) =
    case msg of
        Reset ->
            reinit model

        Smaller ->
            reinit { model | size = max 0 <| model.size - 1 }

        Bigger ->
            reinit { model | size = model.size + 1 }

        Rotat ab cd ef ->
            ( { model | grid = Maybe.map (applyRotation ab cd ef) model.grid }
            , Cmd.none
            )

        RandomStep ->
            case model.grid of
                Nothing ->
                    ( model, Cmd.none )

                Just grid ->
                    let
                        ( grid_, seed_ ) =
                            applyRandomRotation model.seed grid
                    in
                    ( { model | grid = Just grid_, seed = seed_ }
                    , Cmd.none
                    )

        Animate animate ->
            ( { model | animate = animate }, Cmd.none )

        AnimateEvery animateEvery ->
            ( { model | animateEvery = animateEvery }, Cmd.none )

        ShowLabels showLabels ->
            ( { model | viewOptions = { viewOptions | showLabels = showLabels } }, Cmd.none )

        ShowDots showDots ->
            ( { model | viewOptions = { viewOptions | showDots = showDots } }, Cmd.none )

        GotSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        SaveLength saveLength ->
            ( { model | saveLength = saveLength }, Cmd.none )

        Save ->
            case ( model.grid, String.toInt model.saveLength ) of
                ( Just grid, Just saveLength ) ->
                    ( { model
                        | animate = False
                        , saving = SavingRunning saveLength grid
                      }
                    , resetPickedDirectory ()
                    )

                _ ->
                    ( model, Cmd.none )

        PickedDirectoryReset ->
            case model.saving of
                SavingRunning _ grid ->
                    ( model, saveFile { number = 0, content = toSvgString model.viewOptions grid } )

                _ ->
                    ( { model | saving = SavingError }, Cmd.none )

        SaveSucceded i ->
            case model.saving of
                SavingRunning count grid ->
                    if i == count - 1 then
                        ( { model | saving = SavingSuccess }, Cmd.none )

                    else
                        let
                            ( grid_, seed_ ) =
                                applyRandomRotation model.seed grid
                        in
                        ( { model | seed = seed_, saving = SavingRunning count grid_ }
                        , saveFile { number = i + 1, content = toSvgString model.viewOptions grid_ }
                        )

                _ ->
                    ( { model | saving = SavingError }, Cmd.none )

        SaveFailed _ ->
            ( { model | saving = SavingError }, Cmd.none )


applyRandomRotation : Seed -> Grid -> ( Grid, Seed )
applyRandomRotation seed grid =
    let
        generator =
            getRotations grid
                |> Random.List.choose
                |> Random.map Tuple.first

        ( rotation, newSeed ) =
            Random.step generator seed
    in
    case rotation of
        Nothing ->
            ( grid, newSeed )

        Just ( ab, cd, ef ) ->
            ( applyRotation ab cd ef grid
            , newSeed
            )


applyRotation : Link -> Link -> Link -> Grid -> Grid
applyRotation (( a, b ) as ab) (( c, d ) as cd) (( e, f ) as ef) grid =
    { links =
        grid.links
            |> Set.remove ab
            |> Set.remove cd
            |> Set.remove ef
            |> Set.insert (toLink a d)
            |> Set.insert (toLink e b)
            |> Set.insert (toLink c f)
    }


subscriptions : Model -> Sub Msg
subscriptions { animate, animateEvery } =
    let
        animation =
            case ( animate, String.toFloat animateEvery ) of
                ( True, Just interval ) ->
                    Time.every interval <| \_ -> RandomStep

                _ ->
                    Sub.none
    in
    Sub.batch
        [ animation
        , pickedDirectoryReset <| \_ -> PickedDirectoryReset
        , saveFailed SaveFailed
        , saveSucceeded SaveSucceded
        ]


buildGrid : GridBuilder -> Maybe Grid
buildGrid builder =
    let
        go p newOpen stack =
            case stack of
                [] ->
                    Nothing

                stackh :: stackt ->
                    let
                        link =
                            toLink p stackh
                    in
                    case
                        buildGrid
                            { open =
                                newOpen
                                    |> Dict.values
                                    |> List.concatMap Dict.toList
                                    |> List.filter (\( on, _ ) -> on /= stackh)
                                    |> List.map (\( on, ons ) -> ( on, List.filter ((/=) p) ons ))
                                    |> gatherLinks
                            , links = Set.insert link builder.links
                            }
                    of
                        Just gg ->
                            Just gg

                        Nothing ->
                            go p newOpen stackt
    in
    case pop builder.open of
        Nothing ->
            Just { links = builder.links }

        Just ( ( p, ns ), newOpen ) ->
            go p newOpen ns


toLink : Point -> Point -> Link
toLink p stackh =
    if
        (plusX p == stackh)
            || (plusY p == stackh)
            || (plusZ p == stackh)
    then
        ( p, stackh )

    else if
        (plusX stackh == p)
            || (plusY stackh == p)
            || (plusZ stackh == p)
    then
        ( stackh, p )

    else
        ( stackh, p )


plusX : Point -> Point
plusX ( x, y, z ) =
    ( x + 1, y, z )


plusY : Point -> Point
plusY ( x, y, z ) =
    ( x, y + 1, z )


plusZ : Point -> Point
plusZ ( x, y, z ) =
    ( x, y, z + 1 )


minusY : Point -> Point
minusY ( x, y, z ) =
    ( x, y - 1, z )


minusZ : Point -> Point
minusZ ( x, y, z ) =
    ( x, y, z - 1 )


pop : OpenList -> Maybe ( ( Point, List Point ), OpenList )
pop open =
    case Dict.toList open of
        [] ->
            Nothing

        ( 0, d ) :: _ ->
            if Dict.isEmpty d then
                pop (Dict.remove 0 open)

            else
                Nothing

        ( h, d ) :: tail ->
            case Dict.toList d of
                [] ->
                    pop (Dict.remove h open)

                [ p ] ->
                    Just ( p, Dict.fromList tail )

                p :: ps ->
                    Just ( p, Dict.insert h (Dict.fromList ps) (Dict.fromList tail) )


buildSkeleton : Int -> GridBuilder
buildSkeleton =
    let
        go acc size =
            if size <= 0 then
                acc

            else
                go (expand <| expand acc) (size - 1)
    in
    go baseHexagon


getPointsBoundingBox : List Point -> BoundingBox
getPointsBoundingBox pointsList =
    case pointsList of
        [] ->
            { minx = 0, miny = 0, maxx = 1, maxy = 1, cx = 0.5 }

        h :: t ->
            List.foldl
                (\e acc ->
                    let
                        boundingBox =
                            getTriangleBoundingBox e
                    in
                    { minx = min acc.minx boundingBox.minx
                    , miny = min acc.miny boundingBox.miny
                    , maxx = max acc.maxx boundingBox.maxx
                    , maxy = max acc.maxy boundingBox.maxy
                    , cx = 0
                    }
                )
                (getTriangleBoundingBox h)
                t


type alias Point =
    ( Int, Int, Int )


baseHexagon : GridBuilder
baseHexagon =
    let
        top =
            ( 0, 0, 0 )

        tl =
            ( 0, 0, 1 )

        tr =
            ( 0, 1, 0 )

        bl =
            ( -1, 0, 1 )

        br =
            ( -1, 1, 0 )

        bottom =
            ( -1, 1, 1 )
    in
    { open =
        [ ( 2
          , [ ( top, [ tl, tr ] )
            , ( tl, [ top, bl ] )
            , ( tr, [ top, br ] )
            , ( bl, [ tl, bottom ] )
            , ( br, [ tr, bottom ] )
            , ( bottom, [ bl, br ] )
            ]
                |> Dict.fromList
          )
        ]
            |> Dict.fromList
    , links = Set.empty
    }


type Xml
    = Tag String (List XmlAttribute) (List Xml)
    | Text String


type alias XmlAttribute =
    ( String, String )


toSvgString : ViewOptions -> Grid -> String
toSvgString viewOptions grid =
    let
        toXml : Writer msg XmlAttribute Xml
        toXml =
            { node = \name children -> Tag name (List.filter ((/=) ( "", "" )) children)
            , text = Text
            , attribute = \k v -> ( k, v )
            , on = \_ _ -> ( "", "" )
            }
    in
    encodeXml <| toSvg toXml viewOptions grid


encodeXml : Xml -> String
encodeXml xml =
    case xml of
        Tag name attrs children ->
            let
                attrsString =
                    List.map
                        (\( k, v ) -> " " ++ k ++ "=\"" ++ v ++ "\"")
                        attrs
            in
            String.concat
                ([ "<", name ]
                    ++ attrsString
                    ++ ">"
                    :: List.map encodeXml children
                    ++ [ "</", name, ">" ]
                )

        Text t ->
            t


viewGrid : ViewOptions -> Grid -> Html Msg
viewGrid =
    let
        toHtml : Writer Msg (Svg.Attribute Msg) (Svg Msg)
        toHtml =
            { node = \name attrs children -> VirtualDom.nodeNS "http://www.w3.org/2000/svg" name attrs children
            , text = Svg.text
            , attribute = VirtualDom.attribute
            , on = Svg.Events.on
            }
    in
    toSvg toHtml


type alias Writer msg attr node =
    { node : String -> List attr -> List node -> node
    , text : String -> node
    , attribute : String -> String -> attr
    , on : String -> Json.Decode.Decoder msg -> attr
    }


toSvg :
    Writer Msg attr node
    -> ViewOptions
    -> Grid
    -> node
toSvg ({ node, attribute } as writer) viewOptions grid =
    let
        { minx, miny, maxx, maxy } =
            getPointsBoundingBox pointsList

        pointsList =
            grid.links
                |> Set.toList
                |> List.concatMap (\( a, b ) -> [ a, b ])

        width =
            maxx - minx

        height =
            maxy - miny

        padding =
            min width height / 20

        viewBox =
            [ miny - padding, minx - padding, height + 2 * padding, width + 2 * padding ]
                |> List.map String.fromFloat
                |> String.join " "
                |> attribute "viewBox"

        ( links, buttons ) =
            Set.toList grid.links
                |> List.map (viewLink writer viewOptions grid.links)
                |> List.unzip
    in
    node "svg"
        [ viewBox, attribute "style" <| "stroke:black;stroke-width:" ++ String.fromFloat (scale * 0.02) ]
        (links ++ List.concat buttons)


getRotations : Grid -> List ( Link, Link, Link )
getRotations grid =
    Set.toList grid.links
        |> List.map (getRotationsForLink grid.links)
        |> List.concat


viewLink : Writer Msg attr node -> ViewOptions -> Set Link -> Link -> ( node, List node )
viewLink ({ node, attribute, text, on } as writer) { showLabels, showDots } links (( a, b ) as link) =
    let
        aBox =
            getTriangleBoundingBox a

        bBox =
            getTriangleBoundingBox b

        labels =
            if showLabels then
                [ label writer stroke a
                , label writer stroke b
                ]

            else
                []

        ( points, ( fill, stroke ), buttons ) =
            if b == plusY a then
                ( [ ( aBox.minx, aBox.maxy )
                  , ( aBox.maxx, aBox.maxy )
                  , ( bBox.maxx, aBox.miny )
                  , ( bBox.minx, aBox.miny )
                  ]
                , ( "gray", Nothing )
                , []
                )

            else if b == plusZ a then
                ( [ ( aBox.minx, aBox.maxy )
                  , ( aBox.maxx, aBox.maxy )
                  , ( bBox.maxx, aBox.miny )
                  , ( bBox.minx, aBox.miny )
                  ]
                , ( "black", Just "#555" )
                , []
                )

            else if b == plusX a then
                let
                    rotatButton cx cy ab cd ef =
                        let
                            msg =
                                Rotat ab cd ef
                        in
                        [ node "circle"
                            [ attribute "cx" <| String.fromFloat cx
                            , attribute "cy" <| String.fromFloat cy
                            , attribute "r" "0.4"
                            , attribute "style" "fill:transparent;stroke:transparent"
                            , on "click" <| Json.Decode.succeed msg
                            ]
                            []
                        , if showDots then
                            node "circle"
                                [ attribute "cx" <| String.fromFloat cx
                                , attribute "cy" <| String.fromFloat cy
                                , attribute "r" "0.1"
                                , attribute "style" "fill:blue"
                                , on "click" <| Json.Decode.succeed msg
                                ]
                                []

                          else
                            text ""
                        ]

                    downlink =
                        let
                            d =
                                plusY a

                            e =
                                minusZ b

                            cd =
                                ( minusZ d, d )

                            ef =
                                ( e, plusY e )
                        in
                        if Set.member cd links && Set.member ef links then
                            [ rotatButton aBox.maxy aBox.maxx link cd ef ]

                        else
                            []

                    uplink =
                        let
                            d =
                                plusZ a

                            e =
                                minusY b

                            cd =
                                ( minusY d, d )

                            ef =
                                ( e, plusZ e )
                        in
                        if Set.member cd links && Set.member ef links then
                            [ rotatButton aBox.maxy aBox.minx link cd ef ]

                        else
                            []
                in
                ( [ ( aBox.minx, aBox.maxy )
                  , ( (aBox.minx + aBox.maxx) / 2, bBox.maxy )
                  , ( aBox.maxx, aBox.maxy )
                  , ( (aBox.minx + aBox.maxx) / 2, aBox.miny )
                  ]
                , ( "white", Nothing )
                , downlink ++ uplink
                )

            else
                -- No other options, guaranteed when building the graph
                ( [ ( bBox.minx, bBox.maxy )
                  , ( bBox.maxx, bBox.maxy )
                  , ( aBox.maxx, bBox.miny )
                  , ( aBox.minx, bBox.miny )
                  ]
                , ( "red", Just "red" )
                , []
                )
    in
    ( node "polygon"
        [ points
            |> List.map (\( py, px ) -> String.fromFloat px ++ "," ++ String.fromFloat py)
            |> String.join " "
            |> attribute "points"
        , attribute "style" <| "fill:" ++ fill ++ maybeStrokeStyle stroke
        ]
        []
    , labels ++ List.concat buttons
    )


maybeStrokeStyle : Maybe String -> String
maybeStrokeStyle stroke =
    case stroke of
        Nothing ->
            ""

        Just s ->
            ";stroke:" ++ s


getRotationsForLink : Set Link -> Link -> List ( Link, Link, Link )
getRotationsForLink links (( a, b ) as link) =
    let
        messages =
            if b == plusX a then
                let
                    downlink =
                        let
                            d =
                                plusY a

                            e =
                                minusZ b

                            cd =
                                ( minusZ d, d )

                            ef =
                                ( e, plusY e )
                        in
                        if Set.member cd links && Set.member ef links then
                            [ ( link, cd, ef ) ]

                        else
                            []

                    uplink =
                        let
                            d =
                                plusZ a

                            e =
                                minusY b

                            cd =
                                ( minusY d, d )

                            ef =
                                ( e, plusZ e )
                        in
                        if Set.member cd links && Set.member ef links then
                            [ ( link, cd, ef ) ]

                        else
                            []
                in
                downlink ++ uplink

            else
                []
    in
    messages


label : Writer msg attr node -> Maybe String -> Point -> node
label { node, attribute, text } stroke (( x, y, z ) as p) =
    let
        ( cx, cy ) =
            getCenter p
    in
    node "text"
        [ attribute "style" <|
            "font-size:"
                ++ String.fromFloat (scale * 0.07)
                ++ "pt;stroke-width:"
                ++ String.fromFloat (scale * 0.01)
                ++ maybeStrokeStyle stroke
        , attribute "x" <| String.fromFloat cy
        , attribute "y" <| String.fromFloat cx
        , attribute "textAnchor" "middle"
        ]
        [ text <| String.join " " <| List.map String.fromInt [ x, y, z ] ]


getCenter : Point -> ( Float, Float )
getCenter ( x, y, z ) =
    let
        hThird =
            sqrt 3 / 6

        cy =
            toFloat (x * 2 - y - z) * hThird

        cx =
            0.5 * toFloat (y - z)
    in
    ( cx * scale, cy * scale )


type alias BoundingBox =
    { minx : Float
    , maxx : Float
    , miny : Float
    , maxy : Float
    , cx : Float
    }


getTriangleBoundingBox : Point -> BoundingBox
getTriangleBoundingBox ( x, y, z ) =
    let
        hThird =
            sqrt 3 / 6 * scale

        upwards =
            x + y + z == 1

        ( cx, cy ) =
            getCenter ( x, y, z )

        miny =
            if upwards then
                cy - hThird

            else
                cy - 2 * hThird
    in
    { minx = cx - 0.5 * scale
    , maxx = cx + 0.5 * scale
    , miny = miny
    , maxy = miny + hThird * 3
    , cx = cx
    }


expand : GridBuilder -> GridBuilder
expand s =
    let
        newList =
            s.open
                |> Dict.values
                |> List.concatMap Dict.keys
                |> List.concatMap (\p -> p :: neighbours p)
                |> List.map (\p -> ( p, neighbours p ))
    in
    { open = gatherLinks newList
    , links = Set.empty
    }


gatherLinks : List ( Point, List Point ) -> OpenList
gatherLinks newList =
    let
        newSet =
            Set.fromList <| List.map Tuple.first newList
    in
    newList
        |> List.map
            (\( p, ns ) ->
                ( p
                , List.filter (\n -> Set.member n newSet) ns
                )
            )
        |> List.Extra.gatherEqualsBy (Tuple.second >> List.length)
        |> List.map
            (\( h, t ) ->
                ( List.length <| Tuple.second h
                , Dict.fromList <| h :: t
                )
            )
        |> Dict.fromList


neighbours : Point -> List Point
neighbours ( x, y, z ) =
    if x + y + z == 0 then
        [ ( x + 1, y, z ), ( x, y + 1, z ), ( x, y, z + 1 ) ]

    else
        [ ( x - 1, y, z ), ( x, y - 1, z ), ( x, y, z - 1 ) ]


type alias OpenList =
    Dict Int (Dict Point (List Point))


type alias GridBuilder =
    { open : OpenList
    , links : Set Link
    }


type alias Link =
    ( Point, Point )


type alias Grid =
    { links : Set Link
    }
