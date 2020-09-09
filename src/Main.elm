module Main exposing (main)

import Browser
import Html exposing (Html, br, button, div, hr, input, span, text)
import Html.Attributes exposing (readonly, style, type_, value)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- TYPES


type Operator
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | None


type alias Model =
    { input : Int
    , result : Int
    , operation : Operator
    }


type Msg
    = Number Int
    | Operation Operator
    | Calculate
    | Clear



-- INIT


init : Model
init =
    { input = 0
    , result = 0
    , operation = None
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Number n ->
            { model | input = model.input * 10 + n }

        Operation op ->
            update Calculate model
                |> (\m -> { m | operation = op })

        Calculate ->
            case model.operation of
                Addition ->
                    { model | result = model.result + model.input, input = 0, operation = None }

                Subtraction ->
                    { model | result = model.result - model.input, input = 0, operation = None }

                Multiplication ->
                    { model | result = model.result * model.input, input = 0, operation = None }

                Division ->
                    { model | result = model.result // model.input, input = 0, operation = None }

                None ->
                    if model.input /= 0 then
                        { model | result = model.input, input = 0, operation = None }

                    else
                        model

        Clear ->
            init



-- VIEW


view : Model -> Html Msg
view model =
    let
        buttons : List (List Msg)
        buttons =
            [ [ Number 1, Number 2, Number 3 ]
            , [ Number 4, Number 5, Number 6 ]
            , [ Number 7, Number 8, Number 9 ]
            , [ Operation Addition, Number 0, Operation Subtraction ]
            , [ Operation Multiplication, Operation Division, Calculate ]
            , [ Clear ]
            ]

        operatorToString : Operator -> String
        operatorToString op =
            case op of
                Addition ->
                    "+"

                Subtraction ->
                    "-"

                Multiplication ->
                    "*"

                Division ->
                    "/"

                None ->
                    ""

        msgToString : Msg -> String
        msgToString msg =
            case msg of
                Number n ->
                    String.fromInt n

                Operation op ->
                    operatorToString op

                Calculate ->
                    "="

                Clear ->
                    "C"

        buttonFromMsg : Msg -> Html Msg
        buttonFromMsg msg =
            button [ onClick msg, style "width" "33%" ] [ text <| msgToString msg ]

        buttonsHtml : List (Html Msg)
        buttonsHtml =
            buttons
                |> List.map (List.map buttonFromMsg)
                |> List.intersperse [ br [] [] ]
                |> List.concat
    in
    div
        [ style "background-color" "#0A7CC4"
        , style "box-shadow" "5px 5px #075485"
        , style "width" "200px"
        , style "padding" "10px"
        , style "margin-left" "auto"
        , style "margin-right" "auto"
        ]
        ([ input [ type_ "text", readonly True, style "width" "80%", value <| String.fromInt model.result ] []
         , span [] [ text " " ]
         , span [] [ model.operation |> operatorToString |> text ]
         , input [ type_ "text", readonly True, style "width" "80%", value <| String.fromInt model.input ] []
         , hr [] []
         ]
            ++ buttonsHtml
        )
