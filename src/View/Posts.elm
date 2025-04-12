module View.Posts exposing (..)

import Html exposing (Html, div, text, table, tbody, thead, tr, th, td, select, option, input)
import Html.Attributes exposing (href, type_, class)
import Html.Events exposing (onInput)
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), sortFromString, sortOptions, sortToString)
import Time
import Util.Time exposing (formatTime, formatDuration, durationBetween)
import Html exposing (a)
import Html exposing (label)
import Html.Attributes exposing (value)
import Html.Attributes exposing (selected)
import Html.Attributes exposing (for)
import Html.Attributes exposing (id)
import Html.Attributes exposing (checked)
import Model.PostsConfig exposing (Change(..))
import Html.Events exposing (onCheck)
import Model.PostsConfig exposing (Change(..))
import Model.PostsConfig exposing (Change(..))
import Model.PostsConfig as PostsConfig


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable config currentTime posts =
    let
        filteredPosts =
            PostsConfig.filterPosts config posts
    in
    
    div []
        [ table [ class "post-table" ]
            [ thead []
                [ tr []
                    [ th [ class "post-score" ] [ text "Score" ]
                    , th [ class "post-title" ] [ text "Title" ]
                    , th [ class "post-type" ] [ text "Type" ]
                    , th [ class "post-time" ] [ text "Posted Date" ]
                    , th [ class "post-url" ] [ text "Link" ]
                    ]
                ]
            , tbody [] (List.map (postRow currentTime) filteredPosts)
            ]
        ]

maybeStringToString : Maybe String -> String
maybeStringToString maybeString =
    case maybeString of
        Just value ->
            value

        Nothing ->
            "No link"

postRow : Time.Posix -> Post -> Html Msg
postRow currentTime post =
    let
        formattedTime =
            formatTime Time.utc post.time

        linkAttributes =
            [ href (maybeStringToString post.url), class "post-url" ]

        postDuration =
            case durationBetween post.time currentTime of
                Just duration ->
                    formatDuration duration

                Nothing ->
                    "Unknown duration"
    in
    tr []
        [ td [ class "post-score" ] [ text <| String.fromInt post.score ]
        , td [ class "post-title" ] [ text post.title ]
        , td [ class "post-type" ] [ text post.type_ ]
        , td [ class "post-time" ] [ text <| formattedTime ++ " (" ++ postDuration ++ ")" ]
        , td [] [ a linkAttributes [ text "Link" ] ]        ]


{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}

maybeIntToInt : Maybe Int -> Int
maybeIntToInt maybeString =
    case maybeString of
        Just value ->
            value

        Nothing ->
            0

postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    div []
        [ label [ for "select-posts-per-page" ] [ text "Show:" ]
        , select [ id "select-posts-per-page", onInput (\x -> String.toInt x |> Maybe.withDefault 0 |> ChangePostsPerPage >> ConfigChanged) ]
            [ option [ value "10", selected (config.postsToShow == 10) ] [ text "10" ]
            , option [ value "25", selected (config.postsToShow == 25) ] [ text "25" ]
            , option [ value "50", selected (config.postsToShow == 50) ] [ text "50" ]
            ]
        
        , label [ for "select-sort-by" ] [ text "Sort by:" ]
        , select [ id "select-sort-by", onInput (\x -> ConfigChanged (ChangeSortBy (sortFromString x |> Maybe.withDefault None))) ]
            (List.map (\sort ->
                option [ value (sortToString sort), selected (config.sortBy == sort) ] [ text (sortToString sort) ]
            ) sortOptions)
        --Query.find [ tag "input", attribute "type" "checkbox", attribute "id" "checkbox-show-job-posts" ]
        , label [ for "checkbox-show-job-posts" ] 
    [ input 
        [ id "checkbox-show-job-posts"
        , type_ "checkbox"
        , onCheck (\checked -> ConfigChanged (ToggleShowJobs checked))
        , checked config.showJobs 
        ] 
        []
    , text "Show job posts"
    ]

    

        , label [ for "checkbox-show-text-only-posts" ] 
    [ input 
        [ id "checkbox-show-text-only-posts"
        , type_ "checkbox"
        , onCheck (\checked -> ConfigChanged (ToggleShowTextOnly checked))
        , checked config.showTextOnly 
        ] 
        []
    , text "Show text-only posts"
    ]

    ]

