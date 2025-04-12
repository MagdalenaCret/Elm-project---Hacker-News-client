module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)

import Html.Attributes exposing (scope)
import Model.Post exposing (Post)
import Time


type SortBy
    = Score
    | Title
    | Posted
    | None


sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]


sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"

        Title ->
            "Title"

        Posted ->
            "Posted"

        None ->
            "None"


{-|

    sortFromString "Score" --> Just Score

    sortFromString "Invalid" --> Nothing

    sortFromString "Title" --> Just Title

-}
sortFromString : String -> Maybe SortBy
sortFromString str =
    case String.toLower str of
        "score" ->
            Just Score

        "title" ->
            Just Title

        "posted" ->
            Just Posted

        "none" ->
            Just None

        _ ->
            Nothing

    
sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score ->
            \postA postB -> compare postB.score postA.score

        Title ->
            \postA postB -> compare postA.title postB.title

        Posted ->
            \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ


type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }

defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True



{-| A type that describes what option changed and how
-}
type Change
    = ChangePostsPerPage Int
    | ChangeSortBy SortBy
    | ToggleShowJobs Bool
    | ToggleShowTextOnly Bool


{-| Given a change and the current configuration, return a new configuration with the changes applied
-}
applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges change config =
    case change of
        ChangePostsPerPage newCount ->
            { config | postsToShow = newCount }

        ChangeSortBy newSort ->
            { config | sortBy = newSort }

        ToggleShowJobs showJobs ->
            { config | showJobs = showJobs }

        ToggleShowTextOnly showTextOnly ->
            { config | showTextOnly = showTextOnly }


        

{-| Given the configuration and a list of posts, return the relevant subset of posts according to the configuration

Relevant local functions:

  - sortToCompareFn

Relevant library functions:

  - List.sortWith

-}
filterPosts : PostsConfig -> List Post -> List Post
filterPosts config posts =
    let
        filteredJobs =
            if config.showJobs then
                posts
            else
                List.filter (\post -> config.showJobs || post.type_ /= "job") posts
        filteredTextOnly =
            if config.showTextOnly then
                filteredJobs
            else
                List.filter (\post -> post.url /= Nothing) filteredJobs
        limitedPosts =
            List.take config.postsToShow filteredTextOnly
        sortedPosts =
            List.sortWith (sortToCompareFn config.sortBy) limitedPosts
    in
    if List.isEmpty sortedPosts then
        []
    else
        sortedPosts