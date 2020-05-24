module Main exposing
   (..)

{- | The application main. -}

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Utilities.Display as Display
import Browser
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, div, text, ul, li, h1, h2, h3)
import Html
import Html.Attributes exposing (href)
import Html.Attributes as Attr
import Http
import List
import Markdown
import Time
import Date exposing (Date)
import Date
import Url exposing (Url)
import Url
import Url.Builder as UrlB
import Result
import String
import Task

import Bridge exposing
    (BRecipeSummary, BMealPlan, BRecipeID, BRecipe(..))
import Bridge
import CalEntry exposing (CalEntry, Calendar, DayMeal)
import CalEntry
import Coming exposing (Coming(..))
import Coming
import DateUtil
import ListUtil
import Locale
import Locale exposing (Locale(..), LocaleImpl)
import MealPhase exposing (MealPhase(..))
import MealPhase
import Page exposing (Page(..), recipePageLink)
import Page
import Recipe exposing (recipeName)

---- Model Types

{- | The model.
-}
type alias Model =
    { locale : Locale
    -- | 'Nothing' before initialized.
    , clock : Maybe MClock
    , page : Page
    , navKey : Nav.Key
    , calendar : Calendar
      -- | 'True' if MealPlans are loaded to the calendar.
    , mealPlansLoaded : Coming String ()
    , errorMsg : Maybe String
    , loadedRecipe : Coming String MRecipe  -- TODO: maybe we should move this under PageRecipe varient.
    }

{- | Clock in the model.
-}
type alias MClock =
    { curTime : Time.Posix
    , timeZone : Time.Zone
    }

{- | Loaded recipe in the model.
-}
type alias MRecipe =
    { id : BRecipeID
    , recipe : BRecipe
    }

setClock : Time.Zone -> Time.Posix -> Model -> Model
setClock tz t model = { model | clock = Just { curTime = t, timeZone = tz } }

tickClock : Time.Posix -> Model -> Model
tickClock t model =
    case model.clock of
        Nothing -> model
        Just c -> { model | clock = Just { c | curTime = t } }

initCalendar : Time.Zone -> Time.Posix -> Model -> Model
initCalendar tz t model =
    let cals = CalEntry.forDays (calendarStart tz t) calendarPeriodDays
    in { model | calendar = cals }

modelToday : Model -> Maybe Date
modelToday m = Maybe.map (\mc -> Date.fromPosix mc.timeZone mc.curTime) m.clock

---- Main

main = Browser.application
       { init = appInit
       , view = appView
       , update = appUpdate
       , subscriptions = appSub
       , onUrlRequest = appOnUrlRequest
       , onUrlChange = appOnUrlChange
       }

{- | Main program msg
-}
type Msg = NoOp
         | InitModel
         -- | Initiallize the current time
         | InitTime Time.Posix Time.Zone
         -- | Update the current time
         | TickTime Time.Posix
         | MealPlansLoaded (Result String (List BMealPlan))
         | ErrorMsg String
         | UrlRequestMsg UrlRequest
         | UrlChangeMsg Url
         | RecipeLoaded (Result String MRecipe)

calendarPeriodDays : Int
calendarPeriodDays = 9

{- | Make the start date of the calendar from the current time.
-}
calendarStart : Time.Zone -> Time.Posix -> Date
calendarStart tz t = Date.add Date.Days (-1) <| Date.fromPosix tz t
        
appInit : () -> Url -> Nav.Key -> (Model, Cmd Msg)
appInit _ url key =
    let model_base = { locale = LJaJP
                     , clock = Nothing
                     , page = PageTop
                     , navKey = key
                     , calendar = []
                     , mealPlansLoaded = NotStarted
                     , errorMsg = Nothing
                     , loadedRecipe = NotStarted
                     }
        model = appUrlChange url model_base
        (cmd, modifyModel) = concatCmds <| appUpdateCmd InitModel model
    in (modifyModel model, cmd)

appView : Model -> Document Msg
appView m =
    let result = { title = ListUtil.join " - " (page_title ++ ["kon-board"])
                 , body = viewBody m
                 }
        page_title =
            case m.page of
                PageTop -> []
                PageRecipe _ ->
                    case Coming.success m.loadedRecipe of
                        Nothing -> []
                        Just mr -> [recipeName mr.recipe]
    in result

appUpdate : Msg -> Model -> (Model, Cmd Msg)
appUpdate msg model =
    let new_model = appUpdateModel msg model
        (cmd, modifyModel) = concatCmds <| appUpdateCmd msg new_model
    in (modifyModel new_model, cmd)

appUpdateModel : Msg -> Model -> Model
appUpdateModel msg model =
    case msg of
        NoOp -> model
        InitModel -> model
        InitTime t z -> initCalendar z t <| setClock z t model
        TickTime t -> tickClock t model
        MealPlansLoaded e_mps ->
            case Result.andThen (\mps -> CalEntry.addMealPlans mps model.calendar) e_mps of
                Err e -> { model | errorMsg = Just e, mealPlansLoaded = Failure e }
                Ok new_cal -> { model | calendar = new_cal, mealPlansLoaded = Success () }
        ErrorMsg e -> { model | errorMsg = Just e }
        UrlRequestMsg _ -> model
        UrlChangeMsg u -> appUrlChange u model
        RecipeLoaded e_mr ->
            case e_mr of
                Err e -> { model | errorMsg = Just e, loadedRecipe = Failure e }
                Ok mr -> { model | loadedRecipe = Success mr }

appUrlChange : Url -> Model -> Model
appUrlChange u model = 
    case Page.parseUrl u of
        Nothing -> let err = ("Unknown URL: " ++ Url.toString u)
                   in { model | errorMsg = Just err }
        Just p -> { model | page = p, loadedRecipe = NotStarted }

concatCmds : List (Cmd m, Model -> Model) -> (Cmd m, Model -> Model)
concatCmds cs =
    let result = fstBatch <| List.foldr f ([], identity) cs
        f (c, m) (acc_cmds, acc_mod) = (c :: acc_cmds, m >> acc_mod)
        fstBatch (cmds, m) = (Cmd.batch cmds, m)
    in result

appUpdateCmd : Msg -> Model -> List (Cmd Msg, Model -> Model)
appUpdateCmd msg model =
    let result = initTimeCmd ++ loadMealPlanCmd ++ urlRequestCmd ++ loadRecipeCmd
        initTimeCmd =
            case model.clock of
                Nothing -> [(Task.perform identity <| Task.map2 InitTime Time.now Time.here, identity)]
                Just _ -> []
        loadMealPlanCmd =
            if Coming.hasStarted model.mealPlansLoaded
            then []
            else
                case model.clock of
                    Nothing -> []
                    Just c -> [(loadMealPlans c.curTime c.timeZone
                               , (\m -> { m | mealPlansLoaded = Pending })
                               )]
        urlRequestCmd =
            case msg of
                UrlRequestMsg (Browser.Internal u) ->
                    [(Nav.pushUrl model.navKey <| Url.toString u, identity)]
                UrlRequestMsg (Browser.External s) ->
                    [(Nav.load s, identity)]
                _ -> []
        loadRecipeCmd =
            let mkResult rid = [(loadRecipeByID rid, (\m -> { m | loadedRecipe = Pending }))]
            in case (model.page, Coming.success model.loadedRecipe) of
                   (PageRecipe rid, Nothing) ->
                       if Coming.hasStarted model.loadedRecipe
                       then []
                       else mkResult rid
                   (PageRecipe rid, Just mr) ->
                       if rid == mr.id
                       then []
                       else mkResult rid
                   _ -> []
    in result

appSub : Model -> Sub Msg
appSub _ = Time.every 5000 TickTime

appOnUrlRequest : UrlRequest -> Msg
appOnUrlRequest = UrlRequestMsg

appOnUrlChange : Url -> Msg
appOnUrlChange = UrlChangeMsg

loadMealPlans : Time.Posix -> Time.Zone -> Cmd Msg
loadMealPlans time zone =
    let start_day = calendarStart zone time
        end_day = Date.add Date.Days calendarPeriodDays start_day
        handle ret =
            let mkErrorMsg http_err = "Error in loadMealPlans: " ++ showHttpError http_err
            in MealPlansLoaded <| Result.mapError mkErrorMsg ret
    in Bridge.getApiV1Mealplans (Date.toIsoString start_day) (Date.toIsoString end_day) handle

loadRecipeByID : BRecipeID -> Cmd Msg
loadRecipeByID rid =
    let result = Bridge.getApiV1RecipesByRecipeid rid handle
        handle ret =
            let content = 
                    case ret of
                        Ok r -> Ok <| { id = rid, recipe = r }
                        Err err -> Err <| "Error in loadRecipeByID: " ++ showHttpError err
            in RecipeLoaded content
    in result

showHttpError : Http.Error -> String
showHttpError e =
    case e of
        Http.BadUrl u -> "Bad URL: " ++ u
        Http.Timeout -> "HTTP timeout"
        Http.NetworkError -> "Network error"
        Http.BadStatus s -> "Server returned error status " ++ String.fromInt s
        Http.BadBody b -> "Bad HTTP response body: " ++ b

---- View

viewBody : Model -> List (Html Msg)
viewBody model =
    let result = [ Grid.container []
                       [ Grid.row []
                             [ Grid.col [ Col.md3, Col.lg2
                                        , Col.attrs
                                            [ Attr.class "sidebar"
                                            , Display.none
                                            , Display.blockMd
                                            ]
                                        ] sidebar
                             , Grid.col [ Col.xs12, Col.md9, Col.lg10
                                        , Col.attrs [Attr.class "mainbox"]
                                        ] mainbox
                             ]
                       ]
                 ]
        sidebar = viewCurTime model.locale model.clock ++ err_msg ++ viewNav model.page
        mainbox =
            case model.page of
                PageTop ->
                    case modelToday model of
                        Nothing -> []
                        Just today -> viewCalendar model.locale today model.calendar
                PageRecipe r -> viewRecipePage r model
        err_msg =
            case model.errorMsg of
                Nothing -> []
                Just e -> [ Alert.simpleDanger [] [text ("Error: " ++ e)] ]
    in result

viewCurTime : Locale -> Maybe MClock -> List (Html Msg)
viewCurTime locale mc =
    case mc of
        Nothing -> []
        Just c ->
            let result = [Alert.simpleInfo [Attr.class "clock-panel"] panel_content]
                panel_content =
                    (.viewDateLong) (Locale.get locale) date
                        ++
                        [ div [] [ Html.span [Attr.class "clock-time", Attr.class "text-nowrap"]
                                       [text (hour ++ ":" ++ minute)]
                                 ]
                        ]
                date = Date.fromPosix c.timeZone c.curTime
                hour = String.padLeft 2 '0' <| String.fromInt <| Time.toHour c.timeZone c.curTime
                minute = String.padLeft 2 '0' <| String.fromInt <| Time.toMinute c.timeZone c.curTime
            in result

viewNav : Page -> List (Html Msg)
viewNav p =
    case p of
        PageTop -> []
        PageRecipe _ ->
            [ div []
                  [ Button.linkButton
                        [ Button.secondary
                        , Button.attrs
                              [ href <| UrlB.absolute [] []
                              , Attr.class "kon-nav-button"
                              ]
                        ]
                        [iconBootstrap Nothing "house-door-fill-white" <| Just "Home"]
                  ]
            ]
                  
tableMealPhases : List MealPhase
tableMealPhases = [Lunch, Dinner]
        
viewCalendar : Locale -> Date -> Calendar -> List (Html Msg)
viewCalendar locale today cal = List.concatMap (viewCalEntry locale today) cal
    -- let result = [Table.table { options = opts, thead = thead, tbody = tbody }]
    --     opts = [Table.striped]
    --     thead = Table.thead [] [Table.tr [] head_cells]
    --     mkIcon = iconBootstrap (Just "cal-icon")
    --     head_cells = [ Table.th [Table.cellAttr <| Attr.class "cal-day"]
    --                        [mkIcon "calendar" <| Just <| (.showCalDay) <| Locale.get locale ]
    --                  ]
    --                  ++ List.map mkPhaseHeaderCell tableMealPhases
    --     mkPhaseHeaderCell p = Table.td [] <| mkMealPhaseIcon p
    --     mkMealPhaseIcon p =
    --         let result_icon =
    --                 case m_icon_name of
    --                     Nothing -> []
    --                     Just icon_name -> [mkIcon icon_name (Just <| MealPhase.toString p)]
    --             m_icon_name =
    --                 case p of
    --                     Lunch -> Just "brightness-high-fill"
    --                     Dinner -> Just "moon"
    --                     _ -> Nothing
    --         in result_icon
    --     tbody = Table.tbody [] <| List.map (viewCalEntry locale today) cal
    -- in result

viewCalEntry : Locale -> Date -> CalEntry -> List (Html Msg)
viewCalEntry locale today centry =
    let result = [Grid.row row_attrs [col_date_head, col_date_body]]
        row_attrs = [ Row.attrs [Attr.class "cal-day-row", Attr.class stripe_class]
                    ]
        stripe_class = if modBy 2 (Date.toRataDie centry.day) == 0
                       then "cal-day-row-even"
                       else "cal-day-row-odd"
        col_date_head = Grid.col
                        [Col.xs3, Col.md2]
                        [div date_head_attrs <| (.viewDateShort) (Locale.get locale) centry.day]
        date_head_attrs = [Attr.class "cal-day"]
                          ++ if today == centry.day
                             then [Attr.class "cal-today"]
                             else []
        col_date_body = Grid.col [Col.xs9, Col.md10] [Grid.row [] <| List.map mkColForPhase tableMealPhases]
        mkColForPhase p = Grid.col [Col.xs12, Col.sm6] <| viewDayMeal p <| CalEntry.mealFor p centry
    in result

viewDayMeal : MealPhase -> Maybe DayMeal -> List (Html Msg)
viewDayMeal phase mdm =
    let result = [div [] [ul [] list_content]]
        list_content =
            case mdm of
                Nothing -> []
                Just dm -> List.map mkRecipe dm.recipes
        mkRecipe r = li [] <| viewLinkRecipe r.id [text r.name]
    in result

viewLinkRecipe : BRecipeID -> List (Html a) -> List (Html a)
viewLinkRecipe rid content =
    let result = [Html.a attrs content]
        attrs = [href <| recipePageLink rid]
    in result

viewRecipePage : BRecipeID -> Model -> List (Html Msg)
viewRecipePage rid model =
    let result = recipe_body
                 ++ [div [Attr.class "recipe-id-box"] [text ("Recipe ID: " ++ rid)]]
        recipe_body =
            case Coming.success model.loadedRecipe of
                Nothing -> []
                Just mr -> viewRecipe model.locale mr.recipe
    in result

viewRecipe : Locale -> BRecipe -> List (Html Msg)
viewRecipe locale br =
    let result = [div [Attr.class "recipe-box"] recipe_content]
        recipe_content = 
            case br of
                BRIn rin -> viewRecipeIn rin
                BRURL ru -> viewRecipeURL ru
                BRExt re -> viewRecipeExt re
        viewName n = [h1 [] [text n]]
        viewIngDescs ings = [ h2 [] [text <| (.showIngredients) <| Locale.get locale]
                            , ul [] <| List.concatMap viewIngDesc ings
                            ]
        viewIngDesc ing =
            case ing.group of
                Nothing -> List.map viewIng ing.ings
                Just g -> [ li [] [text g]
                          , ul [] <| List.map viewIng ing.ings
                          ]
        viewIng ing = li [] <| (.viewIngredient) (Locale.get locale) ing
        viewDesc desc = [h2 [] [text <| (.showRecipeSteps) <| Locale.get locale]] ++ Markdown.toHtml Nothing desc
        viewRefURL src murl =
            let ret_refurl = [ h2 [] [text <| (.showRecipeReference) <| Locale.get locale]
                         , ul [] [li [] ref_body]
                         ]
                ref_body =
                    case murl of
                        Nothing -> [text src]
                        Just url -> [Html.a [href url, Attr.target "_blank"] [text src]]
            in ret_refurl
        viewRecipeIn rin = viewName rin.name ++ viewIngDescs rin.ings
                           ++ viewDesc rin.desc
                           ++ ( case rin.ref_url of
                                    Nothing -> []
                                    Just u -> viewRefURL u (Just u)
                              )
        viewRecipeURL ru = viewName ru.name ++ viewRefURL ru.url (Just ru.url)
        viewRecipeExt re = viewName re.name ++ viewRefURL re.source re.ext_url
    in result

iconBootstrap : Maybe String -> String -> Maybe String -> Html Msg
iconBootstrap mclass icon_name malt =
    let result = Html.img attrs []
        attrs = [ Attr.src ("/static/icons/twbs/" ++ icon_name ++ ".svg")
                , Attr.alt <| Maybe.withDefault "" malt
                ]
                ++ case mclass of
                       Nothing -> []
                       Just c -> [Attr.class c]
    in result
