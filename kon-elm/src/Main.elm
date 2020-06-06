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
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Html exposing (Html, div, text, ul, li, h1, h2, h3)
import Html
import Html.Attributes exposing (href)
import Html.Attributes as Attr
import Http
import List
import Markdown
import Task exposing (Task)
import Time
import Date exposing (Date)
import Date
import Url exposing (Url)
import Url
import Url.Builder as UrlB
import Process
import Result
import String
import Task
import Tuple exposing (first, second)

import Bridge exposing
    (BRecipeSummary, BMealPlan, BRecipeID, BRecipe(..))
import Bridge
import Calendar exposing (CalEntry, Calendar, DayMeal)
import Calendar as Cal
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
    , calendar : Maybe Calendar
      -- | 'True' if MealPlans are loaded to the calendar.
    , mealPlansLoaded : Coming String ()
    , errorMsg : (Alert.Visibility, String)
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

calendarInitStartWeeks : Int
calendarInitStartWeeks = 4

calendarInitEndWeeks : Int
calendarInitEndWeeks = 2

initCalendar : Time.Zone -> Time.Posix -> Model -> Model
initCalendar tz t model =
    let result = { model | calendar = Just init_cal }
        init_cal = Cal.forWeeks cur_date Time.Sun weeks_future weeks_past
        cur_date = Date.fromPosix tz t
        weeks_future = 2
        weeks_past = 4
    in result

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
         | ErrorMsgVisibility Alert.Visibility
         | UrlRequestMsg UrlRequest
         | UrlChangeMsg Url
         | RecipeLoaded (Result String MRecipe)
         | ViewportAdjusted (Result String ())

appInit : () -> Url -> Nav.Key -> (Model, Cmd Msg)
appInit _ url key =
    let model_base = { locale = LJaJP
                     , clock = Nothing
                     , page = Page.initPage
                     , navKey = key
                     , calendar = Nothing
                     , mealPlansLoaded = NotStarted
                     , errorMsg = (Alert.closed, "")
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
                PageTop _ -> []
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
            let e_new_cal =
                    e_mps |> Result.andThen
                    (\ mps -> e_cal |> Result.andThen
                         (\ cal ->
                              Cal.addMealPlans mps cal
                         )
                    )
                e_cal = Result.fromMaybe "MealPlansLoaded: Calendar is not initialized yet." model.calendar
            in case e_new_cal of
                   Err e -> { model | errorMsg = (Alert.shown, e), mealPlansLoaded = Failure e }
                   Ok new_cal -> { model | calendar = Just new_cal, mealPlansLoaded = Success () }
        ErrorMsgVisibility v -> { model | errorMsg = (v, second model.errorMsg) }
        UrlRequestMsg _ -> model
        UrlChangeMsg u -> appUrlChange u model
        RecipeLoaded e_mr ->
            case e_mr of
                Err e -> { model | errorMsg = (Alert.shown, e), loadedRecipe = Failure e }
                Ok mr -> { model | loadedRecipe = Success mr }
        ViewportAdjusted adjust_ret ->
            case model.page of
                PageTop v ->
                    case adjust_ret of
                        Ok () -> { model | page = PageTop (Success ()) }
                        Err e -> { model | errorMsg = (Alert.shown, "ViewportAdjust error: " ++ e)
                                 , page = PageTop (Failure e)
                                 }
                _ -> { model | errorMsg = (Alert.shown, "Unexpected ViewportAdjust msg to non-PageTop page.") }

appUrlChange : Url -> Model -> Model
appUrlChange u model = 
    case Page.parseUrl u of
        Nothing -> let err = ("Unknown URL: " ++ Url.toString u)
                   in { model | errorMsg = (Alert.shown, err) }
        Just p -> { model | page = p, loadedRecipe = NotStarted }

concatCmds : List (Cmd m, Model -> Model) -> (Cmd m, Model -> Model)
concatCmds cs =
    let result = fstBatch <| List.foldr f ([], identity) cs
        f (c, m) (acc_cmds, acc_mod) = (c :: acc_cmds, m >> acc_mod)
        fstBatch (cmds, m) = (Cmd.batch cmds, m)
    in result

appUpdateCmd : Msg -> Model -> List (Cmd Msg, Model -> Model)
appUpdateCmd msg model =
    let result = initTimeCmd ++ loadMealPlanCmd ++ urlRequestCmd
                 ++ loadRecipeCmd ++ viewportAdjustCmd
        initTimeCmd =
            case model.clock of
                Nothing -> [(Task.perform identity <| Task.map2 InitTime Time.now Time.here, identity)]
                Just _ -> []
        loadMealPlanCmd =
            if Coming.hasStarted model.mealPlansLoaded
            then []
            else
                case model.calendar of
                    Nothing -> []
                    Just cal ->
                        let (start, end) = Cal.startAndEnd cal
                        in [ ( loadMealPlans start end
                             , (\m -> { m | mealPlansLoaded = Pending })
                             )
                           ]
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
        viewportAdjustCmd =
            case (model.calendar, model.mealPlansLoaded, model.page) of
                (Just _, Success _, PageTop NotStarted) ->
                    let cmd = Task.attempt ViewportAdjusted
                              <| setCalendarViewportTask relative_adjust
                        new_page = PageTop Pending
                        relative_adjust = -10.0
                    in [(cmd, (\m -> { m | page = new_page }))]
                _ -> []
    in result

appSub : Model -> Sub Msg
appSub model =
    Sub.batch
        [ Time.every 5000 TickTime
        ]

appOnUrlRequest : UrlRequest -> Msg
appOnUrlRequest = UrlRequestMsg

appOnUrlChange : Url -> Msg
appOnUrlChange = UrlChangeMsg

loadMealPlans : Date -> Date -> Cmd Msg
loadMealPlans start_day end_day =
    let handle ret =
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

{- | Task to set viewport (y position) relative to the element of
"today".
-}
setCalendarViewportTask : Float -> Task String ()
setCalendarViewportTask rel_y =
    let result = Task.mapError toString <| action
        action =
            Dom.getElement todayCellID |> Task.andThen
            (\ elem ->
                 let new_x = elem.viewport.x
                     new_y = elem.element.y + rel_y
                 in Dom.setViewport new_x new_y
            )
        toString (Dom.NotFound e) =
            "Cannot find #" ++ todayCellID ++ ": " ++ e
    in result
    

---- View

viewBody : Model -> List (Html Msg)
viewBody model =
    let result = viewNavbar model.page ++ main_container ++ err_msg
        main_container =
            [ Grid.container []
                  [ Grid.row []
                        [ Grid.col
                              [ Col.md3, Col.lg2
                              , Col.attrs
                                  [ Display.none
                                  , Display.blockMd
                                  ]
                              ]
                              [div [Attr.class "sidebar"] sidebar]
                        , Grid.col [ Col.xs12, Col.md9, Col.lg10
                                   , Col.attrs [Attr.class "mainbox"]
                                   ] mainbox
                        ]
                  ]
            ]
        sidebar = viewCurTime model.locale model.clock
        mainbox =
            case model.page of
                PageTop _ ->
                    case (modelToday model, model.calendar) of
                        (Just today, Just cal) -> viewCalendar model.locale today cal
                        _ -> []
                PageRecipe r -> viewRecipePage r model
        err_msg =
            let alert_conf =
                    Alert.children [text <| second model.errorMsg]
                    <| Alert.dismissable ErrorMsgVisibility
                    <| Alert.danger
                    <| Alert.config
            in [Alert.view (first model.errorMsg) alert_conf]
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

viewNavbar : Page -> List (Html Msg)
viewNavbar page =
    let result = [ Html.nav
                       (List.map Attr.class ["navbar", "fixed-top", "navbar-light", "bg-light"])
                       navbar_content
                 ]
        navbar_content =
            [ Html.form [Attr.class "form-inline"]
              [ Html.a [href <| UrlB.absolute [] []]
                    [ Html.img
                          [ Attr.src <| iconPath "d/kon.svg"
                          , Attr.alt "Home"
                          , Attr.width 16
                          , Attr.height 16
                          ]
                          []
                    ]
              ]
            ]
    in result

tableMealPhases : List MealPhase
tableMealPhases = [Lunch, Dinner]
        
viewCalendar : Locale -> Date -> Calendar -> List (Html Msg)
viewCalendar locale today cal = List.concatMap (viewCalEntry locale today) <| Cal.entries cal

todayCellID : String
todayCellID = "cal-today-cell"

viewCalEntry : Locale -> Date -> CalEntry -> List (Html Msg)
viewCalEntry locale today centry =
    let result = [Grid.row row_attrs [col_date_head, col_date_body]]
        row_attrs = [ Row.attrs ([Attr.class "cal-day-row", Attr.class stripe_class] ++ today_row_attrs)
                    ]
        today_row_attrs = if today == centry.day
                          then [Attr.id todayCellID]
                          else []
        stripe_class = if modBy 2 (Date.toRataDie centry.day) == 0
                       then "cal-day-row-even"
                       else "cal-day-row-odd"
        col_date_head = Grid.col
                        [Col.xs3, Col.md2, Col.attrs [Attr.class "cal-col-entry-head"]]
                        [div date_head_attrs <| (.viewDateShort) (Locale.get locale) centry.day]
        date_head_attrs = [Attr.class "cal-day"]
                          ++ if today == centry.day
                             then [Attr.class "cal-today"]
                             else []
        col_date_body = Grid.col
                        [Col.xs9, Col.md10]
                        [Grid.row [] <| List.map mkColForPhase tableMealPhases]
        mkColForPhase p = Grid.col [Col.xs12, Col.sm6, Col.attrs [Attr.class "cal-col-meal-plan"]] <| viewDayMeal p <| Cal.mealFor p centry
    in result

viewDayMeal : MealPhase -> Maybe DayMeal -> List (Html Msg)
viewDayMeal phase mdm =
    let result = [div [Attr.class "cal-phase", Attr.class mp_class] [ul [Attr.class "cal-meal-plan-list"] list_content]]
        mp_class =
            case phase of
                Breakfast -> "cal-phase-breakfast"
                Lunch -> "cal-phase-lunch"
                Dinner -> "cal-phase-dinner"
                MealOther _ -> "cal-phase-other"
        list_content =
            case mdm of
                Nothing -> []
                Just dm -> List.map mkRecipe dm.recipes
        mkRecipe r = li [Attr.class "cal-meal-plan-item-meal"] <| viewLinkRecipe r.id [text r.name]
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
                         , ul [] [li [Attr.class "recipe-ref"] ref_body]
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

iconPath : String -> String
iconPath icon_path = "/static/icons/" ++ icon_path

iconImg : Maybe String -> String -> Maybe String -> Html Msg
iconImg mclass icon_path malt =
    let result = Html.img attrs []
        attrs = [ Attr.src <| iconPath icon_path
                , Attr.alt <| Maybe.withDefault "" malt
                ]
                ++ case mclass of
                       Nothing -> []
                       Just c -> [Attr.class c]
    in result

iconBootstrap : Maybe String -> String -> Maybe String -> Html Msg
iconBootstrap mclass icon_name malt = iconImg mclass ("twbs/" ++ icon_name ++ ".svg") malt
