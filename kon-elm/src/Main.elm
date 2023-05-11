port module Main exposing
   (..)

{- | The application main. -}

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Spinner as Spinner
import Bootstrap.Utilities.Display as Display
import Browser
import Browser exposing (Document, UrlRequest)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Date exposing (Date)
import Date
import FeatherIcons as FIcons
import Html exposing (Html, div, text, ul, li, h1, h2, h3)
import Html
import Html.Attributes exposing (href)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode exposing (Value)
import List
import Markdown
import Maybe.Extra exposing (isNothing)
import Process
import Result
import String
import Task exposing (Task)
import Task
import Time
import Tuple exposing (first, second)
import Url exposing (Url)
import Url
import Url.Builder as UrlB

import Bridge exposing
    (BRecipeStored, BMealPlan, BRecipeId, BIngDesc(..))
import Bridge
import CalSpy exposing
    ( CalLayout
    , todayCellID, monthAnchorCellID
    , relativeCalendarViewportY, setCalendarViewportTask, getCalLayoutTask
    )
import CalSpy
import Calendar exposing (CalEntry, Calendar, DayMeal, MonthAnchor)
import Calendar as Cal
import Coming exposing (Coming(..), isPending)
import Coming
import DateUtil
import HttpUtil exposing (showHttpError)
import ListUtil
import Locale
import Locale exposing (Locale(..), LocaleImpl)
import MealPhase exposing (MealPhase(..))
import MealPhase
import MealPlanLoader exposing (MealPlanLoader)
import MealPlanLoader
import Page exposing (Page(..), recipePageLink, PRecipeModel, PDayModel, dayPageLink, PRecipeSearchModel)
import Page

---- Ports

port portOnScroll : (Value -> msg) -> Sub msg

---- Model Types

{- | The model.
-}
type alias Model =
    { locale : Locale
    -- | 'Nothing' before initialized.
    , clock : Maybe MClock
    , page : Page
    , navKey : Nav.Key
    , navbarMenuState : NavbarMenuState
    , calendar : Maybe Calendar
    -- | Y position of the viewport (in a calendar view) relative to the "today" element.
    , calendarViewport : Float
    , calendarViewType : CalendarView
    , mealPlanLoader : Coming String MealPlanLoader
    , errorMsg : (Alert.Visibility, String)
    }

{- | Clock in the model.
-}
type alias MClock =
    { curTime : Time.Posix
    , timeZone : Time.Zone
    }

{- | newtype for 'Dropdown.State' of navbar menu.
-}
type NavbarMenuState = NavbarMenuState Dropdown.State

{- | Type of calendar view.
-}
type CalendarView = CalViewList
                  | CalViewTable

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
         -- | Current time is initialized
         | InitTime Time.Posix Time.Zone
         -- | Update the current time
         | TickTime Time.Posix
         -- | MealPlanLoader is initialized with the initial meal plans.
         | InitMealPlanLoader (Result String (MealPlanLoader, List BMealPlan))
         -- | Change visibility of error message box
         | ErrorMsgVisibility Alert.Visibility
         -- | UrlRequest browser event
         | UrlRequestMsg UrlRequest
         -- | UrlChange browser event
         | UrlChangeMsg Url
         -- | Got result of loading a Recipe from backend.
         | RecipeLoaded (Result String (BRecipeId, BRecipeStored))
         -- | Set viewport of calendar relative to the "today" element.
         | ViewportSet Float
         -- | Got result of adjusting viewport.
         | ViewportAdjusted (Result String ())
         -- | Got result of getting the calendar layout.
         | CalLayoutObtained (Result String CalLayout)
         -- | Got window.onscroll event (from a port) for a calendar view.
         | CalendarScrollEvent
         -- | Navbar menu is updated.
         | NavbarMenuUpdate NavbarMenuState
         -- | Calendar view style has been changed.
         | CalViewChanged CalendarView
         -- | Pressed a button to load more calendar entries.
         | CalLoadMore LoadMore
         -- | "Load more" is done.
         | CalLoadMoreDone MealPlanLoader (Result String (List BMealPlan))
         -- | Finish loading meal plans for a day
         | DayMealPlanLoaded (Result String (Date, List BMealPlan))
         | MsgRecipeSearch Page.MsgRecipeSearch

{- | Type of "loadMore".
-}
type LoadMore = LoadMorePast
              | LoadMoreFuture

loadMoreWidth : Int -> LoadMore -> Int
loadMoreWidth w lm =
    let result = sign * abs w
        sign =
            case lm of
                LoadMorePast -> (-1)
                LoadMoreFuture -> 1
    in result

isLoading : Model -> Bool
isLoading m =
    let result = Page.isLoading m.page || model_loading
        model_loading = isNothing m.clock || isNothing m.calendar || isPending m.mealPlanLoader 
    in result

initialViewport : Float
initialViewport = -130.0

appInit : () -> Url -> Nav.Key -> (Model, Cmd Msg)
appInit _ url key =
    let model_base = { locale = LJaJP
                     , clock = Nothing
                     , page = Page.initPage
                     , navKey = key
                     , navbarMenuState = NavbarMenuState Dropdown.initialState
                     , calendar = Nothing
                     , calendarViewport = initialViewport
                     , calendarViewType = CalViewList
                     , mealPlanLoader = NotStarted
                     , errorMsg = (Alert.closed, "")
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
                PageRecipe t ->
                    case Coming.success t.recipe of
                        Nothing -> []
                        Just r -> [r.name]
                PageDay d -> [(.showDateYMDA) (Locale.get m.locale) d.day]
                PageRecipeSearch _ -> []
    in result

appUpdate : Msg -> Model -> (Model, Cmd Msg)
appUpdate msg model =
    let (reactedModel, reactCmds) = appUpdateReact msg model
        (cmd, modifyModel) = concatCmds <| appUpdateCmd msg reactedModel
    in (modifyModel reactedModel, Cmd.batch (reactCmds ++ [cmd]))

-- In this program, `appUpdate` is composed of `appUpdateModel : Msg -> Model -> Model` and `appUpdateCmd : Msg -> Model -> List (Cmd Msg, Model -> Model)`.
-- However, now I think we should use `appUpdateReact : Msg -> Model -> (Cmd Msg, Model)` and `appUpdateAuto : Model -> Maybe (Cmd Msg, Model)`.
-- We call appUpdateAuto recursively until we get Nothing. All Cmds obtained are combined and sent to the Elm runtime.

addMealPlansToModel : String -> Result String (List BMealPlan) -> Model -> Result String Model
addMealPlansToModel error_label ret_mps model =
    let result = 
            ret_mps |> Result.andThen
            ( \mps -> e_cal |> Result.andThen
              ( \cal -> Cal.addMealPlans mps cal |> Result.andThen
                ( \new_cal -> Ok { model | calendar = Just new_cal }
                )
              )
            )
        e_cal = Result.fromMaybe (error_label ++ ": Calendar is not initialized yet.") model.calendar
    in result

triggerViewportAdjust : Model -> Model
triggerViewportAdjust model =
    case model.page of
        PageTop tm -> { model | page = PageTop { tm | viewportAdjusted = NotStarted } }
        _ -> model

appUpdateReact : Msg -> Model -> (Model, List (Cmd Msg))
appUpdateReact msg model = 
    case msg of
        NoOp -> (model, [])
        InitModel -> (model, [])
        InitTime t z -> (initCalendar z t <| setClock z t model, [])
        TickTime t -> (tickClock t model, [])
        InitMealPlanLoader ret ->
            let setErrorToModel e =
                    { model | errorMsg = (Alert.shown, e), mealPlanLoader = Failure e }
                ret_updated_model =
                    addMealPlansToModel "InitMealPlanLoader" (Result.map (\(_, c) -> c) ret) model
                final_ret =
                    ret |> Result.andThen
                    ( \(loader, _) -> ret_updated_model |> Result.andThen
                      ( \updated_model -> Ok { updated_model | mealPlanLoader = Success loader }
                      )
                    )
            in case final_ret of
                   Err e -> (setErrorToModel e, [])
                   Ok m -> (m, [])
        ErrorMsgVisibility v -> ({ model | errorMsg = (v, second model.errorMsg) }, [])
        UrlRequestMsg r ->
            case r of
                Browser.Internal u -> (model, [Nav.pushUrl model.navKey <| Url.toString u])
                Browser.External s -> (model, [Nav.load s])
        UrlChangeMsg u -> (appUrlChange u model, [])
        RecipeLoaded ret ->
            let setError e =
                    case model.page of
                        PageRecipe rm -> { model | errorMsg = (Alert.shown, e), page = PageRecipe <| { rm | recipe = Failure e } }
                        _ -> { model | errorMsg = (Alert.shown, e) }
                resultModel =
                    case ret of
                        Err e -> setError e
                        Ok (rid, r) ->
                            case model.page of
                                PageRecipe rm ->
                                    if rm.recipeID == rid
                                    then { model | page = PageRecipe <| { rm | recipe = Success r } }
                                    else setError ("Got recipe for " ++ rid ++ ", but expects " ++ rm.recipeID)
                                _ -> setError ("Got recipe for " ++ rid ++ ", but the page is not PageRecipe.")
            in (resultModel, [])
        ViewportSet rel_pos ->
            let ad_model = triggerViewportAdjust model
            in ({ ad_model | calendarViewport = rel_pos }, [])
        ViewportAdjusted adjust_ret ->
            let resultModel = 
                    case model.page of
                        PageTop pt ->
                            case adjust_ret of
                                Ok () -> { model | page = PageTop { pt | viewportAdjusted = Success () } }
                                Err e -> { model | errorMsg = (Alert.shown, "ViewportAdjust error: " ++ e)
                                         , page = PageTop { pt | viewportAdjusted = Failure e }
                                         }
                        _ -> { model | errorMsg = (Alert.shown, "Unexpected ViewportAdjust msg to non-PageTop page.") }
                resultCmds =
                    case model.page of
                        PageTop _ -> cmdCalLayoutObtain model
                        _ -> []
            in (resultModel, resultCmds)
        CalLayoutObtained ret_cl ->
            let resultModel =
                    case ret_cl of
                        Ok cl ->
                            let new_page =
                                    case model.page of
                                        PageTop pt ->
                                            PageTop { pt | currentAnchor = Success <| CalSpy.currentMonthAnchor (navbarHeight model) cl }
                                        _ -> model.page
                            in { model | calendarViewport = relativeCalendarViewportY cl, page = new_page }
                        Err e -> { model | errorMsg = (Alert.shown, "CalLayoutObtained error: " ++ e) }
            in (resultModel, [])
        CalendarScrollEvent ->
            let cmds =
                    case model.page of
                        PageTop pt ->
                            case pt.viewportAdjusted of
                                Success () -> cmdCalLayoutObtain model
                                _ -> []
                        _ -> []
            in (model, cmds)
        NavbarMenuUpdate s -> ({ model | navbarMenuState = s }, [])
        CalViewChanged cv ->
            let newModel = { model | calendarViewType = cv }
                cmds =
                    case model.page of
                        PageTop _ -> cmdCalLayoutObtain model
                        _ -> []
            in (newModel, cmds)
        CalLoadMore direction ->
            case (model.mealPlanLoader, model.calendar) of
                (Success mploader, Just cal) ->
                    let extend_width = loadMoreWidth 4 direction
                        (new_cal, load_start, load_end) = Cal.extend extend_width cal
                        load_cmd = MealPlanLoader.loadMore load_start load_end mploader CalLoadMoreDone
                    in ({ model | mealPlanLoader = Pending, calendar = Just new_cal }, [load_cmd])
                _ -> (model, [])
        CalLoadMoreDone mpl ret_mps ->
            let mpl_model = triggerViewportAdjust <| { model | mealPlanLoader = Success mpl }
                ret_model = addMealPlansToModel "CalLoadMoreDone" ret_mps mpl_model
            in case ret_model of
                   Err e -> ({ mpl_model | errorMsg = (Alert.shown, e) }, [])
                   Ok m -> (m, [])
        DayMealPlanLoaded ret ->
            let ret_pday_model =
                    case model.page of
                        PageDay dm -> Ok dm
                        _ -> Err ("Got a DayMealPlanLoaded message while not in PageDay.")
                final_ret =
                    ret |> Result.andThen
                    ( \(date, mps) -> ret_pday_model |> Result.andThen
                      ( \pday_model ->
                            ( if pday_model.day == date
                              then Ok ()
                              else Err ( "Got a DayMealPlanLoaded message for " ++ Date.toIsoString date
                                         ++ ", but the page is for " ++ Date.toIsoString pday_model.day
                                       )
                            ) |> Result.andThen
                            ( \() -> Cal.calEntryFromMealPlans date mps
                            )
                      )
                    )
                resultModel =
                    case ret_pday_model of
                        Ok dm ->
                            case final_ret of
                                Ok c -> { model | page = PageDay { dm | calEntry = Success c } }
                                Err e -> { model | errorMsg = (Alert.shown, e), page = PageDay { dm | calEntry = Failure e } }
                        Err e -> { model | errorMsg = (Alert.shown, e) }
            in (resultModel, [])
        MsgRecipeSearch m ->
            case model.page of
                PageRecipeSearch p -> ({ model | page = PageRecipeSearch <| Page.updatePRecipeSearchModel m p }, [])
                _ -> (model, [])


appUrlChange : Url -> Model -> Model
appUrlChange u model =
    let result = 
            case Page.parseUrl u of
                Nothing -> let err = ("Unknown URL: " ++ Url.toString u)
                           in { model | errorMsg = (Alert.shown, err) }
                Just p -> { model | page = initPageWithModel model p }
    in result

initPageWithModel : Model -> Page -> Page
initPageWithModel model page =
    case page of
        PageDay dm ->
            case (dm.calEntry, model.calendar) of
                (NotStarted, Just cal) ->
                    case Cal.entryFor dm.day cal of
                        Nothing -> page
                        Just centry -> PageDay { dm | calEntry = Success centry }
                _ -> page
        _ -> page

concatCmds : List (Cmd m, Model -> Model) -> (Cmd m, Model -> Model)
concatCmds cs =
    let result = fstBatch <| List.foldr f ([], identity) cs
        f (c, m) (acc_cmds, acc_mod) = (c :: acc_cmds, m >> acc_mod)
        fstBatch (cmds, m) = (Cmd.batch cmds, m)
    in result

appUpdateCmd : Msg -> Model -> List (Cmd Msg, Model -> Model)
appUpdateCmd _ model =
    let result = initTimeCmd ++ initMealPlanCmd
                 ++ loadRecipeCmd ++ viewportAdjustCmd
                 ++ loadDayMealPlanCmd
        initTimeCmd =
            case model.clock of
                Nothing -> [(Task.perform identity <| Task.map2 InitTime Time.now Time.here, identity)]
                Just _ -> []
        initMealPlanCmd =
            if Coming.hasStarted model.mealPlanLoader
            then []
            else
                case model.calendar of
                    Nothing -> []
                    Just cal ->
                        [ ( MealPlanLoader.loadInit cal InitMealPlanLoader
                          , (\m -> { m | mealPlanLoader = Pending })
                          )
                        ]
        loadRecipeCmd =
            case model.page of
                PageRecipe rp ->
                    case rp.recipe of
                        NotStarted ->
                            [ ( loadRecipeByID rp.recipeID
                              , (\m -> { m | page = PageRecipe <| { rp | recipe = Pending } } )
                              )
                            ]
                        _ -> []
                _ -> []
        viewportAdjustCmd =
            case (model.calendar, model.mealPlanLoader, model.page) of
                (Just _, Success _, PageTop pt) ->
                    case pt.viewportAdjusted of
                        NotStarted -> 
                            let cmd = Task.attempt ViewportAdjusted
                                      <| setCalendarViewportTask model.calendarViewport
                                new_page = PageTop { pt | viewportAdjusted = Pending }
                            in [(cmd, (\m -> { m | page = new_page }))]
                        _ -> []
                _ -> []
        loadDayMealPlanCmd =
            case model.page of
                PageDay pm ->
                    case pm.calEntry of
                        NotStarted ->
                            let cmd = MealPlanLoader.loadOneDay pm.day DayMealPlanLoaded
                                updateM m = { m | page = PageDay { pm | calEntry = Pending } }
                            in [(cmd, updateM)]
                        _ -> []
                _ -> []
    in result

cmdCalLayoutObtain : Model -> List (Cmd Msg)
cmdCalLayoutObtain model = 
    case (model.calendar, modelToday model) of
        (Just cal, Just today) -> [Task.attempt CalLayoutObtained <| getCalLayoutTask today <| Cal.monthAnchors cal]
        _ -> []

appSub : Model -> Sub Msg
appSub model =
    let result = Sub.batch ([Time.every 5000 TickTime] ++ sub_scroll_events ++ sub_navbar)
        sub_scroll_events =
            case model.page of
                PageTop _ -> [portOnScroll (\_ -> CalendarScrollEvent)]
                _ -> []
        sub_navbar =
            let (NavbarMenuState dstate) = model.navbarMenuState
            in [Dropdown.subscriptions dstate (\s -> NavbarMenuUpdate <| NavbarMenuState s)]
    in result

appOnUrlRequest : UrlRequest -> Msg
appOnUrlRequest = UrlRequestMsg

appOnUrlChange : Url -> Msg
appOnUrlChange = UrlChangeMsg

loadRecipeByID : BRecipeId -> Cmd Msg
loadRecipeByID rid =
    let result = Bridge.getApiV1RecipesByRecipeid rid handle
        handle ret =
            let content = 
                    case ret of
                        Ok r -> Ok (rid, r)
                        Err err -> Err <| "Error in loadRecipeByID: " ++ showHttpError err
            in RecipeLoaded content
    in result

---- View

viewBody : Model -> List (Html Msg)
viewBody model =
    let result = viewNavbar model.locale model.page model.calendarViewType model.navbarMenuState (isLoading model)
                 ++ main_container
                 ++ err_msg
        main_container =
            [ Html.div [Attr.class "container-xl", Attr.class "ml-xl-2", Attr.class "top-container"]
                  [ Grid.row []
                        [ Grid.col [Col.attrs [Attr.class "mainbox"]] mainbox
                        ]
                        ---- [ Grid.col
                        ----       [ Col.md3, Col.lg2
                        ----       , Col.attrs
                        ----           [ Display.none
                        ----           , Display.blockMd
                        ----           ]
                        ----       ]
                        ----       [div [Attr.class "sidebar"] sidebar]
                        ---- , Grid.col [ Col.xs12, Col.md9, Col.lg10
                        ----            , Col.attrs [Attr.class "mainbox"]
                        ----            ] mainbox
                        ---- ]
                  ]
            ]
        sidebar = viewCurTime model.locale model.clock
        mainbox =
            case model.page of
                PageTop _  ->
                    case (modelToday model, model.calendar) of
                        (Just today, Just cal) ->
                            viewCalendar model.locale model.mealPlanLoader today model.calendarViewType cal
                        _ -> []
                PageRecipe rm -> viewRecipePage model.locale rm
                PageDay dm -> viewDayPage model.locale dm
                PageRecipeSearch m -> viewRecipeSearch model.locale m
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
                    (.viewDateYMDA) (Locale.get locale) date
                        ++
                        [ div [] [ Html.span [Attr.class "clock-time", Attr.class "text-nowrap"]
                                       [text (hour ++ ":" ++ minute)]
                                 ]
                        ]
                date = Date.fromPosix c.timeZone c.curTime
                hour = String.padLeft 2 '0' <| String.fromInt <| Time.toHour c.timeZone c.curTime
                minute = String.padLeft 2 '0' <| String.fromInt <| Time.toMinute c.timeZone c.curTime
            in result

navbarHeight : Model -> Float
navbarHeight m =
    case (m.page, m.calendarViewType) of
        (PageTop _, CalViewTable) -> 61
        _ -> 40

viewNavbar : Locale -> Page -> CalendarView -> NavbarMenuState -> Bool -> List (Html Msg)
viewNavbar locale page calview (NavbarMenuState menu_state) is_loading =
    let result = [ Html.nav
                       (List.map Attr.class ["navbar", "fixed-top", "navbar-light", "bg-light"])
                       navbar_content
                 ]
        navbar_content =
            [ Html.form [Attr.class "form-inline"] dropdown_menu ]
            ++ viewNavbarCenter locale page
            ++ [ Html.form [Attr.class "form-inline"] [kon_main_button] ]
        kon_main_button = Html.a [href <| UrlB.absolute [] []] [if is_loading then kon_spinner else kon_icon]
        kon_spinner =
            Spinner.spinner
                [Spinner.attrs [Attr.class "navbar-spinner"]]
                [Spinner.srMessage "Loading"]
        kon_icon =
            Html.img
                [ Attr.src <| iconPath "d/kon.svg"
                , Attr.alt "Home"
                , Attr.width 22
                , Attr.height 22
                ]
                []
        dropdown_menu =
            if List.length menu_items == 0
            then []
            else
                [ Dropdown.dropdown
                  menu_state
                  { toggleMsg = (\s -> NavbarMenuUpdate <| NavbarMenuState s)
                  , toggleButton =
                      Dropdown.toggle [Button.small]
                          [ FIcons.toHtml [] <| FIcons.withSize 18 <| FIcons.menu
                          ]
                  , options = [Dropdown.menuAttrs [Attr.class "kon-navbar-menu"]]
                  , items = menu_items
                  }
                ]
        menu_items = defaultMenuItems
                     ++ case page of
                            (PageTop _) -> viewMenuCalView locale calview
                            _ -> []
        defaultMenuItems = [Dropdown.anchorItem [href "/recipes/"] [text "Search recipes"]] -- TODO: i18n
    in result

viewNavbarCenter : Locale -> Page -> List (Html Msg)
viewNavbarCenter locale page =
    let result =
            case page of
                PageTop pt -> mkForTop pt
                _ -> []
        mkForTop pt =
            case pt.currentAnchor of
                Success ma ->
                    [ Html.form [Attr.class "form-inline"] (today_button ++ mkCurrentAnchor ma)
                    ]
                _ -> []
        today_button =
            [ Button.button
                  [ Button.onClick <| ViewportSet initialViewport
                  , Button.roleLink
                  , Button.small
                  ]
                  [ FIcons.toHtml [] <| FIcons.withSize 16 <| FIcons.calendar
                  ]
            ]
        mkCurrentAnchor ma =
            [ Html.span [Attr.class "pl-2"] [text <| (.showMonthAnchor) (Locale.get locale) ma] ]
    in result


navbarMenuIconSize : Float
navbarMenuIconSize = 16

viewMenuCalView : Locale -> CalendarView -> List (Dropdown.DropdownItem Msg)
viewMenuCalView locale calview =
    let result = 
            [ Dropdown.buttonItem (attrs_list)
                  [ FIcons.toHtml [] <| FIcons.withSize navbarMenuIconSize <| FIcons.list
                  , text " "
                  , text <| (.showNavMenuCalList) <| Locale.get locale
                  ]
            , Dropdown.buttonItem (attrs_table)
                [ FIcons.toHtml [] <| FIcons.withSize navbarMenuIconSize <| FIcons.grid
                , text " "
                , text <| (.showNavMenuCalTable) <| Locale.get locale
                ]
            ]
        (attrs_list, attrs_table) =
            case calview of
                CalViewList ->  ([Attr.class "active"] ++ events_list, [] ++ events_table)
                CalViewTable -> ([] ++ events_list, [Attr.class "active"] ++ events_table)
        events_list =  [Events.onClick <| CalViewChanged CalViewList]
        events_table = [Events.onClick <| CalViewChanged CalViewTable]
    in result


importantMealPhases : List MealPhase
importantMealPhases = [Lunch, Dinner]

renderedMealPhases : CalEntry -> List MealPhase
renderedMealPhases centry =
    let result = importantMealPhases ++ others
        others = List.filter notImportant <| List.map (\dm -> dm.phase) <| centry.meals
        notImportant p = not <| List.member p importantMealPhases
    in result
        
viewCalendar : Locale -> Coming e MealPlanLoader -> Date -> CalendarView -> Calendar -> List (Html Msg)
viewCalendar locale cmploader today calview cal =
    let result =
            viewLoadMoreButton LoadMorePast cmploader
            ++ calendar_body
            ++ viewLoadMoreButton LoadMoreFuture cmploader
        calendar_body = 
            case calview of
                CalViewList -> List.concatMap (viewCalEntry locale today) <| Cal.entries cal
                CalViewTable ->
                    (viewCalWeekHead locale <| Cal.oneWeek cal)
                    ++ (List.concatMap (viewCalWeek locale today) <| Cal.weekEntries cal)
    in result

viewLoadMoreButton : LoadMore -> Coming e MealPlanLoader -> List (Html Msg)
viewLoadMoreButton load_more cmploader =
    let result =
            [ Grid.row [] [ Grid.col
                            [Col.attrs [Attr.class "cal-col-load-more-button"]]
                            [Button.button button_attrs button_label]
                          ]
            ]
        is_load_ok =
            case cmploader of
                Success _ -> True
                _ -> False
        button_label =
            case load_more of
                LoadMoreFuture ->
                    [FIcons.toHtml [] <| FIcons.chevronDown]
                LoadMorePast ->
                    [FIcons.toHtml [] <| FIcons.chevronUp]
        button_attrs =
            [ Button.disabled <| not is_load_ok
            , Button.primary
            , Button.block
            , Button.onClick <| CalLoadMore load_more
            ]
    in result

viewDateLabel : Date -> Date -> List (Html msg) -> List (Html msg)
viewDateLabel today button_day content =
    let result = [Button.linkButton opts content]
        opts = [ ( if is_today
                   then Button.primary
                   else Button.attrs [Attr.class "btn-light-trans"]
                 )
               , Button.block
               , Button.attrs ([Attr.class "cal-day", Attr.href <| dayPageLink button_day] ++ day_class)
               ]
        is_today = today == button_day
        day_class =
            if is_today
            then [Attr.class "cal-today"]
            else []
    in result

viewCalEntry : Locale -> Date -> CalEntry -> List (Html Msg)
viewCalEntry locale today centry =
    let result = row_month_anchor
                 ++ [Grid.row row_attrs [col_date_head, col_date_body]]
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
                        <| viewDateLabel today centry.day <| (.viewDateDA) (Locale.get locale) centry.day
        col_date_body = Grid.col
                        [Col.xs9, Col.md10]
                        [Grid.row [] <| List.map mkColForPhase <| renderedMealPhases centry]
        mkColForPhase p = Grid.col [Col.xs12, Col.sm6, Col.attrs [Attr.class "cal-col-meal-plan"]] <| viewDayMeal p <| Cal.mealFor p centry
        row_month_anchor =
            case Cal.dateToMonthAnchor centry.day of
                Nothing -> []
                Just ma -> [ Grid.row [ Row.attrs [ Attr.id <| monthAnchorCellID ma
                                                  , Attr.class "cal-day-row"
                                                  , Attr.class "cal-row-month-anchor"
                                                  ]
                                      ]
                             <| colMonthAnchor ma
                           ]
        colMonthAnchor ma = [ Grid.col [Col.attrs [Attr.class "cal-col-month-anchor"]]
                              [text <| (.showMonthAnchor) (Locale.get locale) ma ]
                            ]
    in result

viewCalWeekHead : Locale -> List Time.Weekday -> List (Html Msg)
viewCalWeekHead locale wdays =
    let result = [Grid.row [Row.attrs [Attr.class "caltable-wday-head"]] <| List.map mkCol wdays]
        mkCol wday = Grid.col
                     [Col.attrs [Attr.class "col-1-over-7", Attr.class "col-caltable", Attr.class "col-caltable-day"]]
                     [ Html.span [Attr.class <| colorClass wday]
                       [text <| (.showWeekday) (Locale.get locale) wday]
                     ]
        colorClass wday =
            case wday of
                Time.Sun -> "day-sun"
                Time.Sat -> "day-sat"
                _ -> "day-week"
    in result

viewCalWeek : Locale -> Date -> List CalEntry -> List (Html Msg)
viewCalWeek locale today entries =
    let result = [Grid.row [] <| List.map mkCol entries]
        mkCol entry = Grid.col
                      [ Col.attrs
                        [ Attr.class "col-1-over-7", Attr.class "col-caltable", Attr.class "col-caltable-day"
                        , Attr.class <| monthAttr entry.day
                        ]
                      ]
                      (mkDateRow entry ++ (List.map (mkPhaseRow entry) <| renderedMealPhases entry))
        monthAttr day =
            if modBy 2 (Date.monthToNumber <| Date.month day) == 0
            then "cal-month-even"
            else "cal-month-odd"
        monthAnchorElem entry =
            case Cal.dateToMonthAnchor entry.day of
                Nothing -> []
                Just ma -> [Html.a [Attr.id <| monthAnchorCellID ma] []]
        mkDateRow entry = [ Grid.row (mkDateRowAttrs entry)
                                [ Grid.col [Col.attrs [Attr.class "col-caltable"]]
                                  ( monthAnchorElem entry
                                    ++ (viewDateLabel today entry.day <| mkDateText entry.day)
                                  )
                                ]
                          ]
        mkDateText day =
            if Date.day day == 1
            then (.viewDateMD) (Locale.get locale) day
            else [text <| String.fromInt <| Date.day day]
        mkPhaseRow entry p = Grid.row [Row.attrs [Attr.class "row-caltable"]]
                             [Grid.col [Col.attrs [Attr.class "col-caltable"]]
                             <| viewDayMeal p <| Cal.mealFor p entry]
        mkDateRowAttrs entry =
            [ Row.attrs
                  ( [Attr.class "row-caltable"]
                    ++ if entry.day == today
                       then [Attr.id todayCellID]
                       else []
                  )
            ]
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
                Just dm ->
                    List.map mkRecipe dm.recipes
                        ++ List.map mkNote dm.notes
        mkRecipe r = li [Attr.class "cal-meal-plan-item", Attr.class "cal-meal-plan-item-meal"]
                     <| viewLinkRecipe r.id [text r.name]
        mkNote n = li [Attr.class "cal-meal-plan-item", Attr.class "cal-meal-plan-item-note"]
                   <| [text n]
    in result

viewLinkRecipe : BRecipeId -> List (Html a) -> List (Html a)
viewLinkRecipe rid content =
    let result = [Html.a attrs content]
        attrs = [href <| recipePageLink rid]
    in result

viewRecipePage : Locale -> PRecipeModel -> List (Html Msg)
viewRecipePage locale rmodel =
    let result = recipe_body
                 ++ [div [Attr.class "recipe-id-box"] [text ("Recipe ID: " ++ rmodel.recipeID)]]
        recipe_body =
            case Coming.success rmodel.recipe of
                Nothing -> []
                Just r -> viewRecipe locale r
    in result

viewRecipe : Locale -> BRecipeStored -> List (Html Msg)
viewRecipe locale br =
    let result = [div [Attr.class "recipe-box"] recipeContent]
        recipeContent = viewName br.name ++ viewIngDescs br.ings ++ viewDesc br.desc ++ viewRefs br.ref
        viewName n = [h1 [] [text n]]
        viewIngDescs ings =
            if List.isEmpty ings
            then []
            else [ h2 [] [text <| (.showIngredients) <| Locale.get locale]
                 , ul [] <| List.concatMap viewIngDesc ings
                 ]
        viewIngDesc ingDesc =
            case ingDesc of
                BIngGroup ingG -> [ li [] [text ingG.g]
                                  , ul [] <| List.map viewIng ingG.ings
                                  ]
                BIngSingle ing -> [viewIng ing]
        viewIng ing = li [] <| (.viewIngredient) (Locale.get locale) ing
        viewDesc desc =
            if desc == ""
            then []
            else [h2 [] [text <| (.showRecipeSteps) <| Locale.get locale]] ++ Markdown.toHtml Nothing desc
        viewRefs refs =
            if List.isEmpty refs
            then []
            else [ h2 [] [text <| (.showRecipeReference) <| Locale.get locale]
                 , ul [] <| List.concatMap viewRef refs
                 ]
        viewRefLi content = li [Attr.class "recipe-ref"] content
        viewExtLink url t = [Html.a [href url, Attr.target "_blank"] [text t]]
        viewRef ref =
            case (ref.source, ref.url) of
                (Nothing, Nothing) -> []
                (Just s, Nothing)  -> [viewRefLi <| [text s]]
                (Nothing, Just u)  -> [viewRefLi <| viewExtLink u u]
                (Just s, Just u)   -> [viewRefLi <| viewExtLink u s]
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

viewDayPage : Locale -> PDayModel -> List (Html Msg)
viewDayPage locale dm =
    let result =
            [ Html.h1 [] [text <| (.showDateYMDA) (Locale.get locale) <| dm.day]
            ]
            ++ ( case Coming.success dm.calEntry of
                     Nothing -> []
                     Just cale -> List.concatMap (viewDayPageDayMeal locale) <| dayMeals cale
               )
        dayMeals cale = List.filterMap (\p -> Cal.mealFor p cale) <| renderedMealPhases cale
    in result

viewDayPageDayMeal : Locale -> DayMeal -> List (Html Msg)
viewDayPageDayMeal locale dm =
    let result =
            [ Html.h2 [] [text <| (.showMealPhase) (Locale.get locale) dm.phase]
            , Html.ul [Attr.class "day-meal-plan-list"] list_contents
            ]
        list_contents =
            List.map viewRecipeSummary dm.recipes
                ++ List.map viewNote dm.notes
        viewRecipeSummary rs =
            Html.li
                [Attr.class "cal-meal-plan-item", Attr.class "cal-meal-plan-item-meal"]
                [ Html.a [Attr.href <| recipePageLink rs.id] [Html.text rs.name]
                ]
        viewNote n =
            Html.li
                [Attr.class "cal-meal-plan-item", Attr.class "cal-meal-plan-item-note"]
                [Html.text n]
    in result

viewRecipeSearch : Locale -> PRecipeSearchModel -> List (Html Msg)
viewRecipeSearch _ m = 
    let result =
          [ Html.form []
            [ Html.div [Attr.class "form-row"]
              [ Html.div [Attr.class "col-10"]
                [ Html.input [ Attr.type_ "search", Attr.class "form-control", Attr.id "q", Attr.name "q", Attr.autofocus True, Attr.placeholder "search recipe"
                             , Attr.value m.formQuery, Events.onInput (\s -> MsgRecipeSearch <| Page.RSUpdateFormQuery s)
                             ] []
                ] -- TODO: i18n
              , Html.div [Attr.class "col"]
                [ Html.button [Attr.type_ "submit", Attr.class "btn btn-primary"] [FIcons.toHtml [] <| FIcons.withSize 16 <| FIcons.search] ]
              ]
            ]
          ]
    in result
                

