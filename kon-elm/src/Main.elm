port module Main exposing
   (..)

{- | The application main. -}

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Utilities.Display as Display
import Bootstrap.Dropdown as Dropdown
import Browser
import Browser exposing (Document, UrlRequest)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import FeatherIcons as FIcons
import Json.Decode exposing (Value)
import Html exposing (Html, div, text, ul, li, h1, h2, h3)
import Html
import Html.Attributes exposing (href)
import Html.Attributes as Attr
import Html.Events as Events
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
import Calendar exposing (CalEntry, Calendar, DayMeal, MonthAnchor)
import Calendar as Cal
import CalSpy exposing
    ( CalLayout
    , todayCellID, monthAnchorCellID
    , relativeCalendarViewportY, setCalendarViewportTask, getCalLayoutTask
    )
import CalSpy
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
    -- | 'Success' if MealPlans are loaded to the calendar.
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
         -- | Initiallize the current time
         | InitTime Time.Posix Time.Zone
         -- | Update the current time
         | TickTime Time.Posix
         -- | Got result of loading MealPlans from backend.
         | MealPlansLoaded (Result String (List BMealPlan))
         -- | Change visibility of error message box
         | ErrorMsgVisibility Alert.Visibility
         | UrlRequestMsg UrlRequest
         | UrlChangeMsg Url
         -- | Got result of laoding a Recipe from backend.
         | RecipeLoaded (Result String MRecipe)
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

appInit : () -> Url -> Nav.Key -> (Model, Cmd Msg)
appInit _ url key =
    let model_base = { locale = LJaJP
                     , clock = Nothing
                     , page = Page.initPage
                     , navKey = key
                     , navbarMenuState = NavbarMenuState Dropdown.initialState
                     , calendar = Nothing
                     , calendarViewport = -130.0
                     , calendarViewType = CalViewList
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
                PageTop pt ->
                    case adjust_ret of
                        Ok () -> { model | page = PageTop { pt | viewportAdjusted = Success () } }
                        Err e -> { model | errorMsg = (Alert.shown, "ViewportAdjust error: " ++ e)
                                 , page = PageTop { pt | viewportAdjusted = Failure e }
                                 }
                _ -> { model | errorMsg = (Alert.shown, "Unexpected ViewportAdjust msg to non-PageTop page.") }
        CalLayoutObtained ret_cl ->
            case ret_cl of
                Ok cl ->
                    let new_page =
                            case model.page of
                                PageTop pt ->
                                    PageTop { pt | currentAnchor = Success <| CalSpy.currentMonthAnchor (navbarHeight model) cl }
                                _ -> model.page
                    in { model | calendarViewport = relativeCalendarViewportY cl, page = new_page }
                Err e -> { model | errorMsg = (Alert.shown, "CalLayoutObtained error: " ++ e) }
        CalendarScrollEvent -> model
        NavbarMenuUpdate s -> { model | navbarMenuState = s }
        CalViewChanged cv -> { model | calendarViewType = cv }

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
                 ++ loadRecipeCmd ++ viewportAdjustCmd ++ calLayoutObtainCmd
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
                (Just _, Success _, PageTop pt) ->
                    case pt.viewportAdjusted of
                        NotStarted -> 
                            let cmd = Task.attempt ViewportAdjusted
                                      <| setCalendarViewportTask model.calendarViewport
                                new_page = PageTop { pt | viewportAdjusted = Pending }
                            in [(cmd, (\m -> { m | page = new_page }))]
                        _ -> []
                _ -> []
        calLayoutObtainCmd =
            case (model.calendar, modelToday model) of
                (Just cal, Just today) ->
                    let the_cmds = [( Task.attempt CalLayoutObtained <| getCalLayoutTask today <| Cal.monthAnchors cal
                                    , identity
                                    )]
                    in case (msg, model.page) of
                           (CalendarScrollEvent, PageTop pt) ->
                               case pt.viewportAdjusted of
                                   Success () -> the_cmds
                                   _ -> []
                           (ViewportAdjusted _, PageTop _) -> the_cmds
                           (CalViewChanged _, PageTop _) -> the_cmds
                           _ -> []
                _ -> []
    in result

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

---- View

viewBody : Model -> List (Html Msg)
viewBody model =
    let result = viewNavbar model.locale model.page model.calendarViewType model.navbarMenuState ++ main_container ++ err_msg
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
                        (Just today, Just cal) -> viewCalendar model.locale today model.calendarViewType cal
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

viewNavbar : Locale -> Page -> CalendarView -> NavbarMenuState -> List (Html Msg)
viewNavbar locale page calview (NavbarMenuState menu_state) =
    let result = [ Html.nav
                       (List.map Attr.class ["navbar", "fixed-top", "navbar-light", "bg-light"])
                       navbar_content
                 ]
        navbar_content =
            [ Html.form [Attr.class "form-inline"] dropdown_menu ]
            ++ viewNavbarCenter locale page
            ++ [ Html.form [Attr.class "form-inline"] [kon_icon] ]
        kon_icon =
            Html.a [href <| UrlB.absolute [] []]
                [ Html.img
                      [ Attr.src <| iconPath "d/kon.svg"
                      , Attr.alt "Home"
                      , Attr.width 22
                      , Attr.height 22
                      ]
                      []
                ]
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
        menu_items =
            case page of
                (PageTop _) -> viewMenuCalView locale calview
                _ -> []
    in result

viewNavbarCenter : Locale -> Page -> List (Html Msg)
viewNavbarCenter locale page =
    let result =
            case page of
                PageTop pt ->
                    case pt.currentAnchor of
                        Success ma -> [Html.form [Attr.class "form-inline"] <| mkContent ma]
                        _ -> []
                _ -> []
        mkContent ma = [text <| (.showMonthAnchor) (Locale.get locale) ma]
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


tableMealPhases : List MealPhase
tableMealPhases = [Lunch, Dinner]
        
viewCalendar : Locale -> Date -> CalendarView -> Calendar -> List (Html Msg)
viewCalendar locale today calview cal =
    case calview of
        CalViewList -> List.concatMap (viewCalEntry locale today) <| Cal.entries cal
        CalViewTable ->
            (viewCalWeekHead locale <| Cal.oneWeek cal)
            ++ (List.concatMap (viewCalWeek locale today) <| Cal.weekEntries cal)

viewDateLabelWith : Bool -> List (Html msg) -> List (Html msg)
viewDateLabelWith is_today content =
    let result = [div attrs content]
        attrs = [Attr.class "cal-day"]
                ++ if is_today
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
                        <| viewDateLabelWith (today == centry.day) <| (.viewDateDA) (Locale.get locale) centry.day
        col_date_body = Grid.col
                        [Col.xs9, Col.md10]
                        [Grid.row [] <| List.map mkColForPhase tableMealPhases]
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
                        [Attr.class "col-1-over-7", Attr.class "col-caltable", Attr.class "col-caltable-day"]
                      ]
                      (mkDateRow entry ++ List.map (mkPhaseRow entry) tableMealPhases)
        monthAnchorElem entry =
            case Cal.dateToMonthAnchor entry.day of
                Nothing -> []
                Just ma -> [Html.a [Attr.id <| monthAnchorCellID ma] []]
        mkDateRow entry = [ Grid.row (mkDateRowAttrs entry)
                                [ Grid.col [Col.attrs [Attr.class "col-caltable"]]
                                  ( monthAnchorElem entry
                                    ++ (viewDateLabelWith (today == entry.day) <| mkDateText entry.day)
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
