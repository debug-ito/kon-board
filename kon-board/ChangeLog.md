# Revision history for kon-board

## 0.4.0.0  -- 2020-08-08

### Backend (kon-board)

* **BREAKING CHANGE** `MealPlan` type: add `mealNotes` field, change the type of `mealRecipes` from `NonEmpty` to `[]`.
  The change of the type was necessary because now a meal plan can have some notes but no recipes.
* **BREAKING CHANGE** `BMealPlan` type: apply changes corresponding to the changes to `MealPlan` type.
* (Internal) `Util.YAML` module: add `ArrayOrSingle` type.

### Frontend

* Show notes for meal plans on the calendar and the day page.
* Add "Jump to today" button to navbar.

## 0.3.0.0  -- 2020-07-26

### Frontend

* Now it officially supports "breakfast" and custom meal phases.
* Add links to dates in the calendar. The link leads to a page for the date.
* Now it shows meal plans for all phases in addition to "lunch" and "dinner".


### Backend (kon-board)

* **BREAKING CHANGE**: Text format of `MealPhase` has changed (especially for `MealOther` variant).
* **BREAKING CHANGE**: `ToJSON` and `FromJSON` instances for `MealPhase` are removed.
  Now those are defined internally in `MealPlan.Store` module.


## 0.2.1.0  -- 2020-07-11

* Now it's possible to load meal plans in past and future, by pressing buttons.
* Now it shows a spinner icon (instead of Kon icon) when it's loading something.
* Calendar table view: add color to months of even number.

## 0.2.0.0  -- 2020-06-28

* Remove the sidebar.
* Calendar views: add the current year and month to the navbar.
* Calendar list view: add the month anchor rows.
* Update Bootstrap CSS from v4.0.0 to v4.5.0.
* Now the top containers uses .container-xl class.

## 0.1.2.0  -- 2020-06-15

* Fix the initial viewport position, considering the height of navbar.
* The Kon icon is moved from the left to the right of the navbar.
* Now we use [Feather icons](https://feathericons.com/), instead of Twitter Bootstrap icons.
* Add the "table" view of the calendar.


## 0.1.1.0  -- 2020-06-06

* Add the kon-board icon to navbar and favicon.
* Now the calendar keeps a lot more days, especially in the past.
* Initially the browser viewport is now adjusted around today's entry in the calendar.

## 0.1.0.2  -- 2020-05-25

* (internal change) kon-board-gen-elm and kon-board-server projects
  are split from kon-board Haskell project.
* Update view of meal plan calendar.
  * Meal plans are now rendered as a vertical stack in small display.
  * Meal plans are now grouped in colored boxes.
  * Add the navigation bar at the top.
  * Now the sidebar is fixed (sticky), and is hidden in small display.
  * Now the error message is rendered in a fixed dismissible alert.


## 0.1.0.1  -- 2020-05-22

* Preliminary release.
* Add colors to day of week.

## 0.1.0.0  -- 2020-05-17

* Preliminary release.
