module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import String exposing (toUpper, repeat, trimRight)

import StartApp

-- VIEW

title message times =
  message ++ " "
    |> toUpper
    |> repeat times
    |> trimRight
    |> text

pageHeader =
  h1 [ id "logo", class "classy" ] [ title "bingo!" 3 ]

pageFooter =
  footer []
    [ a [ href "http://brandonrichey.com" ]
        [ text "My Great Blog" ]
    ]

entryItem entry =
  li [ ]
    [
      span [ class "phrase" ] [ text entry.phrase ],
      span [ class "points" ] [ entry.points |> toString |> text ]
    ]

entryList entries =
  ul [ ] (List.map entryItem entries)

view address model =
  div [ id "container" ]
  [
    pageHeader,
    entryList model.entries,
    button
      [ class "sort", onClick address Sort ] [ text "Sort" ],
    pageFooter
  ]

-- MODEL

newEntry phrase points id =
  {
    phrase    = phrase,
    points    = points,
    wasSpoken = False,
    id        = id
  }

initialModel =
  {
    entries = [
      newEntry "Future-Proof" 100 1,
      newEntry "In The Cloud" 300 3,
      newEntry "Doing Agile" 200 2,
      newEntry "Rock-Star Ninja" 400 4
    ]
  }

-- UPDATE

type Action
  = NoOp
  | Sort

update action model =
  case action of
    NoOp ->
      model
    Sort ->
      { model | entries <- List.sortBy .points model.entries }

-- WIRE IT ALL TOGETHER

main =
  -- view (update Sort initialModel)
  -- initialModel
  --  |> update Sort
  --  |> view
  StartApp.start
    {
      model  = initialModel,
      view   = view,
      update = update
    }
