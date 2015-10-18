module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import String exposing (toUpper, repeat, trimRight)

import StartApp
import BingoUtils as Utils

-- VIEW

title : String -> Int -> Html
title message times =
  message ++ " "
    |> toUpper
    |> repeat times
    |> trimRight
    |> text

pageHeader : Html
pageHeader =
  h1 [ id "logo", class "classy" ] [ title "bingo!" 3 ]

pageFooter : Html
pageFooter =
  footer []
    [ a [ href "http://brandonrichey.com" ]
        [ text "My Great Blog" ]
    ]

totalPoints : List Entry -> Int
totalPoints entries =
  let
    spokenEntries = List.filter .wasSpoken entries
  in
    List.sum (List.map .points spokenEntries)

totalItem : Int -> Html
totalItem total =
  li
    [ class "total" ]
    [
      span [ class "label" ] [ text "total" ],
      span [ class "points" ] [ text (toString total) ]
    ]

entryItem : Address Action -> Entry -> Html
entryItem address entry =
  li
    [ 
      classList [ ("highlight", entry.wasSpoken) ],
      onClick address (Mark entry.id)
    ]
    [
      span [ class "phrase" ] [ text entry.phrase ],
      span [ class "points" ] [ entry.points |> toString |> text ],
      button
        [ class "delete", onClick address (Delete entry.id) ]
        [ ]
    ]

entryList : Address Action -> List Entry -> Html
entryList address entries =
  let
    entryItems = List.map (entryItem address) entries
    items = entryItems ++ [ totalItem (totalPoints entries) ]
  in
    ul [ ] items 

entryForm : Address Action -> Model -> Html
entryForm address model =
  div [ ]
  [
    input [ type' "text", placeholder "phrase", value model.phraseInput, name "phrase", autofocus True, Utils.onInput address UpdatePhraseInput ] [ ],
    input [ type' "number", placeholder "points", value model.pointsInput, name "points", Utils.onInput address UpdatePointsInput ] [ ],
    button [ class "add", onClick address Add ] [ text "Add" ],
    h2 [ ] [ text (model.phraseInput ++ " " ++ model.pointsInput) ]
  ]


view : Address Action -> Model -> Html
view address model =
  div [ id "container" ]
  [
    pageHeader,
    entryForm address model,
    entryList address model.entries,
    button
      [ class "sort", onClick address Sort ] [ text "Sort" ],
    pageFooter
  ]

-- MODEL

type alias Entry =
  { phrase: String, points: Int, wasSpoken: Bool, id: Int   }

type alias Model =
  {
    entries: List Entry,
    phraseInput: String,
    pointsInput: String,
    nextId: Int
  }

newEntry : String -> Int -> Int -> Entry
newEntry phrase points id =
  {
    phrase    = phrase,
    points    = points,
    wasSpoken = False,
    id        = id
  }

initialModel : Model
initialModel =
  {
    entries = [
      newEntry "Future-Proof" 100 1,
      newEntry "In The Cloud" 300 3,
      newEntry "Doing Agile" 200 2,
      newEntry "Rock-Star Ninja" 400 4
    ],
    phraseInput = "",
    pointsInput = "",
    nextId = 5
  }

-- UPDATE

type Action
  = NoOp
  | Sort
  | Delete Int
  | Mark Int
  | UpdatePhraseInput String
  | UpdatePointsInput String
  | Add

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Sort ->
      { model | entries <- List.sortBy .points model.entries }
    Delete id ->
      let remainingEntries =
        List.filter (\e -> e.id /= id) model.entries
      in
        { model | entries <- remainingEntries }
    Mark id ->
      let updateEntry e =
        if e.id == id then { e | wasSpoken <- (not e.wasSpoken) } else e
      in
         { model | entries <- List.map updateEntry model.entries }
    UpdatePhraseInput contents ->
      { model | phraseInput <- contents }
    UpdatePointsInput contents ->
      { model | pointsInput <- contents }
    Add ->
      let
        entryToAdd =
          newEntry model.phraseInput (Utils.parseInt model.pointsInput) model.nextId
        isInvalid model =
          String.isEmpty model.phraseInput || String.isEmpty model.pointsInput
      in
        if isInvalid model
        then model
        else
          { model |
            phraseInput <- "",
            pointsInput <- "",
            entries     <- entryToAdd :: model.entries,
            nextId      <- model.nextId + 1
          }
        

-- WIRE IT ALL TOGETHER

main : Signal Html
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
