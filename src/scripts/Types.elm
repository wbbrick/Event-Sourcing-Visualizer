module Types exposing (..)

type EventType = Update | Delete | Create

type alias Todo =
  {
    id: Int
  , completed: Bool
  , description: String
  }

type alias Event =
  {
    type': EventType
  , payload: Todo
  , progress: Float
  }

type CurrentEvent = Latest | Index Int

emptyTodo : Int -> Todo
emptyTodo id =
  {
    id = id
  , completed = False
  , description = ""
  }
