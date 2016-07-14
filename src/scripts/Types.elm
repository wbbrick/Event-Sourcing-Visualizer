module Types exposing (..)

type EventType = Update | Delete | Create

type alias Todo =
  {
    completed: Bool
  , description: String
  }

type alias Event =
  {
    type': EventType
  , payload: Todo
  , progress: Int
  }

emptyTodo : Todo
emptyTodo =
  {
    completed = False
  , description = ""
  }
