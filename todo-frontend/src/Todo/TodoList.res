open TodoItem

type state = {
  latestId: int,
  editingId: option<int>,
  todos: array<todo>,
}

type action =
  | AddTodo(string)
  | UpdateTodo(todo)
  | ToggleEdit(option<int>)
  | DeleteTodo(int)

let initialState = {
  latestId: 1,
  editingId: None,
  todos: [],
}

exception TodoNotFound

let reducer = (state, action) =>
  switch action {
  | AddTodo(string) => {
      let newTodo = {
        id: state.latestId,
        task: string,
        is_done: false,
      }

      {
        ...state,
        latestId: state.latestId + 1,
        todos: Belt.Array.concat(state.todos, [newTodo]),
      }
    }
  | UpdateTodo(todo) => {
      let todos = Belt.Array.copy(state.todos)
      let optI = Belt.Array.getIndexBy(todos, t => {
        t.id === todo.id
      })

      switch optI {
      | None => raise(TodoNotFound)
      | Some(i) => {
          Belt.Array.setExn(todos, i, todo)

          {
            ...state,
            todos: todos,
          }
        }
      }
    }
  | ToggleEdit(id) => {
      ...state,
      editingId: id,
    }
  | DeleteTodo(id) => {
      let optI = Belt.Array.getIndexBy(state.todos, t => {
        t.id === id
      })

      switch optI {
      | None => raise(TodoNotFound)
      | Some(i) => {
          let first = Belt.Array.slice(state.todos, ~offset=0, ~len=i)
          let second = Belt.Array.sliceToEnd(state.todos, i + 1)

          {
            ...state,
            todos: Belt.Array.concat(first, second),
          }
        }
      }
    }
  }

@react.component
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initialState)

  // We can use a fragment here, but we don't, because we want to style the counter
  <div>
    <input
      onKeyPress={event => {
        if (
          ReactEvent.Keyboard.key(event) === "Enter" &&
            ReactEvent.Keyboard.target(event)["value"] !== ""
        ) {
          dispatch(AddTodo(ReactEvent.Keyboard.target(event)["value"]))
          ReactEvent.Keyboard.target(event)["value"] = ""
        }
      }}
    />
    <table>
      <tbody>
        {state.todos
        ->Belt.Array.map(todo => {
          if state.editingId === Some(todo.id) {
            <TodoItemForm
              key={Belt.Int.toString(todo.id)}
              todo
              saveTodo={task => {
                dispatch(
                  UpdateTodo({
                    ...todo,
                    task: task,
                  }),
                )
                dispatch(ToggleEdit(None))
              }}
            />
          } else {
            <TodoItem
              key={Belt.Int.toString(todo.id)}
              todo
              toggleTodo={todo =>
                dispatch(
                  UpdateTodo({
                    ...todo,
                    is_done: !todo.is_done,
                  }),
                )}
              editTodo={id => dispatch(ToggleEdit(Some(id)))}
              deleteTodo={id => dispatch(DeleteTodo(id))}
            />
          }
        })
        ->React.array}
      </tbody>
    </table>
  </div>
}
