type todo = {
  id: int,
  task: string,
  is_done: bool,
}

@react.component
let make = (~todo: todo, ~toggleTodo, ~editTodo, ~deleteTodo) => {
  let style = if todo.is_done {
    ReactDOMRe.Style.make(~textDecoration="line-through", ())
  } else {
    ReactDOMRe.Style.make(~textDecoration="none", ())
  }
  <tr>
    <td style onClick={_event => toggleTodo(todo)}> {React.string(todo.task)} </td>
    <td> <a href="#" onClick={_event => editTodo(todo.id)}> {React.string("Edit")} </a> </td>
    <td> <a href="#" onClick={_event => deleteTodo(todo.id)}> {React.string("Delete")} </a> </td>
  </tr>
}
