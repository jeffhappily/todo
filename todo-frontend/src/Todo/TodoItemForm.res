open TodoItem

@react.component
let make = (~todo: todo, ~saveTodo) => {
  <tr>
    <td>
      <input
        defaultValue=todo.task
        onKeyPress={event => {
          if (
            ReactEvent.Keyboard.key(event) === "Enter" &&
              ReactEvent.Keyboard.target(event)["value"] !== ""
          ) {
            saveTodo(ReactEvent.Keyboard.target(event)["value"])
          }
        }}
      />
    </td>
  </tr>
}
