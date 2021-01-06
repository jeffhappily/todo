// Entry point

@val external document: {..} = "document"

let cssStyle = j`
  body {
    background-color: rgb(224, 226, 229);
    display: flex;
    flex-direction: column;
    align-items: center;
    font-size: 26px;
  }
`

let style = document["createElement"]("style")
document["head"]["appendChild"](style)
style["innerHTML"] = cssStyle

let root = document["getElementById"]("root")

ReactDOMRe.render(
  <TodoList />,
  root,
)
