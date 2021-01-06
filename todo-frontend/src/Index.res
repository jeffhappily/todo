// Entry point

@bs.val external document: {..} = "document"

// We're using raw DOM manipulations here, to avoid making you read
// ReasonReact when you might precisely be trying to learn it for the first
// time through the examples later.
let style = document["createElement"]("style")
document["head"]["appendChild"](style)
style["innerHTML"] = ExampleStyles.style

let root = document["getElementById"]("root")

// All 4 examples.
ReactDOMRe.render(
  <TodoList />,
  root,
)

// ReactDOMRe.render(<ReducerFromReactJSDocs />, makeContainer("Reducer From ReactJS Docs"))

// ReactDOMRe.render(<FetchedDogPictures />, makeContainer("Fetched Dog Pictures"))

// ReactDOMRe.render(<ReasonUsingJSUsingReason />, makeContainer("Reason Using JS Using Reason"))
