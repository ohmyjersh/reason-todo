type item = {
  id: int,
  title:string,
  completed:bool
};

type state = {
  items: list item,
};

type action =
  | AddItem string
  | ToggleItem int;

let component = ReasonReact.reducerComponent "Todo";
let se = ReasonReact.stringToElement;
let lastId = ref 0;
let newItem text => {
  lastId := !lastId + 1;
  {id: !lastId, title: text, completed: true}
};

let module TodoItem = {
  let component = ReasonReact.statelessComponent "TodoItem";
  let make ::item ::onToggle children => {
    ...component,
    render: fun self =>
      <div className="item" onClick=(fun _evt => onToggle())>
        <input
          _type="checkbox"
          checked=(Js.Boolean.to_js_boolean item.completed)
          /* TODO make interactive */
        />
        (se item.title)
      </div>
  };
};

let module Input = {
  type state = string;
  let component = ReasonReact.reducerComponent "Input";
  let valueFromEvent evt: string =>
  (evt
    |> ReactEventRe.Form.target
    |> ReactDOMRe.domElementToObj
  )##value;
  let make ::onSubmit _ => {
    ...component,
    initialState: fun () => "hi",
    reducer: fun newText _text => ReasonReact.Update newText,
    render: fun {state: text, reduce} =>
    <input
      value=text
      _type="text"
      placeholder="Write something to do"
      onChange=(reduce (fun evt => (valueFromEvent evt)))
      onKeyDown=(fun evt => (
        if (ReactEventRe.Keyboard.key evt == "Enter") {
          onSubmit text;
          (reduce (fun () => "")) ()
        }
      ))
    />
  }
};

let make children => {
    ...component,
    initialState: fun () => {
      items: [{
         id: 0,
          title: "hi",
          completed: false,
        }]
    },
    reducer: fun action {items} => switch action {
      | AddItem text => ReasonReact.Update {items: [newItem text, ...items]}
      | ToggleItem id => {
        let items = List.map 
          (fun item => item.id === id
            ? {...item, completed: not item.completed}
            : item)
          items;
        ReasonReact.Update {items: items};
      }
    },
    render: fun {state: {items}, reduce} => {
      let numItems = List.length items;
      let itemMessage = numItems == 1 ? "item" : "items";
      <div className="app">
      <div className="title">
      (se "What to do")
      <Input onSubmit=(reduce (fun text => AddItem text)) />
    </div>
        <div className="items">
        (ReasonReact.arrayToElement
        (Array.of_list
          (List.map (fun item => <TodoItem
          key=(string_of_int item.id)
          onToggle=(reduce (fun () => ToggleItem item.id))
           item 
           />) items)
        )
      )
        </div>
        <div className="footer">
          (se ((string_of_int numItems) ^ " " ^ (itemMessage)))
        </div>
      </div>
    }
  };