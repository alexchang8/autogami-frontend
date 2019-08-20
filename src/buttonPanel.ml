open Types

(*these should be moved to properties/state*)
let radius = 10

let rsq = radius * radius

let width = 800

let height = 800

let init_state init_nodes init_edges =
  let nodes = List.mapi (fun i x -> i, x) init_nodes in
  { nodes = nodes;
    edges = List.map (fun (x, y) -> x, y, Mountain) init_edges;
    mouse_pos = (-50, -50);
    mode = Idle;
    next_id = List.length nodes;
    grid = None
  }

let rel_coords e =
  ReactEvent.Mouse.stopPropagation e;
  let native = ReactEvent.Mouse.nativeEvent e in
  let x, y = int_of_float native##offsetX, int_of_float native##offsetY in
  (x, y)

let make ~init_nodes ~init_edges =
  let state, dispatch = React.useReducer State_logic.reduce @@ init_state init_nodes init_edges in
  let coord_dispatch f e = rel_coords e |> f |> dispatch in
  let full_nodes =
    match state.mode with
    | Idle | AddEdge _ | RemoveEdge _ | RemoveNode -> state.nodes
    | Dragging d -> d.active :: state.nodes
  in
  let valleys, mountains =
    let part_fold (vlist, mlist) (id1, id2, fold) =
      match fold with
      | Valley -> ((id1, id2) :: vlist, mlist)
      | Mountain -> (vlist, (id1, id2) :: mlist)
    in
    List.fold_left part_fold ([], []) state.edges
  in
  let button_style =
    ReactDOMRe.Style.make ~border:"none" ~textAlign:"center" ()
  in
  let mode_string state =
    match state.mode with
    | Dragging _ | Idle -> "normal mode"
    | RemoveEdge _ -> "remove edge mode"
    | AddEdge _ -> "add edge mode"
    | RemoveNode -> "remove node mode"
  in
  let active_node state =
    let add_hover lst =
      match
        List.find_opt (State_logic.in_circle state.mouse_pos) state.nodes
      with
      | Some n -> n :: lst
      | None -> lst
    in
    match state.mode with
    | Dragging d -> [ d.active ]
    | AddEdge (Some id, _) | RemoveEdge (Some id) ->
        add_hover [ List.find (fun (id2, _) -> id = id2) state.nodes ]
    | Idle | AddEdge _ | RemoveEdge _ | RemoveNode -> add_hover []
  in
  let keyHandler e =
    match ReactEvent.Keyboard.key e with
    | "a" -> dispatch AddNodeClick
    | "d" -> dispatch RemoveNodeClick
    | "v" -> dispatch @@ AddEdgeClick Valley
    | "m" -> dispatch @@ AddEdgeClick Mountain
    | "r" -> dispatch RemoveEdgeClick
    | "Escape" -> dispatch ClearMode
    | _ -> ()
  in
  [%bsx "
    <div>
      <p style = "(ReactDOMRe.Style.make ~margin: "0px" ~padding: "0px" ())" >"(mode_string state |> React.string)"</p>
      <Viewport
        onMouseDown = "(coord_dispatch (fun c -> MouseDown c))"
        onMouseMove = "(coord_dispatch (fun c -> MouseMove c))"
        onMouseUp = "(coord_dispatch (fun c -> MouseUp c))"
        height = "height"
        width = "width"
        radius = "10"
        nodes = "full_nodes"
        mountains = "mountains"
        valleys = "valleys"
        hNodes = "(active_node state)"
        grid = "state.grid"
        keyHandler = "keyHandler"
        />
      <div>
        <button
          style = "button_style"
          onClick = "(fun _ -> dispatch GcodeClick)"
          >Download Gcode</button>
        <button
          style = "button_style"
          onClick = "(fun _ -> dispatch AddNodeClick)"
          >Add Node</button>
        <button
          style = "button_style"
          onClick = "(fun _ -> dispatch RemoveNodeClick)"
          >Remove Node</button>
        <button
          style = "button_style"
          onClick = "(fun _ -> dispatch @@ AddEdgeClick Valley)"
          >Add Valley</button>
        <button
          style = "button_style"
          onClick = "(fun _ -> dispatch @@ AddEdgeClick Mountain)"
          >Add Mountain</button>
        <button
          style = "button_style"
          onClick = "(fun _ -> dispatch RemoveEdgeClick)"
          >Remove Edge</button>
        <button
          style = "button_style"
          onClick = "(fun _ -> dispatch ClearMode)"
          >Clear Mode</button>
        <button
          style = "button_style"
          onClick = "(fun _ -> dispatch SnapAllNodesClick)"
          >Snap Nodes to Grid</button>
        <button
          style = "button_style"
          onClick = "(fun _ -> dispatch SwapEdges)"
          >Swap Edge Assign.</button>
        <GridButton
          setGrid = "(fun x -> dispatch @@ SetGrid x)"
          removeGrid = "(fun () -> dispatch RemoveGrid)"/>
      </div>
    </div>
  "]
[@@react.component]
