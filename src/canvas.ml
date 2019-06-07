type n_id = int
type node = n_id * (int * int)
type graph = {nodes: node list; edges: (n_id * n_id) list}
type state =
  Dragging of node * graph | Idle of graph | AddEdgeSel of n_id * graph | AddEdge of graph

let radius = 10
let rsq = radius * radius
let width = 500
let height = 500

let in_circle (x1, y1) (_,(x2,y2)) =
  let sq z = z*z in
  sq (x1 - x2) + sq (y1 - y2) <= rsq

let rel_coords e =
    ReactEvent.Mouse.preventDefault e;
    ReactEvent.Mouse.stopPropagation e;
    let native = ReactEvent.Mouse.nativeEvent e in
    let x = int_of_float native##offsetX in
    let y = int_of_float native##offsetY in
    x,y

let init_state = {nodes= [(0, (50, 50));(1, (100, 100));(2, (100, 150))];
                  edges = [(0,1);(0,2);(2, 1)]} |. Idle

let make () =
  let state, setState = React.useState (fun () -> init_state) in

  let down_handler e =
    let c = rel_coords e in
    setState (fun _ ->
      match state with
      | Dragging(n, g) -> {g with nodes = n::g.nodes} |. Idle
      | Idle(g) -> begin
        match List.find_opt (in_circle c) g.nodes with
        | None -> state
        | Some((id, _)) -> ((id, c), {g with nodes = List.remove_assoc id g.nodes}) |. Dragging
      end
      | AddEdge(g) -> begin
        match List.find_opt (in_circle c) g.nodes with
        | None -> state
        | Some((id,_)) -> (id, g) |. AddEdgeSel
      end
      | AddEdgeSel(id1, g) -> begin
        match List.find_opt (in_circle c) g.nodes with
        | Some((id2,_)) when id1 <> id2 -> {g with edges=(id1,id2)::g.edges} |. Idle
        | _ -> state
      end
    )
  in

  let move_handler e =
    let c = rel_coords e in
    setState (fun _ ->
      match state with
      | Idle(_) -> state
      | Dragging((id, _),g) -> ((id, c), g) |. Dragging
      | AddEdgeSel(_) | AddEdge(_) -> state
    )
  in

  let up_handler _ =
    setState(fun _ ->
      match state with
       | Idle(_) -> state
       | Dragging(n, g) -> {g with nodes = n::g.nodes} |. Idle
       | AddEdgeSel(_) | AddEdge(_) -> state
    )
  in

  let add_node _ =
    setState(fun _ ->
      match state with
      | Idle(g) -> ((List.length g.nodes, (250,250)), g) |. Dragging
      | Dragging(_) -> state
      | AddEdgeSel(_) | AddEdge(_) -> state
    )
  in

  let add_edge_button _ =
    match state with
    | Idle(g) -> setState (fun _ -> AddEdge(g))
    | _ -> ()
  in

  let button_style =
    ReactDOMRe.Style.make ~border: "none" ~textAlign: "center" () in

  let full_nodes, full_edges =
    match state with
    | Idle(g) | AddEdgeSel(_, g) | AddEdge(g) -> g.nodes, g.edges
    | Dragging(n, g) -> (n::g.nodes), g.edges
  in


  [%bsx "
    <>
      <Viewport
        onMouseDown = "down_handler"
        onMouseMove = "move_handler"
        onMouseUp = "up_handler"
        height="height"
        width="width"
        radius ="10"
        nodes = "full_nodes"
        edges = "full_edges"
        />
      <div>
        <button
          style = "button_style"
          onClick = "add_node"
          >Add Node</button>
        <button
          style = "button_style"
          onClick = "add_edge_button"
          >Add Edge</button>
      </div>
    </>
  "]
[@@react.component]
