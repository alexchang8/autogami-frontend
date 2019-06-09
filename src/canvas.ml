type coord = int * int
type n_id = int

type node = n_id * coord

type graph = {nodes: node list; edges: (n_id * n_id) list; mouse_pos: int*int}

type drag_state = {active: node; graph: graph; offset: int*int}

type state =
  Dragging of drag_state | Idle of graph | AddEdgeSel of n_id * graph |
  AddEdge of graph | RemoveEdge of graph | RemoveEdgeSel of n_id * graph |
  RemoveNode of graph

type action =
  MouseDown of coord | MouseMove of coord | MouseUp of coord | AddNodeClick |
  AddEdgeClick | ClearMode | RemoveEdgeClick | RemoveNodeClick

let grid = None
let radius = 10
let rsq = radius * radius
let width = 800
let height = 800

let in_circle (x1, y1) (_,(x2,y2)) =
  let sq z = z*z in
  sq (x1 - x2) + sq (y1 - y2) <= rsq

let rel_coords e =
    ReactEvent.Mouse.stopPropagation e;
    let native = ReactEvent.Mouse.nativeEvent e in
    let x = int_of_float native##offsetX in
    let y = int_of_float native##offsetY in
    x,y

let init_state = {nodes= [(0, (50, 50));(1, (100, 100));(2, (100, 150))];
                  edges = [(0,1);(0,2);(2, 1)]; mouse_pos = -50,-50} |. Idle

let make () =

  let dragging_to_idle d = {d.graph with nodes = d.active::d.graph.nodes} |. Idle in

  let down_handler state c =
    let find_node_f f g =
      match List.find_opt (in_circle c) g.nodes with
      | None -> state
      | Some(id, coords) -> f id coords
    in
    match state with
    | Dragging(d) -> dragging_to_idle d
    | Idle(g) ->
      find_node_f (fun id (x2,y2) ->
        let x1, y1 = c in
        let g' = {g with nodes = List.remove_assoc id g.nodes} in
        {active = id,(x2,y2); graph = g'; offset= x2-x1, y2-y1} |. Dragging) g
    | AddEdge(g) ->
      find_node_f (fun id _ -> (id, g) |. AddEdgeSel) g
    | AddEdgeSel(id1, g) ->
      find_node_f (fun id2 _ -> if id1 = id2 then state
                   else {g with edges=(id1,id2)::g.edges} |. AddEdge) g
    | RemoveEdge(g) ->
      find_node_f (fun id _ -> (id, g) |. RemoveEdgeSel) g
    | RemoveEdgeSel(id1, g) ->
      find_node_f (fun id2 _ ->
        let e' = List.filter (fun (e1,e2) ->
          ((e1,e2) <> (id1, id2)) && ((e2, e1) <> (id1, id2))) g.edges in
          {g with edges =e'} |. RemoveEdge) g
    | RemoveNode(g) ->
      find_node_f (fun id _ ->
        let e' = List.filter (fun (e1,e2) -> (e1 <> id) && (e2 <> id)) g.edges and
        n' = List.remove_assoc id g.nodes in
        {g with edges = e'; nodes = n'} |. RemoveNode) g
  in

  let make_handler state f1 f2 =
    match state with
    | Idle(g) -> f1 g
    | Dragging(d) -> f2 d
    | AddEdgeSel(_) | AddEdge(_) | RemoveEdge(_) | RemoveEdgeSel(_)
      | RemoveNode(_) -> state
  in

  let check_snap (x,y) =
    match grid with
    | None -> (x,y)
    | Some grid ->
      let x,y = abs x, abs y and
      dx = width/grid and
      dy = height/grid in
      let mx = x mod dx and
      my = y mod dy in
      let x' = if mx <= 15 then (x/dx)*dx
        else if dx - mx <= 15 then (x/dx + 1)*dx
        else x and
      y' = if my <= 15 then (y/dy)*dy
        else if dy - my <= 15 then (y/dy + 1)*dy
        else y in
      (x', y')
  in

  let move_handler state c =
    make_handler state (fun _ -> state) (fun d ->
      let id,_ = d.active in
      let x1, y1 = c in
      let x2, y2 = d.offset in
      {d with active = id, check_snap (x1 + x2, y1 + y2)} |. Dragging)
  in

  let up_handler state =
    make_handler state (fun _ -> state) dragging_to_idle in

  let mode_switch f = function
    | Dragging(d) -> Dragging(d)
    | Idle(g) | AddEdgeSel(_, g) | AddEdge(g) | RemoveEdge(g)
      | RemoveEdgeSel(_, g) | RemoveNode(g) -> f g in

  let add_node = mode_switch (fun g ->
      {active = List.length g.nodes, (250,250); graph = g; offset = 0,0} |. Dragging)
  in

  let reduce st a = match a with
    | MouseDown(c) -> down_handler st c
    | MouseMove(c) -> move_handler st c
    | MouseUp(_) -> up_handler st
    | AddNodeClick -> add_node st
    | AddEdgeClick -> mode_switch (fun g -> AddEdge(g)) st
    | ClearMode -> mode_switch (fun g -> Idle(g)) st
    | RemoveEdgeClick -> mode_switch (fun g -> RemoveEdge(g)) st
    | RemoveNodeClick -> mode_switch (fun g -> RemoveNode(g)) st
  in

  let state, dispatch = React.useReducer reduce init_state in

  let coord_dispatch f e = rel_coords e |> f |> dispatch in

  let full_nodes, full_edges =
    match state with
    | Idle(g) | AddEdgeSel(_, g) | AddEdge(g) | RemoveEdge(g)
      | RemoveEdgeSel(_,g) | RemoveNode(g) -> g.nodes, g.edges
    | Dragging(d) -> (d.active::d.graph.nodes), d.graph.edges
  in

  let button_style =
    ReactDOMRe.Style.make ~border: "none" ~textAlign: "center" () in

  let mode_string = function
    | Dragging(_) | Idle(_) -> "normal mode"
    | RemoveEdgeSel(_) | RemoveEdge(_) -> "remove edge mode"
    | AddEdgeSel(_) | AddEdge(_) -> "add edge mode"
    | RemoveNode(_) -> "remove node mode"
  in

  let active_node = function
    | Dragging(d) -> Some(d.active)
    | AddEdgeSel(id, g) | RemoveEdgeSel(id, g) ->
      List.find (fun (id2, _) -> id = id2) g.nodes |. Some
    | Idle(_) | AddEdge(_) | RemoveEdge(_) | RemoveNode(_) -> None
  in

  let keyHandler e =
    match ReactEvent.Keyboard.key e with
    | "a" -> dispatch AddNodeClick
    | "d" -> dispatch RemoveNodeClick
    | "e" -> dispatch AddEdgeClick
    | "r" -> dispatch RemoveEdgeClick
    | "Escape" -> dispatch ClearMode
    | _ -> ()

  in

  [%bsx "
    <div>
      <p
        style = "(ReactDOMRe.Style.make ~margin: "0px" ~padding: "0px" ())"
        >"(mode_string state |> React.string)"</p>
      <Viewport
        onMouseDown = "(coord_dispatch (fun c -> MouseDown c))"
        onMouseMove = "(coord_dispatch (fun c -> MouseMove c))"
        onMouseUp = "(coord_dispatch (fun c -> MouseUp c))"
        height = "height"
        width = "width"
        radius = "10"
        nodes = "full_nodes"
        edges = "full_edges"
        activeNode = "(active_node state)"
        grid = "grid"
        keyHandler = "keyHandler"
        />
      <div>
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
          onClick = "(fun _ -> dispatch AddEdgeClick)"
          >Add Edge</button>
        <button
          style = "button_style"
          onClick = "(fun _ -> dispatch RemoveEdgeClick)"
          >Remove Edge</button>
        <button
          style = "button_style"
          onClick = "(fun _ -> dispatch ClearMode)"
          >Clear Mode</button>
      </div>
    </div>
  "]
[@@react.component]
