open Types
open File_saver_bind

let width = 800

let height = 800

let radius = 10

let rsq = radius * radius

let in_circle (x1, y1) (_, (x2, y2)) =
  let sq z = z * z in
  sq (x1 - x2) + sq (y1 - y2) <= 100

let dragging_to_idle s d = { s with nodes = d.active :: s.nodes; mode = Idle }

let down_handler state c =
  let find_node f =
    match List.find_opt (in_circle c) state.nodes with
    | None -> state
    | Some (id, coords) -> f id coords
  in
  match state.mode with
  | Dragging d -> dragging_to_idle state d
  | Idle ->
      find_node (fun id (x2, y2) ->
          let x1, y1 = c in
          let d = { active = (id, (x2, y2)); offset = (x2 - x1, y2 - y1) } in
          { state with
            nodes = List.remove_assoc id state.nodes;
            mode = Dragging d
          } )
  | AddEdge (None, fold) ->
      find_node (fun id _ -> { state with mode = (Some id, fold) |. AddEdge })
  | AddEdge (Some id1, fold) ->
      find_node (fun id2 _ ->
          if id1 = id2 then state
          else
            { state with
              edges = (id1, id2, fold) :: state.edges;
              mode = AddEdge (None, fold)
            } )
  | RemoveEdge None ->
      find_node (fun id _ -> { state with mode = Some id |. RemoveEdge })
  | RemoveEdge (Some id1) ->
      find_node (fun id2 _ ->
          { state with
            edges =
              List.filter
                (fun (n1, n2, _) ->
                  (n1, n2) <> (id1, id2) && (n2, n1) <> (id1, id2) )
                state.edges;
            mode = RemoveEdge None
          } )
  | RemoveNode ->
      find_node (fun id _ ->
          { state with
            edges =
              List.filter (fun (n1, n2, _) -> n1 <> id && n2 <> id) state.edges;
            nodes = List.remove_assoc id state.nodes;
            mode = RemoveNode
          } )

let mode_handler state f1 f2 =
  match state.mode with
  | Idle -> f1 state
  | Dragging d -> f2 d
  | AddEdge _ | RemoveNode | RemoveEdge _ -> state

(**return coordinates after snapping*)
let check_snap grid (x, y) =
  match grid with
  | None -> (x, y)
  | Some grid ->
      let x, y = (abs x, abs y) in
      let dx, dy = (width / grid, height / grid) in
      let mx, my = (x mod dx, y mod dy) in
      let x' =
        if mx <= 15 then x / dx * dx
        else if dx - mx <= 15 then ((x / dx) + 1) * dx
        else x
      and y' =
        if my <= 15 then y / dy * dy
        else if dy - my <= 15 then ((y / dy) + 1) * dy
        else y
      in
      (x', y')

let move_handler state (x1, y1) =
  match state.mode with
  | Dragging d ->
      let id, _ = d.active in
      let x2, y2 = d.offset in
      let d' =
        { d with active = (id, check_snap state.grid (x1 + x2, y1 + y2)) }
      in
      { state with mode = Dragging d'; mouse_pos = (x1, y1) }
  | Idle | AddEdge _ | RemoveNode | RemoveEdge _ ->
      { state with mouse_pos = (x1, y1) }

let up_handler state =
  mode_handler state (fun _ -> state) (dragging_to_idle state)

let mode_switch f state =
  match state.mode with
  | Dragging _ -> state
  | Idle | AddEdge _ | RemoveEdge _ | RemoveNode -> f ()

let add_node state =
  mode_switch
    (fun () ->
      let d' = { active = (state.next_id, (250, 250)); offset = (0, 0) } in
      { state with mode = Dragging d'; next_id = state.next_id + 1 } )
    state

let swap_fold = function
  | Valley -> Mountain
  | Mountain -> Valley

let swap_edges st =
  let e' =
    List.rev_map (fun (x, y, fold) -> (x, y, swap_fold fold)) st.edges
  in
  { st with edges = e' }

let min_list f lst =
  let h, t = (List.hd lst, List.tl lst) in
  List.fold_left
    (fun (acc, acc_score) x ->
      match f x with
      | score when score < acc_score -> (x, score)
      | _ -> (acc, acc_score) )
    (h, f h)
    t
  |> fst

let snap_nodes st =
  match st.grid with
  | None -> st
  | Some grid ->
      let width, height = (float_of_int width, float_of_int height) in
      let grid = float_of_int grid in
      let dx, dy = (width /. grid, height /. grid) in
      let closest_lattice (id, (x, y)) =
        let x, y = (float_of_int x, float_of_int y) in
        let cx, cy = (x /. dx |> floor, y /. dy |> floor) in
        let x', y' =
          [ (cx, cy); (cx +. 1., cy); (cx, cy +. 1.); (cx +. 1., cy +. 1.) ]
          |> min_list (fun (x2, y2) ->
                 ((x -. (x2 *. dx)) ** 2.) +. ((y -. (y2 *. dy)) ** 2.) )
        in
        (id, (x' *. dx |> int_of_float, y' *. dx |> int_of_float))
      in
      let nodes' = List.map closest_lattice st.nodes in
      { st with nodes = nodes' }

let gcode st =
  let scale_factor = 150. /. float_of_int height in
  let scale n offset =
    (float_of_int n *. scale_factor) +. offset |> Js.Float.toString
  in
  let go_nodes =
    List.map
      (fun (n1, n2, _) ->
        let x1, y1 = List.assoc n1 st.nodes in
        let x2, y2 = List.assoc n2 st.nodes in
        let x1', y1' = (scale x1 200., scale y1 5.) in
        let x2', y2' = (scale x2 200., scale y2 5.) in
        [ "G90";
          Printf.sprintf "G1 X%s Y%s F2000" x1' y1';
          "G91";
          "G1 Z-10";
          "G90";
          Printf.sprintf "G1 X%s Y%s F100" x2' y2';
          "G91";
          "G1 Z10"
        ] )
      st.edges
    |> List.concat
  in
  [ "G28 X Y"; "G91"; "G1 Z10" ] @ go_nodes |> String.concat "\n"

let download_gcode st =
  let blob =
    makeBlob [| gcode st |] @@ saver_config ~type_:"text/plain;charset=utf-8"
  in
  saveAs blob "creases.gcode";
  st

let reduce st = function
  | MouseDown c -> down_handler st c
  | MouseMove c -> move_handler st c
  | MouseUp _ -> up_handler st
  | AddNodeClick -> add_node st
  | AddEdgeClick fold ->
      mode_switch (fun () -> { st with mode = AddEdge (None, fold) }) st
  | ClearMode -> mode_switch (fun () -> { st with mode = Idle }) st
  | RemoveEdgeClick ->
      mode_switch (fun () -> { st with mode = RemoveEdge None }) st
  | RemoveNodeClick -> mode_switch (fun () -> { st with mode = RemoveNode }) st
  | SetGrid n -> { st with grid = Some n }
  | RemoveGrid -> { st with grid = None }
  | SwapEdges -> swap_edges st
  | SnapAllNodesClick -> snap_nodes st
  | GcodeClick -> download_gcode st
