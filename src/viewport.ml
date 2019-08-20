open Dom_html

let make ~onMouseDown ~onMouseMove ~onMouseUp ~nodes ~mountains ~valleys
    ~radius ~width ~height ~hNodes ~grid ~keyHandler =
  let canvas_ref = React.useRef Js.Nullable.null in
  let draw_grid ctx =
    setLineDash ctx [||];
    match grid with
    | None -> ()
    | Some grid ->
        strokeStyle ctx "#8eb2f1";
        beginPath ctx;
        let dx = width / grid in
        let dy = height / grid in
        let rec draw_line f d n =
          if n = 0 then ()
          else
            let c = n * d in
            f c;
            draw_line f d (n - 1)
        in
        let draw_line_vert =
          draw_line
            (fun x ->
              ctx |. moveTo x 0;
              ctx |. lineTo x height )
            dx
        in
        let draw_line_horiz =
          draw_line
            (fun y ->
              ctx |. moveTo 0 y;
              ctx |. lineTo width y )
            dy
        in
        draw_line_vert (grid - 1);
        draw_line_horiz (grid - 1);
        stroke ctx
  in
  let draw ctx =
    let draw_edge nodes (id1, id2) =
      let x1, y1 = List.assoc id1 nodes in
      let x2, y2 = List.assoc id2 nodes in
      moveTo ctx x1 y1;
      lineTo ctx x2 y2
    in
    let draw_edges nodes edges =
      beginPath ctx;
      strokeStyle ctx "#000000";
      List.iter (draw_edge nodes) edges;
      stroke ctx
    in
    let draw_mountains nodes edges =
      setLineDash ctx [||];
      draw_edges nodes edges
    in
    let draw_valleys nodes edges =
      setLineDash ctx [| 10; 10 |];
      draw_edges nodes edges
    in
    let draw_node (_, (x, y)) =
      moveTo ctx x y;
      arc ctx x y radius 0. (Js.Math._PI *. 2.)
    in
    let draw_nodes c lst =
      beginPath ctx;
      ctx |. fillStyle c;
      List.iter draw_node lst;
      ctx |. fill
    in
    clearRect ctx 0 0 width height;
    draw_grid ctx;
    draw_mountains nodes mountains;
    draw_valleys nodes valleys;
    (* draw_nodes "#FF7F50" nodes; *)
    draw_nodes "rgba(255, 127, 80, 0.5)" nodes;
    draw_nodes "#6495ED" hNodes
  in
  React.useEffect (fun () ->
      let ctx =
        canvas_ref |> React.Ref.current |> Js.Nullable.toOption |> domToCanvas
        |. getContext "2d"
      in
      draw ctx;
      None );
  [%bsx "
    <canvas
      tabIndex = "0"
      onKeyDown = "keyHandler"
      ref = "(ReactDOMRe.Ref.domRef canvas_ref)"
      onMouseDown = "onMouseDown"
      onMouseMove = "onMouseMove"
      onMouseUp = "onMouseUp"
      height = "(string_of_int height)"
      width = "(string_of_int width)"
      style = "(ReactDOMRe.Style.make ~border:"1px solid #000000" ~outline:"none" ())"
      />
  "] [@@react.component]
