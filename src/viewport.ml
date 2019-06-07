open Dom_html

let make ~onMouseDown ~onMouseMove ~onMouseUp ~nodes ~edges ~radius ~width ~height =
  let canvas_ref = React.useRef Js.Nullable.null in


  let draw ctx =
    let draw_edge nodes (id1,id2) =
      let x1, y1 = List.assoc id1 nodes in
      let x2, y2 = List.assoc id2 nodes in
      moveTo ctx x1 y1;
      lineTo ctx x2 y2;
      stroke ctx
    in
    let draw_edges nodes edges = List.iter (draw_edge nodes) edges in

    let draw_node (_,(x,y)) =
      moveTo ctx x y;
      arc ctx x y radius 0. (Js.Math._PI *. 2.)
    in
    clearRect ctx 0 0 width height;
    beginPath ctx;
    draw_edges nodes edges;
    beginPath ctx;
    setFillStyle ctx "orange";
    List.iter draw_node nodes;
    fill ctx;

  in

  React.useEffect (fun () ->
    let ctx = canvas_ref |> React.Ref.current |> Js.Nullable.toOption
              |> domToCanvas |. getContext "2d" in
    draw ctx;
    None);


  [%bsx "
    <canvas ref = "(ReactDOMRe.Ref.domRef canvas_ref)"
      onMouseDown = "onMouseDown"
      onMouseMove = "onMouseMove"
      onMouseUp = "onMouseUp"
      height = "(string_of_int height)"
      width = "(string_of_int width)"
      style = "(ReactDOMRe.Style.make ~border:"1px solid #000000" ())"
      />
  "] [@@react.component]
