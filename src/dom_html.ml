type context = {
  mutable fillStyle: string;
  mutable strokeStyle: string
} [@@bs.deriving abstract]
type canvas

external getContext : canvas -> string -> context = "getContext" [@@bs.send]

external domToCanvasext : Dom.element -> canvas = "%identity"

let domToCanvas = function
  | Some x -> domToCanvasext x
  | None -> failwith "canvas undefined"

external arc : context -> int -> int -> int -> float -> float -> unit = "" [@@bs.send]

external beginPath : context -> unit = "" [@@bs.send]

external fill : context -> unit = "" [@@bs.send]

external stroke : context -> unit = "" [@@bs.send]

external moveTo : context -> int -> int -> unit = "" [@@bs.send]

external clearRect : context -> int -> int -> int -> int -> unit = "" [@@bs.send]

external lineTo : context -> int -> int -> unit = "" [@@bs.send]

let setFillStyle = fillStyleSet
let setStrokeStyle = strokeStyleSet
