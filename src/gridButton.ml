
type state = GridInput of int option | NotClicked

let make ~setGrid ~removeGrid =
  let state, setState = React.useState (fun () -> NotClicked) in

  let handleChange e =
    let (s:string) = (ReactEvent.Form.target e)##value in
    match int_of_string_opt s with
      | Some x when x > 0 -> begin
        setGrid x;
        setState (fun _ -> Some x |. GridInput)
        end
      | None when s = "" -> setState (fun _ -> GridInput(None))
      | None | Some _ -> ()
  in

  match state with
    | NotClicked ->
      [%bsx "
          <button onClick = "(fun _ -> setGrid 2; setState (fun _ -> GridInput(Some 2)))">
            Add Grid</button>
      "]
    | GridInput(s) ->
      let s' = match s with
        | None -> ""
        | Some x -> string_of_int x
      in
      [%bsx "
          <>
          <input type="("text")"
            onChange = "handleChange"
            value="s'"/>
          <button onClick = "(fun _ -> removeGrid (); setState (fun _ -> NotClicked))">
           Remove Grid</button>
          </>
      "]
  [@@react.component]
