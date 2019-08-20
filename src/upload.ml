open BsReactstrap
type form_data = <name: string> Js.t
type edges = (Types.n_id * Types.n_id) list
type state = Empty | SelectedFile of form_data | Response of Types.coord list * edges | Loading of int
external create_form_data: unit -> form_data = "FormData" [@@bs.new]
external form_append: form_data -> string -> 'a -> unit = "append" [@@bs.send]
let styles : UploadStyles.definition = [%raw "require('../../../styles/upload.css')"]
let button_class = "btn btn-success btn-block"

let make () =

let state, setState = React.useState (fun () -> Empty) in

let form_handler e =
  let f = (ReactEvent.Form.target e)##files |. Array.get 0 in
  let data = create_form_data () in
  let () =  data |. form_append "file" f in
  setState (fun _ -> SelectedFile data)
  in

let button_handler _ =
  match state with
  | Empty | Response _ | Loading _ -> ()
  | SelectedFile data ->
    let onUploadProgress p =
      setState (fun _ ->
        (p##loaded /. p##total *. 100.) |> int_of_float |. Loading)
    in
    let config =
      Axios.makeConfig ~onUploadProgress:onUploadProgress ()
    in
    Js.log "sending";
    Js.Promise.(
      Axios.postDatac "/api/extract-graph" data config
      |> then_ @@ fun res ->
        let nodes = res##data##nodes |> Array.to_list in
        let edges = res##data##edges |> Array.to_list in
        let tup_map = List.map @@ fun (x: int array) -> x.(0), x.(1) in
        setState (fun _ -> Response(tup_map nodes, tup_map edges)) |> resolve
    ) |> ignore
  in

let upload_jsx pr =
 [%bsx "
    <div className=container>
      <div className=row>
        <div className=col-md-6>
            <form method=post action=# id=#>
              <div className="("form-group " ^ styles##files)">
                <label>Upload Your File</label>
                <input type=file class=form-control multiple="false" onChange="(form_handler)"></input>
              </div>
            </form>
            <div class=form-group>
            <Progress max=100 color=success value="pr">"(string_of_int pr ^ "%" |> React.string)"</Progress>
            </div>
            <button type=button className="button_class" onClick="button_handler">Upload</button>
        </div>
      </div>
    </div>
  "]
in

match state with
| Empty | SelectedFile _ -> upload_jsx 0
| Loading pr -> upload_jsx pr
| Response(nodes, edges) ->
  [%bsx "<ButtonPanel init_nodes="nodes" init_edges="edges" /> "]
[@@react.component]
