type blob
type saver_config =
{
  type_: string [@bs.as "type"];
} [@@bs.deriving abstract]

external makeBlob: string array -> saver_config -> blob = "Blob" [@@bs.new]

external saveAs: blob -> string -> unit = "saveAs" [@@bs.module "file-saver"]
