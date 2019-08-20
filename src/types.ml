
type coord = int * int
type n_id = int

type node = n_id * coord

type drag_state =
  {active: node;
   offset: coord}

type fold = Mountain | Valley

type mode =
  | Dragging of drag_state
  | Idle
  | AddEdge of n_id option * fold
  | RemoveEdge of n_id option
  | RemoveNode

type state =
  {nodes: node list;
   edges: (n_id * n_id * fold) list;
   mouse_pos: coord;
   mode: mode;
   next_id: n_id;
   grid: int option}

type action =
  | MouseDown of coord
  | MouseMove of coord
  | MouseUp of coord
  | AddNodeClick
  | AddEdgeClick of fold
  | ClearMode
  | RemoveEdgeClick
  | RemoveNodeClick
  | SetGrid of int
  | RemoveGrid
  | SwapEdges
  | SnapAllNodesClick
  | GcodeClick
