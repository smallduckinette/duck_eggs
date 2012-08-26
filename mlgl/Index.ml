module OrderedInt =
struct
  type t = int
  let compare = compare
end

module MapInt = Map.Make (OrderedInt)

type 'a t = int * 'a MapInt.t

let zero_based = (0, MapInt.empty)

let one_based = (1, MapInt.empty)

let push_back (i, map) entry =
  (i + 1, MapInt.add i entry map)

let get (_, map) i = 
  MapInt.find i map

let next (i, _) = i
