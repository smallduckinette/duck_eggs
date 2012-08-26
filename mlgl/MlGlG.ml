let fully_inside stacked_matrix bounding_volume frustum =
  Bound.is_fully_inside 
	(Bound.transform stacked_matrix bounding_volume) frustum

let fully_outside stacked_matrix bounding_volume frustum =
  Bound.is_fully_outside 
	(Bound.transform stacked_matrix bounding_volume) frustum
	
let cull_children children stacked_matrix frustum obj =
  let culled_children = 
	List.map
	  (fun e -> e#cull stacked_matrix frustum)
	  children
  in
  let empty l = match l with [] -> true | _ -> false in
  let valid_children = 
	let rec filter l = 
	  match l with
		  [] -> []
		| (Some e)::q -> e::(filter q)
		| None::q -> filter q
	in
	  filter culled_children
  in
	if empty valid_children then None
	else Some (obj valid_children)

class type nodeT =
object
  method render : unit -> unit
  method get_bounding_volume : Bound.t
  method cull : Math.Matrix.t -> Frustum.t -> nodeT option
end

class virtual node =
object(self)
  method virtual render : unit -> unit
  method virtual get_bounding_volume : Bound.t
  method cull m f = 
	if Bound.is_fully_outside (Bound.transform m self#get_bounding_volume) f 
	then
	  None
	else
	  Some (self :> nodeT)
end
  
class group (e:nodeT list) =
object(self)
  inherit node

  val bounding_volume = 
	Bound.merge
	  (List.map (fun e -> e#get_bounding_volume) e)
	  
  method render () =
	List.iter (fun e -> e#render ()) e

  method get_bounding_volume = 
	bounding_volume

  method cull m f = 
	if fully_outside m self#get_bounding_volume f then
	  None
	else if fully_inside m self#get_bounding_volume f then
	  Some (self :> nodeT)
	else
	  cull_children e m f (new group)
end

