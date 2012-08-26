type t = Math.Vector.t list

let box v1 v2 = 
  let x = Math.Vector.get_x
  and y = Math.Vector.get_y
  and z = Math.Vector.get_z in
	[
	  Math.Vector.make (x v1) (y v1) (z v1);
	  Math.Vector.make (x v1) (y v1) (z v2);
	  Math.Vector.make (x v1) (y v2) (z v1);
	  Math.Vector.make (x v1) (y v2) (z v2);
	  Math.Vector.make (x v2) (y v1) (z v1);
	  Math.Vector.make (x v2) (y v1) (z v2);
	  Math.Vector.make (x v2) (y v2) (z v1);
	  Math.Vector.make (x v2) (y v2) (z v2)
	]
	  
let transform m b =
  List.map
	(Math.Matrix.v_mult m)
	b

let merge bs = 
  let bv1, bv2 = 
	match (List.concat bs) with
		[] -> Math.Vector.zero, Math.Vector.zero
	  | t::q ->
		  List.fold_left
			(fun (v1, v2) v -> 
			   (Math.Vector.min_v v1 v, Math.Vector.max_v v2 v))
			(t, t)
			q
  in
	box bv1 bv2
	  
let from_raw vectors = 
  merge [vectors]

let is_fully_outside bs f = 
  Frustum.is_fully_outside f bs

let is_fully_inside bs f =
  Frustum.is_fully_inside f bs

let to_string bv = 
  String.concat "\n" (List.map Math.Vector.to_string bv)

let render bv =
  GlDraw.begins `line_strip;
  List.iter
	(fun e -> 
	   GlDraw.vertex
		 ~x:(Math.Vector.get_x e)
		 ~y:(Math.Vector.get_y e)
		 ~z:(Math.Vector.get_z e)
		 ())
	bv;
  GlDraw.ends()
	   
