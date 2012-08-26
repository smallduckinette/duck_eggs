open Math.Vector.Ops

let epsilon = 0.0001

module Plane = 
struct
  type t = 
	  {
		p : Math.Vector.t;
		n : Math.Vector.t
	  }
end
  
module Ray = 
struct
  type t = 
	  {
		p : Math.Vector.t;
		n : Math.Vector.t
	  }
end

module Sphere = 
struct
  type t = 
	  {
		c : Math.Vector.t;
		r : float
	  }
end

let distance_plane_ray p r = 
  let num = p.Plane.p *| p.Plane.n -. r.Ray.p *| p.Plane.n
  and denom = p.Plane.n *| r.Ray.n in
	if (abs_float denom) < epsilon then
	  if (abs_float num) > epsilon then
		None
	  else
		Some 0.
	else
	  let distance = num /. denom in
		if distance > 0. then 
		  Some distance
		else if distance > (-. epsilon) then
		  Some 0.
		else
		  None

let distance_plane_point plane p = 
  (p -| plane.Plane.p) *| plane.Plane.n

let distance_sphere_ray s r = 
  let b = r.Ray.n *| (r.Ray.p -| s.Sphere.c)
  and c = (Math.Vector.norm (r.Ray.p -| s.Sphere.c)) -. s.Sphere.r *. s.Sphere.r
  in
  let delta = b -. c in
	if delta < 0. then
	  None
	else
	  Some ((-. b) -. (sqrt(delta)))

let intersection_point_segment p a b = 
  let d = (p -| a) *| (b -| a) in
	if d < 0. then a
	else if d > 0. then b
	else
	  a +| ((b -| a) **| d)

let collision_plane_sphere p s v = 
  let d = distance_plane_point p (s.Sphere.c +| v) in
	if d >= s.Sphere.r then
	  None
	else
	  let coll_p = s.Sphere.c -| (p.Plane.n **| s.Sphere.r) in
	  let v_d = distance_plane_ray p {Ray.p = coll_p; Ray.n = (Math.Vector.normalize v)} in
		match v_d with
			None -> None
		  | Some e when (e >= Math.Vector.norm v) -> None
		  | Some e -> Some (e, p.Plane.n)

let rec collision_planes_sphere p s v =
  if Math.Vector.norm v < epsilon then
	Math.Vector.zero
  else
	let min_plan p1 p2 = 
	  match (p1, p2) with
		  None, None -> None
		| Some e, None -> Some e
		| None, Some e -> Some e
		| Some (d1, n1), Some (d2, n2) -> 
			if d1 < d2 then 
			  Some (d1, n1) 
			else 
			  Some (d2, n2)
	in
	let collision_plane = 
	  List.fold_left 
		(
		  fun best_plan plan -> 
			let other_plan = collision_plane_sphere plan s v in
			  min_plan best_plan other_plan
		)
		None
		p
	in
	  match collision_plane with
		  None -> v
		| Some (d, n) ->
			let norm_v = Math.Vector.norm v in
			let before_collision = v **| (d /. norm_v) in
			let after_collision = v **| ((norm_v -. d) /. norm_v) in
			let new_s = {s with Sphere.c = s.Sphere.c +| before_collision} in
			let slided_after_collision = after_collision -| (n **| (after_collision *| n)) in
			  before_collision +| (collision_planes_sphere p new_s (slided_after_collision))
