type plane = 
	{
	  a : float;
	  b : float;
	  c : float;
	  d : float
	}

let normalize p = 
  let n = sqrt (p.a *. p.a +. p.b *. p.b +. p.c *. p.c) in
	{
	  a = p.a /. n;
	  b = p.b /. n;
	  c = p.c /. n;
	  d = p.d /. n
	}

type t =
	{
	  right : plane;
	  left : plane;
	  bottom : plane;
	  top : plane;
	  far : plane;
	  near : plane
	}

let create proj view = 
  let m = Math.Matrix.m_mult proj view in
  let right = 
	{
	  a = (Math.Matrix.get m 0 3) -. (Math.Matrix.get m 0 0);
	  b = (Math.Matrix.get m 1 3) -. (Math.Matrix.get m 1 0);
	  c = (Math.Matrix.get m 2 3) -. (Math.Matrix.get m 2 0);
	  d = (Math.Matrix.get m 3 3) -. (Math.Matrix.get m 3 0)
	}
  and left =
	{
	  a = (Math.Matrix.get m 0 3) +. (Math.Matrix.get m 0 0);
	  b = (Math.Matrix.get m 1 3) +. (Math.Matrix.get m 1 0);
	  c = (Math.Matrix.get m 2 3) +. (Math.Matrix.get m 2 0);
	  d = (Math.Matrix.get m 3 3) +. (Math.Matrix.get m 3 0)  
	}
  and bottom = 
	{
	  a = (Math.Matrix.get m 0 3) +. (Math.Matrix.get m 0 1);
	  b = (Math.Matrix.get m 1 3) +. (Math.Matrix.get m 1 1);
	  c = (Math.Matrix.get m 2 3) +. (Math.Matrix.get m 2 1);
	  d = (Math.Matrix.get m 3 3) +. (Math.Matrix.get m 3 1)  
	}
  and top =
	{
	  a = (Math.Matrix.get m 0 3) -. (Math.Matrix.get m 0 1);
	  b = (Math.Matrix.get m 1 3) -. (Math.Matrix.get m 1 1);
	  c = (Math.Matrix.get m 2 3) -. (Math.Matrix.get m 2 1);
	  d = (Math.Matrix.get m 3 3) -. (Math.Matrix.get m 3 1)  
	}
  and far = 
	{
	  a = (Math.Matrix.get m 0 3) -. (Math.Matrix.get m 0 2);
	  b = (Math.Matrix.get m 1 3) -. (Math.Matrix.get m 1 2);
	  c = (Math.Matrix.get m 2 3) -. (Math.Matrix.get m 2 2);
	  d = (Math.Matrix.get m 3 3) -. (Math.Matrix.get m 3 2)  	  
	}
  and near = 
	{
	  a = (Math.Matrix.get m 0 3) +. (Math.Matrix.get m 0 2);
	  b = (Math.Matrix.get m 1 3) +. (Math.Matrix.get m 1 2);
	  c = (Math.Matrix.get m 2 3) +. (Math.Matrix.get m 2 2);
	  d = (Math.Matrix.get m 3 3) +. (Math.Matrix.get m 3 2)  	  
	}
  in 
	{
	  right = normalize right;
	  left = normalize left;
	  bottom = normalize bottom;
	  top = normalize top;
	  far = normalize far;
	  near = normalize near
	}

let is_inside_plane p v = 
  (p.a *. (Math.Vector.get_x v)
   +. p.b *. (Math.Vector.get_y v)
   +. p.c *. (Math.Vector.get_z v)
   +. p.d) >= 0.
	
let is_inside f v = 
  (is_inside_plane f.right v)
  && (is_inside_plane f.left v)
  && (is_inside_plane f.bottom v)
  && (is_inside_plane f.top v)
  && (is_inside_plane f.far v)
  && (is_inside_plane f.near v)
	  
let is_fully_outside f l = 
  let fully_outside p = 
	List.for_all 
	  (fun e -> not (is_inside_plane p e))
	  l
  in
	(fully_outside f.right)
	|| (fully_outside f.left)
	|| (fully_outside f.bottom)
	|| (fully_outside f.top)
	|| (fully_outside f.far)
	|| (fully_outside f.near)

let is_fully_inside f l =
  let fully_inside p = 
	List.for_all 
	  (fun e -> is_inside_plane p e)
	  l
  in
	(fully_inside f.right)
	&& (fully_inside f.left)
	&& (fully_inside f.bottom)
	&& (fully_inside f.top)
	&& (fully_inside f.far)
	&& (fully_inside f.near)
	  

let test_inside f v =
  let is_inside_plane p = 
	(p.a *. (Math.Vector.get_x v)
	 +. p.b *. (Math.Vector.get_y v)
	 +. p.c *. (Math.Vector.get_z v)
	 +. p.d) >= 0.
  in
	"right   : " ^ (string_of_bool (is_inside_plane f.right)) ^ "\n"
	^ "left    : " ^ (string_of_bool (is_inside_plane f.left)) ^ "\n"
	^ "bottom  : " ^ (string_of_bool (is_inside_plane f.bottom)) ^ "\n"
	^ "top     : " ^ (string_of_bool (is_inside_plane f.top)) ^ "\n"
	^ "far     : " ^ (string_of_bool (is_inside_plane f.far)) ^ "\n"
	^ "near    : " ^ (string_of_bool (is_inside_plane f.near))
	

let to_string f = 
  let plane_to_string p = 
	(string_of_float p.a) ^ " "
	^ (string_of_float p.b) ^ " "
	^ (string_of_float p.c) ^ " "
	^ (string_of_float p.d)
  in
	"right - " ^ (plane_to_string f.right) ^ "\n"
	^ "left - " ^ (plane_to_string f.left) ^ "\n"
	^ "bottom - " ^ (plane_to_string f.bottom) ^ "\n"
	^ "top - " ^ (plane_to_string f.top) ^ "\n"
	^ "far - " ^ (plane_to_string f.far) ^ "\n"
	^ "near - " ^ (plane_to_string f.near)
