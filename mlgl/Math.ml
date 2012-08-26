(* The vector type *)

(* The float precision. Very arbitrary number *)
let precision = 0.00001

(* Floats compare function, based on the precision *)
let equal f1 f2 = 
  abs_float (f2 -. f1) < precision

let pi = 3.1415926535897932384626433832795028841971693

(* Some 2nd degree equation resolution functions *)
let delta a b c = b *. b -. 4. *. a *. c

let solutions a b delta = 
  let sqrt_delta = sqrt delta in
  let x1 = (-. b -. sqrt_delta) /. 2. *. a and
      x2 = (-. b +. sqrt_delta) /. 2. *. a in
    (x1, x2)

module Vector = 
struct
  type t = 
      {
	x : float;
	y : float;
	z : float;
	w : float;
      }
	
  (* Creation, getters *)
  let make x y z = 
    {
      x = x;
      y = y;
      z = z;
      w = 1.
    }
      
  let get_x v = v.x
    
  let get_y v = v.y
    
  let get_z v = v.z
    
  let zero =
  {
    x = 0.;
    y = 0.;
    z = 0.;
    w = 1.
  }
    
  let x =
    {
      x = 1.;
      y = 0.;
      z = 0.;
      w = 1.
    }
      
  let y =
    {
      x = 0.;
      y = 1.;
      z = 0.;
      w = 1.
    }

  let z =
    {
      x = 0.;
      y = 0.;
      z = 1.;
      w = 1.
    }
      
  (* Operations *)
      
  let add v1 v2 =
    {
      x = v1.x +. v2.x;
      y = v1.y +. v2.y;
      z = v1.z +. v2.z;
      w = 1.
    }
      
  let sub v1 v2 =
    {
      x = v1.x -. v2.x;
      y = v1.y -. v2.y;
      z = v1.z -. v2.z;
      w = 1.
    }
      
  let dot v1 v2 =
    v1.x *. v2.x
    +. v1.y *. v2.y
    +. v1.z *. v2.z
      
  let cross v1 v2 =
    {
      x = v1.y *. v2.z -. v1.z *. v2.y;
      y = -. v1.x *. v2.z +. v1.z *. v2.x;
      z = v1.x *. v2.y -. v1.y *. v2.x;
      w = 1.
    }
      
  let mult v r =
    {
      x = v.x *. r;
      y = v.y *. r;
      z = v.z *. r;
      w = 1.
    }

  let v_mult v1 v2 = 
    {
      x = v1.x *. v2.x;
      y = v1.y *. v2.y;
      z = v1.z *. v2.z;
      w = 1.
    }

  let equal v1 v2 = 
    equal v1.x v2.x
    && equal v1.y v2.y
    && equal v1.z v2.z
    && equal v1.w v2.w
     
  let normalize v = 
    mult v (1. /. sqrt (dot v v))
 
  let sqnorm v = 
    dot v v

  let norm v = sqrt(sqnorm v)

  let min_v v1 v2 = 
	{
	  x = min v1.x v2.x;
	  y = min v1.y v2.y;
	  z = min v1.z v2.z;
	  w = 1.
	}

  let max_v v1 v2 = 
	{
	  x = max v1.x v2.x;
	  y = max v1.y v2.y;
	  z = max v1.z v2.z;
	  w = 1.
	}

  let min_l l =
	match l with
		[] -> raise (Invalid_argument "min_l")
	  | t::q ->
		  List.fold_left
			min_v
			t
			q

  let max_l l =
	match l with
		[] -> raise (Invalid_argument "max_l")
	  | t::q ->
		  List.fold_left
			max_v
			t
			q

  let to_string v =
	"(" ^ (string_of_float v.x) 
	^ ", " ^ (string_of_float v.y) 
	^ ", " ^ (string_of_float v.z)
	^ ")"

  module Ops = 
  struct
	let ( +| ) = add
	let ( -| ) = sub
	let ( *| ) = dot
	let ( ^| ) = cross
	let ( **| ) = mult
  end
end

(* The matrix type *)
(* Just for info, most of the code here has been *)
(* generated, because I'm lazy *)

module Matrix = 
struct
  type t =
      {
	m00 : float;
	m01 : float;
	m02 : float;
	m03 : float;
	m10 : float;
	m11 : float;
	m12 : float;
	m13 : float;
	m20 : float;
	m21 : float;
	m22 : float;
	m23 : float;
	m30 : float;
	m31 : float;
	m32 : float;
	m33 : float
      }
	
  let zero =
    {
      m00 = 0.;
      m01 = 0.;
      m02 = 0.;
      m03 = 0.;
      m10 = 0.;
      m11 = 0.;
      m12 = 0.;
      m13 = 0.;
      m20 = 0.;
      m21 = 0.;
      m22 = 0.;
      m23 = 0.;
      m30 = 0.;
      m31 = 0.;
      m32 = 0.;
      m33 = 0.
    }  

  let identity =  
    {
      m00 = 1.;
      m01 = 0.;
      m02 = 0.;
      m03 = 0.;
      m10 = 0.;
      m11 = 1.;
      m12 = 0.;
      m13 = 0.;
      m20 = 0.;
      m21 = 0.;
      m22 = 1.;
      m23 = 0.;
      m30 = 0.;
      m31 = 0.;
      m32 = 0.;
      m33 = 1.
    }
	  
  let get m i j =
	match(i, j) with
		(0, 0) -> m.m00
	  | (0, 1) -> m.m01
	  | (0, 2) -> m.m02
	  | (0, 3) -> m.m03
	  | (1, 0) -> m.m10
	  | (1, 1) -> m.m11
	  | (1, 2) -> m.m12
	  | (1, 3) -> m.m13
	  | (2, 0) -> m.m20
	  | (2, 1) -> m.m21
	  | (2, 2) -> m.m22
	  | (2, 3) -> m.m23
	  | (3, 0) -> m.m30
	  | (3, 1) -> m.m31
	  | (3, 2) -> m.m32
	  | (3, 3) -> m.m33
	  | _ -> raise (Invalid_argument "Out of range")	  

  let add m1 m2 =  
    {
      m00 = m1.m00 +. m2.m00;
      m01 = m1.m01 +. m2.m01;
      m02 = m1.m02 +. m2.m02;
      m03 = m1.m03 +. m2.m03;
      m10 = m1.m10 +. m2.m10;
      m11 = m1.m11 +. m2.m11;
      m12 = m1.m12 +. m2.m12;
      m13 = m1.m13 +. m2.m13;
      m20 = m1.m20 +. m2.m20;
      m21 = m1.m21 +. m2.m21;
      m22 = m1.m22 +. m2.m22;
      m23 = m1.m23 +. m2.m23;
      m30 = m1.m30 +. m2.m30;
      m31 = m1.m31 +. m2.m31;
      m32 = m1.m32 +. m2.m32;
      m33 = m1.m33 +. m2.m33
    }

  let to_array m =
	[|
	  [|m.m00; m.m01; m.m02; m.m03|];
	  [|m.m10; m.m11; m.m12; m.m13|];
	  [|m.m20; m.m21; m.m22; m.m23|];
	  [|m.m30; m.m31; m.m32; m.m33|]
	|]

  let from_array a =
	match a with 
		[|
		  [|m00; m01; m02; m03|];
		  [|m10; m11; m12; m13|];
		  [|m20; m21; m22; m23|];
		  [|m30; m31; m32; m33|]
		|] -> 
		  {
			m00 = m00;
			m01 = m01;
			m02 = m02;
			m03 = m03;
			m10 = m10;
			m11 = m11;
			m12 = m12;
			m13 = m13;
			m20 = m20;
			m21 = m21;
			m22 = m22;
			m23 = m23;
			m30 = m30;
			m31 = m31;
			m32 = m32;
			m33 = m33
		  }
	  | _ -> raise (Invalid_argument "from_array")
		
  let transpose m =
	{
	  m00 = m.m00;
	  m01 = m.m10;
	  m02 = m.m20;
	  m03 = m.m30;
	  m10 = m.m01;
	  m11 = m.m11;
	  m12 = m.m21;
	  m13 = m.m31;
	  m20 = m.m02;
	  m21 = m.m12;
	  m22 = m.m22;
	  m23 = m.m32;
	  m30 = m.m03;
	  m31 = m.m13;
	  m32 = m.m23;
	  m33 = m.m33
	}
	  

  let m_mult m1 m2 =
    {
      m00 = m1.m00*.m2.m00 +. m1.m01*.m2.m10 +. m1.m02*.m2.m20 +. m1.m03*.m2.m30;
      m01 = m1.m00*.m2.m01 +. m1.m01*.m2.m11 +. m1.m02*.m2.m21 +. m1.m03*.m2.m31;
      m02 = m1.m00*.m2.m02 +. m1.m01*.m2.m12 +. m1.m02*.m2.m22 +. m1.m03*.m2.m32;
      m03 = m1.m00*.m2.m03 +. m1.m01*.m2.m13 +. m1.m02*.m2.m23 +. m1.m03*.m2.m33;
      m10 = m1.m10*.m2.m00 +. m1.m11*.m2.m10 +. m1.m12*.m2.m20 +. m1.m13*.m2.m30;
      m11 = m1.m10*.m2.m01 +. m1.m11*.m2.m11 +. m1.m12*.m2.m21 +. m1.m13*.m2.m31;
      m12 = m1.m10*.m2.m02 +. m1.m11*.m2.m12 +. m1.m12*.m2.m22 +. m1.m13*.m2.m32;
      m13 = m1.m10*.m2.m03 +. m1.m11*.m2.m13 +. m1.m12*.m2.m23 +. m1.m13*.m2.m33;
      m20 = m1.m20*.m2.m00 +. m1.m21*.m2.m10 +. m1.m22*.m2.m20 +. m1.m23*.m2.m30;
      m21 = m1.m20*.m2.m01 +. m1.m21*.m2.m11 +. m1.m22*.m2.m21 +. m1.m23*.m2.m31;
      m22 = m1.m20*.m2.m02 +. m1.m21*.m2.m12 +. m1.m22*.m2.m22 +. m1.m23*.m2.m32;
      m23 = m1.m20*.m2.m03 +. m1.m21*.m2.m13 +. m1.m22*.m2.m23 +. m1.m23*.m2.m33;
      m30 = m1.m30*.m2.m00 +. m1.m31*.m2.m10 +. m1.m32*.m2.m20 +. m1.m33*.m2.m30;
      m31 = m1.m30*.m2.m01 +. m1.m31*.m2.m11 +. m1.m32*.m2.m21 +. m1.m33*.m2.m31;
      m32 = m1.m30*.m2.m02 +. m1.m31*.m2.m12 +. m1.m32*.m2.m22 +. m1.m33*.m2.m32;
      m33 = m1.m30*.m2.m03 +. m1.m31*.m2.m13 +. m1.m32*.m2.m23 +. m1.m33*.m2.m33
    }

  let v_mult m v = 
    {
      Vector.x = m.m00 *. v.Vector.x +. m.m01 *. v.Vector.y 
	+. m.m02 *. v.Vector.z +. m.m03 *. v.Vector.w;
      Vector.y = m.m10 *. v.Vector.x +. m.m11 *. v.Vector.y 
	+. m.m12 *. v.Vector.z +. m.m13 *. v.Vector.w;
      Vector.z = m.m20 *. v.Vector.x +. m.m21 *. v.Vector.y 
	+. m.m22 *. v.Vector.z +. m.m23 *. v.Vector.w;
      Vector.w = m.m30 *. v.Vector.x +. m.m31 *. v.Vector.y 
	+. m.m32 *. v.Vector.z +. m.m33 *. v.Vector.w;
    }
      
  let v_mult_t v m = 
    {
      Vector.x = m.m00 *. v.Vector.x +. m.m10 *. v.Vector.y 
	+. m.m20 *. v.Vector.z +. m.m30 *. v.Vector.w;
      Vector.y = m.m01 *. v.Vector.x +. m.m11 *. v.Vector.y 
	+. m.m21 *. v.Vector.z +. m.m31 *. v.Vector.w;
      Vector.z = m.m02 *. v.Vector.x +. m.m12 *. v.Vector.y 
	+. m.m22 *. v.Vector.z +. m.m32 *. v.Vector.w;
      Vector.w = m.m03 *. v.Vector.x +. m.m13 *. v.Vector.y 
	+. m.m23 *. v.Vector.z +. m.m33 *. v.Vector.w;
    }

  let equal m1 m2 =
    equal m1.m00 m2.m00
    && equal m1.m01 m2.m01
    && equal m1.m02 m2.m02
    && equal m1.m03 m2.m03
    && equal m1.m10 m2.m10
    && equal m1.m11 m2.m11
    && equal m1.m12 m2.m12
    && equal m1.m13 m2.m13
    && equal m1.m20 m2.m20
    && equal m1.m21 m2.m21
    && equal m1.m22 m2.m22
    && equal m1.m23 m2.m23
    && equal m1.m30 m2.m30
    && equal m1.m31 m2.m31
    && equal m1.m32 m2.m32
    && equal m1.m33 m2.m33

(* The transformation matrices *)
  let translate v =
    {
      m00 = 1.;
      m01 = 0.;
      m02 = 0.;
      m03 = v.Vector.x;
      m10 = 0.;
      m11 = 1.;
      m12 = 0.;
      m13 = v.Vector.y;
      m20 = 0.;
      m21 = 0.;
      m22 = 1.;
      m23 = v.Vector.z;
      m30 = 0.;
      m31 = 0.;
      m32 = 0.;
      m33 = 1.
    }

  let translate_t v =
	transpose (translate v)

  let scale v =
    {
      m00 = v.Vector.x;
      m01 = 0.;
      m02 = 0.;
      m03 = 0.;
      m10 = 0.;
      m11 = v.Vector.y;
      m12 = 0.;
      m13 = 0.;
      m20 = 0.;
      m21 = 0.;
      m22 = v.Vector.z;
      m23 = 0.;
      m30 = 0.;
      m31 = 0.;
      m32 = 0.;
      m33 = 1.
    }

  let scale_t = scale

  let rotate_x alpha =
    {
      m00 = 1.;
      m01 = 0.;
      m02 = 0.;
      m03 = 0.;
      m10 = 0.;
      m11 = cos alpha;
      m12 = -. sin alpha;
      m13 = 0.;
      m20 = 0.;
      m21 = sin alpha;
      m22 = cos alpha;
      m23 = 0.;
      m30 = 0.;
      m31 = 0.;
      m32 = 0.;
      m33 = 1.
    }

  let rotate_x_t alpha =
	transpose (rotate_x alpha)
      
  let rotate_y beta =
    {
      m00 = cos beta;
      m01 = 0.;
      m02 = sin beta;
      m03 = 0.;
      m10 = 0.;
      m11 = 1.;
      m12 = 0.;
      m13 = 0.;
      m20 = -. sin beta;
      m21 = 0.;
      m22 = cos beta;
      m23 = 0.;
      m30 = 0.;
      m31 = 0.;
      m32 = 0.;
      m33 = 1.
    }

  let rotate_y_t beta =
	transpose (rotate_y beta)

  let rotate_z gamma =
    {
      m00 = cos gamma;
      m01 = -. sin gamma;
      m02 = 0.;
      m03 = 0.;
      m10 = sin gamma;
      m11 = cos gamma;
      m12 = 0.;
      m13 = 0.;
      m20 = 0.;
      m21 = 0.;
      m22 = 1.;
      m23 = 0.;
      m30 = 0.;
      m31 = 0.;
      m32 = 0.;
      m33 = 1.
    }

  let rotate_z_t gamma =
	transpose (rotate_z gamma)

  let rotate v theta = 
	let v = Vector.normalize v in
	let x = Vector.get_x v
	and y = Vector.get_y v
	and z = Vector.get_z v
	and cost = cos theta
	and sint = sin theta 
	and icost = 1. -. (cos theta) in
	  {
		m00 = cost +. icost *. x *. x;
		m01 = icost *. x *. y -. sint *. z;
		m02 = icost *. x *. y +. sint *. y;
		m03 = 0.;
		m10 = icost *. y *. x +. sint *. z;
		m11 = cost +. icost *. y *. y;
		m12 = icost *. y *. z -. sint *. x;
		m13 = 0.;
		m20 = icost *. z *. x -. sint *. y;
		m21 = icost *. z *. y +. sint *. x;
		m22 = cost +. icost *. z *. z;
		m23 = 0.;
		m30 = 0.;
		m31 = 0.;
		m32 = 0.;
		m33 = 1.
	  }

  let rotate_t v theta = 
	transpose (rotate v theta)

  let rotate_vec v1 v2 = 
	let normal_float f = 
	  classify_float f = FP_normal || 
	  classify_float f = FP_subnormal ||
	  classify_float f = FP_zero
	in
	let v1 = Vector.normalize v1
	and v2 = Vector.normalize v2 in
	let normal =
	  let n = Vector.normalize (Vector.cross v1 v2) in
		if 
		  normal_float (Vector.get_x n)
		  && normal_float (Vector.get_y n)
		  && normal_float (Vector.get_z n) then
			n
		else
		  Vector.x
	in	
	let angle = acos (Vector.dot v1 v2) in
	  rotate normal angle

  let projection fovy aspect znear zfar =
	let f = 1. /. (tan (fovy /. 2.)) in
	  {
		m00 = f /. aspect;
		m01 = 0.;
		m02 = 0.;
		m03 = 0.;
		m10 = 0.;
		m11 = f;
		m12 = 0.;
		m13 = 0.;
		m20 = 0.;
		m21 = 0.;
		m22 = (zfar +. znear) /. (znear -. zfar);
		m23 = 2. *. zfar *. znear /. (znear -. zfar);
		m30 = 0.;
		m31 = 0.;
		m32 = -. 1.;
		m33 = 0.
	  }
		
  let projection_t fovy aspect znear zfar =
	transpose (projection fovy aspect znear zfar)
	  
  let to_string m = 
	let f = string_of_float in
	  (f m.m00) ^ "    " ^ (f m.m01) ^ "    " ^ (f m.m02) ^ "    " ^ (f m.m03) ^ "\n"
	  ^ (f m.m10) ^ "    " ^ (f m.m11) ^ "    " ^ (f m.m12) ^ "    " ^ (f m.m13) ^ "\n"
	  ^ (f m.m20) ^ "    " ^ (f m.m21) ^ "    " ^ (f m.m22) ^ "    " ^ (f m.m23) ^ "\n"
	  ^ (f m.m30) ^ "    " ^ (f m.m31) ^ "    " ^ (f m.m32) ^ "    " ^ (f m.m33)
		
		
end


module NVector = 
struct
  type t = 
      {
	x : int;
	y : int;
	z : int
      }
	
  (* Creation, getters *)
  let make x y z = 
    {
      x = x;
      y = y;
      z = z
    }
      
  let get_x v = v.x
    
  let get_y v = v.y
    
  let get_z v = v.z
    
  let zero =
  {
    x = 0;
    y = 0;
    z = 0
  }
    
  let x =
    {
      x = 1;
      y = 0;
      z = 0
    }
      
  let y =
    {
      x = 0;
      y = 1;
      z = 0
    }

  let z =
    {
      x = 0;
      y = 0;
      z = 1
    }
      
  (* Operations *)
      
  let add v1 v2 =
    {
      x = v1.x + v2.x;
      y = v1.y + v2.y;
      z = v1.z + v2.z
    }
      
  let sub v1 v2 =
    {
      x = v1.x - v2.x;
      y = v1.y - v2.y;
      z = v1.z - v2.z
    }
      
  let dot v1 v2 =
    v1.x * v2.x
    + v1.y * v2.y
    + v1.z * v2.z
      
  let cross v1 v2 =
    {
      x = v1.y * v2.z - v1.z * v2.y;
      y = - v1.x * v2.z + v1.z * v2.x;
      z = v1.x * v2.y - v1.y * v2.x
    }
      
  let mult v r =
    {
      x = v.x * r;
      y = v.y * r;
      z = v.z * r
    }

  let v_mult v1 v2 = 
    {
      x = v1.x * v2.x;
      y = v1.y * v2.y;
      z = v1.z * v2.z
    }

  let equal v1 v2 = 
    v1.x = v2.x
    && v1.y = v2.y
    && v1.z = v2.z

  let to_string v =
	"(" ^ (string_of_int v.x) 
	^ ", " ^ (string_of_int v.y) 
	^ ", " ^ (string_of_int v.z)
	^ ")"

  let sqnorm v = 
    dot v v
     
end
