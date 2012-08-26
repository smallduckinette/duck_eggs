
let w t = t *. t *. t *. (t *. (6. *. t -. 15.) +. 10.)

let w_d t = t *. t *. ( t *. (30. *. t -. 60.) +. 30.)

let smooth w a b x =
  a *. (1. -. (w x)) +. b *. (w x)

let smooth_d w_d a b x = 
  -. a *. (w_d x) +. b *. (w_d x)

let noise_1d n = 
  let n = (n lsl 13) lor n in
	1. -. (
	  (float_of_int 
		 ((n * (n * n * 15731 + 789221) + 376312589) land 0x3fffffff))
	  /. 
		536870912.)
	  
let noise_2d x y =
  noise_1d (x + 57 * y)

let interpolated_noise x y = 
  let x = x
  and y = y in
  let imodf f = 
	let frac, integral = modf f in
	  frac, int_of_float integral
  in
  let make_vector x y =
	let angle = (noise_2d x y) *. Math.pi in
	  Math.Vector.make
		(cos angle)
		(sin angle)
		0.
  in
  let s = smooth w
  and s_d = smooth_d w_d in
  let (f_x, i_x) = imodf x
  and (f_y, i_y) = imodf y in
  let p = Math.Vector.make f_x f_y 0. in
  let v_0 = make_vector i_x i_y
  and v_x = make_vector (i_x + 1) i_y
  and v_y = make_vector i_x (i_y + 1)
  and v_xy = make_vector (i_x + 1) (i_y + 1) in
  let p_0 = Math.Vector.sub p (Math.Vector.make 0. 0. 0.)
  and p_x = Math.Vector.sub p (Math.Vector.make 1. 0. 0.)
  and p_y = Math.Vector.sub p (Math.Vector.make 0. 1. 0.)
  and p_xy = Math.Vector.sub p (Math.Vector.make 1. 1. 0.)
  in
  let e_0 = (Math.Vector.dot p_0 v_0) /. 10.
  and e_x = (Math.Vector.dot p_x v_x) /. 10.
  and e_y = (Math.Vector.dot p_y v_y) /. 10.
  and e_xy = (Math.Vector.dot p_xy v_xy) /. 10. in
  let v1 = s e_0 e_x f_x
  and v2 = s e_y e_xy f_x
  and v1_d = s_d e_0 e_x f_x
  and v2_d = s_d e_y e_xy f_x
  in
	(s v1 v2 f_y,
	 Math.Vector.make
	   (-. (s v1_d v2_d f_y))
	   (1.)
	   (-. (s_d v1 v2 f_y)))
