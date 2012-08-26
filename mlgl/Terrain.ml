
let make_terrain octaves x y = 
  List.fold_left
	(fun (height, normal) (octave, scale) -> 
	   let (h, n) = Noise.interpolated_noise (x *. octave) (y *. octave) in
		 (height +. (h *. scale),
		  Math.Vector.v_mult
			n
			(Math.Vector.make (scale *. octave) 1. (scale *. octave))))
	(0., Math.Vector.zero)
	octaves

let terrain x y = 
  let (h, n) = make_terrain [(5., 1.);(10., 0.50);(20., 0.25); (40., 0.1)] x y
  in
	if h > 0.01 then
	  (h, n, Math.Vector.make 1. 1. 1.)
	else if h < -.0.04 then
	  (h, n, Math.Vector.make 0. 0.2 0.)
	else
	  (h, n, Math.Vector.make 0. 0.5 0.)
