type t = Math.Vector.t * float

let contains (c, r) p =
  Math.Vector.sqnorm (Math.Vector.sub c p) <= r *. r

let contains_sphere (c1, r1) (c2, r2) = 
  (Math.Vector.norm (Math.Vector.sub c1 c2)) +. r2 <= r1

let intersect (c1, r1) (c2, r2) =
  Math.Vector.sqnorm (Math.Vector.sub c1 c2) <= (r1 +. r2) *. (r1 +. r2)

let merge l = 
  let total_weights = List.fold_left
	(fun acc (_, r) -> acc +. r)
	0.
	l
  in
  let vectors = List.map
	(fun (c, r) -> Math.Vector.mult c (r /. total_weights))
	l
  in
  let center = List.fold_left
	Math.Vector.add
	Math.Vector.zero
	vectors
  in
  let distances = 
	List.map
	  (fun (c, r) -> (Math.Vector.norm (Math.Vector.sub c center)) +. r)
	  l
  in
  let radius = 
	List.fold_left
	  max
	  0.
	  distances
  in
	(center, radius)
