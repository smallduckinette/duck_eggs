
class cube side =
object
  inherit(MlGlG.node)
  
  val bounding_volume = 
	Bound.box
	  (Math.Vector.make (-. side) (-. side) (-. side))
	  (Math.Vector.make side side side)
	  

  method render () =
	GlDraw.begins `quads;
	GlDraw.normal ~x:(0.)      ~y:(0.)      ~z:(1.)    ();
	GlDraw.vertex ~x:(-. side) ~y:(-. side) ~z:(side)  ();
	GlDraw.vertex ~x:(side)    ~y:(-. side) ~z:(side)  ();
	GlDraw.vertex ~x:(side)    ~y:(side)    ~z:(side)  ();
	GlDraw.vertex ~x:(-. side) ~y:(side)    ~z:(side)  ();
	
	GlDraw.normal ~x:(0.)      ~y:(0.)      ~z:(-.1.)  ();
	GlDraw.vertex ~x:(-. side) ~y:(-. side) ~z:(-.side)();
	GlDraw.vertex ~x:(-. side) ~y:(side)    ~z:(-.side)();
	GlDraw.vertex ~x:(side)    ~y:(side)    ~z:(-.side)();
	GlDraw.vertex ~x:(side)    ~y:(-. side) ~z:(-.side)();
	
	GlDraw.normal ~z:(0.)      ~x:(0.)      ~y:(1.)    ();
	GlDraw.vertex ~z:(-. side) ~x:(-. side) ~y:(side)  ();
	GlDraw.vertex ~z:(side)    ~x:(-. side) ~y:(side)  ();
	GlDraw.vertex ~z:(side)    ~x:(side)    ~y:(side)  ();
	GlDraw.vertex ~z:(-. side) ~x:(side)    ~y:(side)  ();
	
	GlDraw.normal ~z:(0.)      ~x:(0.)      ~y:(-.1.)  ();
	GlDraw.vertex ~z:(-. side) ~x:(-. side) ~y:(-.side)();
	GlDraw.vertex ~z:(-. side) ~x:(side)    ~y:(-.side)();
	GlDraw.vertex ~z:(side)    ~x:(side)    ~y:(-.side)();
	GlDraw.vertex ~z:(side)    ~x:(-. side) ~y:(-.side)();
	
	GlDraw.normal ~y:(0.)      ~z:(0.)      ~x:(1.)    ();
	GlDraw.vertex ~y:(-. side) ~z:(-. side) ~x:(side)  ();
	GlDraw.vertex ~y:(side)    ~z:(-. side) ~x:(side)  ();
	GlDraw.vertex ~y:(side)    ~z:(side)    ~x:(side)  ();
	GlDraw.vertex ~y:(-. side) ~z:(side)    ~x:(side)  ();
	
	GlDraw.normal ~y:(0.)      ~z:(0.)      ~x:(-.1.)  ();
	GlDraw.vertex ~y:(-. side) ~z:(-. side) ~x:(-.side)();
	GlDraw.vertex ~y:(-. side) ~z:(side)    ~x:(-.side)();
	GlDraw.vertex ~y:(side)    ~z:(side)    ~x:(-.side)();
	GlDraw.vertex ~y:(side)    ~z:(-. side) ~x:(-.side)();
	
	GlDraw.ends ()
	  
  method get_bounding_volume = 
	bounding_volume
	  
end

type side = Outside | Inside

type tex = Textured | Not_Textured

class cylinder tex side steps =
object
  inherit(MlGlG.node)

  val bounding_volume = 
	Bound.box
	  (Math.Vector.make (-. 1.) (-. 1.) (-. 1.))
	  (Math.Vector.make 1. 1. 1.)

  method render () =
	let vec s z = 
	  let a = 2. *. Math.pi *. (float_of_int s) /. (float_of_int steps) in
		Math.Vector.make
		  (cos a)
		  (sin a)
		  z
	in
	let draw_normal v =
	  GlDraw.normal 
		~x:(Math.Vector.get_x v)
		~y:(Math.Vector.get_y v)
		~z:(Math.Vector.get_z v)
		();
	in
	let draw_texture tu = 
	  match tex with
		  Textured -> GlTex.coord2 tu
		| Not_Textured -> ()
	in
	let draw_vertex v =
	  GlDraw.vertex
		~x:(Math.Vector.get_x v)
		~y:(Math.Vector.get_y v)
		~z:(Math.Vector.get_z v)
		();	  
	in
	  GlDraw.begins `quads;
	  for i = 0 to steps - 1 do
		let n1 = vec i 0.
		and v1 = vec i (1.)
		and t1 = ((float_of_int i) /. (float_of_int steps), 1.)
		and v2 = vec i (-.1.)
		and t2 = ((float_of_int i) /. (float_of_int steps), 0.)
		and n2 = vec (i + 1) 0.
		and v3 = vec (i + 1) (-.1.)
		and t3 = ((float_of_int (i + 1)) /. (float_of_int steps), 0.)
		and v4 = vec (i + 1) 1.
		and t4 = ((float_of_int (i + 1)) /. (float_of_int steps), 1.)
		in
		  match side with
			  Outside -> 
				begin
				  draw_normal n1;
				  draw_texture t1;
				  draw_vertex v1;
				  draw_texture t2;
				  draw_vertex v2;
				  draw_normal n2;
				  draw_texture t3;
				  draw_vertex v3;
				  draw_texture t4;
				  draw_vertex v4		  
				end
			| Inside -> 
				begin
				  draw_normal (Math.Vector.sub Math.Vector.zero n2);
				  draw_texture t4;
				  draw_vertex v4;
				  draw_texture t3;
				  draw_vertex v3;
				  draw_normal (Math.Vector.sub Math.Vector.zero n1);
				  draw_texture t2;
				  draw_vertex v2;
				  draw_texture t1;
				  draw_vertex v1
				end;
	  done;
	  GlDraw.ends ()
		
  method get_bounding_volume = 
	bounding_volume
end
  
class torus r1 r2 s1 s2 =
object
  inherit(MlGlG.node)

  val bounding_volume = 
	Bound.box
	  (Math.Vector.make (-. (r1 +. r2)) (-. (r1 +. r2)) (-. r2))
	  (Math.Vector.make (r1 +. r2) (r1 +. r2) r2)
	  

  method render () = 
	let vec alpha beta = 
	  let v = Math.Vector.make r2 0. 0.
	  and n = Math.Vector.make 1. 0. 0.
	  and rot1 = Math.Matrix.rotate_z alpha 
	  and t = Math.Matrix.translate (Math.Vector.make r1 0. 0.)
	  and rot2 = Math.Matrix.rotate_y beta
	  in
	  let trans = Math.Matrix.m_mult rot1 (Math.Matrix.m_mult t rot2)
	  and ntrans = Math.Matrix.m_mult rot1 rot2 in
	  let v = Math.Matrix.v_mult trans v
	  and n = Math.Matrix.v_mult ntrans n in
	  GlDraw.normal 
		~x:(Math.Vector.get_x n)
		~y:(Math.Vector.get_y n)
		~z:(Math.Vector.get_z n)
		();
	  GlDraw.vertex
		~x:(Math.Vector.get_x v)
		~y:(Math.Vector.get_y v)
		~z:(Math.Vector.get_z v)
		()  
	in
	let do_vec a b =
	  let alpha = 2. *. Math.pi *. (float_of_int a) /. (float_of_int s1)
	  and beta = 2. *. Math.pi *. (float_of_int b) /. (float_of_int s2) in
		vec alpha beta
	in
	  GlDraw.begins `quads;
	  for a = 0 to s1 - 1 do
		for b = 0 to s2 - 1 do
		  do_vec a b;
		  do_vec a (b + 1);
		  do_vec (a + 1) (b + 1);
		  do_vec (a + 1) b
		done;
	  done;
	  GlDraw.ends ()

  method get_bounding_volume = 
	bounding_volume
end

class uvsphere tex s1 s2 = 
object
  inherit(MlGlG.node)

  val bounding_volume = 
	Bound.box
	  (Math.Vector.make (-. 1.) (-. 1.) (-. 1.))
	  (Math.Vector.make 1. 1. 1.)
	

  method render () = 
	let do_tex i j = 
	  match tex with
		  Textured -> 
			let u = (float_of_int i)/.(float_of_int s1)
			and v = (float_of_int j)/.(float_of_int s2) in
			  GlTex.coord2 (u, v)
		| Not_Textured -> ()
	in
	let vec delta gamma = 
	  let one = Math.Vector.make 1. 0. 0.
	  and rot_delta = Math.Matrix.rotate_y delta
	  and rot_gamma = Math.Matrix.rotate_z gamma in
	  let trans = 
		Math.Matrix.m_mult rot_delta rot_gamma
	  in
	  let v = Math.Matrix.v_mult trans one
	  in
		GlDraw.normal 
		  ~x:(Math.Vector.get_x v)
		  ~y:(Math.Vector.get_y v)
		  ~z:(Math.Vector.get_z v)
		  ();
		GlDraw.vertex
		  ~x:(Math.Vector.get_x v)
		  ~y:(Math.Vector.get_y v)
		  ~z:(Math.Vector.get_z v)
		  ()  
	in
	let do_vec i j =
	  let delta = (float_of_int i) *. 2. *. Math.pi /. (float_of_int s1)
	  and gamma = -. Math.pi /. 2. +. Math.pi *. (float_of_int j) /. (float_of_int s2)
	  in
		do_tex i j;
		vec delta gamma
	in
	  GlDraw.begins `quads;
	  for i = 0 to s1 - 1 do
		for j = 0 to s2 - 1 do
		  do_vec i j;
		  do_vec (i + 1) j;
		  do_vec (i + 1) (j + 1);
		  do_vec i (j + 1)
		done
	  done;
	  GlDraw.ends ()

  method get_bounding_volume = 
	bounding_volume
end

class textured_square =
object
  inherit(MlGlG.node)
	
  val bounding_volume = 
	Bound.box
	  (Math.Vector.make (-. 1.) (-. 1.) (0.))
	  (Math.Vector.make 1. 1. 0.)
	
  method render () = 
	GlDraw.begins `quads;
	GlDraw.normal ~x:0. ~y:0. ~z:1. ();
	GlTex.coord2 (0., 0.);
	GlDraw.vertex ~x:(-. 1.) ~y:(-. 1.) ~z:(0.)  ();
	GlTex.coord2 (1., 0.);
	GlDraw.vertex ~x:(1.)    ~y:(-. 1.) ~z:(0.)  ();
	GlTex.coord2 (1., 1.);
	GlDraw.vertex ~x:(1.)    ~y:(1.)    ~z:(0.)  ();
	GlTex.coord2 (0., 1.);
	GlDraw.vertex ~x:(-. 1.) ~y:(1.)    ~z:(0.)  ();
	GlDraw.ends ()
	  
  method get_bounding_volume = 
	bounding_volume
end

class grid step f = 
object
  inherit(MlGlG.node)

  val bounding_volume = 
	Bound.box
	  (Math.Vector.make 0. (-.1.) 0.)
	  (Math.Vector.make 1. 1. 1.)

  method render () = 
	let make_array size =
	  Bigarray.Array3.create
		Bigarray.float32
		Bigarray.c_layout
		size
		size
		7
	in
	let fill_array array i j = 
	  let x = (float_of_int i) /. (float_of_int step)
	  and z = (float_of_int j) /. (float_of_int step) in
	  let (height, normal, color) = f x z in
		Bigarray.Array3.set array i j 0 height;
		Bigarray.Array3.set array i j 1 (Math.Vector.get_x normal);
		Bigarray.Array3.set array i j 2 (Math.Vector.get_y normal);
		Bigarray.Array3.set array i j 3 (Math.Vector.get_z normal);
		Bigarray.Array3.set array i j 4 (Math.Vector.get_x color);
		Bigarray.Array3.set array i j 5 (Math.Vector.get_y color);
		Bigarray.Array3.set array i j 6 (Math.Vector.get_z color)
	in
	let make_vec array i j = 
	  let x = (float_of_int i) /. (float_of_int step)
	  and z = (float_of_int j) /. (float_of_int step) in
		GlDraw.normal 
		  ~x:(Bigarray.Array3.get array i j 1)
		  ~y:(Bigarray.Array3.get array i j 2) 
		  ~z:(Bigarray.Array3.get array i j 3) 
		  ();
		(*GlLight.material ~face:`front 
		  (`diffuse 
			 (Bigarray.Array3.get array i j 4,
			  Bigarray.Array3.get array i j 5,
			  Bigarray.Array3.get array i j 6,
			  0.));*)
		GlDraw.vertex 
		  ~x 
		  ~y:(Bigarray.Array3.get array i j 0)
		  ~z ()
	in
	let array = make_array (step + 1) in
	  for i = 0 to step do
		for j = 0 to step do
		  fill_array array i j
		done;
	  done;
	  GlDraw.begins `quads;
	  for i = 0 to step - 1 do
		for j = 0 to step - 1 do
		  make_vec array i j;
		  make_vec array i (j+1);
		  make_vec array (i+1) (j+1);
		  make_vec array (i+1) j;
		done;
	  done;
	  GlDraw.ends ()
		
  method get_bounding_volume = 
	bounding_volume
end

class normal_grid step f n = 
object
  inherit(MlGlG.node)

  val bounding_volume = 
	Bound.box
	  (Math.Vector.make 0. 0. 0.)
	  (Math.Vector.make 1. 0. 1.)

  method render () = 
	let make_line i j = 
	  let x = (float_of_int i) /. (float_of_int step)
	  and z = (float_of_int j) /. (float_of_int step) in
	  let normal = Math.Vector.mult (Math.Vector.normalize (n x z)) 0.01
	  and start = Math.Vector.make x (f x z) z in
	  let finish = Math.Vector.add start normal in
		GlDraw.normal 
		  ~x:(Math.Vector.get_x normal)
		  ~y:(Math.Vector.get_y normal) 
		  ~z:(Math.Vector.get_z normal) 
		  ();
		GlDraw.vertex 
		  ~x:(Math.Vector.get_x start)
		  ~y:(Math.Vector.get_y start)
		  ~z:(Math.Vector.get_z start)
		  ();
		GlDraw.vertex 
		  ~x:(Math.Vector.get_x finish)
		  ~y:(Math.Vector.get_y finish)
		  ~z:(Math.Vector.get_z finish)
		  ()
	in
	GlDraw.begins `lines;
	for i = 0 to step - 1 do
	  for j = 0 to step - 1 do
		make_line i j
	  done;
	done;
	GlDraw.ends ()

  method get_bounding_volume = 
	bounding_volume
end

class auto_grid step f = 
object
  inherit(MlGlG.node)

  val bounding_volume = 
	Bound.box
	  (Math.Vector.make 0. 0. 0.)
	  (Math.Vector.make 1. 0. 1.)

  method render () = 
	let make_array size =
	  Bigarray.Array2.create
		Bigarray.float32
		Bigarray.c_layout
		size
		size
	in
	let fill_array array i j = 
	  let x = (float_of_int i) /. (float_of_int step)
	  and z = (float_of_int j) /. (float_of_int step) in
		Bigarray.Array2.set array i j (f x z)
	in
	let make_face array i j =
	  let get_vector i j = 
		let x = (float_of_int i) /. (float_of_int step)
		and z = (float_of_int j) /. (float_of_int step) in
		  Math.Vector.make
			x
			(Bigarray.Array2.get array i j)
			z
	  in
	  let make_normal v = 
		GlDraw.normal 
		  ~x:(Math.Vector.get_x v)
		  ~y:(Math.Vector.get_y v) 
		  ~z:(Math.Vector.get_z v) 
		  ()		
	  in
	  let make_vertex v =
		GlDraw.vertex
		~x:(Math.Vector.get_x v)
		~y:(Math.Vector.get_y v) 
		~z:(Math.Vector.get_z v) 
		()		
	  in
	  let vec_0 = get_vector i j
	  and vec_x = get_vector (i + 1) j
	  and vec_y = get_vector i (j + 1)
	  and vec_xy = get_vector (i + 1) (j + 1) in
	  let normal =
		Math.Vector.normalize(
		  Math.Vector.cross 
			(Math.Vector.sub vec_y vec_0)
			(Math.Vector.sub vec_x vec_0))
	  in
		make_normal normal;
		make_vertex vec_0;
		make_vertex vec_y;
		make_vertex vec_xy;
		make_vertex vec_x
	in
	let array = make_array (step + 1) in
	  for i = 0 to step do
		for j = 0 to step do
		  fill_array array i j;
		done;
	  done;
	  GlDraw.begins `quads;
	  for i = 0 to step - 1 do
		for j = 0 to step - 1 do
		  make_face array i j
		done;
	  done;
	  GlDraw.ends ()
		
  method get_bounding_volume = 
	bounding_volume
end

class simple_grid step = 
object
  inherit(MlGlG.node)

  val bounding_volume = 
	Bound.box
	  (Math.Vector.make 0. 0. 0.)
	  (Math.Vector.make 1. 0. 1.)

  method render () = 
	let make_square i j = 
	  let make_vector i j = 
		let ri = 
		  if j = 0 && i mod 2 = 1 then i - 1
		  else if j = step && i mod 2 = 1 then i + 1
		  else i
		and rj =
		  if i = 0 && j mod 2 = 1 then j - 1
		  (*else if i = step && j mod 2 = 1 then j + 1*)
		  else j in
		Math.Vector.make
		  ((float_of_int ri) /. (float_of_int step))
		  ((float_of_int rj) /. (float_of_int step))
		  0.
	  in
	  let v1 = make_vector i j
	  and v2 = make_vector i (j + 1)
	  and v3 = make_vector (i + 1) (j + 1)
	  and v4 = make_vector (i + 1) j in
		GlDraw.vertex 
		  ~x:(Math.Vector.get_x v1)
		  ~y:(Math.Vector.get_y v1)
		  ~z:(Math.Vector.get_z v1)
		  ();
		GlDraw.vertex 
		  ~x:(Math.Vector.get_x v2)
		  ~y:(Math.Vector.get_y v2)
		  ~z:(Math.Vector.get_z v2)
		  ();
		GlDraw.vertex 
		  ~x:(Math.Vector.get_x v3)
		  ~y:(Math.Vector.get_y v3)
		  ~z:(Math.Vector.get_z v3)
		  ();
		GlDraw.vertex 
		  ~x:(Math.Vector.get_x v4)
		  ~y:(Math.Vector.get_y v4)
		  ~z:(Math.Vector.get_z v4)
		  ()
	in
	  GlDraw.begins `quads;
	  GlDraw.normal 
		~x:(0.)
		~y:(1.) 
		~z:(0.) 
		  ();	
	  for i = 0 to step - 1 do
		for j = 0 to step - 1 do
		  make_square i j
		done;
	  done;
	  GlDraw.ends ()
		
  method get_bounding_volume = 
	bounding_volume
end
  
