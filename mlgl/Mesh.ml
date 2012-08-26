type t =
	{
	  vectors : Math.Vector.t list;
	  normals : Math.Vector.t list;
	  textures : Math.Vector.t list;
	  polys : (int * int option * int option) list list
	}

let empty =
  {
	vectors = [];
	normals = [];
	textures = [];
	polys = []
  }

let add_vector v e = 
  {
	vectors = v::e.vectors;
	normals = e.normals;
	textures = e.textures;
	polys = e.polys
  }

let add_normal n e =  
  {
	vectors = e.vectors;
	normals = n::e.normals;
	textures = e.textures;
	polys = e.polys
  }
	
let add_texture t e =
  {
	vectors = e.vectors;
	normals = e.normals;
	textures = t::e.textures;
	polys = e.polys
  }
  
let add_polygon p e =
  {
	vectors = e.vectors;
	normals = e.normals;
	textures = e.textures;
	polys = p::e.polys
  }

let statistics e =
  List.length e.vectors,
  List.length e.normals,
  List.length e.textures,
  List.length e.polys

type point = Math.Vector.t * Math.Vector.t option * Math.Vector.t option

type polys = 
	Line of point * point
  | Triangle of point * point * point
  | Quad of point * point * point * point
  | Poly of point list

let explode e = 
  let vectors = 
	List.fold_left
	  (fun i e -> Index.push_back i e)
	  Index.one_based
	  e.vectors
  in
  let normals =
	List.fold_left
	  (fun i e -> Index.push_back i e)
	  Index.one_based
	  e.normals
  in	
  let poly_to_point (vertex, _, o_normal) =
	(
	  Index.get vectors vertex,
	  None,
	  (match o_normal with 
		   None -> None
		 | Some normal -> Some (Index.get normals normal))
	)
  in
  let polys = List.map 
	(fun e ->
	   match e with
		   [e1; e2] -> Line (poly_to_point e1, 
							 poly_to_point e2)
		 | [e1; e2; e3] -> Triangle (poly_to_point e1, 
									 poly_to_point e2,
									 poly_to_point e3)
		 | [e1; e2; e3; e4] -> Quad (poly_to_point e1, 
									 poly_to_point e2,
									 poly_to_point e3,
									 poly_to_point e4)
		 | l -> Poly (List.map poly_to_point l))
	e.polys
  in
	List.rev polys


let display p = 
  let make_vector (vertex, _, o_normal) =
	begin
	  match o_normal with
		  Some normal ->  	
			GlDraw.normal
			  ~x:(Math.Vector.get_x normal) 
			  ~y:(Math.Vector.get_y normal) 
			  ~z:(Math.Vector.get_z normal)  ()
		| None -> ()
	end;
	GlDraw.vertex 
	  ~x:(Math.Vector.get_x vertex) 
	  ~y:(Math.Vector.get_y vertex) 
	  ~z:(Math.Vector.get_z vertex)  ()
  in
  List.iter
	(fun e -> match e with
		 Line (p1, p2) ->
		   begin
			 GlDraw.begins `lines;
			 make_vector p1;
			 make_vector p2;
			 GlDraw.ends ()
		   end
	   | Triangle (p1, p2, p3) ->
		   begin
			 GlDraw.begins `triangles;
			 make_vector p1;
			 make_vector p2;
			 make_vector p3;
			 GlDraw.ends ()
		   end
	   | Quad (p1, p2, p3, p4) ->
		   begin
			 GlDraw.begins `quads;
			 make_vector p1;
			 make_vector p2;
			 make_vector p3;
			 make_vector p4;
			 GlDraw.ends ()			 
		   end
	   | Poly l ->
		   begin
			 GlDraw.begins `polygon;
			 List.iter make_vector l;
			 GlDraw.ends ()
		   end
	  )
	(explode p)
	
let bound mesh = 
  Bound.from_raw mesh.vectors

