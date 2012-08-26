type t = 
	{
	  width : int;
	  height : int;
	  view_matrix : Math.Matrix.t;
	  proj_matrix : Math.Matrix.t;
	  frustum : Frustum.t
	}
	  
let create width height =
  let view_matrix = Math.Matrix.identity
  and proj_matrix = Math.Matrix.projection_t
	  0.7853
	  ((float_of_int width)/.(float_of_int height))
	  0.1
	  100.
  in
	{
	  width = width;
	  height = height;
	  view_matrix = view_matrix;
	  proj_matrix = proj_matrix;
	  frustum = Frustum.create view_matrix proj_matrix
	}

let init t =
  GlDraw.shade_model `smooth;
  GlClear.color ~alpha:0.0 (0.0, 0.0, 0.0);
  GlClear.depth 1.0;
  GlFunc.depth_func `lequal;
  GlMisc.hint `perspective_correction `nicest;
  GlLight.light ~num:0 (`position (-.10., 10., 10., 0.));
  List.iter Gl.enable
    [`cull_face; 
	 `depth_test; 
	 `lighting; 
	 `light0;
	 `texture_2d;
	 `normalize];
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.mult (GlMat.of_array (Math.Matrix.to_array t.proj_matrix));
  GlMat.mode `modelview;
  GlMat.load_identity ()
	
let update_view t gamma =
  let new_view = Math.Matrix.rotate_y_t gamma in
	{
	  width = t.width;
	  height = t.height;
	  view_matrix = new_view;
	  proj_matrix = t.proj_matrix;
	  frustum = Frustum.create new_view t.proj_matrix
	}

let render t node = 
  GlClear.clear [`color; `depth];
  GlMisc.push_attrib [`current];
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlMat.mult (GlMat.of_array (Math.Matrix.to_array t.view_matrix));  
  begin
	match node#cull Math.Matrix.identity t.frustum with
		None -> ()
	  | Some e -> e#render ()
  end;
  GlMisc.pop_attrib ();
  Sdlgl.swap_buffers ()
	
