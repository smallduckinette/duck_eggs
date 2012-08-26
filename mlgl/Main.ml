open Math.Vector.Ops

let sphere = ref
  {
	Collision.Sphere.c = Math.Vector.make 0. 0. (-.30.);
	Collision.Sphere.r = 1.
  }

let v = ref Math.Vector.zero

let _ =
  Sdl.init [`VIDEO];
  at_exit Sdl.quit;
  Sdlwm.set_caption 
	~title:"AdH"
	~icon:"AdH";
  Sdlgl.set_attr [Sdlgl.DOUBLEBUFFER(true)];
  ignore(Sdlvideo.set_video_mode 
		   ~w:1024
		   ~h:768
		   ~bpp:32 
		   [`DOUBLEBUF ; `HWSURFACE ; `OPENGL]);
  let render = ref (Renderer.create 1024 768) in
	Renderer.init !render;
	
	let comp = 
	  MlGl.compile
		[MlGl.texture
		   (MlGlTexture.create "mlgl/earth_day.jpg")
		   [MlGl.textured_uvsphere 32 32]] in
	let cube = 
	  MlGl.compile
		[MlGl.translate (Math.Vector.make 0. (-.5.) (-.30.)) [MlGl.cube 1.]]
	in
	  
	let create_scene r f = 
	  (if f then MlGl.group else MlGl.wireframe)
		[
		  MlGl.translate (!sphere.Collision.Sphere.c)
			[
			  MlGl.rotate r (Math.Vector.y) [comp]
			];
		  cube
		]
	in
	let frame_ticks = 40 in
	let lastticks = ref (Sdltimer.get_ticks ()) in
	let current_angle = ref 3.0 in
	let view_angle = ref 0. in
	let filled = ref true in
	  begin try while true do
		(* Slow down the loop to 100 frames/second *)
		let ticks = Sdltimer.get_ticks () in
		  if ticks - !lastticks < frame_ticks
		  then Sdltimer.delay (frame_ticks - (ticks - !lastticks));
		  lastticks := ticks ;
		  current_angle := !current_angle +. 0.005;
		  while
			(* Check for events *)
			match Sdlevent.poll () with
			  | None -> false
			  | Some evt ->
				  match evt with
					| Sdlevent.QUIT -> raise Exit
					| Sdlevent.KEYDOWN k ->
						begin
						  match k.Sdlevent.keysym with
							  Sdlkey.KEY_LEFT -> 
								view_angle := !view_angle -. 0.05;
								render := Renderer.update_view !render !view_angle
							| Sdlkey.KEY_RIGHT ->
								view_angle := !view_angle +. 0.05;
								render := Renderer.update_view !render !view_angle
							| Sdlkey.KEY_UP -> 
								(
								  filled := true; 
								  v := Math.Vector.make 0. (-.0.01) 0.
								)
							| Sdlkey.KEY_DOWN -> filled := false
							| Sdlkey.KEY_ESCAPE -> raise Exit
							| _ -> ()
						end;
						true
					| _ -> true
		  do ()
		  done;
(*		  let r_v = Collision.collision_planes_sphere [plane] !sphere !v in
			sphere :=
			  {
				!sphere with
				  Collision.Sphere.c = !sphere.Collision.Sphere.c +| r_v
			  };*)
			Renderer.render !render (create_scene !current_angle !filled);
			
	  done
	  with Exit -> () 
	  end
		
