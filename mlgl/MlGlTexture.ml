

type t = GlTex.texture_id

let load_gl_data file = 
  let surface = Sdlloader.load_image file in
  let surface_data = Sdlvideo.surface_info surface in
  let w = surface_data.Sdlvideo.w
  and h = surface_data.Sdlvideo.h in
  let raw = Sdlgl.to_raw(surface) in
	GlPix.of_raw raw ~format:`rgb ~width:w ~height:h	  
	  
let create file = 
  let texture_id = GlTex.gen_texture () in
	GlTex.bind_texture `texture_2d texture_id;
	GlTex.parameter `texture_2d (`wrap_s `repeat);
	GlTex.parameter `texture_2d (`wrap_t `repeat);
	GlTex.parameter `texture_2d (`min_filter `linear);
	GlTex.parameter `texture_2d (`mag_filter `linear);
	GlTex.env (`mode `modulate);
	GlTex.image2d (load_gl_data file);
	texture_id
	  
let select t =
  GlTex.bind_texture `texture_2d t
  
