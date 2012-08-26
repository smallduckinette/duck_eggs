class color v elts =
object
  inherit(MlGlG.group elts) as super

  method render () =
	GlMisc.push_attrib [`current; `lighting];
	GlLight.material ~face:`front 
	  (`ambient_and_diffuse 
		 (Math.Vector.get_x v, 
		  Math.Vector.get_y v, 
		  Math.Vector.get_z v,
		  1.0));
	super#render ();
	GlMisc.pop_attrib ()
end

class wireframe elts =
object
  inherit(MlGlG.group elts) as super

  method render () =
	GlMisc.push_attrib [`polygon];
	GlDraw.polygon_mode `both `line;
	super#render ();
	GlMisc.pop_attrib ()

end

class texture tex elts = 
object
  inherit(MlGlG.group elts) as super
	
  method render () =
	GlMisc.push_attrib [`texture];
	MlGlTexture.select tex;
	super#render ();
	GlMisc.pop_attrib ()
	  
end
