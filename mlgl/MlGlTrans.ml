
class transformation trans elts = 
object(self)
  inherit(MlGlG.group elts) as super

  method get_bounding_volume = 
	Bound.transform
	  trans
	  (Bound.merge
		 (List.map (fun e -> e#get_bounding_volume) elts))
	  
  method private do_cull m f obj = 
	if MlGlG.fully_outside m self#get_bounding_volume f
	then
	  None
	else if MlGlG.fully_inside m self#get_bounding_volume f
	then
	  Some (self :> MlGlG.nodeT)
	else
	  MlGlG.cull_children
		elts
		(Math.Matrix.m_mult m trans)
		f
		obj
end

class translate trans elts =
object(self)
  inherit(transformation (Math.Matrix.translate trans) elts) as super

  method render () =
	GlMat.mode `modelview;
	GlMat.push ();
	GlMat.translate 
	  ~x:(Math.Vector.get_x trans) 
	  ~y:(Math.Vector.get_y trans)
	  ~z:(Math.Vector.get_z trans)
	  ();
	super#render ();
	GlMat.pop ()

  method cull m f = 
	super#do_cull 
	  m 
	  f 
	  (new translate trans :> (MlGlG.nodeT list -> MlGlG.nodeT))

end

class rotate angle trans elts =
object
  inherit(transformation (Math.Matrix.rotate trans angle) elts) as super

  method render () =
	GlMat.mode `modelview;
	GlMat.push ();
	GlMat.rotate
	  ~angle:(angle *. 360. /. (2. *. Math.pi))
	  ~x:(Math.Vector.get_x trans) 
	  ~y:(Math.Vector.get_y trans)
	  ~z:(Math.Vector.get_z trans)
	  ();
	super#render ();
	GlMat.pop ()	

  method cull m f = 
	super#do_cull 
	  m 
	  f 
	  (new rotate angle trans :> (MlGlG.nodeT list -> MlGlG.nodeT))
end

class scale trans elts =
object
  inherit(transformation (Math.Matrix.scale trans) elts) as super

  method render () =
	GlMat.mode `modelview;
	GlMat.push ();
	GlMat.scale
	  ~x:(Math.Vector.get_x trans) 
	  ~y:(Math.Vector.get_y trans)
	  ~z:(Math.Vector.get_z trans)
	  ();
	super#render ();
	GlMat.pop ()

  method cull m f = 
	super#do_cull 
	  m 
	  f 
	  (new scale trans :> (MlGlG.nodeT list -> MlGlG.nodeT))
end

