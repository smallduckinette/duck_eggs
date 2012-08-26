class compile elts =
object(self)
  inherit(MlGlG.group elts) as super
	
  val mutable list = None
	
  method render context =
	match list with
		None ->
		  begin
			list <- (Some (GlList.create `compile_and_execute));
			super#render context;
			GlList.ends ()
		  end
	  | Some l ->
		  GlList.call l
			
  method cull m f = 
	if MlGlG.fully_outside m self#get_bounding_volume f then
	  None
	else 
	  Some (self :> MlGlG.nodeT)
end
  
