let unitcube = new MlGlShapes.cube 1.

let cube = new MlGlShapes.cube

let outside_cylinder = 
  new MlGlShapes.cylinder 
	MlGlShapes.Not_Textured 
	MlGlShapes.Outside

let inside_cylinder = 
  new MlGlShapes.cylinder 
	MlGlShapes.Not_Textured 
	MlGlShapes.Inside

let textured_outside_cylinder = 
  new MlGlShapes.cylinder 
	MlGlShapes.Textured 
	MlGlShapes.Outside

let textured_inside_cylinder = 
  new MlGlShapes.cylinder 
	MlGlShapes.Textured 
	MlGlShapes.Inside

let torus = new MlGlShapes.torus

let uvsphere = 
  new MlGlShapes.uvsphere
	MlGlShapes.Not_Textured 

let textured_uvsphere = 
  new MlGlShapes.uvsphere
	MlGlShapes.Textured 

let textured_square = new MlGlShapes.textured_square

let grid = new MlGlShapes.grid

let normal_grid = new MlGlShapes.normal_grid

let auto_grid = new MlGlShapes.auto_grid

let group = new MlGlG.group

let translate = new MlGlTrans.translate

let scale = new MlGlTrans.scale

let rotate = new MlGlTrans.rotate

let color = new MlGlStyle.color

let wireframe = new MlGlStyle.wireframe

let compile = new MlGlUtils.compile

let texture = new MlGlStyle.texture
