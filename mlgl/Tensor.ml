type t =
	{
	  force : Math.Vector.t;
	  moment : Math.Vector.t;
	  application : Math.Vector.t
	}

let create force moment application = 
  {
	force = force;
	moment = moment;
	application = application
  }

let get_resultant t = t.force

let get_moment t = t.moment

let get_application t = t.application

let change_app new_application old = 
  {
	force = old.force;
	moment = 
	  Math.Vector.add 
		old.moment 
		(Math.Vector.cross 
		   (Math.Vector.sub 
			  old.application 
			  new_application) 
		   old.force);
	application = new_application
  }

let add application tensors = 
  let applied_tensors = List.map (change_app application) tensors in
	{
	  force = List.fold_left
		(fun v t -> Math.Vector.add v (t.force))
		Math.Vector.zero 
		applied_tensors;
	  moment = List.fold_left
		(fun v t -> Math.Vector.add v (t.moment))
		Math.Vector.zero 
		applied_tensors;
	  application = application
	}

let to_string tensor = 
  "{ R = " 
  ^ (Math.Vector.to_string tensor.force)
  ^ ", M = "
  ^ (Math.Vector.to_string tensor.moment)
  ^ "} on "
  ^ (Math.Vector.to_string tensor.application)
