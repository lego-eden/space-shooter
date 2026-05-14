trait Entity:
  var pos: Vec[Double]
  final def screenPos(using drawing: Camera.Drawing): Vec[Double] =
    drawing.screenPos(pos)
    
  def step(dt: Double)(using inputState: InputState): Unit
  def draw()(using drawing: Camera.Drawing): Unit = ()
  
