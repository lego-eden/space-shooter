package spacegame

trait Entity:
  var pos: Vec[Double]
  final def screenPos(using drawing: Camera.Drawing): Vec[Double] =
    drawing.screenPosOf(pos)
    
  def step(dt: Double)(using State): Unit
  def draw()(using Camera.Drawing): Unit = ()
  def destroy(): Unit = ()
  
