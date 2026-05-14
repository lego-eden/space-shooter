trait Entity:
  var pos: Vec[Double]
  def step(dt: Double)(using inputState: InputState): Unit
  def draw()(using drawing: Camera.Drawing): Unit = ()
