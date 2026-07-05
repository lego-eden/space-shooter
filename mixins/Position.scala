package spacegame

trait Position:
  var pos: Vec[Double]

  final def screenPos(using drawing: Camera.Drawing): Vec[Double] =
    drawing.screenPosOf(pos)
