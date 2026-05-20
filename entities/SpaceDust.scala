import Camera.Drawing
import bearlyb.video.BlendMode
import bearlyb.rect.Rect
import scala.util.Random
import Vec.*

class SpaceDust(var pos: Vec[Double], var time: Double, var animationLength: Double) extends Entity:
  def randomize(windowRect: Rect[Double]): Unit =
    pos = Vec.randomInRect(windowRect)//.map(_.toInt)
    animationLength = SpaceDust.randomAnimationLength()
    time = 0

  override def step(dt: Double)(using inputState: State): Unit =
    if (!inputState.windowRect.contains(pos)) then
      randomize(inputState.windowRect)
    else
      val period = 2*math.Pi*time/animationLength
      if period >= 2*math.Pi then
        randomize(inputState.windowRect)
      time += dt

  override def draw()(using drawing: Drawing): Unit =
    import drawing.*
    val period = 2*math.Pi*time/animationLength
    val opacity = (-math.cos(period)*0.5)+0.5

    drawBlendMode = BlendMode.Blend
    drawColorFloat = (1,1,1,opacity.toFloat)
    drawPoint(screenPos)

object SpaceDust:
  def randomAnimationLength(): Double =
    Random.between(3.0, 8.0)

  def randomSpaceDust(windowRect: Rect[Double]): SpaceDust =
    new SpaceDust(
      Vec.randomInRect(windowRect).map(_.toInt),
      0,
      randomAnimationLength(),
    )
