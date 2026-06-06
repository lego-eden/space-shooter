package spacegame

import Camera.Drawing
import bearlyb.video.BlendMode
import bearlyb.rect.Rect
import scala.util.Random
import Vec.*

import util.*

class SpaceDust(var pos: Vec[Double], var time: Double, var animationLength: Double) extends Entity:
  def containment(windowRect: Rect[Double]): Rect[Double] = 
    val Rect(rx, ry, rw, rh) = windowRect
    Rect(rx-rw, ry-rh, 3*rw, 3*rh)

  def randomize(windowRect: Rect[Double]): Unit =
    pos = Vec.randomInRect(containment(windowRect)).floor
    animationLength = SpaceDust.randomAnimationLength()
    time = 0

  override def step(dt: Double)(using state: State): Unit =
    val period = 2*math.Pi*time/animationLength
    if period >= 2*math.Pi then
      randomize(state.windowRect)
    time += dt
    val wrappingRect = containment(state.windowRect)
    pos = pos.wrap(wrappingRect)

  override def draw()(using drawing: Drawing): Unit =
    import drawing.*
    val period = 2*math.Pi*time/animationLength
    val opacity = (-math.cos(period)*0.5)+0.5

    drawBlendMode = BlendMode.Blend
    drawColorFloat = (1f,1f,1f,opacity.toFloat*0.75f)
    drawPoint(screenPos)

object SpaceDust:
  def randomAnimationLength(): Double =
    Random.between(0.5, 5.0)

  def randomSpaceDust(windowRect: Rect[Double]): SpaceDust =
    val dust = new SpaceDust((0, 0), 0, 1)
    dust.randomize(windowRect)
    dust

