package spacegame.util

import spacegame.Vec, Vec.*
import bearlyb.Rect
import spacegame.{State, Debug}
import scala.compiletime.ops.int.S
import scala.compiletime.constValue

def debug(using state: State): Debug = state.debug

extension (d: Double)
  def wrap(min: Double, width: Double): Double =
    val shifted = d-min
    val timesWidth = (shifted/width).floor
    val normalisedValue = shifted - timesWidth*width
    min + normalisedValue

  def clamp(min: Double, max: Double): Double =
    if d < min then min
    else if d > max then max
    else d

extension (rect: Rect[Double])
  def expand(margin: Double): Rect[Double] =
    val Rect(x, y, w, h) = rect
    Rect(x-margin, y-margin, w+margin*2, h+margin*2)

  def expandN(n: Double) =
    val Rect(x, y, w, h) = rect
    Rect(
      x-w*(n-1)/2,
      y-h*(n-1)/2,
      w*n,
      h*n
    )

extension (u: Vec[Double])
  def wrap(rect: Rect[Double]): Vec[Double] =
    val x = u.x.wrap(rect.x, rect.w)
    val y = u.y.wrap(rect.y, rect.h)
    (x, y)

  def clamp(rect: Rect[Double]): Vec[Double] =
    val x = u.x.clamp(rect.x, rect.xmax)
    val y = u.y.clamp(rect.y, rect.ymax)
    (x, y)

extension [A: Fractional as frac](from: A)
  def lerp(to: A, t: A): A =
    frac.plus(from, frac.times(frac.minus(to, from), t))
