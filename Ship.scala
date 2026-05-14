import bearlyb.scancode.Scancode
import Vec.*

import scala.math

class Ship(var pos: Vec[Double], var dir: Double) extends Entity:
  def step(dt: Double)(using inputState: InputState): Unit =
    import inputState.*
    if keyDown(Scancode.Left) then
      turn(-Ship.turnRate*dt)
    if keyDown(Scancode.Right) then
      turn(Ship.turnRate*dt)

  def turn(angle: Double): Unit = dir += angle

  override def draw()(using drawing: Camera.Drawing): Unit =
    import drawing.*

    drawColor = (255, 255, 255, 255)
    drawLine(shipFront, wingTipLeft)
    drawLine(shipFront, wingTipRight)
    drawLine(wingTipRight, shipBack)
    drawLine(wingTipLeft, shipBack)
  end draw

  def offset(d: Vec[Double])(using Camera.Drawing) =
    screenPos + d.rotate(dir)

  def shipFront(using Camera.Drawing)    = offset( 0.0, -10.0)
  def shipBack(using Camera.Drawing)     = offset( 0.0,   3.0)
  def wingTipLeft(using Camera.Drawing)  = offset(-8.0,   6.0)
  def wingTipRight(using Camera.Drawing) = offset( 8.0,   6.0)

end Ship

object Ship:
  val turnRate = (2*math.Pi / 0.6)
