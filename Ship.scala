import bearlyb.render.Renderer
import bearlyb.scancode.Scancode
import Vec.*

import scala.math

case class Ship(x: Double, y: Double, dir: Double) extends Drawable, Entity[Ship]:
  import Ship.turnRate

  def step(dt: Double)(using inputState: InputState): Ship =
    import inputState.*
    var ship = this
    if keyDown(Scancode.Left) then
      ship = ship.turn(-Ship.turnRate*dt)
    if keyDown(Scancode.Right) then
      ship = ship.turn(Ship.turnRate*dt)

    ship

  def turn(angle: Double): Ship =
    copy(dir = dir + angle)

  def draw()(using r: Renderer): Unit =
    import r.*
    drawColor = (255, 255, 255, 255)
    // ln(shipFront, wingTipRight, shipBack, wingTipLeft, shipFront)
    drawLine(shipFront, wingTipLeft)
    drawLine(shipFront, wingTipRight)
    drawLine(wingTipRight, shipBack)
    drawLine(wingTipLeft, shipBack)
  end draw

  def offset(d: Vec[Double]) =
    pos + d.rotate(dir)

  val pos = (x, y)
  lazy val shipFront    = offset(  0.0, -12.0)
  lazy val shipBack     = offset(  0.0,   5.0)
  lazy val wingTipLeft  = offset(-10.0,   8.0)
  lazy val wingTipRight = offset( 10.0,   8.0)

end Ship

object Ship:

  val turnRate = (2*math.Pi / 0.4)

end Ship
