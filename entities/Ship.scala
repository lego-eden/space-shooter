package spacegame

import bearlyb.scancode.Scancode
import Vec.*
import bearlyb.render.VertexBuffer

import scala.math
import scala.util.Using
import bearlyb.render.Vertex

class Ship(var pos: Vec[Double], var vel: Vec[Double], var dir: Double) extends Entity, KinematicBody:
  def step(dt: Double)(using inputState: State): Unit =
    import inputState.*
    if keyDown(Scancode.Left) then
      turn(-Ship.turnRate*dt)
    if keyDown(Scancode.Right) then
      turn(Ship.turnRate*dt)

    val acc: Vec[Double] =
      if keyDown(Scancode.Up) then
        (1, 0).rotate(dir) * Ship.accel
      else 
        (0, 0)

    move(acc, dt)

  def turn(angle: Double): Unit = dir += angle

  override def draw()(using drawing: Camera.Drawing): Unit =
    import drawing.*

    val points = Seq(shipFront, wingTipRight, shipBack, shipFront, wingTipLeft, shipBack)
    val verts: Seq[Vertex[Double]] = points.map(p => Vertex(p+(0.5,0.5), (0,0,0,255), (0.0,0.0)))
    Using.resource(VertexBuffer(verts*)): vb =>
      renderGeometry(vb)

    drawColor = (255, 255, 255, 255)
    drawLine(shipFront, wingTipLeft)
    drawLine(shipFront, wingTipRight)
    drawLine(wingTipRight, shipBack)
    drawLine(wingTipLeft, shipBack)
    
  end draw

  def offset(d: Vec[Double])(using Camera.Drawing) =
    (screenPos + d.rotate(dir).floor)

  def shipFront(using Camera.Drawing)    = offset(10.0,  0.0)
  def shipBack(using Camera.Drawing)     = offset(-3.0,  0.0)
  def wingTipLeft(using Camera.Drawing)  = offset(-6.0,  8.0)
  def wingTipRight(using Camera.Drawing) = offset(-6.0, -8.0)

end Ship

object Ship:
  val turnRate = (2*math.Pi / 0.6)
  val accel = 200.0
