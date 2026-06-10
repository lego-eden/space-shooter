package spacegame

import bearlyb.scancode.Scancode
import Vec.{`*`, +, -, /, rotate, map}
import bearlyb.render.VertexBuffer

import scala.math
import scala.util.Using
import bearlyb.render.Vertex
import bearlyb.Rect
import scala.collection.mutable.ArrayBuffer

import spacegame.util.lerp

class Ship(
    var pos: Vec[Double],
    var vel: Vec[Double],
    var dir: Double,
    var shootCooldown: Double = Ship.MaxShootCooldown,
) extends Entity, KinematicBody, Collider[Ship]:
  
  val relCollider = Shape.Circle((0.0, 0.0), 5)

  override def step(dt: Double)(using state: State): Unit =
    import state.*
    // save for interpolation of bullets
    val prevPos = pos
    val prevVel = vel
    val prevDir = dir

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

    if keyDown(Scancode.Space) then
      val prevShootCooldown = shootCooldown
      shootCooldown += dt

      var timeToNextShot = Ship.MaxShootCooldown - prevShootCooldown

      while shootCooldown >= Ship.MaxShootCooldown do
        val t = (timeToNextShot / dt) min 1.0
        shootCooldown -= Ship.MaxShootCooldown
        timeToNextShot += Ship.MaxShootCooldown
        shoot(t, prevPos, prevVel, prevDir, acc, dt)
      end while
    end if

  end step

  def shoot(t: Double, prevPos: Vec[Double], prevVel: Vec[Double], prevDir: Double, acc: Vec[Double], dt: Double)(using state: State): Unit =
    val tau = t * dt

    val spawnPos = prevPos + prevVel*tau + 0.5*acc*tau*tau
    val spawnVel = prevVel + acc*tau
    val spawnDir = prevDir.lerp(dir, t)

    val muzzle = spawnPos + (10.0, 0.0).rotate(spawnDir)

    val shotVel = spawnVel + (ShipGunShot.Speed, 0.0).rotate(spawnDir)

    val remainingTime = dt - tau
    val finalSpawnPos = muzzle + shotVel * remainingTime
    val shot = ShipGunShot(finalSpawnPos, finalSpawnPos, shotVel)
    state.spawn(shot)

  def turn(angle: Double): Unit = dir += angle

  override def draw()(using drawing: Camera.Drawing): Unit =
    import drawing.*

    val points = Seq(shipFront, wingTipRight, shipBack, shipFront, wingTipLeft, shipBack)
    val verts: Seq[Vertex[Double]] = points.map(p => Vertex(p+(0.5,0.5), Color.black, (0.0,0.0)))
    Using.resource(VertexBuffer(verts*)): vb =>
      renderGeometry(vb)

    drawColorFloat = Color.white
    drawLine(shipFront, wingTipLeft)
    drawLine(shipFront, wingTipRight)
    drawLine(wingTipRight, shipBack)
    drawLine(wingTipLeft, shipBack)
    
  end draw

  def offset(d: Vec[Double])(using Camera.Drawing) =
    (screenPos + d.rotate(dir).map(_.round.toDouble))

  def shipFront(using Camera.Drawing)    = offset(10.0,  0.0)
  def shipBack(using Camera.Drawing)     = offset(-3.0,  0.0)
  def wingTipLeft(using Camera.Drawing)  = offset(-6.0,  8.0)
  def wingTipRight(using Camera.Drawing) = offset(-6.0, -8.0)

end Ship

object Ship:
  val turnRate = (2*math.Pi / 1.0)
  val accel = 200.0
  val MaxShootCooldown = 0.2
