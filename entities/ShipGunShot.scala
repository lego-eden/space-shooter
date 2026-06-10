package spacegame

import spacegame.Camera.Drawing
import spacegame.util.*
import Vec.*

class ShipGunShot(
    var pos: Vec[Double],
    var prevPos: Vec[Double],
    var vel: Vec[Double],
) extends Entity, KinematicBody, Collider[ShipGunShot]:

  override def relCollider: Shape = Shape.Circle((0.0, 0.0), 1.0)

  override def step(dt: Double)(using state: State): Unit =
    if !state.windowRect.expand(20.0).contains(pos) then state.destroy(this)
    prevPos = pos
    move((0, 0), dt)

  override def endDraw()(using d: Drawing): Unit =
    d.drawColorFloat = Color.white
    d.drawLine(screenPos - vel.normalize*1.0, screenPos)

object ShipGunShot:
  val Speed = 500.0
