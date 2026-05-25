import Vec.*
import Camera.Drawing
import bearlyb.rect.Rect

case class ShipFollow(val target: Ship, var pos: Vec[Double]) extends Entity:
  def step(dt: Double)(using inputState: State): Unit =
    val velOffset = target.vel * ShipFollow.FollowAhead
    val lookOffset = (ShipFollow.LookOffset, 0.0).rotate(target.dir)
    pos = target.pos + velOffset + lookOffset

object ShipFollow:
  val FollowAhead = 0.4
  val LookOffset = 20.0
