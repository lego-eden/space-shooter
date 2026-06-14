package spacegame

import spacegame.Camera.Drawing
import spacegame.util.*
import Vec.*
import scala.util.Using
import bearlyb.render.VertexBuffer
import bearlyb.render.Vertex

class ShipGunShot(
    val shotFrom: Ship,
    var pos: Vec[Double],
    var prevPos: Vec[Double],
    var vel: Vec[Double],
) extends Entity, KinematicBody, Collider[ShipGunShot]:

  override def relCollider: Shape = Shape.Circle((0.0, 0.0), 1.0)

  override def step(dt: Double)(using state: State): Unit =
    if !state.windowRect.expand(20.0).contains(pos) then state.destroy(this)
    prevPos = pos
    move((0, 0), dt)

  def offset(p: Vec[Double])(using d: Drawing): Vec[Double] =
    d.screenPosOf(pos + p.rotate(vel.dir))

  def cornerPosition(i: Int)(using Drawing): Vec[Double] =
    i match
      case 0 => offset(-3d, -0.5d)
      case 1 => offset(-3d, 0.5d)
      case 2 => offset(0d, -0.5d)
      case 3 => offset(0d, 0.5d)

  override def endDraw()(using d: Drawing): Unit =
    d.drawColorFloat = Color.white
    // d.drawLine(screenPos - vel.normalize*1.0, screenPos)
    var i = 0
    ShipGunShot.ProjectileBuf.mapInPlace(v =>
      var result = v.copy(cornerPosition(i).map(_.toFloat))
      i += 1
      result
    ): Unit
    d.renderGeometry(
      ShipGunShot.ProjectileBuf,
      indices = ShipGunShot.ProjectileBufIndices
    )

object ShipGunShot:
  val Speed = 800.0
  val ProjectileBuf = VertexBuffer(
    Vertex((0d,0d), Color.white, (0d, 0d)),
    Vertex((0d,0d), Color.white, (0d, 0d)),
    Vertex((0d,0d), Color.white, (0d, 0d)),
    Vertex((0d,0d), Color.white, (0d, 0d)),
  )
  val ProjectileBufIndices = Seq(
    0, 1, 2,
    1, 2, 3,
  )
