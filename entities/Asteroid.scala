package spacegame

import Camera.Drawing
import Vec.*
import util.*

import scala.util.Random
import scala.util.Using.Releasable
import bearlyb.render.VertexBuffer
import bearlyb.render.Vertex

class Asteroid(
    val id: Int,
    var pos: Vec[Double],
    var vel: Vec[Double],
    var angle: Double,
    var secondsPerRotation: Double,
    val corners: Seq[Vec[Double]],
    val cluster: AsteroidCluster,
    var vertexBuffer: VertexBuffer,
) extends Entity, KinematicBody:

  def rotationSpeed: Double = 2*math.Pi / secondsPerRotation

  override def destroy(): Unit =
    summon[Releasable[VertexBuffer]].release(vertexBuffer)
    cluster.destroy(this)

  override def step(dt: Double)(using state: State): Unit =
    angle += rotationSpeed * dt
    if angle < 0 then
      angle += 2*math.Pi
    else if angle > 2*math.Pi then
      angle -= 2*math.Pi

    pos = pos.wrap(state.windowRect.expandN(3))

    move((0,0), dt)

  def renderAngle: Double =
    val snapToN = 20.0
    val snapAngle = 2*math.Pi / snapToN
    (angle / snapAngle).floor * snapAngle

  def cornersOnScreen(using Drawing): Seq[Vec[Double]] =
    corners.map(p => screenPos + p.rotate(renderAngle).floor)

  override def draw()(using drawing: Drawing): Unit =
    import drawing.*
    val corners = cornersOnScreen

    // val triangles = corners.sliding(2).flatMap(pair => pair :+ screenPos).toSeq
    // Using.resource(VertexBuffer.from(triangles.map(p => Vertex(p+(0.5,0.5), (0,0,0,255), (0.0,0.0))))): verts =>
    //   renderGeometry(verts)
    val cornersWithMiddle = corners :+ screenPos
    var i = 0
    vertexBuffer.mapInPlace: oldVert =>
      val newVert = oldVert.copy(pos=cornersWithMiddle(i).map(_.toFloat+0.5f))
      i += 1
      newVert
    : Unit
    val triangleIndices = (corners.indices.toSeq :+ 0)
      .sliding(2)
      .flatMap(pair => pair :+ corners.length)
      .toIndexedSeq
    renderGeometry(vertexBuffer, indices=triangleIndices)

    drawColorFloat = (1f,1f,1f,1f)
    drawLines(corners :+ corners.head)
  end draw

  override def hashCode: Int = id

object Asteroid:
  val Speed = 20.0

  enum Size:
    case Small, Medium, Large

    lazy val smaller: Option[Size] = this match
      case Small => None
      case Medium => Some(Small)
      case Large => Some(Medium)
  
    lazy val numCorners: Int = this match
      case Small => 4
      case Medium => 5
      case Large => 7

    lazy val radiuses: Seq[Double] =
      val sizeIncr = this match
        case Small => 3.0
        case Medium => 3.0
        case Large => 3.0
      val originalRadius = this match
        case Small => 3.0
        case Medium => 12.0
        case Large => 20.0
      Seq.iterate(originalRadius, numCorners)(_+sizeIncr)

    def randomCorners(): Seq[Vec[Double]] =
      val angleDelta = 2*math.Pi/numCorners
      val cornerAngles = Seq.tabulate(numCorners)(i => angleDelta*i)
      val radiuses = Random.shuffle(this.radiuses)
      cornerAngles.zip(radiuses).map((a, r) =>
        (r, 0.0).rotate(a).map(_.toInt.toDouble)
      )
  end Size

  object Size:
    def random(): Size =
      val values = this.values
      val i = Random.between(0, values.length)
      values(i)

  def random(id: Int, pos: Vec[Double], size: Size, cluster: AsteroidCluster): Asteroid =
    val angle = Random.between(0, 2*math.Pi)
    val vel = (Speed, 0.0).rotate(angle)
    val secondsPerRotation = Random.between(3.0, 5.0)
    val rotationDir = if Random.nextBoolean() then -1.0 else 1.0
    val corners = size.randomCorners()
    new Asteroid(
      id,
      pos,
      vel,
      angle,
      secondsPerRotation*rotationDir,
      corners,
      cluster,
      defaultVertexBuffer(corners.size)
    )

  private val DefaultVertex = Vertex((0f, 0f), (0f, 0f, 0f, 1f), (0f, 0f))
  def defaultVertexBuffer(numCorners: Int): VertexBuffer =
    VertexBuffer.from(Seq.fill(numCorners+1)(DefaultVertex))
  
end Asteroid

