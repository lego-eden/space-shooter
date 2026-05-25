import bearlyb.render.Renderer
import bearlyb.render.Texture
import bearlyb.video.Window
import Vec.*
import bearlyb.pixels.PixelFormat
import bearlyb.render.TextureAccess
import bearlyb.surface.ScaleMode
import bearlyb.video.BlendMode
import bearlyb.rect.Rect

class Camera private (
    val r: Renderer,
    var tex: Texture,
    var pos: Vec[Double],
    var vel: Vec[Double],
    val follow: Entity & KinematicBody,
) extends Entity, KinematicBody:
  import Camera.{PositionMod, VelocityMod}

  def step(dt: Double)(using inputState: State): Unit =
    val posDelta = (follow.pos - pos) * PositionMod
    val velDelta = (follow.vel - vel) * VelocityMod
    val acc = posDelta + velDelta
    move(acc, dt)

  def draw(entity: Entity): Unit =
    entity.draw()(using Camera.Drawing(this))

  def resize(dim: Vec[Int]): Unit =
    tex.destroy()
    tex = r.createTexture(PixelFormat.RGBA8888, TextureAccess.Target, dim.x, dim.y)
    tex.scaleMode = ScaleMode.Nearest
    tex.blendMode = BlendMode.Blend

  def followScreenPos: Vec[Double] =
    val texDim = (tex.w, tex.h).map(_.toDouble)
    (follow.pos - pos + texDim.map(_/2)).map(_.round.toDouble)

  def rect: Rect[Double] =
    val (x, y) = pos
    val (w, h) = (tex.w, tex.h)
    Rect(x-w/2, y-h/2, w, h)

  def resizedRect(n: Int): Rect[Double] =
    val nOdd = if n % 2 == 0 then n+1 else n
    val Rect(rx, ry, rw, rh) = rect
    Rect(rx-rw*(nOdd/2), ry-rh*(nOdd/2), nOdd*rw, nOdd*rh)

object Camera:
  def apply(r: Renderer, pos: Vec[Double], dim: Vec[Int], follow: Entity & KinematicBody): Camera =
      val tex = r.createTexture(PixelFormat.RGBA8888, TextureAccess.Target, dim.x, dim.y)
      tex.scaleMode = ScaleMode.Nearest
      new Camera(r, tex, pos, (0,0), follow)

  case class Drawing private[Camera] (private val cam: Camera):
    export cam.r.*
    def screenPosOf(p: Vec[Double]): Vec[Double] =
      val followScreenPos = cam.followScreenPos
      val offset = p - cam.follow.pos.map(_.toInt.toDouble)
      followScreenPos + offset

  val PositionMod = 10.0
  val VelocityMod = math.sqrt(PositionMod)
