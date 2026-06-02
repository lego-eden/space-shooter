package spacegame

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
    val follow: Entity,
) extends Entity:

  def step(dt: Double)(using inputState: State): Unit =
    pos = pos.lerp(follow.pos, 1.0 - math.pow(Camera.FollowSpeed, dt))

  def draw(entity: Entity): Unit =
    entity.draw()(using Camera.Drawing(this))

  def resize(dim: Vec[Int]): Unit =
    tex.destroy()
    tex = r.createTexture(PixelFormat.RGBA8888, TextureAccess.Target, dim.x, dim.y)
    tex.scaleMode = ScaleMode.Nearest
    tex.blendMode = BlendMode.Blend

  def followScreenPos: Vec[Double] =
    val texDim = (tex.w, tex.h).map(_.toDouble)
    (follow.pos - pos + texDim.map(_/2)).map(_.floor)

  def rect: Rect[Double] =
    val (x, y) = pos
    val (w, h) = (tex.w, tex.h)
    Rect(x-w/2, y-h/2, w, h)

object Camera:
  def apply(r: Renderer, pos: Vec[Double], dim: Vec[Int], follow: Entity): Camera =
      val tex = r.createTexture(PixelFormat.RGBA8888, TextureAccess.Target, dim.x, dim.y)
      tex.scaleMode = ScaleMode.Nearest
      new Camera(r, tex, pos, follow)

  case class Drawing private[Camera] (private val cam: Camera):
    export cam.r.*
    def screenPosOf(p: Vec[Double]): Vec[Double] =
      // val followScreenPos = cam.followScreenPos
      // val offset = p - cam.follow.pos.map(_.toInt.toDouble)
      // followScreenPos + offset
      
      // (p - cam.pos + cam.tex.dim.toDoubleVec/2.0).floor
      p - cam.pos + cam.tex.dim.toDoubleVec/2.0

  val FollowSpeed = 0.05
