package spacegame

import bearlyb.render.Renderer
import bearlyb.render.Texture
import bearlyb.video.Window
import bearlyb.pixels.PixelFormat
import bearlyb.render.TextureAccess
import bearlyb.surface.ScaleMode
import bearlyb.video.BlendMode
import bearlyb.rect.Rect
import spacegame.util.debug
import Vec.*

class Camera private (
    val r: Renderer,
    var tex: Texture,
    var pos: Vec[Double],
    val follow: Entity,
) extends Entity:
  def step(dt: Double)(using state: State): Unit =
    pos = pos.lerp(follow.pos, 1.0 - math.pow(Camera.FollowSpeed, dt))

  def draw(entity: Entity): Unit =
    entity.draw()(using Camera.Drawing(this))
    

  def resize(dim: Vec[Int]): Unit =
    tex.destroy()
    tex = r.createTexture(PixelFormat.RGBA8888, TextureAccess.Target, dim.x, dim.y)
    tex.scaleMode = ScaleMode.Nearest
    tex.blendMode = BlendMode.Blend

  def rect: Rect[Double] =
    val (x, y) = pos
    val (w, h) = (tex.w, tex.h)
    Rect(x-w/2, y-h/2, w, h)

object Camera:
  val FollowSpeed = 0.05

  def apply(r: Renderer, pos: Vec[Double], dim: Vec[Int], follow: Entity): Camera =
      val tex = r.createTexture(PixelFormat.RGBA8888, TextureAccess.Target, dim.x, dim.y)
      tex.scaleMode = ScaleMode.Nearest
      tex.blendMode = BlendMode.Blend
      new Camera(r, tex, pos, follow)

  case class Drawing private[Camera] (private val cam: Camera):
    export cam.r.*
    def screenPosOf(p: Vec[Double]): Vec[Double] =
      p - cam.pos + cam.tex.dim.toDoubleVec/2.0
