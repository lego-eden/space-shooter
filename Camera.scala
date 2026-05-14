import bearlyb.render.Renderer
import bearlyb.render.Texture
import bearlyb.video.Window
import Vec.*
import bearlyb.pixels.PixelFormat
import bearlyb.render.TextureAccess
import bearlyb.surface.ScaleMode

class Camera private (val r: Renderer, var pos: Vec[Double], var tex: Texture):

  def draw(entity: Entity): Unit =
    entity.draw()(using Camera.Drawing(this))

  def resize(dim: Vec[Int]): Unit =
    tex.destroy()
    tex = r.createTexture(PixelFormat.RGBA8888, TextureAccess.Target, dim.x, dim.y)
    tex.scaleMode = ScaleMode.Nearest

object Camera:
  def apply(r: Renderer, pos: Vec[Double], dim: Vec[Int]): Camera =
      val tex = r.createTexture(PixelFormat.RGBA8888, TextureAccess.Target, dim.x, dim.y)
      tex.scaleMode = ScaleMode.Nearest
      new Camera(r, pos, tex)

  case class Drawing private[Camera] (private val cam: Camera):
    export cam.r.*
    def screenPos(p: Vec[Double]): Vec[Double] =
      val texDim = (cam.tex.w.toDouble, cam.tex.h.toDouble)
      p - cam.pos + texDim.map(_/2)
