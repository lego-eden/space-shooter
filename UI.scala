package spacegame

import bearlyb.render.Renderer
import bearlyb.rect.Rect
import scala.collection.mutable.ArrayBuffer
import Vec.*

class UI private (private val drawCmds: ArrayBuffer[Camera => Unit]):

  def text(at: UI.Placement, text: String, maxWidth: Float = 0f): Unit =
    // val (w, h) = UI.Font.measure(text, maxWidth)
    val w = 8f * text.length
    val h = 8f
    drawCmds.addOne: cam => 
      val pt = UI.coords(at, cam, w, h)
      cam.r.drawColorFloat = Color.white
      cam.r.renderDebugText(pt.map(_.toInt))(text)

  def draw(cam: Camera): Unit =
    drawCmds.foreach(cmd => cmd(cam))
    drawCmds.clear()

object UI:
  export VPlacement.*, HPlacement.*

  val Font = bearlyb.render.Font.default
    .withTextSize(12)

  def apply(): UI = new UI(
    ArrayBuffer.empty
  )

  enum VPlacement:
    case Top, Middle, Bottom
    def y(cam: Camera, h: Float): Float =
      this match
        case Top => 0f
        case Middle => cam.tex.h.toFloat/2f - h/2f
        case Bottom => cam.tex.h.toFloat - h

  enum HPlacement:
    case Left, Center, Right
    def x(cam: Camera, w: Float): Float =
      this match
        case Left => 0f
        case Center => cam.tex.w.toFloat/2f - w/2f
        case Right => cam.tex.w.toFloat - w

  type Placement = (v: VPlacement, h: HPlacement)

  def coords(at: Placement, cam: Camera, w: Float, h: Float): Vec[Float] =
    import VPlacement.*, HPlacement.*
    (at.h.x(cam, w), at.v.y(cam, h))
end UI
