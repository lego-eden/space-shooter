package spacegame

import spacegame.Camera.Drawing
import scala.collection.mutable.Buffer
import bearlyb.rect.Rect

class Debug(drawCmds: Buffer[Drawing ?=> Unit] = Buffer.empty, var pos: Vec[Double] = (0,0)) extends Entity:
  override def step(dt: Double)(using inputState: State): Unit = ()

  def apply(f: Drawing ?=> Unit): Unit =
    drawCmds += f

  override def endDraw()(using drawing: Drawing): Unit =
    drawCmds.foreach(_(using drawing))
    drawCmds.clear()

  def drawRect(rect: Rect[Double], r: Int, g: Int, b: Int): Unit =
    apply: drawing ?=>
      val (x, y) = drawing.screenPosOf(rect.pos)
      drawing.drawColor = (r,g,b,255)
      drawing.drawRect(rect.copy(x, y))

  def drawRed(rect: Rect[Double]): Unit = drawRect(rect, 255, 0, 0)
  def drawGreen(rect: Rect[Double]): Unit = drawRect(rect, 0, 255, 0)
  def drawBlue(rect: Rect[Double]): Unit = drawRect(rect, 0, 0, 255)
