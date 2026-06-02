package spacegame

import spacegame.Camera.Drawing
import scala.collection.mutable.Buffer

class Debug(drawCmds: Buffer[Drawing => Unit] = Buffer.empty, var pos: Vec[Double] = (0,0)) extends Entity:
  override def step(dt: Double)(using inputState: State): Unit = ()

  def apply(f: Drawing => Unit): Unit =
    drawCmds += f

  override def draw()(using drawing: Drawing): Unit =
    drawCmds.foreach(_(drawing))
    drawCmds.clear()
