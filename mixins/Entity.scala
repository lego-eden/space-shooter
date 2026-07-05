package spacegame

import scala.collection.mutable.ArrayBuffer

trait Entity:
  private val onDestroyBuffer = ArrayBuffer.empty[State ?=> Unit]

  def onDestroy(f: State ?=> Unit): Unit =
    onDestroyBuffer += f
    
  def beginStep(dt: Double)(using State): Unit = ()
  def step(dt: Double)(using State): Unit = ()
  def endStep(dt: Double)(using State): Unit = ()
  
  def beginDraw()(using Camera.Drawing): Unit = ()
  def draw()(using Camera.Drawing): Unit = ()
  def endDraw()(using Camera.Drawing): Unit = ()
  
  final def destroy()(using State): Unit =
    onDestroyBuffer.foreach(f => f)
