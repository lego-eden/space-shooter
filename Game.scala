package spacegame

import scala.collection.mutable.ArrayBuffer

class Game(
  entities: ArrayBuffer[Entity],
):
  def step(dt: Double)(using inputState: State): Unit =
    entities.foreach(_.step(dt))

  def draw(camera: Camera): Unit =
    entities.foreach(camera.draw)

  def add(e: Entity*): Unit =
    entities ++= e

  def destroy(): Unit =
    for e <- entities do
      try
        e.destroy()
      catch case exc =>
        Console.err.println(s"failed to destroy ${e.toString}: ${exc.getMessage()}")
        exc.printStackTrace()

object Game:
  def apply(): Game = new Game(ArrayBuffer.empty)
