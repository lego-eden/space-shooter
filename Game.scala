import scala.collection.mutable.ArrayBuffer

class Game(
  entities: ArrayBuffer[Entity],
):
  def step(dt: Double)(using inputState: InputState): Unit =
    entities.foreach(_.step(dt))

  def draw(camera: Camera): Unit =
    entities.iterator
      .foreach(camera.draw)

  def addEntity(e: Entity): Unit =
    entities += e

object Game:
  def apply(): Game = new Game(ArrayBuffer.empty)
