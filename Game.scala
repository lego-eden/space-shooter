import bearlyb.video.Window
import bearlyb.render.Renderer
import bearlyb.render.Texture
import bearlyb.events.Event

import scala.util.boundary

case class Game(
  w: Window,
  r: Renderer,
  ship: Ship,
):
  def step(dt: Double)(using inputState: InputState): Game =
    copy(
      ship = ship.step(dt)
    )
  end step

  def draw()(using r: Renderer): Unit =
    ship.draw()
