import bearlyb as bl
import bl.time.Clock
import bl.rect.Rect
import bl.Event

import scala.util.boundary
import bl.video.Window
import bl.render.Renderer.LogicalPresentation

def run(): Unit =
  val TileSize = 8
  lazy val MaxTilesVisible = 30
  var (worldW, worldH) = ((MaxTilesVisible*1.5).toInt*TileSize, MaxTilesVisible*TileSize)

  val w = Window("Space game", worldW*4, worldH*4, Window.Flag.Fullscreen, Window.Flag.Resizable)
  val r = w.renderer

  val ship = Ship((0, 0), (0, 0), 0)
  val shipFollow = ShipFollow(ship, (0,0))
  val cam = Camera(r, (0, 0), (worldW, worldH), shipFollow)
  val game = Game()
  val asteroids = AsteroidCluster.fillWithin(50, cam.resizedRect(3), cam.rect)
  val spaceDust = Seq.fill(2000)(SpaceDust.randomSpaceDust(cam.rect))
  game.add(spaceDust*)
  game.add(ship)
  game.add(shipFollow)
  game.add(asteroids)
  game.add(cam)
  val inputState = State(cam)
  val clock = Clock()
  var time = 0.0
  var fps = 0.0

  boundary {
    while true do
      inputState.step() // clear the justPressed and justReleased cache
      Event.pollEvents().foreach:
        case Event.Quit(_) => boundary.break()
        case Event.Key.Down(scancode=sc) =>
          inputState.registerDown(sc)
        case Event.Key.Up(scancode=sc) =>
          inputState.registerUp(sc)
        case Event.Window.Resized(w=newW, h=newH) => 
          val timesTilesVisible = ((newW min newH) / (MaxTilesVisible*TileSize)) max 1
          worldW = newW / timesTilesVisible
          worldH = newH / timesTilesVisible
          cam.resize(worldW, worldH)
        case _ =>

      game.step(clock.deltaDouble)(using inputState)

      r.drawColor = (255, 255, 255, 255)
      r.clear()

      r.target = Some(cam.tex)
      r.drawColor = (0, 0, 0, 255)
      r.clear()

      game.draw(cam)

      r.target = None
      r.logicalPresentation = (worldW, worldH, LogicalPresentation.IntegerScale)
      r.renderTexture[Double](cam.tex)
      r.logicalPresentation = (0, 0, LogicalPresentation.Disabled)

      if time > 0.35 then
        fps = clock.fps
        time -= 0.35
      time += clock.deltaDouble

      r.renderScale = (2, 2)
      r.drawColor = (255, 255, 0, 255)
      r.fillRect(Rect(0, 0, 8*5, 8))
      r.drawColor = (0, 0, 0, 255)
      r.renderDebugText(0, 0)(f"${fps}%5.0f")
      r.renderScale = (1, 1)

      r.present()

      clock.tick(): Unit
    end while
  }
end run

