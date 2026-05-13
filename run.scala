import bearlyb as bl
import bl.time.Clock
import bl.rect.Rect
import bl.Event

import scala.util.boundary
import bl.render.Renderer
import bl.video.Window
import bl.render.Renderer.LogicalPresentation
import bl.pixels.PixelFormat
import bl.render.TextureAccess
import bl.surface.ScaleMode

val (worldW, worldH) = (150*2, 100*2)

def run(): Unit =
  val w = Window("Space game", worldW*8, worldH*8, Window.Flag.Fullscreen, Window.Flag.Resizable)
  val r = w.renderer
  val worldTex = r.createTexture(PixelFormat.RGBA8888, TextureAccess.Target, worldW, worldH)
  worldTex.scaleMode = ScaleMode.Nearest

  val ship = Ship(worldW/2, worldH/2, 0)
  var game = Game(w, r, ship)
  val inputState = InputStateTracker()
  val clock = Clock()
  var time = 0.0
  var fps = 0.0

  boundary:
    while true do
      inputState.step() // clear the justPressed and justReleased cache
      Event.pollEvents().foreach:
        case Event.Quit(_) => boundary.break()
        case Event.Key.Down(scancode=sc) =>
          inputState.registerDown(sc)
        case Event.Key.Up(scancode=sc) =>
          inputState.registerUp(sc)
        case _ =>

      game = game.step(clock.deltaDouble)(using inputState)

      r.drawColor = (255, 255, 255, 255)
      r.clear()

      r.target = Some(worldTex)
      r.drawColor = (0, 0, 0, 255)
      r.clear()

      game.draw()(using r)

      r.target = None
      r.logicalPresentation = (worldW, worldH, LogicalPresentation.IntegerScale)
      r.renderTexture[Double](worldTex)
      r.logicalPresentation = (0, 0, LogicalPresentation.Disabled)

      if time > 0.35 then
        fps = clock.fps
        time -= 0.35
      time += clock.deltaDouble

      r.renderScale = (3, 3)
      r.drawColor = (255, 255, 0, 255)
      r.fillRect(Rect(0, 0, 8*5, 8))
      r.drawColor = (0, 0, 0, 255)
      r.renderDebugText(0, 0)(f"${fps}%5.0f")
      r.renderScale = (1, 1)

      r.present()

      clock.tick()
    end while
end run

