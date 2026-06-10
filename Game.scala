package spacegame

import scala.language.experimental.multiSpreads

import scala.collection.mutable.{ArrayBuffer, ArrayDeque}
import scala.util.boundary, boundary.Label as Scope
import scala.annotation.tailrec

import bearlyb.*
import bearlyb.time.Clock
import bearlyb.render.Renderer.LogicalPresentation
import bearlyb.video.BlendMode

import spacegame.util.*
import scala.collection.mutable.HashSet

class Game(
  var worldDim: (w: Int, h: Int),
  entities: ArrayBuffer[Entity],
  spawnQueue: ArrayDeque[Entity],
  destroyQueue: HashSet[Entity],
  cam: Camera,
  debug: Debug,
  clock: Clock,
  var time: Double,
  var fps: Double,
):
  import Game.MaxTilesVisible, Game.TileSize

  val state = State(cam, debug, spawn, destroy, isColliding)

  def step(dt: Double): Unit =
    entities.foreach(_.beginStep(dt)(using state))
    entities.foreach(_.step(dt)(using state))
    entities.foreach(_.endStep(dt)(using state))

  def draw(camera: Camera): Unit =
    entities.foreach(camera.beginDraw)
    entities.foreach(camera.draw)
    entities.foreach(camera.endDraw)

  def spawn(e: Entity): Unit =
    spawnQueue.append(e)

  def spawn(e: Entity*): Unit =
    spawnQueue.appendAll(e)

  def destroy(e: Entity): Unit =
    // destroyQueue.append(e)
    destroyQueue += e

  def destroy(e: Entity*): Unit =
    // destroyQueue.appendAll(e)
    destroyQueue ++= e

  def doSpawn(): Unit =
    entities ++= spawnQueue.removeAll()

  def doDestroy(): Unit =
    for e <- destroyQueue do
      try
        e.destroy()(using state)
      catch case exc =>
        Console.err.println(s"failed to destroy ${e.toString}: ${exc.getMessage()}")
        exc.printStackTrace()
      entities -= e

    destroyQueue.clear()

  def isColliding(e1: Entity, e2: Entity): Boolean = false

  def finish(): Unit =
    for e <- entities do
      try
        e.destroy()(using state)
      catch case exc =>
        Console.err.println(s"failed to destroy ${e.toString}: ${exc.getMessage()}")
        exc.printStackTrace()

  @tailrec
  private def gameloop()(using Scope[Unit]): Nothing =
    state.step() // clear the justPressed and justReleased cache
    Event.pollEvents().foreach:
      case Event.Quit(_) => boundary.break()
      case Event.Key.Down(scancode=sc, repeat=false) =>
        state.registerDown(sc)
      case Event.Key.Up(scancode=sc) =>
        state.registerUp(sc)
      case Event.Window.Resized(w=newW, h=newH) => 
        val timesTilesVisible = ((newW min newH) / (MaxTilesVisible*TileSize)) max 1
        worldDim = (newW / timesTilesVisible, newH / timesTilesVisible)
        cam.resize(worldDim)
      case _ =>

    step(clock.deltaDouble)

    doDestroy()
    doSpawn()

    val r = cam.r

    r.drawColor = (255, 255, 255, 255)
    r.clear()

    r.target = Some(cam.tex)
    r.drawBlendMode = BlendMode.Blend
    r.drawColor = (0, 0, 0, 255)
    r.clear()

    draw(cam)

    r.target = None
    r.logicalPresentation = (worldDim.w, worldDim.h, LogicalPresentation.IntegerScale)
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
    
    gameloop()
  end gameloop

  def run(): Unit =
    boundary:
      gameloop()
    finish()

object Game:

  val TileSize = 8
  val MaxTilesVisible = 30

  def run(): Unit =
    val worldDim: (w: Int, h: Int) = ((MaxTilesVisible*1.5).toInt*TileSize, MaxTilesVisible*TileSize)

    val w = Window("Space game", worldDim.w*4, worldDim.h*4, Window.Flag.Fullscreen, Window.Flag.Resizable)
    val r = w.renderer

    val debug = Debug()
    val ship = Ship((0, 0), (0, 0), 0)
    val shipFollow = ShipFollow(ship, (0,0))
    val cam = Camera(r, (0, 0), worldDim, shipFollow)
    val spaceDust = Seq.fill(1000)(SpaceDust.randomSpaceDust(cam.rect))

    val game = new Game(
      worldDim,
      entities = ArrayBuffer.empty,
      spawnQueue = ArrayDeque.empty,
      destroyQueue = HashSet.empty, //ArrayDeque.empty,
      cam,
      debug,
      clock = Clock(),
      time = 0.0,
      fps = 0.0,
    )

    val asteroids = AsteroidCluster.fillWithin(50, game.state.windowRect.expandN(3), game.state.windowRect)(using game.state)

    game.spawn(cam, spaceDust*, ship, shipFollow, asteroids, debug)
    game.run()

end Game
