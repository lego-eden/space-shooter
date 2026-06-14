package spacegame

import bearlyb.scancode.Scancode

import scala.collection.mutable.Set as MutSet
import bearlyb.rect.Rect

class State(
  val justPressed: MutSet[Scancode],
  val keyDown: MutSet[Scancode],
  val justReleased: MutSet[Scancode],
  val camera: Camera,
  val debug: Debug,
  val particle: ParticleManager,
  val ui: UI,
  val spawn: Entity => Unit,
  val destroy: Entity => Unit,
  val isColliding: (Entity, Entity) => Boolean,
):

  def keyUp(key: Scancode) = !keyDown(key)

  def step(): Unit =
    justPressed.clear()
    justReleased.clear()

  def registerDown(key: Scancode): Unit =
    justPressed += key
    keyDown += key

  def registerUp(key: Scancode): Unit =
    justReleased += key
    keyDown -= key

  def windowRect: Rect[Double] =
    camera.rect

object State:
  def apply(
      camera: Camera,
      debug: Debug,
      particle: ParticleManager,
      ui: UI,
      spawn: Entity => Unit,
      destroy: Entity => Unit,
      isColliding: (Entity, Entity) => Boolean
  ): State =
    new State(
      justPressed = MutSet.empty,
      keyDown = MutSet.empty,
      justReleased = MutSet.empty,
      camera = camera,
      debug = debug,
      particle,
      ui,
      spawn,
      destroy,
      isColliding,
    )
