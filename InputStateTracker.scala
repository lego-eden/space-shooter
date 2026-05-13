import bearlyb.scancode.Scancode

import scala.collection.mutable.Set as MutSet

class InputStateTracker(
  justPressedSet: MutSet[Scancode],
  keyDownSet: MutSet[Scancode],
  justReleasedSet: MutSet[Scancode],
) extends InputState:
  def keyUp(key: Scancode) = !keyDownSet(key)
  def keyDown(key: Scancode) = keyDownSet(key)
  def justPressed(key: Scancode) = justPressedSet(key)
  def justReleased(key: Scancode) = justReleasedSet(key)

  def step(): Unit =
    justPressedSet.clear()
    justReleasedSet.clear()

  def registerDown(key: Scancode) =
    justPressedSet += key
    keyDownSet += key

  def registerUp(key: Scancode) =
    justReleasedSet += key
    keyDownSet -= key

object InputStateTracker:
  def apply(): InputStateTracker =
    new InputStateTracker(
      justPressedSet = MutSet.empty,
      keyDownSet = MutSet.empty,
      justReleasedSet = MutSet.empty,
    )
