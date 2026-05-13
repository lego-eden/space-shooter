import bearlyb.scancode.Scancode

trait InputState:
  def keyUp(key: Scancode): Boolean
  def keyDown(key: Scancode): Boolean
  def justPressed(key: Scancode): Boolean
  def justReleased(key: Scancode): Boolean
