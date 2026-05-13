import bearlyb.video.Window

trait Entity[A]:
  def step(dt: Double)(using inputState: InputState): A 
