import Vec.*

trait KinematicBody:
  this: Entity =>
  
  var vel: Vec[Double]
  def move(acc: Vec[Double], dt: Double): Unit =
    vel += 0.5*acc*dt
    pos = pos + vel*dt
    vel += 0.5*acc*dt
