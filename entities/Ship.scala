package spacegame

import bearlyb.scancode.Scancode
import Vec.{`*`, +, -, /, rotate, map, dir, lerp as vlerp, project, x, y, onScreen}
import bearlyb.render.VertexBuffer

import scala.math
import scala.util.Using
import bearlyb.render.Vertex
import bearlyb.Rect
import scala.collection.mutable.ArrayBuffer

import spacegame.util.lerp
import scala.util.Random

class Ship(
    var hp: Int = Ship.MaxHealth,
    var pos: Vec[Double] = (0, 0),
    var vel: Vec[Double] = (0, 0),
    var dir: Double = 0,
    var shootCooldown: Double = Ship.MaxShootCooldown,
    var trailCooldown: Double = Ship.MaxTrailCooldown,
    var score: Long = 0,
    var justGotHitTimer: Double = 0,
) extends Entity, Position, KinematicBody, Collider[Ship], Health:
  
  // val relCollider = Shape.Circle((0.0, 0.0), 5)
  def relCollider = Shape.Convex(Vector(shipFront, wingTipRight*0.5, wingTipLeft*0.5))

  override def step(dt: Double)(using state: State): Unit =
    import state.*
    // save for interpolation of bullets
    val prevPos = pos
    val prevVel = vel
    val prevDir = dir

    if keyDown(Scancode.Left) then
      turn(-Ship.turnRate*dt)
    if keyDown(Scancode.Right) then
      turn(Ship.turnRate*dt)

    val acc: Vec[Double] =
      if keyDown(Scancode.Up) then
        (1, 0).rotate(dir) * Ship.accel
      else 
        (0, 0)

    move(acc, dt)

    if justGotHit then
      justGotHitTimer -= dt
    end if

    onCollision((other, thisTrVec, otherTrVec) => other match
      case asteroid: Asteroid =>
        val vA1 = this.vel.project(thisTrVec)
        val vA1Rest = this.vel - vA1
        val vB1 = asteroid.vel.project(otherTrVec)
        val vB1Rest = asteroid.vel - vB1
        val mA = Ship.Mass
        val mB = asteroid.size.mass
        val mDen = Ship.Dampening/(mA + mB)
        val vA2 = (mA-mB)*mDen*vA1 + 2*mB*mDen*vB1
        val vB2 = 2*mA*mDen*vA1 + (mB-mA)*mDen*vB1
        this.vel = vA2 + vA1Rest
        asteroid.vel = vB2 + vB1Rest
        this.pos += thisTrVec
        asteroid.pos += otherTrVec
        if !justGotHit then
          state.particle.random(
            pos,
            speed = 10d -> 200d,
            lifetime = 0.1d -> 1.4d,
            n = 80,
          )
          hp -= 1
          justGotHitTimer = Ship.JustGotHitInvincibilityTime
      case _ =>
    )

    if keyDown(Scancode.Space) then
      val prevShootCooldown = shootCooldown
      shootCooldown += dt

      var timeToNextShot = Ship.MaxShootCooldown - prevShootCooldown

      while shootCooldown >= Ship.MaxShootCooldown do
        val t = (timeToNextShot / dt) min 1.0
        shootCooldown -= Ship.MaxShootCooldown
        timeToNextShot += Ship.MaxShootCooldown
        shoot(t, prevPos, prevVel, prevDir, acc, dt)
    end if

    if acc != (0d, 0d) then
      val prevTrailCooldown = trailCooldown
      trailCooldown += dt

      var timeToNextParticle = Ship.MaxTrailCooldown - prevTrailCooldown

      while trailCooldown >= Ship.MaxTrailCooldown do
        val t = (timeToNextParticle / dt) min 1.0
        trailCooldown -= Ship.MaxTrailCooldown
        timeToNextParticle += Ship.MaxTrailCooldown
        spawnTrailParticle(t, prevPos, prevVel, prevDir, acc, dt)
    end if

    state.ui.text((UI.Top, UI.Center), s"$score")
    state.ui.text((UI.Top, UI.Left), s"$hp HP")
  end step

  override def endStep(dt: Double)(using state: State): Unit =
    ()
    // val from1 = pos + (20d, 0d).rotate(dir)
    // val to1 = pos + (80d, 0d).rotate(dir)
    //
    // state.debug: drawing ?=>
    //   drawing.drawColorFloat = Color.green
    //   drawing.drawLine(drawing.screenPosOf(from1), drawing.screenPosOf(to1))
    //
    // state.qt.foreachLineIntersection(from1, to1):
    //   case (a: Asteroid, near, far) =>
    //     state.debug: drawing ?=>
    //       drawing.drawColorFloat = Color.blue
    //       a.absCollider.draw()
    //     for p <- Seq(near, far) do
    //       state.debug.drawRed(Rect(p.x-1d, p.y-1d, 3d, 3d))
    //   case _ => ()
  end endStep


  def shoot(
      t: Double,
      prevPos: Vec[Double],
      prevVel: Vec[Double],
      prevDir: Double,
      acc: Vec[Double],
      dt: Double
  )(using state: State): Unit =
    val tau = t * dt

    val spawnPos = prevPos + prevVel*tau + 0.5*acc*tau*tau
    val spawnVel = prevVel + acc*tau
    val spawnDir = prevDir.lerp(dir, t)

    val muzzle = spawnPos + (10.0, 0.0).rotate(spawnDir)

    val shotVel = spawnVel + (ShipGunShot.Speed, 0.0).rotate(spawnDir)

    val remainingTime = dt - tau
    val finalSpawnPos = muzzle + shotVel * remainingTime
    val shot = ShipGunShot(this, finalSpawnPos, finalSpawnPos, shotVel)
    state.particle.random(
      finalSpawnPos,
      baseVel = spawnVel,
      speed = 30d -> 40d,
      lifetime = (0.05) -> (0.2),
      initTime = tau,
      angle = (shotVel.dir - math.Pi*0.25) -> (shotVel.dir + math.Pi*0.25),
      n = 3,
    )
    state.particle.random(
      finalSpawnPos,
      baseVel = spawnVel,
      speed = 80d -> 100d,
      lifetime = (0.15-tau) -> (0.3-tau),
      angle = (shotVel.dir - math.Pi*0.05) -> (shotVel.dir + math.Pi*0.05),
      n = 3,
    )
    state.spawn(shot)
  end shoot

  def spawnTrailParticle(
      t: Double,
      prevPos: Vec[Double],
      prevVel: Vec[Double],
      prevDir: Double,
      acc: Vec[Double],
      dt: Double
  )(using state: State): Unit =
    val tau = t * dt

    val spawnPos = prevPos + prevVel*tau + 0.5*acc*tau*tau
    val spawnVel = prevVel + acc*tau
    val spawnDir = prevDir.lerp(dir, t)

    for offset <- (-2 to 2).map(_.toDouble) do
      val pDir = spawnDir + offset * math.Pi * 0.015
      val lerpTarget = (-6.0, offset.sign*8.0)
      val engine = spawnPos + (-3.0, 0.0).vlerp(lerpTarget, offset.abs/8).rotate(pDir)

      val pVel = spawnVel - (Ship.TrailParticleSpeed, 0.0).rotate(pDir)

      val remainingTime = dt - tau
      val finalSpawnPos = engine + pVel * remainingTime
      state.particle.random(
        finalSpawnPos,
        baseVel = spawnVel,
        speed = -Ship.TrailParticleSpeed,
        lifetime = 0.1 -> 0.15,
        initTime = remainingTime, //tau,
        angle = pDir,
      )
    end for
  end spawnTrailParticle


  def turn(angle: Double): Unit = dir += angle

  override def draw()(using drawing: Camera.Drawing): Unit =
    import drawing.*

    val points = Seq(screenShipFront, screenWingTipRight, screenShipBack, screenShipFront, screenWingTipLeft, screenShipBack)
    val verts: Seq[Vertex[Double]] = points.map(p => Vertex(p+(0.5,0.5), Color.black, (0.0,0.0)))
    Using.resource(VertexBuffer(verts*)): vb =>
      renderGeometry(vb)

    if justGotHit then
      val alpha = math.sin(4*math.Pi*justGotHitTimer)*0.25 + 0.75
      drawColorFloat = Color.white.withAlpha(alpha.toFloat)
    else
      drawColorFloat = Color.white
    drawLine(screenShipFront, screenWingTipLeft)
    drawLine(screenShipFront, screenWingTipRight)
    drawLine(screenWingTipRight, screenShipBack)
    drawLine(screenWingTipLeft, screenShipBack)
    
  end draw

  def offset(d: Vec[Double]): Vec[Double] =
    (d.rotate(dir).map(_.round.toDouble))

  def screenShipFront(using Camera.Drawing)    = (pos + shipFront).onScreen
  def screenShipBack(using Camera.Drawing)     = (pos + shipBack).onScreen
  def screenWingTipLeft(using Camera.Drawing)  = (pos + wingTipLeft).onScreen
  def screenWingTipRight(using Camera.Drawing) = (pos + wingTipRight).onScreen

  def shipFront    = offset(10.0,  0.0)
  def shipBack     = offset(-3.0,  0.0)
  def wingTipLeft  = offset(-6.0,  8.0)
  def wingTipRight = offset(-6.0, -8.0)

  def justGotHit: Boolean = justGotHitTimer > 0d

end Ship

object Ship:
  val turnRate = (2*math.Pi / 1.0)
  val accel = 200.0
  val ShotsPerSecond = 5.0
  val MaxShootCooldown = 1/ShotsPerSecond
  val TrailParticlesPerSecond = 150.0
  val MaxTrailCooldown = 1/TrailParticlesPerSecond
  val TrailParticleSpeed = 120.0
  val Mass = 0.001
  val Dampening = 0.5
  val MaxHealth = 3
  val JustGotHitInvincibilityTime = 2.0
