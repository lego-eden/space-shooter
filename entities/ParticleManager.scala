package spacegame

import scala.util.Random
import Vec.*
import scala.collection.mutable.BitSet
import spacegame.Camera.Drawing
import bearlyb.rect.Rect
import util.lerp as slerp

class ParticleManager private (
    private val px: Array[Double],
    private val py: Array[Double],
    private val pvx: Array[Double],
    private val pvy: Array[Double],
    private val pt: Array[Double],
    private val plifetime: Array[Double],
    private var dt: Double = 0.0,
) extends Entity:
  var pos = (0, 0)

  val freeParticles =
    (0 until ParticleManager.NParticles).to(BitSet)

  def random(
      pos: Vec[Double],
      speed: (Double, Double) | Double,
      lifetime: (Double, Double) | Double,
      baseVel: Vec[Double] = (0.0, 0.0),
      initTime: Double = 0.0,
      angle: (Double, Double) | Double = (0.0, math.Pi*2),
      n: Int = 1,
  ): Unit =
    for _ <- 0 until n do
      val dir = angle match
        case (from, to) => Random.between(from, to)
        case const: Double => const

      val spd = speed match
        case (from, to) => Random.between(from, to)
        case const: Double => const

      val vel = baseVel + (spd, 0.0).rotate(dir)
      val lftime = lifetime match
        case (from, to) => Random.between(from, to)
        case const: Double => const

      create(pos, vel, lftime, initTime)
  end random

  def create(pos: Vec[Double], vel: Vec[Double], lifetime: Double, initTime: Double = 0.0): Unit =
    freeParticles.headOption match
      case Some(i) =>
        freeParticles.remove(i)
        px(i) = pos.x
        py(i) = pos.y
        pvx(i) = vel.x
        pvy(i) = vel.y
        pt(i) = initTime min lifetime
        plifetime(i) = lifetime
      case None =>
        Console.err.println("Err: failed to create particle")
    
  override def step(dt: Double)(using State): Unit =
    this.dt = dt

  inline def alpha(inline i: Int) = 1.0 - (pt(i)/plifetime(i))

  override def beginDraw()(using drawing: Drawing): Unit =
    for
      i <- 0 until ParticleManager.NParticles
      // if pt(i) < 1.0
      if !freeParticles(i)
    do
      drawing.drawColorFloat = Color.white.withAlpha(alpha(i).toFloat)
      drawing.drawPoint(drawing.screenPosOf(px(i), py(i)))

      px(i) += dt * pvx(i)
      py(i) += dt * pvy(i)
      pt(i) = plifetime(i) min (pt(i) + dt)

      if pt(i) >= plifetime(i) then
        freeParticles.add(i): Unit
  end beginDraw

object ParticleManager:
  
  inline val NParticles = 4096
  private def emptyArr = Array.ofDim[Double](NParticles)

  def apply(): ParticleManager =
    new ParticleManager(
      px = emptyArr,
      py = emptyArr,
      pvx = emptyArr,
      pvy = emptyArr,
      pt = emptyArr,
      plifetime = emptyArr,
    )

  class Particle(var pos: Vec[Double], var vel: Vec[Double], var t: Double, var lifetime: Double)

end ParticleManager
