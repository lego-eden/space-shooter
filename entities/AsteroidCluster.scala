package spacegame

import scala.collection.mutable.HashSet as MutSet
import scala.collection.mutable.ArrayDeque
import bearlyb.rect.Rect
import util.*

class AsteroidCluster(var spawnCooldown: Double, var maxSpawnCooldown: Double, var increaseSpawnRateCooldown: Double) extends Entity:

  var pos: Vec[Double] = (0, 0)

  override def step(dt: Double)(using state: State): Unit =
    spawnCooldown += dt
    increaseSpawnRateCooldown += dt
    lazy val boundary = state.windowRect.expandN(3)
    lazy val notWithin = state.windowRect
    
    while increaseSpawnRateCooldown >= AsteroidCluster.TimeBetweenSpawnrateIncrease do
      increaseSpawnRateCooldown -= AsteroidCluster.TimeBetweenSpawnrateIncrease
      maxSpawnCooldown = maxSpawnCooldown * AsteroidCluster.SpawnCooldownMul max 1E-3

    while spawnCooldown >= maxSpawnCooldown do
      spawnCooldown -= maxSpawnCooldown
      createRandom(boundary, notWithin)
  end step

  override def draw()(using Camera.Drawing): Unit = ()

  def createRandom(boundary: Rect[Double], notWithin: Rect[Double])(using State): Unit =
    create(
      Asteroid.Size.random(),
      Vec.randomInRect(boundary, notWithin.expand(100.0))
    )

  def create(size: Asteroid.Size, pos: Vec[Double])(using state: State): Unit =
    state.spawn(Asteroid.random(pos, size, this))

object AsteroidCluster:
  val InitMaxSpawnCooldown = 2.0
  val SpawnCooldownMul = 0.99
  val TimeBetweenSpawnrateIncrease = 3.0

  // def fillWithin(numAsteroids: Int, boundary: Rect[Double], notWithin: Rect[Double])(using State): AsteroidCluster =
  //   val cluster = AsteroidCluster(InitMaxSpawnCooldown, InitMaxSpawnCooldown)
  //   for _ <- 0 until numAsteroids do
  //     cluster.create(
  //       Asteroid.Size.random(),
  //       Vec.randomInRect(boundary, notWithin.expand(100.0)),
  //     )
  //   cluster
  def apply(): AsteroidCluster =
    new AsteroidCluster(InitMaxSpawnCooldown, InitMaxSpawnCooldown, 0.0)
