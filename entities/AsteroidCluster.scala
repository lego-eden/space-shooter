package spacegame

import scala.collection.mutable.HashSet as MutSet
import scala.collection.mutable.ArrayDeque
import bearlyb.rect.Rect
import util.*

class AsteroidCluster() extends Entity:

  var pos: Vec[Double] = (0, 0)

  override def step(dt: Double)(using State): Unit = ()

  override def draw()(using Camera.Drawing): Unit = ()

  def create(size: Asteroid.Size, pos: Vec[Double])(using state: State): Unit =
    state.spawn(Asteroid.random(pos, size, this))

object AsteroidCluster:
  def fillWithin(numAsteroids: Int, boundary: Rect[Double], notWithin: Rect[Double])(using State): AsteroidCluster =
    val cluster = AsteroidCluster()
    for _ <- 0 until numAsteroids do
      cluster.create(
        Asteroid.Size.random(),
        Vec.randomInRect(boundary, notWithin.expand(100.0)),
      )
    cluster
