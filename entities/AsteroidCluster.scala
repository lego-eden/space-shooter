import scala.collection.mutable.HashSet as MutSet
import scala.collection.mutable.ArrayDeque
import bearlyb.rect.Rect
import util.*

class AsteroidCluster(val asteroids: MutSet[Asteroid]) extends Entity:

  var pos: Vec[Double] = (0, 0)

  override def step(dt: Double)(using State): Unit =
    asteroids.foreach(_.step(dt))

  override def draw()(using Camera.Drawing): Unit =
    asteroids.foreach(_.draw())

  given AsteroidCluster = this

  def create(size: Asteroid.Size, pos: Vec[Double]): Unit =
    val id = nextId()
    val wasAdded = asteroids.add(Asteroid.random(id, pos, size))
    assert(wasAdded, s"The asteroid with id $id already existed")

  def destroy(a: Asteroid): Unit =
    val wasRemoved = asteroids.remove(a)
    assert(wasRemoved, s"The asteroid with id ${a.id} did not exist when it was destroyed")

  private var _nextId = -1
  private val reuse = ArrayDeque.empty[Int]
  private def nextId(): Int = reuse.removeHeadOption().getOrElse:
    _nextId += 1
    _nextId

object AsteroidCluster:
  def fillWithin(numAsteroids: Int, boundary: Rect[Double], notWithin: Rect[Double]): AsteroidCluster =
    val cluster = AsteroidCluster(MutSet.empty)
    for _ <- 0 until numAsteroids do
      cluster.create(
        Asteroid.Size.random(),
        Vec.randomInRect(boundary, notWithin.expand(100.0))
      )
    cluster
