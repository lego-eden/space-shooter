package spacegame

import bearlyb.Rect as BlRect
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.reflect.ClassTag

trait Collider[T](using tag: ClassTag[T]):
  this: Entity & T =>

  cache += this
  onDestroy(cache -= this)

  def cache(using cache: Collider.Cache[T]) = cache

  def relCollider: Shape

  def absCollider: Shape =
    relCollider.relativeTo(pos)

  def isColliding[B](other: Collider[B]): Boolean =
    absCollider.overlaps(other.absCollider)

  def onOverlap[B <: Collider[?]](body: B => Unit)(using otherCache: Collider.Cache[B]): Unit =
    otherCache.filter(isColliding).foreach(body)

  def onCollision(body: (Collider[?], Vec[Double], Vec[Double]) => Unit)(using state: State): Unit =
    state.qt.foreachCollision(this)(body)

  def foreachCollider[B <: Collider[?]](body: B => Unit)(using otherCache: Collider.Cache[B]): Unit =
    otherCache.foreach(body)

  def collisionDebug(testAgainst: Option[Collider[?]] = None)(using drawing: Camera.Drawing): Unit =
    drawing.drawColorFloat = Color.blue
    testAgainst.foreach: other =>
      if isColliding(other) then
        drawing.drawColorFloat = Color.red
    absCollider.draw()

object Collider:
  opaque type Cache[T] <: HashSet[T] = HashSet[T]
  object Cache:
    private val cacheStore = HashMap.empty[ClassTag[?], HashSet[?]]
    given [T] => (tag: ClassTag[T]) => Cache[T] =
      cacheStore
        .getOrElseUpdate(tag, HashSet.empty[T])
        .asInstanceOf[Cache[T]]
