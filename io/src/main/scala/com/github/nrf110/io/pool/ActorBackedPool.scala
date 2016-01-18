package com.github.nrf110.io.pool

import scala.annotation.tailrec
import scala.collection.immutable
import scala.concurrent.{Promise, Future}
import scala.concurrent.duration._

import akka.actor._

import com.github.nrf110.util.extensions.future._

abstract class ActorBackedPool[A](resizer: PoolResizer,
                                  factory: () => A,
                                  destroy: A => Unit)
                                 (implicit system: ActorSystem) extends Pool[A] {

  import system.dispatcher

  val underlying = system.actorOf(Props(new PoolActor()))

  def reserve(timeout: Option[FiniteDuration] = None): Future[A] = {
    val promise = Promise[A]()
    underlying ! Reserve(promise)

    Future.firstCompletedOf[A](
      immutable.Seq(promise.future) ++
        timeout.map(Future.timeout(_))
    )
  }

  def release(item: A): Unit =
    underlying ! Release(item)

  private[this] class PoolActor extends Actor {
    var items: immutable.Queue[A] = immutable.Queue.empty[A]
    var requests: immutable.Queue[Reserve] = immutable.Queue.empty[Reserve]
    var totalPoolSize: Int = 0

    system.scheduler.schedule(0.seconds, 5.seconds, self, Resize)

    def receive: Receive = {
      case request: Reserve =>
        requests = requests enqueue request
        drain()

      case Release(item) =>
        items = items enqueue item
        drain()

      case Resize => resize()
    }

    @tailrec
    private[this] def drain(): Unit =
      items.dequeueOption.zip(requests.dequeueOption).headOption match {
        case Some(((item, remainingItems), (request, remainingRequests))) =>
          /**
            * If the request has already timed out, the promise will already be completed and trySuccess will return false.
            * If this happens, discard the request and keep the item.
            */
          items = if (request.promise.trySuccess(item)) remainingItems else items
          drain()

        case _ =>
      }

    private[this] def resize(): Unit =
      resizer.resize(requests.length, items.length, totalPoolSize) match {
        case ResizeResult.NoOp =>

        case ResizeResult.Grow(increaseBy) =>
          items = items enqueue (0 until increaseBy).map(_ => factory())
          totalPoolSize = totalPoolSize + increaseBy

          if (requests.nonEmpty) drain()

        case ResizeResult.Shrink(decreaseBy) =>
          val (itemsToDestroy, itemsToKeep) = items.splitAt(decreaseBy)
          itemsToDestroy foreach destroy

          items = itemsToKeep
          totalPoolSize = totalPoolSize - itemsToDestroy.length
      }
  }

  private[this] case class Reserve(promise: Promise[A])
  private[this] case class Release(item: A)
  private[this] case object Resize
}
