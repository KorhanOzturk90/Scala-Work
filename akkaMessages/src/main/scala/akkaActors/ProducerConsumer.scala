package akkaActors

import akka.actor._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Random

/**
 * A simple class demonstrating how messages flow with Akka Actors.
 */
object ProducerConsumer extends App{

  var sharedQueue = mutable.Queue[Int]()
  val system = ActorSystem("Producer-Consumer")
  val actor = system.actorOf(Props[QueueManager], "queueManager")

  actor.tell(Produce(sharedQueue), ActorRef.noSender)

  system.scheduler.schedule(0.seconds, 1.second, actor, Produce(sharedQueue))(system.dispatcher)

}

class QueueManager extends Actor {
  override def receive: Receive = {
    case Consume(messageQueue) => val consumedItem = messageQueue.dequeue()
      printf(s"consumed item: $consumedItem \n")
      //self ! Produce(messageQueue)
    case Produce(messageQueue) => var newValue = Random.nextInt(1000)
      messageQueue += newValue
      printf(s"added item: $newValue \n")
      self ! Consume(messageQueue)
  }
}

case class Consume(sharedQueue : mutable.Queue[Int])
case class Produce(sharedQueue : mutable.Queue[Int])

