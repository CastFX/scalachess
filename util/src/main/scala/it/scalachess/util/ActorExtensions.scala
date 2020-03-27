package it.scalachess.util

import akka.actor.typed.ActorRef

object ActorExtensions {
  implicit class OptionActor[A, T <: ActorRef[A]](actor: Option[T]) {
    def applyIfPresent(f: ActorRef[A] => Unit): Unit = if (actor isDefined) f(actor.get)
    def ![B <: A](msg: B): Unit                      = applyIfPresent(_ ! msg)
  }
}
