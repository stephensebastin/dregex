package dregex.impl

import java.util.concurrent.atomic.AtomicInteger

import dregex.impl.RegexTree.CaptureGroup

trait State

class SimpleState extends State {
  val id = SimpleState.counter.getAndIncrement()
  override def toString() = s"s${Util.toSubscriptString(id)}"
}

object SimpleState {
  private val counter = new AtomicInteger
}

case class BiState[A <: State](first: A, second: A) extends State {
  override def toString() = {
    s"$first,$second"
  }
}

case class MultiState(states: Set[State]) extends State  {
  override def toString() = {
    states.mkString(",")
  }
}

sealed trait TagType
object TagType {
  case object Start extends TagType
  case object End extends TagType
}

class Tag(val capturingGroup: CaptureGroup, val tagType: TagType) {

  override def toString = {
    s"Tag(group=${capturingGroup.id},$tagType)"
  }

}