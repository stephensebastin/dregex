package dregex.impl

import com.typesafe.scalalogging.StrictLogging

import scala.collection.immutable.SortedMap
import scala.collection.mutable

case class TagValueFunction(map: Map[Tag, Int]) {
  override def toString = {
    val mapStr = (map.map { case (k, v) => s"$k->$v" }).mkString(",")
    s"TVF($mapStr)"
  }
}

sealed trait Instruction {
  def apply(
      addrs: mutable.Map[TaggedDfa.Address, TagValueFunction],
      from: TaggedDfaState,
      to: TaggedDfaState,
      currentPos: Int
  ): Unit
}

case class Save(state: State, tag: Tag) extends Instruction {

  override def apply(
      addrs: mutable.Map[TaggedDfa.Address, TagValueFunction],
      from: TaggedDfaState,
      to: TaggedDfaState,
      currentPos: Int) = {
    val key = (to, state)
    val tvf = addrs(key)
    val newTvf = tvf.copy(map = tvf.map + (tag -> currentPos))
    addrs(key) = newTvf
  }

}

case class Copy(fromState: State, toState: State) extends Instruction {

  override def apply(
      addrs: mutable.Map[TaggedDfa.Address, TagValueFunction],
      fromDfaState: TaggedDfaState,
      toDfaState: TaggedDfaState,
      currentPos: Int) = {
    val fromKey = (fromDfaState, fromState)
    val toKey = (toDfaState, toState)
    addrs(toKey) = addrs.getOrElse(fromKey, TagValueFunction(Map()))
  }

  override def toString = s"Copy(from=$fromState,to=$toState)"

}

case class LocalCopy(fromState: State, toState: State) extends Instruction {

  override def apply(
    addrs: mutable.Map[TaggedDfa.Address, TagValueFunction],
    fromDfaState: TaggedDfaState,
    toDfaState: TaggedDfaState,
    currentPos: Int) = {
    val fromKey = (toDfaState, fromState)
    val toKey = (toDfaState, toState)
    addrs(toKey) = addrs.getOrElse(fromKey, TagValueFunction(Map()))
    //println(s"Executing LocalCopy. fromKey:$fromKey, $toKey; after copy: ${addrs(toKey)}")
  }

  override def toString = s"LocalCopy(from=$fromState,to=$toState)"

}

case class TaggedDfaState(states: Set[State]) {

  assert(states.nonEmpty)

  override def toString = {
    s"S{${states.mkString(",")}}"
  }

}

case class TaggedDfa(
  initial: TaggedDfaState,
  initialInstructions: Seq[Instruction],
  defTransitions: Map[TaggedDfaState, TaggedDfa.StateTransitions],
  accepting: Set[TaggedDfaState],
  acceptingSubStates: Set[State],
  minimal: Boolean = false
) extends StrictLogging {

  override def toString() = {
    s"initial: $initial; initialInstructions: $initialInstructions; transitions: $defTransitions; accepting: $accepting; acceptingInstructions: $acceptingSubStates"
  }

  lazy val allStates: Set[TaggedDfaState] = {
    Set(initial) ++
      defTransitions.keySet ++
      defTransitions.values.flatMap(_.values.map(_._1)).toSet ++
      accepting
  }

  lazy val allButAccepting: Set[TaggedDfaState] = allStates diff accepting

  lazy val allChars: Set[CharInterval] = defTransitions.values.map(_.keys).flatten.toSet

  lazy val stateCount: Int = allStates.size

  def transitionMap(state: TaggedDfaState): TaggedDfa.StateTransitions =
    defTransitions.getOrElse(state, SortedMap.empty)

}

object TaggedDfa {

  type StateTransitions = SortedMap[CharInterval, (TaggedDfaState, Seq[Instruction])]
  type Address = (TaggedDfaState, State)

  /**
    * Match-nothing DFA
    */
  val NothingDfa = TaggedDfa(
    initial = TaggedDfaState(Set()),
    initialInstructions = Seq(),
    defTransitions = Map(),
    accepting = Set(),
    acceptingSubStates = Set())

}