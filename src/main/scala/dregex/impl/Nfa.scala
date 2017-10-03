package dregex.impl

import scala.collection.immutable.Seq

import Util.StrictMap
import scala.collection.breakOut

case class NfaTransition(from: State, to: State, char: AtomPart, tag: Option[Tag] = None) {

  override def toString() = {
    val tagStr = tag match {
      case Some(t) => s"[$t]"
      case None => ""
    }
    s"$from->$char$tagStr->$to"
  }

}

case class Nfa(initial: State, transitions: Seq[NfaTransition], accepting: Set[State]) {

  override def toString() = {
    val transStr = transitions.mkString("[", "; ", "]")
    val acceptStr = accepting.mkString("{", "; ", "}")
    s"initial: $initial; transitions: $transStr; accepting: $acceptStr"
  }

  lazy val allStates = {
    Set(initial) ++
      transitions.map(_.from) ++
      transitions.map(_.to) ++
      accepting
  }

  /*
   * Group the list of transitions of the NFA into a nested map, for easy lookup.
   */
  val transitionMap: Map[State, Nfa.StateTransitionMap] = {
    transitions.groupBy(_.from).mapValuesNow { stateTransitions =>
      stateTransitions.groupBy(_.char).mapValuesNow { states =>
        states.map(s => (s.to, s.tag)) (breakOut)
      }
    }
  }

}

object Nfa {
  type StateTransitionMap = Map[AtomPart, Map[State, Option[Tag]]]
}