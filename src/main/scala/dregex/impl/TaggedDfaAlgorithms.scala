package dregex.impl

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import Util.StrictMap
import dregex.impl.RegexTree.CaptureGroup

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.collection.mutable

object TaggedDfaAlgorithms {

  /**
    * Produce a DFA from a NFA using the 'power set construction'
    * https://en.wikipedia.org/w/index.php?title=Powerset_construction&oldid=547783241
    */
  def fromNfa(nfa: Nfa, minimal: Boolean = false): TaggedDfa = {

    val epsilonFreeTransitions = nfa.transitionMap.mapValuesNow { trans =>
      // warn: partial function in for comprehension!
      for ((char: CharInterval, target) <- trans) yield char -> target
    }

    val epsilonExpansionCache = mutable.Map[Set[State], (TaggedDfaState, Seq[Instruction])]()

    // Given a transition map and a set of states of a NFA, this function augments that set, following all epsilon
    // transitions recursively
    def followEpsilon(current: Set[State]) = epsilonExpansionCache.get(current).getOrElse {
      val res = followEpsilonImpl(current, Seq())
      epsilonExpansionCache(current) = res
      println(s"followEpsilon(${current}): $res")
      res
    }
    @tailrec
    def followEpsilonImpl(currentStates: Set[State], currentInstructions: Seq[Instruction]): (TaggedDfaState, Seq[Instruction]) = {
      val newInstructions = mutable.Map[(State, State), Seq[Instruction]]()

      val immediate: Seq[Set[State]] = currentStates.map { state =>
        val stateTrans = nfa.transitionMap.getOrElse(state, Map())
        val epsilonNeighbors = stateTrans.getOrElse(Epsilon, Map())
        epsilonNeighbors.map { case (targetState, tag) =>
          tag match {
            case Some(t) =>
              //println("Existing: " + instruction.get((state, targetState)))
              newInstructions((state, targetState)) = Seq(
                LocalCopy(fromState = state, toState = targetState),
                Save(state = targetState, tag = t))
            case None =>
              //println("Existing: " + instruction.get((state, targetState)))
              newInstructions((state, targetState)) = Seq(
                LocalCopy(fromState = state, toState = targetState))
          }
          targetState
        }.toSet
      }.toSeq
      val expandedState = immediate.flatMap(currentStates ++ _).toSet

      if (expandedState == currentStates) {
        (TaggedDfaState(currentStates), currentInstructions)
      } else {
        followEpsilonImpl(expandedState, currentInstructions ++ newInstructions.values.flatten)
      }
    }

    val (dfaInitial, initialInstructions) = followEpsilon(Set(nfa.initial))
    val dfaTransitions = mutable.Map[TaggedDfaState, SortedMap[CharInterval, (TaggedDfaState, Seq[Instruction])]]()
    val dfaStates = mutable.Set[TaggedDfaState]()
    val pending = mutable.Queue[TaggedDfaState](dfaInitial)

    while (pending.nonEmpty) {
      val current = pending.dequeue()
      dfaStates.add(current)
      // The set of all transition maps of the members of the current state
      val currentTrans: Set[Map[CharInterval, Map[State, Seq[Instruction]]]] = current.states.map { state =>
        epsilonFreeTransitions
          .getOrElse(state, Map())
          .mapValuesNow { x =>
            // TODO: ignore tags as this is epsilon-free?
            assert(!x.values.exists(_.isDefined))
            x.keySet.map { targetState =>
              (targetState, Seq(Copy(fromState = state, toState = targetState)))
            }.toMap
          }
      }
      // The transition function of the current state
      val mergedCurrentTrans = currentTrans.reduceLeft { (left, right) =>
        Util.merge(left, right) { (l, r) =>
          Util.merge(l, r) { (a, b) =>
            a ++ b
          }
        }
      }

      // use a temporary set before enqueueing to avoid adding the same state twice
      val newPending = mutable.Set[TaggedDfaState]()

      val dfaCurrentTrans = for ((char, stateMap) <- mergedCurrentTrans) yield {
        val (targetState, instructions) = followEpsilon(stateMap.keySet)
        if (!dfaStates.contains(targetState))
          newPending += targetState
        char -> (targetState, stateMap.values.flatten.toSeq ++ instructions)
      }
      pending.enqueue(newPending.toSeq: _*)
      if (dfaCurrentTrans.nonEmpty)
        dfaTransitions(current) = SortedMap(dfaCurrentTrans.toSeq: _*)
    }
    // a DFA state is accepting if any of its NFA member-states is
    val dfaAccepting = dfaStates.filter(st => Util.doIntersect(st.states, nfa.accepting)).toSet
    TaggedDfa(dfaInitial, initialInstructions, dfaTransitions.toMap, dfaAccepting, nfa.accepting, minimal)
  }

  case class CapturedGroup(group: CaptureGroup, start: Int, end: Int) {
    override def toString = {
      s"Captured(${group.id}=[$start-$end])"
    }
  }

  def matchString(dfa: TaggedDfa, string: String): (Boolean, Seq[CapturedGroup], Int) = {
    val tvfs = mutable.Map[TaggedDfa.Address, TagValueFunction]()
    var current = dfa.initial
    var currentPos = 0
    for (instruction <- dfa.initialInstructions) {
      instruction.apply(tvfs, current, current, currentPos)
    }

    for (codePoint <- string.codePoints.iterator.asScala) {
      val char = UnicodeChar(codePoint)
      val currentTrans = dfa.defTransitions.getOrElse(current, SortedMap[CharInterval, (TaggedDfaState, Seq[Instruction])]())
      // O(log transitions) search in the range tree
      val x = Util.floorEntry(currentTrans, CharInterval(from = char, to = char))
      val newState = x.flatMap {
        case (interval, (state, instructions)) =>
          if (interval.to >= char) {
            for (instruction <- instructions) {
              instruction.apply(tvfs, current, state, currentPos + 1)
            }
            println("Tags: " + tvfs)
            Some(state)
          } else {
            None
          }
      }
      newState match {
        case Some(state) =>
          current = state
        case None =>
          return (false, Seq(), currentPos)
      }
      currentPos += 1
    }
    if (dfa.accepting.contains(current)) {
      val tvf = {
        val acceptingSubStates = (current.states intersect dfa.acceptingSubStates)
        val acceptingSubState = acceptingSubStates.toSeq match {
          case Seq() => throw new IllegalStateException("at least one final sub-state expected")
          case Seq(single) => single
          case multi => throw new IllegalStateException("more than one sub-state found, I don't know what to do")
        }
        tvfs.getOrElse((current, acceptingSubState), TagValueFunction(Map()))
      }
      val groups = tvf.map.groupBy { case (tag, pos) =>
        tag.capturingGroup
      } map { case (group, startEndMap) =>
        val start = startEndMap.find(entry => entry._1.tagType == TagType.Start).get._2
        val end = startEndMap.find(entry => entry._1.tagType == TagType.End).get._2
        CapturedGroup(group, start, end)
      }
      (true, groups.toSeq, currentPos)
    } else {
      (false, Seq(), currentPos)
    }
  }

}
