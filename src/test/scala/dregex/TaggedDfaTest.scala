package dregex

import org.scalatest.FunSuite
import com.typesafe.scalalogging.StrictLogging
import dregex.impl.Compiler
import dregex.impl.Nfa
import dregex.impl.SimpleState
import dregex.impl.TaggedDfaAlgorithms
import dregex.impl.TaggedDfaAlgorithms.CapturedGroup

import scala.collection.immutable.Seq

class TaggedDfaTest extends FunSuite with StrictLogging {

  def testTaggedRegex(regex: String, string: String): collection.Seq[CapturedGroup] = {
    val tree = Regex.parse(regex)
    val universe = new Universe(Seq(tree))
    val compiler = new Compiler(universe.alphabet)
    val initial = new SimpleState
    val accepting = new SimpleState
    val transitions = compiler.fromTreeImpl(tree.tree, initial, accepting)
    val nfa = Nfa(initial, transitions, Set(accepting))
    println("NFA:\n" + nfa)
    val taggedDfa = TaggedDfaAlgorithms.fromNfa(nfa)
    println("DFA:\n" + taggedDfa)
    TaggedDfaAlgorithms.matchString(taggedDfa, string)._2
  }

  test("from regex") {
    assert(testTaggedRegex("([a-z]+)[0-9]*", "sfsdfs9").exists(cg => cg.start == 0 && cg.end == 6))
    assert(testTaggedRegex("([a-z]+)[0-9]*", "sfsdfs").exists(cg => cg.start == 0 && cg.end == 6))
    val capture = testTaggedRegex("(([a-z]+)[0-9]+([a-z]*))", "z1dfs")
    assert(capture.exists(cg => cg.start == 0 && cg.end == 1))
    assert(capture.exists(cg => cg.start == 2 && cg.end == 5))
    //println(testTaggedRegex("(a*)[a-b]*", "aa"))
    println(testTaggedRegex("(a*).*", "aa"))
  }

}