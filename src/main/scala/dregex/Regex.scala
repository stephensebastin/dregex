package dregex

import dregex.impl.RegexParser
import dregex.impl.Dfa
import com.typesafe.scalalogging.slf4j.StrictLogging
import dregex.impl.Util
import dregex.impl.RegexTree

/**
 * A regular expression, ready to be tested against strings, or to take part in an operation against another.
 * Internally, instances of this type have a DFA (Deterministic Finite Automaton).
 */
trait Regex extends StrictLogging {

  def dfa: Dfa
  def universe: Universe

  private def checkUniverse(other: Regex): Unit = {
    if (other.universe != universe)
      throw new Exception("cannot make operations between regex from different universes")
  }

  /**
   * Return whether a string is matched by the regular expression (i.e. whether the string is included in the language
   * generated by the expression).
   * As the match is done using a DFA, its complexity is O(n), where n is the length of the string. It is constant
   * with respect to the length of the expression.
   */
  def matches(string: String): Boolean = {
    val (result, _) = matchAndReport(string)
    result
  }

  /**
   * Similar to method [[matches]], except that also return how many characters were successfully matched in case of
   * failure.
   */
  def matchAndReport(string: String): (Boolean, Int) = {
    val genDfa = dfa.impl
    var current = genDfa.initial
    var i = 0
    for (char <- string) {
      val currentTrans = genDfa.defTransitions.getOrElse(current, Map())
      val litChar = RegexTree.Lit(char)
      val effChar = if (universe.alphabet.contains(litChar))
        litChar
      else
        RegexTree.Other
      current = currentTrans.get(effChar) match {
        case Some(newState) => newState
        case None => return (false, i)
      }
      i += 1
    }
    (genDfa.accepting.contains(current), i)
  }

  /**
   * Intersect this regular expression with another. The resulting expression will match the strings that are
   * matched by the operands, and only those. Intersections take O(n*m) time, where n and m are the number of states of
   * the DFA of the operands.
   */
  def intersect(other: Regex): Regex = {
    val (res, time) = Util.time {
      checkUniverse(other)
      new SynteticRegex(dfa intersect other.dfa, universe)
    }
    logger.debug(s"$this and $other intersected in $time")
    res
  }

  /**
   * Subtract other regular expression from this one. The resulting expression will match the strings that are
   * matched this expression and are not matched by the other, and only those. Differences take O(n*m) time, where n
   * and m are the number of states of the DFA of the operands.
   */
  def diff(other: Regex): Regex = {
    val (res, time) = Util.time {
      checkUniverse(other)
      new SynteticRegex(dfa diff other.dfa, universe)
    }
    logger.debug(s"$this and $other diffed in $time")
    res
  }

  /**
   * Unite this regular expression with another. The resulting expression will match the strings that are matched by
   * either of the operands, and only those. Unions take O(n*m) time, where n and m are the number of states of the DFA
   * of the operands.
   */
  def union(other: Regex): Regex = {
    val (res, time) = Util.time {
      checkUniverse(other)
      new SynteticRegex(dfa union other.dfa, universe)
    }
    logger.debug(s"$this and $other unioned in $time")
    res
  }

  /**
   * Return whether this expression matches at least one string in common with another. Intersections take O(n*m) time,
   * where n and m are the number of states of the DFA of the operands.
   */
  def doIntersect(other: Regex): Boolean = intersect(other).matchesAnything()

  def isSubsetOf(other: Regex): Boolean = !(this diff other matchesAnything)

  def isProperSubsetOf(other: Regex): Boolean = (this isSubsetOf other) && (other diff this matchesAnything)

  /**
   * Return whether this regular expression is equivalent to other. Two regular expressions are equivalent if they
   * match exactly the same set of strings. This operation takes O(n*m) time, where n and m are the number of states of
   * the DFA of the operands.
   */
  def equiv(other: Regex): Boolean = {
    checkUniverse(other)
    !(dfa diff other.dfa).matchesAnything() && !(other.dfa diff dfa).matchesAnything()
  }

  /**
   * Return whether this regular expression matches anything. Note that the empty string is a valid match.
   */
  def matchesAnything() = dfa.matchesAnything()

}

object Regex extends StrictLogging {

  def parse(regex: String): ParsedRegex = {
    val (parsedRegex, time) = Util.time {
      new ParsedRegex(RegexParser.parse(regex))
    }
    // log parsing time as trace, because this is usually quite fast
    logger.trace(s"regex [$regex] parsed in $time")
    parsedRegex
  }

  def compile(regex: String): CompiledRegex = {
    val tree = parse(regex)
    val (compiled, time) = Util.time {
      new CompiledRegex(regex, tree, new Universe(Seq(tree)))
    }
    logger.debug(s"$compiled compiled in $time")
    compiled
  }

  def compileParsed(originalString: String, tree: ParsedRegex, universe: Universe): CompiledRegex = {
    val (compiled, time) = Util.time {
      new CompiledRegex(originalString, tree, universe)
    }
    logger.debug(s"$compiled compiled in $time")
    compiled
  }

  def compile(regexs: Seq[String]): Seq[(String, CompiledRegex)] = {
    val trees = regexs.map(r => (r, parse(r)))
    val universe = new Universe(trees.unzip._2)
    for ((regex, tree) <- trees) yield {
      val (res, time) = Util.time {
        regex -> new CompiledRegex(regex, tree, universe)
      }
      logger.debug(s"${res._2} compiled in $time")
      res
    }
  }

  /**
   * Create a regular expression that does not match anything. Note that that is different from matching the empty
   * string. Despite the theoretical equivalence of automata and regular expressions, in practice there is no regular
   * expression that does not match anything.
   */
  def nullRegex(u: Universe) = new SynteticRegex(Dfa.NothingDfa, u)

}
