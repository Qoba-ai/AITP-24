import be.adamv.llgraph.graphs.DNIELMG.{DNIELMG, given}
import be.adamv.llgraph.graphs.PatchG.{C, PatchG, RewriteRule}
import be.adamv.llgraph.nodeId
import be.adamv.llgraph.algorithms.MetaRewriting
import be.adamv.llgraph.<

import java.io.{File, FileOutputStream, FileWriter, PrintStream}

object BuchiReduction:
  def rewriteSameOutgoing[A](g: DNIELMG[A]): Iterator[DNIELMG[A]] =
    val lhs = DNIELMG[Char]()
    val l = lhs.newNodes(2)

    val lpg = PatchG[Char, lhs.type](lhs)(List(C -> l(0), C -> l(1), l(0) -> l(0), l(1) -> l(1), l(0) -> C, l(1) -> C))

    val rhs = DNIELMG[Char]()
    val r = rhs.newNodes(1)

    val rpg = PatchG[Char, rhs.type](rhs)(List(C -> r(0), C -> r(0), r(0) -> r(0), r(0) -> C))

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 2, 4 -> 3, 5 -> 3)).applyIt(g)


  def rewriteSameIncoming[A](g: DNIELMG[A]): Iterator[DNIELMG[A]] =
    val lhs = DNIELMG[Char]()
    val l = lhs.newNodes(2)

    val lpg = PatchG[Char, lhs.type](lhs)(List(C -> l(0), C -> l(1), l(0) -> l(0), l(1) -> l(1), l(0) -> C, l(1) -> C))

    val rhs = DNIELMG[Char]()
    val r = rhs.newNodes(1)

    val rpg = PatchG[Char, rhs.type](rhs)(List(C -> r(0), r(0) -> r(0), r(0) -> C, r(0) -> C))

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 0, 2 -> 1, 3 -> 1, 4 -> 2, 5 -> 3)).applyIt(g)


  def rewriteIntoSelf[A](g: DNIELMG[A]): Iterator[DNIELMG[A]] =
    val lhs = DNIELMG[Char]()
    val l = lhs.newNodes(2)
    lhs.connect(l(0), l(1), 'a')
    lhs.connect(l(1), l(1), 'a')

    val lpg = PatchG[Char, lhs.type](lhs)(List(l(0) -> l(1), l(1) -> l(1),
      l(1) -> l(0), l(0) -> l(0),
      C -> l(0), C -> l(1),
      l(0) -> C, l(1) -> C))

    val rhs = DNIELMG[Char]()
    val r = rhs.newNodes(1)
    rhs.connect(r(0), r(0), 'a')

    val rpg = PatchG[Char, rhs.type](rhs)(List(r(0) -> r(0), r(0) -> r(0), C -> r(0), C -> r(0), r(0) -> C))

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 0, 2 -> 1, 3 -> 1, 4 -> 2, 5 -> 3, 6 -> 4, 7 -> 4)).applyIt(g)


  def rewriteOutSelf[A](g: DNIELMG[A]): Iterator[DNIELMG[A]] =
    val lhs = DNIELMG[Char]()
    val l = lhs.newNodes(2)
    lhs.connect(l(0), l(0), 'a')
    lhs.connect(l(0), l(1), 'a')

    val lpg = PatchG[Char, lhs.type](lhs)(List(l(0) -> l(0), l(0) -> l(1),
      l(1) -> l(1), l(1) -> l(0),
      C -> l(0), C -> l(1),
      l(0) -> C, l(1) -> C))

    val rhs = DNIELMG[Char]()
    val r = rhs.newNodes(1)
    rhs.connect(r(0), r(0), 'a')

    val rpg = PatchG[Char, rhs.type](rhs)(List(r(0) -> r(0), r(0) -> r(0), C -> r(0), r(0) -> C, r(0) -> C))

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 0, 2 -> 1, 3 -> 1, 4 -> 2, 5 -> 2, 6 -> 3, 7 -> 4)).applyIt(g)

  def apply[A](g: DNIELMG[A]): DNIELMG[A] =
    MetaRewriting.Eager.winStay(g, Seq(rewriteIntoSelf, rewriteSameOutgoing, rewriteSameIncoming, rewriteOutSelf))

  def removeSubsumed[A](g: DNIELMG[Set[A]]): DNIELMG[Set[A]] =
    val g_reduced = DNIELMG[Set[A]]()
    for case ((src, tgt), labels) <- g.labeling
        label <- labels.filter(lb => !labels.exists(l => lb < l)) do
      g_reduced.foreignConnect(src, tgt, label)
    g_reduced

  val example_graph = {
    // graph corresponding to X(("a" \/ ("a" /\ "b")) /\ X(G("a")))
    val g = DNIELMG[Set[Char]]()
    val n = g.newNodes(5)
    g.connect(n(0), n(1), Set())
    g.connect(n(1), n(2), Set('a'))
    g.connect(n(1), n(4), Set('a', 'b'))
    g.connect(n(4), n(3), Set('a'))
    g.connect(n(2), n(3), Set('a'))
    g.connect(n(3), n(3), Set('a'))
    g
  }
end BuchiReduction

object BackwardChaining:
  val isFrog = {
    val lhs = DNIELMG[String]()
    val l = lhs.newNodes(2)
    lhs.connect(l(0), l(0), "goal")
    lhs.connect(l(0), l(1), "is")
    lhs.connect(l(1), l(1), "frog")

    val lpg = PatchG[String, lhs.type](lhs)(List(
      C -> l(0)))

    val rhs = DNIELMG[String]()
    val r = rhs.newNodes(5)
    rhs.connect(r(0), r(0), "goal")
    rhs.connect(r(0), r(1), "is")
    rhs.connect(r(1), r(1), "frog")

    rhs.connect(r(2), r(2), "goal")
    rhs.connect(r(2), r(3), "is")
    rhs.connect(r(3), r(3), "croaks")
    rhs.connect(r(2), r(4), "is")
    rhs.connect(r(4), r(4), "eats flies")

    rhs.connect(r(0), r(2), "subgoal")

    val rpg = PatchG[String, rhs.type](rhs)(List(
      C -> r(0)))

    RewriteRule(lpg, rpg, Set(0 -> 0))
  }

  val markFacts = {
    val lhs = DNIELMG[String]()
    val l = lhs.newNodes(2)
    lhs.connect(l(0), l(0), "goal")
    lhs.connect(l(1), l(1), "fact")

    val lpg = PatchG[String, lhs.type](lhs)(List(
      C -> l(0),
      l(0) -> C,
      C -> l(1),
      l(1) -> C,
    ))

    val rhs = DNIELMG[String]()
    val r = rhs.newNodes(2)
    rhs.connect(r(0), r(0), "fact")
    rhs.connect(r(1), r(1), "fact")

    val rpg = PatchG[String, rhs.type](rhs)(List(
      C -> r(0),
      r(0) -> C,
      C -> r(1),
      r(1) -> C,
    ))

    RewriteRule(lpg, rpg, Set(1 -> 1, 3 -> 1, 1 -> 3, 3 -> 3, 0 -> 0, 2 -> 2))
  }

  val backtrackAnd = {
    // TODO right mapping
    val lhs = DNIELMG[String]()
    val l = lhs.newNodes(5)
    lhs.connect(l(0), l(0), "and")
    lhs.connect(l(1), l(1), "true")
    lhs.connect(l(4), l(4), "unknown")

    lhs.connect(l(0), l(2), "")
    lhs.connect(l(0), l(3), "")
    lhs.connect(l(1), l(2), "")
    lhs.connect(l(1), l(3), "")

    lhs.connect(l(4), l(0), "")


    val lpg = PatchG[String, lhs.type](lhs)(List(
      l(2) -> l(2),
      l(3) -> l(3),
      l(1) -> C,
      C -> l(0),
      l(2) -> C,
      C -> l(2),
      l(3) -> C,
      C -> l(3),
      l(4) -> C
    ))

    val rhs = DNIELMG[String]()
    val r = rhs.newNodes(5)
    rhs.connect(r(0), r(0), "and")
    rhs.connect(r(1), r(1), "true")
    rhs.connect(r(4), r(4), "unknown")
    rhs.connect(r(1), r(0), "")

    rhs.connect(r(0), r(2), "")
    rhs.connect(r(1), r(2), "")
    rhs.connect(r(0), r(3), "")
    rhs.connect(r(1), r(3), "")

    val rpg = PatchG[String, rhs.type](rhs)(List(
      r(2) -> r(2),
      r(3) -> r(3),
      r(1) -> C,
      C -> r(0),
      r(2) -> C,
      C -> r(2),
      r(3) -> C,
      C -> r(3),
      r(4) -> C
    ))

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6, 7 -> 7, 8 -> 8))
  }

  val subgoal_completed = {
    val lhs = DNIELMG[String]()
    val l = lhs.newNodes(5)
    val (top, unknown, n_unknown, n_true, goal) = (0, 1, 2, 3, 4)
    lhs.connect(l(0), l(0), "true")
    lhs.connect(l(1), l(1), "unknown")
    lhs.connect(l(1), l(2), "")
    lhs.connect(l(0), l(3), "")
    lhs.connect(l(2), l(3), "subgoal")
    lhs.connect(l(4), l(4), "main goal")

    val lpg = PatchG[String, lhs.type](lhs)(List(
      l(2) -> l(2),
      l(3) -> l(3),
      l(0) -> C,
      l(1) -> C,
      C -> l(2),
      l(2) -> C,
      C -> l(3),
      l(3) -> C,
      l(4) -> C,
      l(n_true) -> C,
      l(n_unknown) -> C,
      l(goal) -> l(n_unknown)))

    val rhs = DNIELMG[String]()
    val r = rhs.newNodes(5)
    rhs.connect(r(0), r(0), "true")
    rhs.connect(r(1), r(1), "unknown")
    rhs.connect(r(0), r(2), "")
    rhs.connect(r(0), r(3), "")
    rhs.connect(r(2), r(3), "subgoal")
    rhs.connect(r(4), r(4), "main goal")

    val rpg = PatchG[String, rhs.type](rhs)(List(
      r(2) -> r(2),
      r(3) -> r(3),
      r(0) -> C,
      r(1) -> C,
      C -> r(2),
      r(2) -> C,
      C -> r(3),
      r(3) -> C,
      r(4) -> C,
      r(n_true) -> C,
      r(n_unknown) -> C,
      r(goal) -> r(n_unknown)))

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6, 7 -> 7, 8 -> 8, 9 -> 10, 10 -> 9, 9 -> 9, 10 -> 10, 11 -> 11))
  }

  val subgoal_completed_minimal = {
    val lhs = DNIELMG[String]()
    val l = lhs.newNodes(5)
    val (top, unknown, n_unknown, n_true, goal) = (0, 1, 2, 3, 4)
    lhs.connect(l(0), l(0), "true")
    lhs.connect(l(1), l(1), "unknown")
    lhs.connect(l(1), l(2), "")
    lhs.connect(l(0), l(3), "")
    lhs.connect(l(2), l(3), "subgoal")
    lhs.connect(l(4), l(4), "main goal")

    val lpg = PatchG[String, lhs.type](lhs)(List(
      l(2) -> l(2),
      l(3) -> l(3),
      l(0) -> C,
      l(1) -> C,
      C -> l(2),
      l(2) -> C,
      C -> l(3),
      l(3) -> C,
      l(4) -> C))

    val rhs = DNIELMG[String]()
    val r = rhs.newNodes(5)
    rhs.connect(r(0), r(0), "true")
    rhs.connect(r(1), r(1), "unknown")
    rhs.connect(r(0), r(2), "")
    rhs.connect(r(0), r(3), "")
    rhs.connect(r(2), r(3), "subgoal")
    rhs.connect(r(4), r(4), "main goal")

    val rpg = PatchG[String, rhs.type](rhs)(List(
      r(2) -> r(2),
      r(3) -> r(3),
      r(0) -> C,
      r(1) -> C,
      C -> r(2),
      r(2) -> C,
      C -> r(3),
      r(3) -> C,
      r(4) -> C))

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6, 7 -> 7, 8 -> 8))
  }

  val check_prop = {
    // not fully correct yet -> if prop node already has "subgoal" edge it does not work
    val lhs = DNIELMG[String]()
    val l = lhs.newNodes(5)
    val (top, unknown, n_unknown, n_true, goal) = (0, 1, 2, 3, 4)

    lhs.connect(l(top), l(top), "true")
    lhs.connect(l(unknown), l(unknown), "unknown")
    lhs.connect(l(unknown), l(n_unknown), "")
    lhs.connect(l(top), l(n_true), "")
    lhs.connect(l(4), l(4), "main goal")


    val lpg = PatchG[String, lhs.type](lhs)(List(
      l(n_unknown) -> l(n_unknown),
      l(n_true) -> l(n_true),
      l(top) -> C,
      l(unknown) -> C,
      C -> l(n_unknown),
      l(n_unknown) -> C,
      C -> l(n_true),
      l(n_true) -> C,
      l(4) -> C))

    val rhs = DNIELMG[String]()
    val r = rhs.newNodes(5)
    rhs.connect(r(top), r(top), "true")
    rhs.connect(r(unknown), r(unknown), "unknown")
    rhs.connect(r(top), r(n_true), "")
    rhs.connect(r(top), r(n_unknown), "")
    rhs.connect(r(4), r(4), "main goal")

    val rpg = PatchG[String, rhs.type](rhs)(List(
      r(n_unknown) -> r(n_unknown),
      r(n_true) -> r(n_true),
      r(top) -> C,
      r(unknown) -> C,
      C -> r(n_unknown),
      r(n_unknown) -> C,
      C -> r(n_true),
      r(n_true) -> C,
      r(4) -> C))

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1, 0 -> 1, 1 -> 0, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6, 7 -> 7, 8 -> 8, 5 -> 7, 7 -> 5))

  }

  val end_criterion = {
    val lhs = DNIELMG[String]()
    val l = lhs.newNodes(3)
    val (top, goal) = (0, 1)

    lhs.connect(l(top), l(top), "true")
    lhs.connect(l(goal), l(goal), "main goal")
    lhs.connect(l(0), l(2), "")
    lhs.connect(l(1), l(2), "")

    val lpg = PatchG[String, lhs.type](lhs)(List(
      l(2) -> l(2),
      l(0) -> C,
      l(2) -> C
    ))

    val rhs = DNIELMG[String]()
    val r = rhs.newNodes(3)

    rhs.connect(r(0), r(0), "true")
    rhs.connect(r(1), r(1), "proven")
    rhs.connect(r(0), r(2), "")
    rhs.connect(r(1), r(2), "")

    val rpg = PatchG[String, rhs.type](rhs)(List(
      r(2) -> r(2),
      r(0) -> C,
      r(2) -> C
    ))

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1, 2 -> 2))

  }


  val frogRule1 = {
    val lhs = DNIELMG[String]()
    val l = lhs.newNodes(5)
    val (current, unknown, frog, x, goal) = (0, 1, 2, 3, 4)

    lhs.connect(l(current), l(current), "current")
    lhs.connect(l(unknown), l(unknown), "unknown")
    lhs.connect(l(frog), l(frog), "frog")
    lhs.connect(l(goal), l(goal), "main goal")


    lhs.connect(l(frog), l(x), "")
    lhs.connect(l(current), l(frog), "")
    lhs.connect(l(unknown), l(frog), "")

    val lpg = PatchG[String, lhs.type](lhs)(List(
      l(x) -> l(x),
      l(unknown) -> C,
      l(frog) -> C,
      C -> l(frog),
      l(current) -> C,
      C -> l(x),
      l(goal) -> C
    ))


    val rhs = DNIELMG[String]()
    val r = rhs.newNodes(8)
    val (and, croaks, eats) = (5, 6, 7)

    rhs.connect(r(current), r(current), "current")
    rhs.connect(r(unknown), r(unknown), "unknown")
    rhs.connect(r(frog), r(frog), "frog")
    rhs.connect(r(and), r(and), "and")
    rhs.connect(r(croaks), r(croaks), "croaks")
    rhs.connect(r(eats), r(eats), "eats flies")
    rhs.connect(r(goal), r(goal), "main goal")

    rhs.connect(r(frog), r(x), "")
    rhs.connect(r(eats), r(x), "")
    rhs.connect(r(croaks), r(x), "")

    rhs.connect(r(unknown), r(frog), "")
    rhs.connect(r(unknown), r(and), "")
    rhs.connect(r(unknown), r(croaks), "")
    rhs.connect(r(unknown), r(eats), "")

    rhs.connect(r(frog), r(and), "subgoal")

    rhs.connect(r(and), r(croaks), "")
    rhs.connect(r(and), r(eats), "")

    rhs.connect(r(current), r(croaks), "")
    rhs.connect(r(current), r(eats), "")


    val rpg = PatchG[String, rhs.type](rhs)(List(
      r(x) -> r(x),
      r(unknown) -> C,
      r(frog) -> C,
      C -> r(frog),
      r(current) -> C,
      C -> r(x),
      r(goal) -> C
    ))

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6))

  }

  val frog_rule_2 = {
    val lhs = DNIELMG[String]()
    // val l = lhs.newNodes(5)
    val l = lhs.newNodes(4)
    // val (current, unknown, green, x, goal) = (0, 1, 2, 3, 4)
    val (current, unknown, green, x) = (0, 1, 2, 3)

    lhs.connect(l(current), l(current), "current")
    lhs.connect(l(unknown), l(unknown), "unknown")
    lhs.connect(l(green), l(green), "is green")
    // lhs.connect(l(goal), l(goal), "main goal")


    lhs.connect(l(green), l(x), "")
    lhs.connect(l(current), l(green), "")
    lhs.connect(l(unknown), l(green), "")

    val lpg = PatchG[String, lhs.type](lhs)(List(
      l(x) -> l(x),
      l(unknown) -> C,
      l(green) -> C,
      C -> l(green),
      l(current) -> C,
      C -> l(x),
      // l(goal) -> C
    ))

    val rhs = DNIELMG[String]()
    val r = rhs.newNodes(5)
    //val r = rhs.newNodes(6)
    val (frog) = (4)
    //val (frog) = (5)

    rhs.connect(r(current), r(current), "current")
    rhs.connect(r(unknown), r(unknown), "unknown")
    rhs.connect(r(frog), r(frog), "frog")
    rhs.connect(r(green), r(green), "is green")
    // rhs.connect(r(goal), r(goal), "main goal")

    rhs.connect(r(frog), r(x), "")
    rhs.connect(r(green), r(x), "")

    rhs.connect(r(unknown), r(frog), "")
    rhs.connect(r(unknown), r(green), "")

    rhs.connect(r(green), r(frog), "subgoal")

    rhs.connect(r(current), r(frog), "")

    val rpg = PatchG[String, rhs.type](rhs)(List(
      r(x) -> r(x),
      r(unknown) -> C,
      r(green) -> C,
      C -> r(green),
      r(current) -> C,
      C -> r(x),
      // r(goal) -> C
    ))

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5))


  }


  val example_graph = {
    val g = DNIELMG[String]()
    val n = g.newNodes(8)
    val (goal, fact, green, eats, croaks, fritz, current, unknown) = (0, 1, 2, 3, 4, 5, 6, 7)
    g.connect(n(0), n(0), "main goal")
    g.connect(n(1), n(1), "true")
    g.connect(n(unknown), n(unknown), "unknown")

    g.connect(n(0), n(2), "")
    g.connect(n(2), n(2), "is green")

    g.connect(n(unknown), n(green), "")

    g.connect(n(1), n(3), "")
    g.connect(n(3), n(3), "eats flies")
    g.connect(n(1), n(4), "")
    g.connect(n(4), n(4), "croaks")

    g.connect(n(current), n(current), "current")
    g.connect(n(fritz), n(fritz), "fritz")
    g.connect(n(green), n(fritz), "")
    g.connect(n(eats), n(fritz), "")
    g.connect(n(croaks), n(fritz), "")
    g.connect(n(current), n(green), "")


    g
  }

object bugExamples:
  val example_graph_edges_merged = {
    val g = DNIELMG[String]()
    val n = g.newNodes(12)

    val (top, eats_t, croaks_t, unknown, eats_u, croaks_u, x, goal, and, current, green, frog) = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

    g.connect(n(top), n(top), "true")
    g.connect(n(and), n(and), "and")
    g.connect(n(unknown), n(unknown), "unknown")
    g.connect(n(goal), n(goal), "main goal")
    g.connect(n(x), n(x), "x")
    g.connect(n(current), n(current), "current")
    g.connect(n(green), n(green), "is green")
    g.connect(n(frog), n(frog), "frog")

    g.connect(n(green), n(x), "")
    g.connect(n(frog), n(x), "")
    g.connect(n(goal), n(green), "")

    g.connect(n(green), n(frog), "subgoal")
    g.connect(n(frog), n(and), "subgoal")

    g.connect(n(eats_t), n(eats_t), "eats flies")
    g.connect(n(eats_u), n(eats_u), "eats flies")
    g.connect(n(croaks_t), n(croaks_t), "croaks")
    g.connect(n(croaks_u), n(croaks_u), "croaks")

    g.connect(n(croaks_t), n(x), "")
    g.connect(n(croaks_u), n(x), "")
    g.connect(n(eats_u), n(x), "")
    g.connect(n(eats_t), n(x), "")

    g.connect(n(top), n(eats_t), "")
    g.connect(n(top), n(croaks_t), "")
    g.connect(n(top), n(eats_u), "")
    g.connect(n(unknown), n(croaks_u), "")
    g.connect(n(unknown), n(and), "")
    g.connect(n(unknown), n(frog), "")
    g.connect(n(unknown), n(green), "")

    g.connect(n(and), n(eats_u), "")
    g.connect(n(and), n(croaks_u), "")

    g.connect(n(current), n(croaks_u), "")
    g.connect(n(current), n(eats_u), "")


    g
  }

  val before_subgoal_completed = {
    val g = DNIELMG[String]()
    val n = g.newNodes(12)

    val (top, eats_t, croaks_t, unknown, eats_u, croaks_u, x, goal, and, current, green, frog) = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

    g.connect(n(top), n(top), "true")
    g.connect(n(and), n(and), "and")
    g.connect(n(unknown), n(unknown), "unknown")
    g.connect(n(goal), n(goal), "main goal")
    g.connect(n(x), n(x), "x")
    g.connect(n(current), n(current), "current")
    g.connect(n(green), n(green), "is green")
    g.connect(n(frog), n(frog), "frog")

    g.connect(n(green), n(x), "")
    g.connect(n(frog), n(x), "")
    g.connect(n(goal), n(green), "")

    g.connect(n(green), n(frog), "subgoal")
    g.connect(n(frog), n(and), "subgoal")

    g.connect(n(eats_t), n(eats_t), "eats flies")
    g.connect(n(eats_u), n(eats_u), "eats flies")
    g.connect(n(croaks_t), n(croaks_t), "croaks")
    g.connect(n(croaks_u), n(croaks_u), "croaks")

    g.connect(n(croaks_t), n(x), "")
    g.connect(n(croaks_u), n(x), "")
    g.connect(n(eats_u), n(x), "")
    g.connect(n(eats_t), n(x), "")

    g.connect(n(top), n(eats_t), "")
    g.connect(n(top), n(croaks_t), "")
    g.connect(n(top), n(eats_u), "")
    g.connect(n(top), n(croaks_u), "")
    g.connect(n(top), n(and), "")

    g.connect(n(unknown), n(frog), "")
    g.connect(n(unknown), n(green), "")

    g.connect(n(and), n(eats_u), "")
    g.connect(n(and), n(croaks_u), "")

    g.connect(n(current), n(croaks_u), "")
    g.connect(n(current), n(eats_u), "")


    g
  }

import scala.sys.process.*
def plot(title: String)(body: (String => Unit) ?=> Unit): Process =
  val safer_title = title.replace(' ', '_')
  // only works when forking process
  new java.io.PrintWriter(s"plots/$safer_title.dot") {
    write("digraph G {\n")
    write(s"label = \"$title\"\n")
    body(using x => write(x + "\n"))
    write("}\n")
    close()
  }
  Process(s"dot -Tpng plots/$safer_title.dot -o plots/$safer_title.png").run()


@main def m =
  // Yeey!
  // BuchiReduction.removeSubsumed(BuchiReduction(BuchiReduction.example_graph)).plot()

//  BackwardChaining.isFrog.plot()
//  plot("mark facts") { BackwardChaining.markFacts.plot() }
//  plot("backtrackAnd") { BackwardChaining.backtrackAnd.plot() }
//  plot("original") {
//    BackwardChaining.example_graph.plot
//  }

  import be.adamv.llgraph.graphs.DNIELMG.DSLUnsound.{*, given}
  val g = {
    val g = DNIELMG[String]()
    given g.type = g

    val List(goal, fact, green, eats_flies, croaks) = g.newNodes(5)

    goal ("goal")-> goal
    fact ("goal")-> fact
    goal ("green")-> green
    eats_flies ("eats flies")-> eats_flies
    croaks ("croaks")-> croaks

    goal ("is")-> green
    fact ("holds")-> eats_flies
    fact ("holds")-> croaks

    g
  }
  plot("croaks pretty") { g.plot }
  g.labelMap(_.mkString("\"", "", "\"")).showDSL
  plot("croaks pretty transformed") { g.labelMap(_.reverse).plot }
  //  BackwardChaining.isFrog.plot()
  //  plot("mark facts") { BackwardChaining.markFacts.plot() }
  //  plot("backtrackAnd") { BackwardChaining.backtrackAnd.plot }
  plot("subgoalCompleted") { BackwardChaining.subgoal_completed.plot }
  plot("subgoalCompletedMinimal") { BackwardChaining.subgoal_completed_minimal.plot }
  plot("checkProposition") { BackwardChaining.check_prop.plot }
  //  plot("endCriterion") { BackwardChaining.end_criterion.plot }
  plot("frogRule1") { BackwardChaining.frogRule1.plot }
  plot("frogRule2") { BackwardChaining.frog_rule_2.plot }
  plot("original") { BackwardChaining.example_graph.plot }
  //  plot("mock") { BackwardChaining.mock_rule.plot }
  plot("mock") { bugExamples.example_graph_edges_merged.plot }
  val m1 = BackwardChaining.check_prop.applyItLabeled(bugExamples.example_graph_edges_merged).next()
  val m2 = BackwardChaining.backtrackAnd.applyItLabeled(m1).next()
  plot("mock_id") { m1.plot }
  plot("mock_id2") { m2.plot }
  //  plot("oneStepMock") { BackwardChaining.mock_rule.applyItLabeled(BackwardChaining.example_graph).next().plot }
  val s1 = BackwardChaining.frog_rule_2.applyItLabeled(BackwardChaining.example_graph).next()
  val s2 = BackwardChaining.frogRule1.applyItLabeled(s1).next()
  val mock_3 = BackwardChaining.check_prop.applyItLabeled(s2)
  val s3 = mock_3.next()
  val s3b = mock_3.next()
  val mock_ids = BackwardChaining.check_prop.applyItLabeled(s3b)
  val s4 = mock_ids.next()
  val s4cheat = BackwardChaining.check_prop.applyItLabeled(bugExamples.example_graph_edges_merged).next()
  val s5 = BackwardChaining.backtrackAnd.applyItLabeled(s4cheat).next()
  val s6 = BackwardChaining.subgoal_completed_minimal.applyItLabeled(s5).next()
  //  val s7 = BackwardChaining.subgoal_completed.applyItLabeled(s6).next()
  //  val s8 = BackwardChaining.end_criterion.applyItLabeled(s7).next()
  //  val s5 = BackwardChaining.backtrackAnd.applyItLabeled(s4).next()
  plot("FrogStep1") { BackwardChaining.frog_rule_2.applyItLabeled(BackwardChaining.example_graph).next().plot }
  plot("FrogStep2") { s2.plot }
  plot("FrogStep3") { s3b.plot }
  plot("FrogStep4") { s4.plot }
  plot("FrogStep4Cheat") { s4cheat.plot }
  plot("FrogStep5Cheat") { s5.plot }
  plot("FrogStep6Cheat") { s6.plot }
  //  plot("FrogStep7Cheat") { s7.plot }
  //  plot("FrogStep8Cheat") { s8.plot }

  val test_context = BackwardChaining.subgoal_completed_minimal.applyItLabeled(bugExamples.before_subgoal_completed).next()

//  for case (g, i) <- BackwardChaining.markFacts.applyIt(BackwardChaining.example_graph).zipWithIndex do
//    println(i)
//    plot(s"markFacts $i") {
//      g.plot()
//    }
//    g.plot()
//  plot("mark facts") {
//    BackwardChaining.markFacts.applyIt(BackwardChaining.example_graph).next().plot()
//  }
//  BackwardChaining.example_graph.plot()
