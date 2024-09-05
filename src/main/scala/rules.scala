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
    lhs.connect(l(1), l(1), "is frog")

    val lpg = PatchG[String, lhs.type](lhs)(List(
      C -> l(0)))

    val rhs = DNIELMG[String]()
    val r = rhs.newNodes(5)
    rhs.connect(r(0), r(0), "goal")
    rhs.connect(r(0), r(1), "is")
    rhs.connect(r(1), r(1), "is frog")

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
    lhs.connect(l(1), l(1), "TRUE")
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
    rhs.connect(r(1), r(1), "TRUE")
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

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4))
  }


  val example_graph = {
    // graph corresponding to X(("a" \/ ("a" /\ "b")) /\ X(G("a")))
    val g = DNIELMG[String]()
    // 0: goal, 1: fact, 2: green, 3: eats flies, 4: croaks
    val n = g.newNodes(5)
    g.connect(n(0), n(0), "goal")
    g.connect(n(1), n(1), "fact")

    g.connect(n(0), n(2), "is")
    g.connect(n(2), n(2), "green")

    g.connect(n(1), n(3), "holds")
    g.connect(n(3), n(3), "eats flies")
    g.connect(n(1), n(4), "holds")
    g.connect(n(4), n(4), "croaks")

    g
  }

import scala.sys.process.*
def plot(title: String)(body: (String => Unit) ?=> Unit): Process =
  // only works when forking process
  new java.io.PrintWriter(s"plots/$title.dot") {
    write("digraph G {\n")
    write(s"label = \"$title\"\n")
    body(using x => write(x + "\n"))
    write("}\n")
    close()
  }
  Process(s"dot -Tpng plots/$title.dot -o plots/$title.png").run()


@main def m =
  // Yeey!
  // BuchiReduction.removeSubsumed(BuchiReduction(BuchiReduction.example_graph)).plot()

//  BackwardChaining.isFrog.plot()
//  plot("mark facts") { BackwardChaining.markFacts.plot() }
//  plot("backtrackAnd") { BackwardChaining.backtrackAnd.plot() }
  plot("original") {
    BackwardChaining.example_graph.plot
  }
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
