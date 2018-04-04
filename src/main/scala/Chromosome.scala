import scalax.collection.mutable.Graph
import scalax.collection.edge.WDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._

object Chromosome {

  val CHROMOSOME_CHARACTERS = Set('A', 'C', 'G', 'T')

  def isStrictChromosome(fragments: Seq[String]): Boolean =
    fragments != null && fragments.toSet.forall(_.toUpperCase.toSet.subsetOf(CHROMOSOME_CHARACTERS))
}

class StrictChromosome(iFragmentSeq: Seq[String]) {

  import Chromosome._

  protected val fragments: Set[String] = {
    if (!isStrictChromosome(iFragmentSeq))
      throw IllegalChromosome()
    if (iFragmentSeq == null) Set.empty else iFragmentSeq.map(_.toUpperCase).toSet
  }

  protected val commonLength: Int = if (fragments.isEmpty) 0 else fragments.map(_.length).max
  protected val tail: Option[String] = {
    val tails = fragments.filter(_.length != commonLength)
    tails.size match {
      case 0 => None
      case 1 => Option(tails.head)
      case _ => throw IllegalChromosome("Strict chromosome set should have at most one chromosome which is shorter than the others.")
    }
  }

  protected val graphStore: Graph[String, WDiEdge] = {
    val g = Graph[String, WDiEdge]()
    fragments.size match {
      case 0 =>
      case 1 => g.add(fragments.head)
      case _ =>
        val start = System.currentTimeMillis
        fragments.toList.combinations(2).foreach(c => {
          List(c, c.reverse).foreach(i => {
            val minMatchLength = (i.last.length / 2.0).round.toInt
            val maxMatchLength = Utils.getMaxMatchLength(i.head, i.last, minMatchLength)
            if (maxMatchLength >= minMatchLength && !(tail.isDefined && i.head == tail.get))
              g.add(i.last ~> i.head % maxMatchLength)
          })
        })
        val end = System.currentTimeMillis
    }
    g
  }

  private def path2String(path: List[graphStore.NodeT]): String = {
    var res = path.head.value
    for (i <- 0 until path.size - 1) {
      val source = path(i)
      val target = path(i + 1)
      // This must always be found instead of option
      val edge = source.outgoingTo(target).head
      val overlapLength = edge.weight.toInt
      res = target.value.slice(0, target.value.length - overlapLength) + res
    }
    res
  }

  private def DFS(start: graphStore.NodeT): List[graphStore.NodeT] = {
    def DFS0(v: graphStore.NodeT, visited: List[graphStore.NodeT]): List[graphStore.NodeT] = {
      if (visited.contains(v))
        visited
      else {
        val availableNeighbors = v.diSuccessors -- visited.toSet
        availableNeighbors.foldLeft(v :: visited)((b,a) => DFS0(a,b))
      }
    }

    DFS0(start,List()).reverse
  }

  def reassemble(): String = {
    fragments.size match {
      case 0 => ""
      case 1 => fragments.head
      case _ =>
        tail match {
          case Some(x) =>
            val tailNode = graphStore.get(x)
            val path = DFS(tailNode)
            if (path.size != fragments.size)
              throw NoReassembleWay()
            path2String(path)
          case None =>
            graphStore.nodes.foreach(n => {
              val path = DFS(n)
              if (path.size == fragments.size)
                return path2String(path)
            })
            throw NoReassembleWay()
        }
    }
  }
}
