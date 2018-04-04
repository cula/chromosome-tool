import scalax.collection.Graph
import scalax.collection.edge.WDiEdge

object Utils {
  def getMaxMatchLength(s: String, t: String, minMatchLength: Int): Int = {
    if (minMatchLength > s.length || minMatchLength > t.length)
      return 0
    val minSubStr = t.slice(0, minMatchLength)
    val matched = minSubStr.r.findAllMatchIn(s).map(_.start).filter(i => {
      val matchLength = s.length - i
      if (matchLength < minMatchLength)
        false
      else if (s.slice(i, s.length) == t.slice(0, matchLength))
        true
      else
        false
    }).map(s.length - _)
    if (matched.isEmpty) 0 else matched.max
  }
}
