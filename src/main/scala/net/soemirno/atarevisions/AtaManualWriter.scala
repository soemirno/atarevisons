package net.soemirno.atarevisions

import xml._
import xml.transform.{RewriteRule, BasicTransformer}
import collection.mutable.Map

object AtaManualWriter {

  def updateRevisionIndicators(document: Node, revisionMap: Map[String, RevisionIndicator]): Elem = {

    val updateRevisionIndicators = new RewriteRule {
      override def transform(n: Node) = n match {
        case e: Elem if ((e \ "@chg").text != "") => {

          val key = (e \ "@key").text
          val change = new UnprefixedAttribute("chg", revisionMap(key).changeType, xml.Null)
          val revdate = new UnprefixedAttribute("revdate", revisionMap(key).revdate, xml.Null)
          val attributes = e.attributes.
                  remove("chg").
                  remove("revdate").
                  append(change).
                  append(revdate)

          Elem(e.prefix, e.label, attributes, e.scope, e.child: _*)
        }

        case _ => n
      }
    }

    val n = new CustomRuleTransformer(updateRevisionIndicators).transform(document)
    Elem(document.prefix, document.label, document.attributes, document.scope, n.first.child.toList: _*)

  }
}
class CustomRuleTransformer(rules: RewriteRule*) extends BasicTransformer {
  override def transform(n: Node): Seq[Node] = {
    var m: Seq[Node] = super.transform(n)
    val it = rules.elements;
    while (it.hasNext) {
      val rule = it.next
      val m2 = rule.transform(m)
      m = m2
    }
    m
  }

  /**Returns a new node buffer with the first <code>pos</code> elements
   *  from <code>ns</code>.
   *
   * @param pos..
   * @param ns..
   * @return..
   */
  override protected def buffer(pos: Int, ns: Seq[Node]): NodeBuffer = {
    val nb = new NodeBuffer()
    var jt = ns.elements
    var j = 0;
    while (j < pos) {
      nb.append(jt.next)
      j += 1
    }
    nb
  }

}

 
