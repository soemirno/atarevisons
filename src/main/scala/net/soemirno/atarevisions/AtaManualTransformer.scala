package net.soemirno.atarevisions

import xml._
import xml.transform.{RewriteRule, BasicTransformer}

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

 
