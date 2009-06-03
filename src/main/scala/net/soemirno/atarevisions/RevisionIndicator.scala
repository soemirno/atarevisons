package net.soemirno.atarevisions


import xml._

object RevisionIndicator {
  def apply(changeElem: Node) = {
    val changeType = (changeElem \ "@chg").text
    val key = (changeElem \ "@key").text
    new RevisionIndicator(key, changeType, changeElem.asInstanceOf[Elem])
  }

  def apply(key: String, changeType: String, node: Node) = new RevisionIndicator(key, changeType, node.asInstanceOf[Elem])
}

class RevisionIndicator(key: String, changeType: String, elem: Elem) extends
      Elem(elem.prefix, elem.label, elem.attributes, elem.scope, elem.child: _*) {

  private val childElem = child.filter(n => n.isInstanceOf[Elem])

  def key(): String = key

  def changeType(): String = changeType

  def children(): NodeSeq = childElem

  override def toString(): String = key + "," + changeType + ":\n" + super.toString

  override def equals(x: Any): Boolean = x match {
    case g:Group => false
    case that: RevisionIndicator => isSame(this, that)
    case _ => false
  }

  private def isSame(thisElem: Node, thatElem: Node): Boolean = {
    val ignoreList = List("chg", "revdate", "targetrefid")

    val sameTag = ((thatElem.prefix == thisElem.prefix)
              && (thatElem.label == thisElem.label)
              && (thatElem.attributes.filter(a => !ignoreList.contains(a.key)) ==
              thisElem.attributes.filter(a => !ignoreList.contains(a.key))))

    if (!sameTag) return false

    if (thisElem.child.length == 1 && thisElem.child(0).isInstanceOf[Text] &&
            thatElem.child.length == 1 && thatElem.child(0).isInstanceOf[Text])
      return thisElem.text.trim == thatElem.text.trim

    return hasSameChildren(thatElem.child, thisElem.child)

  }

  private def hasSameChildren(a :NodeSeq, b :NodeSeq):Boolean = {
    val ita = a.filter(e => e.isInstanceOf[Elem]).elements
    val itb = b.filter(e => e.isInstanceOf[Elem]).elements
    var res = true
    while (res && ita.hasNext && itb.hasNext) {
      res = (isSame(ita.next, itb.next))
    }
    !ita.hasNext && !itb.hasNext && res
  }

}