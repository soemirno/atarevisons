package net.soemirno.atarevisions


import xml._

object RevisionIndicator {

  def apply(changeElem: Node) = {
    val changeType = (changeElem \ "@chg").text
    val key = (changeElem \ "@key").text
    new RevisionIndicator(key, changeType, changeElem.asInstanceOf[Elem])
  }

  def apply(changeType: String, ri: RevisionIndicator) = new RevisionIndicator(ri.key, changeType, ri)
}

class RevisionIndicator(key: String, changeType: String, elem: Elem) extends
      Elem(elem.prefix, elem.label, elem.attributes, elem.scope, elem.child: _*) {

  def key(): String = key

  def changeType(): String = changeType

  override def toString(): String = key + "," + changeType + ":\n" + super.toString

  def isSameAs(other: RevisionIndicator): Boolean = isSame(this, other)

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

    return hasSameChildren(thatElem, thisElem)

  }

  private def hasSameChildren(a :Node, b :Node):Boolean = {
    val ita = a.child.filter(e => e.isInstanceOf[Elem]).elements
    val itb = b.child.filter(e => e.isInstanceOf[Elem]).elements
    var res = true
    while (res && ita.hasNext && itb.hasNext) {
      res = (isSame(ita.next, itb.next))
    }
    !ita.hasNext && !itb.hasNext && res
  }

}