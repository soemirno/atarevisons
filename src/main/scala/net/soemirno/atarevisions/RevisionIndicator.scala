package net.soemirno.atarevisions

import xml._

/**
 *  responsible for tracking changes on elements
 */
class RevisionIndicator(key: String, changeType: String, elem: Elem) extends
      Elem(elem.prefix, elem.label, elem.attributes, elem.scope, elem.child: _*) {

  def this(node: Node) = {
    this((node\ "@key").text, (node \"@chg").text, node.asInstanceOf[Elem])
  }
  
  def key(): String = key

  def changeType(): String = changeType

  /**
   *  return true if actual content is the same (ignore some metadata and whitespace)
   */
  def isSameAs(other: RevisionIndicator): Boolean = isSame(this, other)

  /**
   * helper method so we can recursively compare elements
   */
  private def isSame(thisElem: Node, thatElem: Node): Boolean = {

    if (!isSameTag(thisElem, thatElem)) return false

    if (hasNoChildren (thisElem, thatElem)) return thisElem.text.trim == thatElem.text.trim

    return hasSameChildren(thisElem, thatElem)

  }

  private def hasNoChildren(thisElem :Node, thatElem :Node) :Boolean = {
      return thisElem.child.length == 1 && thisElem.child(0).isInstanceOf[Text] &&
              thatElem.child.length == 1 && thatElem.child(0).isInstanceOf[Text]
  }

  private def isSameTag(thisElem :Node, thatElem :Node) :Boolean = {
    val ignoreList = List("chg", "revdate", "targetrefid")

    return ((thatElem.prefix == thisElem.prefix)
            && (thatElem.label == thisElem.label)
            && (thatElem.attributes.filter(a => !ignoreList.contains(a.key)) ==
                thisElem.attributes.filter(a => !ignoreList.contains(a.key))))
    
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