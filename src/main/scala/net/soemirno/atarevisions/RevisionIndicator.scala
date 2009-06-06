package net.soemirno.atarevisions

import xml._
import collection.mutable.Map
import transform.RewriteRule

/**
 *  responsible for tracking changes on elements
 */
class RevisionIndicator(key: String, changeType: String, date: String, elem: Elem) extends
      Elem(elem.prefix, elem.label, elem.attributes, elem.scope, elem.child: _*) {

  def this(node: Node, noRevdate :Boolean) = {
    this((node\ "@key").text, (node \"@chg").text,  if (noRevdate ) "" else (node \"@revdate").text, node.asInstanceOf[Elem])
  }

  def revdate() : String = date
  
  def key(): String = key

  def changeType(): String = changeType

  def writeToFile(fileName :String, updatedRevisionIndicators: Map[String, RevisionIndicator]) = {
    if (updatedRevisionIndicators.isEmpty)
      scala.xml.XML.saveFull(fileName, this, "UTF-8", true, null)
    else
      scala.xml.XML.saveFull(fileName, updateRevisionIndicators(updatedRevisionIndicators), "UTF-8", true, null)
  }

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

  /**
   * updates all chg and revdate attributes with values from revisionMap
   */
  def updateRevisionIndicators(revisionMap: Map[String, RevisionIndicator]): Elem = {

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
    val n = new CustomRuleTransformer(updateRevisionIndicators).transform(this)
    Elem(this.prefix, this.label, this.attributes, this.scope, n.first.child.toList: _*)

  }

}