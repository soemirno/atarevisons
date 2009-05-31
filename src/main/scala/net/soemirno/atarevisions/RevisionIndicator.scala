package net.soemirno.atarevisions


import xml.{Elem, Node}

object RevisionIndicator {
  def apply(changeElem: Node) = {
    val changeType = (changeElem \ "@chg").text
    val revisionDate = (changeElem \ "@revdate").text
    val key = (changeElem \ "@key").text
    new RevisionIndicator(key, changeType, revisionDate, changeElem.asInstanceOf[Elem])
  }

  def apply(key: String, changeType: String, date: String, node: Node) = new RevisionIndicator(key, changeType, date, node.asInstanceOf[Elem])
}

class RevisionIndicator(key: String, changeType: String, date: String, elem: Elem) {

  def element(): Elem = elem
  
  def key(): String = key

  def changeType(): String = changeType

  def date(): String = date

  override def toString(): String = key + "," + changeType + "," + date
}