package net.soemirno.atarevisions

import xml.Node

object Change {

  def apply(changeElem: Node) = {
    val changeType = (changeElem \ "@chg").text
    val revisionDate = (changeElem \ "@revdate").text
    val key = (changeElem \ "@key").text
    new Change(key, changeType, revisionDate)
  }

  def apply(key: String, changeType: String, date: String) = new Change(key, changeType, date)
}

class Change(key : String, changeType: String, date: String) {

  def key(): String = key
  
  def changeType(): String = changeType

  def date(): String = date

  override def toString(): String =  key + " = " + changeType + "," + date
}