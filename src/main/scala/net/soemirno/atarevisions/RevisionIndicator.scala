package net.soemirno.atarevisions

import xml.Node

object RevisionIndicator {
  def apply(changeElem: Node) = {
    val changeType = (changeElem \ "@chg").text
    val revisionDate = (changeElem \ "@revdate").text
    val key = (changeElem \ "@key").text
    new RevisionIndicator(key, changeType, revisionDate)
  }

  def apply(key: String, changeType: String, date: String) = new RevisionIndicator(key, changeType, date)
}

class RevisionIndicator(key: String, changeType: String, date: String) {
  def key(): String = key

  def changeType(): String = changeType

  def date(): String = date

  override def toString(): String = key + " = " + changeType + "," + date
}