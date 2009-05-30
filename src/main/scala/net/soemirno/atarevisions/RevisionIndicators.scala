package net.soemirno.atarevisions

import collection.mutable.HashMap

class RevisionIndicators extends HashMap[String, RevisionIndicator] {

  def add(change: Option[RevisionIndicator]){
    change match {
      case Some(reI) => add(reI)
      case None =>
    }
  }

  def add(change: RevisionIndicator) = {
    val key = change.key().asInstanceOf[String]
    val value = change.asInstanceOf[RevisionIndicator]
    update(key, value)
  }
}