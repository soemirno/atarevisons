package net.soemirno.atarevisions

import collection.mutable.HashMap

class ChangeList extends HashMap[String, Change] {

  def add(change: Change) = {
    val key = change.key().asInstanceOf[String]
    val value  = change.asInstanceOf[Change]
    update(key, value)
  }
}