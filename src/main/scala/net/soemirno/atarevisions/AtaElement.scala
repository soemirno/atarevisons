package net.soemirno.atarevisions

import collection.mutable.HashMap
import java.io.File
import xml.{NodeSeq, Node, Elem}
object AtaElement {
  def apply(elem: Elem) = {
    new AtaElement(elem)
  }

  def apply(sourceFile: File) = {
    val document: Elem = {
      if (isValid(sourceFile))
        xml.XML loadFile (sourceFile)
      else
        throw new IllegalArgumentException("use valid xml file")
    }
    new AtaElement(document)
  }

  def isValid(sourceFile: File): Boolean = {
    if (sourceFile == null) return false
    sourceFile exists
  }

}

class AtaElement(elem: Elem) {

  private val revs = {
    val keyedElems = (elem \\ "_") filter (element => element \ "@chg" != "")
    val list = new RevisionIndicators
    for (element <- keyedElems)
      list add (RevisionIndicator(element))
    list
  }

  private val visitedList = new RevisionIndicators

  def element(): Elem = elem

  def revisionIndicators(): RevisionIndicators = revs

  def diff(other: AtaElement, revisionDate: String): RevisionIndicators = {
    val prevChanges = other.revisionIndicators
    for (rev <- revs.values)
      compare(rev, prevChanges, revisionDate)
    return visitedList
  }

  def compare(rev: RevisionIndicator, prevChanges: RevisionIndicators, revisionDate: String): Option[RevisionIndicator] = {
    if (visitedList.contains(rev.key)) return None

    Console.println("comparing " + rev.key)

    if (!prevChanges.contains(rev.key()) || prevChanges(rev.key()).changeType == "D") {
      val change = Some(RevisionIndicator(rev.key, "N", revisionDate, rev.element))
      visitedList add change
      return change
    }
    else if (rev.element.child.filter(n => n.isInstanceOf[Elem]).size !=
            prevChanges(rev.key).element.child.filter(n => n.isInstanceOf[Elem]).size) {
      val change = Some(RevisionIndicator(rev.key, "R", revisionDate, rev.element))
      visitedList add change
      return change
    } else {
      if (childHasChanged(rev, prevChanges, revisionDate)) {
        val change = Some(RevisionIndicator(rev.key, "R", revisionDate, rev.element))
        visitedList add change
        return change
      }
      else {
        return None
      }
    }

  }

  def childHasChanged(rev: RevisionIndicator, prevChanges: RevisionIndicators, revisionDate: String): Boolean = {
    var hasChanged = false
    var prevParent = prevChanges(rev.key).element
    for (child <- rev.element.child.filter(n => n.isInstanceOf[Elem])) {
      if (child \ "@chg" != "") {
        val key = (child \ "@key").text
        val childRev = revs(key)
        val change: Option[RevisionIndicator] = compare(childRev, prevChanges, revisionDate)
        visitedList add change

        if (!hasChanged) {
          change match {
            case Some(rei) => {
              if (rei.changeType == "N" || rei.changeType == "R") hasChanged = true
            }
            case None =>
          }
        }
      } else {
        if (!hasChanged) {
          val currentText = (rev.element \ child.label).text.trim
          val prevText = (prevParent \ child.label).text.trim
//          Console.println("number of same elements for " + rev.key + ": " + child.label + ":  " + (prevParent \ child.label).size)
          if (currentText != prevText) {
            val change = Some(RevisionIndicator(rev.key, "R", revisionDate, rev.element))
            visitedList add change
            hasChanged = true
          }
        }
      }

    }
    return hasChanged

  }

}