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
    findNew(prevChanges, revisionDate)
    findChangedLength(prevChanges, revisionDate)
    findChangedInChildren(prevChanges, revisionDate)
    return visitedList
  }

  def findNew(prevChanges: RevisionIndicators, revisionDate: String): Unit = {
    for (rev <- revs.values if !visitedList.contains(rev.key)) {
      Console.println("comparing new " + rev.key)
      if (!prevChanges.contains(rev.key()) || prevChanges(rev.key()).changeType == "D")
        visitedList add Some(RevisionIndicator(rev.key, "N", revisionDate, rev.element))
    }
  }

  def findChangedLength(prevChanges: RevisionIndicators, revisionDate: String): Unit = {
    for (rev <- revs.values if !visitedList.contains(rev.key)) {
      Console.println("comparing length " + rev.key)

      val currentLength = rev.children.size
      val previousLenght = prevChanges(rev.key).children.size

      if (currentLength != previousLenght)
        visitedList add Some(RevisionIndicator(rev.key, "R", revisionDate, rev.element))

    }
  }


  def findChangedInChildren(prevChanges: RevisionIndicators, revisionDate: String): Unit = {
    for (rev <- revs.values if !visitedList.contains(rev.key)) {
      Console.println("comparing children " + rev.key)
      if (childHasChanged(rev, prevChanges, revisionDate))
        visitedList add Some(RevisionIndicator(rev.key, "R", revisionDate, rev.element))
    }
  }

  def childHasChanged(parent: RevisionIndicator, prevChanges: RevisionIndicators, revisionDate: String): Boolean = {
    if (visitedList.contains(parent.key)) return false

    for (child <- parent.children if (child \ "@chg" != "")) {
      val key = (child \ "@key").text

      if (childHasChanged(revs(key), prevChanges, revisionDate)) {
        visitedList add Some(RevisionIndicator(key, "R", revisionDate, revs(key).element))
        return true
      }
    }
    return false

  }

}