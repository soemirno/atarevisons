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
    findChangedEffectivity(prevChanges, revisionDate)
    findChangedInChildren(prevChanges, revisionDate)
    findDeleted(prevChanges, revisionDate)
    return visitedList
  }

  def findNew(prevChanges: RevisionIndicators, revisionDate: String): Unit = {
    for (rev <- revs.values if !visitedList.contains(rev.key)) {
      Console.println("comparing new " + rev.key)
      if (!prevChanges.contains(rev.key()) || prevChanges(rev.key()).changeType == "D")
        visitedList add Some(RevisionIndicator(rev.key, "N", revisionDate, rev.element))
    }
  }

  def findDeleted(prevChanges: RevisionIndicators, revisionDate: String): Unit = {
    for (rev <- prevChanges.values if !visitedList.contains(rev.key)) {
      Console.println("comparing deleted " + rev.key)
      if (!revs.contains(rev.key()))
        visitedList add Some(RevisionIndicator(rev.key, "D", revisionDate, rev.element))
    }
  }

  def findChangedEffectivity(prevChanges: RevisionIndicators, revisionDate: String): Unit = {
    for (rev <- revs.values if !visitedList.contains(rev.key)) {
      Console.println("comparing effectivities " + rev.key)

      if (hasEffectivityChanges(rev.element, prevChanges(rev.key).element))
        visitedList add Some(RevisionIndicator(rev.key, "R", revisionDate, rev.element))

    }
  }

  def hasEffectivityChanges(current: Elem, previous: Elem): Boolean = {
    val currentEffectElem = current \ "effect"
    val previousEffectElem = previous \ "effect"

    if (currentEffectElem.size == 0 && previousEffectElem.size == 0) return false

    val currentEffectivity = (currentEffectElem \ "@effrg").text
    val previousEffectivity = (previousEffectElem \ "@effrg").text
    if (currentEffectivity != previousEffectivity) return true

    val currentSB = (currentEffectElem \ "sbeff")
    val previousSB = (previousEffectElem \ "sbeff")

    if (currentSB.size != previousSB.size) return true

    for (sb <- currentSB) {
      val prevSbItem = previousSB.filter(
        n => (n \ "@sbnbr").text == (sb \ "@sbnbr").text &&
             (n \ "@sbcond").text == (sb \ "@sbcond").text
        )
      if (prevSbItem.size == 0) return true
      if ((prevSbItem \ "@effrg").text != (sb \ "@effrg").text) return true
    }

    val currentCoc = (currentEffectElem \ "coceff")
    val previousCoc = (previousEffectElem \ "coceff")

    if (currentCoc.size != previousCoc.size) return true

    for (coc <- currentCoc) {
      val prevCocItem = previousCoc.filter(n => (n \ "@cocnbr").text == (coc \ "@cocnbr").text)
      if (prevCocItem.size == 0) return true
      if ((prevCocItem \ "@effrg").text != (coc \ "@effrg").text) return true
    }

    return false
  }

  def findChangedLength(prevChanges: RevisionIndicators, revisionDate: String): Unit = {
    for (rev <- revs.values if !visitedList.contains(rev.key)) {
      Console.println("comparing length " + rev.key)

      val currentLength = rev.children.size
      val previousLength = prevChanges(rev.key).children.size

      if (currentLength != previousLength)
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
    for (child <- parent.children if (child \ "@chg" != "")) {
      val key = (child \ "@key").text
      if (visitedList.contains(key)) return true
      if (childHasChanged(revs(key), prevChanges, revisionDate)) {
        visitedList add Some(RevisionIndicator(key, "R", revisionDate, revs(key).element))
        return true
      }
    }

    val previousChildren = prevChanges(parent.key).children.filter(n => n \ "@chg" == "")

    var index = 0
    for (child <- parent.children if (child \ "@chg" == "")) {
      val prevChild = previousChildren(index)
      if (prevChild.label != child.label) return true
      if (prevChild.text.trim != child.text.trim) return true
      index = index + 1
    }
    return false

  }

}