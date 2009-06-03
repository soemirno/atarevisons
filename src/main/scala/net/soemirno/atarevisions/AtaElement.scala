package net.soemirno.atarevisions

import collection.mutable.HashSet
import java.io.File
import xml.{Text, Node, NodeSeq, Elem}
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
  
  private val revIndicators = {
    val elementsContainingRevInd = (elem \\ "_").filter(e => e \ "@chg" != "")
    val list = new RevisionIndicators

    for (element <- elementsContainingRevInd) list.add (RevisionIndicator(element))
    list
  }

  def revisionIndicators(): RevisionIndicators = revIndicators

  def diff(previous: AtaElement, revisionDate: String): RevisionIndicators = {
    val result = new RevisionIndicators
    val previousIndicators = previous.revisionIndicators

    result ++ findChanges ( previousIndicators, revisionDate)
    result ++ findDeleted ( previousIndicators, revisionDate)

    return result
  }


  def findDeleted(prevChanges: RevisionIndicators,
                  revisionDate: String): RevisionIndicators = {
    val result = new RevisionIndicators

    for (rev <- prevChanges.values) {
      Console.println("finding deleted " + rev.key)

      if (!revIndicators.contains(rev.key()))
        result add (RevisionIndicator(rev.key, "D", revisionDate, rev))
    }

    return result
  }

  def findChanges(prevChanges: RevisionIndicators,
                          revisionDate: String): RevisionIndicators =  {
    val result = new RevisionIndicators

    for (rev <- revIndicators.values) {
      Console.println("finding changes " + rev.key)
      if (!prevChanges.contains(rev.key()) || prevChanges(rev.key()).changeType == "D")
        result add (RevisionIndicator(rev.key, "N", revisionDate, rev))
      
      else if (rev == (prevChanges(rev.key)))
        result add (RevisionIndicator(rev.key, "U", prevChanges(rev.key).revdate, rev))

      else
        result add (RevisionIndicator(rev.key, "R", revisionDate, rev))
    }
    return result
  }

}