package net.soemirno.atarevisions

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

    result ++ findNew ( previousIndicators, revisionDate, result)
    result ++ findChildrenChanges ( previousIndicators, revisionDate, result)
    result ++ findDeleted ( previousIndicators, revisionDate, result)

    return result
  }

  def findNew(prevChanges: RevisionIndicators,
              revisionDate: String,
              foundChanges: RevisionIndicators): RevisionIndicators = {
    val result = new RevisionIndicators

    for (rev <- revIndicators.values if !foundChanges.contains(rev.key)) {
      Console.println("finding new " + rev.key)

      if (!prevChanges.contains(rev.key()) || prevChanges(rev.key()).changeType == "D")
        result add Some(RevisionIndicator(rev.key, "N", revisionDate, rev))
    }

    return result
  }

  def findDeleted(prevChanges: RevisionIndicators,
                  revisionDate: String,
                  foundChanges: RevisionIndicators): RevisionIndicators = {
    val result = new RevisionIndicators

    for (rev <- prevChanges.values if !foundChanges.contains(rev.key)) {
      Console.println("finding deleted " + rev.key)

      if (!revIndicators.contains(rev.key()))
        result add Some(RevisionIndicator(rev.key, "D", revisionDate, rev))
    }

    return result
  }

  def findChildrenChanges(prevChanges: RevisionIndicators,
                          revisionDate: String,
                          foundChanges: RevisionIndicators): RevisionIndicators =  {
    val result = new RevisionIndicators

    for (rev <- revIndicators.values if !foundChanges.contains(rev.key)) {
      Console.println("finding children changes " + rev.key)

      if (isSame(rev, prevChanges(rev.key)))
        result add Some(RevisionIndicator(rev.key, "U", revisionDate, rev))
      else
        result add Some(RevisionIndicator(rev.key, "R", revisionDate, rev))
    }
    return result
  }


  def isSame(thisElem: Node, thatElem: Node): Boolean = {
    val ignoreList = List("chg", "revdate", "targetrefid")

    val sameTag = ((thatElem.prefix == thisElem.prefix)
              && (thatElem.label == thisElem.label)
              && (thatElem.attributes.filter(a => !ignoreList.contains(a.key)) ==
              thisElem.attributes.filter(a => !ignoreList.contains(a.key))))

    if (!sameTag) return false

    if (thisElem.child.length == 1 && thatElem.child.length == 1 ) return thisElem == thatElem

    return hasSameChildren(thatElem.child, thisElem.child)
  }

  def hasSameChildren(a :NodeSeq, b :NodeSeq):Boolean = {
    val ita = a.filter(e => e.isInstanceOf[Elem]).elements
    val itb = b.filter(e => e.isInstanceOf[Elem]).elements
    var res = true
    while (res && ita.hasNext && itb.hasNext) {
      res = (isSame(ita.next, itb.next))
    }
    !ita.hasNext && !itb.hasNext && res
  }

}