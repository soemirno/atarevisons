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
  private val cache = new RevisionIndicators
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
    result ++ findChildrenChangesBruteForce ( previousIndicators, revisionDate, result)
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

  def findEffectivityChanges(prevChanges: RevisionIndicators,
                             revisionDate: String,
                             foundChanges: RevisionIndicators): RevisionIndicators = {
    val result = new RevisionIndicators

    for (rev <- revIndicators.values if !foundChanges.contains(rev.key)) {
      Console.println("finding effectivity changes " + rev.key)

      if (hasEffectivityChanges(rev, prevChanges(rev.key)))
        result add Some(RevisionIndicator(rev.key, "R", revisionDate, rev))

    }

    return result
  }

  def hasEffectivityChanges(current: RevisionIndicator, previous: RevisionIndicator): Boolean = {
    val currentEffectElem = current \ "effect"
    val previousEffectElem = previous \ "effect"

    if (currentEffectElem.size == 0 && previousEffectElem.size == 0) return false

    val currentEffectivity = currentEffectElem \ "@effrg"
    val previousEffectivity = previousEffectElem \ "@effrg"

    if (currentEffectivity != previousEffectivity) return true

    val currentSB = currentEffectElem \ "sbeff"
    val previousSB = previousEffectElem \ "sbeff"

    if (currentSB.size != previousSB.size) return true

    for (sb <- currentSB) {
      val prevSbItem = previousSB.filter(
        n => n \ "@sbnbr" == sb \ "@sbnbr" &&
             n \ "@sbcond" == sb \ "@sbcond"
        )
      if (prevSbItem.size == 0) return true
      if (prevSbItem != sb) return true
    }

    val currentCoc = currentEffectElem \ "coceff"
    val previousCoc = previousEffectElem \ "coceff"

    if (currentCoc.size != previousCoc.size) return true

    for (coc <- currentCoc) {
      val prevCocItem = previousCoc.filter(n => n \ "@cocnbr" == coc \ "@cocnbr")
      if (prevCocItem.size == 0) return true
      if (prevCocItem != coc) return true
    }

    return false
  }

  def findLengthChanges(prevChanges: RevisionIndicators,
                        revisionDate: String,
                        foundChanges: RevisionIndicators): RevisionIndicators  = {
    val result = new RevisionIndicators

    for (rev <- revIndicators.values if !foundChanges.contains(rev.key)) {
      Console.println("finding length changes" + rev.key)

      val currentLength = rev.children.size
      val previousLength = prevChanges(rev.key).children.size

      if (currentLength != previousLength)
        result add Some(RevisionIndicator(rev.key, "R", revisionDate, rev))

    }

    return result
  }

  def findChildrenChanges(prevChanges: RevisionIndicators,
                          revisionDate: String,
                          foundChanges: RevisionIndicators): RevisionIndicators =  {
    val result = new RevisionIndicators

    for (rev <- revIndicators.values if !foundChanges.contains(rev.key)) {
      Console.println("finding children changes" + rev.key)

      if (childHasChanged(rev, prevChanges, revisionDate, foundChanges))
        result add Some(RevisionIndicator(rev.key, "R", revisionDate, rev))
    }
    return result
  }

  def findChildrenChangesBruteForce(prevChanges: RevisionIndicators,
                          revisionDate: String,
                          foundChanges: RevisionIndicators): RevisionIndicators =  {

    for (rev <- revIndicators.values if !foundChanges.contains(rev.key)) {
       Console.println("finding children changes " + rev.key)
      if(!cache.contains(rev.key))
        equalsWithoutText(rev, prevChanges(rev.key), revisionDate)        
    }

    return cache
  }

  def childHasChanged(parent: RevisionIndicator,
                      prevChanges: RevisionIndicators,
                      revisionDate: String,
                      foundChanges: RevisionIndicators): Boolean = {
    for (child <- parent.children if child \ "@chg" != "") {
      val key = (child \ "@key").text

      if (foundChanges.contains(key)) return true

      if (childHasChanged(revIndicators(key), prevChanges, revisionDate, foundChanges)) return true
      
    }

    val previousChildren = prevChanges(parent.key).children.filter(n => n \ "@chg" == "")
    var index = 0

    for (child <- parent.children if child \ "@chg" == "") {
      val prevChild = previousChildren(index)

      if (prevChild.label != child.label) return true

      if (prevChild.text.trim != child.text.trim) return true

      index = index + 1
    }

    return false
  }

  def equalsWithoutText(thisElem: Node, thatElem: Node, revisionDate :String): Boolean = {
    val ignoreList = List("chg", "revdate", "targetrefid")
    var isEqual = false

    val sameTag = ((thatElem.prefix == thisElem.prefix)
              && (thatElem.label == thisElem.label)
              && (thatElem.attributes.filter(a => !ignoreList.contains(a.key)) ==
              thisElem.attributes.filter(a => !ignoreList.contains(a.key))))

    if (!sameTag) {
      if(thisElem.isInstanceOf[RevisionIndicator]){
        val ri = thisElem.asInstanceOf[RevisionIndicator]
        cache add  Some(RevisionIndicator(ri.key, "R", revisionDate, ri))
      }
      return false
    }

    if (thisElem.child.length == 1 && thisElem.child(0).isInstanceOf[Text] &&
            thatElem.child.length == 1 && thatElem.child(0).isInstanceOf[Text]){
      isEqual = thisElem.text.trim == thatElem.text.trim
      if (!isEqual && thisElem.isInstanceOf[RevisionIndicator]) {
        val ri = thisElem.asInstanceOf[RevisionIndicator]
        cache add  Some(RevisionIndicator(ri.key, "R", revisionDate, ri))
      }
      return isEqual
    }

    isEqual = hasSameChildren(thatElem.child, thisElem.child, revisionDate)
    if (!isEqual && thisElem.isInstanceOf[RevisionIndicator]) {
      val ri = thisElem.asInstanceOf[RevisionIndicator]
      cache add  Some(RevisionIndicator(ri.key, "R", revisionDate, ri))
    }
    return isEqual
  }

  def hasSameChildren(a :NodeSeq, b :NodeSeq, revisionDate: String):Boolean = {
    val ita = a.filter(e => e.isInstanceOf[Elem]).elements
    val itb = b.filter(e => e.isInstanceOf[Elem]).elements
    var res = true
    while (res && ita.hasNext && itb.hasNext) {
      res = (equalsWithoutText(ita.next, itb.next, revisionDate))
    }
    !ita.hasNext && !itb.hasNext && res
  }


}