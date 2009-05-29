package net.soemirno.atarevisions

import collection.mutable.HashMap
import java.io.File
import xml.{Node, Elem}

object AtaElement {
  def apply(sourceFile: File) = {
    val document: Elem = {
      if (isValid(sourceFile))
        xml.XML.loadFile(sourceFile)
      else
        throw new IllegalArgumentException("use valid xml file")
    }
    new AtaElement(document)
  }

  def isValid(sourceFile: File): Boolean = {
    if (sourceFile == null) return false
    sourceFile.exists
  }

}

class AtaElement(element: Elem) {
  val keyedElements = (element \\ "_").filter(element => element \ "@key" != "")

  def revisionIndicators() = {
    val list = new RevisionIndicators
    for (element <- keyedElements) {
      list.add(RevisionIndicator(element))
    }
    list
  }

  def diff(other: AtaElement, revisionDate: String) = {
    val list = new RevisionIndicators
    val prevChanges = other.revisionIndicators

    for (key <- revisionIndicators().keys) {
      if (!prevChanges.contains(key) || prevChanges(key).changeType == "D") {
        val change = RevisionIndicator(key, "N", revisionDate)
        list.add(change)
      }
    }
    list
  }

}