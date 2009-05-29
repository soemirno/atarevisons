package net.soemirno.atarevisions

import collection.mutable.HashMap
import java.io.File
import xml.{Node, Elem}

object AtaDocument {
  def apply(sourceFile: File) = {
    val document: Elem = {
      if (isValid(sourceFile))
        xml.XML.loadFile(sourceFile)
      else
        throw new IllegalArgumentException("use valid xml file")
    }
    new AtaDocument(document)
  }

  def isValid(sourceFile: File): Boolean = {
    if (sourceFile == null) return false
    sourceFile.exists
  }

}

class AtaDocument(document: Elem) {
  val keyedElements = (document \\ "_").filter(element => element \ "@key" != "")

  def changeList() = {
    val changeList = new ChangeList
    for (element <- keyedElements) {
      changeList.add(Change(element))
    }
    changeList
  }

  def diff(otherDocument: AtaDocument, revisionDate: String) = {
    val changeList = new ChangeList
    val prevChanges = otherDocument.changeList

    for (key <- changeList().keys) {
      if (!prevChanges.contains(key) || prevChanges(key).changeType == "D") {
        val change = Change(key, "N", revisionDate)
        changeList.add(change)
      }
    }
    changeList
  }

}