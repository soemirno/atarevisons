package net.soemirno.atarevisions

import collection.mutable.HashSet
import java.io.File
import org.slf4j.LoggerFactory
import xml.Elem
import scala.actors.Actor._

object Starter {
  val logger = LoggerFactory.getLogger(this.getClass)
  def main(args: Array[String]) {
    var current: AtaElement = null
    var previous: AtaElement = null

    def comparer = actor {
      while (true) {
        var complete = false
        receive {
          case "LOADED" => {
            complete = (current != null && previous != null)
            if (complete) compare(current, previous, new File(args(2)))
          }
        }
      }

    }

    def docLoader = actor {
      receive {
        case file: File => {
          logger.info("loading current")
          current = AtaElement(file)
          comparer ! "LOADED"
        }
      }
    }

    def prevLoader = actor {
      receive {
        case file: File => {
          logger.info("loading previous")
          previous = AtaElement(file)          
          comparer ! "LOADED"
        }
      }
    }
    docLoader ! new File(args(0))
    prevLoader ! new File(args(1))
  }

  def compare(curr: AtaElement, prev: AtaElement, resultSource: File) = {
    val changes = curr.diff(prev)
    val checks = AtaElement(resultSource).revisionIndicators

    for (check <- checks.values) {
      if (check.changeType != "U" && !changes.contains(check.key))
        logger.info("not found: " + check.key + "," + check.changeType)
    }

    logger.info("--- Changes ---")
    for (change: RevisionIndicator <- changes.values) {
      logger.info("detected: " + change.key + "," + change.changeType)

      if (!checks.contains(change.key))
        logger.info("missing: " + change.key + "," + change.changeType)

      else if (change.changeType != checks(change.key).changeType)
        logger.info("expected: " + change.key + "," + checks(change.key).changeType)
    }
  }  
}

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

    for (element <- elementsContainingRevInd)
      list.add (RevisionIndicator(element))

    list
  }

  def revisionIndicators(): RevisionIndicators = revIndicators

  def diff(previous: AtaElement): RevisionIndicators = {
    val result = new RevisionIndicators
    val previousIndicators = previous.revisionIndicators

    result ++ findChanges ( previousIndicators)
    result ++ findDeleted ( previousIndicators)

    return result
  }


  def findDeleted(prevChanges: RevisionIndicators): RevisionIndicators = {
    val result = new RevisionIndicators

    for (rev <- prevChanges.values) {
      Console.println("finding deleted " + rev.key)

      if (!revIndicators.contains(rev.key()))
        result add (RevisionIndicator("D", rev))
    }

    return result
  }

  def findChanges(prevChanges: RevisionIndicators): RevisionIndicators =  {
    val result = new RevisionIndicators

    for (rev <- revIndicators.values) {
      Console.println("finding changes " + rev.key)
      if (!prevChanges.contains(rev.key()) || prevChanges(rev.key()).changeType == "D")
        result add (RevisionIndicator("N", rev))
      
      else if (rev isSameAs (prevChanges(rev.key)))
        result add (RevisionIndicator("U", rev))

      else
        result add (RevisionIndicator("R", rev))
    }
    return result
  }

}