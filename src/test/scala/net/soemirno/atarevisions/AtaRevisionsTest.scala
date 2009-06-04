package net.soemirno.atarevisions

import collection.mutable.HashMap
import java.io.File
import org.scalatest.junit.JUnit3Suite
import xml.{Text, NodeSeq, Group, Node, Elem}
import scala.actors.Actor._



class AtaRevisionsTest extends JUnit3Suite with Fixtures {
  val PREVIOUS_SOURCE = new File(FIXTURES_FOLDER, "previous.xml")
  val CURRENT_SOURCE = new File(FIXTURES_FOLDER, "current.xml")
  val RESULT_SOURCE = new File(FIXTURES_FOLDER, "result.xml")

  def testCreateAtaDocumentWithInvalidFile() = {
    try {
      AtaElement(new File("some_file_which_does_not_exist"))
      fail("should throw exception")
    } catch {
      case iae: IllegalArgumentException =>
    }

  }

  def testHasRevisedElements() = {
    var current :AtaElement = null
    var previous :AtaElement = null

    def comparer = actor {
      var complete = false
      receive {
        case "LOADED" => {
          complete = (current != null && previous!= null)
          if (complete) compare( current, previous)
        }
      }
    }

    def docLoader = actor {
      receive {
        case file: File => {
          current = AtaElement(file)
          println("loading current")
          comparer ! "LOADED"
        }
      }
    }

  def prevLoader = actor {
      receive {
        case file: File => {
          previous = AtaElement(file)
          println("loading previous")
          comparer ! "LOADED"
        }
      }
    }
    docLoader ! CURRENT_SOURCE
    prevLoader ! PREVIOUS_SOURCE

  }

  def compare (curr :AtaElement, prev :AtaElement ) = {
    val changes = curr.diff(prev)
    val checks = AtaElement(RESULT_SOURCE).revisionIndicators

    for (check <- checks.values) {
      if (check.changeType != "U" && !changes.contains(check.key))
        Console.println("not found: " + check.key + "," + check.changeType)
    }

    Console.println("--- Changes ---")
    for (change: RevisionIndicator <- changes.values) {
      Console.println("detected: " + change.key + "," + change.changeType)

      if (!checks.contains(change.key))
        Console.println("missing: " + change.key + "," + change.changeType)

      else if (change.changeType != checks(change.key).changeType)
        Console.println("expected: " + change.key + "," + checks(change.key).changeType)
    }
  }

}