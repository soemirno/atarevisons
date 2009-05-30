package net.soemirno.atarevisions

import java.io.File
import org.scalatest.junit.JUnit3Suite

class AtaRevisionsTest extends JUnit3Suite with Fixtures {
  val PREVIOUS_SOURCE = new File(FIXTURES_FOLDER, "previous.xml")
  val CURRENT_SOURCE = new File(FIXTURES_FOLDER, "current.xml")


  def testCreateAtaDocumentWithInvalidFile() = {
    try {
      AtaElement(new File("some_file_which_does_not_exist"))
      fail("should throw exception")
    } catch {
      case iae: IllegalArgumentException =>
    }

  }

  def testHasRevisedElements() = {
    val previous = AtaElement(PREVIOUS_SOURCE)
    val current = AtaElement(CURRENT_SOURCE)

    //    Console.println("--- Previous ---" )
    //    for (element <- previous.revisionIndicators)
    //      Console.println(element )
    //
//        Console.println("--- Current ---")
//        for (element <- current.revisionIndicators)
//          Console.println(element)

    val changes = current.diff(previous, "20090201")

    Console.println("--- Changes ---")
    for (change <- changes)
      Console.println("detected: "  + change)

    assert(changes.keySet.contains("W0000019"))

    assert(!changes.keySet.contains("W0317651"))
    assert(changes.keySet.contains("W0317650"))


    //content change
    assert(changes.keySet.contains("W0000013"))
    assert(changes("W0000013").changeType == "R")
    assert(changes("W0000013").date == "20090201")

    assert(changes("W0000019").changeType == "N")
    assert(changes("W0000019").date == "20090201")

    assert(changes("W0000002").changeType == "N")
    assert(changes("W0000002").date == "20090201")

    assert(changes("W0000006").changeType == "R")
    assert(changes("W0000006").date == "20090201")
  }
}