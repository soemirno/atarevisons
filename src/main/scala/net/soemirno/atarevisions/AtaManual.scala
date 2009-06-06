package net.soemirno.atarevisions

import collection.mutable.HashMap
import collection.mutable.Map
import java.io.File
import org.slf4j.LoggerFactory

object Starter {
  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]) {
    logger.info("start comparing")

    val changes = AtaElement(new File(args(0))).diff(AtaElement(new File(args(1))))

    for (change: RevisionIndicator <- changes.values)
      logger.info("detected: " + change.key + "," + change.changeType)

    if (args.length == 3) {

      val checks = AtaElement(new File(args(2))).revisionIndicators

      for (check <- checks.values) {
        if (check.changeType != "U" && !changes.contains(check.key))
          logger.info("not found: " + check.key + "," + check.changeType)
      }

      logger.info("--- Changes ---")
      for (change: RevisionIndicator <- changes.values) {
        if (!checks.contains(change.key))
          logger.info("missing: " + change.key + "," + change.changeType)

        else if (change.changeType != checks(change.key).changeType)
          logger.info("expected: " + change.key + "," + checks(change.key).changeType)
      }
    }
  }
}

object AtaManual {

  /**
   *
   */
  def apply(sourceFile: File) = {
    val document = xml.XML loadFile (sourceFile)
    val elementsContainingRevInd = (document \\ "_").filter(e => e \ "@key" != "")

    val list = HashMap[String, RevisionIndicator]()
    for (element <- elementsContainingRevInd) {
      val rev = RevisionIndicator(element)
      list += Pair(rev.key, rev)
    }

    new AtaManual(list)
  }

}

class AtaManual(revIndicators: collection.mutable.Map[String, RevisionIndicator]) {
  val logger = LoggerFactory.getLogger(this.getClass)

  def revisionIndicators(): Map[String, RevisionIndicator] = revIndicators

  def diff(previous: AtaManual): Map[String, RevisionIndicator] = {
    val result = new HashMap[String, RevisionIndicator]
    val previousIndicators = previous.revisionIndicators

    result ++ findChanges(previousIndicators)
    result ++ findDeleted(previousIndicators)

    return rolledUpResults(result)
  }

  /**
   *  make sure parent elements reflect changes in children
   */
  def rolledUpResults(result: Map[String, RevisionIndicator]): Map[String, RevisionIndicator] = {
    logger.info("rollup results")
    var foundChange = true

    while (foundChange) {
      foundChange = false
      for (rev <- result.values if rev.changeType() == "U") {
        for (child <- rev.child if (child \ "@chg" != "")) {
          if (result((child \ "@key").text).changeType != "U") {
            logger.info("rolling up " + rev.key)
            foundChange = true
            result += Pair(rev.key, RevisionIndicator("R", rev))
          }
        }
      }
    }

    return result
  }

  /**
   * Find elements removed in this revision
   */
  def findDeleted(prevChanges: Map[String, RevisionIndicator]): Map[String, RevisionIndicator] = {
    val result = new HashMap[String, RevisionIndicator]
    logger.info("finding deleted elements")

    for (rev <- prevChanges.values if !revIndicators.contains(rev.key()))
      result += Pair(rev.key, RevisionIndicator("D", rev))

    return result
  }

  /**
   * Find elements changed in this revision
   */
  def findChanges(prevChanges: Map[String, RevisionIndicator]): Map[String, RevisionIndicator] = {
    val result = new HashMap[String, RevisionIndicator]
    logger.info("finding changed elements")

    for (rev <- revIndicators.values) {

      if (!prevChanges.contains(rev.key()) || prevChanges(rev.key()).changeType == "D")
        result += Pair(rev.key, RevisionIndicator("N", rev))

      else if (rev isSameAs prevChanges(rev.key))
        result += Pair(rev.key, RevisionIndicator("U", rev))

      else
        result += Pair(rev.key, RevisionIndicator("R", rev))
    }
    return result
  }

}