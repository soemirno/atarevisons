package net.soemirno.atarevisions

import java.io.File
import org.slf4j.LoggerFactory

object Starter {
  val logger = LoggerFactory.getLogger(this.getClass)

  /**
   * main function to start process from the commandline
   */
  def main(args: Array[String]) {
    logger.info("start comparing")

    val changes = AtaManual(new File(args(1))).diff(AtaManual(new File(args(2))), args(0))

    logger.info("start write revised document")
    scala.xml.XML.saveFull(args(0) + ".xml", AtaManualWriter.updateRevisionIndicators(changes(""), changes), "UTF-8", true, null)
    logger.info("finished write revised document")

    for (change: RevisionIndicator <- changes.values)
      logger.debug("detected: " + change.key + "," + change.changeType)

    if (args.length == 4) {

      val checks = AtaManual(new File(args(3))).revisionIndicators

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
    logger.info("finished comparing")
  }
}
