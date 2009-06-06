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

    val changes = AtaManual(new File(args(0))).diff(AtaManual(new File(args(1))))

    for (change: RevisionIndicator <- changes.values)
      logger.debug("detected: " + change.key + "," + change.changeType)

    if (args.length == 3) {

      val checks = AtaManual(new File(args(2))).revisionIndicators

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
