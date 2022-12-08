import SensorHumidityStats.SensorHumidityStatsProcessor.{
  collectCSVFiles,
  printReport,
  processStatsFiles
}

object Main {
  def main(args: Array[String]): Unit = {
    val csvFilesToProcess = collectCSVFiles(args.headOption)
    csvFilesToProcess collect { case files =>
      val (sensorsCalculatedStats, filesProcessedCount) = processStatsFiles(
        files
      )
      printReport(sensorsCalculatedStats, filesProcessedCount)
    }
  }
}
