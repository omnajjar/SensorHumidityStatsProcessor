import SensorHumidityStats.SensorHumidityStatsProcessor.{
  collectCSVFile,
  printReport,
  processStatsFiles
}

object Main {
  def main(args: Array[String]): Unit = {
    val csvFilesToProcess = collectCSVFile(args.headOption)
    csvFilesToProcess collect { case files =>
      val (sensorsCalculatedStats, filesProcessedCount) = processStatsFiles(
        files
      )
      printReport(sensorsCalculatedStats, filesProcessedCount)
    }
  }
}
