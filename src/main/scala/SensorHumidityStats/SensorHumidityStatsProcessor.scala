package SensorHumidityStats

import models.{
  BrokenSensorStats,
  FailedSensorMeasurement,
  HealthySensorStats,
  SensorMeasurement,
  SensorStats,
  SuccessfulSensorMeasurement
}

import java.io.{File, FileFilter}
import scala.io.Source
import scala.util.Try

object SensorHumidityStatsProcessor {

  def collectCSVFiles(directoryName: Option[String]): Option[Iterator[File]] = {
    directoryName match {
      case Some(dirName) =>
        val directory = new File(dirName)
        if (directory.exists()) Some(directory.getCSVFiles)
        else {
          println(
            s"Could not find directory '$dirName'. Please provide a valid directory path."
          )
          Option.empty
        }
      case _ =>
        println(
          "Could not find a valid directory path! The application expects a valid path but got nothing"
        )
        Option.empty
    }
  }

  def processStatsFiles(
      files: Iterator[File]
  ): (Iterable[SensorStats], Int) = {
    val filesProcessedCount = files.size
    val allStats = files
      /* Todo: Improvement, cli user cannot select whether to drop (or not) headers. Make it configurable in the next iteration. */
      .flatMap(a => processStatFile(a, true))
      .toList

    val groupedStats = allStats
      .groupBy(stat => stat.id)

    val calculatedStats = groupedStats
      .map { case (sensorId, measurements) =>
        calculateSensorMeasurements(sensorId, measurements)
      }

    (calculatedStats, filesProcessedCount)
  }

  def printReport(
      sensorsStats: Iterable[SensorStats],
      fileProcessedCount: Int
  ) = {
    val orderedStats = sensorsStats.toSeq.sortWith((a, b) => b.avg < a.avg)
    val allMeasurementsCount =
      orderedStats.map(sensorsStats => sensorsStats.measurements).sum
    val allFailuresCount =
      orderedStats.map(sensorsStats => sensorsStats.failures).sum

    println(s"Num of processed files: $fileProcessedCount")
    println(s"Num of processed measurements: $allMeasurementsCount")
    println(s"Num of failed measurements: $allFailuresCount\n")

    println("Sensors with highest avg humidity:\n")
    println("sensor-id,min,avg,max")
    orderedStats.foreach(println)
  }

  private def processStatFile(
      file: File,
      dropHeaders: Boolean
  ): Iterator[SensorMeasurement] = {
    Try {
      val source = Source.fromFile(file)
      val lines = source.getLines().drop(if (dropHeaders) 1 else 0)
      for (line <- lines) yield processStatLine(line)
    }.getOrElse({
      println(
        s"Oops! something went wrong while processing file: ${file.getName}"
      )
      Iterator.empty
    })
  }

  private def processStatLine(line: String): SensorMeasurement = {
    val Array(sensorId, humidityStr) = line.split(",")
    humidityStr.toIntOption match {
      case Some(humidity) => SuccessfulSensorMeasurement(sensorId, humidity)
      case None           => FailedSensorMeasurement(sensorId)
    }
  }

  private def calculateSensorMeasurements(
      sensorId: String,
      sensorMeasurements: List[SensorMeasurement]
  ): SensorStats = {
    val (successfulSensorMeasurement, failedSensorMeasurement) =
      sensorMeasurements.foldLeft(
        (List[SuccessfulSensorMeasurement](), List[FailedSensorMeasurement]())
      )((acc, sm) =>
        sm match {
          case ssm: SuccessfulSensorMeasurement => (acc._1 ++ List(ssm), acc._2)
          case fsm: FailedSensorMeasurement     => (acc._1, acc._2 ++ List(fsm))
        }
      )

    val humidityValues = successfulSensorMeasurement.map(_.humidity)

    if (successfulSensorMeasurement.isEmpty) {
      BrokenSensorStats(
        sensorId,
        failedSensorMeasurement.size,
        failedSensorMeasurement.size
      )
    } else {
      val min = humidityValues.min
      val max = humidityValues.max
      val avg = humidityValues.sum.toFloat / humidityValues.size
      HealthySensorStats(
        sensorId,
        min,
        avg,
        max,
        sensorMeasurements.size,
        failedSensorMeasurement.size
      )
    }
  }

  private implicit class FileHelper(file: File) {
    private val csvFileFilter = new FileFilter {
      override def accept(file: File): Boolean = file.getName.endsWith(".csv")
    }

    def getCSVFiles: Iterator[File] = file.listFiles(csvFileFilter).iterator
  }
}
