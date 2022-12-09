package SensorHumidityStats

import SensorHumidityStats.SensorHumidityStatsProcessor.{
  collectCSVFiles,
  printReport,
  processStatsFiles
}
import models.{BrokenSensorStats, HealthySensorStats, SensorStats}
import org.scalatest.Inspectors.forAll
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class SensorHumidityStatsProcessorTest extends AnyFunSpec with Matchers {
  val resourcesBaseDir =
    s"${System.getProperty("user.dir")}/src/test/resources/testData"
  val testStatsDir =
    s"${System.getProperty("user.dir")}/src/test/resources/testData/statsData"

  describe("collectCSVFiles") {
    it("should return nothing when no directory is provided") {
      val stream = new java.io.ByteArrayOutputStream()
      Console.withOut(stream) {
        val result = collectCSVFiles(None)

        result should be(Option.empty)
        stream.toString should include(
          "Could not find a valid directory path! The application expects a valid path but got nothing"
        )
      }
    }

    it("should return nothing when directory does not exist") {
      val stream = new java.io.ByteArrayOutputStream()
      Console.withOut(stream) {
        val nonExistingDir = s"$resourcesBaseDir/NonExistingDir"
        val result = collectCSVFiles(Some(nonExistingDir))

        result should be(Option.empty)
        stream.toString should include(
          s"Could not find directory '$nonExistingDir'. Please provide a valid directory path."
        )
      }
    }

    it("should return an empty files when directory is empty") {
      val emptyDir = s"$resourcesBaseDir/emptyDir"
      val result = collectCSVFiles(Some(emptyDir))

      result.get should have size 0
    }

    it("should return only .csv files") {
      val mixedCSVAndNonCSVFilesDir =
        s"$resourcesBaseDir/mixedCSVAndNonCSVFiles"
      val result = collectCSVFiles(Some(mixedCSVAndNonCSVFilesDir))
      val files = result.get

      files should have size 2
      forAll(files.toList) { file => file.getName should endWith(".csv") }
    }
  }

  describe("processStatsFiles") {
    it(
      "should return the correct number of processed files and the results for only successful measurements"
    ) {
      val onlySuccessfulMeasurementsDir =
        s"$testStatsDir/onlySuccessfulMeasurements"
      val files = collectCSVFiles(Some(onlySuccessfulMeasurementsDir)).get
      val (results, filesCount) = processStatsFiles(files)

      filesCount should be(2)

      results should have size 2
      forAll(results) { sensorStats =>
        sensorStats shouldBe a[HealthySensorStats]
      }

      val sensorS1Stats = results.find(s => s.id == "s1").get
      verifySensorStats(sensorS1Stats, 10, 16.666666f, 30, 3, 0, true)

      val sensorS2Stats = results.find(s => s.id == "s2").get
      verifySensorStats(sensorS2Stats, 10, 20f, 30, 3, 0, true)
    }

    it(
      "should return the correct number of processed files and the results for only failed measurements"
    ) {
      val onlyFailedMeasurementsDir =
        s"$testStatsDir/onlyFailedMeasurements"
      val files = collectCSVFiles(Some(onlyFailedMeasurementsDir)).get
      val (results, filesCount) = processStatsFiles(files)

      filesCount should be(2)

      results should have size 2
      forAll(results) { sensorStats =>
        sensorStats shouldBe a[BrokenSensorStats]
      }

      val sensorS1Stats = results.find(s => s.id == "s1").get
      verifySensorStats(sensorS1Stats, -1, -1f, -1, 3, 3, false)

      val sensorS2Stats = results.find(s => s.id == "s2").get
      verifySensorStats(sensorS2Stats, -1, -1f, -1, 3, 3, false)
    }

    it(
      "should return the correct number of processed files and the results for a mix of failed and successful measurements"
    ) {
      val failedAndSuccessfulMeasurementsDir =
        s"$testStatsDir/failedAndSuccessfulMeasurements"
      val files = collectCSVFiles(Some(failedAndSuccessfulMeasurementsDir)).get
      val (results, filesCount) = processStatsFiles(files)

      filesCount should be(2)

      results should have size 3

      val sensorS1Stats = results.find(s => s.id == "s1").get
      verifySensorStats(sensorS1Stats, 10, 25.0f, 40, 3, 1, true)

      val sensorS2Stats = results.find(s => s.id == "s2").get
      verifySensorStats(sensorS2Stats, 10, 20.0f, 30, 3, 0, true)

      val sensorS3Stats = results.find(s => s.id == "s3").get
      verifySensorStats(sensorS3Stats, -1, -1f, -1, 1, 1, false)
    }
  }

  describe("printReport") {
    it("prints the report of the processed stats correctly") {
      val failedAndSuccessfulMeasurementsDir =
        s"$testStatsDir/failedAndSuccessfulMeasurements"
      val files = collectCSVFiles(Some(failedAndSuccessfulMeasurementsDir)).get

      val stream = new java.io.ByteArrayOutputStream()
      Console.withOut(stream) {
        val (result, filesCount) = processStatsFiles(files)

        printReport(result, filesCount)

        stream.toString should include(
          "Num of processed files: 2\n" +
            "Num of processed measurements: 7\n" +
            "Num of failed measurements: 2\n\n" +
            "Sensors with highest avg humidity:\n\n" +
            "sensor-id,min,avg,max\n" + "s1,10,25.0,40\n" +
            "s2,10,20.0,30\n" +
            "s3,NaN,Nan,NaN"
        )
      }
    }
  }

  def verifySensorStats(
      receivedSensorStats: SensorStats,
      expectedMin: Int,
      expectedAvg: Float,
      expectedMax: Int,
      expectedMeasurementsCount: Int,
      expectedFailuresCount: Int,
      expectedHealthyStatus: Boolean
  ): Unit = {
    receivedSensorStats.min should be(expectedMin)

    receivedSensorStats.avg shouldBe a[Float]
    receivedSensorStats.avg should be(expectedAvg)

    receivedSensorStats.max should be(expectedMax)

    receivedSensorStats.measurements should be(expectedMeasurementsCount)
    receivedSensorStats.failures should be(expectedFailuresCount)
    receivedSensorStats.healthy should be(expectedHealthyStatus)
  }
}
