package SensorHumidityStats

import SensorHumidityStats.SensorHumidityStatsProcessor.collectCSVFiles
import org.scalatest.Inspectors.forAll
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class SensorHumidityStatsProcessorTest extends AnyFunSpec with Matchers {
  val resourcesBaseDir =
    s"${System.getProperty("user.dir")}/src/test/resources/testData"

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
}
