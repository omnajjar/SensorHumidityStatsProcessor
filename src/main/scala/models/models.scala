package models

trait SensorMeasurement {
  val id: String
}

final case class SuccessfulSensorMeasurement(id: String, humidity: Int)
    extends SensorMeasurement

final case class FailedSensorMeasurement(id: String) extends SensorMeasurement

trait SensorStats {
  val id: String
  val min: Int
  val avg: Float
  val max: Int
  val failures: Int
  val measurements: Int
  val healthy: Boolean
}

final case class BrokenSensorStats(
    id: String,
    measurements: Int,
    failures: Int
) extends SensorStats {
  override val min: Int = -1
  override val avg: Float = -1
  override val max: Int = -1
  override val healthy: Boolean = false

  override def toString() = {
    s"$id,NaN,Nan,NaN"
  }
}

final case class HealthySensorStats(
    id: String,
    min: Int,
    avg: Float,
    max: Int,
    measurements: Int,
    failures: Int
) extends SensorStats {
  override val healthy: Boolean = true

  override def toString() = {
    s"$id,$min,$avg,$max"
  }
}
