import scala.io.Source

case class TemperatureRecord(day: Int, doy: Int, month: Int, stateId: Int, year: Int, precip: Double,
                             tAvg: Double, tMax: Double, tMin: Double)


object TemperatureExploration {
  def convert(line: Array[String]): TemperatureRecord = {
    TemperatureRecord(line.head.toInt, line(1).toInt, line(2).toInt, line(3).replace("'", "").toInt,
      line(4).toInt, line(5).toDouble, line(6).toDouble, line(7).toDouble, line(8).toDouble)
  }

  def toDoubleOrNeg(s: String): Double =
    try{
      s.toDouble
    } catch {
      case _ : NumberFormatException => -1
    }

  def main(args: Array[String]): Unit = {
    val fileData = Source.fromFile("src/main/resources/TX417945_8515.csv")
    val lines = fileData.getLines().drop(1)
    val tempData = lines.map{ line => convert(line.split(",")) }.toArray
    fileData.close()

    // Finding the max, min temp
    val maxTemp = tempData.maxBy(_.tMax).tMax
    val minTemp = tempData.foldLeft(Double.MaxValue) { case(a, b) => if (b.tMin != 0.0) Math.min(a, b.tMin) else a }
    val mostAvgTemp = tempData.groupBy(_.tAvg).maxBy(_._2.length)._1
    val avgTemp = tempData.map(_.tAvg).sum/tempData.length
    val monthlyAvgTemp = tempData.map(x => (x.month, x.tAvg)).groupBy(_._1)
      .map{ case(k,v) => k -> v.foldLeft(0.0){ case(a,b) => a+b._2 }/v.length }
//    println(s"Max Temp: $maxTemp, Min Temp: $minTemp and most Avg Temp: $mostAvgTemp and actual avg: $avgTemp")
    println(monthlyAvgTemp)
  }
}