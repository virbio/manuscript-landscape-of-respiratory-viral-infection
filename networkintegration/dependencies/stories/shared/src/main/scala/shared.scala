package stories

import org.saddle._

case class GraphDTO(
    nodeLocations: String,
    edges: String,
    nodeClusters: String,
    legendText: String,
    extraText: String,
    edgeColorText: String
)
object GraphDTO {
  import _root_.io.circe.generic.semiauto._
  implicit val codec = deriveCodec[GraphDTO]
}




case class ScatterPanelDescription(
    data: Frame[String, Int, Double],
    main: String,
)
object ScatterPanelDescription {
  import _root_.io.circe.generic.semiauto._
  import org.saddle.circe._
  implicit val codec = deriveCodec[ScatterPanelDescription]
}

case class ScatterDescription(
  panels: Vector[ScatterPanelDescription],
  rowDescriptions: Frame[String,String,String],
  columns: Int
)

object ScatterDescription {
  import _root_.io.circe.generic.semiauto._
  import org.saddle.circe._
  implicit val codec = deriveCodec[ScatterDescription]
}