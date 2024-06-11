package vir.networkintegration.rwr
import org.saddle._
import org.saddle.order._

sealed trait DegreeNormalization
case object KLDivergence extends DegreeNormalization
case object LRT extends DegreeNormalization
case object NoDegreeNormalization extends DegreeNormalization

object DegreeNormalization {
  implicit val ord: ORD[DegreeNormalization] = cats.Order.by(d =>
    d match {
      case KLDivergence          => 0
      case LRT                   => 1
      case NoDegreeNormalization => 2
    }
  )
}

case class Hyperparameter(
    iterations: Int,
    restartProbability: Double,
    degreeNormalization: DegreeNormalization
) {
  val degreeNormalizationAsString = degreeNormalization match {
    case LRT                   => "LR_"
    case NoDegreeNormalization => "NO_"
    case KLDivergence          => "KL_"
  }
  override def toString =
    s"i$iterations-r$restartProbability-d$degreeNormalizationAsString"
}

object Hyperparameter {
  implicit val ord: ORD[Hyperparameter] = cats.Order.by(h =>
    (
      h.iterations,
      h.restartProbability,
      h.degreeNormalization
    )
  )
}
