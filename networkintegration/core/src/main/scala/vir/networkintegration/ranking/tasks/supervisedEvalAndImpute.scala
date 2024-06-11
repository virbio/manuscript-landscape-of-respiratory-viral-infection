package vir.networkintegration.ranking.tasks

import tasks._
import org.saddle.order._
import tasks.circesupport._
import scala.concurrent.Future

import _root_.io.circe.generic.semiauto._
import vir.networkintegration.ranking
import org.saddle._
import org.saddle.circe._
import stat.cv.CVSplit
import _root_.io.circe.Codec
import _root_.io.circe.Encoder
import _root_.io.circe.Decoder
import stat.cv.KFold
import _root_.io.circe.generic.auto._
import cats.effect.IO
import vir.networkintegration.notebook.Notebook1

case class SupervisedEvalAndImputeInput(
    crisprFeatures: Option[Frame[String, String, Double]],
    mashupFeatures: SharedFile,
    target: Series[String, Double]
)

object SupervisedEvalAndImputeInput {

  implicit val imputeInputCodec = deriveCodec[SupervisedEvalAndImputeInput]

}

object SupervisedEvalAndImputeTask {
  type ResultType =
    (List[Series[String, Double]], Frame[String, Int, Double])

  val task = Task[SupervisedEvalAndImputeInput, ResultType](
    "supervisedevalandimpute",
    3
  ) { input => implicit ctx =>
    (for {
      mashupF <- input.mashupFeatures.file

    } yield {
      val mashupFeatures = Notebook1.readBinaryFrameFromFile(mashupF)

      val rng = new scala.util.Random(234543)
      ranking.supervisedEvalAndImpute(
        input.crisprFeatures,
        mashupFeatures,
        input.target,
        rng
      )
    }).use(a => IO.pure(a))
  }

}
