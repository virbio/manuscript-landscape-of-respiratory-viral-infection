package vir.networkintegration.ranking.tasks

import tasks._
import org.saddle.order._
import tasks.circesupport._
import scala.concurrent.Future

import io.circe.generic.semiauto._
import vir.networkintegration.ranking
import org.saddle.Series
import org.saddle.circe._
import stat.cv.CVSplit
import io.circe.Codec
import io.circe.Encoder
import io.circe.Decoder
import stat.cv.KFold
import io.circe.generic.auto._
import vir.networkintegration.rwr.Hyperparameter
import vir.networkintegration.rwr
import fileutils.TempFile
import cats.effect.IO
import vir.networkintegration.notebook.Notebook1

case class MashupInput(
    edgeLists: List[Series[(String, String), Double]],
    hyperparameter: Hyperparameter,
    numberOfComponents: Int
)
object MashupInput {

  implicit val imputeInputCodec = deriveCodec[MashupInput]

}

object MashupTask {

  val task = Task[MashupInput, SharedFile]("mashup", 1) { input => implicit ctx =>
    val frame = rwr.mashup(
      input.edgeLists,
      input.hyperparameter,
      input.numberOfComponents,
      resourceAllocated.cpu
    )
    val file = TempFile.createTempFile(".saddle")
    Notebook1.writeBinaryFrameToFile(file, frame)

    SharedFile.apply(
      file,
      java.util.UUID.randomUUID.toString + ".saddle",
      true
    )
  }
  val diffusionState = Task[MashupInput, List[SharedFile]]("diffusionstate", 1) { input => implicit ctx =>
    val frames = rwr.diffusionStateMatrix(
      input.edgeLists,
      input.hyperparameter,
      resourceAllocated.cpu
    )
    IO.parSequenceN(1)(frames.map { frame =>
      val file = TempFile.createTempFile(".saddle")
      Notebook1.writeBinaryFrameToFile(file, frame)

      SharedFile.apply(
        file,
        java.util.UUID.randomUUID.toString + ".saddle",
        true
      )
    })
  }
}
