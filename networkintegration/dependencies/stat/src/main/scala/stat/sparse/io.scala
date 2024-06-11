package stat.sparse

import org.saddle._
import org.saddle.order._
import java.nio.channels.WritableByteChannel
import org.saddle.binary.Writer
import org.saddle.binary.Reader
import java.nio.channels.ReadableByteChannel

object IO {
  def writeSMatIntoChannel(
      smat: SMat,
      channel: WritableByteChannel
  ) = {
    val lengths = Frame(smat.map(_.length).toVec)
    Writer.writeFrameIntoChannel(lengths, channel)
    smat.foreach { row =>
      Writer.writeFrameIntoChannel(Frame(row.values), channel)
    }
  }

  def readFrameFromChannel(
      channel: ReadableByteChannel
  ) = {
    for {
      lengths <- Reader.readFrameFromChannel[Int](channel)
      lengthsAsSeq = lengths.toMat.toVec.toSeq
      values <- {
        val v =
          lengthsAsSeq.map(
            _ => Reader.readFrameFromChannel[Double](channel)
          )
        if (v.forall(_.isRight))
          Right(v.map(_.right.get.colAt(0).mapIndex(_.toInt)))
        else Left(v.find(_.isLeft).get.left.get)
      }
    } yield {
      lengthsAsSeq
        .zip(values)
        .map { case (length, value) => SVec(value, length) }
        .toVector
    }

  }
}
