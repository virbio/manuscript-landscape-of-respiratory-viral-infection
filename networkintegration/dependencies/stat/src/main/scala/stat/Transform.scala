package stat

trait Transform[I, O, T] { self: T =>
  def transform(data: I): O
  def concat[O2, T2](
      t2: T2
  )(
      implicit ev: T2 <:< Transform[O, O2, T2],
      ev1: T <:< Transform[I, O, T]
  ): CompositeTransform[I, O, O2, T, T2] =
    CompositeTransform(self, t2)
  def prepend[I2](fun: I2 => I)(
      implicit
      ev1: T <:< Transform[I, O, T]
  ) =
    CompositeTransform[I2, I, O, FunTransform[I2, I], T](
      FunTransform(fun),
      self
    )
  def map[O2](
      fun: O => O2
  )(
      implicit
      ev1: T <:< Transform[I, O, T]
  ): CompositeTransform[I, O, O2, T, FunTransform[O, O2]] =
    CompositeTransform(self, FunTransform(fun))
}

trait Transform1[I, T <: Transform[I, I, T]] extends Transform[I, I, T] {
  self: T =>
}

case class FunTransform[I, O](fun: I => O)
    extends Transform[I, O, FunTransform[I, O]] {
  def transform(i: I) = fun(i)
}

case class CompositeTransform[I, O, O2, T1, T2](first: T1, second: T2)(
    implicit ev1: T1 <:< Transform[I, O, T1],
    ev2: T2 <:< Transform[
      O,
      O2,
      T2
    ]
) extends Transform[I, O2, CompositeTransform[I, O, O2, T1, T2]] {
  def transform(data: I) = second.transform(first.transform(data))
}

trait TrainTransform[I, O, T <: Transform[I, O, T]] { self =>
  def train(data: I): T
  def concat[O2, T2 <: Transform[O, O2, T2]](
      t2: TrainTransform[O, O2, T2]
  ): TrainTransform[I, O2, CompositeTransform[I, O, O2, T, T2]] =
    new TrainTransform[I, O2, CompositeTransform[I, O, O2, T, T2]] {
      def train(data: I) = {
        val trainedI = self.train(data)
        val o = trainedI.transform(data)
        val trainedO = t2.train(o)
        trainedI.concat(trainedO)
      }
    }
  def map[O2](
      fun: O => O2
  ): TrainTransform[I, O2, CompositeTransform[I, O, O2, T, FunTransform[
    O,
    O2
  ]]] =
    new TrainTransform[I, O2, CompositeTransform[I, O, O2, T, FunTransform[
      O,
      O2
    ]]] {
      def train(data: I) = self.train(data).map(fun)
    }
}

trait TrainTransform1[I, T <: Transform[I, I, T]]
    extends TrainTransform[I, I, T]
