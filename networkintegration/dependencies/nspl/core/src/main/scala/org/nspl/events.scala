package org.nspl

object Build {
  def apply[A](f: A)(pf: PartialFunction[(Option[A], Event,Boolean), A]): Build[A] = {
    case x => pf.applyOrElse(x, (x: (Option[A], Event, Boolean)) => x._1.getOrElse(f))
  }

  def const[A](f: => A): Build[A] = {
    val pf: PartialFunction[Event, A] = { case _ => f }
    withoutState(pf)
  }
  def withState[A](pf: PartialFunction[(Option[A], Event, Boolean), A]): Build[A] = pf
  def withoutState[A](pf: PartialFunction[Event, A]): Build[A] = {
    case (o, e,_) => pf(e)
  }
}

trait Events {

  trait Event {
    def plotAreaId : AnyRef
  }
  case class Scroll(
      v: Double,
      location: Point,
      plotArea: PlotAreaDescriptor
  ) extends Event {
    def plotAreaId: AnyRef =  plotArea.id
  }
  case class Drag(
      start: Point,
      current: Point,
      plotArea: PlotAreaDescriptor
  ) extends Event {
    def plotAreaId: AnyRef =  plotArea.id
  }
  case class Selection(
      start: Point,
      current: Point,
      plotArea: PlotAreaDescriptor
  ) extends Event {
    def plotAreaId: AnyRef =  plotArea.id
  }
  case object BuildEvent extends Event {
    def plotAreaId: AnyRef =  ""
  }

}
