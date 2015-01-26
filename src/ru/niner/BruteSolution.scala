package ru.niner

object BruteSolution {
  def getPoints(points : List[Point], left : Point, right : Point) : Option[List[Point]] = {
    // match all the rest of parameters of this single found point
    def matchPoint(piq : Point, left : Point, right : Point, dim : Int, mDim : Int) : Option[Point] = {
      if (dim >= mDim) Some(piq) else {
        if (left.coordinates(dim) <= piq.coordinates(dim) && right.coordinates(dim) >= piq.coordinates(dim)) matchPoint(piq,left,right,dim+1,mDim) else None
      }
    }
    var res : List[Point] = List()
    for (point <- points) {
      matchPoint(point,left,right,0,point.coordinates.length) match {
        case Some(p) => res = res ++ List(p)
        case None =>
      }
    }
    res.length match {
      case 0 => None
      case _ => Some(res)
    }
  }
}

object Testing extends App {
  val a = List(new Point(List(0,0,0,0)), new Point(List(1,1,-1,-1)), new Point(List(1,1,1,1)),new Point(List(-1,-1,-1,-1)),new Point(List(-1,-1,1,1)),new Point(List(0,0,1,1)),
    new Point(List(1,1,0,0)))
  println(BruteSolution.getPoints(a,new Point(List(-1,-1,-1,-1)), new Point(List(1,1,1,1))))
}
