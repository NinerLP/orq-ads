package ru.niner

/*
 Range Tree, Empty is not used, but let it be there
 */
abstract class RangeTree
case object Empty extends RangeTree
case class Leaf(point : Point) extends RangeTree
// list of all poinst in this node sorted by current dim coordinate, left node, right node, range of points, RangeTree for next dimension, cascadelist if in last dimension
case class Node(points : List[Point], left : RangeTree, right : RangeTree, leftMin : Int, rightMax : Int, nextDim : RangeTree, cascadeList : Cascade) extends RangeTree

abstract class Cascade
case object NoCascade extends Cascade
case class CascadeList(cascadeList : List[CascadeEntry]) extends Cascade

object RangeTree {
  /*
    Build a d-dimensional range tree from list of points
    dim is current dimension being computed, mDim - total number of dimensions
   */
  def toTree(pts : List[Point], dim : Int, mDim : Int) : RangeTree = {
    /*
      simply build tree
     */
    def toTreeEx(pts : List[Point], n : Int, dim : Int, mDim : Int) : (List[Point], RangeTree) = {
      if (n == 0) {
        (pts, Empty) }
      else if (n == 1) {
        (pts, Leaf(pts(0)))
      } else {
        val (left, right) = pts.splitAt(n/2)
        val (_, lt) = toTreeEx(left, left.length, dim, mDim)
        val (xr, rt) = toTreeEx(right, right.length, dim, mDim)

        //  inb4 adding cascades, log^d ver
        // (xr, Node(pts, lt, rt, left(0).coordinates(dim), right(right.length-1).coordinates(dim), if (dim + 1 >= mDim) null else RangeTree.toTree(pts,dim+1,mDim), NoCascade))
        (xr, Node(pts, lt, rt, left(0).coordinates(dim), right(right.length-1).coordinates(dim), RangeTree.toTree(pts,dim+1,mDim), NoCascade))
      }
    }

    /*
      build last two dimensions of tree with cascading instead of extra trees
     */
    def toTreeLast(pts : List[Point], n : Int, dim : Int) : (List[Point], RangeTree) = {
      if (n == 0) {
        (pts, Empty)
      } else if (n == 1) {
        (pts, Leaf(pts(0)))
      } else {
        // split points by "x"
        val (left, right) = pts.splitAt(n/2)

        var lCount = 0
        var rCount = 0
        var lIndex = 0
        var rIndex = 0
        // sort point by "y" to build cascading list for search
        val leftSorted = left.sorted(new PointOrdering(dim+1))
        val rightSorted = right.sorted(new PointOrdering(dim+1))
        var cascadeList : List[CascadeEntry] = List()
        var prev : Point = null
        var fst = true

        // Build a cascade list for this entry
        while (lCount < leftSorted.length && rCount < rightSorted.length) {
          val leftP = leftSorted(lCount)
          val rightP = rightSorted(rCount)
          if (leftP.coordinates(dim+1) <= rightP.coordinates(dim+1)) {
            if (lCount + rCount > 0) {
              //val leftPrev = leftSorted(lCount-1)
              if (prev.coordinates(dim+1) != leftP.coordinates(dim+1)) {
                lIndex = lCount
                rIndex = rCount
              }
            }
            cascadeList = cascadeList ++ List(new CascadeEntry(leftP,lIndex,rIndex))
            lCount += 1
            prev = leftP
          } else {
            if (rCount + lCount > 0) {
              //val rightPrev = rightSorted(rCount-1)
              if (prev.coordinates(dim+1) != rightP.coordinates(dim+1)) {
                rIndex = rCount
                lIndex = lCount
              }
            }
            cascadeList = cascadeList ++ List(new CascadeEntry(rightP,lIndex,rIndex))
            rCount += 1
            prev = rightP
          }
        }

        while (lCount < leftSorted.length) {
          val leftP = leftSorted(lCount)
          if (lCount > 0) {
            val leftPrev = leftSorted(lCount-1)
            if (leftPrev.coordinates(dim+1) != leftP.coordinates(dim+1)) lIndex = lCount
          }
          cascadeList = cascadeList ++ List(new CascadeEntry(leftP,lIndex,rIndex))
          lCount += 1
        }

        while (rCount < rightSorted.length) {
          val rightP = rightSorted(rCount)
          if (rCount > 0) {
            val rightPrev = rightSorted(rCount-1)
            if (rightPrev.coordinates(dim+1) != rightP.coordinates(dim+1)) rIndex = rCount
          }
          cascadeList = cascadeList ++ List(new CascadeEntry(rightP,lIndex,rIndex))
          rCount += 1
        }

        // and then build next trees
        val (_, lt) = toTreeLast(left, left.length, dim)
        val (xr, rt) = toTreeLast(right, right.length, dim)

        (xr, Node(pts, lt, rt, left(0).coordinates(dim), right(right.length-1).coordinates(dim), null, CascadeList(cascadeList)))
      }
    }

    if (dim + 2 >= mDim) {
      // last 2 dimensions, build last tree with cascade by last dimension
      toTreeLast(pts.sorted(new PointOrdering(dim)), pts.length, dim)._2
    } else {
      toTreeEx(pts.sorted(new PointOrdering(dim)), pts.length, dim, mDim)._2
    }
  }

  //function without extra parameters, assumes all points to be of same dimension size
  def makeTree(pts : List[Point]): RangeTree = {
    toTree(pts, 0, pts(0).coordinates.length)
  }

  /*
    Search a d-dimensional range tree, left point coordinates are always considered to be less or equal to the right limit coordinates
   */
  def searchTree(tree : RangeTree, left : Point, right : Point, dim : Int, mDim : Int) : Option[List[Point]] = {

    // match all the rest of parameters of this single found point
    def matchPoint(piq : Point, left : Point, right : Point, dim : Int, mDim : Int) : Option[Point] = {
      if (dim >= mDim) Some(piq) else {
        if (left.coordinates(dim) <= piq.coordinates(dim) && right.coordinates(dim) >= piq.coordinates(dim)) matchPoint(piq,left,right,dim+1,mDim) else None
      }
    }

    // find first cascade element >= than our left side by "y"
    /*def binSearch(target : Int, cascade : Cascade, dim : Int): Int = {
      cascade match {
        case CascadeList(l) => {
          var low = 0
          var high = l.length - 1
          while (low <= high) {
            var mid = (low+high)/2
            if (l(mid).point.coordinates(dim) > target) high = mid - 1 else if (l(mid).point.coordinates(dim) < target) low = mid + 1 else
            {
              while (mid > 0 && l(mid).point.coordinates(dim) >= target) {
                mid -= 1
              }
              return mid
            }
          }
          //println(high)
          if (high < 0) 0 else high
        }
      }
    }*/

    def binSearch(target : Int, cascade : Cascade, dim : Int) : Int = {
      cascade match {
        case CascadeList(l) => {
          var low = -1
          var high = l.length
          while (low < high - 1) {
            var mid = (low+high)/2
            if (l(mid).point.coordinates(dim) > target) high = mid else if (l(mid).point.coordinates(dim) < target) low = mid else
            {
              while (mid > 0 && l(mid).point.coordinates(dim) >= target) {
                mid -= 1
              }
              return mid
            }
          }
          //println(high)
          if (high < 0) 0 else high
        }
      }
    }

    if (dim+2 >= mDim)
      //we are actually in last tree now
      //fwd to searchlast
      tree match {
        case Leaf(x) => matchPoint(x, left, right, dim, mDim) match {
          case Some(p) => Some(List(p))
          case None => None
        }
        case Node(_,_,_,_,_,_,cascadeList) => searchLast(tree,left.coordinates(dim), left.coordinates(dim+1),
          right.coordinates(dim),right.coordinates(dim+1),dim,binSearch(left.coordinates(dim+1),cascadeList, dim+1))
      }
    else
    tree match {
      // in leaf just check found point
      case Leaf(x) => matchPoint(x,left,right,dim,mDim) match {
        case Some(p) => Some(List(p))
        case None => None
      }
      // in node do actual work
      case Node(points, lNode, rNode, leftMin, rightMax, nextDim, cascadeList) => {
        if (leftMin >= left.coordinates(dim) && rightMax <= right.coordinates(dim)) {
          if (dim+1 >= mDim) Some(points) else searchTree(nextDim, left, right, dim+1, mDim)
        } else if (points(0).coordinates(dim) > right.coordinates(dim) || points(points.length-1).coordinates(dim) < left.coordinates(dim)) {
          None
        /*else if (leftMin >= left.coordinates(dim)) {
          searchTree(lNode, left, right, dim, mDim)
        } else if (rightMax <= right.coordinates(dim)) {
          searchTree(rNode, left, right, dim, mDim)*/
        } else {
          (searchTree(lNode, left, right, dim, mDim), searchTree(rNode, left, right, dim, mDim)) match {
            case (Some(l), Some(r)) => Some(l++r)
            case (Some(l), _) => Some(l)
            case (_, Some(r)) => Some(r)
            case (_,_) => None
          }
        }
      }
      case Empty => None
    }
  }

  def searchLast(tree : RangeTree, leftX : Int, leftY : Int, rightX : Int, rightY : Int, dim : Int, cascadeIdx : Int) : Option[List[Point]] = {
    tree match {
      // in leaf just check two coordinates here
      case (Leaf(p)) => if (p.coordinates(dim) >= leftX && p.coordinates(dim) <= rightX && p.coordinates(dim+1) >= leftY && p.coordinates(dim+1) <= rightY) Some(List(p)) else None
      // in Node go as previosly, but save index by Y in cascading lists, to search last dimension just in cascadelist
      case Node(points, lNode, rNode, leftMin, rightMax, _, cascadeList : CascadeList) => {
        if (leftMin >= leftX && rightMax <= rightX) {
          //search just here, it's enoguh
          //println(cascadeIdx)
          var result : List[Point] = List()
          var count = cascadeIdx
          while (count < cascadeList.cascadeList.length && cascadeList.cascadeList(count).point.coordinates(dim+1) <= rightY) {
            //println(cascadeList.cascadeList(count).point.coordinates(dim+1) + " | " + rightY)
            result = result ++ List(cascadeList.cascadeList(count).point)
            count += 1
          }
          if (result.isEmpty) None else Some(result)
        } else if (points(0).coordinates(dim) > rightX || points(points.length-1).coordinates(dim) < leftX) {
          None
         /*else if (leftMin >= leftX) {
          searchLast(lNode, leftX, leftY, rightX, rightY, dim, cascadeList.cascadeList(cascadeIdx).leftIndex)
        } else if (rightMax <= rightX) {
          searchLast(rNode, leftX, leftY, rightX, rightY, dim, cascadeList.cascadeList(cascadeIdx).rightIndex)*/
        } else {
          if (cascadeIdx >= cascadeList.cascadeList.length) {
            None
          } else {
            val left = searchLast(lNode, leftX, leftY, rightX, rightY, dim, cascadeList.cascadeList(cascadeIdx).leftIndex)
            val right = searchLast(rNode, leftX, leftY, rightX, rightY, dim, cascadeList.cascadeList(cascadeIdx).rightIndex)
            (left, right) match {
              case (Some(l), Some(r)) => Some(l ++ r)
              case (Some(l), None) => Some(l)
              case (None, Some(r)) => Some(r)
              case (None, None) => None
            }
          }
        }
      }
    }
  }

  def doSearch(tree : RangeTree, left : Point, right : Point): Option[List[Point]] = {
    searchTree(tree, left, right, 0, left.coordinates.length)
  }
}

class Point(val coordinates : List[Int]) {
  override def toString : String = {
    coordinates.mkString("[", ", ", "]")
  }
}

class PointOrdering(val dimension : Int) extends Ordering[Point] {
  def compare(a : Point, b : Point) = a.coordinates(dimension) compare b.coordinates(dimension)
}

class CascadeEntry(val point : Point, val leftIndex : Int, val rightIndex : Int)

object Testing extends App {
  val a = List(new Point(List(0,0,0,0)), new Point(List(1,1,-1,-1)), new Point(List(1,1,1,1)),new Point(List(-1,-1,-1,-1)),new Point(List(-1,-1,1,1)),new Point(List(0,0,1,1)),
  new Point(List(1,1,0,0)))
  val tr = RangeTree.makeTree(a)
  println("")
  println(RangeTree.doSearch(tr,new Point(List(-1,-1,-1,-1)), new Point(List(1,1,1,1))))

}