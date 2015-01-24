package ru.niner

import org.scalatest._

class RangeTreeSpec extends FlatSpec with Matchers {
  "Range Tree Search" should "find None when no points are in range" in {
    val rt = RangeTree.makeTree(List(new Point(List(-1,-1,-1)), new Point(List(0,0,0)), new Point(List(1,1,1))))
    RangeTree.doSearch(rt, new Point(List(-10,-10,-10)), new Point(List(-5,-5,-5))) should be (None)
  }

  it should "get the point equal to range if range is given as same point and that point exists in our set" in {
    val rp = new Point(List(0,0,0))
    val rt = RangeTree.makeTree(List(new Point(List(-1,-1,-1)), rp, new Point(List(1,1,1))))
    RangeTree.doSearch(rt,rp,rp) should be (Some(List(rp)))
  }

  it should "get all the points withing some specified range" in {
    val rp1 = new Point(List(-2,-2,-2,-2))
    val rp2 = new Point(List(-1,-1,1,-1))
    val rp3 = new Point(List(1,1,1,1))
    val rp4 = new Point(List(2,2,2,2))

    val rt = RangeTree.makeTree(List(rp3,rp2,rp1,rp4))
    RangeTree.doSearch(rt, new Point(List(-5,-5,-5,-5)), new Point(List(5,5,5,5))) should be (Some(List(rp1,rp2,rp3,rp4)))
  }
}
