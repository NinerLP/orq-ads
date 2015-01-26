package ru.niner

import scala.util.Random

object Tester extends App {

  def time[A](f : => A) = {
    val s = System.nanoTime()
    val ret = f
    println("time: "+ (System.nanoTime()-s)/1e6 + "ms")
    ret
  }

  val DIMENSIONS = 8
  val NUMBEROFPOINTS = 1000

  //generate points
  var points : List[Point] = List()
  for (i <- 1 to NUMBEROFPOINTS) {
    var b : List[Int] = List()
    for (k <- 1 to DIMENSIONS) {
      b = b ++ List(Random.nextInt())
    }
    points = points ++ List(new Point(b))
  }

  //generate borders
  var leftC : List[Int] = List()
  var rightC : List[Int] = List()
  for (i <- 1 to DIMENSIONS) {
    var a = Random.nextInt()
    var b = Random.nextInt()
    if (a > b) {
      val tmp = b
      b = a
      a = tmp
    }
    leftC = leftC ++ List(a)
    rightC = rightC ++ List(b)
  }

  val pLeft = new Point(leftC)
  val pRight = new Point(rightC)

  // build tree
  println("Building tree")
  val tree = time {RangeTree.makeTree(points)}

  // get answer from tree
  println("Searching tree")
  val result = time { RangeTree.doSearch(tree,pLeft,pRight) }

  // get answer brute
  println("Searching Brute")
  val bruteResult = time { BruteSolution.getPoints(points, pLeft, pRight)}

  for (i <- 1 to DIMENSIONS) {
    //println(leftC(i-1) + " - " + rightC(i-1))
  }
  println(result)
  println(bruteResult)
  var size = 0
  (result, bruteResult) match {
    case (Some(a), Some(b)) => { println(a.length-b.length); size = a.length }
    case (_,_) => ()
  }
}

