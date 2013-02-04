def mergesort(a: Array[Int]): Tuple2[Long, Array[Int]] = {
  if(a.size <= 1) {
    (0L, a)
  } else {
    val mid = a.size / 2
    val (left, right) = a splitAt mid
    val (lcount, lsorted) = mergesort(left)
    val (rcount, rsorted) = mergesort(right)
    val (scount, merged)  = merge(lsorted, rsorted)
    (lcount + rcount + scount, merged)
  }
}

def merge(left: Array[Int], right: Array[Int]) = {
  val size = left.size + right.size
  val res = Array.ofDim[Int](size)
  var count = 0L
  var i, j = 0
  for(k <- 0 until size) {
    if(i >= left.size) {
      res(k) = right(j)
      j += 1
    } else if(j >= right.size) {
      res(k) = left(i)
      i += 1
    } else if(left(i) < right(j)) {
      res(k) = left(i)
      i += 1
    } else {
      res(k) = right(j)
      count += (left.size - i)
      j += 1
    }
  }
  (count, res)
}


import scala.io.Source._

def stopwatch = System.currentTimeMillis
val numbers = fromFile("entrada").getLines.map(_.toInt).toArray
val start = stopwatch
val (count, sorted) = mergesort(numbers)
val stop = stopwatch
println("Took " + (stop - start))
println(count)
