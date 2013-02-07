import scala.io.Source._
import scala.collection.mutable.IndexedSeq

def quicksort(arr: IndexedSeq[Int], calcPivot: IndexedSeq[Int] => Int): Long = {
  if(arr.size <= 1) 0L
  else {
    val pivot  = calcPivot(arr)
    val tmp    = arr(0)
    arr(0)     = arr(pivot)
    arr(pivot) = tmp
    val mid    = partition(arr)
    arr.size + quicksort(arr.view(0, mid - 1), calcPivot) + quicksort(arr.view(mid, arr.size), calcPivot) - 1
  }
}

def partition(arr: IndexedSeq[Int]) = {
  var i = 1
  val pivot = arr(0)
  for(j <- 1 until arr.size) {
    var el = arr(j)
    if(el <= pivot) {
      //swap
      arr(j) = arr(i)
      arr(i) = el
      i += 1
    }
  }
  arr(0) = arr(i - 1)
  arr(i - 1) = pivot
  i
}

def firstPivot(arr: IndexedSeq[Int]) = 0
def lastPivot(arr: IndexedSeq[Int]) = arr.size - 1
def medianPivot(arr: IndexedSeq[Int]) = {
  val idx = if(arr.size % 2 == 0) (arr.size / 2) - 1
            else arr.size / 2
  val first = arr(0)
  val mid   = arr(idx)
  val last  = arr(arr.size - 1)
  if((first < mid && mid < last) || (last < mid && mid < first)) {
    idx
  } else if((mid < first && first < last) || (last < first && first < mid)) {
    0
  } else {
    arr.size - 1
  }
}

def stopwatch = System.currentTimeMillis
val numbers = fromFile("QuickSort.txt").getLines.map(_.toInt).toArray
val originalNumbers = Array.ofDim[Int](numbers.size)
numbers copyToArray originalNumbers

val start1 = stopwatch
val count1 = quicksort(numbers, firstPivot)
val stop1 = stopwatch

println(count1)
println("Took " + (stop1 - start1))

val numbers2 = Array.ofDim[Int](numbers.size)
originalNumbers copyToArray numbers2

val start2 = stopwatch
val count2 = quicksort(numbers2, lastPivot)
val stop2 = stopwatch

println(count2)
println("Took " + (stop2 - start2))

val numbers3 = Array.ofDim[Int](numbers.size)
originalNumbers copyToArray numbers3

val start3 = stopwatch
val count3 = quicksort(numbers3, medianPivot)
val stop3 = stopwatch

println(count3)
println("Took " + (stop3 - start3))
