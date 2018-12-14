package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {

    def sort (d: Seq[Int]):Seq[Int]  = d match {
      case Nil => Nil-
      case a:: Nil => Seq(a)
      case d => {
        val (r, l) = split (d)
        merge (Seq(), sort(r), sort(l))
      }
    }

    def split (d: Seq[Int]) : (Seq[Int], Seq[Int]) = {
      val mid = d.size/2
      d.splitAt(mid)
    }

    def merge(findata: Seq[Int] = Seq(), adata: Seq[Int], bdata: Seq[Int]): Seq[Int] = {
      if (adata.nonEmpty && bdata.nonEmpty) {
        if (adata.head < bdata.head) merge(findata :+ adata.head, adata.tail, bdata)
        else merge(findata :+ bdata.head, adata, bdata.tail)
      }
      else if (adata.isEmpty) findata ++ bdata
      else if (bdata.isEmpty) findata ++ adata
      else findata
    }

    sort(data)
  }

  val a: Seq[Int] = Seq(7,6,5,4,3,2,1)
  println(mergeSort(a))
}
