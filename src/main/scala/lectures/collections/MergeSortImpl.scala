package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {


    def split (a: Int, b: Int) : List[(Int, Int)] = {
      if (b - a> 1) split(a, (a+b)/2) ++ split (1 + (a+b)/2 , b)
      else if  (b - a == 1) List((a,b))
      else  List((a,a))
    }
    //  if (data.size>1) mergeSort(data.take(data.size/2)) ++ mergeSort(data.drop(data.size/2))
    // else

    def merge(findata: Seq[Int], adata: Seq[Int], bdata: Seq[Int]): Seq[Int] = {
      if (adata.nonEmpty && bdata.nonEmpty) {
        if (adata.head < bdata.head) merge(findata :+ adata.head, adata.tail, bdata)
        else merge(findata :+ bdata.head, adata, bdata.tail)
      }
      else if (adata.isEmpty) findata ++ bdata
      else if (bdata.isEmpty) findata ++ adata
      else findata
    }

    data
  }

}
