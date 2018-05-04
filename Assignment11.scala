

object Assignment11 {
  
  def main(args: Array[String])
    {
//def vectorAdd (x: List[Int], y: List[Int]) = (x zip y).map({ case (a,b) => a+b} )
//def svProduct (i: Int, lst: List[Int]) = lst map (x => x * i)
//def vmProduct(v: List[Int], m: List[List[Int]]) = ((v zip m) map (x => svProduct(x._1, x._2))) reduce vectorAdd
//def matrixProduct(m: List[List[Int]], n: List[List[Int]]) = m map (x => vmProduct(x, n))
    
    		val list1 = List(1, 2, 3)  //tests
	  val list2 = List(4, 5, 6)
    
	  def vectorAdd (list1: List[Int], list2: List[Int]): List[Int] = for ((x, y) <- list1 zip list2) yield x + y
    def svProduct (i: Int, list1: List[Int]): List[Int] = for(x <- list1) yield (i * x)
    
    def vmProduct (list1: List[Int], list2: List[List[Int]]): List[Int] = 
      {
      val tmp = for ((a, b) <- list1 zip list2) yield (svProduct(a, b))
       
      tmp.reduce(vectorAdd(_, _))
      }
    		
    def matrixProduct(list1: List[List[Int]], list2: List[List[Int]]): List[List[Int]] = for (a <- list1)
                                                                                              yield (vmProduct(a, list2))
    
         println(vectorAdd(list1, list2))
    		 println(svProduct(2, list1))
    		 println(vmProduct(List(1,2,3), List(List(1,1), List(2,1), List(3,1))))
    		 println(matrixProduct(List( List(1, 2, 3), List(1, 1, 1) ), List( List(1, 1), List(2, 1), List(3, 1) )))
  }
}