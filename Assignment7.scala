

object Assignment7 {
	def main(args: Array[String]) {

		//val list1 = List(1, 2, 3)  //tests
	  //val list2 = List(4, 5, 6)


				def vectorAdd(list1: List[Int], list2: List[Int]): List[Int] = 
			{
					//						val finalList = list1.zip(list2)
					//								finalList map { case (x, y) => x + y}

					list1.zip(list2).map { case (x, y) => x + y}
			}

		def svProduct(x: Int, list1: List[Int]): List[Int] =
			{
					//		        val finalList = list1
					//		        finalList map { case (y) => x * y}

					list1.map { case (y) => x * y }
					// x map { case (y) => x*y } why doesnt this work?
			}

		def vmProduct(list1: List[Int], list2: List[List[Int]]) : List[Int] = 
			{

					//val store: List[List[Int]] = list1.zip(list2).map{ case (x, y) => svProduct(x, y) }
					val store: List[List[Int]] = list1.zip(list2).map(a => svProduct(a._1, a._2))

					store.reduce((x,y) => vectorAdd(x,y))
					//store.map { case (x, y) => vectorAdd(x, y) }
					//fun vmProduct(v, m) = reduce vectorAdd (map svProduct (zip (v, m))); 
					//l2.reduce(vectorAdd(map svProduct (l1.zip(l2))
					
			}

		def matrixProduct(list1: List[List[Int]], list2: List[List[Int]]) : List[List[Int]] =
			{
					//fun matrixProduc(m, n) = map (fn x => vmProduct(x, n)) m;
					// list1.map((x, y) => vmProduct(x,y))
					list1.map { case (x) => vmProduct(x,list2) }
			}
		//test + print
//		println(matrixProduct(List(List(1,2,3), List(1,1,1)),List(List(1,1), List(2,1), List(3,1))))
//		println(vmProduct(List(1,2,3), List(List(1,1), List(2,1), List(3,1))))
//		println(vectorAdd(list1, list2))
//		println(svProduct(2, list1))

		//test cases from assignment

		println(vectorAdd (List(1,2,3), List(4,5,6)))
		println(svProduct(2, List(1,2,3)))
		println(vmProduct(List(1,2,3), List(List(1,1), List(2,1), List(3,1))))
		println(matrixProduct(List( List(1, 2, 3), List(1, 1, 1) ),
				List( List(1, 1), List(2, 1), List(3, 1) )))
	}
}
