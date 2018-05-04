

object Assignment10 {
	//referenced stack overflow, youtube, and used designs and concepts 
	//worked with Breanna, James, Peter
	def main(args: Array[String]) {
		val x = List(1.47,1.50,1.52,1.55,1.57,1.60,1.63,1.65,1.68,1.70,1.73,1.75,1.78,1.80,1.83)
				val y = List(52.21,53.12,54.48,55.84,57.20,58.57,59.93,61.29,63.11,64.47,66.28,68.10,69.92,72.19,74.46)
				println("x = " + x)
				println("y = " + y)

				val meanY = y.reduce(_+_) / y.length//sum(y) / y.length
				val meanX = x.reduce(_+_) / x.length//sum(x) / x.length

				printf("alpha = %.3f with std = %.3f\n", alpha(x,y), alpha_std(x,y))
				printf("beta = %.3f with std = %.3f\n", beta(x,y), beta_std(x,y))

				//				x = List(1.47, 1.5, 1.52, 1.55, 1.57, 1.6, 1.63, 1.65, 1.68, 1.7,
				//						1.73, 1.75, 1.78, 1.8, 1.83)
				//				y = List(52.21, 53.12, 54.48, 55.84, 57.2, 58.57, 59.93, 61.29,
				//						63.11, 64.47, 66.28, 68.1, 69.92, 72.19, 74.46)

				//				alpha = -39.062 with std = 2.938
				//				beta = 61.272 with std = 1.776

				def sqr(x:Double) = x*x 

				def alpha(x: List[Double], y: List[Double]): Double =
			{

					meanY - (beta(x, y) * meanX)
			}
		def beta (x: List[Double], y: List[Double]): Double =
			{
					x.map(x => x - meanX).zip(y.map(a => a - meanY)).map(b => b._1 * b._2 ).reduce(_+_) / x.map(x => sqr(x - meanX)).reduce(_+_)
					//sum((x.map(x => x- meanX).zip(y.map(x => x - meanY))).map { case (x, y) => x * y}) / sqr(sum((x.map(x => x - meanX))))
					// /:(x - meanX)(y - meanY) / /:(x - meanX).pow(2)
			}

		def alpha_std(x: List[Double], y: List[Double]): Double =
			{
					val useThis = beta_std(x, y)

							beta_std(x ,y) * (Math.sqrt((1.0/x.length) * x.map(x => sqr(x)).reduce(_+_)))
			}

		def wierdE(x: List[Double], y: List[Double]): List[Double] =
			{
					val alphaTemp = alpha(x, y)
							val betaTemp = beta(x, y)
							val first = y.map(y => y - alphaTemp)
							val second = x.map(x => x * betaTemp)

							//first.zip(second).map { case (x, y) => x - y}
							first.zip(second).map(a => a._1 - a._2)
							//y.map(y => y - alpha(x, y)) - (x.map(x => x * beta(x, y)) 
			}

		def beta_std (x: List[Double], y: List[Double]): Double = 
			{ 
					Math.sqrt(((1.0/(y.length - 2.0)) * wierdE(x, y).map(bigE => sqr(bigE)).reduce(_+_)) / (x.map(a => sqr(a - meanX)).reduce(_+_)))
			}
	}

}