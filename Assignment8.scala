

object Assignment8 {

	def main(args : Array[String]) {

		abstract class Exp 
	case class Const(x:Int) extends Exp
	case class Var(x: String) extends Exp
	case class Times(x: Exp, y: Exp) extends Exp
	case class Pow(x: Exp, y: Int) extends Exp
	case class Plus(x: Exp, y: Exp) extends Exp

		val e = Times (Times (Var("x"), Var("y")), Plus (Var("x"), Const(3)))

				val e1 = Pow (Var("x"), 4);

	def print(e: Exp) : String = 
{
		e match 
		{
		case Const(n) => n.toString()
		case Var(n) => n.toString()
		case Plus(x: Exp, y: Exp) => ("(" + print(x) + "+" + print(y) + ")")
		case Times(x: Exp, y: Exp) => ("(" + print(x) + "*" + print(y) + ")")
		case Pow(x: Exp, y: Int) => ("(" + print(x) + "^" + y.toString() + ")")
		}
}

		def deriv(e: Exp, f: String) : Exp =
			{
					e match 
					{
					case Const(n) => Const(0)
					case Var(n) => if(n == f) Const(1) else Const(0)
					case Plus(n1, n2) => Plus(deriv(n1, f), deriv(n2, f))
					case Times(n1, n2) => Plus(Times(deriv(n1, f), n2), Times(n1, deriv(n2, f)))
					case Pow(n1, c) => Times(Times(Const(c), Pow(n1, c-1)), deriv( n1, f))
					}
			}

		def simp(e: Exp) : Exp = 
			{
					e match
					{
					case Plus(n1, Const(0)) => n1
					case Plus(Const(0), n2) => n2
					case Times(e1, Const(0)) => Const(0)
					case Times(Const(0), e2) => Const(0)
					case Times(e1, Const(1)) => e1
					case Times(Const(1), e2) => e2
					case Pow(e1, 1) => e1
					case Pow(e1, 0) => Const(1)
					case _ => e
					}
			}

		def simplify(e: Exp) : Exp =
			{
					e match
					{
					case Plus(e1, e2) => simp(Plus(simplify(e1), simplify(e2)))
					case Times(e1, e2) => simp(Times(simplify(e1), simplify(e2)))
					case Pow(e1, c) => simp(Pow(simplify(e1), c))
					case _ => e; 
					}				
			}

		println(print(e))
		println(print(e1))
		println(print(deriv(e, "x")))
		println(print(simplify(deriv(e, "x"))))

	}
}