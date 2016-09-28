package psksvp

/**
  * Created by psksvp on 23/9/16.
  * code copied/adapted from
  * https://github.com/jjfiv/qmm-scala
  */
object QuineMcCluskey
{
  import scala.annotation.tailrec

  /**
    *
    * @param x
    * @return
    */
  def bitCount(x: Int): Int =
  {
    @tailrec
    def bcrec(accum: Int, n: Int): Int = n match
    {
      case 0 => accum
      case x => bcrec(accum + (x % 2),(x>>>1))
    }
    bcrec(0, x)
  }

  /**
    *
    * @param zero_cubes
    * @param order
    * @return
    */
  def genImplicants(zero_cubes: List[Implicant], order: Int): List[Implicant] =
  {
    import scala.collection.mutable.{Set => MutableSet}

    //--- generate list and populate with zero-cubes
    var implicants = MutableSet[Implicant]()
    for(i <- zero_cubes) implicants += i

    //--- operate on current order until highest reached
    for(currentOrder <- 0 until order)
    {
      //--- grab all implicants of the current order
      val data = implicants.toList.filter(_.order == currentOrder)

      for(List(a, b) <- data.combinations(2))
      {
        if(a.canCombine(b))
        {
          a.prime = false
          b.prime = false

          val n = a.combine(b)

          implicants += n
        }
      }
    }

    implicants.toList
  }

  /**
    *
    * @param implicant
    * @param terms
    */
  class PrimeImplicant(val implicant: Implicant, val terms: Set[Int])
  {
    val tag = implicant.tag
    val order = implicant.order()

    //--- remove given terms from this data's effect
    def reduce(coveredTerms: Set[Int]) =  new PrimeImplicant(implicant, terms -- coveredTerms)

    //--- if this is higher order and contains all the minterms of the other
    def dominates(other: PrimeImplicant) = ((other.order <= order) && (other.terms.subsetOf(terms)))
    //--- whether this contains a given minterm or not
    def covers(minterm: Int) = terms.contains(minterm)
    //--- whether this is now empty
    def empty() = terms.size == 0

    override def equals(that: Any) = that match
    {
      case other: PrimeImplicant => hashCode == other.hashCode
      case _ => false
    }
    //override def hashCode = implicant.hashCode
    override def toString = terms.mkString("",",","")
  }

  /**
    *
    */
  object PITable
  {
    def solve(primeImplicants: List[Implicant],
              minterms: List[Int],
              vars: List[String]) =
    {
      val start = new PITable(primeImplicants.map(x => new PrimeImplicant(x, x.terms().toSet)),
                              minterms.toSet,
                              Set[Implicant](),
                              vars)

      bestSolution(start)
    }
    //@tailrec
    def bestSolution(t: PITable): PITable = t.finished match
    {
      case true  => t
      case false => val branches = for(row <- t.rows) yield bestSolution(reduceTable(t.selectRow(row)))
                    branches.minBy(_.cost(t.vars.length))
    }


    @tailrec
    def reduceTable(t: PITable): PITable = t.selectEssential match
    {
      case (true, newTable) => reduceTable(newTable.reduceRows)
      case (false, _)       => t.reduceRows
    }
  }

  /**
    *
    * @param rows
    * @param cols
    * @param results
    * @param vars
    */
  case class PITable(rows: List[PrimeImplicant],
                     cols: Set[Int],
                     results: Set[Implicant],
                     vars: List[String])
  {

    def cost(order: Int) =
    {
      results.foldLeft(0){_ + _.cost(order)}
    }

    def finished = cols.isEmpty //cols.size == 0

    def selectRow(row: PrimeImplicant) =
    {
      val nRows = rows.filter(_ != row).map(_.reduce(row.terms))
      val nCols = cols -- row.terms
      val nRes  = results + row.implicant
      this.copy(rows=nRows, cols=nCols, results=nRes)
    }

    def reduceRows =
    {
      var nRows = rows.filter(!_.empty())
      for(List(a,b) <- rows.combinations(2))
      {
        if(a dominates b)
        {
          nRows = nRows.filter(_ != b)
        }
        else if(b dominates a)
        {
          nRows = nRows.filter(_ != a)
        }
      }
      this.copy(rows=nRows)
    }

    def rowsForMinterm(m: Int) = (for (row <- rows if row.covers(m)) yield row).filter(_ != ())

    def selectEssential =
    {
      var newTable = this
      var done = false
      var effective = false

      while(!done)
      {
        done = true
        for (m <- cols)
        {
          newTable.rowsForMinterm(m) match
          {
            case List(x: PrimeImplicant) =>
            {
              done = false
              effective = true
              newTable = newTable.selectRow(x)
            }
            case _ => ()
          }
        }
      }

      (effective, newTable)
    }

    def toSumOfProducts(vars: List[String]) =
    {
      assert(finished)

      results.map(_.withVars(vars)).toList.sorted.reduceLeft(_ + " + " + _)
    }
  }

  /**
    *
    * @param minterm
    * @param tag
    * @param group
    */
  class Implicant(val minterm: Int, val tag:Int=1, val group:List[Int]=Nil)
  {
    var prime: Boolean = true

    def cost(order: Int): Int =
    {
      order - group.size
    }

    def order(): Int =
    {
      group.length
    }

    def terms() =
    {
      var terms: List[Int] = List(minterm)
      for (difference <- group)
      {
        terms = terms ::: terms.map(_ + difference)
      }
      terms
    }

    def canCombine(other: Implicant): Boolean =
    {
      //--- if the other one is less than this, don't bother comparing
      //if (other.minterm < minterm)
      //return false

      //--- only include ones that exist in at least one function
      if ((other.tag & tag) == 0)
        return false

      //--- if differences are not equivalent, don't bother comparing
      if (group != other.group)
        return false

      def bitdist(x: Int, y: Int) = bitCount(x ^ y)

      //--- difference needs to be just one bit
      if (bitdist(other.minterm, minterm) != 1)
        return false

      return true
    }

    override def equals(that: Any) = that match
    {
      case other: Implicant => hashCode == other.hashCode
      case _                => false
    }

    override def hashCode = terms().hashCode

    def combine(other: Implicant): Implicant =
    {
      val newtag = other.tag & tag
      val diff = math.abs(other.minterm - minterm)
      val newgroup = (group ::: List(diff)).sorted
      val newmt = if (minterm > other.minterm) other.minterm else minterm

      new Implicant(newmt, newtag, newgroup)
    }

    override def toString = terms().mkString("<", ",", ">")

    def printTerms() = println(terms().mkString("(", ",", ")"))

    def print()
    {
      printf("%d %s tag=%d %s\n", minterm, group.mkString("(", ",", ")"), tag, if (prime)
      {
        "prime"
      }
      else
      {
        ""
      })
    }

    def withVars(vars: List[String]): String =
    {
      val weights = (0 until vars.length).map(1 << _).reverse
      val varByWeight = (weights zip vars).toMap
      //println(varByWeight)

      val expression = for (w <- weights) yield
                       {
                         if (!group.contains(w))
                         {
                           if ((minterm & w) != 0)
                             varByWeight(w)
                           else
                             varByWeight(w) + "'"
                         }
                         else
                         {
                           ""
                         }
                       }

      expression.filter(_ != "").reduceLeft(_ + _)
    }
  }
}
