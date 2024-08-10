object NumberCategorizer {

    val categorizeNumber: Int => String = {
        case x if x % 3 == 0 && x % 5 == 0 => "Multiple of Both Three and Five"
        case x if x % 3 == 0 =>  "Multiple of Three"
        case x if x % 5 == 0 =>  "Multiple of Five"
        case _ => "Not a Multiple of Three or Five"
    }

}

object Main extends App{

    println("Enter an Integer:")
    val input = scala.io.StdIn.readLine().toInt

    
    println(NumberCategorizer.categorizeNumber(input))
}