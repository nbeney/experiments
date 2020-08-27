import jp.kobe_u.copris._
import jp.kobe_u.copris.dsl._

// Let us assume that there are five houses of different colors next to each other on the same road. In each house lives
// a man of a different nationality. Every man has his favorite drink, his favorite brand of cigarettes, and keeps pets
// of a particular kind.

// The Englishman lives in the red house.
// The Swede keeps dogs.
// The Dane drinks tea.
// The green house is just to the left of the white one.
// The owner of the green house drinks coffee.
// The Pall Mall smoker keeps birds.
// The owner of the yellow house smokes Dunhills.
// The man in the center house drinks milk.
// The Norwegian lives in the first house.
// The Blend smoker has a neighbor who keeps cats.
// The man who smokes Blue Masters drinks bier.
// The man who keeps horses lives next to the Dunhill smoker.
// The German smokes Prince.
// The Norwegian lives next to the blue house.
// The Blend smoker has a neighbor who drinks water.

// The question to be answered is: Who keeps fish?

// See also:
// * https://en.wikipedia.org/wiki/Zebra_Puzzle
// * http://bach.istc.kobe-u.ac.jp/copris/

object Riddle extends App {

  val N = 5

  // Assumption: #colors = #nationalities = #drinks = #cigarettes = #pets = 5
  val colorVars = Color.all.map(_.`var`)
  val nationalityVars = Nationality.all.map(_.`var`)
  val drinkVars = Drink.all.map(_.`var`)
  val cigaretteVars = Cigarette.all.map(_.`var`)
  val petVars = Pet.all.map(_.`var`)

  addConstraints

  if (find) {
    for (sol <- solutions) {
      showSolution(sol)
      println()
    }
  } else {
    println("No solutions found!")
  }

  def addConstraints = {
    // Uniqueness constraints.
    add(Alldifferent(colorVars: _*))
    add(Alldifferent(nationalityVars: _*))
    add(Alldifferent(drinkVars: _*))
    add(Alldifferent(cigaretteVars: _*))
    add(Alldifferent(petVars: _*))

    // The Englishman lives in the red house.
    add(Nationality.english `with` Color.red)
    // The Swede keeps dogs.
    add(Nationality.swede `with` Pet.dogs)
    // The Dane drinks tea.
    add(Nationality.dane `with` Drink.tea)
    // The green house is just to the left of the white one.
    add(Color.white leftNeighbourOf Color.green)
    // The owner of the green house drinks coffee.
    add(Color.green `with` Drink.coffee)
    // The Pall Mall smoker keeps birds.
    add(Cigarette.pallMall `with` Pet.birds)
    // The owner of the yellow house smokes Dunhills.
    add(Color.yellow `with` Cigarette.dunhills)
    // The man in the center house drinks milk.
    add(drinkVars(N / 2) === Drink.milk.index)
    // The Norwegian lives in the first house.
    add(nationalityVars(0) === Nationality.norwegian.index)
    // The Blend smoker has a neighbor who keeps cats.
    add(Cigarette.blend neighbourOf Pet.cats)
    // The man who smokes Blue Masters drinks beer.
    add(Cigarette.blueMasters `with` Drink.beer)
    // The man who keeps horses lives next to the Dunhills smoker.
    add(Pet.horses neighbourOf Cigarette.dunhills)
    // The German smokes Prince.
    add(Nationality.german `with` Cigarette.prince)
    // The Norwegian lives next to the blue house.
    add(Nationality.norwegian neighbourOf Color.blue)
    // The Blend smoker has a neighbor who drinks water.
    add(Cigarette.blend neighbourOf Drink.water)
  }

  def showSolution(sol: Solution) = {
    def collect(values: List[Value]) = {
      val symbol = values(0).symbol
      (0 until N)
        .map(i => values(sol.intValues(symbol(i))).name)
        .map(_.formatted("%-12s"))
    }

    // Assumption: #colors = #nationalities = #drinks = #cigarettes = #pets = 5
    println(collect(Color.all).mkString(" "))
    println(collect(Nationality.all).mkString(" "))
    println(collect(Drink.all).mkString(" "))
    println(collect(Cigarette.all).mkString(" "))
    println(collect(Pet.all).mkString(" "))
  }

  // Values

  trait Value {
    def index: Int

    def name: String

    def symbol: Symbol

    // Assumption: #colors = #nationalities = #drinks = #cigarettes = #pets = 5
    def `var`(): Var =
      int(symbol(index), 0, N - 1)

    def `with`(other: Value): Constraint =
      (0 until N)
        .map(i => symbol(i) === index && other.symbol(i) === other.index)
        .reduce((a: Constraint, b: Constraint) => a || b)

    def neighbourOf(other: Value): Constraint =
      (this leftNeighbourOf other) || (this rightNeighbourOf other)

    def leftNeighbourOf(other: Value): Constraint =
      (1 until N)
        .map(i => symbol(i) === index && other.symbol(i - 1) === other.index)
        .reduce((a: Constraint, b: Constraint) => a || b)

    def rightNeighbourOf(other: Value): Constraint =
      (0 until N - 1)
        .map(i => symbol(i) === index && other.symbol(i + 1) === other.index)
        .reduce((a: Constraint, b: Constraint) => a || b)
  }

  // Colors

  case class Color(index: Int, name: String) extends Value {
    override def symbol: Symbol = 'col
  }

  object Color {
    val gen = Stream.from(0).toIterator

    val red = Color(gen.next, "red")
    val green = Color(gen.next, "green")
    val white = Color(gen.next, "white")
    val yellow = Color(gen.next, "yellow")
    val blue = Color(gen.next, "blue")

    val all = List(red, green, white, yellow, blue)
  }

  // Nationalities

  case class Nationality(index: Int, name: String) extends Value {
    override def symbol: Symbol = 'nat
  }

  object Nationality {
    val gen = Stream.from(0).toIterator

    val english = Nationality(gen.next, "English")
    val swede = Nationality(gen.next, "Swede")
    val dane = Nationality(gen.next, "Dane")
    val norwegian = Nationality(gen.next, "Norwegian")
    val german = Nationality(gen.next, "German")

    val all = List(english, swede, dane, norwegian, german)
  }

  // Drinks

  case class Drink(index: Int, name: String) extends Value {
    override def symbol: Symbol = 'dri
  }

  object Drink {
    val gen = Stream.from(0).toIterator

    val tea = Drink(gen.next, "tea")
    val coffee = Drink(gen.next, "coffee")
    val milk = Drink(gen.next, "milk")
    val beer = Drink(gen.next, "beer")
    val water = Drink(gen.next, "water")

    val all = List(tea, coffee, milk, beer, water)
  }

  // Cigarettes

  case class Cigarette(index: Int, name: String) extends Value {
    override def symbol: Symbol = 'cig
  }

  object Cigarette {
    val gen = Stream.from(0).toIterator

    val pallMall = Cigarette(gen.next, "Pall Mall")
    val dunhills = Cigarette(gen.next, "Dunhills")
    val blend = Cigarette(gen.next, "Blend")
    val blueMasters = Cigarette(gen.next, "Blue Masters")
    val prince = Cigarette(gen.next, "Prince")

    val all = List(pallMall, dunhills, blend, blueMasters, prince)
  }

  // Pets

  case class Pet(index: Int, name: String) extends Value {
    override def symbol: Symbol = 'pet
  }

  object Pet {
    val gen = Stream.from(0).toIterator

    val dogs = Pet(gen.next, "dogs")
    val birds = Pet(gen.next, "birds")
    val cats = Pet(gen.next, "cats")
    val horses = Pet(gen.next, "horses")
    val fish = Pet(gen.next, "fish")

    val all = List(dogs, birds, cats, horses, fish)
  }

}