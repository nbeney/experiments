import collections.abc
import re
from collections import namedtuple
from operator import attrgetter

import z3
from z3 import *

# Useful non default operator definitions for z3 bools
BoolRef.__and__ = lambda self, other: And(self, other)
BoolRef.__or__ = lambda self, other: Or(self, other)
BoolRef.__xor__ = lambda self, other: Xor(self, other)
BoolRef.__invert__ = lambda self: Not(self)
BoolRef.__rshift__ = lambda self, other: Implies(self, other)


def generate_solutions(solver, max=None):
    """
    Utility function to generate all the possible solutions. By default Z3 returns only one solution but we circumvent
    this by adding constraints to exclude each new solution and by trying to solve again.
    """

    def same(decl):
        if decl.arity() > 0:
            raise Z3Exception("Uninterpreted functions are not supported")

        const = decl()
        if is_array(const) or const.sort().kind() == Z3_UNINTERPRETED_SORT:
            raise Z3Exception("Arrays and uninterpreted sorts are not supported")

        return const == m[decl]

    count = 0
    while solver.check() == sat:
        m = solver.model()
        yield m

        solver.push()
        exclude_c = Not(And([same(decl) for decl in m]))
        solver.add(exclude_c)

        count += 1
        if max is not None and count >= max:
            break


def flatten(l):
    """
    Utility function to recursively flatten the sub-lists.
    """
    for el in l:
        if isinstance(el, collections.abc.Iterable) and not isinstance(el, (str, bytes)):
            yield from flatten(el)
        else:
            yield el


def stamps1():
    """
    Given a supply of 5 and 7 cent stamps. What amounts cannot be produced as a non-negative combination of 5 and 7?
    """
    s = Solver()  # Quantified Linear Integer Arithmetic
    x, y, u = Ints('x y u')
    a = 5
    b = 7
    s.add(
        u > 0,
        u < 100,
        ~Exists(
            [x, y],
            (x >= 0) & (y >= 0) & (u == a * x + b * y)))

    if s.check() == sat:
        for m in generate_solutions(s, max=100):
            print(m)
        if s.check() == sat:
            print("...")
    else:
        print("No solution found!")


def stamps2():
    """
    Given a supply of 5 and 7 cent stamps. Is there a lower bound, after which all denominations of stamps can be
    produced? Thus, find v, such that every u larger or equal to v can be written as a non-negative combination of 5
    and 7.
    """
    s = Solver()  # Quantified Linear Integer Arithmetic
    s.reset()
    x, y, u, v = Ints('x y u v')
    a = 5
    b = 7
    s.add(
        ForAll(
            u,
            (u >= v) >> Exists(
                [x, y],
                (x >= 0) & (y >= 0) & (u == a * x + b * y))))
    print(s.check())
    print(s.model())


def basic1():
    x, y = Ints('x y')
    solve(0 < x, x < 2, y < 10, x + 2 * y == 7)

    x, y = Reals('x y')
    solve(0 < x, x < 2, y < 10, x + 2 * y == 7)

    print(simplify(x + y + 2 * x + 3))
    print(simplify(x < y + x + 2))
    print(simplify(And(x + 1 >= 3, x ** 2 + x ** 2 + y ** 2 + 2 >= 5)))
    print(simplify(2 * (x + y) - x))

    x, y = Bools('x y')
    solve(Implies(x, y), Or(x, y))


def basic2():
    x, y = Ints('x y')

    s = Solver()

    s.add(
        0 < x,
        x < 10,
        y > 0,
        y < 10,
        x + 2 * y == 7
    )

    if s.check() == sat:
        for m in generate_solutions(s, max=10):
            print(m)
        if s.check() == sat:
            print("...")
    else:
        print("No solution found!")


def basic3():
    x, y, z = Ints('x y z')

    s = Solver()

    s.add(
        Sum(x, y, z) == 7,
        x > 0,
        y > 0,
        z > 0,
    )

    if s.check() == sat:
        for m in generate_solutions(s, max=25):
            print(m)
        if s.check() == sat:
            print("...")
    else:
        print("No solution found!")


def dog_cat_mouse():
    """
    Consider the following puzzle. Spend exactly 100 dollars and buy exactly 100 animals. Dogs cost 15 dollars, cats cost
    1 dollar, and mice cost 25 cents each. You have to buy at least one of each. How many of each should you buy?
    """

    dog, cat, mouse = Ints('dog cat mouse')

    solve(
        dog >= 1,  # at least one dog
        cat >= 1,  # at least one cat
        mouse >= 1,  # at least one mouse
        dog + cat + mouse == 100,  # we want to buy 100 animals
        1_500 * dog + 100 * cat + 25 * mouse == 10_000  # we want to spend $100 (= 10000 cents)
    )


def sudoku():
    # 9x9 matrix of integer variables
    X = [[Int("x_%s_%s" % (i + 1, j + 1)) for j in range(9)] for i in range(9)]

    # each cell contains a value in {1, ..., 9}
    cells_c = [And(1 <= X[i][j], X[i][j] <= 9) for i in range(9) for j in range(9)]

    # each row contains a digit at most once
    rows_c = [Distinct(X[i]) for i in range(9)]

    # each column contains a digit at most once
    cols_c = [Distinct([X[i][j] for i in range(9)]) for j in range(9)]

    # each 3x3 square contains a digit at most once
    sq_c = [Distinct([X[3 * i0 + i][3 * j0 + j]
                      for i in range(3) for j in range(3)])
            for i0 in range(3) for j0 in range(3)]

    sudoku_c = cells_c + rows_c + cols_c + sq_c

    # sudoku instance, we use '0' for empty cells
    instance = ((0, 0, 0, 0, 9, 4, 0, 3, 0),
                (0, 0, 0, 5, 1, 0, 0, 0, 7),
                (0, 8, 9, 0, 0, 0, 0, 4, 0),
                (0, 0, 0, 0, 0, 0, 2, 0, 8),
                (0, 6, 0, 2, 0, 1, 0, 5, 0),
                (1, 0, 2, 0, 0, 0, 0, 0, 0),
                (0, 7, 0, 0, 0, 0, 5, 2, 0),
                (9, 0, 0, 0, 6, 5, 0, 0, 0),
                (0, 4, 0, 9, 7, 0, 0, 0, 0))

    instance_c = [If(instance[i][j] == 0, True, X[i][j] == instance[i][j]) for i in range(9) for j in range(9)]

    s = Solver()
    s.add(sudoku_c + instance_c)
    if s.check() == sat:
        for m in generate_solutions(s, max=5):
            r = [[m.eval(X[i][j]) for j in range(9)] for i in range(9)]
            print_matrix(r)
            print()
        if s.check() == sat:
            print("...")
    else:
        print("No solution found!")


def queens():
    def print_board(r):
        for c in r:
            s = ["." for x in range(8)]
            s[c.as_long() - 1] = "X"
            print(' '.join(s))

    # We know each queen must be in a different row.
    # So, we represent each queen by a single integer: the column position
    Q = [Int('Q_%i' % (i + 1)) for i in range(8)]

    # Each queen is in a column {1, ... 8 }
    val_c = [And(1 <= Q[i], Q[i] <= 8) for i in range(8)]

    # At most one queen per column
    col_c = [Distinct(Q)]

    # Diagonal constraint
    diag_c = [If(i == j, True, And(Q[i] - Q[j] != i - j, Q[i] - Q[j] != j - i)) for i in range(8) for j in range(i)]

    # solve(val_c + col_c + diag_c)

    s = Solver()
    s.add(val_c + col_c + diag_c)
    if s.check() == sat:
        for m in generate_solutions(s, max=5):
            r = [m.eval(Q[i]) for i in range(8)]
            # print_matrix(r)
            print_board(r)
            print()
        if s.check() == sat:
            print("...")
    else:
        print("No solution found!")


def strings():
    # Grab a solver
    s = Solver()

    # Create a symbolic string
    string = String('string')

    # len(string) = 8
    s.add(Length(string) == 8)

    # string[6] = 'a'
    s.add(SubSeq(string, 6, 1) == StringVal('a'))

    # string_possible_characters = '3456789a'
    chars = Union([Re(StringVal(c)) for c in '345789a'])  # Note I left 6 out on purpose!
    six = Re(StringVal('6'))

    # string.count("6") = 2
    # Create a regular expression that matches exactly two occurrences
    # of 6's and any other allowed character in other positions
    template = Concat(Star(chars), six, Star(chars), six, Star(chars))

    # Assert our string matches the template
    s.add(InRe(string, template))

    # Get a model
    res = s.check()
    if res == sat:
        print(s.model())
    else:
        print("Solver said:", res)


def riddle():
    """
    Let us assume that there are five houses of different colors next to each other on the same road. In each house lives
    a man of a different nationality. Every man has his favorite drink, his favorite brand of cigarettes, and keeps pets
    of a particular kind.

    The Englishman lives in the red house.
    The Swede keeps dogs.
    The Dane drinks tea.
    The green house is just to the left of the white one.
    The owner of the green house drinks coffee.
    The Pall Mall smoker keeps birds.
    The owner of the yellow house smokes Dunhills.
    The man in the center house drinks milk.
    The Norwegian lives in the first house.
    The Blend smoker has a neighbor who keeps cats.
    The man who smokes Blue Masters drinks bier.
    The man who keeps horses lives next to the Dunhill smoker.
    The German smokes Prince.
    The Norwegian lives next to the blue house.
    The Blend smoker has a neighbor who drinks water.

    The question to be answered is: Who keeps fish?
    """

    # Each house has variables for its index, color, nationality, drink, cigarette and pet.

    House = namedtuple("House", ["index", "color", "nationality", "drink", "cigarette", "pet"])

    houses = [House(
        index=Int(f"idx_{i}"),
        color=String(f"col_{i}"),
        nationality=String(f"nat_{i}"),
        drink=String(f"dri_{i}"),
        cigarette=String(f"cig_{i}"),
        pet=String(f"pet_{i}"),
    ) for i in range(5)]

    # Helper functions to make it easier to build constraints.

    index = attrgetter("index")
    color = attrgetter("color")
    nationality = attrgetter("nationality")
    drink = attrgetter("drink")
    cigarette = attrgetter("cigarette")
    pet = attrgetter("pet")

    def attr_constraints(getter, values):
        return [Or([getter(h) == c for c in values]) for h in houses] + [Distinct([getter(h) for h in houses])]

    def associate(getter1, value1, getter2, value2):
        return [(getter1(h) == value1) >> (getter2(h) == value2) for h in houses]

    def left_neighbor(getter1, value1, getter2, value2):
        return Or([(getter1(houses[i]) == value1) & (getter2(houses[i + 1]) == value2) for i in range(4)])

    def right_neighbor(getter1, value1, getter2, value2):
        return Or([(getter1(houses[i]) == value1) & (getter2(houses[i - 1]) == value2) for i in range(1, 5)])

    def neighbor(getter1, value1, getter2, value2):
        return left_neighbor(getter1, value1, getter2, value2) | right_neighbor(getter1, value1, getter2, value2)

    # Create the constraints for the indices.

    index_cs = [h.index == i for i, h in enumerate(houses)]

    # Create the constraints for the colors.

    col_red = StringVal("red")
    col_green = StringVal("green")
    col_white = StringVal("white")
    col_yellow = StringVal("yellow")
    col_blue = StringVal("blue")
    colors = [col_red, col_green, col_white, col_yellow, col_blue]
    color_cs = attr_constraints(color, colors)

    # Create the constraints for the nationalities.

    nat_english = StringVal("English")
    nat_swede = StringVal("Swede")
    nat_dane = StringVal("Dane")
    nat_norwegian = StringVal("Norwegian")
    nat_german = StringVal("German")
    nationalities = [nat_english, nat_swede, nat_dane, nat_norwegian, nat_german]
    nationality_cs = attr_constraints(nationality, nationalities)

    # Create the constraints for the drinks.

    dri_tea = StringVal("tea")
    dri_coffee = StringVal("coffee")
    dri_milk = StringVal("milk")
    dri_beer = StringVal("beer")
    dri_water = StringVal("water")
    drinks = [dri_tea, dri_coffee, dri_milk, dri_beer, dri_water]
    drink_cs = attr_constraints(drink, drinks)

    # Create the constraints for the cigarettes.

    cig_pallmall = StringVal("Pall-Mall")
    cig_dunhills = StringVal("Dunhills")
    cig_blend = StringVal("Blend")
    cig_bluemasters = StringVal("Blue-Masters")
    cig_prince = StringVal("Prince")
    cigarettes = [cig_pallmall, cig_dunhills, cig_blend, cig_bluemasters, cig_prince]
    cigarette_cs = attr_constraints(cigarette, cigarettes)

    # Create the constraints for the pets.

    pet_dogs = StringVal("dogs")
    pet_birds = StringVal("birds")
    pet_cats = StringVal("cats")
    pet_horses = StringVal("horses")
    pet_fish = StringVal("fish")
    pets = [pet_dogs, pet_birds, pet_cats, pet_horses, pet_fish]
    pet_cs = attr_constraints(pet, pets)

    # Create the other constraints.

    other_cs = flatten([
        # The Englishman lives in the red house.
        associate(nationality, nat_english, color, col_red),
        # The Swede keeps dogs.
        associate(nationality, nat_swede, pet, pet_dogs),
        # The Dane drinks tea.
        associate(nationality, nat_dane, drink, dri_tea),
        # # The green house is just to the left of the white one.
        left_neighbor(color, col_green, color, col_white),
        # The owner of the green house drinks coffee.
        associate(color, col_green, drink, dri_coffee),
        # The Pall Mall smoker keeps birds.
        associate(cigarette, cig_pallmall, pet, pet_birds),
        # The owner of the yellow house smokes Dunhills.
        associate(color, col_yellow, cigarette, cig_dunhills),
        # The man in the center house drinks milk.
        houses[2].drink == dri_milk,
        # The Norwegian lives in the first house.
        houses[0].nationality == nat_norwegian,
        # The Blend smoker has a neighbor who keeps cats.
        neighbor(cigarette, cig_blend, pet, pet_cats),
        # The man who smokes Blue Masters drinks beer.
        associate(cigarette, cig_bluemasters, drink, dri_beer),
        # The man who keeps horses lives next to the Dunhills smoker.
        neighbor(pet, pet_horses, cigarette, cig_dunhills),
        # The German smokes Prince.
        associate(nationality, nat_german, cigarette, cig_prince),
        # The Norwegian lives next to the blue house.
        neighbor(nationality, nat_norwegian, color, col_blue),
        # The Blend smoker has a neighbor who drinks water.
        neighbor(cigarette, cig_blend, drink, dri_water),
    ])

    # Create the solver and add all the constraints.

    s = Solver()

    s.add(*index_cs)
    s.add(*color_cs)
    s.add(*nationality_cs)
    s.add(*drink_cs)
    s.add(*cigarette_cs)
    s.add(*pet_cs)
    s.add(*other_cs)

    # Get the solution(s).

    if s.check() == sat:
        for m in generate_solutions(s, max=10):
            solution = [[m.eval(var).as_string() for var in h] for h in houses]
            for h in solution:
                print(*[_.ljust(15) for _ in h])
            print()
        if s.check() == sat:
            print("...")

        # # >>>>> START TESTING
        # actual = solution
        #
        # expected = """
        #     0   yellow          Norwegian       water           Dunhills        cats
        #     1   blue            Dane            tea             Blend           horses
        #     2   red             English         milk            Pall-Mall       birds
        #     3   green           German          coffee          Prince          fish
        #     4   white           Swede           beer            Blue-Masters    dogs
        # """
        #
        # expected = [re.compile(r"\s+").split(l.strip()) for l in expected.strip().split("\n")]
        #
        # assert actual == expected
        # # >>>>> STOP TESTING
    else:
        print("No solution found!")


def banner(f):
    print("+--------------------------------------------------------------------------------------------")
    print("| " + f.__name__)
    print("+--------------------------------------------------------------------------------------------")


if __name__ == '__main__':
    funcs = [
        stamps1,
        stamps2,
        basic1,
        basic2,
        basic3,
        dog_cat_mouse,
        sudoku,
        queens,
        strings,
        riddle,
    ]

    for f in funcs:
        banner(f)
        f()
        print()
