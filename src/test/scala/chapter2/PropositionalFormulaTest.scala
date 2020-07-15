package chapter2

import org.scalatest.funsuite.AnyFunSuite

class PropositionalFormulaTest extends AnyFunSuite {
  test("someImportantTautologies") {
    val p = Atom("p")
    val q = Atom("q")
    val r = Atom("r")
    assert(Iff(Not(True), False).isTautology)
    assert(Iff(Not(False), True).isTautology)
    assert(Iff(Not(Not(p)), p).isTautology)
    assert(Iff(And(p, False), False).isTautology)
    assert(Iff(And(p, True), p).isTautology)
    assert(Iff(And(p, p), p).isTautology)
    assert(Iff(And(p, Not(p)), False).isTautology)
    assert(Iff(And(p, q), And(q, p)).isTautology)
    assert(Iff(And(And(p, q), r), And(p, And(q, r))).isTautology)
    assert(Iff(Or(p, False), p).isTautology)
    assert(Iff(Or(p, True), True).isTautology)
    assert(Iff(Or(p, p), p).isTautology)
    assert(Iff(Or(p, Not(p)), True).isTautology)
    assert(Iff(Or(p, q), Or(q, p)).isTautology)
    assert(Iff(Or(Or(p, q), r), Or(p, Or(q, r))).isTautology)
    assert(Iff(And(p, Or(q, r)), Or(And(p, q), And(p, r))).isTautology)
    assert(Iff(Or(p, And(q, r)), And(Or(p, q), Or(p, r))).isTautology)
    assert(Iff(Imp(False, p), True).isTautology)
    assert(Iff(Imp(p, True), True).isTautology)
    assert(Iff(Imp(p, False), Not(p)).isTautology)
    assert(Iff(Imp(p, p), True).isTautology)
    assert(Iff(Imp(p, q), Imp(Not(q), Not(p))).isTautology)
    assert(Iff(Imp(p, q), Iff(p, And(p, q))).isTautology)
    assert(Iff(Imp(p, q), Iff(q, Or(q, p))).isTautology)
    assert(Iff(Iff(p, q), Iff(q, p)).isTautology)
    assert(Iff(Iff(Iff(p, q), r), Iff(p, Iff(q, r))).isTautology)
  }
}
