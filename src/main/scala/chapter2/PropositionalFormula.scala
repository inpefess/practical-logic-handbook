package chapter2

sealed trait PropositionalFormula {
  type Valuation = Atom => Boolean
  lazy val atoms: Set[Atom] = {
    this match {
      case True => Set()
      case False => Set()
      case Atom(propositionName) => Set(Atom(propositionName))
      case Not(proposition) => proposition.atoms
      case And(left, right) => left.atoms.union(right.atoms)
      case Or(left, right) => left.atoms.union(right.atoms)
      case Imp(left, right) => left.atoms.union(right.atoms)
      case Iff(left, right) => left.atoms.union(right.atoms)
    }
  }
  lazy val isSatisfiable: Boolean = {
    onAllValuations.exists(truthValue => truthValue)
  }
  lazy val isTautology: Boolean = {
    !Not(this).isSatisfiable
  }
  private lazy val onAllValuations: Iterator[Boolean] = {
    atoms.subsets.map(atomsSubset => eval(atomsSubset.contains))
  }

  def eval(valuation: Valuation): Boolean = {
    this match {
      case False => false
      case True => true
      case Atom(proposition) => valuation(Atom(proposition))
      case Not(proposition) => !proposition.eval(valuation)
      case And(left, right) =>
        if (left.eval(valuation)) right.eval(valuation)
        else false
      case Or(left, right) =>
        if (left.eval(valuation)) true
        else right.eval(valuation)
      case Imp(left, right) =>
        if (left.eval(valuation)) right.eval(valuation)
        else true
      case Iff(left, right) => left.eval(valuation) == right.eval(valuation)
    }
  }
}

case object False extends PropositionalFormula

case object True extends PropositionalFormula

case class Atom(propositionName: String) extends PropositionalFormula

case class Not(formula: PropositionalFormula) extends PropositionalFormula

case class And(left: PropositionalFormula, right: PropositionalFormula) extends PropositionalFormula

case class Or(left: PropositionalFormula, right: PropositionalFormula) extends PropositionalFormula

case class Imp(left: PropositionalFormula, right: PropositionalFormula) extends PropositionalFormula

case class Iff(left: PropositionalFormula, right: PropositionalFormula) extends PropositionalFormula
