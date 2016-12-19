package chapter2

sealed trait PropositionalFormula[T]

case object False extends PropositionalFormula[Nothing]

case object True extends PropositionalFormula[Nothing]

case class Atom[T](something: T) extends PropositionalFormula[T]

case class Not[T](formula: PropositionalFormula[T]) extends PropositionalFormula[T]

case class And[T](left: PropositionalFormula[T], right: PropositionalFormula[T]) extends PropositionalFormula[T]

case class Or[T](left: PropositionalFormula[T], right: PropositionalFormula[T]) extends PropositionalFormula[T]

case class Imp[T](left: PropositionalFormula[T], right: PropositionalFormula[T]) extends PropositionalFormula[T]

case class Iff[T](left: PropositionalFormula[T], right: PropositionalFormula[T]) extends PropositionalFormula[T]

case class Forall[T](variable: String, formula: PropositionalFormula[T]) extends PropositionalFormula[T]

case class Exists[T](variable: String, formula: PropositionalFormula[T]) extends PropositionalFormula[T]
