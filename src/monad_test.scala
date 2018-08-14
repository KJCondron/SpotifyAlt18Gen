
trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  def pure[A](x: A): F[A]
}

object Monad {

  implicit object ListMonad extends Monad[List] {
    def map[A,B](fa: List[A])(f: A => B) = fa map f
    def flatMap[A,B](fa: List[A])(f: A => List[B]) = fa flatMap f
    def pure[A](x: A) = x :: Nil
  }

  implicit object OptionMonad extends Monad[Option] {
    def map[A,B](fa: Option[A])(f: A => B) = fa map f
    def flatMap[A,B](fa: Option[A])(f: A => Option[B]) = fa flatMap f
    def pure[A](x: A) = Some(x)
  }

  def apply[F[_] : Monad]: Monad[F] = implicitly[Monad[F]]

}

final case class ListT[F[_] : Monad, A](fa: F[List[A]]) {
  def map[B](f: A => B) = ListT(Monad[F].map(fa)(_ map f))

  def flatMap[B](f: A => ListT[F, B]) = ListT(Monad[F].flatMap(fa) { _ match {
    case Nil => Monad[F].pure(List[B]())
    case list => list.map(f).reduce(_ ++ _).run
  }})

  def ++(that: ListT[F,A]) = ListT(Monad[F].flatMap(fa) { list1 =>
    Monad[F].map(that.run)(list1 ++ _)
  })

  def run = fa
}


object monad_test extends App {

  println("hello world")
  
  val xs = List(1,2,3)
  val ys = xs.map(_*2)
  
  println(xs)
  println(ys)
  
  val even = (x:Int) => x%2==0
  val dblifeven = (x:Int) => if(even(x)) List(x*2) else List()
  
  val zs = xs.map(dblifeven)
  val qs = xs.flatMap(dblifeven)
  
  println(zs)
  println(qs)
  
  val sxs : Option[List[Int]]= Some(xs)
  val lt1 = ListT(sxs)
  
  val ltlx : ListT[Option, Int]= ListT(Some(xs))
  
  val nil : Option[List[Int]] = None
  
  val lt2 = lt1.map(even) // the ListT[Option[Int]] is of type M[Int] so we can map Int->Bool over it 
  lt2.map(e=>print(e+",")) // to get M[Bool]
  println
  
  val x : ListT[Option, Int] = ListT(sxs)
  //val y : ListT[Option, Int] = ListT(Nothing)
  
  val bindfn1 = (x:Int) => if(even(x)) ListT(Option(List(x*2))) else ListT(Option(List(x*3)))
  val bindfn2 = (x:Int) => if(even(x)) ListT(Option(List(x*2))) else ListT(nil)
  
  val lt3 = lt1.flatMap(bindfn2)
  lt3.map(e=>print(e+","))
  
}