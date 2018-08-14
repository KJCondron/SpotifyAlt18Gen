
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
  
  //val ltlx : ListT[Option, Int]= ListT(Some(xs))
  
  val lt2 = lt1.map(even) // the ListT[Option[Int]] is of type M[Int] so we can map Int->Bool over it 
  lt2.map(e=>print(e+",")) // to get M[Bool]
  println
  
  val nil : Option[List[Int]] = None
  val bindfn1 = (x:Int) => if(even(x)) ListT(Option(List(x*2.2))) else ListT(Option(List(x*3.3)))
  
  val lt3 = lt1.flatMap(bindfn1) // as it is M[Int] we can bind an Int->ListT[Option,X]
  lt3.map(e=>print(e+",")) // to produce a new M[X], here X=Double
  println("Fn1 Done")
  
  // It is unlikely we have a function to ListT[M,X], more likely we have a List[X]
  // and a function X->M[Y]
  
  //val nbind1 = (x:Int) => if(even(x)) { val y : Option[Int] = Some(x); y } else { val z : Option[Int] = None; z}  
  val nbind2 : Int=>Option[Double] = (x:Int) => if(even(x)) Some(4.4*x) else Some(5.5*x)
  
  // so we can nearly bind this function, but it doesn't quite return the right type
  // but we can 'lift' the function to the higher monad, with this simple function
  val bindfn2 = (x:Int) => { 
    val y = nbind2(x).map( e => List(e) )
    ListT(y)
  }
  // note that the only thing we need from the function is that we can call map on its result
  // ie that the function returns a Monad (or maybe even just a functor!?)
  
  // here is the generic version
  def liftToListT[M[_] : Monad, X, Y]( fn:X=>M[Y] ) =
  {
    val newFn = (x:X) => { 
       val y = Monad[M].map(fn(x))( e => List(e) )
       ListT(y)
    }
    newFn
  }
  
  val bindfn2a = liftToListT(nbind2)
  
  lt1.flatMap(bindfn2).map(e=>print(e+",")) 
  println("Fn2 Done")
  
  lt1.flatMap(bindfn2a).map(e=>print(e+",")) 
  println("Fn2a Done")
  
  // Obviously we are using Option because one of them may be None
  // it looks even better now though
  val nbind3 = (x:Int) => if(x<30) Some(x) else None
  val bindfn3 = liftToListT(nbind3)
  lt1.flatMap(bindfn3).map(e=>print(e+",")) 
  println("Fn3 Done")
  
  // here none failed, but if just 1 fails
  val xxs = List(1,2,50)
  val ltx = ListT(Option(xxs))
  ltx.flatMap(bindfn3).map(e=>print(e+",")) 
  println("Fn3x Done")
 
  def prints(x:Any) { print(x+",") }
  ListT(Option(List(1,2,50))).flatMap(bindfn3).map(prints);println("Beginning")
  ListT(Option(List(1,50,2))).flatMap(bindfn3).map(prints);println("Middle")
  ListT(Option(List(50,1,2))).flatMap(bindfn3).map(prints);println("End")
 
  val bindfn4 = (x:Int) => if(even(x)) ListT(Option(List(x*2))) else ListT(nil)
  // Note presence of just one None makes whole listT None!
  // this is a bit different from what we hoped
  lt1.flatMap(bindfn4).map(e=>print(e+",")) 
  println("Fn4 Done")
  
  // We noticed that List(1,2,3).flatMap( x=> if(even) List(x) else List() )
  // returned List(2) as both List and Option are Monads there isn't much
  // difference between first function and List(1,2,3).flatMap( x=>if(even) Some(x) else None )
  // but M1[X].flatMap( X->M2[x] ) is obviously not the correct signature
  // we are trying to use Monad Transformers to 'fix' it
   
}


// List is also a monad so we can replicate ListT[Option, Int] above
// with ListT[List, Int]. Removing some of the unnecessary distractions
object monad_test2 extends App {
  
  val xs = List(1,2,3)
  
  val even = (x:Int) => x%2==0
  
  val sxs = List(xs)
  val lt1 = ListT(sxs)
  
  val bindfn1 = (x:Int) => if(even(x)) ListT(List(List(x*2.2))) else ListT(List(List(x*3.3)))
  
  val lt3 = lt1.flatMap(bindfn1) // as it is M[Int] we can bind an Int->ListT[Option,X]
  lt3.map(e=>print(e+",")) // to produce a new M[X], here X=Double
  println("Fn1 Done")
  
  // It is unlikely we have a function to ListT[M,X], more likely we have a List[X]
  // and a function X->M[Y]
  
  val nbind2 = (x:Int) => if(even(x)) List(4.4*x) else List(5.5*x)
  
  // here is the generic version
  def liftToListT[M[_] : Monad, X, Y]( fn:X=>M[Y] ) =
  {
    val newFn = (x:X) => { 
       val y = Monad[M].map(fn(x))( e => List(e) )
       ListT(y)
    }
    newFn
  }
  
  val bindfn2 = liftToListT(nbind2)
  
  lt1.flatMap(bindfn2).map(e=>print(e+",")) 
  println("Fn2 Done")
  
  // Obviously we are using Option because one of them may be None
  // it looks even better now though
  val nbind3 = (x:Int) => if(x<30) List(x) else List()
  val bindfn3 = liftToListT(nbind3)
  lt1.flatMap(bindfn3).map(e=>print(e+",")) 
  println("Fn3 Done")
  
  // here none failed, but if just 1 fails
  val xxs = List(1,2,50)
  val ltx = ListT(List(xxs))
  ltx.flatMap(bindfn3).map(e=>print(e+",")) 
  println("Fn3x Done")
 
  def prints(x:Any) { print(x+",") }
  ListT(List(List(1,2,50))).flatMap(bindfn3).map(prints);println("Beginning")
  ListT(List(List(1,50,2))).flatMap(bindfn3).map(prints);println("Middle")
  ListT(List(List(50,1,2))).flatMap(bindfn3).map(prints);println("End")
  
  // So we were trying to replicate List(1,2,3).bind( x=>if(even(x)) List(x) else List() )
  // with List(1,2,3).bind( x=>if(even(x)) Some(x) else None )
  // but that didn't work with a ListT
  // but even if we replace Option in ListT[Option, Int] with
  // ListT[List, Int] then 'binding' x=>if(even(x)) List(x) else List()
  // doesn't get us back to original situation
  // so it is obviosuly something in ListT rather than the specifics of
  // Option vs List Monads (which is good because ListT is meant to be generic
  // regardless of the inner monad type
  
  // So here is why
  
  val x1 = ListT(List(List(1,2,50))).flatMap(bindfn3)
  // =>
  val fa = List(List(1,2,50))
  val res = fa.flatMap( innerList => innerList match {
    case Nil => List(List()) // if the list is empty, you just get our inner 'monad' wrapping an empty list 
    case inner => inner.map(bindfn3).reduce(_ ++ _).run // so this is the line that really happens
  } )
 
  val inner = List(1,2,50)
  val mapped = inner.map(bindfn3) // == List(ListT(List(List(1))),ListT(List(List(2))),ListT(List(List())))
  // or if we were using Option List(ListT(Some(List(1))),ListT(Some(List(2))),ListT(None))
  // now reduce
  val r1 = ListT(Option(List(1))) ++ ListT(Option(List(2)))
  val r1a = ListT(Option(List(1)).flatMap( list1 => Option(List(2)).map( list2 => list1 ++ list2 ) ))
  val r1b = ListT(Option(List(1,2)))
  // r1==r1a==r1b
  
  val n : Option[List[Int]] = None
  val nn : ListT[Option, Int] = ListT(n)
  val r2 = r1b ++ nn
  val r2inner = Option(List(1,2)).flatMap( list1 => None.map( list2 => list1 ++ list2 ) )
  val r2innera = Option(List(1,2)).flatMap( list1 => None ) // as None.map(f) == None
  val r2innerb = None // as Option(x).flatMap(_=>None) == None
  
  // doesn't matter where the None comes because ++ uses flatMap and Map any None in the chain
  // causes the whole chain to be None
  
  // with List as the inner monad
  val nx : List[List[Int]] = List(List())
  val nnx : ListT[List, Int] = ListT(nx)
  val r2x = ListT(List(List(1,2))) ++ nnx
  val r2innerx = List(List(1,2)).flatMap( list1 => List().map( list2 => list1 ++ list2 ) )
  val r2innerax = List(List(1,2)).flatMap( list1 => List() ) // as List().map(f) == None
  val r2innerbx = None // as Option(x).flatMap(_=>None) == None
  
  
  
  
}