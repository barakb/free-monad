package typeclasses



/**
  * Created by Barak Bar Orion
  * on 4/12/17.
  */
object FreeMonadExample extends App {


  // val l = List(Ask("What is your name ?"), Tell(s"Hi $name"))

  trait Monad[M[_]]{
    def pure[A](a: A): M[A]
    def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]
  }

  object Monad{
     def apply[M[_]: Monad]: Monad[M] = implicitly[Monad[M]]
  }

  sealed trait Free[F[_], A]{
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
        // Return(a) flatMap f
      case Return(a) => f(a)
        // Bind(fg, g) flatMap f
      case Bind(fg, g) => Bind(fg, g andThen (a => a flatMap f))
    }
//    f : A => B  -> f: A => Free[F, B]
    def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))

    // evaluate the free monad i.e translate it to from F to G where G can convert to Monad.
    def interpret[G[_]: Monad](translate: (F ~> G)): G[A] = this match {
      case Return(a) => Monad[G].pure(a)
      case Bind(fa, g) => Monad[G].flatMap(translate(fa))(a => g(a).interpret(translate))
    }
  }

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Bind[F[_], A, B](fa: F[A], f: A => Free[F, B]) extends Free[F, B]

  implicit def lift[F[_], A](fa: F[A]): Free[F, A] = Bind(fa, (a: A) => Return(a))


  trait ~>[F[_], G[_]]{
    def apply[A](fa: F[A]): G[A]
  }
  

  type Id[A] = A

  implicit object IdentityMonad extends Monad[Id]{
    override def pure[A](a: A): Id[A] = a
    override def flatMap[A, B](m: Id[A])(f: (A) => Id[B]): Id[B] = f(m)
  }

  sealed trait Interact[A]

  case class Ask(prompt: String) extends Interact[String]

  case class Tell(msg: String) extends Interact[Unit]

  val prg: Free[Interact, Unit] = for {
    first <- Ask("What’s your first name?")
    last  <- Ask("What's your last name?")
    _     <- Tell(s"Hello, $first, $last!")
  } yield Unit

  object consoleInterpretation extends (Interact ~> Id){
    override def apply[A](fa: Interact[A]): Id[A] = fa match {
      case Ask(msg) =>
        println(msg)
        readLine()
      case Tell(msg) =>
        println(msg)
    }
  }

  def createInteraction(answers: Map[String, String]): (Interact ~> Id) = {
    new (Interact ~> Id){
      override def apply[A](fa: Interact[A]): Id[A] = fa match {
        case Ask(msg) =>
          println(msg)
          answers.getOrElse(msg, "<<Unknow>>")
        case Tell(msg) =>
          println(msg)
      }
    }
  }


//  Ask("What’s your first name?").flatMap(first => Ask("What's your last name?").flatMap(last => Tell(s"Hello, $first, $last!").map(_ => Unit)))

  println(prg)
  prg.interpret(createInteraction(Map("What’s your first name?" -> "Barak", "What's your last name?" -> "Bar Orion")))

}
