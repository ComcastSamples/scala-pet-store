package io.github.pauljamescleary.petstore
package service

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalacheck._
import cats.{Monad, InjectK, ~>}
import cats.data._
import cats.free._
import cats.instances.list._
import org.scalacheck.support.cats._

import model._
import repository._

class OrderServiceSpec
    extends FunSuite
    with Matchers
    with PropertyChecks
    with PetStoreArbitraries {

  class OrderRepositoryGeneratingInterpreter(response: Gen[Option[Order]], genId: Gen[Long])(implicit longInput: Cogen[Long])
      extends OrderRepositoryAlgebra[Gen] {

    def put(order: Order): Gen[Order] =
      for {
        id <- order.id.map(Gen.const(_)).getOrElse(genId).map(Some(_))
        order <- Gen.const(order.copy(id = id))
      } yield order

    def get(orderId: Long): Gen[Option[Order]] =
      longInput.cogen(orderId, response)

    def delete(orderId: Long): Gen[Option[Order]] =
      longInput.cogen(orderId, response)

  }


  sealed trait OrderRepositoryTrace

  case class TraceGet(orderId: Long) extends OrderRepositoryTrace
  case class TracePut(order: Order) extends OrderRepositoryTrace
  case class TraceDelete(orderId: Long) extends OrderRepositoryTrace

  class OrderRepositoryTraceInterpreter[F[_]](wrapped: OrderRepositoryAlgebra[F])(implicit M: Monad[F]) extends OrderRepositoryAlgebra[WriterT[F, List[OrderRepositoryTrace], ?]] {


    def log[A](message: OrderRepositoryTrace)(g: F[A]): WriterT[F, List[OrderRepositoryTrace], A] =
      for {
        _ <- WriterT.tell[F, List[OrderRepositoryTrace]](List(message))
        result <- WriterT.lift[F, List[OrderRepositoryTrace], A](g)
      } yield result

    def put(order: Order): WriterT[F, List[OrderRepositoryTrace], Order] =
      log(TracePut(order)) {
        wrapped.put(order)
      }

    def get(orderId: Long): WriterT[F, List[OrderRepositoryTrace], Option[Order]] =
      log(TraceGet(orderId)) {
        wrapped.get(orderId)
      }

    def delete(orderId: Long): WriterT[F, List[OrderRepositoryTrace], Option[Order]] =
      log(TraceDelete(orderId)) {
        wrapped.delete(orderId)
      }

  }

  test("never delete when updating status") {
    val orderGenInterpreter = new OrderRepositoryGeneratingInterpreter(Gen.option(order.arbitrary), Gen.posNum[Long])
    val orderRepo = new OrderRepositoryTraceInterpreter(orderGenInterpreter)
    val orderService = OrderService(orderRepo)

    implicit val arbitraryWalk: Arbitrary[List[OrderRepositoryTrace]] =
      Arbitrary(orderService.updateStatus(5, Delivered).value.written)

    forAll { (walk: List[OrderRepositoryTrace]) =>
      println(walk)
      assert(!walk.exists {
        case TraceDelete(_) => true
        case _ => false
      })
    }
  }

  sealed trait OrderRepositoryOp[A]

  case class PutOp(order: Order) extends OrderRepositoryOp[Order]
  case class GetOp(orderId: Long) extends OrderRepositoryOp[Option[Order]]
  case class DeleteOp(orderId: Long) extends OrderRepositoryOp[Option[Order]]

  class OrderRepositoryFree[F[_]](implicit I: InjectK[OrderRepositoryOp, F]) extends OrderRepositoryAlgebra[Free[F, ?]] {

    def put(order: Order): Free[F, Order] =
        Free.inject[OrderRepositoryOp, F](PutOp(order))

    def get(orderId: Long): Free[F, Option[Order]] =
      Free.inject[OrderRepositoryOp, F](GetOp(orderId))

    def delete(orderId: Long): Free[F, Option[Order]] =
      Free.inject[OrderRepositoryOp, F](DeleteOp(orderId))

  }

  class OrderRepositoryOpAsGen(genInterpreter: OrderRepositoryGeneratingInterpreter)
      extends (OrderRepositoryOp ~> Gen) {

    def apply[A](in: OrderRepositoryOp[A]): Gen[A] = in match {
      case PutOp(order) => genInterpreter.put(order)
      case GetOp(orderId) => genInterpreter.get(orderId)
      case DeleteOp(orderId) => genInterpreter.delete(orderId)
    }
  }

  class Trace[F[_], G[_]: Monad](nt: F ~> G) extends (F ~> WriterT[G, List[F[_]], ?]) {

    def apply[A](f: F[A]): WriterT[G, List[F[_]], A] =
      for {
        _ <- WriterT.tell[G, List[F[_]]](List(f))
        a <- WriterT.lift[G, List[F[_]], A](nt(f))
      } yield a

  }

  test("never delete in update take 2") {
    val orderGenInterpreter = new OrderRepositoryGeneratingInterpreter(Gen.option(order.arbitrary), Gen.posNum[Long])
    val interpreter = new Trace(new OrderRepositoryOpAsGen(orderGenInterpreter))
    val orderRepo = new OrderRepositoryFree[OrderRepositoryOp]
    val orderService = OrderService(orderRepo)

    implicit val arbitraryWalk: Arbitrary[List[OrderRepositoryOp[_]]] =
      Arbitrary(orderService.updateStatus(5, Delivered).value.foldMap(interpreter).written)

    forAll { (walk: List[OrderRepositoryOp[_]]) =>
      assert(!walk.exists {
        case DeleteOp(_) => true
        case _ => false
      })
    }
  }
}
