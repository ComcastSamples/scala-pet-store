package io.github.pauljamescleary.petstore.service

import cats._
import cats.data._
import io.github.pauljamescleary.petstore.model.{Order, OrderStatus}
import io.github.pauljamescleary.petstore.repository.OrderRepositoryAlgebra

import scala.language.higherKinds

sealed trait OrderError
case class OrderNotFound(orderId: Long) extends OrderError

class OrderService[F[_]](orderRepo: OrderRepositoryAlgebra[F]) {

  def placeOrder(order: Order): F[Order] = orderRepo.put(order)

  def updateStatus(orderId: Long, status: OrderStatus)(implicit M: Monad[F]): EitherT[F, OrderError, Order] =
    for {
      order <- EitherT.fromOptionF(orderRepo.get(orderId), OrderNotFound(orderId))
      updated = order.copy(status = status)
      _ <- EitherT.right[OrderError](orderRepo.put(updated))
    } yield updated
}

object OrderService {
  def apply[F[_]](orderRepo: OrderRepositoryAlgebra[F]): OrderService[F] =
    new OrderService(orderRepo)
}
