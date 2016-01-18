package com.github.nrf110.io.transport

import java.net.InetSocketAddress

import akka.actor._
import akka.io.Tcp.Connect
import akka.io.{IO, Tcp}
import akka.util.ByteString

class ConnectionHandler(remoteAddress: InetSocketAddress, localAddress: Option[InetSocketAddress], listener: ActorRef)
  extends Actor with ActorLogging {

  IO(Tcp)(context.system) ! Connect(remoteAddress, localAddress)

  def initial: Receive = {
    case fail @ Tcp.CommandFailed(_: Tcp.Connect) =>
      log.warning("Failed to connect to {}", remoteAddress)
      listener ! ConnectionFailed
      die()
    case c @ Tcp.Connected(remote, local) =>
      listener ! c
      val connection = sender()
      connection ! Tcp.Register(self)
      context become connected(connection)
  }

  def connected(connection: ActorRef): Receive = {
    case data: ByteString =>
      connection ! Tcp.Write(data)

    case Tcp.CommandFailed(w: Tcp.Write) =>
      log.warning("Write failed to {}", remoteAddress)
      listener ! WriteFailed

    case Tcp.Received(data) =>
      listener ! data

    case Close =>
      connection ! Tcp.Close

    case _: Tcp.ConnectionClosed =>
      log.info("Closed connection to {}", remoteAddress)
      listener ! Closed
      die()
  }

  def die(): Unit = context stop self

  def receive: Receive = initial
}

object ConnectionHandler {
  trait Factory {
    def apply(remoteAddress: InetSocketAddress, localAddress: Option[InetSocketAddress], handler: ActorRef): ActorRef
  }

  def apply(remoteAddress: InetSocketAddress, localAddress: Option[InetSocketAddress], handler: ActorRef)(implicit factory: ActorRefFactory): ActorRef =
    factory.actorOf(Props(classOf[ConnectionHandler], remoteAddress, handler))
}
