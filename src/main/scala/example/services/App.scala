package example.services

import cats.effect.unsafe.implicits.global
import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s._
import fs2.Chunk
import fs2.io.net.{Datagram, Network}
import org.nustaq.serialization.FSTConfiguration

import scala.concurrent.duration.DurationInt

final case class Wahoo(a: String) extends Serializable

object App extends IOApp {

  val conf = FSTConfiguration.createDefaultConfiguration();

  val stuff = Wahoo("wahoooo")

  val barry = conf.asByteArray("hello");

  val reciever =
    Network[IO].openDatagramSocket(port = Some(port"8194")).use { socket =>
      pprint.log("Opened UDP")
      socket.reads
        .map { dg =>
          val parsed = conf.asObject(dg.bytes.toArray).asInstanceOf[String]
          pprint.log("Read!")
          pprint.log(parsed)
        }
        .compile
        .drain
    }

  val sender =
    Network[IO].openDatagramSocket().use { socket =>
      def doSingleWrite = {
        pprint.log("Write!")
        socket.write(
          Datagram(
            SocketAddress[IpAddress](ipv4"127.0.0.1", port"8194"),
            Chunk.array(barry)
          )
        )
      }
      for {
        _ <- doSingleWrite
        _ <- IO.sleep(1.second)
        _ <- doSingleWrite
        _ <- IO.sleep(1.second)
        _ <- doSingleWrite
        _ <- IO.sleep(1.second)
        _ <- doSingleWrite
        _ <- IO.sleep(1.second)
        _ <- doSingleWrite
      } yield ()

    }

  override def run(args: List[String]): IO[ExitCode] =

    for {
      _ <- IO(reciever.unsafeRunAndForget())
      _ <- sender
    } yield ExitCode.Success

}
