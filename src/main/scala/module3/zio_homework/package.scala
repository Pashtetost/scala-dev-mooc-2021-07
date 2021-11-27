package module3

import zio.{Has, IO, Task, ULayer, URIO, ZIO, ZLayer, clock}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */



  lazy val guessProgram = for {
    _ <- putStrLn("Try to guess a number from 1 to 3:")
    tryStr <- getStrLn
    num <- ZIO.effect(tryStr.toInt)
    ran <- nextIntBetween(1,3)
  } yield if(num == ran) putStrLn("Yes")
          else putStrLn("No")

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[A, E, R](cond: A => Boolean)(eff: ZIO[R,E,A]): ZIO[R, E, A] = {
    //eff.filterOrElse(cond)( _ => doWhile(cond)(eff))
    eff.flatMap {
      case value if cond(value) => eff
      case _ => doWhile(cond)(eff)
    }
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault = {
    import pureconfig.ConfigSource
    import config.AppConfig
    import pureconfig.generic.auto._

    for {
      config <- config.load.orElse(Task.effect(ConfigSource.default.load[AppConfig]))
      _ <- putStrLn(config.toString)
    } yield config
  }


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff = for{
    _ <- sleep(1 second)
    ran <- nextIntBetween(0,10)
  } yield ran

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: Seq[ZIO[Random with Clock, Nothing, Int]] = List.fill(10)(eff)

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = zioConcurrency.printEffectRunningTime(
    effects.reduceLeft((acc, elem) =>
    for{
      intf <- elem
      ints <- acc
  } yield intf + ints).flatMap(sum => putStrLn(s"$sum").map(_ => sum)))


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = zioConcurrency.printEffectRunningTime(
    for {
      sum <- ZIO.collectAllPar(effects).map(col => col.sum)
      _ <- putStrLn(s"$sum")
    } yield sum)


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  type EffectRunningTime = Has[EffectRunningTime.Service]

  @accessible 
  object EffectRunningTime {
    trait Service {
      def printEffectRunningTime[R, E, A](effect: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A]
    }

    object Service {
      val live: Service = new Service {
        override def printEffectRunningTime[R, E, A](effect: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A] =
          for{
            start <- clock.currentTime(TimeUnit.SECONDS)
            z <- effect
            end <- clock.currentTime(TimeUnit.SECONDS)
            _ <- putStrLn(s"Running time: ${end - start} seconds")
          } yield z
      }
    }
    val live: ULayer[Has[Service]] = ZLayer.succeed(Service.live)
  }

   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */
  lazy val appWithTimeLogg =  EffectRunningTime.printEffectRunningTime(app)

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = appWithTimeLogg
    .provideSomeLayer[zio.console.Console with zio.random.Random with zio.clock.Clock](EffectRunningTime.live)
  
}
