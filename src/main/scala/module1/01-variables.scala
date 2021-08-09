package module1

object variables {


  /**
   * Переменные
   */

   //1. Объявите две константы, одна будет слово "Month", а друга "Year"

   val Month = "Month"
   val Year = "Year"




   //2. Объявите переменную счетчик, которую затем можно будет инкрементить

   var count = 0


  /**
   * Block expression {}
   * С помощью фигурных скобок, мы можем выделять блоки кода и присваивать их в переменные.
   * При этом тип переменной, будет равен типу последнего выражения в блоке кода, а ее значение - его значению.
   */

    //3. Объявите блок кода, который первым действием сложит числа 3 и 2 и присвоит результат в переменную x,
    // а вторым действием умножит x на 2

    lazy val block: Int = {
      val x = 3 + 2
      x * 2
    }



  /**
   * Управляющие конструкции
   *   if / else
   *   while / do while
   *   for
   */



  /**
   *  Конструкция if / else имеет туже семантику, что и в других ЯП. В зависимости от условия, выполняется либо одна либо
   *  другая ветка.
   *  При этом тип и значение if / else выражения определяется также, как и для блока кода.
   *  Т.е. последним выражением в исполняемой ветке.
   *
   *  ! Если ветки выражения имеют разный тип, то будет взят ближайший общий предок для выражений
    */

  lazy val cond: Boolean = ???


  //4. Напишите выражение, которое в зависимости от значения выражения cond будет возвращать "yes" или "no",
  // присвойте его в переменную х1

  val x1 =
    if(cond) {
      "yes"
    } else
      "no"



  //5. Напишите выражение, но которое в зависимости от значения выражения cond будет печатать "yes" или "no" в консоль,
  // присвойте его в переменную х2

  val x2 =
    if(cond) {
      println("yes")
    } else
      "no"



  //6. Напишите выражение, которое если значение переменной cond будет true напечатает в консоль "yes", а если
  // false то вернет строку "no",
  // присвойте его в переменную х3


  /**
   * циклы while / do while
   * Повторяют выполнение своего тела, пока условие истинно. Подразумевают наличие side effect.
   * Отличаются моментом, когда происходит проверка условия ДО или ПОСЛЕ выполнения тела цикла
   */

   import scala.util.control.Breaks._
   var i = 0
   var cond2 = true

   def breakExample =  breakable{

     do {
       i += 1
     }
     while(cond2)
  }

  /**
   * цикл for позволяет итерироваться по коллекциям, имеет своеобразный синтаксис с обратной стрелочкой
   */

  lazy val arr: Array[Int] = ???


  for(i <- arr.indices){
    println(i)
  }


}