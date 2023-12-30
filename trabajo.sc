case class tweet(id:String, user_followers_count:Int, user_friends_count:Int)

val informacion:List[tweet] = List(
  tweet("1108734367684276231",88,125),
  tweet("1108734260763074561",309,656),
  tweet("1108734212473917442",516,479),
  tweet("1108733855006109696",0,5),
  tweet("1108733839755501569",23571,23791),
  tweet("1108733825100795904",4477,3717),
  tweet("1108733819484540929",1848,3018),
  tweet("1108733813616594945",41783,39449),
  tweet("1108733761003405314",3972,4172),
  tweet("1108733631420235776",30,15),
  tweet("1108733579595579392",349,115),
  tweet("1108733370954076164",2842,4869),
  tweet("1108733315824025600",188,131),
  tweet("1108733071971545088",52374,30928),
  tweet("1108732722657124352",4,26),
  tweet("1108732635713404928",87,123),
  tweet("1108732603719450624",3,8),
  tweet("1108732533016023041",786,3143),
  tweet("1108732452120510464",282,994),
  tweet("1108732302929133568",6690,4967),
  tweet("1108732298436972544",7011,5103),
  tweet("1108732284658745344",4733,4712),
  tweet("1108732239544762369",20846,13664),
  tweet("1108732212537634816",615,562),
  tweet("1108732007947878400",796,71),
  tweet("1108731878339534849",2795,2781),
  tweet("1108731872819990529",15852,8429),
  tweet("1108731846542716928",3172,2481),
  tweet("1108731769011032064",162,84),
  tweet("1108731737452904455",222,231),
)


def calcularMediaX(x:List[tweet]): Double = {
x.map(_.user_followers_count).sum/x.length.toDouble
}
calcularMediaX(informacion)

def calcularMediaY(y: List[tweet]): Double ={
  y.map(_.user_friends_count).sum/y.length.toDouble
}
calcularMediaY(informacion)


def pearson(x: List[tweet], y: List[tweet]): Double = {

  val n = x.length
  val follower: List[Int] = x.map(_.user_followers_count)
  val friends: List[Int] = y.map(_.user_friends_count)

  val promx: Double = follower.sum.toDouble / n
  val promy: Double = friends.sum.toDouble / n

  val x1 = follower.map(x => math.pow(x - promx,2))
  val y1 = friends.map(y => math.pow(y - promy,2))

  val numerador = (0 until informacion.length).map(i => (follower.sum - promx) * (friends.sum - promy)).sum.toDouble
  val denominador = math.sqrt(x1.sum * y1.sum)

  val funcion = numerador / denominador
  pearson(informacion)
}
/*def pearson(listaX: List[tuits], listaY: List[tuits]): Double = {
val numerador = (0 until lista.length).map(_ => (i => (f - promedioX) * (Yi.sum - promedioY)).sum.toDouble


  val n = listaX.length

  val Xi = listaX.map(_.user_followers_count)
  val Yi = listaY.map(_.user_friends_count)

  val MediaX = Xi.sum / n
  val MediaY = Yi.sum / n

  val xi2 = Xi.map(x => math.pow(x - MediaX, 2))
  val yi2 = Yi.map(y => math.pow(y - MediaY, 2))

  val numerador = (0 until n).map(i => (Xi(i) - MediaX) * (Yi(i) - MediaY)).sum

  val denominador = math.sqrt(xi2.sum * yi2.sum)

  val funcionpearson= numerador/denominador
    funcionpearson
}*/
/*
def pearson(list: List[tweets]): Double = {
  val list_follower: List[Int] = list.map(_.follower)
  val list_friends: List[Int] = list.map(_.friends)

  val promediox: Double = list_follower.sum.toDouble / list_follower.size
  val promedioy: Double = list_friends.sum.toDouble / list_friends.size

  def media_y_multiplicacion(): Double = {
    val productos = (0 until list.length)
      .map(i => (list_follower(i) - promediox) * (list_friends(i) - promedioy))
    productos.sum
  }

  def desviacion(lista2: List[Int], promedio: Double): Double = {
    math.sqrt(lista2.map(k => math.pow(k - promedio, 2)).sum)
  }

  val numerador = media_y_multiplicacion
  val denominador = desviacion(list_follower, promediox) * desviacion(list_friends, promedioy)

  numerador / denominador
}

def spearman(list: List[tweets]): Double = {
  val list_follower: List[Int] = list.map(_.follower)
  val list_friends: List[Int] = list.map(_.friends)

  def lista_valor(lista: List[Int]): List[Int] = {
    lista.map(k => lista.count(_ < k) + 1)
  }

  val list_follower_2: List[Int] = lista_valor(list_follower)
  val list_friends_2: List[Int] = lista_valor(list_friends)

  def suma(): Double = {
    val productos = (0 until list.length)
      .map(i => math.pow(list_follower_2(i) - list_friends_2(i), 2))
    productos.sum
  }

  1 - ((6 * suma) / (list.length * (math.pow(list.length,2) - 1)))
}

val pearson1 = pearson(lista)
val spearman2 = spearman(lista)
*/
