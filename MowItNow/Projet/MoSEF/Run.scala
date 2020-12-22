import MoSEF.Coordonnees
import MoSEF.Tondeuse
import scala.io.Source

object Run extends App {
  val fichier = "Projet/MoSEF/Fichier_pilotage.txt"
  var input = {
    val src = Source.fromFile(fichier).getLines().toList
    src
  }

  /* on recupère depuis notre fichier les coordonnées séparées par des espaces */
  val crd = input(0).split(" ")

  /* la première ligne correspond aux coordonnées du coin supérieur droit de la pelouse */
  val gazon = new Coordonnees(crd(0).toInt, crd(1).toInt)
  input = input.patch(0, Nil, 1)
  var mower = 0

  /* Boucle while permet d'utiliser les séquences d'instruction envoyer aux tondeuses pour calculer
    * leur position finale à l'aide de la classe tondeuse_instance*/

  while (input.length > 1){
    mower += 1
    var location = input(0).toString.split(" ")
    var orientation = input(1).toString
    input = input.drop(2)
    var tondeuse_final = new Tondeuse(gazon, location(0).toInt, location(1).toInt, location(2))
    tondeuse_final.motion(orientation)
    println(s"Position finale de la tondeuse numéro $mower : ${tondeuse_final.X} ${tondeuse_final.Y} ${tondeuse_final.O}")

  }
}
