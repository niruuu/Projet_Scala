package MoSEF

class Tondeuse (val coordonnees: Coordonnees, var X: Int, var Y: Int, var O: String) {
  /* X, abscisse de la position initiale de la tondeuse. */
  /* Y, ordonnées de la position initiale de la tondeuse. Pour la première tondeuse par exemple comme
  précisé sur le fichier 1 2 N, donc X=1 Y=2. */
  def positionX(x: Int): Unit ={
    this.X = x
  }

  def positionY(y: Int): Unit ={
    this.Y = y
  }

  /* O, orientation initial de la tondeuse, soit 0 */
  def orientation(o: String, d: String): Unit ={
    this.O = o
    this.O = d
  }

  /* La consigne stipule que si la position après mouvement est en dehors de la pelouse, la tondeuse ne bouge pas, conserve son orientation
  * et traite la commande suivante.
  * C'est pourquoi nous avons définit bord un booleen   */
  def bord(): Boolean = {
    return (0 <= this.X && this.X <= this.coordonnees.crdX && 0 <= this.Y && this.Y <= this.coordonnees.crdY)
  }
  /*Pour contrôler la tondeuse, on lui envoie une séquence simple de lettres. Les lettres
  possibles sont « D », « G » et « A ». « A » signifie que l'on avance la tondeuse d'une case dans la direction à laquelle elle fait face, et sans modifier son orientation. */

  def orientation(deplacement: String): Unit = {
    deplacement match {
      /* */
      /* Dans le cas où la tondeuse doit Avancer son déplacement dépend de son orientation ce dont nous tenons compte dans les
      * cas ci-dessous :*/
      case "A" => this.O match {
        /* La tondeuse est orientée vers le Nord lorsqu'elle avance son ordonnée Y augmente d'une unité.
        * Inversement, si elle est orientée vers le Sud son ordonnée Y diminue d'une unité on a donc :  */
        case "N" => this.positionY(this.Y + 1)
        case "S" => this.positionY(this.Y - 1)

        /* De même si elle est orientée vers l'Est et avance c'est son abscisse X augmente d'une unité
        * Inversement, si elle est orientée vers l'Ouest (W) son abscisse diminue d'une unité d'où : */
        case "E" => this.positionX(this.X + 1)
        case "W" => this.positionX(this.X - 1)
        case _ => ()
      }

      /* « D » et « G » font pivoter la tondeuse de 90° à droite ou à gauche respectivement, sans la déplacer.
      * Du coup si la tondeuse est au nord alors si on on lui envoie G alors la tondeuse sera à l'ouest et est pour D  */
      case ("G" | "D" )=> this.O match {
        case "N" => this.orientation("W", "E")
        case "S" => this.orientation("E", "W")
        case "E" => this.orientation("N", "S")
        case "W" => this.orientation("S", "N")
        case _ => ()
      }
      case _ => ()
    }
  }

  def parcours(deplacement: String): Boolean = {
    assert(List("A", "G", "D") contains (deplacement))
    var tondeuse_final = new Tondeuse(this.coordonnees, this.X, this.Y, this.O)
    tondeuse_final.orientation(deplacement)
    return tondeuse_final.bord()
  }

  def motion(orientation: String): Unit = {
    for (deplacement <- orientation){
      if (this.parcours(deplacement.toString)){
        this.orientation(deplacement.toString)
      }
    }
  }
}
