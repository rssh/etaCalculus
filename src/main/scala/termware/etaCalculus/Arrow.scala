package termware.etaCalculus



trait TCArrow[T,X,Y]
{
  val tcLeft: TCTerm[X]

  val tcRight: TCTerm[Y]

  def left(t:T):X

  def right(t:T):Y
}

trait IArrow
{
  type Carrier

  type Left

  type Right

  def tcArrow: TCArrow[Carrier,Left,Right]

  def carrier: Carrier

}

case class Arrow(left:ITerm, right:ITerm) extends IArrow {

  thisArrow =>

  override type Carrier = this.type

  override type Left = left.Carrier

  override type Right = right.Carrier

  override def carrier: Carrier = this

  val tcArrow = new TCArrow[Carrier,Left,Right] {

    override val tcLeft: TCTerm[Left] = thisArrow.left.tcTerm

    override val tcRight: TCTerm[Right] = thisArrow.right.tcTerm

    override def left(t: Carrier): Left = t.left.carrier

    override def right(t: Carrier): Right = t.right.carrier
  }

}

