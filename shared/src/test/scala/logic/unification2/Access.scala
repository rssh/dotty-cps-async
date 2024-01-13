package logic.unification2

trait Access[-T] {

  def get[S<:T]: S
  
}
