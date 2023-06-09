#' Lot. Voltera Model with hunding added
#'
#' function computes the rate of change of populations in a predator prey interaction
#' @param t  time (days)
#' @param pop datatype list initial conditions; list with two values prey=number of prey and pred=number of predator
#' @param pars datatype list  coefficient in Lotka-Voltera pars$rprey, pars$alpha, pars$eff, par$pmort
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{eff} is the rate of ingestion of prey by predators
#'  \emph{alpha} is a interaction coefficient (higher values greater interaction)
#'  \emph{pmort} mortality rate of predator population
#'  \emph{K} Carrying capacity of prey population
#'  \emph{h} hunting rate
#' @examples
#' lotvod(t=1, pop=list(1,2), pop=list(0.5,0.3,0.2,0.2))
#'
#' pars = c(rprey=0.5, alpha=0.01, eff=0.6, pmort=0.4, k=2000)
#' currpop  = c(prey = 1, pred=1)
#  days = seq(from=1,to=20)
#' res = ode(func=lotvmod, y=currpop, times=days, parms=pars)
#'
#' @return  lotvmod returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey populutation}
#' \item{dpred}{rate of change of preditor populutation}
#'}

lotvmod_hunt = function(t, pop, pars) {
with(as.list(c(pars,pop)), {
  if (prey > 100) {
  
dprey = rprey*(1-prey/K)*prey -  alpha*prey*pred - h*prey
dpred = eff*alpha*prey*pred - pmort*pred
  }
  else {
    dprey = rprey*(1-prey/K)*prey -  alpha*prey*pred
    dpred = eff*alpha*prey*pred - pmort*pred  
  }

return(list(c(dprey,dpred)))})
}




