#  File src/library/stats/R/constrOptim.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


constrOptim<-function(theta,f,grad,ui,ci,mu=0.0001,control=list(),
                  method=if(is.null(grad)) "Nelder-Mead" else "BFGS",
                  outer.iterations=100,outer.eps=0.00001,...){

    if (!is.null(control$fnscale) && control$fnscale<0)
      mu<- -mu ##maximizing
  
    R<-function(theta,theta.old,...){
        ui.theta<-ui%*%theta
        gi<- ui.theta-ci
        if (any(gi<0)) return(NaN)
        gi.old<-ui%*%theta.old-ci
        bar<-sum( gi.old*log(gi)-ui.theta)
        if (!is.finite(bar)) bar<- -Inf
        f(theta,...)-mu*bar
    }
 
    dR<-function(theta,theta.old,...){
        ui.theta<-ui%*%theta
        gi<-drop(ui.theta-ci)
        gi.old<-drop(ui%*%theta.old-ci)
        dbar<-colSums( ui*gi.old/gi-ui)
        grad(theta,...)-mu*dbar
    }

    if (any(ui%*%theta-ci<=0))
        stop("initial value not feasible")
    obj<-f(theta,...)
    r<-R(theta,theta,...)
    for(i in 1L:outer.iterations){
        obj.old<-obj
        r.old<-r
        theta.old<-theta
        fun<-function(theta,...){ R(theta,theta.old,...)}
        
        gradient<-function(theta,...) { dR(theta,theta.old,...)}
        a<-optim(theta.old, fun, gradient, control=control,method=method,...)
        r<-a$value
        if (is.finite(r) && is.finite(r.old) && abs(r-r.old)/(outer.eps+abs(r-r.old))<outer.eps)
            break
        theta<-a$par
        obj<-f(theta,...)
        if (obj>obj.old) break
    }
    if (i==outer.iterations){
        a$convergence<-7
        a$message<-"Barrier algorithm ran out of iterations and did not converge"
    }
    if (mu>0 && obj>obj.old){
        a$convergence<-11
        a$message<-paste("Objective function increased at outer iteration",i)
    }
    if (mu<0 && obj<obj.old){
        a$convergence<-11
        a$message<-paste("Objective function decreased at outer iteration",i)
    }

        
    a$outer.iterations<-i
    a$barrier.value<-a$value
    a$value<-f(a$par,...)
    a$barrier.value<-a$barrier.value-a$value
    a
    
}
