#  File src/library/stats/R/mahalanobis.R
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

mahalanobis <- function(x, center, cov, inverted=FALSE, ...)
{
    x <- if(is.vector(x)) matrix(x, ncol=length(x)) else as.matrix(x)
    ## save speed in customary case:
    ## if(any(center != 0))
    x <- sweep(x, 2, center)# = (x - center)

    ## The following would be considerably faster for  small nrow(x) and
    ## slower otherwise; probably always faster if the two t(.) weren't needed:
    ##
    ##	retval <- apply(x * if(inverted) x%*%cov
    ##	                    else    t(solve(cov,t(x), tol=tol.inv)),
    ##			1, sum)
    if(!inverted)
	cov <- solve(cov, ...)
    retval <- rowSums((x%*%cov) * x)
    names(retval) <- rownames(x)
    retval
}
