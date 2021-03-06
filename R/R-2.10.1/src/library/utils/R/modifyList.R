#  File src/library/utils/R/modifyList.R
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

### Originates from Deepayan Sarkar as  updateList() from 'lattice' package
modifyList <- function(x, val)
{
    stopifnot(is.list(x), is.list(val))
    xnames <- names(x)
    for (v in names(val)) {
	x[[v]] <-
	    if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
		modifyList(x[[v]], val[[v]]) else val[[v]]
    }
    x
}
