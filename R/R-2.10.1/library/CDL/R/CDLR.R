scalarFR <- function(TS, P, Method, state=NULL) { 
	
l = length(TS[1,]);

output = .C("scalarFR",
		x = as.integer(TS[1,]),
		y = as.double(TS[2,]),		
		n = as.integer(l),
		method = as.integer(Method),
		params = as.integer(P),
		np = as.integer(length(P)),
		xout = as.integer(vector("numeric",l)),
		ym = as.double(vector("numeric",l)),
		pvalue = as.double(vector("numeric",l)),
		stateString = as.character(state)
	);

result = list(output$xout,output$ym, output$pvalue, output$state);	

return(result);	




}
