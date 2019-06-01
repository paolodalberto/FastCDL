


#
# * Generates a new time series based upon the scalarF/scalarFBASIC interface algorithm.
# * This function accepts a previous state as argument, a single tuple or list of tuples
# * as the second argument, and a dictionary of configuration parameters (optional). The second
# * argument is the input time series and must consist of x-y value tuples where x is a long and y
# * is a double.
# * @param self NULL pointer.
# * @param args tuple containing the arguments passed to this function.
# * @return tuple containing the new state and the output time series.
# *
# * HoltWintersMethod       0    P = { 1, 1.0, 0.0, 0.0, 1, BOTHSIDE }
# * NonParametricMethod     1    P = (Rsize, Wsize, 1, Ratio=0.2, MST=0/POS=1,1.0) 
# * MovingAverageMethod     2    P = { 1, 1.0, 0.0, 0.0, 1, BOTHSIDE }
# * MartingaleMethod        3    P = (e=0.93, Dt=3,MAX=20, Ratio=0.2, Wsize, 0=n^2/1=average/2=PDF) 
# * CompressionMethod       4    P = (Rsize,Wsize,1,#BOOTSTRAP)
# * KernelMethodMMD         5    P = (Rsize,Wsize,RBF=1,MMD^2=1/Linear=0)
# *
# * 


# examples of parameters array 
PHW  = c(0, 0.3, 0.2, 0, 2, 0)     # Holt-Winters
PNON = c(200,200,1,0.2,0,1.0)      # Non-Parametric 
PMV  = c(20, 0.3, 0.2, 0.4, 2, 0)  # Moving Average
PMAR = c(0.92, 3, 20, 400, 0)      # Martingale
PKS = c(200,200,1,10)              # Compression
PKER = c(250,250,1,0)              # Kernel 

XVAL = 1
YVAL = 2
PVAL = 3
YMVAL= 4
DMVAL= 5
STATEVA=6


CompareCDFs <- function(Y0,Y1, T) { 

  l0 = length(Y0)
  l1 = length(Y1)
  distance =  vector("numeric",1);
  threshold = vector("numeric",1);
  threshold[1] = T
  pvalue = vector("numeric",1);
  output = .C("CompareCDFs",
    y1 = as.double(Y0),
    len1 = as.integer(l0),
    y2 = as.double(Y1),
    len2 = as.integer(l1),
    threshold = as.double(T),
    distance   = as.double(distance),
    pvalue = as.double(pvalue)
    )
  
  result = output$pvalue

}

CompareHistogramNumeric <- function(X0,Y0, X1,Y1, T) { 

  l0 = length(X0)
  l1 = length(X1)
  distance =  vector("numeric",1);
  threshold = vector("numeric",1);
  threshold[1] = T
  pvalue = vector("numeric",1);
  output = .C("CompareHistogramNumeric",
    x1 = as.double(X0),
    y1 = as.double(Y0),
    len1 = as.integer(l0),
    x2 = as.double(X1),
    y2 = as.double(Y1),
    len2 = as.integer(l1),
    threshold = as.double(T),
    distance   = as.double(distance),
    pvalue = as.double(pvalue)
    )
  
  result = output$pvalue

}

ExtendedCompareHistogram <- function(X0,Y0, X1,Y1, T) { 

  l0 = length(X0)
  l1 = length(X1)
  distance =  vector("numeric",1);
  pvalue =  vector("numeric",1);
  threshold = vector("numeric",1);
  L = vector("numeric",1);
  L[1] = 32

  
  ds   =  vector("numeric",32);
  pvs  =  vector("numeric",32);
  
  
  output = .C("ExtendedCompareHistogram",
    x1 = as.character(X0),
    y1 = as.double(Y0),
    len1 = as.integer(l0),
    x2 = as.character(X1),
    y2 = as.double(Y1),
    len2 = as.integer(l1),
    threshold = as.double(T),
    distances = as.double(ds),
    pvalues =   as.double(pvs),
    outlen = as.integer(L),
    distance   = as.double(distance),
    pvalue = as.double(pvalue)
    )
  
  result = list (
    output$distance,
    output$pvalue,
    output$distances,
    output$pvalues
    )
}



scalarFR <- function(TS, P, Method, state) { 
  
    
  
  l   = length(TS[1,]);
  s   = length(TS[,1]) -1 ;
  
  ox  = vector("numeric",l);
  oy = vector("numeric",l);
  opv = vector("numeric",l);
  
  if (Method == 0 || Method == 2)  { 
    totsize = l*s
  } else {
    totsize = l
  }
  
  oym = vector("numeric",totsize);
  odm = vector("numeric",totsize);
  
  
  dime = dim(TS)[1]-1;
  outn = vector("numeric",1)
                                        #print(TS);
                                        #print(P)
                                        #print(Method)
  print(state)
  
  
  print(l);
  print(dime)
  
   #options(digits=14)
  
  output = .C("scalarFRBASIC",
    x = as.integer(TS[1,]),
    y = as.double(TS[2:(dime+1),]),		
    n = as.integer(l),
    dimensions = as.integer(dime),	
    method = as.integer(Method),
    params = as.double(P),
    np = as.integer(length(P)),
    xout = as.integer(ox),
    yout = as.double(oy),
    pvalue = as.double(opv),
    ym = as.double(oym),
    dm = as.double(odm),
    outn = as.integer(outn),
    stateString = as.character(state)
    );

  print("Scal")
    
  if ((Method == 0 || Method == 2) && s>1) { 
    
    result = list(
      output$xout,    
      output$yout,
      output$pvalue, 
      array(output$ym,dim=c(s,l)),
      array(output$dm,dim=c(s,l)),
      output$state);	
  } else {
    
    
    result = list(
      output$xout,    
      output$yout,
      output$pvalue, 
      output$ym,
      output$dm,
      output$state);	
    
    
  }


  return(result);	
}
scalarFRE <- function(TS, P, Method, state) { 
  print("scalarFRE")
  print(TS)
  print(c("P",P))
  print(c("method",Method))
  print(c("state",state)) 
  
  l   = length(TS[1,]);
  s   = length(TS[,1]) -1 ;
  
  ox  = vector("numeric",l);
  oy = vector("numeric",l);
  opv = vector("numeric",l);
  
  if (Method == 0 || Method == 2)  { 
    totsize = l*s
  } else {
    totsize = l
  }
  
  oym = vector("numeric",totsize);
  odm = vector("numeric",totsize);
  
  
  dime = dim(TS)[1]-1;
  outn = vector("numeric",1)
                                        #print(TS);
                                        #print(P)
                                        #print(Method)
  
  print(l);
  print(dime)

  G =  as.double(TS[2:(dime+1),])
  print(G)
  #options(digits=14)
  
  output = .C("scalarFRBASIC",
    x = as.integer(TS[1,]),
    y = as.double(TS[2:(dime+1),]),		
    n = as.integer(l),
    dimensions = as.integer(dime),	
    method = as.integer(Method),
    params = as.double(P),
    np = as.integer(length(P)),
    xout = as.integer(ox),
    yout = as.double(oy),
    pvalue = as.double(opv),
    ym = as.double(oym),
    dm = as.double(odm),
    outn = as.integer(outn),
    stateString = as.character(state)
    );

  if ((Method == 0 || Method == 2) && s>1) { 
    
    result = list(
      output$xout,    
      output$yout,
      output$pvalue, 
      array(output$ym,dim=c(s,l)),
      array(output$dm,dim=c(s,l)),
      output$state);	
  } else {
    
    
    result = list(
      output$xout,    
      output$yout,
      output$pvalue, 
      output$ym,
      output$dm,
      output$state);	
    
    
  }


  return(result);	
}

methodCode <- function(name) { 

code = 0;

if (name =="Non-Parametric")  {
	code =  1;
}  
if (name == "Holt-Winters") { 
	code =  0;
}	
if (name =="Moving-Average")  {
	code =  2;
}  
if (name == "Martingale") { 
	code =  3;
}	
if (name == "Compression") { 
	code =  4;
}	
if (name == "Kernels") { 
	code =  5;
}	


code


}
visualize  <- function(ts, result,P) { 

  # result = list(output$xout,output$yout, output$pvalue, output$ym, output$dm,output$state);
  
  x <- result[[1]]> 0
  c = colors();
  f =  as.integer(runif(dim(ts)[1],0,length(c)))
#  print(c)
#  print(f)
#  print (2*dim(ts)[1])
  if (length(dim(result[[4]])) >0) 
    par(mfrow=c(dim(ts)[1]+dim(result[[4]])+2,1))
  else 
    par(mfrow=c(dim(ts)[1]+2,1))
  
  plot(ts[1,],ts[2,],type='l',col=c[f[1]])
  i =3;
  while (i<=dim(ts)[1]) {
    print(c[f[i]])
    plot(ts[1,],ts[i,],type='l',col=c[f[i]])
    i = i +1
  }     	
  if (length(P)==5) 
    plot(result[[1]][x],log(result[[2]][x],10))
  else
    plot(result[[1]][x],result[[2]][x])

  dout = dim(result[[4]])
  if (length(dout) > 0) {
    f =  as.integer(runif(dout[1],0,length(c)))
    plot(result[[1]][x],result[[4]][1,x],type="l", col=c[f[1]]);
    for (i in 2:dout[1]) {
      plot(result[[1]][x],result[[4]][i,x],type="l", col=c[f[i]]);
    }     	
  }
  else {
    plot(result[[1]][x],result[[4]][x],type="l", col="blue");
    lines(result[[1]][x], result[[4]][x]+P[5]*result[[5]][x],col="red")
    lines(result[[1]][x], result[[4]][x]-P[5]*result[[5]][x], col="red")      
  }
  
  xa <- result[[3]] < 1.0 
  xq <- result[[3]]>= 1.0
  tem = result[[3]]
  tem[xa] = 0
  tem[xq] = 1
  
  plot(result[[1]][x],result[[3]][x], type="l", col="green")
  
  
}

visualize2  <- function(ts, result,P, lg=FALSE,t="") { 

  # result = list(output$xout,output$yout, output$pvalue, output$ym, output$dm,output$state);
  print(c("lg",lg));
  x <- result[[XVAL]]> 0
  c = colors();
  f =  as.integer(runif(dim(ts)[1],0,length(c)))
#  print(c)
#  print(f)
#  print (2*dim(ts)[1])

  par(mfrow=c(4,1))

  M  = max(ts[2:dim(ts)[1],])
  m  = min(ts[2:dim(ts)[1],])
  border = array(c(M,m),dim=c(1,dim(ts)[2]))
  
  
  plot(ts[1,],border,type='n',col=c[f[1]],main=paste("Input series",t,sep=" "))
  i =2;
  while (i<=dim(ts)[1] && i < 10) {
    #print(c[f[i]])
    lines(ts[1,],ts[i,],type='p',col=c[f[i]])
    i = i +1
  }     	

  
  if (length(P)==5) 
    plot(result[[XVAL]][x],log(result[[YVAL]][x],10),main="Response")
  else
    plot(result[[XVAL]][x],result[[YVAL]][x],main="Response")

  
  dout = dim(result[[YMVAL]])
  if (length(dout) > 0) {
    f =  as.integer(runif(dout[1],0,length(c)))
    M  = max(result[[YMVAL]])
    m  = min(result[[YMVAL]])
    border = array(c(M,m),dim=c(1,length(x)))
    plot(result[[XVAL]][x],border,type="n", col=c[f[1]], main="Models");
    for (i in 1:dout[1]) {
      lines(result[[XVAL]][x],result[[YMVAL]][i,x],type="l", col=c[f[i]]);
    }     	
  }
  else {
    if (lg ==TRUE) { 
      plot(result[[XVAL]][x],  log(result[[YMVAL]][x]),type="l", col="blue", main="Martingale");

    } else {
      plot(result[[XVAL]][x],  result[[YMVAL]][x],type="l", col="blue", main="Bounds Models");
      lines(result[[XVAL]][x], result[[YMVAL]][x]+P[5]*result[[DMVAL]][x],col="red")
      lines(result[[XVAL]][x], result[[YMVAL]][x]-P[5]*result[[DMVAL]][x], col="red")
    }
  }
  
  xa <- result[[PVAL]] < 1.0 
  xq <- result[[PVAL]]>= 1.0
  tem = result[[PVAL]]
  tem[xa] = 0
  tem[xq] = 1
  
  plot(result[[XVAL]][x],result[[PVAL]][x], type="b", col="green", main="P-value")
  
  
}
visualize3  <- function(ts, result,P, lg=FALSE,t="", INT) { 

  # result = list(output$xout,output$yout, output$pvalue, output$ym, output$dm,output$state);
  print(c("lg",lg));
  x <- result[[XVAL]]> 0
  c = colors();
  f =  as.integer(runif(dim(ts)[1],0,length(c)))
#  print(c)
#  print(f)
#  print (2*dim(ts)[1])

  if (lg) { 
    par(mfrow=c(4,1))
  }
  else {
    par(mfrow=c(3,1))
  }
  M  = max(ts[2:dim(ts)[1],])
  m  = min(ts[2:dim(ts)[1],])
  border = array(c(M,m),dim=c(1,dim(ts)[2]))
  
  
  plot(ts[1,],border,type='n',col=c[f[1]],main=paste("Input series",t,sep=" "))
  i =2;
  while (i<=dim(ts)[1] && i < 10) {
    #print(c[f[i]])
    lines(ts[1,],ts[i,],type='p',col=c[f[i]])
    i = i +1
  }     	
  abline(v=INT)
  
  if (length(P)==5) 
    plot(result[[XVAL]][x],log(result[[YVAL]][x],10),main="Response")
  else
    plot(result[[XVAL]][x],result[[YVAL]][x],main="Response")

  abline(v=INT)
  
  if (lg ==TRUE) { 
    plot(result[[XVAL]][x],  log(result[[YMVAL]][x]),type="l", col="blue", main="Martingale");
    abline(v=INT)
  }



  xa <- result[[PVAL]] < 1.0 
  xq <- result[[PVAL]]>= 1.0
  tem = result[[PVAL]]
  tem[xa] = 0
  tem[xq] = 1
  
  plot(result[[XVAL]][x],result[[PVAL]][x], type="b", col="green", main="P-value")
  abline(v=INT)
  
}


dis <- function(e,v) {

  result = array(0,c(length(e)))
  
  for (i in 1:length(e)) {
    result[i] = prod(famma(e[i],v) )
   
  }

  result
    
}




createM <- function(d,length,T) {

  l = length+(length - as.integer((length/T))*T)
  print(l)
  time = array(0,dim=c(d,l))
  time[1,] = 1:l

  i = as.integer((length/T))
  #print(c("i",i))
  step = (log(50,10)-log(0.05,10))/(i-3)

  v = 1:(i-2)
  v[1] = log(0.05,10)
  for (i in 2:(i-2)) {
    v[i] = v[i-1] + step
  }
  median = 10^v
  
  
  for (i in 2:d) {
    time[i,1:(2*T)] = rnorm(2*T,0,1);
    for (j in 2:(l/T-1)) {
      #print(c("average",median[j-1]))
      time[i,(1+j*T):((j+1)*T)] = rnorm(T,median[j-1],1); 
      
    }
  }
  time
}

createE <- function(d,length,T) {

  l = length+(length - as.integer((length/T))*T)
  print(l)
  time = array(0,dim=c(d,l))
  time[1,] = 1:l

  i = as.integer((length/T))
  #print(c("i",i))
  step = (log(50,10)-log(0.05,10))/(i-3)

  v = 1:(i-2)
  v[1] = log(0.05,10)
  for (i in 2:(i-2)) {
    v[i] = v[i-1] + step
  }
  median = 10^v
  
  
  for (i in 2:d) {
    t = time[i,1:(T)] = rnorm(T,0,1);
    for (j in 1:(l/T-1)) {
      #print(c("average",median[j-1]))
      time[i,(1+j*T):((j+1)*T)] = t
      
    }
  }
  time
}



createMixing <- function(d,length,T) {

  l = length+(length - as.integer((length/T))*T)
  print(l)
  time = array(0,dim=c(d,l))
  time[1,] = 1:l

  
  for (i in 2:d) {
    time[i,1:(2*T)] = rnorm(2*T,0,1);
   
    for (j in 2:(l/T-1)) {
      #print(c("average",median[j-1]))
      alpha = 1- (j-1)/(l/T-1)
      temp = 1:T
      #print(c(floor(T*alpha)))
      if (alpha>0) {
        temp[1:floor(T*alpha)] =  rnorm(floor(T*alpha),0,1)
      }
      temp[(floor(T*alpha)+1):T] =  runif(T - floor(T*alpha),-2.3,2.3)
      
      time[i,(1+j*T):((j+1)*T)] = sample(temp)
      
    }
  }
  time
}


createV <- function(d,length,T) {

  l = length+(length - as.integer((length/T))*T)
  
  
  time = array(0,dim=c(d,l))
  time[1,] = 1:l


  i = as.integer((length/T))
  print(c("i",i))
  step = (1-0.01)/(i-3)

  v = 1:(i-2)
  v[1] = 0.01
  for (j in 2:(i-2)) {
    v[j] = v[j-1] + step
  }
  median = 10^v

  
  for (i in 2:d) {
    time[i,1:(2*T)] = rnorm(2*T,0,1); 
    for (j in 2:(l/T - 1)) {
      #print(c("sigmaquit",median[j-1]))
      time[i,(j*T+1):((j+1)*T)] = rnorm(T,0,median[j-1]); 
      
    }
  }
  time
}

createV2 <- function(d,length,T) {

  l = length+(length - as.integer((length/T))*T)
  
  
  time = array(0,dim=c(d,l))
  time[1,] = 1:l


  i = as.integer((length/T))
  print(c("i",i))
  step = (1-0.01)/(i-3)

  v = 1:(i-2)

  v[1] = 0.01
  for (j in 2:(i-2)) {
    v[j] = v[j-1] + step
  }

  u = v[length(v):1]
  
  median = 10^u

  
  for (i in 2:d) {
    time[i,1:(2*T)] = rnorm(2*T,0,median[1]); 
    for (j in 2:(l/T - 1)) {
      #print(c("sigmaquit",median[j-1]))
      time[i,(j*T+1):((j+1)*T)] = rnorm(T,0,median[j-1]); 
      
    }
  }
  time
}


createB <- function(d,length,T,create=createM) {
  t = create(d,length,T)
  x = t[1,(1+length/2):(length/2+T)]
  t[,(1+length/2):(length/2+T)] = t[,(length-T+1):length]
  t[1,(1+length/2):(length/2+T)] = x
  t
}


Smolatest <- function(d,times) {
  P = c(250,250,1,0.5,0,1)
  h = (1:times)*0
  for (i in 1:times) {
     state = ""
     t = createM(d,5250,250)
     r = scalarFR(t,P,methodCode("Non-Parametric"),state);
     h[i] = sum(r[[3]])/20
     visualize2(t, r,P)
   }
  print(h)
  print(c("median", median(h)))

}

SmolatestX<- function(d,times,create,p) {
  P = p
  h = (1:times)*0
  for (i in 1:times) {
     state = ""
     t = create(d,5250,250)
     r = scalarFR(t,P,methodCode("Non-Parametric"),state);
     h[i] = sum(r[[3]])/20
     visualize2(t, r,p)
   }
  print(h)
  print(c("median", median(h)))
  median(h)
}
SmolatestXM<- function(d,times,create,p,M,N) {
  P = p
  h = (1:times)*0
  for (i in 1:times) {

    state = ""
    t = create(d,21*N,N)
    r = scalarFR(t,P,methodCode(M),state);
    h[i] = sum(r[[3]])/19
    visualize2(t, r,PNON, lg = (M =="Martingale"))
   }
  print(h)
  print(c("median", median(h)))
  sum(h)/length(h)
  
}
SmolatestXMEarly<- function(d,times,create,p,M,N) {
  P = p
  h = (1:times)*0
  for (i in 1:times) {

    state = ""
    t = create(d,21*N,N)
    r = scalarFR(t,P,methodCode(M),state)
    visualize2(t, r,P, lg = (M =="Martingale"),t = M)
    print(c("x1", 2*N, "len ",  length(r[[1]])))
    if (M== "Martingale") { 
      first = sort(r[[1]][r[[2]]>0.95])
    }
    else {
       first = sort(r[[1]][r[[3]]>0.95])
    }
    
    print(first)
    print(c("First ",first[1], "x1", 2*N, "len ",  length(r[[1]])))
    h[i]  = 1 - (first[1]-2*N)/(length(r[[1]])-2*N)

   }
  print(h)
  print(c("median", median(h)))
  sum(h)/length(h)
  
}
SmolatestXMEarlyP<- function(d,times,create,M,N) {
  
  
  if (M=="Kernels")  {
    PKER[1] = N
    PKER[2] = N

    P = PKER

  } else if (M=="Non-Parametric"){
    PNON[1] = N
    PNON[2] = N

    P = PNON
  } else if (M == "Martingale")  {
    PMAR[5] = N

    P = PMAR
    
  } else if (M == "Compression")  {
    PKS[1] = N
    PKS[2] = N
    
    P = PKS
    
  } else if (M=="Holt-Winters") {
      P = PHW   
  }
  else  {
  }

  h = (1:times)*0
  for (i in 1:times) {

    state = ""
    t = create(d,21*N,N)
    r = scalarFR(t,P,methodCode(M),state)
    #visualize2(t, r,P, lg = (M =="Martingale"),t = M)

    visualize3(t, r,P, lg = (M =="Martingale"),t = M,INT = (1:21)*N)
    print(c("x1", 2*N, "len ",  length(r[[1]])))
    if (M== "Martingale") { 
      first = sort(r[[1]][r[[2]]>0.95])
    }
    else {
       first = sort(r[[1]][r[[3]]>0.95])
    }
    
    print(first)
    if (length(first)==0 || is.na(first)) {
      h[i] = 0
    }
    else { 
      print(c("First ",first[1], "x1", 2*N, "len ",  length(r[[1]])))
      h[i]  = 1 - ((first[1]-2*N))/((length(r[[1]])-2*N))
    }
  }
  h = sort(h)
  print(h)
  print(c("median", median(h)))
  sum(h)/length(h)
  
}

M1 <- function(p,M,N,D,create,times) {
  t = 1:length(D)
  i = 1;
  for ( d in D) {
    print(c("dim", d))
    t[i] = SmolatestXM(d,times,create,p,M,N)
    i = i +1
  }

  #plot(D,t)
  t
}

M2 <- function(p,M,N,D,create,times) {
  t = 1:length(D)
  i = 1;
  for ( d in D) {
    print(c("dim", d))
    t[i] = SmolatestXMEarly(d,times,create,p,M,N)
    i = i +1
  }

  #plot(D,t)
  t
}

M3 <- function(M,N,D,create,times) {
  t = 1:length(D)
  i = 1;
  for ( d in D) {
    print(c("dim", d))
    t[i] = SmolatestXMEarlyP(d,times,create,M,N)
    i = i +1
  }

  #plot(D,t)
  t
}

gplot <- function(d,m,labels) {

  tem = (1:length(d[[1]]))*0
  tem[1] = 1
  plot(log(d[[1]],10),tem,type="n",xlab ="log(d,10)",ylab ="% correctly rejection")

  c = colors();
  
  #f =  as.integer(runif(length(labels),0,length(c)))

  #f = c[f[1:length(labels)]]
  f = c("darkblue", "blue", "darkred", "red", "darkgreen", "green", "darkyellow", "yellow")  
  
  for (i in 1:length(labels)) {
    print(i)
    lines(log(d[[i+1]],10), m[[i]],col=f[i],type="b")
  }
  legend(x="bottomright",legend=labels,col=f,lty=5,lwd=2)
  
  0
  
}

gplot2 <- function(d,m,labels,title) {

  tem = (1:length(d))*0
  tem[1] = 100
  plot(log(d,10),tem,type="n",xlab ="log(d,10)",ylab ="% correct rejection",main = title)

  c = colors();
  
  #f =  as.integer(runif(length(labels),0,length(c)))

  #f = c[f[1:length(labels)]]
  f = c( "blue", "red", "green", "orange", "violet", "cyan","darkblue", "darkred", "darkgreen", "darkorange", "darkviolet", "darkcyan" )
  
  #f = c("darkblue", "blue", "darkred", "red", "darkgreen", "green", "darkorange", "orange", )  
  
  for (i in 1:length(labels)) {
    t = m[[i]]* 100
    print(c(i,labels[i]))
    print(length(t))
    print(t)
    
    
    if (length(d) > length(m[[i]])) { 
      lines(log(d[1:length(m[[i]])],10), t,col=f[i],type="b",lwd=2)
    }
    else {
      lines(log(d,10), t,col=f[i],type="b",lwd=2)
    }
        
        
  }
  legend(x="bottomright",legend=labels,col=f,lty=5,lwd=2, bty="o")
  
  0
  
}

gplot3 <- function(d,m,labels,title,f=c( "blue", "red", "green", "orange", "violet", "cyan","darkblue", "darkred", "darkgreen", "darkorange", "darkviolet", "darkcyan" )) {

  tem = (1:length(d))*0
  tem[1] = 100
  plot(log(d,10),tem,type="n",xlab ="log(d,10)",ylab ="% correct rejection",main = title)

  c = colors();
  
  #f =  as.integer(runif(length(labels),0,length(c)))

  #f = c[f[1:length(labels)]]
  #f = c( "blue", "red", "green", "orange", "violet", "cyan","darkblue", "darkred", "darkgreen", "darkorange", "darkviolet", "darkcyan" )
  
  #f = c("darkblue", "blue", "darkred", "red", "darkgreen", "green", "darkorange", "orange", )  
  
  for (i in 1:length(labels)) {
    t = m[[i]]* 100
    print(c(i,labels[i]))
    print(length(t))
    print(t)
    
    
    if (length(d) > length(m[[i]])) { 
      lines(log(d[1:length(m[[i]])],10), t,col=f[i],type="b",lwd=2)
    }
    else {
      lines(log(d,10), t,col=f[i],type="b",lwd=2)
    }
        
        
  }
  legend(x="bottomright",legend=labels,col=f,lty=5,lwd=2, bty="o")
  
  0
  
}









diff <- function(L) {
  for (i in 1:length(L)) {
    print(c(L[[i]][2],L[[i]][3]))

  }


}

diffa <- function(Ms) {

  for (i in 1:length(Ms[[1]])) {
    
    all = "diff"
    one =  "ind"
    for (j in 1:length(Ms)) {
                                        #print(c(i,j))
                                        #print(Ms[[j]][[i]])
      if (Ms[[j]][[i]][3] == "different") {
        one = "diff"
        if (all == "diff") { all= "diff"} else {all= "ind"}
      } else {   
        all = "ind"
      }
    }
                                        #print(c("all", all, "one",one,Ms[[1]][[i]][2]))
    
    if (all == one) {

      print(c(Ms[[1]][[i]][2], all, one))
    } else {
      print(c("all", all, "one",one))
    }
  }
  
}


patch <- function (l) {

  final = vector(mode="integer",length= 2)
  final[1] = dim(l[[1]])[1]+1
  final[2] = 0;
  
  for (i in 1:length(l)) {
    print (dim(l[[i]]))
    final[2] = final[2] + dim(l[[i]])[2]
  }
  print(final)
  
  t = array(0,dim = c(final[1],final[2]))
  left=0
  for (i in 1:length(l)) {
    print(c("i",i,left))
    print(c(dim(t[,left+(1:dim(l[[i]])[2])]),dim(l[[i]])))
    t[2:final[1],left+(1:dim(l[[i]])[2])] = l[[i]]
    left = left +  dim(l[[i]])[2]
  }

  t[1,] = 1:final[2]
  
t


}

SXMEarlyP<- function(t,ind,M,N) {
  
  
  if (M=="Kernels")  {
    PKER[1] = N
    PKER[2] = N

    P = PKER

  } else if (M=="Non-Parametric"){
    PNON[1] = N
    PNON[2] = N

    P = PNON
  } else if (M == "Martingale")  {
    PMAR[5] = N

    P = PMAR
    
  } else if (M == "Compression")  {
    PKS[1] = N
    PKS[2] = N
    
    P = PKS
    
  } else  { }

  r = scalarFR(t,P,methodCode(M),state)
  visualize3(t, r,P, lg = (M =="Martingale"),t = M,INT =ind)

  if (M== "Martingale") { 
    first = sort(r[[1]][r[[2]]>0.95])
  }
  else {
    first = sort(r[[1]][r[[3]]>0.95])
  }
  
  print(first)
  print(c("First ",first[1], "x1", 2*N, "len ",  length(r[[1]])))
   1 - ((first[1]-2*N))/((length(r[[1]])-2*N))

  
}

SClassification <- function(f,i,N,title) {  
  a     = read.csv(paste("/export/crawlspace/Paolo/FastLibrary_Multidimensional/DATA/",f,sep=""),header=FALSE)
  index = read.csv(paste("/export/crawlspace/Paolo/FastLibrary_Multidimensional/DATA/",i,sep=""),header=FALSE)
  c = t(a)
  R = SXP(c,index,N,title)
}

SXP<- function(t,ind,N,title) {
  
  res = list()
  labels = list()
  

  state = "" 
  M = "Martingale"
  PMAR[5] = N
  P = PMAR
  rmar = scalarFR(t,P,methodCode(M),state)

  res[[1]] = rmar 
  labels[[1]] = "Martingale"


  M="Kernels"
  
  state = ""
  PKER[1] = N
  PKER[2] = N
  PKER[4] = 1
  
  P = PKER
  
  rkers = scalarFR(t,P,methodCode(M),state)

  res[[2]] = rkers
  labels[[2]] = "Kernels"


  PKER[4] = 0
  P = PKER
  rkerl = scalarFR(t,P,methodCode(M),state)

  res[[3]] = rkerl
  labels[[3]] = "Kernels linear"

  
  M="Non-Parametric"
  state = ""
  PNON[1] = N
  PNON[2] = N
  PNON[5] = 0
  P = PNON

  rmst = scalarFR(t,P,methodCode(M),state)

  res[[4]] = rmst
  labels[[4]] = "MST"

  
  PNON[5] = 1
  P = PNON
  state = ""
  rpos = scalarFR(t,P,methodCode(M),state)

  res[[5]] = rpos
  labels[[5]] = "POSET"
  

  
  state = ""
  M = "Compression"
  PKS[1] = N
  PKS[2] = N
  
  P = PKS
  rks = scalarFR(t,P,methodCode(M),state)

  res[[6]] = rks 
  labels[[6]] = "Compression"

#  fname = paste(title,N,".pdf",sep="")
#  print(fname)
#  pdf(file=fname, width=14, height=10)
 
#  visualize4(t,ind,res,labels,title)
#  dev.off()
  fname = paste(title,"_",N,".png",sep="")
  print(fname)
  png(file=fname, width=14, height=10,units="in",bg="transparent",res=120)
 
  visualize4(t,ind,res,labels,title)
  dev.off()
  visualize4(t,ind,res,labels,title)

  
  list(res,labels)
}


visualize4  <- function(ts, INT,result,labels,t,labelsts=c('')) { 

  print("visualize 4")
  f = c( "blue", "red", "green", "orange", "violet", "cyan")
  newlabels = labels
  fr  = f
  for (i in 1:length(labels)) {
    newlabels[i] = labels[length(labels)+1-i]
    fr[i] = f[length(labels)+1-i]

  }
  fp = f
  if (dim(ts)[1]>7) {
    print("using random colors")
    
    c = colors();
    fp =  as.integer(runif(dim(ts)[1],0,length(c)))
    fp = c[fp]
  }
  
  par(mfrow=c(3,1))
  
  
  M  = max(as.double(ts[2:(dim(ts)[1]),]))
  m  = min(as.double(ts[2:(dim(ts)[1]),]))

  print(c(M,m))
  
  border = array(c(max(M),min(m)),dim=c(1,dim(ts)[2]))
  
  
  plot(ts[1,],border,type='n',col=fp[1],main=paste("Input series",t,sep=" "))
  i =2;
  while (i<=dim(ts)[1]) {
    #print(c(fp[i-1],ts[i,]))
    lines(ts[1,],ts[i,],type='p',col=fp[i-1],lty=5,lwd=2)
    i = i +1
  }     	
  abline(v=INT)

  if (length(labelsts)>2) {
    legend(x="topleft",legend=labelsts,col=fp,lty=5,lwd=2, bty="b",cex=1.5)

  }
  


  
  x <- result[[1]][[XVAL]]> 0
  M  = max(result[[1]][[YVAL]][x])
  m  = min(result[[1]][[YVAL]][x])
  
  for ( i in 2:length(result)) {
    x <- result[[i]][[XVAL]]> 0
    M  = max(c(M,max(i*result[[i]][[YVAL]][x])))
    m  = min(c(m,min(i*result[[i]][[YVAL]][x])))
  }

 
  border = array(c(M,m),dim=c(1,dim(ts)[2]))
  plot(ts[1,],border,type='n',col=c[f[1]],main="Response")
  for ( i in 1:length(result)) {
    x <- result[[i]][[XVAL]]> 0
    lines(result[[i]][[XVAL]][x],i*result[[i]][[YVAL]][x],type='b',col=f[i],lty=5,lwd=2)
  }
  
  legend(x="left",legend=newlabels,col=fr,lty=5,lwd=2, bty="b",cex=1.5)
  abline(v=INT)


  

  #for ( i in 1:length(result)) {
  #  print(labels[[i]])
  #  if (labels[[i]] == "Martingale") {
  #    plot(result[[i]][[XVAL]][x],  log(result[[i]][[YMVAL]][x]),type="l", col="blue", main="Martingale");
  #    abline(v=INT)
  #  }
  #}
  
 
  x <- result[[1]][[XVAL]]> 0
  M  = max(result[[1]][[PVAL]][x])
  m  = min(result[[1]][[PVAL]][x])

  for ( i in 2:length(result)) {
   
    x <- result[[i]][[PVAL]]> 0
    M  = max(c(M,max(i*result[[i]][[PVAL]][x])))
    m  = min(c(m,min(i*result[[i]][[PVAL]][x])))
    
  }
  border = array(c(M,m),dim=c(1,dim(ts)[2])) 
  plot(ts[1,],border,type='n',col="green",main="P-Value")
  for ( i in 1:length(result)) {
    x <- result[[i]][[XVAL]]> 0
    print(c("PVAL",labels[[i]]))
    lines(result[[i]][[XVAL]][x],i*result[[i]][[PVAL]][x],type="b",col=f[i],lty=5,lwd=2)
  }
  legend(x="left",legend=newlabels,col=fr,lty=5,lwd=2, bty="b",cex=1.5)
  abline(v=INT)

  
}


reproduce <- function() {

R = SClassification("breast-cancer-wisconsin/wdbc.data.csv", "breast-cancer-wisconsin/index.csv", 50, "Breast-Cancer")
R = SClassification("breast-cancer-wisconsin/wdbc.data.csv", "breast-cancer-wisconsin/index.csv", 100, "Breast-Cancer")
R = SClassification("breast-cancer-wisconsin/wdbc.data.csv", "breast-cancer-wisconsin/index.csv", 200, "Breast-Cancer")
R = SClassification("breast-cancer-wisconsin/wdbc.data.csv", "breast-cancer-wisconsin/index.csv", 300, "Breast-Cancer")


R = SClassification("parkisons/telemonitoring/parkinsons_updrs.data.csv", "parkisons/telemonitoring/index.csv", 400, "Parkinsons")
R = SClassification("parkisons/telemonitoring/parkinsons_updrs.data.csv", "parkisons/telemonitoring/index.csv", 300, "Parkinsons")
R = SClassification("parkisons/telemonitoring/parkinsons_updrs.data.csv", "parkisons/telemonitoring/index.csv", 200, "Parkinsons")
R = SClassification("parkisons/telemonitoring/parkinsons_updrs.data.csv", "parkisons/telemonitoring/index.csv", 100, "Parkinsons")


R = SClassification("abalone/abalone.data.csv", "abalone/index.csv", 400, "Abalone")
R = SClassification("abalone/abalone.data.csv", "abalone/index.csv", 300, "Abalone")
R = SClassification("abalone/abalone.data.csv", "abalone/index.csv", 200, "Abalone")
R = SClassification("abalone/abalone.data.csv", "abalone/index.csv", 100, "Abalone")

R = SClassification("yeast/yeast.data.csv", "yeast/index.csv", 400, "Yeast")
R = SClassification("yeast/yeast.data.csv", "yeast/index.csv", 300, "Yeast")
R = SClassification("yeast/yeast.data.csv", "yeast/index.csv", 200, "Yeast")
R = SClassification("yeast/yeast.data.csv", "yeast/index.csv", 100, "Yeast")

}


#PKER[4] = 1; resMixMMD <- M3("Kernels",250,d,createMixing,100);  write(resMixMMD,file="resMixMMD")
#PKER[4] = 0; resMixMMDL <- M3("Kernels",250,d,createMixing,100); write(resMixMMDL,file="resMixMMDL")
#resMixMar <- M3("Martingale",250,d,createMixing,100); write(resMixMar,file="resMixMar")
#resMixKS <- M3("Compression",250,d,createMixing,100); write(resMixKS,file="resMixKS")
#PNON[5] =1; resMixPOSET <- M3("Non-Parametric",250,d,createMixing,100); write(resMixPOSET,file="resMixPOSET")
#PNON[5] =0; resMixMST <- M3("Non-Parametric",250,d,createMixing,100); write(resMixMST,file="resMixMST")

SpearmanPMachine <- function(machine,wn) {
  
  idpdlocation = paste("/export/crawlspace/Paolo/FastLibrary_Multidimensional/DATA/Ros/IDPD", "_",machine,"/",idpdname,"_",machine,".csv",sep="")
  tempwm = wn
  names  = wn 
  
  for ( i in 1:length(wn)) {
    tempwm[i] = paste("/export/crawlspace/Paolo/FastLibrary_Multidimensional/DATA/Ros/SPEC_CINT2006_",machine,"/",wn[i],".csv",sep="")
    names[i] = paste(wn[i],"_",machine,".png",sep="")
    
  }
  names[length(wn)+1] = paste("idpd","_",machine,".png",sep="")
  
  S = rosarioScan(idpdlocation,tempwm,t=FALSE, rank=FALSE)
  SpearmanPS(S[[2]],names)
  

}


SpearmanPS <- function(u,names) {

  signatures = list()
  
  for (i in 1:length(u)) {
    v = u[[i]]
    d = dim(v)
    
    v = v[2:d[1],]
    png(file=names[i],width=14, height=10,units="in",bg="transparent",res=120)
    signatures[[i]] = SpearmanPR(v,names[i])
    dev.off()
  }


  signatures
}

SpearmanPR <- function(a,n) {

  f = c( "blue", "red", "green", "orange", "violet", "cyan")
  d = dim(a)
  signature = 1:(d[1]-1)
  ave = 1:(d[1])
  normalized = array(0,dim=d)
  sigma = 1:(d[1])
  t = (1:d[2])*0

  for  ( i in 1:(d[1])) {
    v = as.double(a[i,])
    ave[i]        = sum(v)/length(v)
    normalized[i,] = v - ave[i]
    sigma[i]      = sqrt(sum((normalized[i,])^2)/length(v))
    normalized[i,] = normalized[i,]/sigma[i]
  }
  t[1] = min(normalized)
  t[2] = max(normalized)

  plot(1:d[2],t,type='n',main=n)
  abline(h=c(-1,1))
  for  ( i in 1:(d[1])) {
    lines(1:d[2],normalized[i,],type='b',col=f[i]) 
  }

  for  ( i in 1:(d[1]-1)) {
    signature[i] = sum(normalized[1,]*normalized[i+1,])/d[2]
  }
  
  

  signature

}


cdma311009 <- function() { 
  
  resAveKS     = scan("resAveKS")
  resAveMMD    = scan("resAveMMD")
  resAveMMDL   = scan("resAveMMDL")
  resAveMST    = scan("resAveMST")
  resAveMar    = scan("resAveMar")
  resAvePOSET  = scan("resAvePOSET")
  resMixKS     = scan("resMixKS")
  resMixMMD    = scan("resMixMMD")
  resMixMMDL   = scan("resMixMMDL")
  resMixMST    = scan("resMixMST")
  resMixMar    = scan("resMixMar")
  resMixPOSET  = scan("resMixPOSET")
  resVarKS     = scan("resVarKS")
  resVarMMD    = scan("resVarMMD")
  resVarMMDL   = scan("resVarMMDL")
  resVarMST    = scan("resVarMST")
  resVarMar    = scan("resVarMar")
  resVarPOSET  = scan("resVarPOSET")


  ave = list()
  mix = list()
  var = list()

  ave[[1]] = resAveKS     
  ave[[2]] = resAveMMDL   
  ave[[3]] = resAveMMD    
  ave[[4]] = resAveMar    
  ave[[5]] = resAveMST    
  ave[[6]] = resAvePOSET  
  mix[[1]] = resMixKS     
  mix[[2]] = resMixMMDL    
  mix[[3]] = resMixMMD   
  mix[[4]] = resMixMar    
  mix[[5]] = resMixMST    
  mix[[6]] = resMixPOSET  
  var[[1]] = resVarKS     
  var[[2]] = resVarMMDL    
  var[[3]] = resVarMMD   
  var[[4]] = resVarMar    
  var[[5]] = resVarMST   
  var[[6]] = resVarPOSET


  png(file="EarlyAverageChange.png", width=14, height=10,units="in",bg="transparent",res=120)
  gplot2(d,ave,legend,"Average Change");
  dev.off()

  X11()
  gplot2(d,ave,legend,"Average Change");


  png(file="EarlyVarianceChange.png", width=14, height=10,units="in",bg="transparent",res=120)
  gplot2(d,var,legend,"Variance Change");
  dev.off()

  X11()
  gplot2(d,var,legend,"Variance Change");

  
  png(file="EarlyMixingChange.png", width=14, height=10,units="in",bg="transparent",res=120)
  gplot2(d,mix,legend,"Mixing Change");
  dev.off()

  X11()
  gplot2(d,mix,legend,"Mixing Change");

}


cdma311009A <- function() { 
  
  resAveKS     = scan("resM4AveKS")
  resAveMMD    = scan("resM4AveMMD")
  resAveMMDL   = scan("resM4AveMMDL")
  resAveMST    = scan("resM4AveMST")
  resAvePOSET  = scan("resM4AvePOSET")
  resMixKS     = scan("resM4MixKS")
  resMixMMD    = scan("resM4MixMMD")
  resMixMMDL   = scan("resM4MixMMDL")
  resMixMST    = scan("resM4MixMST")
  resMixPOSET  = scan("resM4MixPOSET")
  resVarKS     = scan("resM4VarKS")
  resVarMMD    = scan("resM4VarMMD")
  resVarMMDL   = scan("resM4VarMMDL")
  resVarMST    = scan("resM4VarMST")
  resVarPOSET  = scan("resM4VarPOSET")

  legend = c( "Compression","Kernels Linear", "Kernels","MST",    "POSET")
  color =      c( "blue",       "red",            "green",  "violet", "cyan" )
  ave = list()
  mix = list()
  var = list()

  ave[[1]] = resAveKS     
  ave[[2]] = resAveMMDL   
  ave[[3]] = resAveMMD    
  ave[[4]] = resAveMST    
  ave[[5]] = resAvePOSET  
  mix[[1]] = resMixKS     
  mix[[2]] = resMixMMDL    
  mix[[3]] = resMixMMD   
  mix[[4]] = resMixMST    
  mix[[5]] = resMixPOSET  
  var[[1]] = resVarKS     
  var[[2]] = resVarMMDL    
  var[[3]] = resVarMMD   
  var[[4]] = resVarMST   
  var[[5]] = resVarPOSET


  png(file="AverageChange.png", width=14, height=10,units="in",bg="transparent",res=120)
  gplot3(d,ave,legend,"Average Change",f=color);
  dev.off()

  X11()
  gplot3(d,ave,legend,"Average Change",f=color);


  png(file="VarianceChange.png", width=14, height=10,units="in",bg="transparent",res=120)
  gplot3(d,var,legend,"Variance Change",f=color);
  dev.off()

  X11()
  gplot3(d,var,legend,"Variance Change",f=color);

  
  png(file="MixingChange.png", width=14, height=10,units="in",bg="transparent",res=120)
  gplot3(d,mix,legend,"Mixing Change",f=color);
  dev.off()

  X11()
  gplot3(d,mix,legend,"Mixing Change",f=color);

}



reproduceMMD <- function() {
  
  PKER[4] = 1                                                                                                                                                                                                                              
  resMixMMD <- M3("Kernels",250,d,createMixing,100)                                                                                                                                                                                        
  write(resMixMMD,file="resMixMMD")                                                                                                                                                                                                        
  resAveMMD  <- M3("Kernels",250,d,createM,100)                                                                                                                                                                                            
  write(resAveMMD,file="resAveMMD")                                                                                                                                                                                                        
  resVarMMD   <- M3("Kernels",250,d,createV,100)                                                                                                                                                                                           
  write(resVarMMD,file="resVarMMD")                                                                                                                                                                                                        
}                                                                                                                                                                                                                                          
reproduceMMDL <- function() {                                                                                                                                                                                                              
  PKER[4] = 0                                                                                                                                                                                                                              
  resMixMMDL <- M3("Kernels",250,d,createMixing,100)                                                                                                                                                                                       
  write(resMixMMDL,file="resMixMMDL")                                                                                                                                                                                                      
  resAveMMDL <- M3("Kernels",250,d,createM,100)                                                                                                                                                                                            
  write(resAveMMDL,file="resAveMMDL")                                                                                                                                                                                                      
  resVarMMDL  <- M3("Kernels",250,d,createV,100)                                                                                                                                                                                           
  write(resVarMMDL,file="resVarMMDL")                                                                                                                                                                                                      
}                                                                                                                                                                                                                                          
reproduceMar <- function() {                                                                                                                                                                                                               
  resMixMar <- M3("Martingale",250,d,createMixing,100)                                                                                                                                                                                     
  write(resMixMar,file="resMixMar")                                                                                                                                                                                                        
  resAveMar  <- M3("Martingale",250,d,createM,100)                                                                                                                                                                                         
  write(resAveMar,file="resAveMar")                                                                                                                                                                                                        
  resVarMar   <- M3("Martingale",250,d,createV,100)                                                                                                                                                                                        
  write(resVarMar,file="resVarMar")                                                                                                                                                                                                        
}                                                                                                                                                                                                                                          
reproduceKS <- function() {                                                                                                                                                                                                                
  resMixKS <- M3("Compression",250,d,createMixing,100)                                                                                                                                                                                     
  write(resMixKS,file="resMixKS")                                                                                                                                                                                                          
  resAveKS   <- M3("Compression",250,d,createM,100)                                                                                                                                                                                        
  write(resAveKS,file="resAveKS")                                                                                                                                                                                                          
  resVarKS    <- M3("Compression",250,d,createV,100)                                                                                                                                                                                       
  write(resVarKS,file="resVarKS")                                                                                                                                                                                                          
}                                                                                                                                                                                                                                          
reproducePOSET <- function() {                                                                                                                                                                                                             
  PNON[5] =1                                                                                                                                                                                                                               
  resMixPOSET <- M3("Non-Parametric",250,d,createMixing,100)                                                                                                                                                                               
  write(resMixPOSET,file="resMixPOSET")                                                                                                                                                                                                    
  resAvePOSET <- M3("Non-Parametric",250,d,createM,100)                                                                                                                                                                                    
  write(resAvePOSET,file="resAvePOSET")                                                                                                                                                                                                    
  resVarPOSET <- M3("Non-Parametric",250,d,createV,100)                                                                                                                                                                                    
  write(resVarPOSET,file="resVarPOSET")                                                                                                                                                                                                    
}



M4 <- function(M,N,D,create,times) {
  t = 1:length(D)
  i = 1;
  for ( d in D) {
    print(c("dim", d))
    t[i] = SmolatestXMP(d,times,create,M,N)
    i = i +1
  }
  
                                        #plot(D,t)
  t
}  

SmolatestXMP<- function(d,times,create,M,N) {
  if (M=="Kernels")  {
    PKER[1] = N
    PKER[2] = N
    P = PKER
  } else if (M=="Non-Parametric"){
    PNON[1] = N
    PNON[2] = N
    P = PNON
  } else if (M == "Martingale")  {
    PMAR[5] = N
    P = PMAR
  } else if (M == "Compression")  {
    PKS[1] = N
    PKS[2] = N
    P = PKS
  } else  { }
  h = (1:times)*0
  for (i in 1:times) {
    state = ""
    t = create(d,21*N,N)
    r = scalarFR(t,P,methodCode(M),state)
    
    visualize3(t, r,P, lg = (M =="Martingale"),t = M,INT = (1:21)*N)
    print(c("x1", 2*N, "len ",  length(r[[1]])))
    if (M== "Martingale") {
      first = sort(r[[2]]>0.95)
    }
    else {
      first = sort(r[[3]]>0.95)
    }
    
    if (length(first)==0 || is.na(first)) {
      h[i] = 0
    }
          
    else {
      print(c("flag ",sum(first), "x1", 2*N, "len ",  21))
      h[i]  = sum(first)/21
    }
  }
  h = sort(h)
  print(h)
  print(c("median", median(h)))
  sum(h)/length(h)
} 

  
reproduceMST <- function() {
  PNON[5] =0
  resMixMST <- M3("Non-Parametric",250,d,createMixing,100)
  write(resMixMST,file="resMixMST")
  resAveMST  <- M3("Non-Parametric",250,d,createM,100)
  write(resAveMST,file="resAveMST")
  resVarMST   <- M3("Non-Parametric",250,d,createV,100)
  write(resVarMST,file="resVarMST")
}  


reproduceM4MMD <- function() {
  PKER[4] = 1
  resM4MixMMD <- M4("Kernels",250,d,createMixing,100)
  write(resM4MixMMD,file="resM4MixMMD")
  resM4AveMMD <- M4("Kernels",250,d,createM,100)
  write(resM4AveMMD,file="resM4AveMMD")
  resM4VarMMD <- M4("Kernels",250,d,createV,100)
  write(resM4VarMMD,file="resM4VarMMD")
}
reproduceM4MMDL <- function() {
  PKER[4] = 0
  resM4MixMMDL <- M4("Kernels",250,d,createMixing,100)
  write(resM4MixMMDL,file="resM4MixMMDL")
  resM4AveMMDL <- M4("Kernels",250,d,createM,100)
  write(resM4AveMMDL,file="resM4AveMMDL")
  resM4VarMMDL <- M4("Kernels",250,d,createV,100)
  write(resM4VarMMDL,file="resM4VarMMDL")
}
reproduceM4Mar <-  function() {
  resM4MixMar <- M4("Martingale",250,d,createMixing,100)
  write(resM4MixMar,file="resM4MixMar")
  resM4AveMar <-M4("Martingale",250,d,createM,100)
  write(resM4AveMar,file="resM4AveMar")
  resM4VarMar <-  M4("Martingale",250,d,createV,100)
  write(resM4VarMar,file="resM4VarMar")
}
reproduceM4KS <- function() {
  resM4MixKS <- M4("CompresM4sion",250,d,createMixing,100)
  write(resM4MixKS,file="resM4MixKS")
  resM4AveKS <-M4("CompresM4sion",250,d,createM,100)
  write(resM4AveKS,file="resM4AveKS")
  resM4VarKS <-M4("CompresM4sion",250,d,createV,100)
  write(resM4VarKS,file="resM4VarKS")
}
reproduceM4POSET <- function()  {
  PNON[5] =1
  resM4MixPOSET <-  M4("Non-Parametric",250,d,createMixing,100)
  write(resM4MixPOSET,file="resM4MixPOSET")
  resM4AvePOSET <-  M4("Non-Parametric",250,d,createM,100)
  write(resM4AvePOSET,file="resM4AvePOSET")
  resM4VarPOSET <-  M4("Non-Parametric",250,d,createV,100)
  write(resM4VarPOSET,file="resM4VarPOSET")
}
reproduceM4MST <- function() {
  PNON[5] =0
  resM4MixMST <-  M4("Non-Parametric",250,d,createMixing,100)
  write(resM4MixMST,file="resM4MixMST")
  resM4AveMST <-  M4("Non-Parametric",250,d,createM,100)
  write(resM4AveMST,file="resM4AveMST")
  resM4VarMST <-  M4("Non-Parametric",250,d,createV,100)
  write(resM4VarMST,file="resM4VarMST")
}




spam = c("../DATA/spam2010-03-0.score.txt","../DATA/spam2010-03-1.score.txt","../DATA/spam2010-03-2.score.txt","../DATA/spam2010-06-0.score.txt","../DATA/spam2010-10-0.score.txt")
spamcolor = c("red", "blue", "yellow", "black", "green")

readspam <- function(l)  {

  m = list()
  M = 0
  mi = 10000
  for (n in 1:length(l)) {
    print (l[n])
    m[[n]]  =  read.csv(l[n],header=FALSE,sep="\t")
    print(dim(m[[n]]))
    M = max(m[[n]][,2],M)
    mi = min(m[[n]][,2],mi)
    print (c(M,mi))
  }

  list(M,mi, m)

}

pdfplotspam <- function(M,m,L,C,E) {
  
  x = L[[1]][,1]
  y = (1:length(x) )*0
  y[1] = log2(M+1)
  y[2] = log2(m+1)
  #y[1] = 1
  #y[2] = 0
  
    
  plot(x,y,type="n",xlab="spamscore",ylab="log2(counts +1)")
  for (l in 1:length(L)) {
    
    inv = order(L[[l]][,2])
    logt = sum(log2(L[[l]][inv,2]+1))
    t    = sum(L[[l]][,2])
    print (c("V",t,logt))
    
    T = log2(L[[l]][,2]+1)
    x =L[[l]][,1]
    print( c("X", max(x),min(x)))
    lines(x,T, type="b",col=C[l])
  }

  legend(x="topright",legend=E,col=C,lty=5,lwd=2, bty="b")

  abline(v=50)


}

cdfplotspam <- function(M,m,L,C,E) {
  
  x = L[[1]][,1]
  y = (1:length(x) )*0
  y[1] = 0
  y[2] = 1

  plot(x,y,type="n",xlab="spamscore",ylab="log2(counts +1)")
  for (l in 1:length(L)) {
    t = sum(log2(L[[l]][,2]+1))
    T = log2(L[[l]][,2]+1)/t
    run = 0
    for (k in 1:length(T)) {
      run = run + T[k]
      T[k] = run
      
    }
    lines(L[[l]][,1],T, type="b",col=C[l])
  }

  legend(x="bottomright",legend=E,col=C,lty=5,lwd=2, bty="b")

  abline(v=50)


}


compareAll <- function(L) {
  Res = list()
  count = 1;
  for (i in 1:(length(L)-1)) {
    for (j in (i+1):length(L))  {
      
      res= CompareHistogramNumeric(L[[i]][,1],L[[i]][,2],
                              L[[j]][,1],L[[j]][,2],0.2)
      print(c(i,j,res))
      Res[[count]] = c(i,j,res)
      count = count +1;
    }


  }

  Res

}


