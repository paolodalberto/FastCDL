/******************************************************
  Copyright 2019 Paolo D'Alberto.

  This file is part of the Fast Change Detection Library
  (Multidimensional) FastCDL

  FastCDL is free software: you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  FastCDL is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with FastCDL.  If not, see <http://www.gnu.org/licenses/>.

*/
#define KERNELS_METHODS_MODULE 

#include<kernels.h>


static int debug = 0;


/* This is taken from the code by Malte Rash  */

/* 

  %% LINEAR MMD

  if any(strcmp(stattypes,'linMMD'))
    tic;


    %take only one random permutation
    %[xinds, yinds]= subbootstrap(posidx,negidx,1,0);

    if GLOBAL_K && strcmp(kern,'thumb')
      mmdlin = submmdlin(X,posidx,negidx,kern,alpha,0); %not global for run_times
    else
      mmdlin = submmdlin(X,posidx,negidx,kern,alpha,GLOBAL_K); 
    end
    
    if GLOBAL_K && ~strcmp(kern,'thumb')
      error(sprintf(['I cannot perform bootstrapping for linear MMD, if only ' ...
             'global kernel diags are given.\n Please implement the bound yourself ' ...
              '(e.g. as wrapper for kmd.m) or use spider as input choice.']));
    else
      %appearently the old Gaussian is still valid according to Arthur

      if MMDLINBOOTIF
        %bootstrap the bound
        
        for i = 1:NTIMES
          [xinds, yinds]= subbootstrap(posidx,negidx,FRAC,WITHREPLACEMENT);
          bootmmdlin(i) = submmdlin(X,xinds,yinds,kern,alpha,GLOBAL_K);
        end
        val = sort(cat(1,bootmmdlin.val),'descend');
        aind = floor(alpha*NTIMES); 
        
        %take threshold in between aind and the next smaller value:
        if aind
          bound = sum(val([aind,aind+1]))/2;
        else
          bound = val(1);
        end
        
        if SAVE_ALL
          mmdlin.boot_info.WITHREPLACEMENT = WITHREPLACEMENT;
          mmdlin.boot_info.FRAC = FRAC;
          mmdlin.boot_info.bootval = val;
          mmdlin.oldbound = mmdlin.bound;
          mmdlin.oldH = mmdlin.H;
        end
        mmdlin.bound = bound;
        mmdlin.H = mmdlin.val>mmdlin.bound;

      end
    
    end
    out.mmdlin = mmdlin;
    out.mmdlin.t_total = toc;
  
    
    
  end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
function info = submmdlin(X,posidx,negidx,kern,alpha,GLOBAL_K)
  %implements the linear time MMD3. 
    
    M = min(length(posidx),length(negidx));

    %two subsets
    m2 = floor(M/2);
    rp = randperm(length(posidx));
    rn = randperm(length(negidx));
    
    if GLOBAL_K
      global Kxx1 Kyy1 Kxy2
    else
      x1 = X(posidx(rp(1:m2)),:);
      x2 = X(posidx(rp(m2+1:2*m2)),:); 

      y1 = X(negidx(rn(1:m2)),:);
      y2 = X(negidx(rn(m2+1:2*m2)),:);  

      x =  X(posidx(rp(1:2*m2)),:);
      y =  X(negidx(rn(1:2*m2)),:);
      
      Kxx1 = subCalcKernDiag(x1,x2,kern);
      Kyy1 = subCalcKernDiag(y1,y2,kern);
      Kxy2 = subCalcKernDiag(x,y,kern);
    end
    
    if strcmp(kern,'thumb')
      %get kernels size via medium distance
      Ktmp = [Kxx1;Kyy1;Kxy2];
      mdist = median(Ktmp(Ktmp~=0));

      sigma = sqrt(mdist/2); %devided by two is actually better
      if sigma ==0
        sigma =1;
      end
    
      %apply RBF
      Kxx1 = exp(-1/2/sigma^2 * Kxx1);
      Kyy1 = exp(-1/2/sigma^2 * Kyy1);
      Kxy2 = exp(-1/2/sigma^2 * Kxy2);

      kern = sigma;
    end
    
    %calculate linear MMD
    h = Kxx1 + Kyy1 - Kxy2(1:m2) - Kxy2(m2+1:2*m2); 
    
    
    mmdlin = sum(h)/m2;
    
    siglin = sqrt(sum((h-mmdlin).^2)/(m2-1));
    
    %test: again one-sided
    Dlin =  siglin/sqrt(m2)*erfinv(1-alpha*2)*sqrt(2); %stderror
  
    Hlin = mmdlin > Dlin;
    
    
    %output
    info.val = mmdlin;
    info.bound = Dlin;
    info.p = NaN;
    info.H = Hlin;
    if isnumeric(kern)
      info.rbfsigma = kern;
    end
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
function Kxy2 = subCalcKernDiag(X,Y,kern);

%calculates diagonal of kernel matrix only
    

  if strcmp(kern,'thumb') || isscalar(kern) || strcmp(kern.ker,'rbf')
    %for rbf explicitly (much-much faster)
    
    norx = sum(X.*X,2);
    nory = sum(Y.*Y,2);

    Kxy2 = -2*sum(X.*Y,2) + norx + nory;
    

    
    if ~strcmp(kern,'thumb')
      if ~isscalar(kern)
        kerparam = kern.kerparam;
      else
        kerparam = kern;
      end
      Kxy2 = exp(-1/2/kerparam^2 * Kxy2);
    end
    
  else
    %loop
    m2 = size(X,1);
    Kxy2 = zeros(m2,1);
    dx =  data(X);
    dy =  data(Y);
    
    for i = 1:m2
      %this loop takes probably longer than calculate all combinations...
      Kxy2(i) = calc(kern,dx,dy,i,i);
    end
  end
  

*/


/* example of scalarProduct */


// #define COL 7
// #define PBRAKETS { 0.95, 0.96, 0.97, 0.98, 0.99, 0.999, 0.9999 }

extern Mat PVAL[COL];

// sqrt(2)*erfinv(1-2*alpha) where alpha is 1- pval
static double ERFINV_SQRT2[COL]= { 
  1.1631* M_SQRT2,    
  1.2379* M_SQRT2,    
  1.3299* M_SQRT2,    
  1.4522* M_SQRT2,    
  1.6450* M_SQRT2,    
  2.1851* M_SQRT2,    
  2.6297* M_SQRT2
};


// instance of ScalarProduct
double innerProduct(Mat *x,Mat *y, int n) { 
  double s,s0,s1,s2,s3;
  int i;
  
  if (n<4) {
    s = (double)x[0]*y[0];
    for (i=1;i<n;i++) { 
      s+=(double)x[i]*y[i];
    }
    return s;

  } else {
    s=0;
    s0 = (double)x[0]*y[0];
    s1 = (double)x[1]*y[1];
    s2 = (double)x[2]*y[2];
    s3 = (double)x[3]*y[3];
    for (i=4;i<n-8;i+=4) {
      s += s0+s1+s2+s3;
      s0 = (double)x[i]  *y[i]  ;
      s1 = (double)x[i+1]*y[i+1];
      s2 = (double)x[i+2]*y[i+2];
      s3 = (double)x[i+3]*y[i+3];
    }
    s += s0+s1+s2+s3;
    for (;i<n;i++) {
      s+=(double)x[i]*y[i];
    }
    
    return s;
  }
  
}

double squaredNorm(SinglePoint *a,ScalarProduct ip) { 
  

  return ip(a->y,a->y,a->dimensions);

}
double squaredNormAminusB(SinglePoint *a,SinglePoint *b,ScalarProduct ip) { 
  
  double v = ip(a->y,a->y,a->dimensions)+ip(b->y,b->y,b->dimensions) 
    -2*  ip(a->y,b->y,MIN(a->dimensions,b->dimensions));
					    
  return v;

}


// instance of KernelFunction

//inline 
double rbfKernel(SinglePoint *a, 
		 SinglePoint *b, 
		 double *sigmasquare, 
		 int lp,
		 ScalarProduct ip
		 ) { 
  SinglePoint *t;
  double v; 
  double s = *sigmasquare;
  
  
  ALLOCATE_SP(t,MIN(a->dimensions,b->dimensions));
  SP_ADD(*t,=,*a,-,*b); 
  v = ip(t->y,t->y,t->dimensions);
  FREE_SP(t);
  
  return exp(-v/(2*s));
  
}


//inline 
double gaussianKernel(SinglePoint *a, 
		      SinglePoint *b, 
		      double *sigmasquare, 
		      int lp,
		      ScalarProduct ip
		      ) { 
  SinglePoint *t;
  double v; 
  double s = *sigmasquare;

  return rbfKernel(a,b,sigmasquare,lp,ip)/(2*pow(M_PI,MIN(a->dimensions,b->dimensions)/2)*sqrt(s));
  
}

//inline 
double linearKernel(SinglePoint *a, 
		    SinglePoint *b, 
		    double *s, 
		    int lp,
		    ScalarProduct ip
		    ) { 
  
  return ip(a->y,b->y,MIN(a->dimensions,b->dimensions));
  
}
//inline 
double polyKernel(SinglePoint *a, 
		  SinglePoint *b, 
		  double *d, 
		  int lp,
		  ScalarProduct ip
		  ) { 
  
  return pow(ip(a->y,b->y,MIN(a->dimensions,b->dimensions))+1,*d);
  
}

// instance of KernelFunction




// instance of  KernelFunctionP    
//inline 
double templateKernelP(SinglePoint *a, 
		       SinglePoint *b, 
		       ScalarProduct ip,
		       FeatureSpaceFunction ph,
		       double *sigmasquare,  /* we do not really care about these in general */
		       int lp                 /* but other methods my be parametric */
		       ) { 
  SinglePoint *t, *v;
  double res;
  
  // change of space 
  t = ph(a);
  v = ph(b);
  
  // inner product 
  res = ip(t->y,v->y,MIN(t->dimensions,v->dimensions));
  
  
  FREE_SP(t);
  FREE_SP(v);
  
  
  return res;
  
}



double hsimplified_gaussian(Hdata *hdata) { 
  
  double result= gaussianKernel(hdata->xi,hdata->yi,hdata->sigmasquare,hdata->lp,hdata->ip);
  
  result  +=   gaussianKernel(hdata->xj,hdata->yj,hdata->sigmasquare,hdata->lp,hdata->ip);
  result  -= 2*gaussianKernel(hdata->xi,hdata->yj,hdata->sigmasquare,hdata->lp,hdata->ip);
  
  return result;
}

double hsimplified_general(Hdata *hdata) { 
  
  double result= hdata->k(hdata->xi,hdata->yi,hdata->sigmasquare,hdata->lp,hdata->ip);
  result  += hdata->k(hdata->xj,hdata->yj,hdata->sigmasquare,hdata->lp,hdata->ip);
  result  -= hdata->k(hdata->xi,hdata->yj,hdata->sigmasquare,hdata->lp,hdata->ip);
  result  -= hdata->k(hdata->xj,hdata->yi,hdata->sigmasquare,hdata->lp,hdata->ip);
  
  return result;
}


//inline
double h_general(Hdata *hdata) { 
  
  SinglePoint *xi,*yi,*xj,*yj;
  SinglePoint *l, *r;
  double res;
  
  int NI = MIN(xi->dimensions,yi->dimensions);
  int NJ = MIN(xj->dimensions,yj->dimensions);
    
  xi =  hdata->ph(hdata->xi);
  xj =  hdata->ph(hdata->xj);
  yi =  hdata->ph(hdata->yi);
  yj =  hdata->ph(hdata->yj);

  ALLOCATE_SP(l,NI);
  ALLOCATE_SP(r,NJ);
  
  SP_ADD(*l, = ,*xi,-,*yi);
  SP_ADD(*r, = ,*xj,-,*yj);
  
  res = hdata->ip(l->y,r->y,MIN(NI,NJ));
  
  FREE_SP(xi);
  FREE_SP(yi);
  FREE_SP(xj);
  FREE_SP(yj);
  FREE_SP(l);
  FREE_SP(r);

  return res;
}






Statistic 
MMD_u_g(TimeSeries *r, TimeSeries *w, 
	KernelFunctionH h,
	KernelFunction  k,
	ScalarProduct   sp
	) { 
  
  int i,j;
  double result=0;
  double sigma = 0;
  Statistic res = {1, 1 };
  Hdata hd      = {0,0,0,0, 
		   &sigma,1,
		   k, 0, sp} ;
  double mmd2   = 0;
  int N    = (r->length)*(w->length-1);
  int M    = (r->length)*(w->length);
  double *hi  = (double*) malloc(4*M*sizeof(double));
  double *kxx = (double*) malloc(4*M*sizeof(double));
  double *kyy = kxx + M;
  double *kyx = kyy + M;
  double *kxy = kyx + M;
  
  assert(hi && kxx);


  if (k == rbfKernel) { // no parameters I will estimate sigma  
    int counter = 0;
    double sigma1;
    int SIZE =  w->length;
    for (i=0; i< r->length;i++) { 
      for (j=0; j< w->length;j++) { 
	if (i!=j) { 
	  kxx[i*SIZE +j] = squaredNormAminusB(r->data+ i,r->data +j,sp);
	  kyy[i*SIZE +j] = squaredNormAminusB(w->data+ i,w->data +j,sp);
	  kxy[i*SIZE +j] = squaredNormAminusB(r->data+ i,w->data +j,sp);
	  kyx[i*SIZE +j] = squaredNormAminusB(w->data+ i,r->data +j,sp);
	  if (kxx[i*SIZE +j]) hi[counter++]   =  kxx[i*SIZE +j];
	  if (kyy[i*SIZE +j]) hi[counter++]   =  kyy[i*SIZE +j];
	  if (kxy[i*SIZE +j]) hi[counter++]   =  kxy[i*SIZE +j];
	  if (kyx[i*SIZE +j]) hi[counter++]   =  kyx[i*SIZE +j];
	  
	}
      }
    }
    
    SORT_ARRAY(hi,counter);
    if (debug) { 
      for (i=0; i<counter;i++)
	PRINTF(" hi[i] %e", hi[i]);
      PRINTF("\n");
    }

    if (hi[counter/2] ) 
      sigma1 = hi[counter/2];
    else 
      sigma1 = 1;

    if (debug) { 
      
      PRINTF("Sigma emp%e\n",sigma1);
    }

    for (i=0; i<r->length;i++) {
      result = 0;
      for (j=0;j<w->length;j++) { 
	if (i!=j) { 
	  double t =  
	    exp(-kxx[i*SIZE +j]/sigma1)+
	    exp(-kyy[i*SIZE +j]/sigma1)- 
	    exp(-kxy[i*SIZE +j]/sigma1)- 
	    exp(-kyx[i*SIZE +j]/sigma1);
	  
	  result += t;
	}
      }
      mmd2 += result/N;
      sigma += result*result;
    }

  }
  else {
    for (i=0; i<r->length;i++) {
      result = 0;
      for (j=0;j<w->length;j++) { 
	if ( i!=j) { 
	  
	  hd.xi = r->data + i;      hd.xj = r->data + j;
	  hd.yi = w->data + i;      hd.yj = w->data + j;
	  
	  result += h(&hd);
	}
	
      }
      mmd2 += result/N;
      sigma += result*result;
    }
  }

  if (debug) { 
    PRINTF("Sigma T %e\n",sigma);
  }
  
  sigma = sqrt(fabs((4*sigma/N)/N - 4*mmd2*mmd2/r->length));

  if (debug) { 
    PRINTF("Sigma %e\n",sigma);
  }
  
    
  free(hi);
  free(kxx);

  
  res.val = mmd2;
  mmd2 = fabs(mmd2);
  res.pval = 1;
  
  if (debug) { 
    PRINTF("MMD2  %e sigmaerfinv(1-2alpha) %e erfinv(1-2alpha) %e sigma  %e %d \n",mmd2, sigma*ERFINV_SQRT2[0],ERFINV_SQRT2[0],sigma,0);
  }
  if (mmd2 < sigma*ERFINV_SQRT2[0] || mmd2==0 || sigma==0) {
    res.pval = 0;
    return res;
  }
  
  for (i=1;i<COL;i++) {
    if (debug) { 
      PRINTF("MMD2  %e sigmaerfinv(1-2alpha) %e %d\n",mmd2, sigma*ERFINV_SQRT2[i],i);
    }

    if (mmd2 <= sigma*ERFINV_SQRT2[i])  { 
      res.pval = PVAL[i];
      
      return res;
    }
    
  }
  

  return res;
  
} 




Statistic MMD_l_g(TimeSeries *r, TimeSeries *w, 
		  KernelFunctionH h,
		  KernelFunction  k,
		  ScalarProduct   sp,
		  double *par, 
		  int lpar) { 

  int i,j;
  double mmdlin=0, siglin=0,sum=0;
  double sigma;
  int m2 = MIN(r->length,w->length)/2;
  Statistic res = {1, 1 };
  double *hi =  (double*) malloc(4*m2*sizeof(double));
  double *kxx = (double*) malloc(4*m2*sizeof(double));
  double *kyy = kxx +m2;
  double *kyx = kyy +m2;
  double *kxy = kyx +m2;
  Hdata hd      = {0,0,0,0, 
		   &sigma,1,
		   k, 0, sp} ;
    
  assert(hi && kxx);
      
  // initialize the 





  
  if (!lpar && k == rbfKernel) { // no parameters I will estimate sigma  
    int counter=0;
    for (i=0; i<m2;i++) { 
      
      kxx[i] = squaredNormAminusB(r->data+ 2*i,r->data +2*i+1,sp);
      kyy[i] = squaredNormAminusB(w->data+ 2*i,w->data +2*i+1,sp);
      kxy[i] = squaredNormAminusB(r->data+ 2*i,w->data +2*i+1,sp);
      kyx[i] = squaredNormAminusB(w->data+ 2*i,r->data +2*i+1,sp);
      
      if (kxx[i]) hi[counter++]   =  kxx[i];
      if (kyy[i]) hi[counter++]   =  kyy[i];
      if (kxy[i]) hi[counter++]   =  kxy[i];
      if (kyx[i]) hi[counter++]   =  kyx[i];

    }
    
    SORT_ARRAY(hi,counter);
    if (debug) {  
      for (i=0; i<counter;i++)
	PRINTF(" hi[i] %e", hi[i]);
      PRINTF("\n");
    }
    
    if (i>0 && hi[i/2] ) 
      sigma = hi[i/2];
    else 
      sigma = 1;

    if (debug) { 
      
      PRINTF("Sigma %e\n",sigma);
    }

    for (i=0; i<m2;i++) {
      double t =  
	exp(-kxx[i]/sigma)+
	exp(-kyy[i]/sigma)- 
	exp(-kxy[i]/sigma)- 
	exp(-kyx[i]/sigma);
      
      hi[i] = t;
      sum += hi[i];
    }
  }
  else { 
    
    
    hd.k = k;
    hd.ip = sp;
    if (!lpar) { 
      hd.sigmasquare = kxx;
      hd.sigmasquare[0]= 1;
      hd.lp = 1;
      
    } else { 
      hd.sigmasquare = par;
      hd.lp = lpar;
    }

    
    for (i=0; i<m2;i++) { 
      hd.xi = r->data + 2*i;      hd.xj = r->data + 2*i+1;
      hd.yi = w->data + 2*i;      hd.yj = w->data + 2*i+1;
      
      hi[i]= h(&hd);
      sum += hi[i];
    }
  }
  

  mmdlin = sum /(double)m2;
  for (i=0; i<m2;i++) { 
    double t = (hi[i]-mmdlin);
    siglin += t*t;
  }
    
  free(hi);
  free(kxx);
  res.val = mmdlin;
  
  siglin = sqrt(siglin/(m2*(m2-1)));
  
  if (mmdlin <= siglin*ERFINV_SQRT2[0] || mmdlin==0 || siglin==0) {
    res.pval = 0;
    return res;
  }
  
  for (i=1;i<COL;i++) { 
    if (mmdlin <= siglin*ERFINV_SQRT2[i])  { 
      res.pval = PVAL[i];
      
      return res;
    }
    
  }
  
  return res;

} 









int kernelMethods(TimeSeries *stream,
		  NonParametricState **state,
		  TimeSeries **val,  TimeSeries **pval,
		  int N, int M, int KM, int KK) { 

  NonParametricState *s = (state)?(*state):0;
  TimeSeries *v = *val;
  TimeSeries *pv = *pval;


  if (!state)  { 
   

    if (debug) { 
      PRINTF("State pointer is null ");
    }
    return 0;

  }

  if (!s || 
      N != s->r->ts->max || 
      M != s->w->ts->max) {
    if (debug) { 
      PRINTF("State pointer must be update N %d M %d  This is the old state: \n",
	     N,M);
      PRINTNONPSTATE(s,0);
    }
    FREE_NONP_STATE(s);
  } 

  UPDATESTATE(s,stream,N,M);
  *state = s;
  if (debug) { 
    PRINTF("New state: \n");
    PRINTNONPSTATE(s,0);
  }


  // Running window is larger than 0
  if (s->w->ts->length>0 ) { 
    double sigma = 1;
    Statistic result;
    KernelFunctionH  h=hsimplified_general;
    KernelFunction k;
    int lr = s->r->ts->length;
    int lw = s->w->ts->length;
    int min = MIN(lr,lw); 
    
    


    // choice of kernel functions
    switch (KK) { 

    case KERLINEAR:
      k = linearKernel;
      break;
      
    case KERNPOLY:
      k = polyKernel;
      break;

    case KERRBF:
      h = hsimplified_gaussian;
      k = rbfKernel;
      break;
    case KERGAUSSIAN:
      h = hsimplified_gaussian;
      k = gaussianKernel;
      break;
    default:
      break;
    }
    
    s->r->ts->length = s->w->ts->length = min;
    // Choice of the kernel method MMD
    switch (KM) { 

      case KLINEAR:
	
	result = MMD_l_g(s->r->ts, s->w->ts, 
			 h,k,innerProduct,
			 0, 0);
	
	break;
    case KMMD1:

      result = MMD_u_g(s->r->ts, s->w->ts, 
		       h,k,innerProduct);
      
      break;
    default:
      break;
    }
    
    s->r->ts->length= lr;
    s->w->ts->length = lw;


    ALLOCATETS(v,1,1);
    ALLOCATETS(pv,1,1);
    
    *val = v;
    *pval= pv;

    v->data[0].y[0]  = result.val;
    pv->data[0].y[0] = result.pval;

    v->length= pv->length= 1;
    v->data[0].x = pv->data[0].x  = s->w->ts->data[0].x;

    return 1;
  } else { 
    if(debug) { 
      PRINTF("Nothing to do \n");
      
    }
    return 0;
  }
  

}
