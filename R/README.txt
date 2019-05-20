# how to build the R interface 
cd ../
make clean

# R needs -DRDEF 
cd R/CDL/src
make R

cd ../../
R CMD build CDL
sudo R CMD INSTALL CDL_0.1.tar.gz


R
library(CDL)
source("R/CDL.R")
Smolatest(10,10)
