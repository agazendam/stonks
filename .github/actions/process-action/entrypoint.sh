#!/bin/sh -l

apt-get update

#apt-get install -y --no-install-recommends r-cran-rcpp r-cran-zoo libquantlib0-dev ccache
#install.r tinytest
#mkdir ~/.R
#echo _R_CHECK_FORCE_SUGGESTS_=FALSE > ~/.R/check.Renviron
#echo CC=ccache gcc > ~/.R/Makevars
#echo CXX=ccache g++ >> ~/.R/Makevars
#echo CXX11=ccache g++ >> ~/.R/Makevars
#echo CXX14=ccache g++ >> ~/.R/Makevars

#apt-get install -y -qq r-cran-rquantlib

Rscript process.R $TIINGO_API_KEY