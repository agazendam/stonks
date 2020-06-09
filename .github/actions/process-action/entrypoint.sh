#!/bin/sh -l

apt-get update
apt-get install -y -qq r-cran-rquantlib

Rscript process.R $TIINGO_API_KEY