library(GGIR)
library(bitops)
library(covr)
library(testthat)
library(knitr)
library(rmarkdown)
library(data.table)
library(Rcpp)
library(foreach)
library(doParallel)
library(signal)
library(zoo)
library(matlab)
library(GENEAread)
library(tuneR)
library(unisensR)
library(ineq)
library(stats)
library(utils)

#========================================
#General parameters
#========================================
g.shell.GGIR(#=======================================
             # INPUT NEEDED:
             mode=c(1,2,3,4,5),
             datadir="C:/Users/preot/Desktop/GGIR_demo/TEST",
             outputdir="C:/Users/preot/Desktop/GGIR_demo/OUTPUT",
             f0=1, f1=3,
             #-------------------------------
             # Part 1:
             #-------------------------------
             # Key functions: reading file, auto-calibration, and extracting features
             do.enmo = TRUE,             do.anglez=TRUE,
             chunksize=1,                printsummary=TRUE,
             #-------------------------------
             # Part 2:
             #-------------------------------
             strategy = 2,               ndayswindow=7,
             hrs.del.start = 0,          hrs.del.end = 0,
             maxdur = 9,                 includedaycrit = 16,
             winhr = c(5,10),
             qlevels = c(c(1380/1440),c(1410/1440)),
             qwindow=c(0,24),
             ilevels = c(seq(0,400,by=50),8000),
             mvpathreshold =c(100,120),
             bout.metric = 4,
             closedbout=FALSE,
             #-------------------------------
             # Part 3:
             #-------------------------------
             # Key functions: Sleep detection
             timethreshold= c(5),        anglethreshold=5,
             ignorenonwear = TRUE,
             #-------------------------------
             # Part 4:
             #-------------------------------
             # Key functions: Integrating sleep log (if available) with sleep detection
             # storing day and person specific summaries of sleep

             #-------------------------------
             # Part 5:
             # Key functions: Merging physical activity with sleep analyses
             #-------------------------------
             threshold.lig = c(30), threshold.mod = c(100),  threshold.vig = c(400),
             boutcriter = 0.8,      boutcriter.in = 0.9,     boutcriter.lig = 0.8,
             boutcriter.mvpa = 0.8, boutdur.in = c(1,10,30), boutdur.lig = c(1,10),
             boutdur.mvpa = c(1),   timewindow = c("WW"),
             #-----------------------------------
             # Report generation
             #-------------------------------
             # Key functions: Generating reports based on meta-data
             do.report=c(2,4,5),
             visualreport=TRUE,     dofirstpage = TRUE,
             viewingwindow=1)
