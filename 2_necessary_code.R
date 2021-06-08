library(RBDmap)
library(RBDmapHeLa)

basedir = getBaseDir()
resultdir = file.path( basedir, "inputdata")
dir.create(resultdir, recursive = TRUE,showWarnings=FALSE)

data("ProtFeatures",package="RBDmapHeLa")
data("Index",package="RBDmap")

data("enigmRBP",package="RBDmap")


load(file.path('inputdata','enigmRBP.rda'))
load(file.path('inputdata','ProtFeatures.rda'))
RBDmap::

##########

library(RBDmap)
library(Biostrings)
library(grid)
library(hwriter)
basedir = getBaseDir()


resultdir = file.path( basedir, "result", "Simulation")
dir.create(resultdir, recursive = TRUE,showWarnings=FALSE)

data("ProtFeatures", package="RBDmapHeLa")
data("PeptideIonCounts", package="mRNAinteractomeHeLa")
data("MSMSIdentifications", package="mRNAinteractomeHeLa")
data("ids", package="mRNAinteractomeHeLa")


#####
ProtSeq = ProtFeatures$ProtSeq
n = nchar(ProtSeq)
names(n) = names(ProtSeq)
G2P = tapply(n, ProtFeatures$GeneName, function(x){names(x)[which.max(x)]})
PID = G2P[ids$mRNAinteractome]
PID = PID[!is.na(PID)]
ProtSeq = ProtSeq[PID]

##'
##'

Peps = c()

for (e in seq_along(MSMSIdentifications)){
  for (i in seq_along(MSMSIdentifications[[e]])){
    PSM = MSMSIdentifications[[e]][[i]]
    I = which((G2P[PSM$PSM$group] %in% PID) & (PSM$PSM$qvalue <= 0.01))
    if (length(I) > 0){Peps = rbind(Peps, PSM$PSM[I,c("peptide","group")])
    }
  }
}

Peps = unique(Peps)



#################'
#################'
BiocManager::install("Bioconductor")


source('http://www.bioconductor.org/biocLite.R')
library(Bioconductor)
# biocLite('BiocStyle','knitr','RBDmap','Biobase','Biostrings','RColorBrewer','ape','genefilter','geneplotter','gplots','grid','hwriter','limma','sda','biomaRt','gridSVG')




#