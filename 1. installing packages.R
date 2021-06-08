## implementing  a MS-pulldown Pipeline for OLGA
#
#' Date = 2021.05.25
#' Version = 1.00.1


#' > sessionInfo()  # This code was developed with the following R version and Platform
#          R version 4.0.5 (2021-03-31)
#          Platform: x86_64-w64-mingw32/x64 (64-bit)
#          Running under: Windows 10 x64 (build 19042)


#  ----------------------------------------------
# 1. install recoomended packages
# to uncomment block, highlight it and press " STRG + Umschalt + C "

install.packages("VennDiagram")
install.packages("BiocManager")
BiocManager::install("Biobase")
BiocManager::install("limma")


install.packages("XML")
install.packages("RBDmap")             # if not working, try: BiocManager::install("RBDmap")
install.packages("hwriter")
install.packages("RColorBrewer")
install.packages("Biostrings")       # if not working, try: BiocManager::install("Biostrings")
install.packages("gridSVG")
install.packages("pheatmap")
#  in case package installation does not work, try to install directly from github
install.packages("ggrepel")  # development version of github : devtools::install_github("slowkow/ggrepel")
install.packages("ggplot2")  # try: devtools::install_github("tidyverse/ggplot2")
BiocManager::install("biomaRt")
BiocManager::install("geneplotter")


#  ----------------------------------------------
# 2. loading recomended packages 
library(ggrepel)   
library(VennDiagram)
library(Biobase)  
library(limma)   
library(ggplot2)
library(XML)
library(RBDmap)
library(hwriter)
library(RColorBrewer)
library(Biostrings)
library(gridSVG)
library(pheatmap)
library(hwriter)

library(geneplotter)
library(biomaRt)
# You might be struggling loading all packages, 

install.packages("devtools")
devtools::install_github("hadley/tibble")


install.packages("cachem")

if (!require("remotes")) install.packages("remotes")
remotes::install_github("r-lib/cachem")

#   3. start of the Pipeline
#  ----------------------------------------------
#   3.1 loading datasets






#