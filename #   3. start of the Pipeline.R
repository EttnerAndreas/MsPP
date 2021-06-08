#   3. start of the Pipeline
#  ----------------------------------------------
#   3.1 loading datasets

library(usethis)
library(readxl)
#####  ------------------------------
#
##    Loading all Datasets ()
ComparisonAB_TAIR_SpurB_only <- read_excel("Data/Olga Rudi_ComparisonAB_TAIR_SpurB_only.xlsx")
# usethis::use_data(ComparisonAB_TAIR_SpurB_only, overwrite = TRUE)

ComparisonAB_TAIR            <- read_excel("Data/Olga Rudi_ComparisonAB_TAIR.xlsx")
# usethis::use_data(ComparisonAB_TAIR, overwrite = TRUE)

ComparisonAB_Uniprot_SpurB_only <- read_excel("Data/Olga Rudi_ComparisonAB_Uniprot_SpurB_only.xlsx")
# usethis::use_data(ComparisonAB_Uniprot_SpurB_only overwrite = TRUE)

ComparisonAB_Uniprot            <- read_excel("Data/Olga Rudi_ComparisonAB_Uniprot_SpurB_only.xlsx")
# usethis::use_data(ComparisonAB_Uniprot, overwrite = TRUE)

Olga_Rudi_Compi_TAIR_SpurA <- read_excel("Data/Olga Rudi_Compi_TAIR_SpurA.xlsx")

Olga_Rudi_Compi_TAIR_SpurB <- read_excel("Data/Olga Rudi_Compi_TAIR_SpurB.xlsx")

Olga_Rudi_Compi_Uniprot_SpurA <- read_excel("Data/Olga Rudi_Compi_Uniprot_SpurA.xlsx")
                                            
Olga_Rudi_Compi_Uniprot_SpurB <- read_excel("Data/Olga Rudi_Compi_Uniprot_SpurB.xlsx")

#'
#'
#'
#
#####  TAIR 
summary(ComparisonAB_TAIR)                # Length = 1463
str(ComparisonAB_TAIR)
View(ComparisonAB_TAIR)
head(ComparisonAB_TAIR)

summary(ComparisonAB_TAIR_SpurB_only)     # Length = 493 

summary(Olga_Rudi_Compi_TAIR_SpurA)       # 1032
summary(Olga_Rudi_Compi_TAIR_SpurB)       # 1334


##### UNIPORT 
summary(ComparisonAB_Uniprot)             # Length = 500
summary(ComparisonAB_Uniprot_SpurB_only)  # Length = 500

summary(Olga_Rudi_Compi_Uniprot_SpurA)    # 922
summary(Olga_Rudi_Compi_Uniprot_SpurB)    # 1212
####








#'
#'
#'
#'
         ## ASTRID BRUCKMANN  ==  - 2810    phone number 
# ok == " ? 
# MK /Dalton = größe 
# pi = Isoelectric point  // wo ist ph neutral geladen
# peptide = einzelfragmente, 
# SC 
#
#
# alt protein = alternative Proteine, == isoformen sind damit gemeint, nicht besonders wichtig,
# tair datenbank oder uniprop datenbank 
# UNiprop sind redundant, einträge machinell anotiert,
# scores + zahl der peptide + seqeuence coverage 


# scores ungecrosslinked vs gecrosslinked methode,  
# summary(Olga_Rudi_Compi_TAIR_SpurA)










## kein Uniprop 

#'
#'
#'
#'

##
#


### ----------  STEP 1 / clean dataset by demoving contaminations and redundant "stuff -----------------------


# In Olga´s MS-data are no contaminations, at least not in the data recieved 
# Olga_Rudi_Compi_TAIR_SpurA
# Olga_Rudi_Compi_TAIR_SpurB
# ComparisonAB_TAIR
# ComparisonAB_TAIR_SpurB_only

TAIR_SpurA = Olga_Rudi_Compi_TAIR_SpurA[,c(1,2,4,5,6,7,8,9,10,11)]
TAIR_SpurB = Olga_Rudi_Compi_TAIR_SpurB[,c(1,2,4,5,6,7,8,9,10,11)]

TAIR_Comp  = ComparisonAB_TAIR
TAIR_SpurB_only_Comp = ComparisonAB_TAIR_SpurB_only



#  ----------------------------------------------

###MapProt2Ensg maps the peptide-mapping proteins to genes#
##and return a list of mapped genes

MapProt2Ensg=function(protein_list){ 
  lapply(protein_list,function(x)
    {y=unname(ProtFeatures$GeneName[x])
    unique(y[!is.na(y)])})
}




ProtIDs_who=mapPeptides(PeptideSet=input_raw_who$sequences,
                        c(ProtFeatures$ProtSeq,SV_seq)
                        ,verbose=FALSE)

ENSGid_who=MapProt2Ensg(ProtIDs_who)
ENSGid_who[grep('SV_', ProtIDs_who)]=
  ProtIDs_who[grep('SV_', ProtIDs_who)]
ENSGid_who[grep('SV_', ProtIDs_who)]=
  lapply(ENSGid_who[grep('SV_', ProtIDs_who)],function(x) x[1])


ProtIDs_ric=mapPeptides(PeptideSet=input_raw_ric$sequences,
                        c(ProtFeatures$ProtSeq,SV_seq)
                        ,verbose=FALSE)


ENSGid_ric=MapProt2Ensg(ProtIDs_ric)
ENSGid_ric[grep('SV_', ProtIDs_ric)]=
  ProtIDs_ric[grep('SV_', ProtIDs_ric)]
ENSGid_ric[grep('SV_', ProtIDs_ric)]=
  lapply(ENSGid_ric[grep('SV_', ProtIDs_ric)],function(x) x[1])





### ----------  STEP 2/ rename datasets and remove unused colums ------------------------------------------

controll <- read_excel("C:/Users/andie/Dropbox/Mein PC (DESKTOP-4TB10SM)/Downloads/Pipeline/TREX componenten.xlsx", 
                       col_names = FALSE)
colnames(controll) <- c("Protein", "ATG", "complex")
View(controll)
str(controll)

##### -------------------------------
#


for ( i in 1: nrow(TAIR_SpurA)){
  for (j in 1: nrow(controll)){
    if (TAIR_SpurA$Accession[i] == controll$ATG[j]){
      print(controll$Protein [j])
      print(i)
    }
  }
}
## ------- ###
#

for ( i in 1: nrow(TAIR_Comp)){
  for (j in 1: nrow(controll)){
    if (TAIR_Comp$Accession[i] == controll$ATG[j]){
      print(controll$Protein [j])
      print(i)
    }
  }
}
## ------- ###
#

for ( i in 1: nrow(TAIR_SpurB)){
  for (j in 1: nrow(controll)){
    if (TAIR_SpurB$Accession[i] == controll$ATG[j]){
      print(controll$Protein [j])
      print(i)
    }
  }
}
## ------- ###
#

for ( i in 1: nrow(TAIR_SpurB_only_Comp)){
  for (j in 1: nrow(controll)){
    if (TAIR_SpurB_only_Comp$Accession[i] == controll$ATG[j]){
      print(controll$Protein [j])
      print(i)
    }
  }
}




##### --------------- VENN DIAGRAM ---------------------
devtools::install_github("yanlinlin82/ggvenn@v0.1.0")
library(ggvenn)
df1 = data.frame(TAIR_SpurB)
df2 = data.frame(TAIR_SpurA)

data1 = match_df(df1, df2, on = "Accession")

nrow(df1)
nrow(df2)
nrow(data1)
data1

str(data1)
summary(data1)

for ( i in 1: nrow(data1)){
  for (j in 1: nrow(controll)){
    if (data1$Accession[i] == controll$ATG[j]){
      print(controll$Protein [j])
      print(i)
    }
  }
}

######### setdiff(a$x, b$y)
data_513 = subset(TAIR_SpurB, !(Accession %in% TAIR_SpurA$Accession))
str(data_513)
nrow(data_513)



for ( i in 1: nrow(data_513)){
  for (j in 1: nrow(controll)){
    if (data_513$Accession[i] == controll$ATG[j]){
      print(controll$Protein [j])
      print(data_513$Scores[i])
      print(controll$ATG[j])
    }
  }
}
####### ----------------------------------------------

data_211 = subset(TAIR_SpurA, !(Accession %in% TAIR_SpurB$Accession))
str(data_211)
nrow(data_211)
nrow(data1)


for ( i in 1: nrow(data_211)){
  for (j in 1: nrow(controll)){
    if (data_211$Accession[i] == controll$ATG[j]){
      print(controll$Protein [j])
      print(data_513$Scores[i])
      print(controll$ATG[j])
    }
  }
}

nrow(data1)
for ( i in 1: nrow(data1)){
  for (j in 1: nrow(controll)){
    if (data1$Accession[i] == controll$ATG[j]){
      print(controll$ATG[j])
      print(controll$Protein [j])
      print(i)
    }
  }
}

#######
####
###
###
#
library(ggvenn)
library(ggVennDiagram)
library(VennDiagram)

#'----- new function 
display_venn <- function(x, ...){
  library(VennDiagram)
  grid.newpage()
  venn_object <- venn.diagram(x, filename = NULL, ...)
  grid.draw(venn_object)
}


list1 = list(
  Tair_A = TAIR_SpurA$Accession, 
  Tair_B = TAIR_SpurB$Accession)

list2 = list(
  Matches_A_B = data1$Accession,
  Tair_A = TAIR_SpurA$Accession, 
  Tair_B = TAIR_SpurB$Accession)


ggVennDiagram(list1, label_alpha = 0)


fig1 = display_venn(   
  list1,
  category.names = c( "non-crosslink", "crosslink"),
  fill = c( "#F5793A", "#0F0280")
)###### A = NO_CROSSLINK // B = CROSSLINK   
## #F5793A   ##F5793A


#      #FFD954  #69491A
fig1 = display_venn(   
  list1,
  category.names = c( "non-crosslink", "crosslink"),
  fill = c( "#F2AB39", "#282B42"),#F8981D    #E12E4B
  lwd = 2,
  #lty = 'blank',
  # Numbers
  cex = 1.3,
  fontface = "italic",
  # Set names
  cat.cex = 1.3,
  cat.default.pos = "text",
  cat.fontface = "bold"
  #cat.default.pos = "outer",
)

?display_venn
View(controll)

#### ---------------------------------------
#'###
list3 = list(
  Matches_A_B = data2$Accession,
  Tair_A = TAIR_SpurA$Accession, 
  Tair_B = TAIR_SpurB$Accession)

ggVennDiagram(list3, label_alpha = 0)


fig2 = display_venn(
  list3,
  category.names = c("Matches_A_B"  , "Tair_A", "Tair_B"),
  fill = c( "#E69F00", "#56B4E9", "#009E73")
)
####### ------------------------------------------
######
#####

ggvenn(
  list1, 
  show_elements = FALSE,
  show_percentage = TRUE,
  digits = 1,
  fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"),
  fill_alpha = 0.5,
  stroke_size = 0.5, set_name_size = 4,
)
#####---##
#####---##

library(ggVennDiagram)
library(VennDiagram)

ggVennDiagram(list2, label_alpha = 0)


display_venn(
  list2,
  category.names = c("Matches_A_B"  , "Tair_A", "Tair_B"),
  fill = c( "#E69F00", "#56B4E9", "#009E73")
)


venn.diagram(list1, filename = "venn-4-dimensions.png")

  
# Helper function to display Venn diagram
display_venn <- function(list1, ...){
  library(VennDiagram)
  grid.newpage()
  venn_object <- venn.diagram(list1, filename = NULL, ...)
  grid.draw(venn_object)
}
display_venn(x)

display_venn(
  x,
  category.names = c("Set 1" , "Set 2 " , "Set 3", "Set 4"),
  fill = c("#999999", "#E69F00", "#56B4E9", "#009E73")
)


#### aus Spur A alle aus B rauswerfen 
### alle die in B sind aber nicht in A 

# venndiagram 
venn.diagram()   

### ----------  STEP 3/ map the MS- Pulldowndata according to the amount of mapped proteins  ---------------
library(grid)
library(RBDmap)
library(Biostrings)
basedir = getBaseDir()
resultdir = file.path( basedir, "result", "Simulation")

dir.create(resultdir, recursive = TRUE,showWarnings=FALSE)
data("ProtFeatures", package="RBDmapHeLa")
data("PeptideIonCounts", package="mRNAinteractomeHeLa")
data("MSMSIdentifications", package="mRNAinteractomeHeLa")
data("ids", package="mRNAinteractomeHeLa")




### ----------  STEP 4/ -----------------------------------
ProtSeq = ProtFeatures$ProtSeq
n = nchar(ProtSeq)
names(n) = names(ProtSeq)
G2P = tapply(n, ProtFeatures$GeneName, function(x){names(x)[which.max(x)]})
PID = G2P[ids$mRNAinteractome]
PID = PID[!is.na(PID)]
ProtSeq = ProtSeq[PID]


#### ------- --
# Load identified peptides (FDR 1%) of proteins in the mRNA interactome.
Peps = c()

for (e in seq_along(MSMSIdentifications)){
  for (i in seq_along(MSMSIdentifications[[e]])){
    PSM = MSMSIdentifications[[e]][[i]]
    I = which((G2P[PSM$PSM$group] %in% PID)& (PSM$PSM$qvalue <= 0.01))
    if (length(I) > 0){
      Peps = rbind(Peps, PSM$PSM[I,c("peptide","group")])
      }
    }
  }

Peps = unique(Peps)

#### ------- --
SP = split(Peps$peptide,G2P[Peps$group])
PepRanges <- list()
for (i in 1:length(ProtSeq)){
  n = names(ProtSeq[i])
  
  
  PepRanges[[i]] = sapply(SP[[n]], function(x){
    g = gregexpr(x, ProtSeq[i])[[1]]
    if (length(g) == 1){
      y = c(g[1],g[1] + attr(g,"match.length") - 1)
    }else{
        y = c(-1,-1)
    }
    return(y)
  })
  PepRanges[[i]] = PepRanges[[i]][,PepRanges[[i]][1,] >= 0,drop=FALSE]
}



RS = lapply(CleavagePattern,function(RP){CleavageSites(ProtSeq, cleavagePattern = RP)})


##     RBDmap analysis example
### ----------  STEP 5/ -----------------------------------
#
getFragments = function(rs, pos){
  fragment = list()
  for (i in 1:length(rs)){
    I = min(which(rs[[i]] > pos[i]))-1
    fragment[[i]] = c(rs[[i]][I], rs[[i]][I+1]-1)
    }
  fragment
  }
pos = ceiling(nchar(ProtSeq) / 2)

fragments = lapply(RS, function(rs){getFragments(rs,pos)})



#
resolution = list()
for (k in 1:length(RS)){
  rs = RS[[k]]
  fr = fragments[[k]]
  resolution[[k]] = nchar(ProtSeq)
  for (i in 1:length(ProtSeq)){
    L = nchar(ProtSeq[[i]])
    pileup = rep(0,L)
    for (j in seq_len(ncol(PepRanges[[i]]))){
      isBindingSite = FALSE
      if ((PepRanges[[i]][1,j] >= fr[[i]][1]) & (PepRanges[[i]][2,j] <= fr[[i]][2])){
        isBindingSite = TRUE
        resolution[[k]][i] = fr[[i]][2] - fr[[i]][1] + 1
        }
      pileup[PepRanges[[i]][1,j]:PepRanges[[i]][2,j]] =
        pileup[PepRanges[[i]][1,j]:PepRanges[[i]][2,j]] + 1
      y = max(pileup[PepRanges[[i]][1,j]:PepRanges[[i]][2,j]])
      }
    }
}

names(resolution) = names(RS)
save(resolution, ProtSeq, file=file.path(resultdir, "resolution.rda"))




r = rep(NA, length(resolution))
names(r) = names(resolution)

for (i in seq_along(resolution)){
  r[i] = sum((resolution[[i]] / nchar(ProtSeq)) <= 0.2)
}


par(mar=c(5,10,4,1)+0.1)
barplot(sort(r) / length(ProtSeq),horiz = TRUE,las=2,xlab="Fraction mRNA binding proteins with resolution < 20% of protein length")



#### -----------  STEP 6/ ------------------------------------------------------
###---###
InputData$Uniqueness = Uniqueness

pdf( file=file.path(resultdir,"PeptideMappingBarplotNumGenes-1.pdf") )
barplot(table(InputData$numProt), 
        xlab="Number of matching proteins",ylab="Number of peptides",cex.lab=1.5,cex.axis=1.5,cex.names=1.5)
dev.off()


pdf( file=file.path(resultdir,"PeptideMappingBarplotNumGenes-2.pdf") )
barplot(table(InputData$numENSG), xlab="Number of matching genes",
        ylab="Number of peptides",cex.lab=1.5,cex.axis=1.5,cex.names=1.5)

dev.off()



#### -----------  STEP 8/ ------- Normalization --------------------------
###---###
#    
library(RBDmapHeLa)
library(genefilter)
library(Biobase)

basedir = getBaseDir()
resultdir = file.path( basedir, "result", "Normalization")
dir.create(resultdir, recursive = TRUE,showWarnings=FALSE)

load(file.path(basedir, "result", "peptideMapping", "ESetRaw.rda")) #The  raw  data  in  the  ExpressionSet  is  converted  to  a  new  ExpressionSet  containing  log-ratios  of  peptide  intensities.At first, log-ratios for F1 cCL to F1 noCL background control are computed.key = paste(ESetRaw$Enzyme,ESetRaw$Experiment,sep="_")I = which(ESetRaw$Cond == "F1cCL")J = which(ESetRaw$Cond == "F1noCL")J = J[match(key[I],key[J])]I = I[!is.na(J)]J = J[!is.na(J)]EF1 = ESetRaw[,I]exprs(EF1) = log2(exprs(ESetRaw[,I])) - log2(exprs(ESetRaw[,J]))EF1$Cond = "lrF1"Next, the log-ratios for F3 cCL (second oligo-dT pull-down after proteolysis with enzyme, e.g.  LysC or ArgC) to F2cCL (supernatant of second pull-down) are computed.key = paste(ESetRaw$Enzyme,ESetRaw$Experiment,sep="_")I = which(ESetRaw$Cond == "F3cCL")

RBDmap:::hwriteNormalization(EF3F2, dirname=resultdir)
#The two expression sets are combined and saved.

E = combine(EF1, EF3F2)
E$Cond = factor(E$Cond)
save(E, file=file.path(resultdir,"E.rda"))




#### -----------  STEP 9/ ------- STATISTICAL ANALYIS --------------------------
###---###
#    RBDmap analysis example12
library(RBDmapHeLa)
library(hwriter)
library(Biobase)
library(Biostrings)
library(limma)
library(genefilter)


basedir = getBaseDir("Data/")

resultdir = file.path( basedir, "result", "StatisticalAnalysis")
dir.create(resultdir, recursive = TRUE,showWarnings=FALSE)

load(file.path(basedir, "result", "Normalization", "E.rda"))

#####
efit = list()
for (enzyme in levels(E$Enzyme)){
  efit[[enzyme]] = list()
  for (cond in levels(E$Cond)){
    efit[[enzyme]][[cond]] = eBayes(lmFit(E[,(E$Cond==cond) & (E$Enzyme == enzyme)]))
    efit[[enzyme]][[cond]]$padj = p.adjust(efit[[enzyme]][[cond]]$p.value, method="BH")
    }
  }

save(efit,file=file.path(resultdir,"efit.rda"))




#####   -  A summary webpage is written
FDR.RBDpep = 0.01
FDR.candidateRBDpep = 0.1
Label = c("lrF1" = "F1 cCL input over F1 noCL control","lrF3F2" = "F3 cCL (pull-down) over F2 cCL (supernatant)")

for (cond in levels(E$Cond)){
  for (enzyme in levels(E$Enzyme)){
    RBDmap:::hwriteStatistics(E[,E$Cond==cond & E$Enzyme==enzyme],
                              efit[[enzyme]][[cond]],
                              label = sprintf("%s%s",enzyme,cond),
                              name = sprintf("%s (%s)",Label[cond],enzyme),
                              dirname=resultdir, maxFDR = FDR.RBDpep)
  }
}




Label = c("lrF1" = "F1 cCL / F1 noCL", "lrF3F2" = "F3 cCL / F2 cCL")
t=c()
for (cond in levels(E$Cond)){
  for (enzyme in levels(E$Enzyme)){
    if (cond=="lrF1"){col = ifelse(
      (efit[[enzyme]][[cond]]$padj <= FDR.RBDpep) &(efit[[enzyme]][[cond]]$coefficients > 0.0),
      colRBDmap["Input"],colRBDmap["Background"])
    }else{
      col = ifelse((efit[[enzyme]][[cond]]$padj <= FDR.candidateRBDpep) &
                   (efit[[enzyme]][[cond]]$coefficients > 0.0),
                   colRBDmap["CandidateRBDpep"],colRBDmap["Background"])
      
      col[which((efit[[enzyme]][[cond]]$padj <= FDR.RBDpep) &
                  (efit[[enzyme]][[cond]]$coefficients > 0.0))] = colRBDmap["RBDpep"]
    }
    Esub = E[,E$Cond==cond & E$Enzyme==enzyme]
    r = range(exprs(Esub),finite=TRUE)
    N = dim(Esub)[2]
    k = 1
    for (i in 1:(N-1)){
      for (j in (i+1):N){
        pdf( file=file.path(resultdir, sprintf( "StatisticsScatter%s%s-%i.pdf",
                                                enzyme, cond, k ) ) )
        par(mar=c(4.5,5,3,1.7))
        plot(exprs(Esub[,c(i,j)]),
             pch=20,cex=1.5,cex.axis=1.7,cex.lab=2,cex.main=2.5,col=col,
             main=sprintf("%s (%s)",Label[cond], enzyme),
             xlab=sprintf("%s [log2-ratio]",Esub$Experiment[i]),
             ylab=sprintf("%s [log2-ratio]",Esub$Experiment[j]),xlim=r,ylim=r)
        
        abline(v=0,h=0)
        text(x = r[2],y=r[2], labels = sprintf("R=%0.2f", cor(exprs(Esub[,i]),
             exprs(Esub[,j]), use="pairwise.complete.obs")),adj = c(1,1), cex=2)
        
        dev.off()
        k=k+1
      }
    }
  }
}










##################### AAAAAAAAAAAAA ###########################################
####
####
####
#### 
#### ----------  STEP 11/ -----------------------------------
#   MS-Paper 

# Files are part of RBDmap, see link above
load(file.path('inputdata','enigmRBP.rda'))
load(file.path('inputdata','ProtFeatures.rda'))
summary(ProtFeatures)
load(file.path('inputdata','Index.rda'))
load(file.path('inputdata','ENSG2category.rda'))
load(file.path('inputdata','ENSGannotation.rda'))
source(file.path('R','doGSEA.R'))
source(file.path('R','hwriteSidebar.R'))
source(file.path('R','gridSvgLinePlot.R'))

#Use the following file to define any additional proteins to be added to the mapping e.g. viral proteins. 
#Formatting should be protein sequences in fasta format.

SV_seq=readAAStringSet(file.path('inputdata','SV_proteins.txt'))
head(SV_seq)



# Read peptide sequences and intensities from RIC and WCL

## This loads the peptide output files from Maxquant for both the RIC ("_ric") and Input ("_who") 


input_raw_who= read.table(
  file.path("inputdata",'peptides_who.txt'), 
  sep="\t", comment.char="",quote="", 
  header=TRUE, stringsAsFactors=FALSE)

input_raw_ric= read.table(file.path("inputdata",'peptides_ric.txt'), sep="\t", comment.char="",quote="", 
                          header=TRUE, stringsAsFactors=FALSE)



# Remove contaminations



input_raw_who=input_raw_who[input_raw_who$Potential.contaminant=='' 
                            &input_raw_who$Reverse=='',]
input_raw_ric=input_raw_ric[input_raw_ric$Potential.contaminant=='' &input_raw_ric$Reverse=='',]



# Specify sample names and remove unused columns from peptide table.



sample_names=c('hour18','hour4','mock')

input_raw_who = 
  input_raw_who[,c(grep("Sequence",colnames(input_raw_who)),
                   grep("Intensity.",colnames(input_raw_who)))]

input_raw_who=input_raw_who[,c(1,6:8, 10:12, 14:16)]
input_raw_who[,c(2:10)][input_raw_who[,c(2:10)]<1]=NA
input_raw_who[,c(2:10)]=log2(input_raw_who[,c(2:10)])
input_raw_who=input_raw_who[,c(1,2,4,3,6,5,7,10,9,8)]
# Critical: At this point ensure that the sample order as deposited in input_raw_who from the maxquant file corresponds to the naming which are changed here. 
#Use View(input_raw_who) before and after the next command to ensure this is correctly translated
names(input_raw_who)=
  c('sequences',paste(sample_names,rep(1:3,each=3),sep='_'))



sample_names=c('hour18','hour4','mock')
input_raw_ric = 
  input_raw_ric[,c(grep("Sequence",colnames(input_raw_ric)),
                   grep("Intensity.",colnames(input_raw_ric)))]
input_raw_ric=input_raw_ric[,c(1,6:8, 10:12, 14:16)]
input_raw_ric[,c(2:10)][input_raw_ric[,c(2:10)]<1]=NA
input_raw_ric[,c(2:10)]=log2(input_raw_ric[,c(2:10)])
input_raw_ric=input_raw_ric[,c(1,4,3,2,5,7,6,9,8,10)]
# Critical: At this point ensure that the sample order as deposited in input_raw_who from the maxquant file corresponds to the naming which are changed here. 
#Use View(input_raw_who) before and after the next command to ensure this is correctly translated
names(input_raw_ric)=
  c('sequences',paste(sample_names,rep(1:3,each=3),sep='_'))



# Map the sequences to the protein sequence database

This provides a function to map protein IDs to Ensembl gene IDs 



MapProt2Ensg=function(protein_list){
  ###MapProt2Ensg maps the peptide-mapping proteins to genes
  ###and return a list of mapped genes
  lapply(protein_list,function(x) 
  { y=unname(ProtFeatures$GeneName[x])
  unique(y[!is.na(y)])})
}


# Extract single matched peptides 

(Only considering peptides than can be mapped to a single gene).

The name of additionally added proteins e.g. viral proteins is kept in the conversion from protein id to ENSEMBL gene id.



ProtIDs_who=mapPeptides(PeptideSet=input_raw_who$sequences, 
                        c(ProtFeatures$ProtSeq,SV_seq),
                        verbose=FALSE)
ENSGid_who=MapProt2Ensg(ProtIDs_who)
ENSGid_who[grep('SV_', ProtIDs_who)]=
  ProtIDs_who[grep('SV_', ProtIDs_who)]
ENSGid_who[grep('SV_', ProtIDs_who)]=lapply(ENSGid_who[grep('SV_', ProtIDs_who)], function(x) x[1])

ProtIDs_ric=mapPeptides(PeptideSet=input_raw_ric$sequences, 
                        c(ProtFeatures$ProtSeq,SV_seq),
                        verbose=FALSE)
ENSGid_ric=MapProt2Ensg(ProtIDs_ric)
ENSGid_ric[grep('SV_', ProtIDs_ric)]=ProtIDs_ric[grep('SV_', ProtIDs_ric)]
ENSGid_ric[grep('SV_', ProtIDs_ric)]=lapply(ENSGid_ric[grep('SV_', ProtIDs_ric)], function(x) x[1])




# Mapping summary

The number of protein(s) to which peptides map to is represented on the x-axis. The number of peptides for this grouping is presented on the y-axis. Any peptide not uniquely mapping is discarded from the analysis.




df=as.data.frame(table(listLen(ENSGid_who)))
colnames(df)=c('nr_protein_matches','nr_peptides')
p = ggplot(df, aes(x=nr_protein_matches,y=nr_peptides,
                   fill=factor(nr_protein_matches)))
p = p + geom_bar(stat='identity')+theme(legend.position = "none") +
  geom_text(aes(label = nr_peptides), size = 3)

print(p)

df=as.data.frame(table(listLen(ENSGid_ric)))
colnames(df)=c('nr_protein_matches','nr_peptides')
p = ggplot(df, aes(x=nr_protein_matches,y=nr_peptides,
                   fill=factor(nr_protein_matches)))
p = p + geom_bar(stat='identity')+theme(legend.position = "none") +
  geom_text(aes(label = nr_peptides), size = 3)

print(p)




# Aggregate mean intensity values for each ENSEMBL gene ID

The mean intensity values of peptides mapped to the same gene are calculated to represent the intensity of that protein.




###### Input

ENSGidUnique_who=ENSGid_who
ENSGidUnique_who[listLen(ENSGidUnique_who)!= 1]=NA
ENSGidUnique_who=unlist(ENSGidUnique_who)
input_raw_who=cbind(input_raw_who,ENSGid=ENSGidUnique_who, stringsAsFactors=FALSE)


proteins_who=aggregate(input_raw_who[,2:10], 
                       by=list(input_raw_who$ENSGid), mean, na.rm=TRUE)
names(proteins_who)[1]='ENSGid'


proteins_who$symbol=unname(unlist(
  sapply(proteins_who$ENSGid, 
         function(x) {if 
           (any(which(ProtFeatures$GeneName == x))) 
           ProtFeatures$Symbol[which(ProtFeatures$GeneName == x)]
           else NA})))

proteins_who$symbol[grep('SV_',proteins_who$ENSGid)]=
  as.character(proteins_who$ENSGid[grep('SV_',proteins_who$ENSGid)])

proteins_who$Know_RBP=ifelse(
  proteins_who$ENSGid %in% enigmRBP$Ensembl.gene.ID, 
  'known_RBP', 'no')
proteins_who=proteins_who[,c(1,11,12,2:10)]
proteins_who=data.frame(proteins_who,stringsAsFactors = FALSE )
#row.names(proteins_who)=NULL

###### RIC


ENSGidUnique_ric=ENSGid_ric
ENSGidUnique_ric[listLen(ENSGidUnique_ric)!= 1]=NA
ENSGidUnique_ric=unlist(ENSGidUnique_ric)
input_raw_ric=cbind(input_raw_ric,ENSGid=ENSGidUnique_ric, stringsAsFactors=FALSE)


proteins_ric=aggregate(input_raw_ric[,2:10], 
                       by=list(input_raw_ric$ENSGid), mean, na.rm=TRUE)
names(proteins_ric)[1]='ENSGid'


proteins_ric$symbol=unname(unlist(
  sapply(proteins_ric$ENSGid, 
         function(x) {if 
           (any(which(ProtFeatures$GeneName == x))) 
           ProtFeatures$Symbol[which(ProtFeatures$GeneName == x)]
           else NA})))

proteins_ric$symbol[grep('SV_',proteins_ric$ENSGid)]=
  as.character(proteins_ric$ENSGid[grep('SV_',proteins_ric$ENSGid)])

proteins_ric$Know_RBP=ifelse(
  proteins_ric$ENSGid %in% enigmRBP$Ensembl.gene.ID, 
  'known_RBP', 'no')
proteins_ric=proteins_ric[,c(1,11,12,2:10)]
proteins_ric=data.frame(proteins_ric,stringsAsFactors = FALSE )
#row.names(proteins_who)=NULL





# Multidimensional scaling to identify any potential problems

This approach visualises in an unbiased manner, how similar the samples in the dataset are to each other. 
Clustering of samples is expected to happen according to the experimental conditions. 
If samples cluster according to other variables e.g. biological replicate, this can highlight potential biases in the dataset for example differences in MS depth between replicates.



proteins_prebatch_ric=proteins_ric

MDS_ric=plotMDS(proteins_prebatch_ric[c(4:12)], labels = NULL, pch= c(19), col= c(3:1), cex = 1,   gene.selection = "pairwise", main='Multidimensional scaling RIC', xlab = "Leading logFC dimension 1", ylab = "Leading logFC dimension 2")
text(x=MDS_ric$x, y = MDS_ric$y, labels=c("1","1","1","2","2","2","3","3","3"), pos= 4, col=c(3:1))
legend("top", legend=c("18hpi","4hpi","Mock"), col=3:1, pch=19)

proteins_prebatch_who=proteins_who

MDS_who=plotMDS(proteins_prebatch_who[c(4:12)], labels = NULL, pch= c(19), col= c(3:1), cex = 1,   gene.selection = "pairwise", main='Multidimensional scaling Input', xlab = "Leading logFC dimension 1", ylab = "Leading logFC dimension 2")
text(x=MDS_who$x, y = MDS_who$y, labels=c("1","1","1","2","2","2","3","3","3"), pos= 4, col=c(3:1))
legend("topright", legend=c("18hpi","4hpi","Mock"), col=3:1, pch=19)




# Perform batch correction (only when necessary). 

For statistical analysis, batch correction should be implemented in an appropriate design matrix. 

For plotting of intensity values for visualisation, batch effects can be removed using removeBatchEffect.



## Potential batch effect due to SILAC labelling - Light, Medium and Heavy labels are represented with A, B and C

batch <- c("C","B","A","B","A","C","A","C","B")

## Potential batch effect due to experimental repeat/MS run

batch2 <- c("A","A","A","B","B","B","C","C","C")

# Multidimensional scaling of RIC shows clustering based on treatment but not based in experimental batch. 
# Hence, batch correction is here not required.


# Multidimensional scaling of Input shows clustering based on experimental batch, hence batch correction should be applied. 
# However, we do not observe clustering of treatments after correction, suggesting that samples are very similar between them. 
# Lack of changes between treatments is later confirmed by scatter plot of fold changes and statistical analysis.

proteins_who_batch<-proteins_prebatch_who
proteins_who_batch[,4:12]<-removeBatchEffect(proteins_prebatch_who[,4:12], batch2)

MDS_who=plotMDS(proteins_who_batch[c(4:12)], labels = NULL, pch= c(19), col= c(3:1), cex = 1,   gene.selection = "pairwise", main='Multidimensional scaling Input post batch removal', xlab = "Leading logFC dimension 1", ylab = "Leading logFC dimension 2")
text(x=MDS_who$x, y = MDS_who$y, labels=c("1","1","1","2","2","2","3","3","3"), pos= 4, col=c(3:1))
legend("bottomright", legend=c("18hpi","4hpi","Mock"), col=3:1, pch=19)




# Estimate relative RNA binding activity

Substract WCL from RIC values (already log2 transformed) to estimate how strong the change in RIC is driven by changes in abundance (RIC/WCL)



proteins_int=merge(proteins_who_batch, proteins_ric, 
                   by=c('ENSGid', 'symbol','Know_RBP'))
proteins_int[,4:12]=proteins_int[,13:21]-proteins_int[,4:12]
proteins_int=proteins_int[,1:12]



# Save the proteins lists



saveRDS(proteins_ric, 'Proteins_ric.rds')
saveRDS(proteins_who, 'Proteins_who.rds')
saveRDS(proteins_who_batch, 'Proteins_who_batch.rds')
saveRDS(proteins_int, 'Proteins_int.rds')



# Differential t-test at protein level

Moderated t-test for set enrichment

The aggregated mean intensity values for each protein is tested for enrichment between differnt states of infection. This is done using a paired moderated t-test as implemented in the Bioconductor-package limma.

For more complex scenarios, use an appropriate design matrix.

p-values are corrected for multiple testing using the Benjamini-Hochberg approach.



dirTtest='t_test'
dir.create(file.path(dirTtest), showWarnings = FALSE)

Intensities = as.matrix(proteins_who[,grepl("hour",colnames(proteins_who)) | grepl("mock",colnames(proteins_who))])
cond = sapply(strsplit(colnames(Intensities), split="_"),
              function(x) { x[1] })

sample_combi=combn(sample_names,2,simplify=TRUE)
colnames(sample_combi) = 
  apply(sample_combi,2,
        function(x) {
          paste('diff',paste(x,collapse='_'),sep='_')
        })

diff_table_who = diff_intensities_who = list()
for (s in colnames(sample_combi)) {
  sample1 = Intensities[,cond == sample_combi[1,s]]
  sample2 = Intensities[,cond == sample_combi[2,s]]
  X =sample1-sample2
  
  # This segment performs a median correction
  for (j in 1:length(sample_names)) {
    X[,j] = X[,j] - median(X[,j],na.rm = TRUE)}
  
  diff_intensities_who[[s]] = X
  fit=eBayes(lmFit(X))
  fit$p.adj=p.adjust(fit$p.value,method="BH")
  
  fit$p.adj[is.na(fit$p.adj)]=1
  
  dt = data.frame(ENSGid = proteins_who$ENSGid,
                  symbol = proteins_who$symbol,
                  Know_RBP = proteins_who$Know_RBP,
                  log2FC = fit$coefficients[,1],
                  p.value = fit$p.value[,1],
                  p.adj = fit$p.adj,
                  stringsAsFactors=FALSE)
  dt$sig=''
  dt$sig[dt$p.adj<0.1]='*'
  dt$sig[dt$p.adj<0.01]='**'
  dt=dt[,c(1,2,3,4,5,6,7)]
  diff_table_who[[s]] = dt
}


Intensities = as.matrix(proteins_ric[,grepl("hour",colnames(proteins_ric)) | 
                                       grepl("mock",colnames(proteins_ric))])
cond = sapply(strsplit(colnames(Intensities), split="_"),
              function(x) { x[1] })

sample_combi=combn(sample_names,2,simplify=TRUE)
colnames(sample_combi) = 
  apply(sample_combi,2,
        function(x) {
          paste('diff',paste(x,collapse='_'),sep='_')
        })

diff_table_ric = diff_intensities_ric = list()
for (s in colnames(sample_combi)) {
  sample1 = Intensities[,cond == sample_combi[1,s]]
  sample2 = Intensities[,cond == sample_combi[2,s]]
  X =sample1-sample2
  
  # This segment performs median correction.
  for (j in 1:length(sample_names)) {
    X[,j] = X[,j] - median(X[,j],na.rm = TRUE)}
  
  diff_intensities_ric[[s]] = X
  fit=eBayes(lmFit(X))
  fit$p.adj=p.adjust(fit$p.value,method="BH")
  
  fit$p.adj[is.na(fit$p.adj)]=1
  
  dt = data.frame(ENSGid = proteins_ric$ENSGid,
                  symbol = proteins_ric$symbol,
                  Know_RBP = proteins_ric$Know_RBP,
                  log2FC = fit$coefficients[,1],
                  p.value = fit$p.value[,1],
                  p.adj = fit$p.adj,
                  stringsAsFactors=FALSE)
  dt$sig=''
  dt$sig[dt$p.adj<0.1]='*'
  dt$sig[dt$p.adj<0.01]='**'
  dt=dt[,c(1,2,3,4,5,6,7)]
  diff_table_ric[[s]] = dt
}

save(diff_table_ric, file=file.path("t_test/diff_table_ric.rda"))
save(diff_table_who, file=file.path("t_test/diff_table_who.rda"))




# Saving the diff tables as text files




for (s in names(diff_table_ric)) {
  write.table(diff_table_ric[[s]][order(diff_table_ric[[s]]$p.value,
                                        diff_table_ric[[s]]$log2FC),],
              file=file.path("t_test",paste0("RIC_",s,'.txt')),
              sep="\t",row.names =FALSE, quote=FALSE)
  
  page = openPage(file.path("t_test",paste0("RIC_",s,".html")),link.css="hwriter.css")
  hwrite(paste0("Result list ",s),heading=1,page=page)
  table=diff_table_ric[[s]][order(diff_table_ric[[s]]$p.adj, diff_table_ric[[s]]$log2FC),]
  row.names(table)=NULL
  hwrite(format(table,digits=4),
         page=page)
}

for (s in names(diff_table_who)) {
  write.table(diff_table_who[[s]][order(diff_table_who[[s]]$p.value,
                                        diff_table_who[[s]]$log2FC),],
              file=file.path("t_test",paste0("Input_",s,'.txt')),
              sep="\t",row.names =FALSE, quote=FALSE)
  
  page = openPage(file.path("t_test",paste0("Input_",s,".html")),link.css="hwriter.css")
  hwrite(paste0("Result list ",s),heading=1,page=page)
  table=diff_table_who[[s]][order(diff_table_who[[s]]$p.adj, diff_table_who[[s]]$log2FC),]
  row.names(table)=NULL
  hwrite(format(table,digits=4),
         page=page)
}




# Performing semi-quantitative analysis

This segment performs a semi-quantitative analysis, comparing the different experimental conditions, to identify strong on vs off and off vs on situations which escape the above statistical analysis due to missing values.

The analysis generates tables which contain proteins, which have detectable intensity values in one condition (in 3 or 2 replicates), but are detected only once or absent in a second condition.




semi_dir=file.path('semi_quant')
dir.create(semi_dir, showWarnings = FALSE)

add_summary=function(proteinset){
  hour18_index=grep('18',colnames(proteinset))
  hour4_index=grep('4',colnames(proteinset))
  mock_index=grep('mock',colnames(proteinset))
  proteinset=cbind(proteinset, 
                   hour18_total=apply(proteinset,1, 
                                      function(x) {
                                        sum(!is.na(x[hour18_index]))
                                      }))
  proteinset=cbind(proteinset, 
                   hour4_total=apply(proteinset,1,
                                     function(x) {
                                       sum(!is.na(x[hour4_index]))
                                     }))
  proteinset=cbind(proteinset, 
                   mock_total=apply(proteinset,1,
                                    function(x) {
                                      sum(!is.na(x[mock_index])) 
                                    }))
  proteinset
}

semi_hour4_mock=apply(is.na(Intensities[,cond == "mock"]) - 
                        is.na(Intensities[,cond == "hour4"]), 1, 
                      function(x) { sum(x) } )
semi_hour4_mock_up = proteins_ric[semi_hour4_mock>=2,]
semi_hour4_mock_up =
  semi_hour4_mock_up[,c(1,2,3,6,9,12,5,8,11,4,7,10)]
semi_hour4_mock_up=add_summary(semi_hour4_mock_up)
write.table(semi_hour4_mock_up, file=
              file.path(semi_dir,'semi_hour4_mock_up.txt'),
            row.names=FALSE,quote=FALSE)

semi_hour4_mock_down=proteins_ric[semi_hour4_mock<=-2,]
semi_hour4_mock_down=
  semi_hour4_mock_down[,c(1,2,3,6,9,12,5,8,11,4,7,10)]
semi_hour4_mock_down=add_summary(semi_hour4_mock_down)
write.table(semi_hour4_mock_down, file=
              file.path(semi_dir,'semi_hour4_mock_down.txt'),
            row.names=FALSE,quote=FALSE)

semi_hour18_mock=apply(is.na(Intensities[,cond == "mock"]) - 
                         is.na(Intensities[,cond == "hour18"]), 1, 
                       function(x) { sum(x) } )
semi_hour18_mock_up=proteins_ric[semi_hour18_mock>=2,]
semi_hour18_mock_up=
  semi_hour18_mock_up[,c(1,2,3,6,9,12,5,8,11,4,7,10)]
semi_hour18_mock_up=add_summary(semi_hour18_mock_up )
write.table(semi_hour18_mock_up, file=
              file.path(semi_dir,'semi_hour18_mock_up.txt'),
            row.names=FALSE,quote=FALSE)

semi_hour18_mock_down=proteins_ric[semi_hour18_mock<=-2,]
semi_hour18_mock_down=
  semi_hour18_mock_down[,c(1,2,3,6,9,12,5,8,11,4,7,10)]
semi_hour18_mock_down=add_summary(semi_hour18_mock_down)
write.table(semi_hour18_mock_down, 
            file=file.path(semi_dir,'semi_hour18_mock_down.txt'),
            row.names=FALSE,quote=FALSE)

semi_hour18_hour4=apply(is.na(Intensities[,cond == "hour4"]) - 
                          is.na(Intensities[,cond == "hour18"]), 1, 
                        function(x) { sum(x) } )
semi_hour18_hour4_up=proteins_ric[semi_hour18_hour4>=2,]
semi_hour18_hour4_up=
  semi_hour18_hour4_up[,c(1,2,3,6,9,12,5,8,11,4,7,10)]
semi_hour18_hour4_up=add_summary(semi_hour18_hour4_up)
write.table(semi_hour18_hour4_up, 
            file=file.path(semi_dir,'semi_hour18_hour4_up.txt'),
            row.names=FALSE,quote=FALSE)

semi_hour18_hour4_down=proteins_ric[semi_hour18_hour4<=-2,]
semi_hour18_hour4_down=
  semi_hour18_hour4_down[,c(1,2,3,6,9,12,5,8,11,4,7,10)]
semi_hour18_hour4_down=add_summary(semi_hour18_hour4_down)
write.table(semi_hour18_hour4_down, 
            file=file.path(semi_dir,'semi_hour18_hour4_down.txt'),
            row.names=FALSE,quote=FALSE)





# Scatterplots

This segment generates scatterplots, which display the log2 fold change comparing the different replicates, highlighting significantely changed proteins in colour.



for (s in colnames(sample_combi)) {
  col = ifelse(diff_table_ric[[s]]$p.adj <= 0.1, "orange", "gray")
  col[which(diff_table_ric[[s]]$p.adj <= 0.05)] = "firebrick1"
  col[which(diff_table_ric[[s]]$p.adj <= 0.01)] = "firebrick"
  col[which(diff_table_ric[[s]]$p.adj <= 0.1 & 
              diff_table_ric[[s]]$log2FC < 0)] = "turquoise1"
  col[which(diff_table_ric[[s]]$p.adj <= 0.05 & 
              diff_table_ric[[s]]$log2FC < 0)] = "turquoise4"
  col[which(diff_table_ric[[s]]$p.adj <= 0.01 & 
              diff_table_ric[[s]]$log2FC < 0)] = "blue"
  
  pairs(diff_intensities_ric[[s]],col=col, pch=19,labels = c("Replicate 1", "Replicate 2", "Replicate 3"), main=paste("RIC ", gsub("_mock"," vs Mock",gsub("diff_hour","hpi",s))), xlim=c(-5,5), ylim=c(-5,5))}


for (s in colnames(sample_combi)) {
  col = ifelse(diff_table_who[[s]]$p.adj <= 0.1, "orange", "gray")
  col[which(diff_table_who[[s]]$p.adj <= 0.05)] = "firebrick1"
  col[which(diff_table_who[[s]]$p.adj <= 0.01)] = "firebrick"
  col[which(diff_table_who[[s]]$p.adj <= 0.1 & 
              diff_table_who[[s]]$log2FC < 0)] = "turquoise1"
  col[which(diff_table_who[[s]]$p.adj <= 0.05 & 
              diff_table_who[[s]]$log2FC < 0)] = "turquoise4"
  col[which(diff_table_who[[s]]$p.adj <= 0.01 & 
              diff_table_who[[s]]$log2FC < 0)] = "blue"
  
  pairs(diff_intensities_who[[s]],col=col, pch=19,labels = c("Replicate 1", "Replicate 2", "Replicate 3"), main=paste("Input ", gsub("_mock"," vs Mock",gsub("diff_hour","hpi",s))), xlim=c(-5,5), ylim=c(-5,5))}




# Volcanoplot 

Here volcanoplots displaying the log2 fold change vs significance over the 3 biological replicates are generated, with different significance levels highlighted in colour



for (s in colnames(sample_combi)) {
  volData = diff_table_who[[s]]
  volData = volData[!is.na(volData$p.adj),]
  col = ifelse(volData$p.adj <= 0.1, "orange", "gray")
  col[which(volData$p.adj <= 0.05)] = "firebrick1"
  col[which(volData$p.adj <= 0.01)] = "firebrick"
  col[which(volData$p.adj <= 0.1 &  volData$log2FC < 0)] = "turquoise1"
  col[which(volData$p.adj <= 0.05 &  volData$log2FC < 0)] = "turquoise4"
  col[which(volData$p.adj <= 0.01 &  volData$log2FC < 0)] = "blue"
  col[col=='gray']=adjustcolor('gray', alpha=0.2)
  
  g = ggplot(volData, aes(log2FC,-log10(p.adj)))+
    geom_point(size=1, color=col)+ 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))+
    
    xlim(c(-6, 6)) + ylim(c(0, 3)) +
    xlab("log2 fold change") + ylab("-log10 p-value") +
    labs(title = paste("Input ", gsub("_mock"," vs Mock",gsub("diff_hour","hpi",s))))
  
  print(g)
}

for (s in colnames(sample_combi)) {
  volData = diff_table_ric[[s]]
  volData = volData[!is.na(volData$p.adj),]
  col = ifelse(volData$p.adj <= 0.1, "orange", "gray")
  col[which(volData$p.adj <= 0.05)] = "firebrick1"
  col[which(volData$p.adj <= 0.01)] = "firebrick"
  col[which(volData$p.adj <= 0.1 &  volData$log2FC < 0)] = "turquoise1"
  col[which(volData$p.adj <= 0.05 &  volData$log2FC < 0)] = "turquoise4"
  col[which(volData$p.adj <= 0.01 &  volData$log2FC < 0)] = "blue"
  col[col=='gray']=adjustcolor('gray', alpha=0.2)
  
  g = ggplot(volData, aes(log2FC,-log10(p.adj)))+
    geom_point(size=1, color=col)+ 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))+
    
    xlim(c(-6, 6)) + ylim(c(0, 3)) +
    xlab("log2 fold change") + ylab("-log10 p-value") +
    labs(title = paste("RIC", gsub("_mock"," vs Mock",gsub("diff_hour","hpi",s))))
  
  print(g)
}




# Intensity between two replicates

## Plotting the Intensity of proteins between two replicates in the Input

Selected proteins are highlighted in colour.



grepl('SV_wt_nsP2',proteins_who_batch$ENSGid)->A
grepl('SV_wt_E2',proteins_who_batch$ENSGid)->B

par(pty="s")

plot(proteins_who_batch[,4], proteins_who_batch[,7], col=alpha('gray',0.2),
     pch=19,xlim=c(20,34), ylim=c(20,34),
     xlab='Abundance WCL Replicate 1', ylab='Abundance WCL Replicate 2')
title(main="Protein Abundance in WCL \n (Replicate 1 and 2)")

points(proteins_who_batch[A,4], proteins_who_batch[A,7], col='red', pch=19)
text(proteins_who_batch[A,4], proteins_who_batch[A,7], 
     labels="nsP2", cex= 1, pos=3)

points(proteins_who_batch[B,4], proteins_who_batch[B,7], col='blue', pch=19)
text(proteins_who_batch[B,4], proteins_who_batch[B,7], 
     labels="E2", cex= 1, pos=3)



## Plotting the Intensity of proteins between two replicates in the RIC.

Selected proteins are highlighted in colour.




grepl('SV_wt_nsP2',proteins_ric$ENSGid)->AI
grepl('SV_wt_E2',proteins_ric$ENSGid)->BI

par(pty="s")

plot(proteins_ric[,4], proteins_ric[,7], col=alpha('gray',0.2),
     pch=19,xlim=c(16,29), ylim=c(16,29),
     xlab='Abundance RIC Replicate 1', ylab='Abundance RIC Replicate 2')
title(main="Protein Abundance in RIC")

points(proteins_ric[AI,4], proteins_ric[AI,7], col='red', pch=19)
text(proteins_ric[AI,4], proteins_ric[AI,7], 
     labels="nsP2", cex= 1, pos=3)

points(proteins_ric[BI,4], proteins_ric[BI,7], col='blue', pch=19)
text(proteins_ric[BI,4], proteins_ric[BI,7], 
     labels="E2", cex= 1, pos=3)



## Plotting the Intensity of proteins between two replicates in the RIC/WCL comparision to estimate RNA binding affinity

Selected proteins are highlighted in colour.



grepl('SV_wt_nsP2',proteins_int$ENSGid)->AI
grepl('SV_wt_E2',proteins_int$ENSGid)->BI

par(pty="s")

plot(proteins_int[,4], proteins_int[,7], col=alpha('gray',0.2),
     pch=19,xlim=c(-11,0), ylim=c(-11,0),
     xlab='RIC/WCL Replicate 1', ylab='RIC/WCL Replicate 2')
title(main="Relative RNA binding activity \n (RIC/WCL)")

points(proteins_int[AI,4], proteins_int[AI,7], col='red', pch=19)
text(proteins_int[AI,4], proteins_int[AI,7], 
     labels="nsP2", cex= 1, pos=3)

points(proteins_int[BI,4], proteins_int[BI,7], col='blue', pch=19)
text(proteins_int[BI,4], proteins_int[BI,7], 
     labels="E2", cex= 1, pos=3)



# Sessioninfo



sessionInfo()










#