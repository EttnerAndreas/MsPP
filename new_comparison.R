#  vergleich der daten mit den daten, die die anderen gefunden haben 


b <- read_excel("Data/Olga Rudi_ComparisonAB_TAIR_SpurB_only.xlsx")
nrow(b)


a <- read_excel("Data/Olga Rudi_ComparisonAB_TAIR.xlsx")
nrow(a)


test = subset(subset_B, !(Accession %in% b$Accession))
nrow(test)
test$Accession




Olga_Rudi_Compi_TAIR_SpurA <- read_excel("Data/Olga Rudi_Compi_TAIR_SpurA.xlsx")
Olga_Rudi_Compi_TAIR_SpurB <- read_excel("Data/Olga Rudi_Compi_TAIR_SpurB.xlsx")

## dataframe_A,Bdataframe_A,
for (i in 1:nrow(b)){
  if ( b$Accession[i] == "AT4G32720.2"){
    print("treffer_1")
  }
  if ( b$Accession[i] == "AT3G52660.1"){
    print("treffer_2")
  }
  if ( b$Accession[i] == "AT3G55200.1"){
    print("treffer_3")
  }
  if ( b$Accession[i] == "AT4G08350.1"){
    print("treffer_4")
  }
}

View(b)


for (i in 1:nrow(Olga_Rudi_Compi_TAIR_SpurB)){
  if ( Olga_Rudi_Compi_TAIR_SpurB$Accession[i] == "AT5G24650.1"){
    print("treffer_2")
  }
  if ( Olga_Rudi_Compi_TAIR_SpurB$Accession[i] == "AT3G46430.1"){
    print("treffer_3")
  }
  if ( Olga_Rudi_Compi_TAIR_SpurB$Accession[i] == "AT1G52370.1"){
    print("treffer_4")
  }
  if ( Olga_Rudi_Compi_TAIR_SpurB$Accession[i] == "AT3G06530.3"){
    print("treffer_4")
  }
}

subseee$Accession


##### --------------------------------------------------------------------------
# ------------------------------------------------------------------------------

repeat.experiment_M = data.frame(matrix(NA,300,4))
counter = 1

for ( i in 1: nrow(b)){
  if (b$Accession[i] == "AT4G08350.1"){
    print("1")
  }
  
  if (b$Accession[i] == "AT3G55200.1"){
    print("1")
  }
  
  if (b$Accession[i] == "AT4G32720.2"){
    print("1")
  }
  
}

BETA_CROSSLINKED = repeat.experiment_M[complete.cases(repeat.experiment_M), ]














######
#'
#'
#'

repeat.experiment_B = data.frame(matrix(NA,500,3))
counter = 1

for (i in 1:nrow(subset_B)){
  for (j in 1:nrow(b)){
    if (subset_B$Accession[i] == b$Accession[j]){
      repeat.experiment_B[counter, 1] <- subset_B$Accession[i]
      repeat.experiment_B[counter, 2] <- subset_B$Row[i]
      repeat.experiment_B[counter, 3] <- b$Row[j]
      counter = counter + 1 
    }
      
  }
}

data = repeat.experiment_B[complete.cases(repeat.experiment_B), ]
colnames(data) = c("Accession", "row1", "row2")
data
nrow(data)
##### ------------------------------------------------------------------- ######
sub_ME = subset(subset_B, !(Accession %in% data$Accession))
##   PROTEINS that I found 
nrow(sub_ME)
nrow(subset_B)
nrow(data)






sub_they = subset(b, !(Accession %in% data$Accession))

nrow(sub_THEY)
nrow(b)
nrow(data)

View(sub_THEY)

sub_THEY$Accession








repeat.experiment_they = data.frame(matrix(NA,200,4))
counter = 1


###### detecting the 17 missing ones // these are the proteins, they found extra

for ( i in 1: nrow(sub_THEY)){    # for every element in sub_THEY
  for (j in 1: nrow(reference)){   # for every element in reference
    if (str_detect( as.character( lapply(sub_THEY$Accession[i], tolower)) ,  as.character( lapply(reference$ATG[j], tolower)) ) == TRUE){
      
      repeat.experiment_A[counter, 1] <- sub_THEY$Accession[i]
      repeat.experiment_A[counter, 2] <- reference$ATG[j]
      repeat.experiment_A[counter, 3] <- reference$PROTEIN [j]
      repeat.experiment_A[counter, 4] <- reference$COMPLEX [j]
      
      counter = counter + 1
    }
  }
}
NO_CROSSLINK_they = repeat.experiment_A[complete.cases(repeat.experiment_A), ]
NO_CROSSLINK_they



###### detecting my 37 missing ones // these are the proteins, that i found extra
repeat.experiment_me = data.frame(matrix(NA,200,4))
counter = 1

for ( i in 1: nrow(sub_ME)){    # for every element in sub_ME
  for (j in 1: nrow(reference)){   # for every element in reference
    if (str_detect( as.character( lapply(sub_ME$Accession[i], tolower)) ,  as.character( lapply(reference$ATG[j], tolower)) ) == TRUE){
      
      repeat.experiment_A[counter, 1] <- sub_ME$Accession[i]
      repeat.experiment_A[counter, 2] <- reference$ATG[j]
      repeat.experiment_A[counter, 3] <- reference$PROTEIN [j]
      repeat.experiment_A[counter, 4] <- reference$COMPLEX [j]
      
      counter = counter + 1
    }
  }
}
NO_CROSSLINK_me = repeat.experiment_A[complete.cases(repeat.experiment_A), ]

NO_CROSSLINK_me




#