#  vergleich der daten mit den daten, die die anderen gefunden haben 

library(readxl)

b_only <- read_excel("Data/Olga Rudi_ComparisonAB_TAIR_SpurB_only.xlsx")
nrow(b_only)



comparison = subset(subset_B, !(Accession %in% b_only$Accession))
nrow(comparison)
comparison$Accession

###
###
##### --------------------------------------------------------------------------
# ------------------------------------------------------------------------------

counter_A = 0

for ( i in 1: nrow(Olga_Rudi_Compi_TAIR_SpurA)){
  for ( j in 1: nrow(sub_they)){
    if ( sub_they$Accession[j]  == Olga_Rudi_Compi_TAIR_SpurA$Accession[i]){
      counter_A = counter_A + 1 
    }
  }
}
counter_A


counter_B = 0
for ( i in 1: nrow(Olga_Rudi_Compi_TAIR_SpurB)){
  for ( j in 1: nrow(sub_they)){
    if ( Olga_Rudi_Compi_TAIR_SpurB$Accession[i] == sub_they$Accession[j]){
      counter_B = counter_B + 1 
    }
  }
}
counter_B


######
#'
#'
#'


# ------- COMPARISON OF THEIR AND MY DATASET, for VENN_DIAGRAM
repeat.experiment_data = data.frame(matrix(NA,500,3))
counter = 1

for (i in 1:nrow(subset_B)){
  for (j in 1:nrow(b_only)){
    if (subset_B$Accession[i] == b_only$Accession[j]){
      repeat.experiment_data[counter, 1] <- subset_B$Accession[i]
      repeat.experiment_data[counter, 2] <- subset_B$Row[i]
      repeat.experiment_data[counter, 3] <- b_only$Row[j]
      counter = counter + 1 
    }
      
  }
}


data = repeat.experiment_data[complete.cases(repeat.experiment_data), ]
colnames(data) = c("Accession", "row1", "row2")
data
nrow(data)

### out of 

library(tidyverse)
library(hrbrthemes)
library(VennDiagram)


venn_data = data.frame(matrix(NA,3,1))
venn_data[1,] = nrow(data)
venn_data[2,] = nrow(b_only)
venn_data[3,] = nrow(subset_B)


##
##   ----------------------------------------------------------------------------------------
## 

display_venn <- function(x, ...){
  library(VennDiagram)
  grid.newpage()
  venn_object <- venn.diagram(x, filename = NULL, ...)
  grid.draw(venn_object)
}

mainDir  = getwd()  # check current wroking directory
imageDir <- "img_VENN" 
###  creating output folder for images
# this part, checks if currently putputDirectory exist and creates it if not
if (file.exists(imageDir)){
  setwd(file.path(mainDir, imageDir))
} else {
  dir.create(file.path(mainDir, imageDir), showWarnings = FALSE)
  setwd(file.path(mainDir, imageDir))
}
##
##   ----------------------------------------------------------------------------------------
###  Make the plot
list_me_they = list(
  R_Pipeline = subset_B$Accession, 
  Others = b_only$Accession)

list_A_B = list(
  TAIR_A = Olga_Rudi_Compi_TAIR_SpurA$Accession, 
  TAIR_B = Olga_Rudi_Compi_TAIR_SpurB$Accession)



display_venn(
  list_me_they ,
  category.names = c("KDG_Pipeline" , "recieved results" ),
  # Circles
  lwd = 3,
  lty = 1,
  fill = c("#009E73" , "#000000"),
  col = c(  "#009E73" , "#000000"),
  label.col = "black",
  alpha = 0.3,
  # Numbers
  cex = 2.5,
  fontface = "italic",
  # Set names
  cat.cex = 1,
  cat.fontface = "bold",
  cat.default.pos = "text",
  cat.dist = c(0.1, 0.1),
  margin = 0.1
)



display_venn(
  list_A_B,
  category.names = c("non_crosslinked" , "crosslinked" ),
  # Circles
  lwd = 3,
  lty = 1,
  fill = c("#370077" , "#FFDD00"),
  col = c(  "#370077" , "#FFDD00"),
  label.col = "black",
  alpha = 0.3,
  # Numbers
  cex = 2.5,
  fontface = "italic",
  # Set names
  cat.cex = 1.7,
  cat.fontface = "bold",
  cat.default.pos = "text",
  cat.dist = c(0.1, 0.1),
  margin = 0.1
)

##   ----------------------------------------------------------------------------------------









##   ----------------------------------------------------------------------------------------

##### ------------------------------------------------------------------- ######
sub_ME = subset(subset_B, !(Accession %in% data$Accession))
##   PROTEINS that I found 
nrow(sub_ME)
nrow(subset_B)
nrow(data)


sub_they = subset(b_only, !(Accession %in% data$Accession))
nrow(sub_they)
nrow(b_only)
nrow(data)




##   ----------------------------------------------------------------------------------------
### writing xlsx files 
library(xlsx)
sub_path_ME = file.path(mainDir, imageDir, "/sub_ME.xlsx")  
write.xlsx2(sub_ME, file = sub_path_ME, sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)


sub_path_they = file.path(mainDir, imageDir, "/sub_they.xlsx")  
write.xlsx2(sub_they, file = sub_path_they, sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)
##   ----------------------------------------------------------------------------------------







###### detecting the 17 missing ones // these are the proteins, they found extra
repeat.experiment_they = data.frame(matrix(NA,20,4))
counter = 1

for ( i in 1: nrow(sub_they)){    # for every element in sub_they
  for (j in 1: nrow(reference)){   # for every element in reference
    if (str_detect( as.character( lapply(sub_they$Accession[i], tolower)) ,  as.character( lapply(reference$ATG[j], tolower)) ) == TRUE){
      repeat.experiment_they[counter, 1] <- sub_they$Accession[i]
      rrepeat.experiment_they[counter, 2] <- reference$ATG[j]
      repeat.experiment_they[counter, 3] <- reference$PROTEIN [j]
      repeat.experiment_they[counter, 4] <- reference$COMPLEX [j]
      
      counter = counter + 1
    }
  }
}
POI_they = repeat.experiment_they[complete.cases(repeat.experiment_they), ]
POI_they



###### detecting my 37 missing ones // these are the proteins, that i found extra
repeat.experiment_me = data.frame(matrix(NA,20,4))
counter = 1

for ( i in 1: nrow(sub_ME)){    # for every element in sub_ME
  for (j in 1: nrow(reference)){   # for every element in reference
    if (str_detect( as.character( lapply(sub_ME$Accession[i], tolower)) ,  as.character( lapply(reference$ATG[j], tolower)) ) == TRUE){
      
      repeat.experiment_me[counter, 1] <- sub_ME$Accession[i]
      repeat.experiment_me[counter, 2] <- reference$ATG[j]
      repeat.experiment_me[counter, 3] <- reference$PROTEIN [j]
      repeat.experiment_me[counter, 4] <- reference$COMPLEX [j]
      
      counter = counter + 1
    }
  }
}
POI_me = repeat.experiment_me[complete.cases(repeat.experiment_me), ]
POI_me

colnames(POI_me) = c("Accession", "ATG", "PROTEIN", "COMPLEX" )
colnames(POI_they) = c("Accession", "ATG", "PROTEIN", "COMPLEX" )

POI__me_path = file.path(mainDir, imageDir, "/sub_ME_POI.xlsx")  
write.xlsx2(POI_me, file = POI__me_path, sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)


sub_path_they_path = file.path(mainDir, imageDir, "/sub_they_POI.xlsx")  
write.xlsx2(POI_they, file = sub_path_they_path, sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)

##   ----------------------------------------------------------------------------------------









##   ----------------------------------------------------------------------------------------

list_final = list(
  POI_they, 
  sub_path_they,
  POI_me,
  sub_path_ME)


display_venn(
  list_final ,
  category.names = c("POI_me" , "POI_them" , "additional_me" , "additional_them"),
  # Circles
  lwd = 3,
  lty = 1,
  fill = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
  col = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
  label.col = "black",
  alpha = 0.3,
  # Numbers
  cex = 2.5,
  fontface = "italic",
  # Set names
  cat.cex = 1,
  cat.fontface = "bold",
  cat.default.pos = "text",
  cat.dist = c(0.1, 0.1, 0.1, 0.2),
  margin = 0.1
)

    
#
