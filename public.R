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


######## -------------- REFERENCE ATG XLS FILE ---------------------------------
###
##  
controll <- read_excel("C:/Users/andie/Dropbox/Mein PC (DESKTOP-4TB10SM)/Downloads/Pipeline/TREX componenten.xlsx", 
                       col_names = FALSE)
colnames(controll) <- c("Protein", "ATG", "complex")
str(controll)

library(readxl)
reference <- read_excel("C:/Users/andie/Dropbox/Mein PC (DESKTOP-4TB10SM)/Downloads/Kopie von POI.xlsx", 
                        skip = 1)
# reference = read_excel("C:/Users/andie/Dropbox/Mein PC (DESKTOP-4TB10SM)/Downloads/13007_2016_142_MOESM2_ESM.xlsx", skip = 6)
# reference = reference [, c(1,3,4)]



reference = reference[, c(1:3)]
colnames(reference) = c("ATG" , "PROTEIN", "COMPLEX")
### delete NA´s in the reference dataset
reference = reference[complete.cases(reference), ]
reference$ATG[71] = 	"At1g608512"
# View(reference)
summary(reference)

####
####
####


TAIR_SpurB = Olga_Rudi_Compi_TAIR_SpurB
TAIR_SpurA = Olga_Rudi_Compi_TAIR_SpurA
# 
# devtools::install_github("yanlinlin82/ggvenn@v0.1.0")
# library(ggvenn)
library(plyr)
df1 = data.frame(TAIR_SpurB)
df2 = data.frame(TAIR_SpurA)



df_match1 = match_df(df1, df2, on = "Accession")
df_match2 = match_df(df2, df1, on = "Accession")

nrow(df1)
nrow(df2)
nrow(df_match)
df_match
######
###-------------- SUBSET DATA_ A ------------------------
subset_A = subset(TAIR_SpurA, !(Accession %in% TAIR_SpurB$Accession))

str(subset_A)
nrow(subset_A)



###-------------- SUBSET DATA_ B ------------------------
subset_B = subset(TAIR_SpurB, !(Accession %in% TAIR_SpurA$Accession))
str(subset_B)
nrow(subset_B)

# write.xlsx(subset_B, file = "C:/Users/andie/Dropbox/Mein PC (DESKTOP-4TB10SM)/Downloads/Pipeline/xlsx-Data/513_crosslink_specific.xlsx", sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)

######### setdiff(a$x, b$y)
# install.packages("RecordLinkage")
library(RecordLinkage)

################################################################################
##### -------------------------------------------------------------------- #####
####   A = NO CROSSLINK   ////  B = CROSSLINK  ////  M = matched Proteins == INTERMEDIATE
###
###
#       #RInno

####### ----------------------------------------------
library(stringr)
repeat.experiment_A = data.frame(matrix(NA,200,4))
counter = 1

for ( i in 1: nrow(subset_A)){
  for (j in 1: nrow(reference)){
    if (str_detect( as.character( lapply(subset_A$Accession[i], tolower)) ,  as.character( lapply(reference$ATG[j], tolower)) ) == TRUE){
      # print(reference$PROTEIN [j])
      # print(reference$COMPLEX [j])
      # print("----")
      repeat.experiment_A[counter, 1] <- subset_A$Accession[i]
      repeat.experiment_A[counter, 2] <- reference$ATG[j]
      repeat.experiment_A[counter, 3] <- reference$PROTEIN [j]
      repeat.experiment_A[counter, 4] <- reference$COMPLEX [j]
      
      counter = counter + 1
    }
  }
}
NO_CROSSLINK = repeat.experiment_A[complete.cases(repeat.experiment_A), ]
# View(NO_CROSSLINK)
# saveRDS(NO_CROSSLINK, file = "OLGA_no_CROSSLINK")

###    CO > %>%  < OC -
#       

####### ----------------------------------------------
repeat.experiment_B = data.frame(matrix(NA,200,4))
counter = 1

for ( i in 1: nrow(subset_B)){
  for (j in 1: nrow(reference)){
    if (str_detect( as.character( lapply(subset_B$Accession[i], tolower)) ,  as.character( lapply(reference$ATG[j], tolower)) ) == TRUE){
      # print(reference$PROTEIN [j])
      # print(reference$COMPLEX [j])
      # print("----")
      repeat.experiment_B[counter, 1] <- subset_B$Accession[i]
      repeat.experiment_B[counter, 2] <- reference$ATG[j]
      repeat.experiment_B[counter, 3] <- reference$PROTEIN [j]
      repeat.experiment_B[counter, 4] <- reference$COMPLEX [j]
      
      counter = counter + 1
    }
  }
}

CROSSLINK = repeat.experiment_B[complete.cases(repeat.experiment_B), ]
# View(CROSSLINK)
# saveRDS(CROSSLINK, file = "OLGA_CROSSLINK")



###    CO > %>%  < OC -
#       

####### ----------------------------------------------
repeat.experiment_M = data.frame(matrix(NA,200,4))
counter = 1

for ( i in 1: nrow(df_match)){
  for (j in 1: nrow(reference)){
    if (str_detect( as.character( lapply(df_match$Accession[i], tolower)) ,  as.character( lapply(reference$ATG[j], tolower)) ) == TRUE){
      #
      #
      repeat.experiment_M[counter, 1] <- df_match$Accession[i]
      repeat.experiment_M[counter, 2] <- reference$ATG[j]
      repeat.experiment_M[counter, 3] <- reference$PROTEIN [j]
      repeat.experiment_M[counter, 4] <- reference$COMPLEX [j]
      
      counter = counter + 1
    }
  }
}

INTERMEDIATE = repeat.experiment_M[complete.cases(repeat.experiment_M), ]
# View(INTERMEDIATE)
# saveRDS(CROSSLINK, file = "OLGA_INTERMEDIATE")


################################################################################
######                                                                   #######
#####                                                                      #####
#####                                                                      #####
#### -----------------------------------------------------------------------####
###    CO > %>%  < OC -
#       

####### ----------------------------------------------
repeat.experiment_M = data.frame(matrix(NA,300,4))
counter = 1

for ( i in 1: nrow(TAIR_SpurA)){
  for (j in 1: nrow(reference)){
    if (str_detect( as.character( lapply(TAIR_SpurA$Accession[i], tolower)) ,  as.character( lapply(reference$ATG[j], tolower)) ) == TRUE){
      #
      #
      repeat.experiment_M[counter, 1] <- TAIR_SpurA$Accession[i]
      repeat.experiment_M[counter, 2] <- reference$ATG[j]
      repeat.experiment_M[counter, 3] <- reference$PROTEIN [j]
      repeat.experiment_M[counter, 4] <- reference$COMPLEX [j]
      
      counter = counter + 1
    }
  }
}

ALPHA_NON_CROSSLINKED = repeat.experiment_M[complete.cases(repeat.experiment_M), ]
# View(INTERMEDIATE)
# saveRDS(CROSSLINK, file = "OLGA_INTERMEDIATE")



####### ----------------------------------------------
repeat.experiment_M = data.frame(matrix(NA,300,4))
counter = 1

for ( i in 1: nrow(TAIR_SpurB)){
  for (j in 1: nrow(reference)){
    if (str_detect( as.character( lapply(TAIR_SpurB$Accession[i], tolower)) ,  as.character( lapply(reference$ATG[j], tolower)) ) == TRUE){
      #
      #
      repeat.experiment_M[counter, 1] <- TAIR_SpurB$Accession[i]
      repeat.experiment_M[counter, 2] <- reference$ATG[j]
      repeat.experiment_M[counter, 3] <- reference$PROTEIN [j]
      repeat.experiment_M[counter, 4] <- reference$COMPLEX [j]
      
      counter = counter + 1
    }
  }
}

BETA_CROSSLINKED = repeat.experiment_M[complete.cases(repeat.experiment_M), ]
# View(INTERMEDIATE)
# saveRDS(CROSSLINK, file = "OLGA_INTERMEDIATE")

####### ----------------------------------------------------------------########
#####                                                                      #####
###                        SAVE FILE as xlsx                                 ###

#
library(xlsx)
colnames(CROSSLINK)  = c("MS-ATG", "reference-ATG" , "Protein" , "Complex")
colnames(NO_CROSSLINK) = c("MS-ATG", "reference-ATG" , "Protein" , "Complex")
colnames(INTERMEDIATE) = c("MS-ATG", "reference-ATG" , "Protein" , "Complex")

colnames(ALPHA_NON_CROSSLINKED) = c("MS-ATG", "reference-ATG" , "Protein" , "Complex")
colnames(BETA_CROSSLINKED) = c("MS-ATG", "reference-ATG" , "Protein" , "Complex")

# 
# write.xlsx(CROSSLINK, file = "C:/Users/andie/Dropbox/Mein PC (DESKTOP-4TB10SM)/Downloads/Pipeline/xlsx-Data/CROSSLINK_SPECIFIC.xlsx", sheetName = "Sheet1",
#             col.names = TRUE, row.names = TRUE, append = FALSE)
# 
# 
# write.xlsx2(NO_CROSSLINK, file = "C:/Users/andie/Dropbox/Mein PC (DESKTOP-4TB10SM)/Downloads/Pipeline/xlsx-Data/non_CROSSLINKED_SPECIFIC.xlsx", sheetName = "Sheet1",
#             col.names = TRUE, row.names = TRUE, append = FALSE)
# 
# 
# write.xlsx2(INTERMEDIATE, file = "C:/Users/andie/Dropbox/Mein PC (DESKTOP-4TB10SM)/Downloads/Pipeline/xlsx-Data/INTERMEDIATE_SPECIFIC.xlsx", sheetName = "Sheet1",
#             col.names = TRUE, row.names = TRUE, append = FALSE)
# 
# 
# write.xlsx2(ALPHA_NON_CROSSLINKED, file = "C:/Users/andie/Dropbox/Mein PC (DESKTOP-4TB10SM)/Downloads/Pipeline/xlsx-Data/non-crosslinked_all.xlsx", sheetName = "Sheet1",
#               col.names = TRUE, row.names = TRUE, append = FALSE)
# 
# write.xlsx2(BETA_CROSSLINKED, file = "C:/Users/andie/Dropbox/Mein PC (DESKTOP-4TB10SM)/Downloads/Pipeline/xlsx-Data/crosslinked_all.xlsx", sheetName = "Sheet1",
#             col.names = TRUE, row.names = TRUE, append = FALSE)
# 
library(RColorBrewer)
library(viridis)




pdf(file="./image/crosslink_specific.pdf")
pie(table(CROSSLINK$Complex), names(table(CROSSLINK$Complex)),init.angle =40  ,  clockwise = TRUE ,main = "crosslink_specific", radius = 1.02 , cex = 0.6, col = plasma(as.integer(nrow(table(CROSSLINK$Complex)))  ))
dev.off()
pdf(file="./image/non_crosslinked_specific.pdf")
pie(table(NO_CROSSLINK$Complex), names(table(NO_CROSSLINK$Complex)),clockwise = FALSE, main = "non-crosslinked_specific", radius = 1, cex = 0.4, col = plasma(as.integer(nrow(table(NO_CROSSLINK$Complex)))  )  )
dev.off()
pdf(file="./image/intermediate_specific.pdf")
intermediate_specific = pie(table(INTERMEDIATE$Complex) ,init.angle =10, clockwise =  FALSE, names(table(INTERMEDIATE$Complex)) , main = "intermediate_specific", radius = 1, cex = 0.4, col = plasma(as.integer(nrow(table(INTERMEDIATE$Complex)))  ) )
dev.off()

pdf(file="./image/non_crosslinked.pdf")
crosslinked = pie(table(ALPHA_NON_CROSSLINKED$Complex),init.angle =40,clockwise =  FALSE, names(table(ALPHA_NON_CROSSLINKED$Complex)), main = "non-crosslinked", radius = 1, cex = 0.4, col = plasma(as.integer(nrow(table(ALPHA_NON_CROSSLINKED$Complex)))  ))
dev.off()
pdf(file="./image/crosslinked.pdf")
non_crosslinked = pie(table(BETA_CROSSLINKED$Complex),init.angle =70,clockwise = FALSE, names(table(BETA_CROSSLINKED$Complex)) , main = "crosslinked" , radius = 1, cex = 0.4, col = plasma(as.integer(nrow(table(BETA_CROSSLINKED$Complex)))  ))
dev.off()
### ----------------------------------------------------------------------------
?pie
?dev.off

####### ----------------------------------------------------------------########
#####                                                                      #####
###                                                                          ###
###               DIFFERENCE IN M _ SCORE !! more than 2x ??                 ###
#####                                                                      #####

for ( i in 1:nrow(TAIR_SpurA)){
  if (lapply(TAIR_SpurB$Accession[i], tolower) == "At5g09860"   ){
    print("TREFFER")
  }
}





######   SUB PIE PLOTS 
# library(plotly)
# library(dplyr)
# 
# fig <- plot_ly()
# fig <- fig %>% add_pie(data = count(diamonds, cut), labels = ~cut, values = ~n,
#                        name = "Cut", domain = list(x = c(0, 0.4), y = c(0.4, 1)))
# fig <- fig %>% add_pie(data = count(diamonds, color), labels = ~color, values = ~n,
#                        name = "Color", domain = list(x = c(0.6, 1), y = c(0.4, 1)))
# fig <- fig %>% add_pie(data = count(diamonds, clarity), labels = ~clarity, values = ~n,
#                        name = "Clarity", domain = list(x = c(0.25, 0.75), y = c(0, 0.6)))
# fig <- fig %>% layout(title = "Pie Charts with Subplots", showlegend = F,
#                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
# 
# fig
# 
##############  ----------------------------------------------------------------
#
# ------------------------------------------------------------------------------

# taking a look at intermediate M-score comparison

for (i in 1:nrow(df_match1)){
  len1 = as.integer((nchar(df_match1$Scores[i])-11)/2)
  print( as.numeric(substr(df_match1$Scores[i],1,len1)))
  df_match1$Scores[i] = as.numeric(substr(df_match1$Scores[i],1,len1))
}

for (i in 1:nrow(df_match2)){
  len2 = as.integer((nchar(df_match2$Scores[i])-11)/2)
  print( as.numeric(substr(df_match2$Scores[i],1,len2)))
  df_match2$Scores[i] = as.numeric(substr(df_match2$Scores[i],1,len2))
}





#########
######
#####
# repeat.experiment_Score = data.frame(matrix(NA,400,6))
# counter = 1
# 
# for ( i in 1 : nrow(df_match1)){
#   for ( j in 1 : nrow(df_match2)){
#     if ( df_match1$Accession[i] == df_match2$Accession[j]){
#       if ( as.numeric(df_match1$Scores[i]) > as.numeric(df_match2$Scores[j])) {
#         difference = (as.numeric(df_match1$Scores[i]) / as.numeric(df_match2$Scores[j]) )
#         if ( difference >= 2){
#           
#           repeat.experiment_Score[counter, 1] <- "DF1_bigger" 
#           repeat.experiment_Score[counter, 2] <- df_match1$Accession[i]
#           repeat.experiment_Score[counter, 3] <- df_match2$Accession[j]
#           repeat.experiment_Score[counter, 4] <- df_match1$Scores[i]
#           repeat.experiment_Score[counter, 5] <- df_match2$Scores[j]
#           repeat.experiment_Score[counter, 6] <- difference
#           
#           counter = counter + 1
#       }}
#       else # dataframe2 is bigger than dataframe1
#         if ( difference >= 2){
#           repeat.experiment_Score[counter, 1] <- "DF2_bigger" 
#           repeat.experiment_Score[counter, 2] <- df_match1$Accession[j]
#           repeat.experiment_Score[counter, 3] <- df_match2$Accession[i]
#           repeat.experiment_Score[counter, 4] <- df_match1$Scores[j]
#           repeat.experiment_Score[counter, 5] <- df_match2$Scores[i]
#           repeat.experiment_Score[counter, 6] <- difference
#           counter = counter + 1
#     }}
#   }
# }



repeat.experiment_Score = data.frame(matrix(NA,850,6))
counter = 1

for ( i in 1 : nrow(df_match1)){
  for ( j in 1 : nrow(df_match2)){
    if ( df_match1$Accession[i] == df_match2$Accession[j]){
        difference = (as.numeric(df_match1$Scores[i]) / as.numeric(df_match2$Scores[j]) )

        repeat.experiment_Score[counter, 1] <- i
        repeat.experiment_Score[counter, 2] <- df_match1$Accession[i]
        repeat.experiment_Score[counter, 3] <- df_match2$Accession[j]
        repeat.experiment_Score[counter, 4] <- df_match1$Scores[i]
        repeat.experiment_Score[counter, 5] <- df_match2$Scores[j]
        repeat.experiment_Score[counter, 6] <- difference

        counter = counter + 1
        }
  }
}


Score_comparison = repeat.experiment_Score[complete.cases(repeat.experiment_Score), ]
colnames(Score_comparison)  = c("Bigger","ATG_bigger", "ATG_smaller" , "Score_1" , "Score_2" , "difference")
Score_comparison$Score_1 = as.numeric(Score_comparison$Score_1)
Score_comparison$Score_2 = as.numeric(Score_comparison$Score_2)


pairs(log(Score_comparison[5:6]) ,col = col)
summary(Score_comparison)
str(Score_comparison)
Score_comparison

for (s in nrow(Score_comparison)) {
  col = ifelse(Score_comparison$difference <= 1.3, "gray", "orange")
  col[which(Score_comparison$difference >= 2)] = "firebrick1"
  col[which(Score_comparison$difference >= 5)] = "firebrick"
  col[which(Score_comparison$difference <= 0.7)] = "turquoise1"
  col[which(Score_comparison$difference <= 0.5 )] = "turquoise4"
  col[which(Score_comparison$difference <= 0.4 )] = "blue"
  
  pairs(sqrt(Score_comparison[4:6]),col=col, pch=19,  main = "Olga Rudi crosslinked non-crosslinked", labels = c("crosslinked", "non-crosslinked", "difference"))
}

##############  ----------------------------------------------------------------
#
# ------------------------------------------------------------------------------
#
# ------------------------------------------------------------------------------


repeat.experiment_Score1 = data.frame(matrix(NA,800,6))
repeat.experiment_Score2 = data.frame(matrix(NA,800,6))

counter = 1

for ( i in 1 : nrow(df_match1)){
  for ( j in 1 : nrow(df_match2)){
    if ( df_match1$Accession[i] == df_match2$Accession[j]){
      if ( as.numeric(df_match1$Scores[i]) > as.numeric(df_match2$Scores[j])) {
        difference = (as.numeric(df_match1$Scores[i]) / as.numeric(df_match2$Scores[j]) )

        repeat.experiment_Score1[counter, 1] <- i
        repeat.experiment_Score1[counter, 2] <- df_match1$Accession[i]
        repeat.experiment_Score1[counter, 3] <- df_match2$Accession[j]
        repeat.experiment_Score1[counter, 4] <- df_match1$Scores[i]
        repeat.experiment_Score1[counter, 5] <- df_match2$Scores[j]
        repeat.experiment_Score1[counter, 6] <- difference

                counter = counter + 1
      }
      else # dataframe2 is bigger than dataframe1
        difference = (as.numeric(df_match1$Scores[i]) / as.numeric(df_match2$Scores[j]) )

        repeat.experiment_Score2[counter, 1] <- i
        repeat.experiment_Score2[counter, 2] <- df_match1$Accession[j]
        repeat.experiment_Score2[counter, 3] <- df_match2$Accession[i]
        repeat.experiment_Score2[counter, 4] <- df_match1$Scores[j]
        repeat.experiment_Score2[counter, 5] <- df_match2$Scores[i]
        repeat.experiment_Score2[counter, 6] <- difference

        counter = counter + 1

    }
  }
}


dat1 = repeat.experiment_Score1[complete.cases(repeat.experiment_Score1), ]
dat2 = repeat.experiment_Score2[complete.cases(repeat.experiment_Score2), ]
colnames(dat1)  = c("Bigger","ATG_bigger", "ATG_smaller" , "Score_1" , "Score_2" , "difference")
colnames(dat2)  = c("Bigger","ATG_bigger", "ATG_smaller" , "Score_1" , "Score_2" , "difference")

dat1$Score_1 = as.numeric(dat1$Score_1)
dat1$Score_2 = as.numeric(dat1$Score_2)
dat2$Score_1 = as.numeric(dat2$Score_1)
dat2$Score_2 = as.numeric(dat2$Score_2)


dat1
dat2

data = dat1[4]
head(data) 
for (s in nrow(Score_comparison)) {
  col = ifelse(Score_comparison$difference <= 1.3, "gray", "orange")
  col[which(Score_comparison$difference >= 2)] = "firebrick1"
  col[which(Score_comparison$difference >= 5)] = "firebrick"
  col[which(Score_comparison$difference <= 0.7)] = "turquoise1"
  col[which(Score_comparison$difference <= 0.5 )] = "turquoise4"
  col[which(Score_comparison$difference <= 0.4 )] = "blue"
  
  pairs((Score_comparison[4:5]),col=col, pch=19,  main = "Olga Rudi crosslinked non-crosslinked", labels = c("crosslinked", "non-crosslinked", "difference"))
}




repeat.experiment_B = data.frame(matrix(NA,200,4))
counter = 1

for ( i in 1: nrow(Score_comparison)){
  for (j in 1: nrow(reference)){
    if ( Score_comparison$difference[j] >= 2){
      if (str_detect( as.character( lapply(Score_comparison$ATG_bigger[i], tolower)) ,  as.character( lapply(reference$ATG[j], tolower)) ) == TRUE ){
        print(Score_comparison$difference[j])
        repeat.experiment_B[counter, 1] <- Score_comparison$ATG_bigger[i]
        repeat.experiment_B[counter, 2] <- reference$ATG[j]
        repeat.experiment_B[counter, 3] <- reference$PROTEIN [j]
        repeat.experiment_B[counter, 4] <- reference$COMPLEX [j]

        counter = counter + 1
    }}
  }
}

CROSSLINK = repeat.experiment_B[complete.cases(repeat.experiment_B), ]
CROSSLINK

# date = "2021-06-09"
# type = c("_150-NaCl", "_150-Manitol", "_controll" , "_300-Manitol" , "_heat", "_200-Manitol" )
# genotype = c("_Col-0", "_Vir-1" , "_fip-37"   , "_4b-1" , "_4b-2", "_4b-4")
# group = c("_1" , "_2" , "_3")
# 
# 
# 
# old_files <- list.files("C:/Users/andie/pictures/uni/04.06.2021", pattern = "*.JPG", full.names = TRUE)
# old_files
# 
# new_files <- paste0("C:/Users/andie/pictures/uni/04.06.2021",".JPG")
# new_files
# 
# list = list()
# for (i in 1 : 1){  
#   for (j in 1 : 4){   ### genotype 
#     for (k in 1 : 6 ){
#       for (m in 1: 3){
#         
#         list = c(list,paste0(date, type[j], genotype[k], group[m],".JPG", sep = "") )
#         
#       }
#       
#   }}
# }
# list
# 
# for (i in 1 : length(old_files)){  
#   new_files[i] <- paste0("C:/Users/andie/pictures/uni/04.06.2021/",list[i],".JPG")
#   
# }
# new_files
# 
# 
# file.copy(from = old_files, to = new_files)
###
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
