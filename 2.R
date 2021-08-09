# 1. data loading ---------------------------------------------------------







data <- read.csv(file = "file:///C:/Users/Administrator/OneDrive/Uni/Master Biology/Masterarbeit/Results/Phenotyping/phenotyping_20201118.csv")
data <- read_excel("C:/Users/andie/Downloads/Phenotyping_Excel_sheets.xlsx")


str(data)

#also load required packages

library(ggplot2)

library(multcompView)

library(plyr)

library(gridExtra)

library(dplyr)





# 2. preview of all data and replicates -----------------------------------

#Here, no statistical analysis occurs, this section give you a blank overview over all your data

#in form of Boxplots, sorted according to Genotypes and Replicates

#--> Here you could identify outliers and subset your data according your individual needs afterwards.



tob <- ggplot(data, aes(x=line, y=tob, fill = rep))+ #x= the groups you want on your x-axis, y=the variable you want to plot, fill=the differnt groups, here replicates 
  
  geom_boxplot(alpha=0.7) + #put here all "aestetic parameters, alpha =transparency
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Ssrp1", "TfIIs", "Cdc73", "Spt16xElf1", "Ssrp1xElf1", "TfIIsxElf1", "Cdc73xElf1")) + #name = which labeling on the x scale, limits= the order you want
  
  scale_y_continuous(name= "time [days]", limit = c(17,30)) + #name=s.a, limit= the range of your y-axis
  
  ggtitle("Time of bolting") + #title
  
  theme_bw() + #the backgroundcolour off your plot ()= transparent, theme= font, size and position of the labeling
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"), #hjust = position of the title, 0.5=middle, face=special font format
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face = "bold" ),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        
        legend.position ="none")+
  
  scale_fill_brewer(palette = "Accent") + #your colours
  
  labs(fill = "Replicate") #the labeling of your legend

tob



rdb <- ggplot(data, aes(x= line, y=rdb, fill = rep))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Ssrp1", "TfIIs", "Cdc73", "Spt16xElf1", "Ssrp1xElf1", "TfIIsxElf1", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "diameter [mm]", limit = c(20,100)) +
  
  ggtitle("Rosette diameter at bolting") +
  
  theme_bw() +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face = "bold" ),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        
        legend.position ="none")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate")

rdb



leaves <- ggplot(data, aes(x= line, y=leaves, fill = rep))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Ssrp1", "TfIIs", "Cdc73", "Spt16xElf1", "Ssrp1xElf1", "TfIIsxElf1", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "number", limit = c(5,17)) +
  
  ggtitle("Number of leaves at bolting") +
  
  theme_bw() +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face = "bold" ),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        
        legend.position ="none")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate")

leaves



rd21 <- ggplot(data, aes(x= line, y=rd21, fill = rep))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Ssrp1", "TfIIs", "Cdc73", "Spt16xElf1", "Ssrp1xElf1", "TfIIsxElf1", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "diameter [mm]", limit = c(0,90)) +
  
  ggtitle("Rosette diameter at DAS 21") +
  
  theme_bw() +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face = "bold" ),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        
        legend.position ="none")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate")

rd21



rd28 <- ggplot(data, aes(x= line, y=rd28, fill = rep))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Ssrp1", "TfIIs", "Cdc73", "Spt16xElf1", "Ssrp1xElf1", "TfIIsxElf1", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "diameter [mm]", limit = c(0,150)) +
  
  ggtitle("Rosette diameter at DAS 28") +
  
  theme_bw() +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face = "bold" ),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        
        legend.position ="none")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate")

rd28



rd35 <- ggplot(data, aes(x= line, y=rd35, fill = rep))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Ssrp1", "TfIIs", "Cdc73", "Spt16xElf1", "Ssrp1xElf1", "TfIIsxElf1", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "diameter [mm]", limit = c(0,150)) +
  
  ggtitle("Rosette diameter at DAS 35") +
  
  theme_bw() +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face = "bold" ),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        
        legend.position ="none")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate")

rd35



pI <- ggplot(data, aes(x= line, y=pI, fill = rep))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Ssrp1", "TfIIs", "Cdc73", "Spt16xElf1", "Ssrp1xElf1", "TfIIsxElf1", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "number", limit = c(0,10)) +
  
  ggtitle("primary inflorescence") +
  
  theme_bw() +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face = "bold" ),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        
        legend.position ="none")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate")

pI



sI <- ggplot(data, aes(x= line, y=sI, fill = rep))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Ssrp1", "TfIIs", "Cdc73", "Spt16xElf1", "Ssrp1xElf1", "TfIIsxElf1", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "number", limit = c(0,10)) +
  
  ggtitle("secondary inflorescence") +
  
  theme_bw() +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face = "bold" ),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        
        legend.position ="none")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate")

sI



height42 <- ggplot(data, aes(x= line, y=height42, fill = rep))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Ssrp1", "TfIIs", "Cdc73", "Spt16xElf1", "Ssrp1xElf1", "TfIIsxElf1", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "number", limit = c(0,500)) +
  
  ggtitle("height at DAS 42") +
  
  theme_bw() +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face = "bold" ),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        
        legend.position ="none")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate")

height42



height49 <- ggplot(data, aes(x= line, y=height49, fill = rep))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Ssrp1", "TfIIs", "Cdc73", "Spt16xElf1", "Ssrp1xElf1", "TfIIsxElf1", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "height [mm]", limit = c(200,600)) +
  
  ggtitle("Final height") +
  
  theme_bw() +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face = "bold" ),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        
        legend.position ="none")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate")

height49





pdf(file="Boxplots_Reps.pdf", height = 14, width = 10)

grid.arrange(rdb, tob, leaves,
             
             rd21, rd28, rd35,
             
             pI, sI, height42, 
             
             height49, ncol=3)

dev.off()





# 3. PCA - comparision of all data --> to identify replicates as outlier ----------------------------------------

#Not working! So far



str(data)



df_data_rep <- as.numeric(data$rep)

df_data_line <- as.numeric(data$line)

df_data_tob <- as.numeric(data$tob)

df_data_rdb <- as.numeric(data$rdb)

df_data_leaves <- as.numeric(data$leaves)

df_data_rd21 <- as.numeric(data$rd21)

df_data_rd28 <- as.numeric(data$rd28)

df_data_rd35 <- as.numeric(data$rd35)

df_data_pI <- as.numeric(data$pI)

df_data_sI <- as.numeric(data$sI)

df_data_height42 <- as.numeric(data$height42)

df_data_height49 <- as.numeric(data$height49)

df_data <- as.data.frame(cbind(df_data_rep, df_data_line, df_data_tob, df_data_rdb, df_data_leaves, df_data_rd21, df_data_rd28, df_data_rd35, df_data_pI, df_data_sI, df_data_height42, df_data_height49)) 

str(df_data)

apply(data[,3:12], 2, var, na.rm=TRUE)

scaled_data <- apply(data[,3:12], 2, scale, na.rm=TRUE)

scaled_data <- scale(df_data, center = TRUE, scale = apply(data[,3:12], 12, sd, na.rm = TRUE))

p <- prcomp(na.omit(df_data),center = TRUE, scale=TRUE)







# 4. Subsetting data ------------------------------------------------------

#Only necessary if you have multiple Replicates and Genotypes, that you want to analyze distinct from each other





#If you want to analyse a specific replicate

rd_data <- data[data$rep=="C", ]

Col_0 <- rd_data[rd_data$line=="Col_0",]

Spt16 <- rd_data[rd_data$line == "Spt16",]

TfIIs <- rd_data[rd_data$line=="TfIIs",]

Cdc73 <- rd_data[rd_data$line=="Cdc73",]

Elf1 <- rd_data[rd_data$line=="Elf1",]

Spt16xElf1 <- rd_data[rd_data$line=="Spt16xElf1",]

TfIIsxElf1 <- rd_data[rd_data$line=="TfIIsxElf1",]

Cdc73xElf1 <- rd_data[rd_data$line=="Cdc73xElf1",]

new_data_Spt16 <- rbind(Col_0, Spt16, Elf1, Spt16xElf1)

new_data_TfIIs <- rbind(Col_0, TfIIs, Elf1, TfIIsxElf1)

new_data_Cdc73 <- rbind(Col_0, Cdc73, Elf1, Cdc73xElf1)







#If you want to inlude all replicates but only a subset of genotypes

Col_0 <- data[data$line=="Col_0",]

Spt16 <- data[data$line == "Spt16",]

Ssrp1 <- data[data$line == "Ssrp1",]

TfIIs <- data[data$line=="TfIIs",]

Cdc73 <- data[data$line=="Cdc73",]

Iws1 <- data[data$line == "Iws1",]

Elf1 <- data[data$line=="Elf1",]

Spt16xElf1 <- data[data$line=="Spt16xElf1",]

Ssrp1xElf1 <- data[data$line=="Ssrp1xElf1",]

TfIIsxElf1 <- data[data$line=="TfIIsxElf1",]

Cdc73xElf1 <- data[data$line=="Cdc73xElf1",]

Iws1xElf1 <- data[data$line=="Iws1xElf1",]

new_data_Spt16 <- rbind(Col_0, Spt16, Elf1, Spt16xElf1)

new_data_Ssrp1 <- rbind(Col_0, Ssrp1, Elf1, Ssrp1xElf1)

new_data_TfIIs <- rbind(Col_0, TfIIs, Elf1, TfIIsxElf1)

new_data_Cdc73 <- rbind(Col_0, Cdc73, Elf1, Cdc73xElf1)

new_data_Iws1 <- rbind(Col_0, Iws1, Elf1, Iws1xElf1)







# 5. One way Anova and Tukey ----------------------------------------------







# 5.1 Time of bolting -----------------------------------------------------





ANOVA <- aov(new_data_Spt16$tob ~ new_data_Spt16$line)

summary(ANOVA) # I have a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Spt16$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_tob_Spt16.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Spt16, flev, function (x) max(fivenum(x$tob)) + 3)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_tob_Spt16 <- ggplot(new_data_Spt16, aes(x= line, y=tob))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Spt16xElf1")) + 
  
  scale_y_continuous(name= "Time in Days", limit = c(15,35)) +
  
  ggtitle("Time of bolting") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Spt16$line'), aes(x = plot.labels, y = V1, label = labels))

t_tob_Spt16



###########################################################################################



ANOVA <- aov(new_data_Ssrp1$tob ~ new_data_Ssrp1$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Ssrp1$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_tob_Ssrp1.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Ssrp1, flev, function (x) max(fivenum(x$tob)) + 3)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_tob_Ssrp1 <- ggplot(new_data_Ssrp1, aes(x= line, y=tob))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Ssrp1", "Ssrp1xElf1")) + 
  
  scale_y_continuous(name= "Time in Days", limit = c(15,35)) +
  
  ggtitle("Time of bolting") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Ssrp1$line'), aes(x = plot.labels, y = V1, label = labels))

t_tob_Ssrp1
new_data_Ssrp1$


##############################################################################################



ANOVA <- aov(new_data_TfIIs$tob ~ new_data_TfIIs$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_TfIIs$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_tob_TfIIs.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_TfIIs, flev, function (x) max(fivenum(x$tob)) + 3)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_tob_TfIIs <- ggplot(new_data_TfIIs, aes(x= line, y=tob))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "TfIIs", "TfIIsxElf1")) + 
  
  scale_y_continuous(name= "Time in Days", limit = c(15,35)) +
  
  ggtitle("Time of bolting") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_TfIIs$line'), aes(x = plot.labels, y = V1, label = labels))

t_tob_TfIIs



#######################################################################################



ANOVA <- aov(new_data_Cdc73$tob ~ new_data_Cdc73$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Cdc73$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_tob_Cdc73.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels, reversed = T)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Cdc73, flev, function (x) max(fivenum(x$tob)) + 3)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_tob_Cdc73 <- ggplot(new_data_Cdc73, aes(x= line, y=tob))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Cdc73", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "Time in Days", limit = c(15,35)) +
  
  ggtitle("Time of bolting") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Cdc73$line'), aes(x = plot.labels, y = V1, label = labels))

t_tob_Cdc73





# 5.2 Rosette Diameter at bolting -----------------------------------------







ANOVA <- aov(new_data_Spt16$rdb ~ new_data_Spt16$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Spt16$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rdb_Spt16.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Spt16, flev, function (x) max(fivenum(x$rdb)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rdb_Spt16 <- ggplot(new_data_Spt16, aes(x= line, y=rdb))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Spt16xElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(10,120)) +
  
  ggtitle("Rosette Diameter at Bolting") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Spt16$line'), aes(x = plot.labels, y = V1, label = labels))

t_rdb_Spt16



###########################################################################################



ANOVA <- aov(new_data_Ssrp1$rdb ~ new_data_Ssrp1$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Ssrp1$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rdb_Ssrp1.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Ssrp1, flev, function (x) max(fivenum(x$rdb)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rdb_Ssrp1 <- ggplot(new_data_Ssrp1, aes(x= line, y=rdb))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Ssrp1", "Ssrp1xElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(10,120)) +
  
  ggtitle("Rosette Diameter at Bolting") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Ssrp1$line'), aes(x = plot.labels, y = V1, label = labels))

t_rdb_Ssrp1



##############################################################################################



ANOVA <- aov(new_data_TfIIs$rdb ~ new_data_TfIIs$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_TfIIs$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rdb_TfIIs.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_TfIIs, flev, function (x) max(fivenum(x$rdb)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rdb_TfIIs <- ggplot(new_data_TfIIs, aes(x= line, y=rdb))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "TfIIs", "TfIIsxElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(10,120)) +
  
  ggtitle("Rosette Diameter at Bolting") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_TfIIs$line'), aes(x = plot.labels, y = V1, label = labels))

t_rdb_TfIIs



#######################################################################################



ANOVA <- aov(new_data_Cdc73$rdb ~ new_data_Cdc73$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Cdc73$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rdb_Cdc73.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels, reversed = T)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Cdc73, flev, function (x) max(fivenum(x$rdb)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rdb_Cdc73 <- ggplot(new_data_Cdc73, aes(x= line, y=rdb))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Cdc73", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(10,120)) +
  
  ggtitle("Rosette Diameter at Bolting") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Cdc73$line'), aes(x = plot.labels, y = V1, label = labels))

t_rdb_Cdc73





# 5.3 leaves at bolting ---------------------------------------------------







ANOVA <- aov(new_data_Spt16$leaves ~ new_data_Spt16$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Spt16$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_leaves_Spt16.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Spt16, flev, function (x) max(fivenum(x$leaves)) + 2)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_leaves_Spt16 <- ggplot(new_data_Spt16, aes(x= line, y=leaves))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Spt16xElf1")) + 
  
  scale_y_continuous(name= "Number of Leaves", limit = c(5,18)) +
  
  ggtitle("Leaves at Bolting") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Spt16$line'), aes(x = plot.labels, y = V1, label = labels))

t_leaves_Spt16



##############################################################################################



ANOVA <- aov(new_data_Ssrp1$leaves ~ new_data_Ssrp1$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Ssrp1$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_leaves_Ssrp1.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Ssrp1, flev, function (x) max(fivenum(x$leaves)) + 2)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_leaves_Ssrp1 <- ggplot(new_data_Ssrp1, aes(x= line, y=leaves))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Ssrp1", "Ssrp1xElf1")) + 
  
  scale_y_continuous(name= "Number of Leaves", limit = c(5,21)) +
  
  ggtitle("Leaves at Bolting") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Ssrp1$line'), aes(x = plot.labels, y = V1, label = labels))

t_leaves_Ssrp1



###################################################################################################



ANOVA <- aov(new_data_TfIIs$leaves ~ new_data_TfIIs$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_TfIIs$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_leaves_TfIIs.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_TfIIs, flev, function (x) max(fivenum(x$leaves)) + 2)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_leaves_TfIIs <- ggplot(new_data_TfIIs, aes(x= line, y=leaves))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "TfIIs", "TfIIsxElf1")) + 
  
  scale_y_continuous(name= "Number of Leaves", limit = c(5,18)) +
  
  ggtitle("Leaves at Bolting") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_TfIIs$line'), aes(x = plot.labels, y = V1, label = labels))

t_leaves_TfIIs



#########################################################################################################



ANOVA <- aov(new_data_Cdc73$leaves ~ new_data_Cdc73$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Cdc73$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_leaves_Cdc73.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels, reversed = T)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Cdc73, flev, function (x) max(fivenum(x$leaves)) + 2)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_leaves_Cdc73 <- ggplot(new_data_Cdc73, aes(x= line, y=leaves))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Cdc73", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "Number of Leaves", limit = c(5,18)) +
  
  ggtitle("Leaves at Bolting") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Cdc73$line'), aes(x = plot.labels, y = V1, label = labels))

t_leaves_Cdc73





# 5.4 rosette diameter at DAS21 -------------------------------------------





ANOVA <- aov(new_data_Spt16$rd21 ~ new_data_Spt16$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Spt16$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rd21_Spt16.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Spt16, flev, function (x) max(fivenum(x$rd21)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rd21_Spt16 <- ggplot(new_data_Spt16, aes(x= line, y=rd21))+
  
  geom_boxplot() + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Spt16xElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(10,120)) +
  
  ggtitle("Rosette Diameter  [DAS21]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Spt16$line'), aes(x = plot.labels, y = V1, label = labels))

t_rd21_Spt16



######################################################################################################



ANOVA <- aov(new_data_Ssrp1$rd21 ~ new_data_Ssrp1$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Ssrp1$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rd21_Ssrp1.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Ssrp1, flev, function (x) max(fivenum(x$rd21)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rd21_Ssrp1 <- ggplot(new_data_Ssrp1, aes(x= line, y=rd21))+
  
  geom_boxplot() + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Ssrp1", "Ssrp1xElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(10,130)) +
  
  ggtitle("Rosette Diameter  [DAS21]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Ssrp1$line'), aes(x = plot.labels, y = V1, label = labels))

t_rd21_Ssrp1



######################################################################################################



ANOVA <- aov(new_data_TfIIs$rd21 ~ new_data_TfIIs$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_TfIIs$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rd21_TfIIs.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels, reversed = T)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_TfIIs, flev, function (x) max(fivenum(x$rd21)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rd21_TfIIs <- ggplot(new_data_TfIIs, aes(x= line, y=rd21))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "TfIIs", "TfIIsxElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(10,130)) +
  
  ggtitle("Rosette Diameter  [DAS21]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_TfIIs$line'), aes(x = plot.labels, y = V1, label = labels))

t_rd21_TfIIs



##########################################################################################################



ANOVA <- aov(new_data_Cdc73$rd21 ~ new_data_Cdc73$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Cdc73$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rd21_Cdc73.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels, reversed = T)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Cdc73, flev, function (x) max(fivenum(x$rd21)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rd21_Cdc73 <- ggplot(new_data_Cdc73, aes(x= line, y=rd21))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Cdc73", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(10,120)) +
  
  ggtitle("Rosette Diameter  [DAS21]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Cdc73$line'), aes(x = plot.labels, y = V1, label = labels))

t_rd21_Cdc73







# 5.5 Rosette diameter at DAS28 -------------------------------------------







ANOVA <- aov(new_data_Spt16$rd28 ~ new_data_Spt16$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Spt16$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rd28_Spt16.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Spt16, flev, function (x) max(fivenum(x$rd28)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rd28_Spt16 <- ggplot(new_data_Spt16, aes(x= line, y=rd28))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Spt16xElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(30,150)) +
  
  ggtitle("Rosette Diameter  [DAS28]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Spt16$line'), aes(x = plot.labels, y = V1, label = labels))

t_rd28_Spt16



###############################################################################################



ANOVA <- aov(new_data_Ssrp1$rd28 ~ new_data_Ssrp1$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Ssrp1$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rd28_Ssrp1.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Ssrp1, flev, function (x) max(fivenum(x$rd28)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rd28_Ssrp1 <- ggplot(new_data_Ssrp1, aes(x= line, y=rd28))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Ssrp1", "Ssrp1xElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(30,150)) +
  
  ggtitle("Rosette Diameter  [DAS28]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Ssrp1$line'), aes(x = plot.labels, y = V1, label = labels))

t_rd28_Ssrp1



################################################################################################



ANOVA <- aov(new_data_TfIIs$rd28 ~ new_data_TfIIs$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_TfIIs$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rd28_TfIIs.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_TfIIs, flev, function (x) max(fivenum(x$rd28)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rd28_TfIIs <- ggplot(new_data_TfIIs, aes(x= line, y=rd28))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "TfIIs", "TfIIsxElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(30,150)) +
  
  ggtitle("Rosette Diameter  [DAS28]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_TfIIs$line'), aes(x = plot.labels, y = V1, label = labels))

t_rd28_TfIIs



#####################################################################################################



ANOVA <- aov(new_data_Cdc73$rd28 ~ new_data_Cdc73$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Cdc73$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rd28_Cdc73.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels, reversed = TRUE)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Cdc73, flev, function (x) max(fivenum(x$rd28)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rd28_Cdc73 <- ggplot(new_data_Cdc73, aes(x= line, y=rd28))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Cdc73", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(30,150)) +
  
  ggtitle("Rosette Diameter  [DAS28]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Cdc73$line'), aes(x = plot.labels, y = V1, label = labels))

t_rd28_Cdc73







# 5.6 Rosette diameter at DAS35 -------------------------------------------







ANOVA <- aov(new_data_Spt16$rd35 ~ new_data_Spt16$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Spt16$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rd35_Spt16.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Spt16, flev, function (x) max(fivenum(x$rd35)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rd35_Spt16 <- ggplot(new_data_Spt16, aes(x= line, y=rd35))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Spt16xElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(30,190)) +
  
  ggtitle("Rosette Diameter  [DAS35]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Spt16$line'), aes(x = plot.labels, y = V1, label = labels))

t_rd35_Spt16



###############################################################################################



ANOVA <- aov(new_data_Ssrp1$rd35 ~ new_data_Ssrp1$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Ssrp1$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rd35_Ssrp1.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Ssrp1, flev, function (x) max(fivenum(x$rd35)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rd35_Ssrp1 <- ggplot(new_data_Ssrp1, aes(x= line, y=rd35))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Ssrp1", "Ssrp1xElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(30,200)) +
  
  ggtitle("Rosette Diameter  [DAS35]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Ssrp1$line'), aes(x = plot.labels, y = V1, label = labels))

t_rd35_Ssrp1



################################################################################################



ANOVA <- aov(new_data_TfIIs$rd35 ~ new_data_TfIIs$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_TfIIs$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rd35_TfIIs.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_TfIIs, flev, function (x) max(fivenum(x$rd35)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rd35_TfIIs <- ggplot(new_data_TfIIs, aes(x= line, y=rd35))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "TfIIs", "TfIIsxElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(30,150)) +
  
  ggtitle("Rosette Diameter  [DAS35]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_TfIIs$line'), aes(x = plot.labels, y = V1, label = labels))

t_rd35_TfIIs



#####################################################################################################



ANOVA <- aov(new_data_Cdc73$rd35 ~ new_data_Cdc73$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Cdc73$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_rd35_Cdc73.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels, reversed = TRUE)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Cdc73, flev, function (x) max(fivenum(x$rd35)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_rd35_Cdc73 <- ggplot(new_data_Cdc73, aes(x= line, y=rd35))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Cdc73", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "Diameter [mm]", limit = c(30,200)) +
  
  ggtitle("Rosette Diameter  [DAS35]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Cdc73$line'), aes(x = plot.labels, y = V1, label = labels))

t_rd35_Cdc73





# 5.7 primary Inflrescence DAS42 ------------------------------------------



ANOVA <- aov(new_data_Spt16$pI ~ new_data_Spt16$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Spt16$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_pI_Spt16.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Spt16, flev, function (x) max(fivenum(x$pI)) + 2)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_pI_Spt16 <- ggplot(new_data_Spt16, aes(x= line, y=pI))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Spt16xElf1")) + 
  
  scale_y_continuous(name= "number", limit = c(0,10)) +
  
  ggtitle("Primary inflorescences [DAS42]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Spt16$line'), aes(x = plot.labels, y = V1, label = labels))

t_pI_Spt16



###############################################################################################



ANOVA <- aov(new_data_Ssrp1$pI ~ new_data_Ssrp1$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Ssrp1$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_pI_Ssrp1.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Ssrp1, flev, function (x) max(fivenum(x$pI)) + 2)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_pI_Ssrp1 <- ggplot(new_data_Ssrp1, aes(x= line, y=pI))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Ssrp1", "Ssrp1xElf1")) + 
  
  scale_y_continuous(name= "number", limit = c(0,10)) +
  
  ggtitle("Primary inflorescences [DAS42]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Ssrp1$line'), aes(x = plot.labels, y = V1, label = labels))

t_pI_Ssrp1



################################################################################################



ANOVA <- aov(new_data_TfIIs$pI ~ new_data_TfIIs$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_TfIIs$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_pI_TfIIs.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_TfIIs, flev, function (x) max(fivenum(x$pI)) + 2)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_pI_TfIIs <- ggplot(new_data_TfIIs, aes(x= line, y=pI))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "TfIIs", "TfIIsxElf1")) + 
  
  scale_y_continuous(name= "number", limit = c(0,10)) +
  
  ggtitle("Primary inflorescences [DAS42]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_TfIIs$line'), aes(x = plot.labels, y = V1, label = labels))

t_pI_TfIIs



#####################################################################################################



ANOVA <- aov(new_data_Cdc73$pI ~ new_data_Cdc73$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Cdc73$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_pI_Cdc73.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Cdc73, flev, function (x) max(fivenum(x$pI)) + 2)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_pI_Cdc73 <- ggplot(new_data_Cdc73, aes(x= line, y=pI))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Cdc73", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "number", limit = c(0,10)) +
  
  ggtitle("Primary inflorescences [DAS42]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Cdc73$line'), aes(x = plot.labels, y = V1, label = labels))

t_pI_Cdc73



# 5.8 secondary inflorescences DAS42 --------------------------------------



ANOVA <- aov(new_data_Spt16$sI ~ new_data_Spt16$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Spt16$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_sI_Spt16.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Spt16, flev, function (x) max(fivenum(x$sI)) + 2)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_sI_Spt16 <- ggplot(new_data_Spt16, aes(x= line, y=sI))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Spt16xElf1")) + 
  
  scale_y_continuous(name= "number", limit = c(0,10)) +
  
  ggtitle("secondary inflorescences [DAS42]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Spt16$line'), aes(x = plot.labels, y = V1, label = labels))

t_sI_Spt16



###############################################################################################



ANOVA <- aov(new_data_Ssrp1$sI ~ new_data_Ssrp1$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Ssrp1$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_sI_Ssrp1.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Ssrp1, flev, function (x) max(fivenum(x$sI)) + 2)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_sI_Ssrp1 <- ggplot(new_data_Ssrp1, aes(x= line, y=sI))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Ssrp1", "Ssrp1xElf1")) + 
  
  scale_y_continuous(name= "number", limit = c(0,10)) +
  
  ggtitle("Primary inflorescences [DAS42]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Ssrp1$line'), aes(x = plot.labels, y = V1, label = labels))

t_sI_Ssrp1



################################################################################################



ANOVA <- aov(new_data_TfIIs$sI ~ new_data_TfIIs$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_TfIIs$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_sI_TfIIs.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_TfIIs, flev, function (x) max(fivenum(x$sI)) + 2)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_sI_TfIIs <- ggplot(new_data_TfIIs, aes(x= line, y=sI))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "TfIIs", "TfIIsxElf1")) + 
  
  scale_y_continuous(name= "number", limit = c(0,10)) +
  
  ggtitle("Primary inflorescences [DAS42]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_TfIIs$line'), aes(x = plot.labels, y = V1, label = labels))

t_sI_TfIIs



#####################################################################################################



ANOVA <- aov(new_data_Cdc73$sI ~ new_data_Cdc73$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Cdc73$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_sI_Cdc73.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
    # Extract labels and factor levels from Tukey post-hoc 
  test.levels <- HSD[[flev]][,4]
  print(test.levels)
  test.labels <- multcompLetters(test.levels)['Letters']
  #I need to put the labels in the same order as in the boxplot 
  print(test.labels)
  plot.labels <- names(test.labels[['Letters']])
  print(plot.labels)
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  # upper quantile and label placement
  boxplot.df <- ddply(new_data_Cdc73, flev, function (x) max(fivenum(x$sI)) + 2)
  print(boxplot.df)
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            stringsAsFactors = FALSE)
  print(plot.levels)
  
  # Merge it with the labels
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  print(labels.df)
  return(labels.df)
}



#Draw a basic boxplot

t_sI_Cdc73 <- ggplot(new_data_Cdc73, aes(x= line, y=sI))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("Col_0", "Elf1", "Cdc73", "Cdc73xElf1")) + 
  scale_y_continuous(name= "number", limit = c(0,10)) +
  ggtitle("Secondary inflorescences [DAS42]") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        legend.position ="bottom")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate") +
  geom_text(data = generate_label_df(TUKEY, 'new_data_Cdc73$line'), aes(x = plot.labels, y = V1, label = labels))

t_sI_Cdc73



# 5.9 height DAS42 --------------------------------------------------------





ANOVA <- aov(new_data_Spt16$height42 ~ new_data_Spt16$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Spt16$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_height42_Spt16.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Spt16, flev, function (x) max(fivenum(x$height42)) + 50)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_height42_Spt16 <- ggplot(new_data_Spt16, aes(x= line, y=height42))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Spt16xElf1")) + 
  
  scale_y_continuous(name= "height [mm]", limit = c(70,500)) +
  
  ggtitle("Plant height [DAS42]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Spt16$line'), aes(x = plot.labels, y = V1, label = labels))

t_height42_Spt16



###############################################################################################



ANOVA <- aov(new_data_Ssrp1$height42 ~ new_data_Ssrp1$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Ssrp1$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_height42_Ssrp1.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Ssrp1, flev, function (x) max(fivenum(x$height42)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_height42_Ssrp1 <- ggplot(new_data_Ssrp1, aes(x= line, y=height42))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Ssrp1", "Ssrp1xElf1")) + 
  
  scale_y_continuous(name= "height [mm]", limit = c(30,150)) +
  
  ggtitle("Plant height [DAS42]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Ssrp1$line'), aes(x = plot.labels, y = V1, label = labels))

t_height42_Ssrp1



################################################################################################



ANOVA <- aov(new_data_TfIIs$height42 ~ new_data_TfIIs$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_TfIIs$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_height42_TfIIs.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_TfIIs, flev, function (x) max(fivenum(x$height42)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_height42_TfIIs <- ggplot(new_data_TfIIs, aes(x= line, y=height42))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "TfIIs", "TfIIsxElf1")) + 
  
  scale_y_continuous(name= "height [mm]", limit = c(30,150)) +
  
  ggtitle("Plant height [DAS42]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_TfIIs$line'), aes(x = plot.labels, y = V1, label = labels))

t_height42_TfIIs



#####################################################################################################



ANOVA <- aov(new_data_Cdc73$height42 ~ new_data_Cdc73$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Cdc73$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_height42_Cdc73.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels, reversed = TRUE)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Cdc73, flev, function (x) max(fivenum(x$height42)) + 50)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_height42_Cdc73 <- ggplot(new_data_Cdc73, aes(x= line, y=height42))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Cdc73", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "height [mm]", limit = c(30,600)) +
  
  ggtitle("Plant height [DAS42]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Cdc73$line'), aes(x = plot.labels, y = V1, label = labels))

t_height42_Cdc73







# 5.10 Final height DAS49 -------------------------------------------------





ANOVA <- aov(new_data_Spt16$height49 ~ new_data_Spt16$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Spt16$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_height49_Spt16.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Spt16, flev, function (x) max(fivenum(x$height49)) + 50)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_height49_Spt16 <- ggplot(new_data_Spt16, aes(x= line, y=height49))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Spt16", "Spt16xElf1")) + 
  
  scale_y_continuous(name= "height [mm]", limit = c(30,600)) +
  
  ggtitle("Final height [DAS49]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Spt16$line'), aes(x = plot.labels, y = V1, label = labels))

t_height49_Spt16



###############################################################################################



ANOVA <- aov(new_data_Ssrp1$height49 ~ new_data_Ssrp1$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Ssrp1$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_height49_Ssrp1.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Ssrp1, flev, function (x) max(fivenum(x$height49)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_height49_Ssrp1 <- ggplot(new_data_Ssrp1, aes(x= line, y=height49))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Ssrp1", "Ssrp1xElf1")) + 
  
  scale_y_continuous(name= "height [mm]", limit = c(30,150)) +
  
  ggtitle("Final height [DAS49]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Ssrp1$line'), aes(x = plot.labels, y = V1, label = labels))

t_height49_Ssrp1



################################################################################################



ANOVA <- aov(new_data_TfIIs$height49 ~ new_data_TfIIs$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_TfIIs$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_height49_TfIIs.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_TfIIs, flev, function (x) max(fivenum(x$height49)) + 10)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_height49_TfIIs <- ggplot(new_data_TfIIs, aes(x= line, y=height49))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "TfIIs", "TfIIsxElf1")) + 
  
  scale_y_continuous(name= "height [mm]", limit = c(30,150)) +
  
  ggtitle("Final height [DAS49]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_TfIIs$line'), aes(x = plot.labels, y = V1, label = labels))

t_height49_TfIIs



#####################################################################################################



ANOVA <- aov(new_data_Cdc73$height49 ~ new_data_Cdc73$line)

summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data_Cdc73$line", conf.level = 0.95)

t_Tukey <- as.data.frame(TUKEY[1:1])

print(t_Tukey)

write.csv(t_Tukey, file = "Tukey_height49_Cdc73.csv") #An Excel file will be created, containing the Tukey results

# Tuckey test representation :

plot(TUKEY , las=1 , col="brown" )



# I need to group the treatments that are not different each other together.

generate_label_df <- function(HSD, flev){
  
  
  
  # Extract labels and factor levels from Tukey post-hoc 
  
  test.levels <- HSD[[flev]][,4]
  
  print(test.levels)
  
  test.labels <- multcompLetters(test.levels, reversed = TRUE)['Letters']
  
  #I need to put the labels in the same order as in the boxplot 
  
  print(test.labels)
  
  plot.labels <- names(test.labels[['Letters']])
  
  print(plot.labels)
  
  
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  
  # upper quantile and label placement
  
  boxplot.df <- ddply(new_data_Cdc73, flev, function (x) max(fivenum(x$height49)) + 50)
  
  print(boxplot.df)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            
                            stringsAsFactors = FALSE)
  
  print(plot.levels)
  
  
  
  # Merge it with the labels
  
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  print(labels.df)
  
  return(labels.df)
  
}



#Draw a basic boxplot

t_height49_Cdc73 <- ggplot(new_data_Cdc73, aes(x= line, y=height49))+
  
  geom_boxplot(alpha=0.7) + 
  
  scale_x_discrete(limits = c("Col_0", "Elf1", "Cdc73", "Cdc73xElf1")) + 
  
  scale_y_continuous(name= "height [mm]", limit = c(30,600)) +
  
  ggtitle("Final height [DAS49]") +
  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  
  theme_bw() +
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        
        text = element_text(size = 12),
        
        axis.title.y = element_text(face="bold"),
        
        axis.title.x = element_blank(),
        
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        
        legend.position ="bottom")+
  
  scale_fill_brewer(palette = "Accent") + 
  
  labs(fill = "Replicate") +
  
  geom_text(data = generate_label_df(TUKEY, 'new_data_Cdc73$line'), aes(x = plot.labels, y = V1, label = labels))

t_height49_Cdc73







#######################################################################################################



#Data output, depending on individual needs

pdf(file="Boxplots_20201112_spt16_all.pdf", height = 10, width = 12)

grid.arrange(t_tob_Spt16, t_rdb_Spt16, t_leaves_Spt16, 
             
             t_rd21_Spt16, t_rd28_Spt16, t_rd35_Spt16,
             
             t_pI_Spt16, t_sI_Spt16, t_height42_Spt16, ncol=3)

dev.off()



pdf(file="Boxplots_20201117_spt16.pdf", height = 10, width = 3)

grid.arrange(t_rdb_Spt16, t_rd28_Spt16, t_rd35_Spt16, t_height49_Spt16, ncol=1)

dev.off()



pdf(file="Boxplots_20201112_Ssrp1_all.pdf", height = 10, width = 12)

grid.arrange(t_tob_Ssrp1, t_rdb_Ssrp1, t_leaves_Ssrp1, 
             
             t_rd21_Ssrp1, t_rd28_Ssrp1, t_rd35_Ssrp1, ncol=3)

dev.off()



pdf(file="Boxplots_20201112_Ssrp1.pdf", height =10, width = 3)

grid.arrange(t_leaves_Ssrp1, t_rd21_Ssrp1, t_rd28_Ssrp1, t_rd35_Ssrp1, ncol=1)

dev.off()



pdf(file="Boxplots_20201115_cdc73_all.pdf", height = 6, width = 18)

grid.arrange(t_tob_Cdc73, t_rdb_Cdc73, t_rd21_Cdc73, t_rd28_Cdc73, t_rd35_Cdc73,
             
             t_leaves_Cdc73, t_pI_Cdc73, t_sI_Cdc73, t_height42_Cdc73, t_height49_Cdc73, ncol=5)

dev.off()



pdf(file="Boxplots_ssrp1_tfIIs_cdc73_20201005.pdf", height = 14, width = 10)

grid.arrange(t_rdb_Ssrp1, t_rdb_TfIIs, t_rdb_Cdc73,
             
             t_leaves_Ssrp1, t_leaves_TfIIs, t_leaves_Cdc73,
             
             t_rd21_Ssrp1, t_rd21_TfIIs, t_rd21_Cdc73,
             
             t_rd28_Ssrp1, t_rd28_TfIIs, t_rd28_Cdc73,
             
             t_rd35_Ssrp1, t_rd35_TfIIs, t_rd35_Cdc73, 
             
             t_height49_Ssrp1, t_height49_TfIIs, t_height49_Cdc73, ncol=3)

dev.off()