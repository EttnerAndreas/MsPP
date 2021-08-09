library(multcompView)
library(readxl)
library(ggplot2)
library(multcompView)
library(plyr)
library(gridExtra)
library(dplyr)



input_data <- read_excel("~/Version-controll/MsPP/Plantstress_ImageJ/plant-weight-day21.xlsx")

input_data = input_data[,c(1,2,4,6,7,9)]
input_data = input_data[complete.cases(input_data), ]


colnames(input_data) = c("genotype", "fresh_weight", "dry_weight", "rep", "difference","treatment")

input_data = data.frame(input_data)
class(input_data)
str(input_data)
set.seed(1)

#input_data[,1] = as.factor (input_data[,1])
input_data[,2] = as.numeric (input_data[,2])
input_data[,3] = as.numeric (input_data[,3])
input_data[,4] = as.factor(input_data[,4])
input_data[,5] = as.numeric(input_data[,5])
#input_data[,6] = as.factor(input_data[,6])

for (i in 1:nrow(input_data)){
  input_data$genotype[i] = str_replace_all(input_data$genotype[i], "-", "_")
}




######
data = input_data[1:18,]
# What is the effect of the treatment on the value ?
model=lm( data$difference ~ data$genotype )
ANOVA=aov(model)
# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, "data$genotype" ,conf.level=0.95)
# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")




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
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],stringsAsFactors = FALSE)
  print(plot.levels)
  
  # Merge it with the labels
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  print(labels.df)
  return(labels.df)
}



ANOVA <- aov(data$fresh_weight ~ data$genotype )
summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY
TUKEY <- TukeyHSD(x=ANOVA, "data$genotype", conf.level = 0.95)
t_Tukey <- as.data.frame(TUKEY[1:1])
print(t_Tukey)
write.csv(t_Tukey, file = "Plantstress_ImageJ/aov/tukey/NaCl.csv") #An Excel file will be created, containing the Tukey results
# Tuckey test representation :
plot(TUKEY , las=1 , col="brown" )

sodium <- ggplot(data, aes(x= genotype, y= fresh_weight )) +
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(labels = c("trb_4b-1", "trb_4b-2", "trb_4b-4", "columbia-0" , "fip-37" , "vir-1")) + 
  scale_y_continuous(name= "Weight [g]", limit = c(0.1,2.500)) +
  ggtitle(data$treatment) +
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
  # labs(fill = "rep") +
  # geom_text(data = generate_label_df(TUKEY, "data$genotype"), aes(x = plot.labels , y = V1, label = labels))
  labs(fill = "Replicate") +
  geom_text(data = generate_label_df(TUKEY, "data$genotype"), aes(x = plot.labels, y = V1, label = labels), position = position_dodge(width = 1),   )

sodium








str(data)
class(data)
data[,1] = as.fact( data[,1])




model=lm( data$fresh_weight ~ data$genotype )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'data$genotype', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")


generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

# Apply the function on my dataset
LABELS <- generate_label_df(TUKEY , "data$genotype")


# A panel of colors to draw each group with the same color :
my_colors <- c( 
  rgb(143,199,74,maxColorValue = 255),
  rgb(242,104,34,maxColorValue = 255), 
  rgb(111,145,202,maxColorValue = 255)
)

plot <- ggplot(data, aes(x= genotype, y= fresh_weight , fill = genotype )) +
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(labels = c("trb_4b-1", "trb_4b-2", "trb_4b-4", "columbia-0" , "fip-37" , "vir-1")) + 
  scale_y_continuous(name= "Weight [g]", limit = c(0.1,2.500)) +
  ggtitle(data$treatment) +
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
  labs(fill = "rep") +
  geom_text(data = generate_label_df(TUKEY, "data$genotype"), aes(x = plot.labels , y = V1 , label = labels)  )

plot





# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1 * max( data$fresh_weight)
#Add the labels

text( c(1:6) , data$fresh_weight + over , LABELS[,1]  , col= my_colors[ as.numeric (as.factor(LABELS[,1]))  ] )
my_colors[2]

##################################

##