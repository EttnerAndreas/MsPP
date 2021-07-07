#####

#      current Date: 05.07.2021
#      current Version: 0.00.1

install.packages("unikn")
library('unikn') 
seecol("pair_all")

install.packages("fmsb")
library(fmsb)


devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
library(ggradar)

suppressPackageStartupMessages(library(dplyr))
library(scales)
library(tibble)
library(dplyr)

mtcars %>%
  rownames_to_column( var = "group" ) %>%
  mutate_at(vars(-group),funs(rescale)) %>%
  tail(4) %>% select(1:10) -> mtcars_radar

ggradar(mtcars_radar) 



data = data.frame(matrix(NA,6,8))
# rownames(data) = c("Columbia-0", "vir-1" , "fip-37" , "trm4b-1", "trm4b-2", "trm4b-4")
colnames(data) = c("group", "controll","150 Mannitol", "200 Mannitol", "300 Mannitol" , "37° heat", "frost" , "highlight")
data[1] = c("Columbia-0", "vir-1" , "fip-37" , "trm4b-1", "trm4b-2", "trm4b-4")
data[2] = c(1.00, 0.11, 0.18, 0.67, 0.72, 0.83)
data[3] = c(0.72, 0.04, 0.11, 0.71, 0.59, 0.54)
data[4] = c(0.51, 0.03, 0.10, 0.39, 0.45, 0.40)
data[5] = c(0.42, 0.03, 0.06, 0.35, 0.42, 0.33)
data[6] = c(0.11, 0.02, 0.03, 0.10, 0.10, 0.10)
data[7] = c(0.59, 0.08, 0.12, 0.47, 0.00, 0.64)
data[8] = c(0.45, 0.08, 0.14, 0.39, 0.00, 0.53)


data
?ggradar



ggradar(
  data, 
  values.radar = c("0", "0.4","1.0"),
  grid.min = 0, grid.mid = 0.4, grid.max = 1,
  # Polygons
  group.colours =  c("#004D40", "#58759A", "#26BD6B", "#D2817E",  "#D2C92A" , "#5A1B7F"),
  group.line.width = 1, 
  group.point.size = 2,
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "black",
  legend.position = "left"
)

data
df
radarchart(df)




create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

df
op <- par(mar = c(1, 1, 1, 1))
create_beautiful_radarchart(df)
par(op)





#'
#'
#'
#'
#'
#'
#'
op <- par(mar = c(1, 1, 1, 1))
# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 5, 10, 15, 20),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)




#####################
######################


# RESTART 
scores <- data.frame(
  row.names = c("Columbia-0", "vir-1" , "fip-37" , "trm4b-1", "trm4b-2", "trm4b-4"),
  "controll" = c(1.00, 0.11, 0.18, 0.67, 0.72, 0.83),
  "150 Mannitol" = c(0.72, 0.04, 0.11, 0.71, 0.59, 0.54),
  "200 Mannitol" = c(0.51, 0.03, 0.10, 0.39, 0.45, 0.40),
  "300 Mannitol" = c(0.42, 0.03, 0.06, 0.35, 0.42, 0.33),
  "37° heat" = c(0.11, 0.02, 0.03, 0.10, 0.10, 0.10),
  "frost" = c(0.75, 0.08, 0.12, 0.47, 0.00, 0.64),
  "highlight" = c(0.45, 0.08, 0.14, 0.39, 0.00, 0.53)
)
scores

max_min <- data.frame(
  "controll" = c(1, 0), "150 Mannitol" = c(1, 0), "200 Mannitol" = c(1, 0),
  "300 Mannitol" = c(1, 0), "37° heat" = c(1, 0), "frost" = c(1, 0),
  "highlight" = c(1, 0)
)
rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, scores)
df





##############
create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 1.0,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.2), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

##############
colors <- c("#004D40", "#58759A", "#26BD6B", "#D2817E",  "#D2C92A" , "#5A1B7F")
titles <- c("Columbia-0", "vir-1" , "fip-37" , "trm4b-1", "trm4b-2", "trm4b-4")

par(mfrow=c(1,1))
# Create the radar charts
create_beautiful_radarchart(
  data = df,
  color = colors
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = colors,
  text.col = "black", cex = 0.9, pt.cex = 1.3
)
par(op)



df

# Reduce plot margin using par()
# Split the screen in 3 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,3))

# Create the radar chart
for(i in 1:6){
  create_beautiful_radarchart(
    data = df[c(1, 2, i+2), ],
    color = colors[i], title = titles[i]
  )
}
par(op)










#