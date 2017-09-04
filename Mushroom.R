#HW1-607


library(RCurl)
Mushroom <- getURL('https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data')
?getURL

library(plyr)
df <- data.frame(read.csv(text=Mushroom, header=FALSE),sep="'")
head(df)
dim(df)
str(df)

library(psych) #Summary Statisitcs by Group
#https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.names
#Change names for attributes and their valuses
names(df) <- c("classes","cap_shape","cap_surface","cap_color","bruises","odor","gill_attachment","gill_spacing"
                     ,"gill_size","gill_color","stalk_shape","stalk_root","stalk_surface_above_ring",
                     "stalk_surface_below_ring","stalk_color_above_ring","stalk_color_below_ring",
                     "veil_type","veil_color","ring_number","ring_type","spore_print_color","population","habitat")
names(df)

subDf <- data.frame(df$classes,df$odor,df$spore_print_color,df$stalk_surface_below_ring,df$stalk_color_above_ring,df$habitat,df$cap_color,df$population,
                    df$cap_shape,df$cap_surface,df$bruises,df$gill_attachment,df$gill_spacing,df$gill_size,df$gill_color,df$stalk_shape,df$stalk_root,
                    df$stalk_surface_above_ring,df$stalk_color_below_ring,df$veil_type,df$veil_color,df$ring_number,df$ring_type)
levels(subDf$df.classes) [levels(subDf$df.classes)=="p"]  <- "poisonous"
levels(subDf$df.classes) [levels(subDf$df.classes)=="e"]  <- "edible"

levels(subDf$df.odor) [levels(subDf$df.odor)=="a"] <- "almond"
levels(subDf$df.odor) [levels(subDf$df.odor)=="l"] <- "anise"
levels(subDf$df.odor) [levels(subDf$df.odor)=="c"] <- "creosote"
levels(subDf$df.odor) [levels(subDf$df.odor)=="y"] <- "fishy"
levels(subDf$df.odor) [levels(subDf$df.odor)=="f"] <- "foul"
levels(subDf$df.odor) [levels(subDf$df.odor)=="m"] <- "musty"
levels(subDf$df.odor) [levels(subDf$df.odor)=="n"] <- "none"
levels(subDf$df.odor) [levels(subDf$df.odor)=="p"] <- "pungent"
levels(subDf$df.odor) [levels(subDf$df.odor)=="s"] <- "spicy"

levels(subDf$df.spore_print_color) [levels(subDf$df.spore_print_color)=="k"] <- "black"
levels(subDf$df.spore_print_color) [levels(subDf$df.spore_print_color)=="n"] <- "brown"
levels(subDf$df.spore_print_color) [levels(subDf$df.spore_print_color)=="b"] <- "buff"
levels(subDf$df.spore_print_color) [levels(subDf$df.spore_print_color)=="h"] <- "chocolate"
levels(subDf$df.spore_print_color) [levels(subDf$df.spore_print_color)=="r"] <- "green"
levels(subDf$df.spore_print_color) [levels(subDf$df.spore_print_color)=="o"] <- "orange"
levels(subDf$df.spore_print_color) [levels(subDf$df.spore_print_color)=="u"] <- "purple"
levels(subDf$df.spore_print_color) [levels(subDf$df.spore_print_color)=="w"] <- "white"
levels(subDf$df.spore_print_color) [levels(subDf$df.spore_print_color)=="y"] <- "yellow"

levels(subDf$df.stalk_surface_below_ring) [levels(subDf$df.stalk_surface_below_ring)=="f"] <- "fibrous"
levels(subDf$df.stalk_surface_below_ring) [levels(subDf$df.stalk_surface_below_ring)=="y"] <- "scaly"
levels(subDf$df.stalk_surface_below_ring) [levels(subDf$df.stalk_surface_below_ring)=="k"] <- "silky"
levels(subDf$df.stalk_surface_below_ring) [levels(subDf$df.stalk_surface_below_ring)=="s"] <- "smooth"

levels(subDf$df.stalk_color_above_ring) [levels(subDf$df.stalk_color_above_ring)=="n"] <- "brown"
levels(subDf$df.stalk_color_above_ring) [levels(subDf$df.stalk_color_above_ring)=="b"] <- "buff"
levels(subDf$df.stalk_color_above_ring) [levels(subDf$df.stalk_color_above_ring)=="c"] <- "cinnamon"
levels(subDf$df.stalk_color_above_ring) [levels(subDf$df.stalk_color_above_ring)=="g"] <- "gray"
levels(subDf$df.stalk_color_above_ring) [levels(subDf$df.stalk_color_above_ring)=="o"] <- "orange"
levels(subDf$df.stalk_color_above_ring) [levels(subDf$df.stalk_color_above_ring)=="p"] <- "pink"
levels(subDf$df.stalk_color_above_ring) [levels(subDf$df.stalk_color_above_ring)=="e"] <- "red"
levels(subDf$df.stalk_color_above_ring) [levels(subDf$df.stalk_color_above_ring)=="w"] <- "white"
levels(subDf$df.stalk_color_above_ring) [levels(subDf$df.stalk_color_above_ring)=="y"] <- "yellow"

levels(subDf$df.habitat) [levels(subDf$df.habitat)=="g"] <- "grasses"
levels(subDf$df.habitat) [levels(subDf$df.habitat)=="l"] <- "leaves"
levels(subDf$df.habitat) [levels(subDf$df.habitat)=="m"] <- "meadows"
levels(subDf$df.habitat) [levels(subDf$df.habitat)=="p"] <- "paths"
levels(subDf$df.habitat) [levels(subDf$df.habitat)=="u"] <- "urban"
levels(subDf$df.habitat) [levels(subDf$df.habitat)=="w"] <- "waste"
levels(subDf$df.habitat) [levels(subDf$df.habitat)=="d"] <- "woods"

levels(subDf$df.population) [levels(subDf$df.population)=="a"] <- "abundant"
levels(subDf$df.population) [levels(subDf$df.population)=="c"] <- "clustered"
levels(subDf$df.population) [levels(subDf$df.population)=="n"] <- "numerous"
levels(subDf$df.population) [levels(subDf$df.population)=="s"] <- "scattered"
levels(subDf$df.population) [levels(subDf$df.population)=="v"] <- "several"
levels(subDf$df.population) [levels(subDf$df.population)=="y"] <- "solitary"

levels(subDf$df.cap_color) [levels(subDf$df.cap_color)=="n"] <- "brown"
levels(subDf$df.cap_color) [levels(subDf$df.cap_color)=="b"] <- "buff"
levels(subDf$df.cap_color) [levels(subDf$df.cap_color)=="c"] <- "cinnamon"
levels(subDf$df.cap_color) [levels(subDf$df.cap_color)=="g"] <- "gray"
levels(subDf$df.cap_color) [levels(subDf$df.cap_color)=="r"] <- "green"
levels(subDf$df.cap_color) [levels(subDf$df.cap_color)=="p"] <- "pink"
levels(subDf$df.cap_color) [levels(subDf$df.cap_color)=="u"] <- "purple"
levels(subDf$df.cap_color) [levels(subDf$df.cap_color)=="e"] <- "red"
levels(subDf$df.cap_color) [levels(subDf$df.cap_color)=="w"] <- "white"
levels(subDf$df.cap_color) [levels(subDf$df.cap_color)=="y"] <- "yellow"

head(subDf)
summary(subDf)

#Disjunctive rules for poisonous mushrooms, from most generalto most specific:
#P_1 odor=NOT(almond.OR.anise.OR.none) -> odor=(creosote=c,fishy=y,foul=f,musty=m,pungent=p,spicy=s)
#120 poisonous cases missed, 98.52% accuracy
odor_cyfmps <- as.data.frame(subset(subDf,df.odor!="almond"|df.odor!="anise"|df.odor!="none" & df.classes =="poisonous",select=c(df.classes,df.odor)))
ftable(odor_cyfmps)

library ('ggplot2')
qplot(df.odor, data = odor_cyfmps, fill= df.classes)

#calculate the accuracy
total <- nrow(odor_cyfmps) #Numerical sample size of subset odor
print( 1- 120/total) #Prop. of edible in odor method


#P_2) spore-print-color=green
# 48 cases missed, 99.41% accuracy
sporeGreen <- as.data.frame(subset(subDf,df.spore_print_color!="green", select=c(df.classes,df.spore_print_color)))
ftable(sporeGreen) # frequency of atruribues
#I doute 48 cases missed in this condition since all green mushroom are poisonous. -> 72 missed
sum(is.na(sporeGreen$df.spore_print_color))

library ('ggplot2')
qplot(df.spore_print_color, data = sporeGreen, fill= df.classes)

#calculate the accuracy if 48 cases missed
total <- nrow(sporeGreen) #Numerical sample size of subset odor
print( 1- 48/total) #Prop. of edible in odor method

#P_3) odor=none.AND.stalk-surface-below-ring=scaly.AND.(stalk-color-above-ring=NOT.brown)
#8 cases missed, 99.90% accuracy
odor_stalkFaceBelow_stalkColAbove <-as.data.frame(subset(subDf,df.odor="none" & df.stalk_surface_below_ring=="scaly" & df.stalk_color_above_ring!="brown",select=c(df.classes,df.odor,df.stalk_surface_below_ring,df.stalk_color_above_ring)))
summary(odor_stalkFaceBelow_stalkColAbove)
ftable(odor_stalkFaceBelow_stalkColAbove)

#calculate the accuracy if 8 cases missed
total <- nrow(odor_stalkFaceBelow_stalkColAbove) #Numerical sample size of subset odor
print( 1- 8/total) #Prop. of edible in odor method

#P_4) habitat=leaves.AND.cap-color=white
#100% accuracy
hab_capCol <- as.data.frame(subset(subDf,df.habitat="leaves" & df.cap_color=="white",select=c(df.classes,df.habitat,df.cap_color)))
ftable(hab_capCol)

library ('ggplot2')
qplot(df.habitat=="leaves",df.cap_color=="white", data = hab_capCol, fill=df.classes,color=df.classes)


#P_4') population=clustered.AND.cap_color=white
pop_capCol <- as.data.frame(subset(subDf,df.population="clustered" & df.cap_color=="white",select=c(df.classes,df.population,df.cap_color)))
ftable(pop_capCol) # frequency of attributes

library ('ggplot2')
qplot(df.population=="clustered",df.cap_color=="white", data = pop_capCol, fill=df.classes,color=df.classes)


#Junctive rules: odor=(almond.OR.anise.OR.none).AND.spore-print-color=NOT.green
#gives 48 errors, or 99.41% accuracy on the whole dataset.
odor_spore <- as.data.frame(subset(subDf,df.odor=="almond"|df.odor=="anise"|df.odor=="none" & df.spore_print_color!="green",select=c(df.classes,df.odor,df.spore_print_color)))
ftable(odor_spore) # frequency of attributes

#calculate the accuracy if 8 cases missed
total <- nrow(odor_spore) #Numerical sample size of subset odor
print( 1- 48/total) #Prop. of edible in odor method

library ('ggplot2')
qplot(df.odor=="almond"|df.odor=="anise"|df.odor=="none",df.spore_print_color!="green", data = odor_spore, fill=df.classes,color=df.classes)
