library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggrepel)
library(maps)
library(tidyr)
library(tidyverse)

#dataset tiroteos de policias a civiles
data_shootings <- read.csv('./data/shootings.csv')

colnames(data_shootings)

data_shootings <- subset(data_shootings, select = -c(id, name, date, manner_of_death, 
                                                     armed, age, gender, city, state, 
                                                     signs_of_mental_illness,
                                                     threat_level, flee, body_camera))

data_shootings$arms_category <- ifelse(data_shootings$arms_category == "Unarmed", "Unarmed", "Armed")
data_shootings$race <- ifelse(data_shootings$race == "White", "White", 
                       ifelse(data_shootings$race == "Black", "Black", "Other"))

#visualizacion barras razas-armado/no armado
data_shootings_2 <- data_shootings %>% 
  group_by(race, arms_category) %>% 
  count()

data_shootings_2$race <- factor(data_shootings_2$race,
                                      levels = c("White", "Black", "Other"))
data_shootings_2$arms_category <- factor(data_shootings_2$arms_category,
                                levels = c("Armed","Unarmed"))
  
gg <- ggplot(data=data_shootings_2,aes(x=arms_category, y=n))
gg <- gg + facet_grid(.~race)
gg <- gg + geom_col(aes(fill=arms_category), position="dodge")
gg <- gg + scale_fill_manual(values=c("black", "white"))
gg <- gg + labs(title="Shootings per Race")
gg <- gg + theme_dark()
gg

#visualización asesinados armados/desarmados
#voy a calcular el cociente disparados_desarmados/disparados_totales 
#WHITE -> 0.059%
fr_unarmed_white <- data_shootings_2[data_shootings_2$race=="White" & 
                                     data_shootings_2$arms_category=="Unarmed",][1,3]
total_white <- fr_unarmed_white + data_shootings_2[data_shootings_2$race=="White" & 
                                                  data_shootings_2$arms_category=="Armed",][1,3]
percentage_white <- fr_unarmed_white/total_white

#BLACK -> 0.095%
fr_unarmed_black <- data_shootings_2[data_shootings_2$race=="Black" & 
                                       data_shootings_2$arms_category=="Unarmed",][1,3]
total_black <- fr_unarmed_black + data_shootings_2[data_shootings_2$race=="Black" & 
                                                     data_shootings_2$arms_category=="Armed",][1,3]
percentage_black <- fr_unarmed_black/total_black

#OTHER -> 0.070%
fr_unarmed_other <- data_shootings_2[data_shootings_2$race=="Other" & 
                                       data_shootings_2$arms_category=="Unarmed",][1,3]
total_other <- fr_unarmed_other + data_shootings_2[data_shootings_2$race=="Other" & 
                                                     data_shootings_2$arms_category=="Armed",][1,3]
percentage_other <- fr_unarmed_other/total_other

df <- data.frame(race=c("White","White","White","White","White",
                        "White","White","White","White","White",
                        "White","White","White","White","White",
                        "White","White","White","White","White",
                        "Black","Black","Black","Black","Black",
                        "Black","Black","Black","Black","Black",
                        "Black","Black","Black","Black","Black",
                        "Black","Black","Black","Black","Black",
                        "Other","Other","Other","Other","Other",
                        "Other","Other","Other","Other","Other",
                        "Other","Other","Other","Other","Other",
                        "Other","Other","Other","Other","Other"),
                 armed=c(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                         0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                         0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                 column=c("c00", "c01","c02","c03","c04","c05","c06","c07","c08","c09",
                          "c10", "c11","c12","c13","c14","c15","c16","c17","c18","c19",
                          "c00", "c01","c02","c03","c04","c05","c06","c07","c08","c09",
                          "c10", "c11","c12","c13","c14","c15","c16","c17","c18","c19",
                          "c00", "c01","c02","c03","c04","c05","c06","c07","c08","c09",
                          "c10", "c11","c12","c13","c14","c15","c16","c17","c18","c19"
                          )
                 )
df$armed <- ifelse(df$armed == 0, "Unarmed", "Armed")
df$race <- factor(df$race,levels = c("Other", "Black", "White"))

#grafico de puntos 
gg <- ggplot(data=df,aes(x=column, y=race, fill=armed))
gg <- gg + geom_point(size=10, shape=23)
gg <- gg + scale_colour_manual(values=c("black", "white"),name = "Armed")
gg <- gg + labs(title="Armed/unarmed people for each 10 people shoted of each race",
                caption="For this graph we have to imagine that we have taken
                ten white people, ten black people and ten people from other race.
                Each dot represents a person shoted by a police in US, white dots
                are for unarmed people and black dots for armed ones.",
                x="",y="")
gg <- gg + theme_light()
gg


