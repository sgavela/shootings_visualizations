#VISUALIZACIONES DEMOGRÁFICO POR AÑO

#vamos a tomar una perspectiva histórica y demográfica con un gráfico
#de barras en el que por años se represente la raza, el género y
#la franja de edad de los tiroteados

install.packages("tidyverse")

library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggrepel)
library(maps)
library(tidyr)
library(tidyverse)

#dataset tiroteos de policias a civiles
data_shootings <- read.csv('./data/shootings.csv')

str(data_shootings)

data_shootings$date <- as.Date(data_shootings$date,
                               format = "%Y-%m-%d")
data_shootings$year <- as.numeric(format(data_shootings$date,'%Y'))

#one hot encoding raza
data_shootings_wide <- data_shootings %>% 
  mutate(value = 1) %>% 
  spread(race, value,  fill = 0) 

#one hot encoding género
data_shootings_wide <- data_shootings_wide %>% 
  mutate(value = 1) %>% 
  spread(gender, value,  fill = 0)

#one hot encoding edades (por franjas de 20)
data_shootings_wide$age0_20 <- ifelse(data_shootings$age < 20, 1, 0)
data_shootings_wide$age20_40 <- ifelse(data_shootings$age >= 20 & 
                                         data_shootings$age < 40, 1, 0)
data_shootings_wide$age40_60 <- ifelse(data_shootings$age >= 40 & 
                                         data_shootings$age < 60, 1, 0)
data_shootings_wide$age60_80 <- ifelse(data_shootings$age >= 60 & 
                                         data_shootings$age < 80, 1, 0)

shootings_timeline <- data_shootings_wide %>% 
  group_by(year) %>% 
  summarise(Asian=sum(Asian),
            White=sum(White), Hispanic=sum(Hispanic),
            Black=sum(Black), Other=sum(Other),
            Native=sum(Native), F=sum(F),
            M=sum(M), age0_20=sum(age0_20),
            age20_40=sum(age20_40),age40_60=sum(age40_60),
            age60_80=sum(age60_80))

shootings_timeline_race <- pivot_longer(shootings_timeline, cols=c("Asian","White",
                                                                   "Hispanic","Black",
                                                                   "Other","Native"),
                                        names_to="feature_category", values_to="num_shootings")
shootings_timeline_race$feature <- c("race")


shootings_timeline_gender <- pivot_longer(shootings_timeline, cols=c("F","M"),
                                          names_to="feature_category", values_to="num_shootings")
shootings_timeline_gender$feature <- c("gender")

shootings_timeline_age <- pivot_longer(shootings_timeline, cols=c("age0_20","age20_40", 
                                                                  "age40_60", "age60_80"),
                                       names_to="feature_category", values_to="num_shootings")
shootings_timeline_age$feature <- c("age")

shootings_timeline_race <- subset(shootings_timeline_race, select = -c(F, M, age0_20, age20_40, age40_60, age60_80))
shootings_timeline_gender <- subset(shootings_timeline_gender, select = -c(Asian, White, Hispanic, Black, Native, Other, age0_20, age20_40, age40_60, age60_80))
shootings_timeline_age <- subset(shootings_timeline_age, select = -c(Asian, White, Hispanic, Black, Native, Other, F, M))

shootings_timeline_full <-rbind(shootings_timeline_race, shootings_timeline_gender)
shootings_timeline_full <-rbind(shootings_timeline_full, shootings_timeline_age)

shootings_timeline_full$feature_category <- factor(shootings_timeline_full$feature_category,
                                          levels = c("Other", "Asian", "Native",
                                                     "Hispanic", "Black", "White",
                                                     "F", "M",
                                                     "age60_80", "age40_60", "age20_40",
                                                     "age0_20"))

#TODO JUNTO hecho con faceting, la leyenda queda fatal
gg <- ggplot(data=shootings_timeline_full, aes(x=year, y=num_shootings))
gg <- gg + scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020))
gg <- gg + facet_grid(.~feature)
gg <- gg + geom_col(aes(fill=feature_category), width=0.6)
gg <- gg + scale_fill_calc(name = "Feature Value", labels = c("Race Other", "Race Asian", "Race Native",
                                                              "Race Hispanic", "Race Black", "Race White",
                                                              "Gender Female", "Gender Male",
                                                              "Age 60-80", "Age 40-60", "Age 20-40",
                                                              "Age 0-20"))
gg <- gg + labs(x="Year", y="Number of shootings", title="Number of shootings per year and age",
                caption="*2020 data is only from January to July")
gg <- gg + theme_light()
gg

#RACE
shootings_timeline_race$feature_category <- factor(shootings_timeline_race$feature_category,
                                           levels = c("Other", "Asian", "Native",
                                                      "Hispanic", "Black", "White"))

gg <- ggplot(data=shootings_timeline_race, aes(x=year, y=num_shootings))
gg <- gg + scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020))
gg <- gg + geom_col(aes(fill=feature_category), width=0.4)
gg <- gg + scale_fill_calc(name = "Race")
gg <- gg + labs(x="Year", y="Number of shootings", title="Number of shootings per year and race",
                caption="*2020 data is only from January to July")
gg <- gg + theme_light()
gg

#GENDER
shootings_timeline_gender
gg <- ggplot(data=shootings_timeline_gender, aes(x=year, y=num_shootings))
gg <- gg + scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020))
gg <- gg + geom_col(aes(fill=feature_category), width=0.4)
gg <- gg + scale_fill_calc(name = "Gender", labels=c("Female", "Male"))
gg <- gg + labs(x="Year", y="Number of shootings", title="Number of shootings per year and gender",
                caption="*2020 data is only from January to July")
gg <- gg + theme_light()
gg

#AGE
shootings_timeline_age$feature_category <- factor(shootings_timeline_age$feature_category,
                                                   levels = c("age60_80", "age40_60", "age20_40",
                                                              "age0_20"))

gg <- ggplot(data=shootings_timeline_age, aes(x=year, y=num_shootings))
gg <- gg + scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020))
gg <- gg + geom_col(aes(fill=feature_category), width=0.4)
gg <- gg + scale_fill_calc(name = "Age", labels=c("60-80", "40-60", "20-40", "0-20"))
gg <- gg + labs(x="Year", y="Number of shootings", title="Number of shootings per year and age",
                caption="*2020 data is only from January to July")
gg <- gg + theme_light()
gg




