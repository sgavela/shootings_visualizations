#VISUALIZACIONES TIROTEOS Y ESTADOS

#1 - grafico con puntos en el que se vea población de cada estado frente a numero de asesinatos
#2 - mapa de usa coloreado con asesinatos/habitantes

install.packages("ggthemes")
install.packages("ggrepel")

library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggrepel)
library(maps)

setwd(".")

#VISUALIZACION TIROTEOS POR ESTADO 1

#censo poblacion por estados 2010
usa_census <- read.csv('./data/nst-est2020.csv')
#dataset tiroteos de policias a civiles
data_shootings <- read.csv('./data/shootings.csv')

#renombrar de abreviaturas de estados a nombres de estados
data_shootings <- mutate(data_shootings, region = case_when(
  state == 'AL' ~ 'alabama', state == 'AK' ~ 'alaska',
  state == 'AZ' ~ 'arizona', state == 'AR' ~ 'arkansas',
  state == 'CA' ~ 'california', state == 'CO' ~ 'colorado',
  state == 'CT' ~ 'connecticut', state == 'DE' ~ 'delaware',
  state == 'DC' ~ 'district of columbia', state == 'FL' ~ 'florida',
  state == 'GA' ~ 'georgia', state == 'HI' ~ 'hawaii',
  state == 'ID' ~ 'idaho', state == 'IL' ~ 'illinois',
  state == 'IN' ~ 'indiana', state == 'IA' ~ 'iowa',
  state == 'KS' ~ 'kansas', state == 'KY' ~ 'kentucky',
  state == 'LA' ~ 'louisiana', state == 'ME' ~ 'maine',
  state == 'MD' ~ 'maryland', state == 'MA' ~ 'massachusetts',
  state == 'MI' ~ 'michigan', state == 'MN' ~ 'minnesota',
  state == 'MS' ~ 'mississippi', state == 'MO' ~ 'missouri',
  state == 'MT' ~ 'montana', state == 'NE' ~ 'nebraska',
  state == 'NV' ~ 'nevada', state == 'NH' ~ 'new hampshire',
  state == 'NJ' ~ 'new jersey', state == 'NM' ~ 'new mexico',
  state == 'NY' ~ 'new york', state == 'NC' ~ 'north carolina',
  state == 'ND' ~ 'north dakota', state == 'OH' ~ 'ohio',
  state == 'OK' ~ 'oklahoma', state == 'OR' ~ 'oregon',
  state == 'PA' ~ 'pennsylvania', state == 'RI' ~ 'rhode island',
  state == 'SC' ~ 'south carolina', state == 'SD' ~ 'south dakota',
  state == 'TN' ~ 'tennessee', state == 'TX' ~ 'texas',
  state == 'UT' ~ 'utah', state == 'VT' ~ 'vermont',
  state == 'VA' ~ 'virginia', state == 'WA' ~ 'washington',
  state == 'WV' ~ 'west virginia', state == 'WI' ~ 'wisconsin',
  state == 'WY' ~ 'wyoming'
)
)

#nos quedamos con la region (norte, sur, este, oeste), nombre del estado
#y población
usa_census <- select(usa_census, REGION, NAME, CENSUS2010POP)
#poner en minusculas los nombres de los estados en el censo
usa_census <- mutate(usa_census, region=tolower(NAME))

#ver que esté todo bien
str(usa_census)
str(data_shootings)

#dataframe con tiroteos por estado
shootings_per_state <- data_shootings %>% 
                       group_by(region, state) %>% 
                       summarise(num_shootings = n())

#join de los dos dataframes (para añadir poblacion de cada estado)
shootings_per_state <- left_join(shootings_per_state, usa_census, by='region')

#cambio de codigo a nombre de las regiones
shootings_per_state <- mutate(shootings_per_state, Region = case_when(
  REGION == 1 ~ 'Northeast', REGION == 2 ~ 'Midwest',
  REGION == 3 ~ 'South', REGION == 4 ~ 'West',
)
)

#dataframe asesinatos totales 
shootings_total <- head(usa_census, 1)

#calculamos dataframe asesinatos por región
shootings_per_region <- usa_census[2:5,]
shootings_per_region <- left_join(shootings_per_region, shootings_per_state, by='REGION')
shootings_per_region <- shootings_per_state %>%  
                        group_by(Region) %>% 
                        summarise(num_shootings = sum(num_shootings), CENSUS2010POP=sum(CENSUS2010POP))
shootings_per_region


#calculo columna con densidad de tiroteos (nºtiroteos/nº habitantes)
shootings_per_state <- mutate(shootings_per_state, shooting_density=num_shootings*1000/CENSUS2010POP)
shootings_per_state <- mutate(shootings_per_state, million_population=CENSUS2010POP/1000000)

shootings_per_region <- mutate(shootings_per_region, shooting_density=num_shootings*1000/CENSUS2010POP)
shootings_per_region <- mutate(shootings_per_region, million_population=CENSUS2010POP/1000000)

shootings_total$CENSUS2010POP <- sum(shootings_per_region$CENSUS2010POP)
shootings_total$num_shootings <- sum(shootings_per_region$num_shootings)
shootings_total <- mutate(shootings_total, shooting_density=num_shootings*1000/CENSUS2010POP)
shootings_total <- mutate(shootings_total, million_population=CENSUS2010POP/1000000)

#gráfico por estados
gg <- ggplot(data=shootings_per_state, aes(x=million_population, y=num_shootings,
                                           col=Region))
gg <- gg + scale_y_continuous(trans = 'log10')
gg <- gg + scale_x_continuous(trans = 'log10')
gg <- gg + geom_smooth(method=lm, se=FALSE, colour="black", size=0.8, fullrange=TRUE)
gg <- gg + geom_point(alpha=0.5, show.legend = FALSE)
gg <- gg + geom_label_repel(aes(label=state), direction="y", show.legend = FALSE)
gg <- gg + labs(x="Million inhabitants (log10)", y="nº shootings (log10)", title="SHOOTINGS AND INHABITANTS PER STATE AND REGION")

#al gráfico por estados le sumamos los puntos con los totales de las regioens
gg <- gg + geom_point(data=shootings_per_region, aes(x=million_population, y=num_shootings,
                                                     col=Region), shape=18, size=5, show.legend = FALSE)
gg <- gg + geom_label_repel(data=shootings_per_region, aes(label=Region), direction="y", 
                            size=4.5, show.legend = FALSE, nudge_y=0.1)

gg <- gg + theme_light()
gg

#VISUALIZACION TIROTEOS POR ESTADO 2

states_coords <- data.frame(x = state.center$x, y = state.center$y,
                 region = tolower(state.name))
states_coords <- left_join(states_coords, shootings_per_state, by='region')

usa_map <- map_data("state")

gg <- ggplot()
gg <- gg + geom_map(data=usa_map, map=usa_map,
                    aes(long, lat, map_id=region),
                    color="#b2b2b2", size=0.1, fill=NA)
gg <- gg + geom_map(data=shootings_per_state, map=usa_map,
                    aes(fill=shooting_density, map_id=region),
                    color="#b2b2b2", size=0.1)
gg <- gg + scale_fill_gradient(low = "white", high = "red3")
gg <- gg + coord_fixed(ratio = 1)
gg <- gg + geom_label_repel(data=states_coords, aes(x=x, y=y, label=state), size=4)
gg <- gg + labs(fill="Shooting density (per thousand)", 
     title="SHOOTING'S PER INHABITANTS IN USA DENSITY MAP")
gg <- gg + theme_light()
gg







