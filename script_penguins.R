#install.packages("palmerpenguins")
library(palmerpenguins)
library(dplyr)
library(plyr)
library(ggplot2)
library(patchwork) 



####1) Número de individuos totales (1.1.), masculinos y femeninos por especie (1.2.). 
data(package = 'palmerpenguins')


penguins
species <- group_by(penguins, species)
table(species ~ species$species)
dplyr::count(species)

#1.1.

1 Adelie      152
2 Chinstrap    68
3 Gentoo      124


sex <- group_by(penguins, sex)
count(species, sex)

#1.2.

1 Adelie    female    73
2 Adelie    male      73
3 Adelie    NA         6
4 Chinstrap female    34
5 Chinstrap male      34
6 Gentoo    female    58
7 Gentoo    male      61
8 Gentoo    NA         5


###1.3)La media, desviación estándar, valor mínimo y máximo de la longitud 
#y profundidad del pico, la longitud de la aleta y el tamaño.

#Longitud Pico

media1 <- mean(penguins$bill_length_mm, na.rm = TRUE)
desvest1 <- sd(penguins$bill_length_mm, na.rm = TRUE)
minim1 <- min(penguins$bill_length_mm, na.rm = TRUE)
maxim1 <- max(penguins$bill_length_mm, na.rm = TRUE)

est_long_pico <- c(media1, desvest1, minim1, maxim1)

#Profundidad Pico

media2 <- mean(penguins$bill_depth_mm, na.rm = TRUE)
desvest2 <- sd(penguins$bill_depth_mm, na.rm = TRUE)
minim2 <- min(penguins$bill_depth_mm, na.rm = TRUE)
maxim2 <- max(penguins$bill_depth_mm, na.rm = TRUE)

est_prof_pico <- c(media2, desvest2, minim2, maxim2)

#Longitud Aleta

media3 <- mean(penguins$flipper_length_mm, na.rm = TRUE)
desvest3 <- sd(penguins$flipper_length_mm, na.rm = TRUE)
minim3 <- min(penguins$flipper_length_mm, na.rm = TRUE)
maxim3 <- max(penguins$flipper_length_mm, na.rm = TRUE)

est_long_aleta <- c(media3, desvest3, minim3, maxim3)


#Tamaño (masa)
media4 <- mean(penguins$body_mass_g, na.rm = TRUE)
desvest4 <- sd(penguins$body_mass_g, na.rm = TRUE)
minim4 <- min(penguins$body_mass_g, na.rm = TRUE)
maxim4 <- max(penguins$body_mass_g, na.rm = TRUE)

est_body_weight <- c(media4, desvest4, minim4, maxim4)


nombreIslas <- levels(penguins$island)


islaTorgen <- penguins[penguins$island == "Torgersen",]
islaDream <- penguins[penguins$island == "Dream",]
islaBiscoe <- penguins[penguins$island == "Biscoe",]


countTorgen <- table(islaTorgen$species)
barplot(countTorgen, col = c("orange", "purple", "blue"))

countDream <- table(islaDream$species)
barplot(countDream,  col = c("orange", "purple", "blue"))

countBiscoe <- table(islaBiscoe$species)
barplot(countBiscoe, col= c("orange", "purple", "blue"))

str(chinstrap)
chinstrap$body_mass_g <- as.numeric(chinstrap$body_mass_g)

levels(penguins$species)

chinstrap <- penguins[penguins$species == "Chinstrap",]
p1 <- ggplot(chinstrap, aes(y= body_mass_g, group= sex, x= sex, fill= sex))+ 
  geom_boxplot()+ ylim(c(2500, 6500))+ ylab("Masa corporal (g)") + xlab("Sexo") +plot_annotation(title = "Chinstrap")

adelie <- penguins[penguins$species == "Adelie",]
p2 <- ggplot(adelie, aes(y= body_mass_g, group= sex, x= sex, fill= sex))+ 
  geom_boxplot()+ ylim(c(2500, 6500))+ ylab("Masa corporal (g)") +xlab("Sexo")+plot_annotation(title = "Adelie")

gentoo <- penguins[penguins$species == "Gentoo",]
p3 <-ggplot(gentoo, aes(y= body_mass_g,  x= sex, fill= sex), ylim= c(2500, 6500))+ 
  geom_boxplot()+ ylim(c(2500, 6500))+ ylab("Masa corporal (g)")+ plot_annotation(title = "Gentoo")
 

p1 / p2 / p3 + plot_annotation(title = "Body mass by species (a for Chinstrap, b for Adelie, c for Gentoo)",tag_levels = "a", tag_suffix = ")" )
