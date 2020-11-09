### Home advantage experiment post-Covid restart

# import libraries

library(dplyr)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggplot)

# read data

prem1819 <- read.csv("./Data/PremierLeague1819.csv")
prem1920 <- read.csv("./Data/PremierLeague1920.csv")
prem2021 <- read.csv("./Data/PremierLeague2021.csv")

bund1819 <- read.csv("./Data/Bundesliga1819.csv")
bund1920 <- read.csv("./Data/Bundesliga1920.csv")
bund2021 <- read.csv("./Data/Bundesliga2021.csv")

champ1819 <- read.csv("./Data/Championship1819.csv")
champ1920 <- read.csv("./Data/Championship1920.csv")
champ2021 <- read.csv("./Data/Championship2021.csv")

liga1819 <- read.csv("./Data/LaLiga1819.csv")
liga1920 <- read.csv("./Data/LaLiga1920.csv")
liga2021 <- read.csv("./Data/LaLiga2021.csv")

serie1819 <- read.csv("./Data/SerieA1819.csv")
serie1920 <- read.csv("./Data/SerieA1920.csv")
serie2021 <- read.csv("./Data/SerieA2021.csv")


# join league data

columns <- c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "Covid", "B365H", "B365D", "B365A", "HY", "HC", "HR", "AY", "AC", "AR")

prem <- rbind(rbind(prem1819[, columns], prem1920[, columns]), prem2021[, columns])
bund <- rbind(rbind(bund1819[, columns], bund1920[, columns]), bund2021[, columns])
champ <- rbind(rbind(champ1819[, columns], champ1920[, columns]), champ2021[, columns])
liga <- rbind(rbind(liga1819[, columns], liga1920[, columns]), liga2021[, columns])
serie <- rbind(rbind(serie1819[, columns], serie1920[, columns]), serie2021[, columns])

prem <- rbind(rbind(rbind(rbind(prem, bund), champ), liga), serie)

# implied probability - overround reversion

prem[, 'B365SUM'] <- (1 / prem$B365H) + (1 / prem$B365D) + (1 / prem$B365A)

prem[, 'B365HP'] <- (1 / prem$B365H) - (((1 / prem$B365H) / prem$B365SUM) * (prem$B365SUM - 1))
prem[, 'B365DP'] <- (1 / prem$B365D) - (((1 / prem$B365D) / prem$B365SUM) * (prem$B365SUM - 1))
prem[, 'B365AP'] <- (1 / prem$B365A) - (((1 / prem$B365D) / prem$B365SUM) * (prem$B365SUM - 1))

# result

prem$HW <- prem$FTHG > prem$FTAG
prem$D <- prem$FTAG == prem$FTHG
prem$AW <- prem$FTHG < prem$FTAG

# simple averages

prem %>% group_by(Covid) %>% summarise(home_win_forecast = mean(B365HP, na.rm=TRUE), home_win = mean(HW, na.rm=TRUE))

# breaking down by forecast range

bins = c(0, 0.2, 0.3, 0.4, 0.5, 0.6, 1)

prem[, 'B365HPBIN'] <- cut(prem$B365HP, bins, include.lowest = TRUE, right = FALSE)

# averages

prem <- prem[0 == is.na(prem$B365HPBIN), ]

prem[, 'Time Period'] <- ifelse(prem$Covid, "Post-April 2020", "Pre-April 2020")
prem$`Time Period` <- as.factor(prem$`Time Period`)

prem$`Home Win Forecast Probability` <- prem$B365HP

# group over prob range

grouped_data <- prem %>% group_by(Covid, B365HPBIN) %>% summarise(home_win_forecast = mean(B365HP, na.rm=TRUE), home_win = mean(HW, na.rm=TRUE), count = length(HW))

# plotting: data frame

df <- data.frame(bin = grouped_data[1:6, 'B365HPBIN'],
                 "Count Pre-April 2020" = grouped_data[1:6, 'count'],
                 "Count Post-April 2020" = grouped_data[7:12, 'count'],
                 "Pre-April 2020" = grouped_data[1:6, 'home_win'],
                 "Post-April 2020" = grouped_data[7:12, 'home_win'],
                 colors = ifelse(grouped_data[1:6, 'home_win'] < grouped_data[7:12, 'home_win'],
                                 rgb(247 / 256, 108 / 256, 87 / 256),
                                 rgb(85 / 256, 148 / 256, 242 /256)))

names(df) <- c("bin", "Count Pre-April 2020", "Count Post-April 2020", "Pre-April 2020", "Post-April 2020", "colors")

# plotting: error bars  - standard deviation of sample mean

stdvec <- sqrt(c(0.1, 0.25, 0.35, 0.45, 0.55, 0.8) * (1 - c(0.1, 0.25, 0.35, 0.45, 0.55, 0.8)))

df[, 'Error Pre-April 2020'] <- stdvec / sqrt(df[, 'Count Pre-April 2020'])
df[, 'Error Post-April 2020'] <- stdvec / sqrt(df[, 'Count Post-April 2020'])

# plotting

ggplot(df) +
  geom_segment( aes(x=bin, xend=bin, y=`Pre-April 2020`, yend=`Post-April 2020`, color = colors), size = 3) +
  geom_point( aes(x=bin, y=`Post-April 2020`, color = colors), size=7, alpha = 1) +
  geom_errorbar(aes(x = bin, ymin=`Post-April 2020` - 1 * `Error Post-April 2020`, ymax=`Post-April 2020` + 1 * `Error Post-April 2020`), width=.2, size = 1, alpha = 0.8,
                position=position_dodge(.9)) +
  scale_colour_manual(values = rev(unique(df$colors))) + 
  coord_flip()+
  theme_bw() + theme(panel.border = element_blank(), legend.box = "horizontal") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +  
  xlab("Home Win Forecast Probabilitiy") +
  ylab("Home Win Percentage") + theme(legend.title=element_blank(), legend.position="none") + 
  theme(  
  axis.title.x = element_text(color="forestgreen", vjust=-1),
    axis.title.y = element_text(color="forestgreen" , vjust=3)   
  ) + theme(text=element_text(size=16, color = "forestgreen"))
