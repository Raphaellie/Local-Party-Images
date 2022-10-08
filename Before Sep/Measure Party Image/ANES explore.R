
# Prepare -----------------------------------------------------------------
## packages
library(tidyverse)
library(readstata13)
library(reshape)
library(lfe)
library(ggpubr)
rm(list = ls())

## ANES cumulative data
anes <- read.dta13('data/ANES cleaned.dta')

anes <- anes %>% 
  filter(pid3 == 1 | pid3 == 3)

anes$ft_in <- ifelse(anes$pid3 == 1, anes$ft_dem,anes$ft_gop)
anes$ft_out <- ifelse(anes$pid3 == 1, anes$ft_gop,anes$ft_dem)

# Trend of Party Feelings -------------------------------------------------

pft <- anes %>% 
  filter(is.na(ft_in) == 0, is.na(ft_black) == 0) %>% 
  group_by(year) %>% 
  summarize(ft_in = mean(ft_in,na.rm = TRUE),
            ft_out = mean(ft_out,na.rm = TRUE),
            afp = mean(ft_in - ft_out, na.rm = TRUE),
            ft_white = mean(ft_white,na.rm = TRUE), 
            ft_black = mean(ft_black,na.rm = TRUE),n = n())

afp <- anes %>% 
  filter(is.na(ft_in) == 0) %>% 
  mutate(afp = ft_in - ft_out) %>% 
  group_by(year) %>% 
  summarise(afp_mean = mean(afp, na.rm = TRUE),
            se = sqrt(var(afp,na.rm = TRUE)/n()),
            ciup = afp_mean + qnorm(.975)*se,
            cilow = afp_mean - qnorm(.975)*se)

## due to soem dplyr bug, need to as.data.frame() first :-)
pft2 <- melt(as.data.frame(pft), id = "year", measure = c('ft_in','ft_out','afp'))

feeling.trend <- ggplot(pft2[pft2$variable %in% c('ft_in','ft_out'),],
                        aes(x = year, y = value, linetype = variable)) + 
  geom_line(color = 'navyblue',alpha = 0.7) + 
  geom_point(size = 2.6,color = 'navyblue') + 
  geom_hline(yintercept = 50, linetype = 2) + 
  xlab("Year of Survey") + ylab('Feeling Thermometer') + 
  scale_linetype_manual(values = c(1, 2),
                        labels = c("In-Party Feeling", "Out-Party Feeling")) + 
  theme_bw() + 
  theme(legend.title = element_blank(),
        legend.position = c(0.22,0.23),
        legend.background = element_rect(color = 'black',fill = 0))
feeling.trend


afp.trend <- ggplot(afp,aes(x = year, y = afp_mean, ymax = ciup, ymin = cilow )) + 
  geom_line(linetype = 3, color = 'navyblue') + geom_point(color = 'navyblue') + 
  geom_ribbon(fill = 'dodgerblue3',alpha = 0.15) + 
  xlab("Year of Survey") + ylab('Affective Polarization') + 
  theme_bw()
afp.trend

ggarrange(feeling.trend, afp.trend,ncol = 2,labels = 'AUTO')

ggsave('graphs/feeling-trend.pdf',width = 9, height = 4)


# Race-Party Alignment --------------------------------------------------------

races <- anes %>% 
  filter(is.na(race) == 0) %>% 
  group_by(pid3,year) %>% 
  summarise(white = mean(white), black = mean(black), hispanic = mean(hispan)) %>% 
  mutate(pid3 = as.factor(pid3))

white <- ggplot(races,aes(x = year,y = white, color = pid3)) + 
  geom_point() + theme_bw() + 
  scale_color_manual(name = 'Party',
                     values = c("dodgerblue4", "firebrick3"), # c("#00BFC4","dodgerblue4")
                     labels = c('Democrats','Republicans')) + 
  xlab("Year of Survey") + ylab('% White') + 
  ggtitle("White Presence in Two Parties") + 
  theme(plot.title = element_text(face = 'bold',size = 12))

black <- ggplot(races,aes(x = year,y = black, color = pid3)) + 
  geom_point() + theme_bw() + 
  scale_color_manual(name = 'Party',
                     values = c("dodgerblue4", "firebrick3"), # c("#00BFC4","dodgerblue4")
                     labels = c('Democrats','Republicans')) + 
  xlab("Year of Survey") + ylab('% Black') + 
  ggtitle("Black Presence in Two Parties") + 
  theme(plot.title = element_text(face = 'bold',size = 12))

race.trend <- ggarrange(white, black, common.legend = TRUE, legend = 'bottom')
ggsave('graphs/race-trend.pdf',race.trend, width = 9,height = 4.5)


# Race-Politics Link --------------------------------------------------------

model.dem <- as.formula(ft_dem ~ ft_white + ft_black + as.factor(race)|0|0|0)
model.gop <- as.formula(ft_gop ~ ft_white + ft_black + as.factor(race)|0|0|0)
years <- c(seq(1980,2000,4), seq(2004,2020,4))

## set a extraction function 
get.stats <- function(position, var, reg, note = NA) {
  stats <- data.frame(var = var, 
                      coef = reg$coefficients[position], 
                      se = reg$se[position], note = note)
  stats <- stats %>% mutate(ciup = coef + se * qnorm(.975), cilow = coef - se * qnorm(.975)) # get 95% CIs
  return(stats)
}

stats.dem <- data.frame()
stats.gop <- data.frame()

for (y in years) {
  ## reg on feelings toward Democrats
  reg.dem <- felm(model.dem, data = anes[anes$year == y,])
  stats.dem <- rbind(stats.dem,
                 get.stats(2,'ft_white',reg.dem,y),
                 get.stats(3,'ft_black',reg.dem,y))
  ## reg on feelings toward Republicans
  reg.gop <- felm(model.gop, data = anes[anes$year == y,])
  stats.gop <- rbind(stats.gop,
                     get.stats(2,'ft_white',reg.gop,y),
                     get.stats(3,'ft_black',reg.gop,y))
}

stats.dem <- stats.dem %>% mutate(year = note)
stats.gop <- stats.gop %>% mutate(year = note)

dynamic.coef <- function(results) {
  plot <- ggplot(data = results, aes(x = year, y = coef, ymax = ciup, ymin = cilow)) + # geom_vline(xintercept = 2000,linetype = 2, size = 0.6, alpha = 0.5) +  # reference for any year
    geom_hline(yintercept = 0,linetype = 2, size = 0.6, alpha = 0.5) +  # reference for effect
    geom_ribbon(aes(fill = var), alpha = 0.15) + # shaded area for 95% CIs
    geom_line(aes(color = var),alpha = 0.75) + # line and points for coefficients
    geom_point(aes(color = var),size = 3)+ # aes(size = abs(coef)
    theme_bw() + 
    xlab('Year of Survey') + 
    ylab('Estimated Effect') + # scale_x_continuous(breaks = span[1]:span[2]) + 
    labs(color = 'Explaining Variable', fill = 'Explaining Variable') + 
    scale_color_hue(labels = c('Black Feeling', 'White Feeling')) + 
    scale_fill_hue(labels = c('Black Feeling', 'White Feeling')) + 
    theme(plot.title = element_text(face = 'bold',size = 12),
          panel.grid.minor.x = element_blank(), 
          legend.position = 'bottom')
  return(plot)
  }

dem <- dynamic.coef(stats.dem) + ggtitle('Feelings toward Democrats')
gop <- dynamic.coef(stats.gop) + ggtitle('Feelings toward Republicans')
coef.trend <- ggarrange(dem, gop,ncol=2,  common.legend = TRUE, legend="bottom")
coef.trend
ggsave('graphs/coef-trend.pdf',coef.trend,width = 11,height = 6)

dynamic.coef(stats.dem[stats.dem$var == 'ft_black',])
dynamic.coef(stats.dem[stats.dem$var == 'ft_white',])
dynamic.coef(stats.gop[stats.gop$var == 'ft_black',])
dynamic.coef(stats.gop[stats.gop$var == 'ft_white',])


# Racial Resentment & Politics --------------------------------------------------------










