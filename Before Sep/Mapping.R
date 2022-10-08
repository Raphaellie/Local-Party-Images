rm(list = ls())

# packages
library(ggplot2)
library(dplyr)
library(plyr)
library(readstata13)
library(sp)
library(tigris)
options(tigris_use_cache = TRUE)
library(tidycensus)
library(lfe)
library(lemon)
library(usmap)
library(ggridges)
library(ggpubr)

library(hash)

# Load shape files
counties <- st_read('shape/counties/gz_2010_us_050_00_500k.shp')
states <- st_read('shape/states/gz_2010_us_040_00_500k.shp')

# Load county-level political data
politics <- read.dta13('data/outliner.dta')

# Get FIPS code & Keep only mainland states
counties$GEOID <- str_sub(counties$GEO_ID, start = -5)
counties$State <- strtoi(counties$STATE)
states$State <- strtoi(states$STATE)
counties <- counties %>% 
  filter(State < 60 & State != 2 & State != 15)
states <- states %>% 
  filter(State < 60 & State != 2 & State != 15)

#A function to remove map axes
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# Join geo and political data
politics <- politics %>% 
  mutate(GEOID = county_fips)
final <- left_join(counties,politics, by = 'GEOID' )

rm(counties, states)

# Plot Nationwide ---------------------------------------------------------

map_mgop <- final %>% 
  ggplot() + 
  geom_sf(aes(fill = pid7), lwd = 0) +
  theme_bw() + ditch_the_axes + 
  labs(fill = '% Minority Republicans') + 
  scale_fill_gradient(low = 'grey95', high = 'turquoise4') +  
  theme(legend.position = 'bottom') + 
  geom_sf(data = states, fill = NA, color = 'white') + 
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000)) 

ggsave('Minority Republican Geo.jpeg', width = 9, height = 6, dpi = 600)


# Plot Singular State -----------------------------------------------------

## a dictionary for variable desciprtions
vdesc <- hash()
vdesc[['mgop']] <- 'Minority Republicans'
vdesc[['wdem']] <- 'White Democrats'
vdesc[['pid7']] <- "Dem-Rep Partisanship"
vdesc[['ideo5']] <- "Ideology"
vdesc[['pstr']] <- "Partisan Strength"
vdesc[['istr']] <- "Ideological Strength"

## function for one-state plotting
plot.state <- function( sname = 'Indiana', var = 'wdem', color = 'deepskyblue4') {
  
final_s <- final %>% mutate(fips = county_fips)
df <- us_map(regions = 'counties')
final_s <- left_join(df, final_s, by = "fips")
all <- final_s[,c("fips",var)]

mgop_in <- plot_usmap(regions = "counties", 
                      include = c(sname), data = all, values = var, color = 'grey80',alpha = 0.9) + 
  theme_bw() + ditch_the_axes + 
  labs(fill = "Percentage") + 
  scale_fill_gradient(low="grey95", high=color) + # turquoise4 dodgerblue4 red4 deepskyblue4 steelblue4
  ggtitle(sname, 
          subtitle = vdesc[[var]])  + 
  theme(legend.position="left") + 
  theme(plot.title = element_text(size = 16, face = 'bold'),
        plot.subtitle = element_text(face = 'plain'), 
        text = element_text(family = 'Helvetica'))
return(mgop_in)    
}

indiana <- plot.state('Indiana')
california <- plot.state('California')
illinois <- plot.state('Illinois')
ohio <- plot.state('Ohio','pstr')
arizona <- plot.state('Arizona','pstr')


combined <- ggarrange(indiana, california, ohio, ncol = 3, 
                      common.legend = TRUE, legend = 'bottom')
ggsave('3 states.pdf', combined, width = 12, height = 6,dpi = 600)




