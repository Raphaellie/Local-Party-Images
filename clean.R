# packages
library(tidyverse)
library(readstata13)
library(viridisLite)
library(wesanderson)
library(sf)
library(broom)
library(estimatr)
library(lfe)
library(tidycensus)
library(patchwork)

# pre-cleaned CCES16&20 data ----
cces <- read_csv('input/CCES/cces cleaned.csv') %>% 
  # filter(year != 2018) %>% 
  mutate(pid = case_when(pid7 < 4 ~ 'Democrat',
                         pid7 > 4 ~ 'Republican',
                         pid7 == 4 ~ 'Independent',
                         TRUE ~ NA_character_),
         race  = ifelse(race > 4, 5,race),
         race = factor(race,labels = c('White','Black','Hispanic','Asian','Other')))

## calculate white images of parties ----
dists <- cces %>% 
  filter(!is.na(pid7)) %>% 
  group_by(cd,state,state2,dist,pid,race) %>% 
  summarise(n = n()) %>% 
  group_by(cd,pid) %>% 
  mutate(share = n/sum(n)) %>% 
  group_by(cd) %>% 
  mutate(all_in_dist = sum(n))

## reshape from long_to_wide
dists <- dists %>% 
  filter(race %in% c('White','Black','Hispanic')) %>% 
  select(-n,) %>% 
  # group_by(cd,state,state2,dist,race) %>% 
  pivot_wider(values_from = share,names_from = c(pid,race), names_sep = '_')

# rename at-large districts to match shape files
at.large <- c('AK','VT','ND',"SD",'WY','DE') 

dists <- 
  dists %>% 
  mutate(CDLABEL = as.character(dist),
         CDLABEL = ifelse(state2 %in% at.large,state2,CDLABEL),
         STATEAB = state2)

## mapping using Daily Kos Shapes ----
cd.shape <- st_read('input/HexCD/HexCDv30.shp')

cd <- left_join(cd.shape,dists,by = c('STATEAB','CDLABEL'))


map.dem <- 
cd %>% 
  ggplot() + 
  geom_sf(aes(fill = Democrat_White ),
          lwd = 1/10,col = 'gray20', alpha = 0.7)  +
  scale_fill_binned(breaks = seq(0,1,0.2)) +
  scale_fill_viridis_b(option = 'mako', begin = 0.3, end = 0.95,direction = -1,
                       name = NULL) +
  ggtitle(NULL,'A. White Imagery of Democrats') + 
  theme_void() +
  theme(legend.position = 'bottom',
        plot.subtitle = element_text(face = 'bold'),
        aspect.ratio = 0.68)

map.gop <- 
cd %>% 
  ggplot() + 
  geom_sf(aes(fill = Republican_White ),
          lwd = 1/10,col = 'gray20', alpha = 0.7)  +
  scale_fill_binned(breaks = seq(0,1,0.2)) +
  scale_fill_viridis_b(option = 'cividis', begin = 0.2, end = 0.9,direction = -1,
                       name = NULL) +
  ggtitle(NULL,'B. White Imagery of Republicans') + 
  theme_void() +
  theme(legend.position = 'bottom',
        plot.subtitle = element_text(face = 'bold'),
        aspect.ratio = 0.68)
map.dem + map.gop 
ggsave('figures/map-test.pdf',scale = 1)

# CD demographics ---- 
cd.acs <- 
  get_acs(geography = "congressional district", year = 2020, output = 'tidy',survey = 'acs5',
          variables = c("B01003_001","B03002_003",'B02009_001',"B03001_003"))

## collect geo info
cd.races <- cd.acs %>% 
  rename(var = variable) %>% 
  mutate(var = ifelse(var == "B01003_001",'all',var),
         var = ifelse(var == "B03002_003",'white_pop',var),  
         var = ifelse(var == 'B02009_001','black_pop',var),
         var = ifelse(var == "B03001_003",'hispanic_pop',var),)

## change variable name to races
cd.races <- 
  cd.races %>% 
  mutate(state = str_extract(NAME,', .*') %>% str_sub(start = 3) ,
         dist = str_extract(NAME,'(.*),') %>% str_sub(end = -2)) %>% 
  mutate(state2 = state.abb[match(state,state.name)],
         dist = str_extract(NAME,"[0-9]{1,2}")) %>%
  mutate(dist = ifelse(grepl('at Large',NAME),1,dist)) # consider at-large districts

## long to wider
cd.races <- 
  cd.races %>% 
  select(-moe) %>% 
  group_by(state,dist) %>%  
  mutate(pop = first(estimate), estimate = estimate / first(estimate)) %>% 
  pivot_wider(names_from = var,values_from = estimate) %>% 
  mutate(dist = as.numeric(dist))


# pre-cleaned ANES2020 data ----

anes <- read_csv('input/ANES/anes cleaned.csv') %>% 
  mutate(pid3 = factor(pid3, labels = c('Democrat','Independent','Republican')),
         pid = case_when(pid7 < 4 ~ 'Democrat',
                         pid7 == 4 ~ 'Independent',
                         pid7 > 4 ~ 'Republican', 
                         TRUE ~ NA_character_)) %>% 
  mutate(affct_pol = (fl_ingroup - fl_outgroup) ,
         gop_over_dem  = (fl_gop - fl_dem) ,
         whiteid = 6 - whiteid,
         blackid = 6 - blackid,
         hispanid = 6 - hispanid,
         raceid = 6 - raceid)

anes <- anes %>% 
  left_join(cd.races, by = c('state2','dist')) %>%
  left_join(dists,by = c('state2','dist'))  

anes <- anes %>% 
  mutate(gap = (Republican_White - Democrat_White)*100,
         gap_black =(Democrat_Black - Republican_Black)*100,
         gap_hispanic =(Democrat_Hispanic - Republican_Hispanic)*100)

## data viz ----
# anes %>% 
#   filter(pid3 == 'Republican') %>% 
#   mutate(iv = Republican_White - white_pop) %>% 
#   group_by(iv) %>% 
#   summarise(affct_pol = mean(affct_pol, na.rm = T)) %>% 
#   ggplot(aes(iv, affct_pol)) + 
#   geom_point(alpha = 1/10) + 
#   geom_smooth(method = 'lm')


