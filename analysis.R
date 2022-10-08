# packages
library(tidyverse)
library(broom)
library(estimatr)
library(modelsummary)
library(patchwork)
library(sjPlot)


# Party Imageries in White ---- 
ctrls <- " + white_pop "
setmd <- function(iv, dv){ paste(dv,'~',iv,ctrls) %>% formula }

baseline <- function(data,race = 'White'){
  reg <- function(party){
    model <- setmd(paste0(party,'_',race)) # 
    lm_robust(model,data = data) %>% tidy(conf.level = 0.95) }
  
  dem <-  reg('Democrat') ; ind <-  reg('Independent') ; gop <-  reg('Republican')
  results <- bind_rows(dem,ind,gop) %>% filter(grepl(race, term)) 
  return(results)
}  

setmd <- function(iv, dv = 'affct_pol'){ paste(dv,'~',iv,ctrls) %>% formula }

gop.w <- anes %>% filter(pid == 'Republican', white == 1) %>% 
  baseline('White') %>% mutate(sample = 'White Republicans')

dem.w <- anes %>% filter(pid == 'Democrat', white == 1) %>% 
  baseline('White') %>% mutate(sample = 'White Democrats')

dem.n <- anes %>% filter(pid == 'Democrat', white != 1) %>% 
  baseline('White') %>% mutate(sample = 'Non-White Democrats')

bind_rows(gop.w,dem.w,dem.n) %>% 
  mutate(term = str_extract(term, "(Democrat|Independent|Republican|gap)")) %>% 
  ggplot(aes(x = term, y = estimate,ymin = conf.low,ymax = conf.high,
             col = p.value > 0.05)) +
  geom_hline(yintercept = 0,lty = 2 ,alpha = 0.6) + 
  # geom_crossbar(width = 1/10) + 
  geom_label(aes(label = round(estimate,1), 
                 fontface = ifelse(p.value < 0.05,'bold','plain')),
             nudge_x = 0.32,size = 3,color = 'black', fill = alpha('white',1/3)) +
  geom_pointrange(size = 0.6) + 
  ylab('Coef. Estimate') + 
  facet_wrap(~sample) +  
  scale_colour_viridis_d(end = 0.6,direction = -1) + 
  # theme_light() + 
  theme_sjplot() +   # ylim(c(-50,50)) +
  theme(axis.title.x = element_blank(),
        axis.text = element_text(color = 'black'),
        aspect.ratio = 0.8,
        legend.position = 'none')

ggsave('figures/baseline.pdf',width = 10, height = 3.3)

# Contrast of Party Imageries -----

## functions ---- 

plot <- function(data){
  ggplot(data = data, aes(x = term, y = estimate, 
                          ymin = conf.low,ymax = conf.high,
                          col = p.value < 0.05)) +
    geom_hline(yintercept = 0,lty = 2 ,alpha = 0.6) + 
    # geom_crossbar(width = 1/10) + 
    geom_label(aes(label = round(estimate,1), 
                   fontface = ifelse(p.value < 0.05,'bold','plain')),
               nudge_x = 0.2,size = 3,color = 'black', fill = alpha('white',1/3)) +
    geom_pointrange(size = 0.6) + 
    ylab('Coef. Estimate') + 
    facet_wrap(~sample) +  
    scale_colour_viridis_d(end = 0.55 ) + 
    theme_light() +  # ylim(c(-40,40)) + 
    theme(axis.title.x = element_blank(),
          axis.text = element_text(color = 'black'),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'azure3'),
          aspect.ratio = 0.8,
          plot.subtitle = element_text(face = 'bold',hjust = 1/2),
          legend.position = 'none')
}

baseline2 <- function(data,race = 'White'){
  reg <- function(party){
    model <- setmd('gap') # paste0(party,'_',race)
    lm_robust(model,data = data) %>% tidy }
  
  dem <-  reg('Democrat') ; ind <-  reg('Independent') ; gop <-  reg('Republican')
  results <- bind_rows(dem,ind,gop) %>% filter(grepl('gap', term)) 
  return(results)
}  

## Affective Polarization ---- 

setmd <- function(iv, dv = 'affct_pol'){ paste(dv,'~',iv,ctrls) %>% formula }

gop.w <- anes %>% filter(pid == 'Republican', white == 1) %>% 
  baseline2('White') %>% mutate(sample = 'White Republicans')

dem.w <- anes %>% filter(pid == 'Democrat', white == 1) %>% 
  baseline2('White') %>% mutate(sample = 'White Democrats')

dem.n <- anes %>% filter(pid == 'Democrat', white != 1) %>% 
  baseline2('White') %>% mutate(sample = 'Non-White Democrats')

affective <- 
  bind_rows(gop.w,dem.w,dem.n) %>% 
  mutate(term = str_extract(term, "(Democrat|Independent|Republican|gap)")) %>% 
  mutate(term = 'Imagery Gap') %>% plot 

affective
ggsave('figures/gap-affective.pdf',width = 10, height = 3)

## In-Group Feelings ---- 

setmd <- function(iv, dv = 'fl_ingroup'){ paste(dv,'~',iv,ctrls) %>% formula }

gop.w <- anes %>% filter(pid == 'Republican', white == 1) %>% 
  baseline2('White') %>% mutate(sample = 'White Republicans')

dem.w <- anes %>% filter(pid == 'Democrat', white == 1) %>% 
  baseline2('White') %>% mutate(sample = 'White Democrats')

dem.n <- anes %>% filter(pid == 'Democrat', white != 1) %>% 
  baseline2('White') %>% mutate(sample = 'Non-White Democrats')

ingroup <- 
bind_rows(gop.w,dem.w,dem.n) %>% 
  mutate(term = str_extract(term, "(Democrat|Independent|Republican|gap)")) %>% 
  mutate(term = 'Imagery Gap') %>% plot + ggtitle(NULL,'A. In-Group Feeling')

## Out-Group Feelings ---- 

setmd <- function(iv, dv = 'fl_outgroup'){ paste(dv,'~',iv,ctrls) %>% formula }

gop.w <- anes %>% filter(pid == 'Republican', white == 1) %>% 
  baseline2('White') %>% mutate(sample = 'White Republicans')

dem.w <- anes %>% filter(pid == 'Democrat', white == 1) %>% 
  baseline2('White') %>% mutate(sample = 'White Democrats')

dem.n <- anes %>% filter(pid == 'Democrat', white != 1) %>% 
  baseline2('White') %>% mutate(sample = 'Non-White Democrats')

outgroup <- 
bind_rows(gop.w,dem.w,dem.n) %>% 
  mutate(term = str_extract(term, "(Democrat|Independent|Republican|gap)")) %>% 
  mutate(term = 'Imagery Gap') %>% plot + ggtitle(NULL,'B. Out-Group Feeling')

ingroup / outgroup 
ggsave('figures/gap-decomposition.pdf')


# Mechanism: Group Affect ---- 

plot <- function(df){
  ggplot(data = df,aes(x = valence, y = estimate, 
                       ymin = conf.low,ymax = conf.high,
                       col = p.value < 0.05)) +
    geom_hline(yintercept = 0,lty = 2 ,alpha = 0.6) + 
    geom_label(aes(label = round(estimate,1), 
                   fontface = ifelse(p.value < 0.05,'bold','plain')),
               nudge_x = 0.25,size = 3,color = 'black', fill = alpha('white',1/3)) +
    geom_pointrange(size = 0.6) + 
    ylab('Coef. Estimate') + 
    facet_grid(~sample) +  
    scale_colour_viridis_d(end = 0.55 ) + 
    theme_light() +  # ylim(c(-40,40)) + 
    theme(axis.title.x = element_blank(),
          axis.text = element_text(color = 'black'),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'azure3'),
          aspect.ratio = 0.8,
          plot.subtitle = element_text(face = 'bold',hjust = 1/2),
          legend.position = 'none')
}

## White Feeling ---- 

by.group <- function(dv){
  gop.w <- 
    anes %>% 
    filter(white == 1, pid3 == 'Republican',!is.na(ft_white)) %>% 
    mutate(valence = ifelse(ft_white > 50, "Favor Whites","Disfavor Whites")) %>% 
    nest(data = -c(valence)) %>% 
    mutate(fit = map(data, 
                     ~ lm_robust(setmd('gap',dv), data = .x)),
           results = map(fit, ~tidy(.x) %>% filter(term == 'gap'))) %>% 
    unnest(cols = results) %>% 
    mutate(sample = 'White Republicans')
  
  dem.w <- 
    anes %>% 
    filter(white == 1, pid3 == 'Democrat', !is.na(ft_white)) %>% 
    mutate(valence = ifelse(ft_white > 50, "Favor Whites","Disfavor Whites")) %>% 
    nest(data = -c(valence)) %>% 
    mutate(fit = map(data, 
                     ~ lm_robust(setmd('gap',dv), data = .x)),
           results = map(fit, ~tidy(.x) %>% filter(term == 'gap'))) %>% 
    unnest(cols = results) %>% 
    mutate(sample = 'White Democrats')
  
  dem.n <- 
    anes %>% 
    filter(white == 0, pid3 == 'Democrat', !is.na(ft_white)) %>% 
    mutate(valence = ifelse(ft_white > 50, "Favor Whites","Disfavor Whites")) %>% 
    nest(data = -c(valence)) %>% 
    mutate(fit = map(data, 
                     ~ lm_robust(setmd('gap',dv), data = .x)),
           results = map(fit, ~tidy(.x) %>% filter(term == 'gap'))) %>% 
    unnest(cols = results) %>% 
    mutate(sample = 'Non-White Democrats')
  
  return(bind_rows(gop.w,dem.w,dem.n) %>% select(-fit,-data))
}

by.group('affct_pol') %>% plot + ggtitle(NULL,'Affective Polarization')

ingroup <- by.group('fl_ingroup') %>% plot + ggtitle(NULL,'A. In-Group Feeling')
outgroup <- by.group('fl_outgroup') %>% plot + ggtitle(NULL,'B. Out-Group Feeling')

ingroup/outgroup
ggsave('figures/mechanism-ft.pdf',width = 10,height = 6.6)

## Continuous Racial Identities ---- 

by.group <- function(dv){
  gop.w <- 
    anes %>% 
    filter(white == 1, pid3 == 'Republican',!is.na(raceid)) %>% 
    mutate(valence = ifelse(raceid > 3, "Important","Not Important")) %>% 
    nest(data = -c(valence)) %>% 
    mutate(fit = map(data, 
                     ~ lm_robust(setmd('gap',dv), data = .x)),
           results = map(fit, ~tidy(.x) %>% filter(term == 'gap'))) %>% 
    unnest(cols = results) %>% 
    mutate(sample = 'White Republicans')
  
  dem.w <- 
    anes %>% 
    filter(white == 1, pid3 == 'Democrat', !is.na(raceid)) %>% 
    mutate(valence = ifelse(raceid > 3, "Important","Not Important")) %>% 
    nest(data = -c(valence)) %>% 
    mutate(fit = map(data, 
                     ~ lm_robust(setmd('gap',dv), data = .x)),
           results = map(fit, ~tidy(.x) %>% filter(term == 'gap'))) %>% 
    unnest(cols = results) %>% 
    mutate(sample = 'White Democrats')
  
  dem.n <- 
    anes %>% 
    filter(white == 0, pid3 == 'Democrat', !is.na(raceid)) %>% 
    mutate(valence = ifelse(raceid > 3, "Important","Not Important")) %>% 
    nest(data = -c(valence)) %>% 
    mutate(fit = map(data, 
                     ~ lm_robust(setmd('gap',dv), data = .x)),
           results = map(fit, ~tidy(.x) %>% filter(term == 'gap'))) %>% 
    unnest(cols = results) %>% 
    mutate(sample = 'Non-White Democrats')
  
  return(bind_rows(gop.w,dem.w,dem.n) %>% select(-fit,-data))
}

by.group('affct_pol') %>% plot + ggtitle(NULL,'Affective Polarization')

ingroup <- by.group('fl_ingroup') %>% plot + ggtitle(NULL,'A. In-Group Feeling')
outgroup <- by.group('fl_outgroup') %>% plot + ggtitle(NULL,'B. Out-Group Feeling')

ingroup/outgroup

ggsave('figures/mechanism-raceid-2.pdf',width = 9, height = 6)


## Mechanism on All Sample ---- 

### using jtools ---- 
# 
# gop.ft <- 
#   johnson_neyman(lm(fl_gop ~ ft_white*gap,anes),
#                pred = 'ft_white',modx = 'gap', title = NULL)$plot + 
#   theme_bw() + 
#   ylab('Effect of Racial Considerations') + 
#   xlab('Contrast of Racial Imageries') + 
#   ggtitle(NULL,'DV: Feeling of Republicans') + 
#   theme(legend.position = 'none',
#         aspect.ratio = 1)
# 
# dem.ft <- 
#   johnson_neyman(lm(fl_dem ~ ft_white*gap,anes),
#                  pred = 'ft_white',modx = 'gap', title = NULL)$plot + 
#   theme_bw() + 
#   ylab('Effect of Racial Considerations') + 
#   xlab('Contrast of Racial Imageries') + 
#   ggtitle(NULL,'DV: Feeling of Democrats') + 
#   theme(legend.position = 'none',
#         aspect.ratio = 1)
# 
# gap.ft <- 
#   johnson_neyman(lm(gop_over_dem ~ ft_white*gap,anes),
#                  pred = 'ft_white',modx = 'gap', title = NULL)$plot + 
#   theme_bw() + 
#   ylab('Effect of Racial Considerations') +
#   xlab('Contrast of Racial Imageries') + 
#   ggtitle(NULL,'DV: Feeling of Republicans over Democrats') + 
#   theme(legend.position = 'none',
#         aspect.ratio = 1)
# 
# gop.ft + dem.ft + gap.ft 
# ggsave('figures/mechanism-all-sample.pdf',width = 12.9,height = 4.2)
# 

### using sjPlot ---- 

plot_model(lm(fl_gop ~ ft_white*gap,anes), type = "pred", 
           terms = c("ft_white", "gap[0,50]")) + 
  ggtitle(NULL,'DV: Feeling of Republicans')  + theme_bw()

plot_model(lm(fl_dem ~ ft_white*gap,anes), type = "pred", 
           terms = c("ft_white", "gap[0,50]")) + 
  ggtitle(NULL,'DV: Feeling of Republicans')  + theme_bw()


model <- lm(gop_over_dem ~ ft_white*gap,anes)

sj <- function(reg,title = NULL){
  plot_model(reg, type = "pred", 
             terms = c("ft_white", "gap[0,50]")) + 
    ylab('Linear Predicted Value') +
    xlab('Feeling toward Whites') + 
    ggtitle(NULL,title)  + 
    scale_color_viridis_d(option = 'viridis',end = 0.5, 
                          name = 'Contrast of Party Imageries') + 
    scale_fill_viridis_d(option = 'viridis',end = 0.5) + 
    theme_bw() + 
    theme(aspect.ratio = 1,
          legend.position = 'bottom')
}


( 
  sj(lm(fl_gop ~ ft_white*gap + white_pop,anes), 'DV: Feelings toward Republicans') + 
    sj(lm(fl_dem ~ ft_white*gap + white_pop,anes), 'DV: Feelings toward Democrats') + 
    sj(lm(gop_over_dem ~ ft_white*gap+ white_pop,anes), 'DV: GOP - Dem Feeling') ) +   
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')

ggsave('figures/mechanism-all-sample.pdf',width = 11,height = 5)


