# packages
library(tidyverse)
library(broom)
library(estimatr)
library(modelsummary)
library(patchwork)
library(ggeffects)
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
  baseline('White') %>% mutate(sample = 'Sample: White Republicans')

dem.w <- anes %>% filter(pid == 'Democrat', white == 1) %>% 
  baseline('White') %>% mutate(sample = 'Sample: White Democrats')

dem.n <- anes %>% filter(pid == 'Democrat', white != 1) %>% 
  baseline('White') %>% mutate(sample = 'Sample: Non-White Democrats')

fig.baseline <- 
bind_rows(gop.w,dem.w,dem.n) %>% 
  mutate(term = str_extract(term, "(Democrat|Independent|Republican|gap)")) %>% 
  ggplot(aes(x = term, y = estimate,ymin = conf.low,ymax = conf.high,
             col = p.value > 0.05)) +
  geom_hline(yintercept = 0,lty = 2 ,alpha = 0.6) + 
  # geom_crossbar(width = 1/10) + 
  geom_label(aes(label = round(estimate,1), 
                 fontface = ifelse(p.value < 0.05,'bold','plain')),
             nudge_x = 0.32,size = 3,color = 'black', fill = alpha('white',1/3)) +
  geom_pointrange() + 
  ylab('Coef. Estimate') + 
  facet_wrap(~sample) +  
  scale_colour_viridis_d(end = 0.6,direction = -1) + 
  # theme_light() + 
  theme_sjplot() +   # ylim(c(-50,50)) +
  theme(axis.title.x = element_blank(),
        axis.text = element_text(color = 'black'),
        aspect.ratio = 0.8,
        # strip.text = element_text(color = 'black'),
        # strip.background = element_blank(),
        legend.position = 'none')

fig.baseline

ggsave('figures/baseline.pdf',width = 10, height = 3.3)

# Contrast of Party Imageries -----

## functions ---- 

plot2 <- function(data){
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
  # reg <- function(party){
    # model <- setmd('gap') # paste0(party,'_',race)
    # lm_robust(model,data = data) %>% tidy }
  # dem <-  reg('Democrat') ; ind <-  reg('Independent') ; gop <-  reg('Republican')
  # results <- bind_rows(dem,ind,gop) %>% filter(grepl('gap', term)) 
  results <- lm_robust(setmd('gap'), data = data) %>% 
      tidy %>% filter(grepl('gap', term))
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

in_out <-   bind_rows(gop.w,dem.w,dem.n) %>% 
  mutate(term = str_extract(term, "(Democrat|Independent|Republican|gap)")) %>% 
  mutate(term = 'Imagery Gap')

fig.contrast <- 
  in_out %>% plot2 

fig.contrast

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
  mutate(term = 'Imagery Gap') # %>% plot2 + ggtitle(NULL,'A. In-Group Feeling')

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
  mutate(term = 'Imagery Gap') # %>% plot2 + ggtitle(NULL,'B. Out-Group Feeling')

## Combine ---- 

fig.decompose <- 
  bind_rows(in_out, outgroup,ingroup) %>% 
  mutate(outcome = factor(outcome,levels = c('affct_pol','fl_ingroup','fl_outgroup'),
                          labels = c('Affective Polarization','In-Group Feelings','Out-Group Feelings')),
         outcome = paste('DV:', outcome)) %>% 
  ggplot( aes(x = sample, y = estimate, ymin = conf.low,ymax = conf.high,
                          col = p.value < 0.05)) +
  geom_hline(yintercept = 0,lty = 2 ,alpha = 0.9) + 
  geom_label(aes(label = round(estimate,1),
                 fontface = ifelse(p.value < 0.05,'bold','plain')),
             nudge_x = -0.3,size = 3,color = 'black', fill = alpha('white',0.8)) +
  geom_pointrange() + 
  ylab('Coefficient Estimate of Party Imagery Contrast') + 
  facet_wrap(~outcome,) +  
  coord_flip() + 
  scale_colour_viridis_d(option = 'plasma',end = 0.6 ) +
  theme_sjplot() +  # ylim(c(-40,40)) + 
  theme(axis.title.y = element_blank(),
        axis.text = element_text(color = 'black'),
        aspect.ratio = 0.8,
        legend.position = 'none')

fig.decompose

ggsave('figures/gap-decomposition.pdf')


# Mechanism: Group Affect ---- 

plot1 <- function(df){
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
  
  df <- anes %>% 
    mutate(sample = NA,
           sample = ifelse(white == 1&pid3 == 'Republican','White Republicans',sample),
           sample = ifelse(white == 1&pid3 == 'Democrat','White Democrat',sample),
           sample = ifelse(white == 0&pid3 == 'Democrat','Non-White Democrats',sample) )
  
  results <- 
    df %>% 
    mutate(valence = ifelse(gap < 0.2, "Weak Gap","Strong Gap")) %>% 
    filter(!is.na(sample),!is.na(valence)) %>% 
    nest(data = -c(valence,sample)) %>% 
    mutate(fit = map(data, ~ lm_robust(setmd('ft_white',dv), data = .x)),
           results = map(fit,tidy)) %>% 
    unnest(cols = results) %>% filter(term == 'ft_white')
  
  return(results %>% select(-data,-fit))
}


by.group('affct_pol') %>% plot1 + ggtitle(NULL,'Affective Polarization')
ingroup <- by.group('fl_ingroup') %>% plot1 + ggtitle(NULL,'A. In-Group Feeling')
outgroup <- by.group('fl_outgroup') %>% plot1 + ggtitle(NULL,'B. Out-Group Feeling')
ingroup/outgroup

bind_rows(by.group('fl_gop'), 
          by.group('fl_dem')) %>% 
  mutate(outcome = factor(outcome,levels = c('affct_pol','fl_ingroup','fl_outgroup','fl_gop','fl_dem'),
                          labels = c('Affective Polarization','In-Group Feelings','Out-Group Feelings',
                                     'GOP Feeling','Dem Feeling'))) %>% 
  ggplot(aes(x = sample, y = estimate, ymin = conf.low,ymax = conf.high,
              color = valence)) +
  geom_hline(yintercept = 0,lty = 2 ,alpha = 0.9) + 
  geom_pointrange(position = position_dodge(width = 0.5)) +  # 
  facet_wrap(~outcome,) +  
  coord_flip() + 
  ylab('Coefficient Estimate of Party Imagery Contrast') + 
  scale_colour_viridis_d(option = 'cividis',end = 0.7 , direction = -1) +
  theme_minimal() +  # ylim(c(-0.2,0.4)) + 
  theme(axis.title.y = element_blank(),
        axis.text = element_text(color = 'black'),
        aspect.ratio = 0.8,
        legend.position = 'bottom')
   
ggsave('figures/mechanism-ft.pdf',width = 10,height = 6.6)

### interactional ----

by.group.mod <- function(dv){
  
  df <- anes %>% 
    mutate(sample = NA,
           sample = ifelse(white == 1&pid3 == 'Republican','White Republicans',sample),
           sample = ifelse(white == 1&pid3 == 'Democrat','White Democrat',sample),
           sample = ifelse(white == 0&pid3 == 'Democrat','Non-White Democrats',sample) )
  results <- 
    df %>% 
    filter(!is.na(sample)) %>% 
    nest(data = -c(sample)) %>% 
    mutate(fit = map(data, ~ lm(setmd('gap*ft_white',dv), data = .x)),
           mod = map(fit,
                     ~ggpredict(.x,terms = c("ft_white [20,80]", "gap[0,30]"))))%>% 
    unnest(cols = mod) 
  return(results %>% select(-data,-fit))
}

bind_rows(by.group.mod('fl_gop')) %>% 
  ggplot(aes(x = group, y = predicted, ymin = conf.low,ymax = conf.high,
             color = factor(x))) +
  geom_pointrange(position = position_dodge(width = 0.5)) +  
  facet_wrap(~sample,scales = 'free_y') + 
  # coord_flip() + 
  ylab('Coefficient Estimate of Party Imagery Contrast') + 
  scale_colour_viridis_d(option = 'cividis',end = 0.7 , direction = -1) +
  theme_minimal() +  # ylim(c(-0.2,0.4)) + 
  theme(axis.title.y = element_blank(),
        axis.text = element_text(color = 'black'),
        aspect.ratio = 0.8,
        legend.position = 'bottom')

# Mechanism on All Sample ---- 

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
models <- list(
  'Republican FT' = lm_robust(fl_gop ~ ft_white*gap + white_pop,anes),
  'Democrat FT' = lm_robust(fl_dem ~ ft_white*gap + white_pop,anes),
  'Rep. FT - Dem. FT' = lm_robust(gop_over_dem ~ ft_white*gap + white_pop,anes))

modelsummary(
  models,
  stars = TRUE,
  threeparttable = TRUE,
  gof_map = c('nobs','r.squared'),
  coef_map = c(
    "ft_white" = "White Feeling Thermometer",
    "gap" = 'Party Imagery Contrast',
    "ft_white:gap" = 'White Feelings × Party Imagery Contrast'))


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

fig.mechanism <- ( 
  sj(lm(fl_gop ~ ft_white*gap + white_pop,anes), 'DV: Republicans FT') + 
    sj(lm(fl_dem ~ ft_white*gap + white_pop,anes), 'DV: Democrats FT') + 
    sj(lm(gop_over_dem ~ ft_white*gap+ white_pop,anes), 'DV: GOP FT - Dem FT') ) +   
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom') 

fig.mechanism

ggsave('figures/mechanism-all-sample.pdf',width = 11,height = 5)



# Save Objects -----

save.image('quarto/final.Rdata')
