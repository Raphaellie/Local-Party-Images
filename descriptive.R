# packages
library(ggpubr)
library(usmap)

# Mapping Racial Images ---- 

st.bound <- st_read('input/HexSt/HexSTv30.shp')

map.dem <- 
  ggplot(cd) + 
  geom_sf(aes(fill = Democrat_White ),
          lwd = 1/5,col = 'white', alpha = 0.7)  +
  geom_sf(data =st.bound,color = 'black',fill = alpha('white',0),lwd = 1/5)  +
  scale_fill_viridis_b(option = 'mako', begin = 0.2, end = 0.85,direction = 1,
                       breaks = seq(0,1,0.2),name = NULL) +
  ggtitle('A. White % of Democrats') + 
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(face = 'bold',hjust = 1/2,vjust = -1),
        aspect.ratio = 0.68)

map.gop <- 
  ggplot(cd) + 
  geom_sf(aes(fill = Republican_White ),
          lwd = 1/5,col = 'white', alpha = 0.7)  +
  geom_sf(data =st.bound,color = 'black',fill = alpha('white',0),lwd = 1/5)  +
  scale_fill_viridis_b(option = 'mako', begin = 0.2, end = 0.85,direction = 1,
                       breaks = seq(0,1,0.2),name = NULL) +
  ggtitle('B. White % of Republicans') + 
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(face = 'bold',hjust = 1/2,vjust = -1),
        aspect.ratio = 0.68)
map.ind <- 
  ggplot(cd) + 
  geom_sf(aes(fill = Independent_White ),
          lwd = 1/5,col = 'white', alpha = 0.7)  +
  geom_sf(data =st.bound,color = 'black',fill = alpha('white',0),lwd = 1/5)  +
  scale_fill_viridis_b(option = 'mako', begin = 0.2, end = 0.85,direction = 1,
                       breaks = seq(0,1,0.2),name = NULL) +
  ggtitle('C. White % of Independents') + 
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(face = 'bold',hjust = 1/2,vjust = -1),
        aspect.ratio = 0.68)

map.gap <- 
  ggplot(cd) + 
  geom_sf(aes(fill = (Republican_White - Democrat_White) ),
          lwd = 1/5,col = 'white', alpha = 0.7)  +
  geom_sf(data =st.bound,color = 'black',fill = alpha('white',0),lwd = 1/5)  +
  scale_fill_viridis_b(option = 'cividis', begin = 0.2, end = 0.85,direction = 1,
                       name = NULL) +
  ggtitle('D. Gap of White Imageries') + 
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(face = 'bold',hjust = 1/2,vjust = -1),
        aspect.ratio = 0.68)

layout <- "
AB
CD
"

fig.map <- map.dem  + map.gop + map.ind +  map.gap + 
  plot_layout(design = layout)

fig.map
ggsave('figures/map.pdf',width = 9,height = 9)

# Histogram of Racial Imageries ---- 

fill.style <-   scale_fill_viridis_d(option = 'viridis',end = 0.5,direction = 1) 

hist <- 
cd %>% 
  ggplot() + 
  geom_vline(xintercept = c(0),lty = 2,color = 'black') + 
  geom_histogram(aes(x = Republican_White - Democrat_White,
                     fill = Republican_White - Democrat_White > 0), 
                 alpha = 0.75,color = 'white')  + 
  theme_bw() + 
  ylim(c(NA,70)) + 
  xlab('White % of Republicans - Democrats') + 
  fill.style + 
  theme(legend.position = 'none',
        aspect.ratio = 0.9,
        axis.title.y = element_blank())

hist1 <- 
  cd %>% 
  ggplot() + 
  geom_vline(xintercept = c(0.5), lty = 2,color = 'black') + 
  geom_histogram(aes(x = Democrat_White,fill = Democrat_White > 1/2), 
                 alpha = 0.75, color = 'white')  + 
  theme_bw() + 
  fill.style + 
  ylim(c(NA,70)) + 
  xlab('White % of Democrats') + 
  theme(legend.position = 'none',
        aspect.ratio = 0.9,
        axis.title.y = element_blank())
hist2 <- 
  cd %>% 
  ggplot() + 
  geom_vline(xintercept = c(0.5),lty = 2,color = 'black') + 
  geom_histogram(aes(x = Republican_White,fill = Republican_White > 1/2), 
                 alpha = 0.75,color = 'white')  + 
  theme_bw() + 
  fill.style + 
  ylim(c(NA,70)) + 
  xlab('White % of Republicans') + 
  theme(legend.position = 'none',
        aspect.ratio = 0.9,
        axis.title.y = element_blank())

fig.hist <- 
  hist2 + hist1 + hist + plot_annotation(tag_levels = 'A')
fig.hist

ggsave('figures/desc-imagery-hist.pdf',width = 12, height = 4)
