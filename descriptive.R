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
  scale_fill_viridis_b(option = 'mako', begin = 0.3, end = 0.85,direction = -1,
                       name = NULL) +
  ggtitle('A. White % of Democrats') + 
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(face = 'bold',hjust = 1/2),
        aspect.ratio = 0.68)
map.dem
map.gop <- 
  ggplot(cd) + 
  geom_sf(aes(fill = Republican_White ),
          lwd = 1/5,col = 'white', alpha = 0.7)  +
  geom_sf(data =st.bound,color = 'black',fill = alpha('white',0),lwd = 1/5)  +
  scale_fill_viridis_b(option = 'magma', begin = 0.3, end = 0.85,direction = -1,
                       breaks = seq(0,1,0.2),name = NULL) +
  ggtitle('B. White % of Republicans') + 
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(face = 'bold',hjust = 1/2),
        aspect.ratio = 0.68)

map.gap <- 
  ggplot(cd) + 
  geom_sf(aes(fill = Republican_White - Democrat_White ),
          lwd = 1/5,col = 'white', alpha = 0.7)  +
  geom_sf(data =st.bound,color = 'black',fill = alpha('white',0),lwd = 1/5)  +
  scale_fill_viridis_b(option = 'cividis', begin = 0.1, end = 0.9,direction = -1,
                       name = NULL) +
  ggtitle('C. Gap of White Imageries') + 
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(face = 'bold',hjust = 1/2),
        aspect.ratio = 0.68)

map.dem + map.gop + map.gap
ggsave('figures/map.pdf',width = 18.6,height = 6.4)

# Histogram of Racial Imageries ---- 


hist <- 
cd %>% 
  ggplot() + 
  geom_vline(xintercept = c(0),lty = 2,color = 'black') + 
  geom_histogram(aes(x = Republican_White - Democrat_White,fill = Republican_White - Democrat_White < 0), 
                 alpha = 0.6,color = 'white')  + 
  theme_bw() + 
  ylim(c(NA,70)) + 
  xlab('White % of Republicans - Democrats') + 
  scale_fill_viridis_d(option = 'plasma',end = 0.6) + 
  theme(legend.position = 'none',
        aspect.ratio = 0.9,
        axis.title.y = element_blank())

hist1 <- 
  cd %>% 
  ggplot() + 
  geom_vline(xintercept = c(0.5), lty = 2,color = 'black') + 
  geom_histogram(aes(x = Democrat_White), 
                 alpha = 0.7, fill = 'dodgerblue4',color = 'white')  + 
  theme_bw() + 
  ylim(c(NA,70)) + 
  xlab('White % of Democrats') + 
  theme(legend.position = 'none',
        aspect.ratio = 0.9,
        axis.title.y = element_blank())

hist2 <- 
  cd %>% 
  ggplot() + 
  geom_vline(xintercept = c(0.5),lty = 2,color = 'black') + 
  geom_histogram(aes(x = Republican_White), 
                 alpha = 0.7, fill = 'firebrick3',color = 'white')  + 
  theme_bw() + 
  ylim(c(NA,70)) + 
  xlab('White % of Republicans') + 
  theme(legend.position = 'none',
        aspect.ratio = 0.9,
        axis.title.y = element_blank())

ggarrange(hist2,hist1,hist,nrow = 1)
hist2 + hist1 + hist + plot_annotation(tag_levels = 'A')
ggsave('figures/desc-imagery-hist.pdf',width = 12, height = 4)
mean(cd$gap,na.rm = T)
