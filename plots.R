

library(ggpubr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(mapview)
library(ggthemes)
library(MASS)
library(broom)
library(tidyr)
library(htmlwidgets)
library(leafpop)
library(leaflet)



# load data
ks_bind_join <- read_csv("D:/GoogleDrive/ROTFL/out/ks_tests_sites_all.csv")

cloud_join <- read_csv('D:/GoogleDrive/ROTFL/out/cloudiness_gauges.csv')


cloud_ks <- cloud_join %>%
  inner_join(ks_bind_join %>%
               filter(test =="ks_cloud_free") %>%
               dplyr::select(id:percentile_range_sample),
             by="id")


# relating D stat from KS test to fraction cloud free days per gauge
plot1 <- ggplot(cloud_ks, aes(x=cloud_free_frac, y=d_boot, color=ks_test))+
  geom_point(alpha=0.5)+
  # geom_smooth(method="lm") +
  theme_few() +
  xlab("Fraction cloud-free overpasses") +
  ylab("D statistic (KS-test)") +
  theme(legend.position = c(0.79, 0.95),
        legend.background = element_blank(),
        legend.text = element_text(size=8),
        legend.key.size = unit(0.05, 'lines'),
        axis.title = element_text(size=9),
        axis.text = element_text(size=9)) +
  scale_color_manual(values=c("grey", "black"), labels=c("p < 0.05", "p > 0.05"), name="")

# same with best fit lines
plot1_lines <- ggplot(cloud_ks, aes(x=cloud_free_frac, y=d_boot, color=ks_test))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm", se=F) +
  theme_few() +
  xlab("Fraction of cloud-free overpasses") +
  ylab("D statistic (KS-test)") +
  theme(legend.position = c(0.79, 0.95),
        legend.background = element_blank(),
        legend.key.size = unit(0.05, 'lines'),
        legend.text = element_text(size=8),
        axis.title = element_text(size=9),
        axis.text = element_text(size=9)) +
  scale_color_manual(values=c("grey", "black"), labels=c("p < 0.05", "p > 0.05"), name="")

# relating D stat from KS test to flow flashiness (Richards-Baker Index) per gauge 
plot2 <- ggplot(cloud_ks) +
  geom_point(aes(x=rbi, y=d_boot, color=ks_test), alpha=0.5)+
  theme_few() +
  scale_x_log10(breaks = c(0.03, 0.1, 0.3, 1.0),
                labels = c("0.03", "0.1", "0.3", "1.0")) +
  xlab("Flashiness Index") +
  ylab("") +
  theme(legend.position = "none",
        legend.background = element_blank(),
        axis.title = element_text(size=9),
        axis.text = element_text(size=9)) +
  scale_color_manual(values=c("grey", "black"), labels=c("p < 0.05", "p > 0.05"), name="") 

# cumulative distribution function of the range of flow percentiles in 
# the whole gague record that is captured by Landsat sampling...
# >90% of gages capture 97% perecntiles of flow

plot3 <- ggplot(ks_bind_join %>%
         filter(test == "ks_cloud_free"),
       aes(percentile_range_sample/100)) +
  stat_ecdf(geom = "point", alpha=0.5) +
  theme_few() +
  ylab("Empirical CDF of gauges") +
  xlab("Proportion of flows sampled by LS ") +
  geom_hline(aes(yintercept= 0.1), col="red", linetype=2) +
  geom_vline(aes(xintercept=0.97), col="red", linetype=2) +
  theme(legend.position = "none",
        axis.title = element_text(size=9),
        axis.text = element_text(size=9)) 

#ggsave(paste('figs/', "proportion_flows_captured2.png", sep=""), units='in', width = 3.2, height=3, dpi = 300)

# save 3 panel fig
library(egg)

ggpubr::ggarrange(plot1, plot2, plot3, nrow=2, ncol=2)
ggsave(paste('D:/Dropbox/projects/ROTFL/figs/', "Fig_gauges.tiff", sep=""), units='in', width = 4.5, height=4, dpi = 300)
ggsave(paste('D:/Dropbox/projects/ROTFL/figs/', "Fig_gauges.png", sep=""), units='in', width = 4.5, height=4, dpi = 300)

# save 3 panel fig with best fit lines
ggpubr::ggarrange(plot1_lines, plot2, plot3, nrow=2, ncol=2)
ggsave(paste('D:/Dropbox/projects/ROTFL/figs/', "Fig_gauges_lines.tiff", sep=""), units='in', width = 4.5, height=4, dpi = 300)
ggsave(paste('D:/Dropbox/projects/ROTFL/figs/', "Fig_gauges_lines.png", sep=""), units='in', width = 4.5, height=4, dpi = 300)



# >90% of gages capture 97% perecntiles of flow


lm_same <- lm(d_boot~cloud_free_frac, data=cloud_ks %>%
                filter(ks_test=="same"))

lm_diff <- lm(d_boot~cloud_free_frac, data=cloud_ks %>%
                filter(ks_test=="different"))


# 
# ks_sf <- ks_bind_join %>%
#   filter(test == "ks_cloud_free") %>%
#   st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs= 4326) 
# 
# mapview(ks_sf, zcol='d_boot',  legend=T )
# 







#######################################
### Munge plots
#########################################################
# ks_bind_join %>%
#   filter(test == "ks_cloud_free") %>%
#   group_by(ks_test) %>%
#   summarise(count=n())
# 
# 
# 
# # is there a difference in flashiness based on if pop and sample distributions are 
# # different?  NOPE
# ggplot(ks_bind_join %>%
#          filter(test == "ks_cloud_free") ) +
#   #geom_density(aes(rbi, color=wc_p)) +
#   geom_point(aes(x=d_boot, y=pvalue_boot, color=ks_test)) +
#   theme_few() 
# 
# 
# # with drainage area ? NOPE
# ggplot(ks_bind_join %>%
#          filter(test == "ks_cloud_free") ) +
#   #geom_density(aes(rbi, color=wc_p)) +
#   geom_point(aes(x=drain_area_va, y=d_boot, color=ks_test)) +
#   scale_x_log10()+
#   theme_few() 
# 
# # relationship between flashiess and d stat. Nope
# ggplot(ks_bind_join %>%
#          filter(test == "ks_cloud_free") ) +
#   geom_point(aes(x=rbi, y=d_boot, color=ks_test)) +
#   #scale_x_log10()+
#   theme_few() 
# 
# 
# #
# ggplot(ks_bind_join %>%
#          filter(test == "ks_cloud_free") ) +
#  # geom_histogram(aes(percentile_range_sample, fill=ks_test), alpha=0.5) +
#   geom_point(aes(x=percentile_range_sample, y=d_boot, color=ks_test)) +
#   theme_few() +
#   scale_y_log10()
# 
# ##
# 
#   
# ggplot(ks_bind_join %>%
#          filter(test == "ks_cloud_free"),
#        aes(percentile_range_sample/100)) + 
#   stat_ecdf(geom = "point") +
#   theme_few() +
#   ylab("CDF of gauges") +
#   xlab("Proportion of flows sampled by LS ") +
#   geom_hline(aes(yintercept= 0.1), col="red") +
#   geom_vline(aes(xintercept=0.97), col="red")
# 
# ggsave(paste('figs/', "proportion_flows_captured2.png", sep=""), units='in', width = 3.2, height=3, dpi = 300)
# # >90% of gages capture 97% perecntiles of flow
# 
# 
# 
# # most gages capture close to modal Q. Landsat slightly underestimates on average
# ggplot(join_sum) +
#   geom_density(aes(mode_ratio), fill="grey") +
#   xlim(0, 5)+
#   theme_bw() +
#   geom_vline(xintercept=1, col="red") +
#   xlab("Sample Modal Q / Pop Modal Q")
# 
# #ggsave(paste('figs/', "modalQ_hist.jpeg", sep=""), units='in', width = 6, height=4)
# 
# ggplot(join_sum_site) +
#   geom_point(aes(q_pop_mode, q_sample_mode)) +
#   scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
#                 labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
#                 labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   geom_abline(intercept=0, slope=1, col="red") +
#   theme_few() +
#   xlab("Modal Q (population)") +
#   ylab("Modal Q (sample)")
# 
# #ggsave(paste('figs/', "modalQ_captured.jpeg", sep=""), units='in', width = 5, height=5)
# 
# # does the length of the Q record (population) impact how well? Not really
# ggplot(join_sum_site) +
#   geom_point(aes(n_pop, mode_ratio)) +
#   scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
#                 labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   scale_y_log10() +
#   
#   geom_hline(yintercept=1, col="red")
# 
# # does watershed area affect how well landsat represents modal Q?
# # Yes, there is a convergence to better representation as Area increases
# ggplot(ks_bind_join %>%
#          filter(test == "ks_cloud_free")) +
#   geom_point(aes(drain_area_va, percentile_range_sample)) +
#   scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
#                 labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   #scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
#  #               labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   #ylim(0.2,5) +
#   geom_hline(yintercept=1, col="red") +
#   theme_few() +
#   ylab("") +
#   xlab("Drainage area (mi^2)") +
#   ylab("Sample Modal Q / Pop Modal Q")