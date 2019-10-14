


ks_bind_join %>%
  filter(test == "ks_cloud_free") %>%
  group_by(ks_test) %>%
  summarise(count=n())



# is there a difference in flashiness based on if pop and sample distributions are 
# different?  NOPE
ggplot(ks_bind_join %>%
         filter(test == "ks_cloud_free") ) +
  #geom_density(aes(rbi, color=wc_p)) +
  geom_point(aes(x=d_boot, y=pvalue_boot, color=ks_test)) +
  theme_few() 


# with drainage area ? NOPE
ggplot(ks_bind_join %>%
         filter(test == "ks_cloud_free") ) +
  #geom_density(aes(rbi, color=wc_p)) +
  geom_point(aes(x=drain_area_va, y=d_boot, color=ks_test)) +
  scale_x_log10()+
  theme_few() 

# relationship between flashiess and d stat. Nope
ggplot(ks_bind_join %>%
         filter(test == "ks_cloud_free") ) +
  geom_point(aes(x=rbi, y=d_boot, color=ks_test)) +
  #scale_x_log10()+
  theme_few() 


#
ggplot(ks_bind_join %>%
         filter(test == "ks_cloud_free") ) +
 # geom_histogram(aes(percentile_range_sample, fill=ks_test), alpha=0.5) +
  geom_point(aes(x=percentile_range_sample, y=d_boot, color=ks_test)) +
  theme_few() +
  scale_y_log10()

##

  
ggplot(ks_bind_join %>%
         filter(test == "ks_cloud_free"),
       aes(percentile_range_sample/100)) + 
  stat_ecdf(geom = "point") +
  theme_few() +
  ylab("Proportion of gauges") +
  xlab("Proportion of flows sampled by LS ")

ggsave(paste('figs/', "proportion_flows_captured.png", sep=""), units='in', width = 3.2, height=3, dpi = 300)
# >90% of gages capture 97% perecntiles of flow



# most gages capture close to modal Q. Landsat slightly underestimates on average
ggplot(join_sum) +
  geom_density(aes(mode_ratio), fill="grey") +
  xlim(0, 5)+
  theme_bw() +
  geom_vline(xintercept=1, col="red") +
  xlab("Sample Modal Q / Pop Modal Q")

#ggsave(paste('figs/', "modalQ_hist.jpeg", sep=""), units='in', width = 6, height=4)

ggplot(join_sum_site) +
  geom_point(aes(q_pop_mode, q_sample_mode)) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  geom_abline(intercept=0, slope=1, col="red") +
  theme_few() +
  xlab("Modal Q (population)") +
  ylab("Modal Q (sample)")

#ggsave(paste('figs/', "modalQ_captured.jpeg", sep=""), units='in', width = 5, height=5)

# does the length of the Q record (population) impact how well? Not really
ggplot(join_sum_site) +
  geom_point(aes(n_pop, mode_ratio)) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10() +
  
  geom_hline(yintercept=1, col="red")

# does watershed area affect how well landsat represents modal Q?
# Yes, there is a convergence to better representation as Area increases
ggplot(ks_bind_join %>%
         filter(test == "ks_cloud_free")) +
  geom_point(aes(drain_area_va, percentile_range_sample)) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  #scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
 #               labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  #ylim(0.2,5) +
  geom_hline(yintercept=1, col="red") +
  theme_few() +
  ylab("") +
  xlab("Drainage area (mi^2)") +
  ylab("Sample Modal Q / Pop Modal Q")