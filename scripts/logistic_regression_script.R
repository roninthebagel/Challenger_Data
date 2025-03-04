# read csv file
challenger <- read_csv("data/Challenger.csv")

# filtering data - to only include flights where a failure occured
o_ring_temp_plot <- challenger %>% 
  filter(oring_dt > 0) %>% 
  ggplot(aes(y=oring_dt, x=temp))+geom_point()+
  ggtitle("Temperature on flight launches where an O-ring incident occurred")
print(o_ring_temp_plot)

ggsave("figures/o_ring_temp_plot", 
       plot = o_ring_temp_plot, 
       width = 15, 
       height = 10, 
       units = "cm", 
       device = "pdf")

# using all launch data
all_launch_temp_plot <- challenger |> 
  ggplot(aes(y=oring_dt, 
             x=temp))+
  geom_point()+
  geom_smooth(method="lm")+
  ggtitle("All launch data")
print(all_launch_temp_plot)

ggsave("figures/all_launch_temp_plot", 
       plot = all_launch_temp_plot, 
       width = 15, 
       height = 10, 
       units = "cm", 
       device = "pdf")

# there is a clear relationship between the temperature and risk of o ring failure

