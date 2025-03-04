# read csv file
challenger <- read_csv("data/Challenger.csv")

#__________________________----

# filtering data to make plot - to only include flights where a o ring failure occurred
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

#__________________________----

# create variable containing o rings left intact
challenger <- challenger |> 
  mutate(oring_int = oring_tot - oring_dt) 

# fitting a binary GLM
binary_model <- glm(cbind(oring_dt, oring_int) ~ temp, family = binomial(link = "logit"), data = challenger)

binary_model |> 
  broom::tidy(conf.int=T)

# fitting the logistic Bernoulli distribution model
binary_model <- glm(cbind(oring_dt, oring_int) ~ temp, family=binomial, data=challenger)

#__________________________----

# probability

# making predictions

# using emmeans to calculate the log-odds of o ring failure
emmeans::emmeans(binary_model, specs=~temp, 
                 type="response") # set type = "response to get calculated probabilities

# odds and probability

# using the exponent of the linear regression equation to calculate probability
odds_at_69.6 <- exp(coef(binary_model)[1]+coef(binary_model)[2]*69.6)

# To convert from odds to a probability, divide the odds by one plus the odds
probability <-  odds_at_69.6/(1+odds_at_69.6)
probability
# the risk of an o ring failure on an average day is 0.025

# changes in probability - calculating via emmeans
emmeans::emmeans(binary_model, 
                 specs = ~ temp, 
                 at=list(temp=c(66:27)), 
                 type='response') 

# calculate the probability of failure at different temperatures - plot
failure_probability_plot <- emmeans::emmeans(binary_model, 
                 specs = ~ temp, 
                 at=list(temp=c(27:80)), 
                 type='response') %>% 
  as_tibble() %>% 
  ggplot(aes(x=temp, y=prob))+
  geom_line(aes(x=temp, y=prob))+
  geom_ribbon(aes(ymin=asymp.LCL, 
                  ymax=asymp.UCL), alpha=0.2)

ggsave("figures/failure_probability_temp_plot", 
       plot = failure_probability_plot, 
       width = 15, 
       height = 10, 
       units = "cm", 
       device = "pdf")

#__________________________----

# challenger launch data

# calculating the probability of failure at a specific temperature
emmeans::emmeans(binary_model, 
                 specs = ~ temp, 
                 at = list(temp = 36),                  
                 type='response')
# O-ring failure on the day of the Challenger launch could have been predicted with a probability of 0.91 [91.3%CI: 0.34-99.5]

#__________________________----

# assumptions

# estimating possible overdispersion - using a quasi-likelihood fit
performance::check_model(binary_model,
                         residual_type = "normal",
                         detrend = FALSE)

# fitting a quasibinomial model
quasibinary_model <- glm(cbind(oring_dt, oring_int) ~ temp, 
                         family=quasibinomial, # quasilikelihood
                         data=challenger)

summary(quasibinary_model)

# estimates from the model will not change
# standard errors will be widened by a fixed dispersion parameter (> 1)
# this may increase p-values, but here there is no change in interpretation
