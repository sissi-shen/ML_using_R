#Age visualizations\
p1 <- ggplot(adapt, aes(x=age, fill = adaptivity_level, color = adaptivity_level)) + 
  geom_bar(position = "dodge", alpha=0.5) + 
  theme(legend.position = "top")
p1 + labs(title = "Age vs. Adaptivity Level for Students", 
          x = "Age", y = "Number of Students")

#Gender visualization
adapt_gender <- adapt %>%
  group_by(age) %>%
  select(age, adaptivity_level, gender) %>%
  transform(freq = ave(seq(nrow(adapt_gender)), age, FUN=length))

p2 <- ggballoonplot(adapt_gender, x = "age", y = "adaptivity_level", size = "freq", 
                      fill = "freq", facet.by = "gender", 
                      ggtheme = theme_bw()) + 
      scale_fill_viridis_c(option = "C")
p2

p3 <- ggplot(adapt, aes(x=age, fill = adaptivity_level, color = adaptivity_level)) + 
      geom_bar() + 
      facet_grid(~gender)
p3

#Financial condition with internet type and network type
p4 <- ggplot(adapt, aes(x=financial_condition, fill= network_type, color = network_type)) + 
      geom_bar() +
      facet_grid(~internet_type)
p4

#Financial condition with adaptivity level
p5 <- ggplot(adapt, aes(x=financial_condition, fill = adaptivity_level)) + 
      geom_bar(position = position_dodge()) + 
      geom_text(stat = 'count', aes(label=after_stat(count)), vjust=1.6, color="white", 
                position = position_dodge(0.9), size=3.5)+
      theme_minimal()
p5

# Education variables
p6 <- ggplot(adapt, aes(x=education_level, group=institution_type)) + 
  geom_bar(aes(y=..prop.., fill = factor(..x..)), stat = 'count') +
  geom_text(stat = "count", aes(label=scales::percent(..prop..), y=..prop..), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)+
  labs(y = "Percent", fill = "adaptivity_level") +
  facet_grid(~institution_type) + 
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
p6

p7 <- ggplot(adapt, aes(x=class_duration, fill = adaptivity_level, color = adaptivity_level)) + 
  geom_bar(position = "dodge", alpha=0.5) + 
  theme(legend.position = "top") + 
  labs(x = "Class Duration", y = "Number of Students")
p7

p8 <- ggplot(adapt, aes(x=self_lms, fill = adaptivity_level, color = adaptivity_level)) + 
  geom_bar(position = "dodge", alpha=0.5) + 
  theme(legend.position = "top")
p8