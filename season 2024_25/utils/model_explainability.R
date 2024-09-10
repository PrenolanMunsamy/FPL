augment(wf_fitted,validation_df) %>% 
  mutate(row_id = row_number()) %>% 
  arrange(desc(.pred)) %>% 
  top_n(.pred,n=10) %>% 
  select(total_points,.pred,row_id) %>% 
  View()

# Create an explainer
explainer <- explain_tidymodels(
  wf_fitted,
  data = select(validation_df, -total_points),
  y = validation_df$total_points,
  label = "XGBoost"
)

# Explain a specific instance
instance_1 <- validation_df[3391, ]
instance_2 <- validation_df[1242, ]

explain_instance_1 <- predict_parts(explainer, new_observation = instance_1)
explain_instance_2 <- predict_parts(explainer, new_observation = instance_2)

# Plot the explanation
plot(explain_instance_1)
plot(explain_instance_2)

explain_instance_1 %>% 
  select(variable_name,variable,contribution) %>% 
  left_join(explain_instance_2 %>% 
              select(variable_name,variable,contribution),by = "variable_name") %>% 
  View()

# Combine explanations for comparison
comparison <- rbind(
  data.frame(explain_instance_1, instance = "Instance 1"),
  data.frame(explain_instance_2, instance = "Instance 2")
)

# Plot the comparison
library(ggplot2)
comparison %>% 
  filter(contribution!=0) %>% 
  filter(contribution > 0.1|contribution < -0.1) %>% 
  ggplot(aes(x = reorder(variable_name, -contribution), y = contribution, fill = instance)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Comparison of Feature Contributions", x = "Features", y = "Contribution") +
  theme_minimal()