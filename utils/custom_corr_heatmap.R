# Plot heatmap
custom_corr_heatmap <- 
  function(corr_df){
    
    ggplot(corr_df %>% 
             pivot_longer(cols = -term) %>% 
             mutate(value = ifelse(is.na(value),1,value)), 
           aes(term, name, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab",
                           name="Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 10, hjust = 1),
            axis.text.y = element_text(size = 10)) +
      coord_fixed() +
      labs(title = "Correlation Matrix Heatmap")
    
  }