plot_histograms <- function(data, var_list) {
  for (var in var_list) {
    p <- 
      data %>% 
      ggplot(aes(x = .data[[var]],y = "..density..")) +
      geom_histogram(aes(y = ..density..),binwidth = 1, fill = "blue", color = "black") +
      labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
      theme_minimal()
    
    print(p)
  }
}

plot_bar_plots <- function(data, var_list) {
  for (var in var_list) {
    p <- 
      data %>% 
      ggplot(aes(x = .data[[var]])) +
      geom_bar(aes(y = ..prop.., group = 1), fill = "blue", color = "black") +
      labs(title = paste("Bar Plot of", var), x = var, y = "Proportion") +
      theme_minimal()
    
    print(p)
  }
}

plot_dep_scatter_plots <- function(data, var_list) {
  for (var in var_list) {
    p <- 
      data %>% 
      ggplot(aes(x = .data[[var]],y = total_points)) +
      geom_point() +
      #labs(title = paste("Bar Plot of", var), x = var, y = "Proportion") +
      theme_minimal()
    
    print(p)
  }
}

plot_dep_tile <- function(data,var_list){
  
  for (var in var_list) {
    p <- 
      data %>% 
      count(.data[[var]], total_points) %>%
      ggplot(aes(x = .data[[var]], y = total_points, fill = n)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "blue") +
      labs(title = "Heatmap of Counts",
           x = var, 
           y = "total_points",
           fill = "Count") +
      theme_minimal()
    
    print(p)
  }
}