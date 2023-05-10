#'
#'
#'
#'Make a graph based on variables and variable type
#'
#'@param dat A dataframe
#'@param var1 First variable
#'@param var2 Second variable
#'@param var1type First variable type
#'@param var2type Second variable type
#'
#'@import ggplot2
#'@return A plot based on variable types
#'
#'
#'@export

plot_by_type <- function(data, var1, var1type, var2 = "none", var2type = "none"){

  if (var1type == "numeric" & var2type == "none") {
    p <- ggplot(data = data, aes(x = {{var1}})) +
      geom_histogram(bins = 10, stat = "count", fill = "steelblue", color = "white") +
      labs(title = paste0("Histogram of '", deparse(substitute(var1)), "'"),
           y = "Frequency") +
      theme_classic() +
      theme(axis.text = element_text(size = 16, family = "Times New Roman"),
            axis.title = element_text(size = 18, family = "Times New Roman"),
            plot.title = element_text(size = 18, family = "Times New Roman"))
    return(p)

  } else if (var1type == "categorical" & var2type == "none"){
    p <- ggplot(data = data, aes(x = {{var1}}, fill = {{var1}})) +
      geom_bar(stat = "count", color = "white") +
      scale_fill_manual(values = c("steelblue", "orange", "green", "red", "purple")) +
      labs(title = "Bar Chart", x = "Category", y = "Value") +
      theme_classic() +
      theme(axis.text = element_text(size = 16, family = "Times New Roman"),
            axis.title = element_text(size = 18, family = "Times New Roman"),
            plot.title = element_text(size = 18, family = "Times New Roman"))
    return(p)

  } else if (var1type == "numeric" & var2type == "categorical") {
    p <- ggplot(data = data, aes(x = {{var1}}, y = {{var2}})) +
      geom_boxplot(fill = "steelblue", color = "white") +
      labs(title = paste0("Boxplot of '", deparse(substitute(var2)), "' by '", deparse(substitute(var1)), "'"),
           x = deparse(substitute(var1)),
           y = deparse(substitute(var2))) +
      theme_classic() +
      theme(axis.text = element_text(size = 16, family = "Times New Roman"),
            axis.title = element_text(size = 18, family = "Times New Roman"),
            plot.title = element_text(size = 18, family = "Times New Roman"))
    return(p)

  } else if (var1type == "categorical" & var2type == "numeric") {
    p <- ggplot(data = data, aes(x = {{var1}}, y = {{var2}})) +
      geom_boxplot(fill = "steelblue", color = "white") +
      labs(title = paste0("Boxplot of '", deparse(substitute(var2)), "' by '", deparse(substitute(var1)), "'"),
           x = deparse(substitute(var1)),
           y = deparse(substitute(var2))) +
      theme_classic() +
      theme(axis.text = element_text(size = 16, family = "Times New Roman"),
            axis.title = element_text(size = 18, family = "Times New Roman"),
            plot.title = element_text(size = 18, family = "Times New Roman"))
    return(p)

  } else if (var1type == "numeric" & var2type == "numeric") {
    p <- ggplot(data = data, mapping = aes(x = {{var1}}, y = {{var2}})) +
      geom_point(color = "steelblue") +
      labs(title = paste0("Scatterplot of '", deparse(substitute(var1)), "' and '", deparse(substitute(var2)), "'"),
           x = deparse(substitute(var1)),
           y = deparse(substitute(var2))) +
      theme_classic() +
      theme(axis.text = element_text(size = 16, family = "Times New Roman"),
            axis.title = element_text(size = 18, family = "Times New Roman"),
            plot.title = element_text(size = 18, family = "Times New Roman"))
    return(p)

  } else if (var1type == "categorical" & var2type == "categorical"){
    stop("Categorical-categorical plots are not supported.")

  } else {
    stop("Please choose 'numeric' or 'categorical' as the type.")
  }

}




