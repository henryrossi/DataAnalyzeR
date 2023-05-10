library(testthat)
library(ggplot2)
library(itsadug)
library(vdiffr)

source("~/Desktop/DataAnalyzeR-main/R/plot_by_type.R")

numbers = c(1, 2, 4, 5, 2, 9)
numbers2 = c(1, 2, 4, 5, 6, 10)
abc = factor(c("A", "B", "A", "C", "B", "B"))
yn = factor(c("Yes", "No", "Yes", "Yes", "No", "No"))
lmh = factor(c("Low", "High", "Low", "Medium", "Medium", "High"))

test_that("my_function produces the expected plot", {
data <- data.frame(
  abc = abc,
  yn = yn,
  lmh = lmh,
  numbers = numbers,
  numbers2 = numbers2
)


expected_plot1 <- ggplot(data = data, aes(x = numbers)) +
  geom_histogram(bins = 10, stat = "count", fill = "steelblue", color = "white") +
  labs(title = paste0("Histogram of '", deparse(substitute(numbers)), "'"),
       y = "Frequency") +
  theme_classic() +
  theme(axis.text = element_text(size = 16, family = "Times New Roman"),
        axis.title = element_text(size = 18, family = "Times New Roman"),
        plot.title = element_text(size = 18, family = "Times New Roman"))

expected_plot2 <- ggplot(data = data, aes(x = numbers, fill = numbers)) +
  geom_bar(stat = "count", color = "white") +
  scale_fill_manual(values = c("steelblue", "orange", "green", "red", "purple")) +
  labs(title = "Bar Chart", x = "Category", y = "Value") +
  theme_classic() +
  theme(axis.text = element_text(size = 16, family = "Times New Roman"),
        axis.title = element_text(size = 18, family = "Times New Roman"),
        plot.title = element_text(size = 18, family = "Times New Roman"))

expected_plot3 <- ggplot(data = data, aes(x = numbers, y = yn)) +
  geom_boxplot(fill = "steelblue", color = "white") +
  labs(title = paste0("Boxplot of '", deparse(substitute(yn)), "' by '", deparse(substitute(numbers)), "'"),
       x = deparse(substitute(numbers)),
       y = deparse(substitute(yn))) +
  theme_classic() +
  theme(axis.text = element_text(size = 16, family = "Times New Roman"),
        axis.title = element_text(size = 18, family = "Times New Roman"),
        plot.title = element_text(size = 18, family = "Times New Roman"))

expected_plot4 <- ggplot(data = data, aes(x = yn, y = numbers)) +
  geom_boxplot(fill = "steelblue", color = "white") +
  labs(title = paste0("Boxplot of '", deparse(substitute(numbers)), "' by '", deparse(substitute(yn)), "'"),
       x = deparse(substitute(yn)),
       y = deparse(substitute(numbers))) +
  theme_classic() +
  theme(axis.text = element_text(size = 16, family = "Times New Roman"),
        axis.title = element_text(size = 18, family = "Times New Roman"),
        plot.title = element_text(size = 18, family = "Times New Roman"))

expected_plot5 <- ggplot(data = data, mapping = aes(x = numbers, y = numbers2)) +
  geom_point(color = "steelblue") +
  labs(title = paste0("Scatterplot of '", deparse(substitute(numbers)), "' and '", deparse(substitute(numbers2)), "'"),
       x = deparse(substitute(numbers)),
       y = deparse(substitute(numbers2))) +
  theme_classic() +
  theme(axis.text = element_text(size = 16, family = "Times New Roman"),
        axis.title = element_text(size = 18, family = "Times New Roman"),
        plot.title = element_text(size = 18, family = "Times New Roman"))


# Save the actual plot output
actual_plot1 <- plot_by_type(data, numbers, "numeric")
actual_plot2 <- plot_by_type(data, yn, "categorical")
actual_plot3 <- plot_by_type(data, numbers, "numeric", yn, "categorical")
actual_plot4 <- plot_by_type(data, yn, "categorical", numbers, "numeric")
actual_plot5 <- plot_by_type(data, numbers, "numeric", numbers2, "numeric")

environment(actual_plot1$layers[[1]]$super)$env$user_env <- environment(expected_plot1$layers[[1]]$super)$env$user_env

# Test the plots
expect_doppelganger("Histogram of 'numbers'", actual_plot1)
expect_doppelganger("Bar Chart", actual_plot2)
expect_doppelganger("Boxplot of 'yn' by 'numbers'", actual_plot3)
expect_doppelganger("Boxplot of 'numbers' by 'yn'", actual_plot4)
expect_doppelganger("Scatterplot of 'numbers' and 'number2'", actual_plot5)

#expect_equal(actual_plot2, expected_plot2)
#expect_equal(actual_plot3, expected_plot3)
#expect_equal(actual_plot4, expected_plot4)
#expect_equal(actual_plot5, expected_plot5)




})


test_that("Not listing the type of variable or misspelling gets an error", {
  expect_error(plot_by_type(data, yn))
  expect_error(plot_by_type(data, yn, "categolical"))
})

test_that("Putting two categorical variables is not supported by this function", {
  expect_error(plot_by_type(data, yn, "categorical", lmh, "categorical"))
})


