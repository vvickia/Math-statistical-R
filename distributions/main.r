library(plotly)

xi <- function(n) rnorm(n, mean = 1, sd = 0.25)
eta <- function(n) sample(n, x = c(-1,0,1), prob = c(0.3,0.4,0.3), replace = T)
zeta <- function(n) (xi(n))^2 + eta(n)

# Let "xi" has a distribution density ~ N(1, 1/4) (denote it by "xi_dens") and "eta" has a distribution 
# table ~ ((-1, 0, 1),(0.3, 0.4, 0.3)), and these quantities are independent. Then (eta + xi) has 
# a distribution density f_(eta+xi)(x) = 0.3*xi_dens(x-(-1)) + 0.4*xi_dens(x-0) + 0.3*xi_dens(x-1).
# But we have a random value "zeta" = xi^2 + eta:.

xi_dens <- function(x) dnorm(x, mean = 1, sd = 0.25)

# Let y = x^2, where x is a continuous random variable with density f(x). Then if y < 0, then the distribution
# function F(y) = 0 and density f(y) = 0; in the case when y > 0, we get: f(y) = 1/2/sqrt(y)*(f(sqrt(y))+f(-sqrt(y))).
# xi is continuous random variable with distribution density xi_dens. Find the xisq_dens ("xisq" is xi^2):

xisq_dens <- function(x) ifelse ( x <= 0, 0, (xi_dens(sqrt(x)) + xi_dens(-sqrt(x))) / (2 * sqrt(x)) )
zeta_dens <- function(x) 0.3*(xisq_dens(x + 1) + xisq_dens(x - 1)) + 0.4*xisq_dens(x)

zeta_distr_func <- function(x) (integrate(zeta_dens, -Inf, x))$value    # add $value to get an integral value only

# Define the Mean Value (through the distribution density)
mean_value <- function(dens) 
{
  integrand <- function(x) x * dens(x)
  return((integrate(integrand, -Inf, Inf))$value)
}

# Define the Variance
variance <- function(dens) 
{
  mean_val <- mean_value(dens)
  integrand <- function(x) (x - mean_val)^2 * dens(x)
  return((integrate(integrand, -Inf, Inf))$value)
}

# 1

# Density Chart
density_plot <- function(dens, a, b) 
{
  x <- seq(a, b, by = .1)
  y <- dens(x) 
  mean_val <- mean_value(dens)
  sd <- sqrt(variance(dens))
  fig <- plot_ly() %>%
    add_lines(x = x, y = y, name = "Density", line = list(color = 'rgb(198, 222, 7)', width = 2))  %>% 
    add_lines(x = rep(mean_val,2), y = c(0,max(y)), name = "Mean",
              line = list(color = 'rgb(224, 14, 0)', width = 1))  %>% 
    add_lines(x = rep(mean_val+sd,2), y = c(0,max(y)), name = "Mean+std", 
              line = list(color = 'rgb(23, 84, 145)', width = 1, dash = 'dot')) %>% 
    add_lines(x = rep(mean_val-sd,2), y = c(0,max(y)), name = "Mean-std",
              line = list(color = 'rgb(23, 84, 145)', width = 1, dash = 'dot')) %>% 
    layout(plot_bgcolor = 'rgb(227, 218, 245)', legend = list(orientation = 'h'),
           yaxis = list(zeroline = F), xaxis = list(zeroline = F),
           title = paste("Density of zeta:  M[zeta] =",as.character(round(mean_val,2)),", D[zeta] =",as.character(round(sd^2,2)), "       (Kobozeva V.)"))
  return(fig)
}

# Chart of the Distribution Function
distr_func_plot <- function(distr_func, dens, a, b) 
{
  x <- seq(a, b, by = .1)
  y <- unlist(lapply(x, distr_func))
  mean_val <- mean_value(dens)
  sd <- sqrt(variance(dens))
  fig <- plot_ly() %>% 
    add_lines(x = x, y = y, name = "Distribution Function", line = list(color = 'rgb(66, 227, 209)', width = 2))  %>% 
    add_lines(x = rep(mean_val,2), y = c(0,max(y)), name = "Mean",
              line = list(color = 'rgb(224, 14, 0)', width = 1))  %>% 
    add_lines(x = rep(mean_val+sd,2), y = c(0,max(y)), name = "Mean+std", 
              line = list(color = 'rgb(23, 84, 145)', width = 1, dash = 'dot')) %>% 
    add_lines(x = rep(mean_val-sd,2), y = c(0,max(y)), name = "Mean-std", 
              line = list(color = 'rgb(23, 84, 145)', width = 1, dash = 'dot')) %>% 
    layout(plot_bgcolor = 'rgb(227, 218, 245)', legend = list(orientation = 'h'),
           yaxis = list(zeroline = F), xaxis = list(zeroline = F),
           title = paste("DF of zeta:  M[zeta] =",as.character(round(mean_val,2)),", D[zeta] =",as.character(round(sd^2,2)), "       (Kobozeva V.)"))
  return(fig)
}

# Histogram and Density Curve
histogram_with_density_plot <- function(rval, dens) 
{
  x <- seq(min(rval), max(rval), by = .1)
  y <- dens(x) 
  dens_fig <- density_plot(dens,-2,5) 
  fig <- plot_ly(type = "histogram") %>%       
    add_lines(x = x, y = y, name = "Density", line = list(color = 'rgb(198, 222, 7)', width = 2)) %>%
    add_histogram(x = rval, histnorm = "probability", name = "Histogram for zeta") %>%  # "histnorm" normalizes the histogram
    layout(plot_bgcolor = 'rgb(227, 218, 245)', legend = list(orientation = 'h'),
           title = "Histogram for zeta with Density Curve  (Kobozeva V.)")
  return(fig)
}

# Empirical Distribution Function Plot
empirical_distr_func_plot <- function(rval, distr_func, a, b) 
{
  x <- seq(a, b, by = .1)
  y_ecdf <- ecdf(rval)(x)
  y_distr_func <- unlist(lapply(x, distr_func))
  fig <- plot_ly() %>% 
    add_lines(x = x, y = y_ecdf, name = "Empirical DF", line = list(color = 'rgb(250, 136, 85)')) %>%
    add_lines(x = x, y = y_distr_func, name = "DF", line = list(color = 'rgb(66, 227, 209)')) %>%
    layout(plot_bgcolor = 'rgb(227, 218, 245)', legend = list(orientation = 'h'),
           yaxis = list(zeroline = F), xaxis = list(zeroline = F),
           title = "Empirical DF of zeta  (Kobozeva V.)")
  return(fig)
}

# 2

stars <- read.table("catalog.tsv", header = T, sep = "|") 

histogram_plot <- function(values) 
{
  fig <- plot_ly(type = "histogram") %>%       
    add_histogram(x = values, name = "Masses") %>%
    layout(plot_bgcolor = 'rgb(227, 218, 245)', showlegend = F,
           xaxis = list(title = "Masses of stars (M_sol)"), yaxis = list(title = "Count of stars"),
           title = "Nearby star masses distribution  (Kobozeva V.)")
  return(fig)
}

df_mass_plot <- function(values, a, b) 
{
  q <- quantile(as.numeric(stars$Mass), na.rm = T)
  x <- seq(a, b, by = .1)
  y_ecdf <- ecdf(values)(x)
  fig <- plot_ly() %>% 
    add_lines(x = x, y = y_ecdf, name = "DF") %>%
    add_lines(x = rep(q[1],2), y = c(0,max(y_ecdf)), name = "0 %",
              line = list(color = 'rgb(224, 14, 0)', width = 1)) %>% 
    add_lines(x = rep(q[2],2), y = c(0,max(y_ecdf)), name = "25 %",
              line = list(color = 'rgb(224, 14, 0)', width = 1)) %>% 
    add_lines(x = rep(q[3],2), y = c(0,max(y_ecdf)), name = "50 %",
              line = list(color = 'rgb(224, 14, 0)', width = 1)) %>% 
    add_lines(x = rep(q[4],2), y = c(0,max(y_ecdf)), name = "75 %",
              line = list(color = 'rgb(224, 14, 0)', width = 1)) %>% 
    add_lines(x = rep(q[5],2), y = c(0,max(y_ecdf)), name = "100 %",
              line = list(color = 'rgb(224, 14, 0)', width = 1)) %>% 
    layout(plot_bgcolor = 'rgb(227, 218, 245)', title = "Distribution Function of masses  (Kobozeva V.)",
           xaxis = list(title = "Masses of stars (M_sol)", zeroline = F), 
           yaxis = list(zeroline = F))
  return(fig)
}

show_star_names <- function(stars_df)
{
  q <- quantile(as.numeric(stars_df$Mass), na.rm = T)  # discard NA-rows
  isValid <- (as.numeric(stars_df$Mass) > q["25%"]) & (as.numeric(stars_df$Mass) < q["50%"]) & (!is.na(as.numeric(stars_df$Mass)))
  return(paste(stars$Name[isValid],stars$m_Name[isValid]))
}


# 1
density_plot(zeta_dens, -2, 5)
distr_func_plot(zeta_distr_func, zeta_dens, -2, 5)
histogram_with_density_plot(zeta(300), zeta_dens)
empirical_distr_func_plot(zeta(300), zeta_distr_func, -2, 5)

# 2
histogram_plot(as.numeric(stars$Mass))
df_mass_plot(as.numeric(stars$Mass), -2, 5)

show_star_names(stars)
