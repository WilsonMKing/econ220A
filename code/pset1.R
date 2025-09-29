################################################################################
################################################################################

### Problem Set 1
### Name: Wilson King

### Preamble

# Load Packages
packages <- c("haven", "dplyr", "tidyr", "magrittr", "ggplot2", "stargazer", "readr", "rshape2")
lapply(packages, require, character.only = TRUE)

# Set file paths
root_path <- paste0("/Users/", Sys.getenv("USER"), "/Documents/GitHub/econ220A/")
root_code <- paste0(root_path, "code/")
root_data <- paste0(root_path, "data/")
root_exhibits <- paste0(root_path, "exhibits/")

# Load data
yoghurt_df <- readr::read_csv(paste0(root_data, "data_yoghurt.csv"))

# Create outside option market share variable
yoghurt_df %<>%
  group_by(period, city) %>%
  dplyr::mutate(outside_share = 1 - sum(share)) %>%
  ungroup()

### Question 1

# Create calories/sugar/protein per gram variables
yoghurt_df %<>%
  dplyr::mutate(
    calories = calories / weight,
    sugar = sugar / weight,
    protein = protein / weight)

# Create descriptive statistics table
yoghurt_df %>%
  dplyr::filter(city == 1 & period == 1) %>%
  dplyr::select(c(product, price, share, weight, calories, sugar, protein)) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(., 2))) %>%
  stargazer::stargazer(
    summary = FALSE, 
    rownames = FALSE, 
    digits = 2, 
    title = "Descriptive Statistics on Yoghurt Brands",
    label = "tab:question1",
    covariate.labels = 
      c("Product", "Price", "Market share", "Weight (g)", "Calories/g", "Sugar/g", "Protein/g")) %>%
  starpolishr::star_tex_write(file = paste0(root_exhibits, "question1.tex"))

### Question 2

# Estimate demand system
form <- as.formula("log(share) - log(outside_share) ~ price + weight + calories + sugar + protein - 1")
ols_demand_system <- lm(formula = form, data = yoghurt_df)
robust_vcov <- sandwich::vcovHC(ols_demand_system, type = "HC1")
robust_se <- sqrt(diag(robust_vcov))

# Export regression results
stargazer::stargazer(
  ols_demand_system,
  title = "Estimated Demand System",
  label = "tab:question2",
  dep.var.caption = "",
  se = list(robust_se),
  dep.var.labels = "\\(\\ln s_{jct} - \\ln s_{0ct}\\)",
  covariate.labels = c("Price", "Weight (g)", "Calories/g", "Sugar/g", "Protein/g", "Intercept"),
  omit.stat = c("rsq", "adj.rsq", "f", "ser")) %>%
  starpolishr::star_tex_write(file = paste0(root_exhibits, "question3.tex"))


### Question 4

# Compute price elasticity across all goods
alpha <- ols_demand_system$coefficients["price"]

# Create dataframe from period 1 and city 1
yoghurt_df_1_1 <- yoghurt_df %>% dplyr::filter(period == 1 & city == 1)

# Fill in matrix of elasticities
elasticities_matrix <- matrix(nrow = 5, ncol = 5)
for(j in 1:5){
  for(i in 1:5){
    if(i == j){
      elasticities_matrix[j,j] <- ols_demand_system$coefficients["price"] * yoghurt_df_1_1$price[j] * (1 - yoghurt_df_1_1$share[j])
    } else {
      elasticities_matrix[j,i] <- ols_demand_system$coefficients["price"] * yoghurt_df_1_1$price[i] * yoghurt_df_1_1$share[i]
      elasticities_matrix[i,j] <- ols_demand_system$coefficients["price"] * yoghurt_df_1_1$price[i] * yoghurt_df_1_1$share[i]
    }
  }
}

# Plot matrix of own/cross price elasticities
elasticities_matrix %>%
  reshape2::melt(.) %>%
  ggplot(., aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(alpha = 0.4) +
  geom_text(aes(label = round(value, 2), family = "Palatino", size = 20), color = "black") +
  scale_fill_gradient(low = "yellow", high = "blue") +
  labs(title = "Own/Cross Price Elasticities", x = "Product ID", y = "Product ID", fill = "Value") +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(family = "Palatino"))
ggplot2::ggsave(plot = last_plot(), filename = paste0(root_exhibits, "question4.pdf"), 
                  units = "cm", height = 12, width = 18)

### Question 5

# Fill in matrix of diversion ratios
diversions_matrix <- matrix(nrow = 5, ncol = 5)
for(j in 1:5){
  for(i in 1:5){
    if(i == j){
      diversions_matrix[j,j] <- yoghurt_df_1_1$share[j] / (1 - yoghurt_df_1_1$share[i])
    } else {
      diversions_matrix[j,i] <- yoghurt_df_1_1$share[i] / (1 - yoghurt_df_1_1$share[j])
      diversions_matrix[i,j] <- yoghurt_df_1_1$share[j] / (1 - yoghurt_df_1_1$share[i])
    }
  }
}

diversions_matrix %>%
  reshape2::melt(.) %>%
  ggplot(., aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(alpha = 0.4) +
  geom_text(aes(label = round(value, 2), family = "Palatino", size = 20), color = "black") +
  scale_fill_gradient(low = "yellow", high = "blue") +
  labs(title = "Diversion Ratios", x = "Product j", y = "Product k", fill = "Value") +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(family = "Palatino"))
ggplot2::ggsave(plot = last_plot(), filename = paste0(root_exhibits, "question5.pdf"), 
                units = "cm", height = 12, width = 18)


### Question 10

# Estimate demand system w/ fixed effects in lieu of product characteristics
form <- as.formula("log(share) - log(outside_share) ~ price + as.factor(period) + as.factor(product) + as.factor(city) - 1")
ols_demand_system <- lm(formula = form, data = yoghurt_df)
robust_vcov <- sandwich::vcovHC(ols_demand_system, type = "HC1")
robust_se <- sqrt(diag(robust_vcov))

# Export regression results
stargazer::stargazer(
  ols_demand_system,
  title = "Estimated Demand System",
  label = "tab:question10",
  dep.var.caption = "",
  se = list(robust_se),
  dep.var.labels = "\\(\\ln s_{jct} - \\ln s_{0ct}\\)",
  omit = c("city", "period", "product"),
  covariate.labels = c("Price"),
  omit.stat = c("rsq", "adj.rsq", "f", "ser")) %>%
  starpolishr::star_tex_write(file = paste0(root_exhibits, "question10.tex"))

# Fill in matrix of elasticities
elasticities_matrix <- matrix(nrow = 5, ncol = 5)
for(j in 1:5){
  for(i in 1:5){
    if(i == j){
      elasticities_matrix[j,j] <- ols_demand_system$coefficients["price"] * yoghurt_df_1_1$price[j] * (1 - yoghurt_df_1_1$share[j])
    } else {
      elasticities_matrix[j,i] <- ols_demand_system$coefficients["price"] * yoghurt_df_1_1$price[i] * yoghurt_df_1_1$share[i]
      elasticities_matrix[i,j] <- ols_demand_system$coefficients["price"] * yoghurt_df_1_1$price[i] * yoghurt_df_1_1$share[i]
    }
  }
}

# Plot matrix of own/cross price elasticities
elasticities_matrix %>%
  reshape2::melt(.) %>%
  ggplot(., aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(alpha = 0.4) +
  geom_text(aes(label = round(value, 2), family = "Palatino", size = 20), color = "black") +
  scale_fill_gradient(low = "yellow", high = "blue") +
  labs(title = "Own/Cross Price Elasticities", x = "Product ID", y = "Product ID", fill = "Value") +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(family = "Palatino"))
ggplot2::ggsave(plot = last_plot(), filename = paste0(root_exhibits, "question10.pdf"), 
                units = "cm", height = 12, width = 18)

### Question 11

# Add marginal costs to dataframe
yoghurt_df_1_1 %>%
  dplyr::mutate(
    marginal_cost = round(price + 1/(ols_demand_system$coefficients["price"]*(1-share)),3)) %>%
  dplyr::select(c(product, marginal_cost)) %>%
  stargazer::stargazer(
    .,
    title = "Marginal Costs by Product",
    label = "tab:question11",
    rownames = FALSE,
    summary = FALSE, 
    covariate.labels = c("Product", "Marginal Cost")) %>%
  starpolishr::star_tex_write(file = paste0(root_exhibits, "question11.tex"))

### Question 13

# Estimate demand system w/ fixed effects and IV
ivreg <- AER::ivreg(
  log(share) - log(outside_share) ~ price + as.factor(period) + as.factor(product) + as.factor(city) - 1 |
    I(distance * diesel) + as.factor(period) + as.factor(product) + as.factor(city) - 1,
  data = yoghurt_df)
# robust_vcov <- sandwich::vcovHC(ivreg, type = "HC1")
# robust_se <- sqrt(diag(robust_vcov))

# Fill in matrix of elasticities
elasticities_matrix <- matrix(nrow = 5, ncol = 5)
for(j in 1:5){
  for(i in 1:5){
    if(i == j){
      elasticities_matrix[j,j] <- ivreg$coefficients["price"] * yoghurt_df_1_1$price[j] * (1 - yoghurt_df_1_1$share[j])
    } else {
      elasticities_matrix[j,i] <- ivreg$coefficients["price"] * yoghurt_df_1_1$price[i] * yoghurt_df_1_1$share[i]
      elasticities_matrix[i,j] <- ivreg$coefficients["price"] * yoghurt_df_1_1$price[i] * yoghurt_df_1_1$share[i]
    }
  }
}

# Plot matrix of own/cross price elasticities
elasticities_matrix %>%
  reshape2::melt(.) %>%
  ggplot(., aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(alpha = 0.4) +
  geom_text(aes(label = round(value, 2), family = "Palatino", size = 20), color = "black") +
  scale_fill_gradient(low = "yellow", high = "blue") +
  labs(title = "Own/Cross Price Elasticities", x = "Product ID", y = "Product ID", fill = "Value") +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(family = "Palatino"))
ggplot2::ggsave(plot = last_plot(), filename = paste0(root_exhibits, "question13.pdf"), 
                units = "cm", height = 12, width = 18)

### Question 14

# Report marginal costs
yoghurt_df_1_1 %>%
  dplyr::mutate(
    marginal_cost = round(price + 1/(ivreg$coefficients["price"]*(1-share)),3)) %>%
  dplyr::select(c(product, marginal_cost)) %>%
  stargazer::stargazer(
    .,
    title = "Marginal Costs by Product",
    label = "tab:question14",
    rownames = FALSE,
    summary = FALSE, 
    covariate.labels = c("Product", "Marginal Cost")) %>%
  starpolishr::star_tex_write(file = paste0(root_exhibits, "question14.tex"))

### Question 15
yoghurt_df_1_1 %>%
  dplyr::mutate(
    price = round(price,3),
    marginal_cost = round(price + 1/(ivreg$coefficients["price"]*(1-share)),3),
    price_constructed = round(marginal_cost - 1/(ivreg$coefficients["price"]*(1-share)),3)) %>%
  dplyr::select(c(product, price, marginal_cost, price_constructed)) %>%
  stargazer::stargazer(
    .,
    title = "Constructed Prices by Product",
    label = "tab:question15",
    rownames = FALSE,
    summary = FALSE, 
    digits = 3,
    covariate.labels = c("Product", "Price", "Marginal Cost", "Constructed Price")) %>%
  starpolishr::star_tex_write(file = paste0(root_exhibits, "question15.tex"))

### Question 16

# Save price elasticity
alpha <- ivreg$coefficients["price"]

# Save market shares and prices
s <- yoghurt_df_1_1$share
p <- yoghurt_df_1_1$price

# Compute marginal costs
mc <- p + 1 / (alpha * (1 - s))

# Set up Jacobian for simple logit
J <- outer(1:length(s), 1:length(s), Vectorize(function(j,k) {
  if (j == k) alpha * s[j] * (1 - s[j]) else -alpha * s[j] * s[k]
}))

# Set ownership matrix after merger of products 2 and 3
firm <- yoghurt_df_1_1$product
firm[firm %in% c(2,3)] <- 23   # relabel merged firm
O <- outer(firm, firm, FUN = function(a,b) as.integer(a==b))

# Set delta
Delta <- - J * O

# Compute markups and new prices
m_new <- solve(Delta, s)
p_new <- mc + m_new

# Export results to LaTeX
yoghurt_df_1_1 %>%
  mutate(price_old = round(p,3),
         price_new = round(p_new,3),
         mc = round(mc,3)) %>%
  dplyr::select(c(product, price_old, mc, price_new)) %>%
  stargazer::stargazer(
    .,
    title = "Prices Before vs. After Merger",
    label = "tab:question16",
    rownames = FALSE,
    summary = FALSE, 
    covariate.labels = c("Product", "Old Price", "Marginal Cost", "New Price")) %>%
  starpolishr::star_tex_write(file = paste0(root_exhibits, "question16.tex"))

### Question 17

# Save results
results <- yoghurt_df_1_1 %>%
  mutate(price_old = round(p,3),
         price_new = round(p_new,3),
         mc = round(mc,3))

# Compute change in consumer welfare
delta_cs <- sum((results$price_new - results$price_old)*results$share)
print(delta_cs)

### Question 20

# Estimate demand system w/ fixed effects and IV
yoghurt_df %<>%
  ungroup() %>%
  dplyr::mutate(
    nest = case_when(
      product %in% c(1,4,5) ~ 1L,
      product %in% c(2,3)   ~ 2L,
      TRUE ~ NA_integer_)) %>%
  group_by(period, nest, city) %>%
  dplyr::mutate(
    total_share = sum(share),
    nest_size = n()) %>%
  ungroup() %>%
  dplyr::mutate(nest_share = share / total_share) %>%
  dplyr::arrange(city, period, product)
yoghurt_df_1_1 <- yoghurt_df %>% dplyr::filter(period == 1 & city == 1)

ivreg <- AER::ivreg(
  log(share) - log(outside_share) ~ price + log(nest_share) + as.factor(period) + as.factor(product) + as.factor(city) - 1 |
    I(distance * diesel) + nest_size + as.factor(period) + as.factor(product) + as.factor(city) - 1,
  data = yoghurt_df)
# robust_vcov <- sandwich::vcovHC(ivreg, type = "HC1")
# robust_se <- sqrt(diag(robust_vcov))

### Question 21

# Fill in matrix of elasticities
elasticities_matrix <- matrix(nrow = 5, ncol = 5)
for(j in 1:5){
  for(i in 1:5){
    if(i == j){
      elasticities_matrix[j,j] <- ivreg$coefficients["price"] * yoghurt_df_1_1$price[j] * (1 - yoghurt_df_1_1$share[j])
    } else {
      elasticities_matrix[j,i] <- ivreg$coefficients["price"] * yoghurt_df_1_1$price[i] * yoghurt_df_1_1$share[i]
      elasticities_matrix[i,j] <- ivreg$coefficients["price"] * yoghurt_df_1_1$price[i] * yoghurt_df_1_1$share[i]
    }
  }
}

# Plot matrix of own/cross price elasticities
elasticities_matrix %>%
  reshape2::melt(.) %>%
  ggplot(., aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(alpha = 0.4) +
  geom_text(aes(label = round(value, 2), family = "Palatino", size = 20), color = "black") +
  scale_fill_gradient(low = "yellow", high = "blue") +
  labs(title = "Own/Cross Price Elasticities", x = "Product ID", y = "Product ID", fill = "Value") +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(family = "Palatino"))
ggplot2::ggsave(plot = last_plot(), filename = paste0(root_exhibits, "question21.pdf"), 
                units = "cm", height = 12, width = 18)

### Question 22

# Save price elasticity
alpha <- ivreg$coefficients["price"]       # price coefficient
rho   <- ivreg$coefficients["log(nest_share)"]  # nesting parameter

# Market shares and prices
s    <- yoghurt_df_1_1$share       # unconditional market shares
s_jg <- yoghurt_df_1_1$nest_share  # conditional share within nest
p    <- yoghurt_df_1_1$price

# For single-product firms, nested logit marginal cost formula
mc <- p + 1 / (alpha * (1 - rho + rho * s_jg - s))

# Export results to LaTeX
data.frame(
  Product = c(1,2,3,4,5),
  `Marginal Cost` = mc) %>%
  stargazer::stargazer(
    .,
    title = "Marginal Costs by Product",
    label = "tab:question22",
    rownames = FALSE,
    summary = FALSE, 
    covariate.labels = c("Product", "Marginal Cost")) %>%
  starpolishr::star_tex_write(file = paste0(root_exhibits, "question22.tex"))

