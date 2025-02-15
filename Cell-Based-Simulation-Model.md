Exploring Diabetes Risk Through a Cell-Based Simulation Model
================
Joshua Edefo
2025-02-15

The cell-based model simulates diabetes progression in a synthetic
population of 1000 individuals over 10 years, factoring in age, gender,
diet, and exercise. Each person is assigned a risk level (Low, Moderate,
or High) based on these factors. Transition probabilities vary by risk
level, affecting the likelihood of staying healthy or progressing to
Pre- diabetes, Diabetes, or complications. After 10 years, most
individuals (894) remain healthy, while fewer progress to Pre-diabetes
(78) or Diabetes (27), and only 1 person experiences complications,
reflecting the long-term impact of lifestyle and demographics on
diabetes progression.

Libraries

``` r
library(usethis)
```

    ## Warning: package 'usethis' was built under R version 4.3.2

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.3.3

Cell Based Model

``` r
## Set directory
setwd("C:\\Users\\joe62\\OneDrive - Aberystwyth University\\Apps\\Desktop\\Destop Folder")

## Set parameters
set.seed(123)  # For reproducibility
years <- 10    # Number of years to simulate
population_size <- 1000

## Generate a synthetic population
population <- data.frame(
  id = 1:population_size,
  age = sample(30:80, population_size, replace = TRUE),
  gender = sample(c("Male", "Female"), population_size, replace = TRUE),
  diet = sample(c("Healthy", "Moderate", "Unhealthy"), population_size, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
  exercise = sample(c("High", "Moderate", "Low"), population_size, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
  state = "Healthy"  # Initial state
)

## Function to assign risk category based on demographics, diet, and exercise
assign_risk <- function(age, gender, diet, exercise) {
  risk <- 0
  if (diet == "Unhealthy") risk <- risk + 2
  if (exercise == "Low") risk <- risk + 2
  if (age > 50) risk <- risk + 1
  if (gender == "Male") risk <- risk + 1
  
  if (risk <= 1) return("Low")
  if (risk == 2 | risk == 3) return("Moderate")
  return("High")
}

## Assign risk levels using vectorized approach
population$risk <- mapply(assign_risk, population$age, population$gender, population$diet, population$exercise)

## Transition probabilities based on risk category
transition_probs <- list(
  Low = c(Healthy = 0.98, PreDiabetes = 0.02, Diabetes = 0, Complications = 0),
  Moderate = c(Healthy = 0.90, PreDiabetes = 0.08, Diabetes = 0.02, Complications = 0),
  High = c(Healthy = 0.75, PreDiabetes = 0.15, Diabetes = 0.08, Complications = 0.02)
)

## Function to simulate state transition based on risk
simulate_transition <- function(risk) {
  probs <- transition_probs[[risk]]
  states <- c("Healthy", "PreDiabetes", "Diabetes", "Complications")
  new_state <- sample(states, 1, prob = c(probs["Healthy"], probs["PreDiabetes"], probs["Diabetes"], probs["Complications"]))
  return(new_state)
}

## Simulate over time
for (year in 1:years) {
  population$state <- sapply(population$risk, simulate_transition)
}

## Summary of final states
state_summary <- table(population$state)
print(state_summary)
```

    ## 
    ## Complications      Diabetes       Healthy   PreDiabetes 
    ##             1            27           894            78

summary of the model output After 10 years, the majority of individuals
(894) remained healthy. A smaller number developed PreDiabetes (78) or
Diabetes (27). Only one individual progressed to complications,

Session information

``` r
sessionInfo()
```

    ## R version 4.3.1 (2023-06-16 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 11 x64 (build 22631)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8 
    ## [2] LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] dplyr_1.1.4   usethis_2.2.2
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] digest_0.6.33     utf8_1.2.3        R6_2.5.1          fastmap_1.2.0    
    ##  [5] tidyselect_1.2.0  xfun_0.40         magrittr_2.0.3    glue_1.6.2       
    ##  [9] tibble_3.2.1      knitr_1.44        pkgconfig_2.0.3   htmltools_0.5.8.1
    ## [13] generics_0.1.3    rmarkdown_2.25    lifecycle_1.0.3   cli_3.6.1        
    ## [17] fansi_1.0.4       vctrs_0.6.5       compiler_4.3.1    purrr_1.0.2      
    ## [21] rstudioapi_0.15.0 tools_4.3.1       pillar_1.9.0      evaluate_0.21    
    ## [25] yaml_2.3.7        rlang_1.1.1       fs_1.6.3

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
