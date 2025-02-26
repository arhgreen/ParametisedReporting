library(tibble)
library(dplyr)
library(faker)

generate_synthetic_data <- function(num_branches=5, num_years=3) {
  branches <- replicate(num_branches, company())
  regions <- c('North America', 'Europe', 'Asia', 'South America', 'Africa')

  data <- tibble()

  for (branch in branches) {
    region <- sample(regions, 1)
    for (year in 2023:(2023 + num_years - 1)) {
      income <- runif(1, 1000000, 10000000)
      expenses <- runif(1, 500000, 9000000)
      assets <- runif(1, 5000000, 20000000)
      liabilities <- runif(1, 1000000, 8000000)
      equity <- assets - liabilities
      cash_flow <- income - expenses

      data <- data %>% add_row(
        Branch = branch,
        Region = region,
        Year = year,
        Income = income,
        Expenses = expenses,
        Assets = assets,
        Liabilities = liabilities,
        Equity = equity,
        CashFlow = cash_flow
      )
    }
  }

  return(data)
}

# Generate data
df <- generate_synthetic_data()
write.csv(df, 'financial_data.csv', row.names = FALSE)