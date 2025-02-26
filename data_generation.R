library(tibble)
library(dplyr)
library(charlatan)

generate_synthetic_data <- function(num_branches=5, num_years=3) {
  ch <- charlatan::ch_company
  branches <- replicate(num_branches, ch())
  regions <- c('North America', 'Europe', 'Asia', 'South America', 'Africa')

  data <- tibble(
    Branch = character(),
    Region = character(),
    Year = integer(),
    Revenue = numeric(),
    Sales = numeric(),
    Investments = numeric(),
    Other_Income = numeric(),
    Expenses = numeric(),
    Salaries = numeric(),
    Rent = numeric(),
    Utilities = numeric(),
    Other_Expenses = numeric(),
    Assets = numeric(),
    Liabilities = numeric(),
    Equity = numeric(),
    CashFlow = numeric(),
    Operating_CashFlow = numeric(),
    Investing_CashFlow = numeric(),
    Financing_CashFlow = numeric()
  )

  for (branch in branches) {
    region <- sample(regions, 1)
    for (year in 2023:(2023 + num_years - 1)) {
      revenue <- runif(1, 1000000, 10000000)
      sales <- runif(1, 500000, 5000000)
      investments <- runif(1, 100000, 500000)
      other_income <- revenue - (sales + investments)
      expenses <- runif(1, 500000, 9000000)
      salaries <- runif(1, 200000, 4000000)
      rent <- runif(1, 50000, 200000)
      utilities <- runif(1, 30000, 100000)
      other_expenses <- expenses - (salaries + rent + utilities)
      assets <- runif(1, 5000000, 20000000)
      liabilities <- runif(1, 1000000, 8000000)
      equity <- assets - liabilities
      operating_cash_flow <- runif(1, 200000, 2000000)
      investing_cash_flow <- runif(1, -1000000, 1000000)
      financing_cash_flow <- runif(1, -500000, 500000)
      cash_flow <- operating_cash_flow + investing_cash_flow + financing_cash_flow

      data <- data %>% add_row(
        Branch = branch,
        Region = region,
        Year = year,
        Revenue = revenue,
        Sales = sales,
        Investments = investments,
        Other_Income = other_income,
        Expenses = expenses,
        Salaries = salaries,
        Rent = rent,
        Utilities = utilities,
        Other_Expenses = other_expenses,
        Assets = assets,
        Liabilities = liabilities,
        Equity = equity,
        CashFlow = cash_flow,
        Operating_CashFlow = operating_cash_flow,
        Investing_CashFlow = investing_cash_flow,
        Financing_CashFlow = financing_cash_flow
      )
    }
  }

  return(data)
}

# Generate data
df <- generate_synthetic_data(5, 3)
write.csv(df, 'financial_data.csv', row.names = FALSE)
