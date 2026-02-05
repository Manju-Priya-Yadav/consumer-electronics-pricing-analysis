# pre-requisites

library(readr)
library(dplyr)
library(stringr)

#### (A) INPUT
# ------------------------------------------------------------------
# 1. Load raw data
# ------------------------------------------------------------------

# Loading data
raw_data <- read_csv(
  "data/raw/DatafinitiElectronicsProductsPricingData.csv"
)

# Look at the structure
glimpse(raw_data)

# Check column names
names(raw_data)


#### (B) TRANSFORMATION 
# ------------------------------------------------------------------
# 2. Select pricing-relevant columns
# ------------------------------------------------------------------

pricing_data <- raw_data %>%
  select(
    brand,
    categories,
    prices.amountMin,
    prices.amountMax,
    prices.currency,
    prices.isSale,
    prices.merchant,
    prices.availability,
    dateUpdated
  )

glimpse(pricing_data)


# ------------------------------------------------------------------
# 3. Basic pricing metrics engineering
# ------------------------------------------------------------------


pricing_data <- pricing_data %>%
  mutate(
    # Reference price (used for benchmarking)
    price_reference = (prices.amountMin + prices.amountMax) / 2,
    
    # Price spread (signals price volatility / inconsistent pricing)
    price_spread = prices.amountMax - prices.amountMin,
    
    # Spread % (helps compare across cheap vs expensive products)
    price_spread_pct = if_else(price_reference > 0,
                               price_spread / price_reference,
                               NA_real_),
    
    # Main category = first category listed
    category_main = str_trim(str_extract(categories, "^[^,]+"))
  )

# ------------------------------------------------------------------
# 4. Pricing Data Quality Checks
# ------------------------------------------------------------------

pricing_checks <- pricing_data %>%
  summarise(
    n_rows = n(),
    missing_price = sum(is.na(price_reference)),
    negative_price = sum(price_reference < 0, na.rm = TRUE),
    max_price = max(price_reference, na.rm = TRUE),
    max_spread_pct = max(price_spread_pct, na.rm = TRUE)
  )

print(pricing_checks)

pricing_data <- pricing_data %>%
  mutate(
    flag_missing_category = is.na(category_main),
    flag_missing_merchant = is.na(prices.merchant),
    flag_extreme_spread = price_spread_pct > 1
  )

pricing_data


#### (C) OUTPUT

dir.create("gen/output", recursive = TRUE, showWarnings = FALSE)

write_csv(
  pricing_data,
  "gen/output/pricing_benchmark_input.csv"
)
