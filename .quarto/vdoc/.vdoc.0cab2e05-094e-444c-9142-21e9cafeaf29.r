#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(arrow)
#
#
#
arrow::open_dataset("data/daily_prices.parquet")$schema
#
#
#
arrow::open_dataset("data/daily_prices.parquet") |>
  slice_sample(n = 10) |>
  collect() |>
  as_tibble()
#
#
#
arrow::open_dataset("data/daily_prices.parquet") |>
  filter(date_raw >= "2025-01-01") |>
  group_by(coin_id) |>
  summarize(avg_market_cap = mean(market_cap_usd, na.rm = TRUE)) |>
  arrange(desc(avg_market_cap)) |>
  collect() |>
  as_tibble()
#
#
#
arrow::open_dataset("data/daily_prices.parquet") |>
  collect() |>
  arrow::write_dataset("data/prices_by_coin", partitioning = "coin_id")
#
#
#
prices <- arrow::open_dataset("data/daily_prices.parquet") |>
  collect() |>
  as_tibble()

metadata <- arrow::open_dataset("data/coin_metadata.parquet") |>
  collect() |>
  as_tibble()

categories <- arrow::open_dataset("data/categories.parquet") |>
  collect() |>
  as_tibble()

categories
#
#
#
metadata |>
  select(coin_id, coin_name, launch_date)
#
#
#
metadata |>
  mutate(
    launch_date_parsed = lubridate::parse_date_time(
      launch_date,
      orders = c("ymd", "Bdy"),
      exact = FALSE
    )
  ) |>
  select(coin_id, coin_name, launch_date, launch_date_parsed) |>
  arrange(launch_date_parsed)
#
#
#
prices |>
  filter(coin_id == "btc") |>
  mutate(date = lubridate::ymd(date_raw)) |>
  ggplot(aes(x = date, y = price_usd)) +
  geom_line() +
  labs(
    title = "Bitcoin Daily Price",
    x = "Date",
    y = "Price (USD)"
  )
#
#
#
prices |>
  filter(coin_id == "btc") |>
  mutate(date = lubridate::ymd(date_raw)) |>
  mutate(market_cap_trillions = market_cap_usd / 1e12) |>
  ggplot(aes(x = date, y = market_cap_trillions)) +
  geom_line() +
  labs(
    title = "Bitcoin Market Cap Over Time",
    x = "Date",
    y = "Market Cap (Trillions USD)"
  )
#
#
#
# Calculate median market cap per coin for ordering
coin_order <- prices |>
  mutate(market_cap_billions = market_cap_usd / 1e9) |>
  group_by(coin_id) |>
  summarize(median_market_cap = median(market_cap_billions, na.rm = TRUE)) |>
  arrange(desc(median_market_cap)) |>
  pull(coin_id)

prices |>
  mutate(
    market_cap_billions = market_cap_usd / 1e9,
    coin_id = factor(coin_id, levels = coin_order)
  ) |>
  ggplot(aes(x = coin_id, y = market_cap_billions)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "Market Cap Distribution Across 22 Coins",
    x = "Coin",
    y = "Market Cap (Billions USD, log scale)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
#
#
prices |>
  left_join(metadata, by = "coin_id") |>
  slice_sample(n = 5)
#
#
#
prices |>
  left_join(metadata, by = "coin_id") |>
  left_join(
    categories |>
      rename(category_description = description),
    by = "category_id"
  ) |>
  mutate(date = lubridate::ymd(date_raw)) |>
  summarize(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE)
  )
#
#
#
#
#
