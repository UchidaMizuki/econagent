source("data-raw/setup.R")

# iotable_dummy -----------------------------------------------------------

file <- "data-raw/iotable_dummy.csv"

col_names <- read_csv(file,
                      col_names = FALSE,
                      col_types = cols(.default = "c"),
                      n_max = 2,
                      name_repair = "minimal") |>
  t() |>
  as_tibble(.name_repair = ~str_c("col_name_", 1:2)) |>
  slice(-(1:2)) |>
  unite("col_name", 1:2,
        sep = "/") |>
  pull(col_name)

iotable_dummy <- read_csv(file,
         col_names = c("input_sector_type", "input_sector_name", col_names),
         col_types = cols(.default = "c"),
         skip = 2) |>
  pivot_longer(!starts_with("input"),
               names_to = c("output_sector_type", "output_sector_name"),
               names_sep = "/",
               values_transform = list(value = parse_number)) |>
  filter(input_sector_type != "total",
         output_sector_type != "total")

exdir <- test_path("data")
dir_create(exdir)

write_rds(iotable_dummy,
          path(exdir, "iotable_dummy.rds"))
