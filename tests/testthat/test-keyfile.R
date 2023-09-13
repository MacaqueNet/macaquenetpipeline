# this file tests consistency within the data key
# 1) are matrix IDs unique (e.g. if there are two proximity matrices, are they labeled uniquely?)
# 2) are species names labelled consistently

# context("matrices are labeled uniquely (no two matrices with the same name)")

#### test 1: unique matrix ids ----------------
key <- get_key()

test_that("matrices are labeled uniquely", {
  expect_false(any(duplicated(key$mat_id)))
})

# mistakes, if any:
key$mat_id[duplicated(key$mat_id)]

#### test 2: consistent species names --------------
test_that("species names are consistent in key file", {
  key <- get_key()

  xnames <- names(table(key$species))
  allspecs <- c("mulatta", "sylvanus", "tonkeana", "nemestrina", "fuscata",
                "assamensis", "nigra", "radiata", "silenus", "fascicularis",
                "maura", "thibetana", "leonina", "arctoides")

  expect_true(all(xnames %in% allspecs))
})

#### test 3: sampling_type matches obseff_column and sampling interval ----------------
# only applies to interaction data sets (and only those that are deployable)
key <- get_key(ready_to_deploy_only = TRUE)
key <- key[key$cat_global != "subjectdata", ]

test_that("no NA values in sampling_method, data_type", {
  expect_true(all(!is.na(key$sampling_method)))
  expect_true(all(!is.na(key$data_type)))
})

test_that("no NA values in obseff_column, except for adlib sampling...", {
  expect_true(all(!is.na(key$obseff_column[which(key$sampling_method != "adlib")]))) # only when smpling_method == "adlib" the obseff column can be NA (in fact it should be NA)
  expect_true(all(is.na(key$obseff_column[which(key$sampling_method == "adlib")]))) # only when smpling_method == "adlib" the obseff column can be NA (in fact it should be NA)
})




test_that("'data_type' only contains the two correct possibilities", {
  expect_true(all(key$data_type %in% c("count", "duration")))
})


test_that("'sampling_method' only contains the two correct possibilities", {
  expect_true(all(key$sampling_method %in% c("continuous", "discrete", "adlib")))
})



test_that("when 'sampling_type' is 'discrete': exactly one of the interval columns needs to be not NA", {
  expect_false(any(key$sampling_method == "discrete" & (is.na(key$interval) + is.na(key$interval_2)) != 1))
})
# if any:
key[key$sampling_method == "discrete" & (is.na(key$interval) + is.na(key$interval_2)) != 1,  c("running_number", "unit_code", "cat_global", "cat_sub", "sampling_method", "data_type", "interval", "interval_2", "obseff_column")]


test_that("when 'sampling_type' is 'discrete': 'obseff_column' is 'obseff_samples'", {
  expect_false(any(key$sampling_method == "discrete" & !key$obseff_column %in% c("obseff_samples", "obseff_samples_2")))
})
# if any:
key[key$sampling_method == "discrete" & key$obseff_column != "obseff_samples",  c("running_number", "unit_code", "cat_global", "cat_sub", "sampling_method", "data_type", "interval", "obseff_column")]


test_that("when 'sampling_type' is 'continuous': 'interval' needs to be NA", {
  expect_false(any(key$sampling_method == "continuous" & !is.na(key$interval)))
})
# if any:
key[key$sampling_method == "continuous" & !is.na(key$interval),  c("running_number", "unit_code", "cat_global", "cat_sub", "sampling_method", "data_type", "interval", "obseff_column")]


test_that("when 'sampling_type' is 'continuous': 'obseff_column' is 'obseff_duration' or 'obseff_duration_2'", {
  expect_false(any(key$sampling_method == "continuous" & !(key$obseff_column %in% c("obseff_duration", "obseff_duration_2"))))
})
# if any:
key[key$sampling_method == "continuous" & !(key$obseff_column %in% c("obseff_duration", "obseff_duration_2")),  c("running_number", "unit_code", "cat_global", "cat_sub", "sampling_method", "data_type", "interval", "obseff_column")]

# test 4: unit_codes are internally consistent -----------------
key <- get_key(ready_to_deploy_only = TRUE)
unit_code_by_hand <- paste(key$species, tolower(key$source), tolower(key$group_name), key$year, key$within_year_period, sep = "_")
test_that("unit codes match the rule species_source_groupname_year_period", {
  expect_true(all(unit_code_by_hand == key$unit_code))
})
# errors:
key[key$unit_code != unit_code_by_hand, ]


