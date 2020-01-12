# Test existant file
result01 <- data.frame(
  patid = "01",
  seye = "OD",
  date_of_exam = "2013-06-07 14:02:59",
  frequency = 2,
  rod = -0.003143969,
  scone = -0.003015008,
  mcone = -0.003144055,
  lcone = 0.7192617
)

expect_equal(read.flimmerkiste("data/01_F03_OD_2013-06-07_140259.txt", ConeFundamentals10), 
             result01,
             tolerance = 1e-06)

# Wrong file name
expect_error(read.flimmerkiste("data/not_existant.txt", ConeFundamentals10))

# # No staircase terminated
result02 <- data.frame(
  patid = "21",
  seye = "OD",
  date_of_exam = "2013-06-07 14:02:59",
  frequency = 2,
  rod = mean(NA),
  scone = mean(NA),
  mcone = mean(NA),
  lcone = mean(NA)
)

expect_equal(read.flimmerkiste("data/21_F03_OD_2013-06-07_140259.txt", ConeFundamentals10), 
             result02)

# Added lines in the header
result10 <- data.frame(
  patid = "10",
  seye = "OD",
  date_of_exam = "2013-06-07 14:02:59",
  frequency = 2,
  rod = -0.003143969,
  scone = -0.003015008,
  mcone = -0.003144055,
  lcone = 0.7192617
)
expect_equal(read.flimmerkiste("data/10_F03_OD_2013-06-07_140259.txt", ConeFundamentals10), 
             result10,
             tolerance = 1e-06)

# Removed lines in the header
result20 <- data.frame(
  patid = "20",
  seye = "OD",
  date_of_exam = "2013-06-07 14:02:59",
  frequency = 2,
  rod = -0.003143969,
  scone = -0.003015008,
  mcone = -0.003144055,
  lcone = 0.7192617
)
expect_equal(read.flimmerkiste("data/20_F03_OD_2013-06-07_140259.txt", ConeFundamentals10), 
             result20,
             tolerance = 1e-06)