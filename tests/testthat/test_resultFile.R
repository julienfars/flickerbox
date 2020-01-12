# Test existant file
expect_equal(resultFile("data/01_F03_OD_2013-06-07_140259.txt")$type, "L")
expect_equal(resultFile("data/01_F03_OD_2013-06-07_140259.txt")$sensitivity, 143.7229, tolerance = .0001)

# Wrong file name
expect_error(resultFile("data/not_existant.txt"))

# No staircase terminated
expect_warning(resultFile("data/21_F03_OD_2013-06-07_140259.txt"))

# No answers given
expect_warning(resultFile("data/22_F03_OD_2013-06-07_140259.txt"))
expect_equal(resultFile("data/22_F03_OD_2013-06-07_140259.txt")$numberOfQuestions, 0)

# Added lines in the header
expect_equal(resultFile("data/10_F03_OD_2013-06-07_140259.txt")$type, "L")
expect_equal(resultFile("data/10_F03_OD_2013-06-07_140259.txt")$sensitivity, 143.7229, tolerance = .0001)

# Removed lines in the header
expect_equal(resultFile("data/20_F03_OD_2013-06-07_140259.txt")$type, "L")
expect_equal(resultFile("data/20_F03_OD_2013-06-07_140259.txt")$sensitivity, 143.7229, tolerance = .0001)