expect_error(getPhotoreceptorCoordinates("data"))
expect_warning(getPhotoreceptorCoordinates("empty_folder", ConeFundamentals10))
expect_warning(getPhotoreceptorCoordinates("only_broken_files", ConeFundamentals10))
expect_warning(getPhotoreceptorCoordinates("one_broken_file", ConeFundamentals10))

