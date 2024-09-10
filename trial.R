metabage$...2

# Function to add a decimal point after the first two digits
add_single_decimal <- function(x) {
  # Find the position to insert the decimal point
  # Insert the decimal point after the second digit
  paste0(substr(x, 1, 2), ".", substr(x, 3, nchar(x)))
}

# Apply function to each element
formatted_numbers <- sapply(metabage$...2, add_decimal)
metabage$...2 = paste0(substr(metabage$...2, 1, 2), ".", substr(metabage$...2,3,nchar(metabage$...2)))
metabage$...2 = round(as.numeric(metabage$...2), digits = 2)
sum(is.na(metabage$...2))
