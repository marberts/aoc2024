parse_mul <- function(x) {
  digits <- regmatches(x, gregexpr("\\d+", x))
  vapply(digits, \(x) prod(as.numeric(x)), numeric(1))
}

mem <- paste(readLines("day03/input.txt"), collapse = "")

ans1 <- regmatches(mem, gregexpr("mul\\(\\d+,\\d+\\)", mem))[[1]] |>
  parse_mul() |>
  sum()

print(sprintf("Part 1: %d", ans1))

apply_control <- function(x) {
  res <- rep(FALSE, length(x))
  state <- "do"
  for (i in seq_along(x)) {
    if (x[i] == "do") {
      state <- "do"
    } else if (x[i] == "don't") {
      state <- "don't"
    } else {
      if (state == "do") {
        res[i] <- TRUE
      } else {
        res[i] <- FALSE
      }
    }
  }
  x[res]
}

ans2 <- regmatches(mem, gregexpr("mul\\(\\d+,\\d+\\)|do|don't", mem))[[1]] |>
  apply_control() |>
  parse_mul() |>
  sum()

print(sprintf("part 2: %d", ans2))
