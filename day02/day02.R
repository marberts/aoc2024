safe <- Vectorize(
  function(x) {
    steps <- x[-1] - x[-length(x)]
    range <- range(abs(steps))
    (all(steps > 0) || all(steps < 0)) && range[1] >= 1 && range[2] <= 3
  }
)

reports <- readLines("day02/input.txt") |>
  lapply(\(x) as.numeric(strsplit(x, " ", fixed = TRUE)[[1]]))

safe_reports <- safe(reports)

ans1 <- sum(safe_reports)

print(sprintf("Part 1: %d", ans1))

unsafe_reports <- reports[!safe_reports]

drop1_reports <- lapply(unsafe_reports, \(x) lapply(seq_along(x), \(i) x[-i]))

ans2 <- sum(sapply(drop1_reports, \(x) any(safe(x))))

print(sprintf("Part 2: %d", ans1 + ans2))
