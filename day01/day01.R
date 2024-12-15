input <- read.table("day01/input.txt", col.names = c("left", "right")) |>
  as.list() |>
  lapply(sort)

ans1 <- sum(abs(input$right - input$left))

print(sprintf("Part 1: %d", ans1))

w <- tabulate(input$right, nbins = max(input$left))[input$left]

ans2 <- sum(input$left * w)

print(sprintf("Part 2: %d", ans2))