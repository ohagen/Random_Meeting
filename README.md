# Random Meeting

An R Shiny app for creating balanced random groups while optionally minimizing repeated pairings across rounds and previously saved meetings.

## Run locally

```r
install.packages("shiny")
shiny::runApp()
```

The app accepts comma-, semicolon-, or newline-separated entries. Generate one or more rounds, add accepted results to the in-session meeting history, and download an HTML or CSV report.

The initial example list contains the C3S principal investigators listed on the [C3S “Who we are” page](https://www.c3s-frankfurt.de/who-we-are) when the app was created.
