# Random Meeting

An R Shiny app for creating balanced random groupings while optionally minimizing repeated pairings across rounds and previously saved meetings.

## Access it online

[Random_Meeting](https://ohagen.github.io/Random_Meeting/)

## Run locally

```r
install.packages("shiny")
shiny::runApp()
```

The app accepts comma-, semicolon-, or newline-separated entries. Generate one or more rounds, add accepted results to the in-session meeting history, and download an HTML or CSV report.

The initial example list contains the current C3S principal investigators listed on the [C3S “Who we are” page](https://www.c3s-frankfurt.de/who-we-are) when the app was created (2026-07-10)... Yes, friday afternoon after giving up on the huge TODO list.

To make the drawing transparent and reproducible, the app can generate a fully random drawn or uses a numerical seed. If the same participant list, settings, meeting history, and seed are used (even if not set, for every drawn there is a reported seed), it will generate exactly the same groups. 

Peace!
