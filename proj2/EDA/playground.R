

d1 <- read.csv("data/expired_data.csv")
d2 <- read.csv("data/expired_dataextra.csv")
d3 <- read.csv("data/icu_data.csv")
d4 <- read.csv("data/icu_dataextra.csv")

DataExplorer::create_report(d2)

DataExplorer::create_report(d4)

