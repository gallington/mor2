load("./data/td.RData")

# Can't figure out how to get it to create a nice looking output.
# need to add more to table specifications and also set labels

library(qwraps2)
options(qwraps2_markup = "markdown")
sumdf<- dfSummary(td)
sumdf

view(dfSummary(td, plain.ascii = FALSE))
sumdf<- td[c(1:2, 8:9, 11, 14, 16, 19:21)]
summary_table(sumdf, )

td %>%            # this needs to be adjusted, it's pulling the wrong vars:
  dplyr::select(c(1:2, 8:9, 11, 16, 19:21)) %>%
  summary_table(.)

dfSummary(td, plain.ascii = FALSE, style = "grid", 
          graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp")
