tab.cent <- function(x, d = 3, name = "name", caption = "", label = "label") { #table function
library(kableExtra)
t <- kbl(x, 
      format = "latex", booktabs = TRUE, linesep = "",
      digits = d, row.names = FALSE,
      col.names = c("Node", "Distinction", "SDistinction", "Status", "Constraint", "Scalar"),
      caption = caption, label = label
      ) %>% 
      kable_styling(bootstrap_options = c("hover", "condensed", "responsive")) %>% 
      save_kable(file = here("Tabs", paste(name, ".tex", sep = "")))
      }