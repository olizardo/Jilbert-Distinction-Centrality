tab.cent <- function(x, 
      d = 3, 
      name = "name", 
      caption = "", 
      label = "label",
      rownames) { 
library(kableExtra)
library(dplyr)
x <- distinct(x[, 2:6])
x <- cbind(rownames, x)
t <- kbl(x, 
      format = "latex", booktabs = TRUE, linesep = "",
      digits = d, row.names = FALSE,
      col.names = c("Nodes", "Distinction", "S-Distinction", "Status", "Constraint", "Scalar"),
      caption = caption, label = label
      ) %>% 
      kable_styling(bootstrap_options = c("hover", "condensed", "responsive")) %>% 
      save_kable(file = here("Tabs", paste(name, ".tex", sep = "")))
}