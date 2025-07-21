tab.cent <- function(x, 
      d = 3, 
      name = "name", 
      caption = "", 
      label = "label",
      rownames) { 
library(kableExtra)
library(dplyr)
x <- distinct(x[, 3:6])
x <- cbind(rownames, x)
t <- kbl(x, 
      format = "latex", booktabs = TRUE, linesep = "",
      digits = d, row.names = FALSE,
      col.names = c("Nodes", "$\\beta_i$", "$s_i$", "$\\kappa_i$", "$\\alpha$"),
      caption = caption, label = label, escape = FALSE
      ) %>% 
      kable_styling(bootstrap_options = c("hover", "condensed", "responsive")) %>% 
      save_kable(file = here("Tabs", paste(name, ".tex", sep = "")))
}