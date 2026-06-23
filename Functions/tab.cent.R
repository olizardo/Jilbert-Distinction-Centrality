tab.cent <- function(x, 
      d = 3, 
      name = "name", 
      caption = "", 
      label = "label"
      ) { 
library(kableExtra)
library(dplyr)
w <- distinct(x[, 3:6])
z <- w[, 1]
for (i in 1:length(z)) {
      if (i == 1) {
      e <- as.character(subset(x, scd == z[i])[, 1])
      if (length(e) > 1) {
            e <- paste(e, collapse = ", ")
            }
      l <- e
      }
      if (i > 1) {
      e <- as.character(subset(x, scd == z[i])[, 1])
      if (length(e) > 1) {
            e <- paste(e, collapse = ", ")
            }
      l <- c(l, e)
      }
   }
x <- cbind(l, w)
t <- kbl(x, 
      format = "latex", booktabs = TRUE, linesep = "",
      digits = d, row.names = FALSE,
      col.names = c("Nodes", "$\\beta_i$", "$s_i$", "$\\kappa_i$", "$\\alpha$"),
      caption = caption, label = label, escape = FALSE
      ) %>% 
      kable_styling(bootstrap_options = c("hover", "condensed", "responsive")) %>% 
      save_kable(file = here("Tabs", paste(name, ".tex", sep = "")))
}