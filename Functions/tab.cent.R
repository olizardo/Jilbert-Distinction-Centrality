tab.cent <- function(x, 
      d = 3, 
      name = "name", 
      caption = "", 
      label = "label"
      ) { 
  library(kableExtra)
  library(dplyr)
  library(here)
  
  # Group nodes by their scores to present them together, ensuring no duplicate rows
  x_grouped <- x %>%
    group_by(scd, s, u, scalar) %>%
    summarise(Nodes = paste(n, collapse = ", "), .groups = "drop") %>%
    select(Nodes, scd, s, u, scalar) %>%
    arrange(desc(scd))
  
  t <- kbl(x_grouped, 
        format = "latex", booktabs = TRUE, linesep = "",
        digits = d, row.names = FALSE,
        col.names = c("Nodes", "$\\beta_i$", "$s_i$", "$\\kappa_i$", "$\\alpha$"),
        caption = caption, label = label, escape = FALSE
        ) %>% 
        kable_styling(bootstrap_options = c("hover", "condensed", "responsive")) %>% 
        save_kable(file = here("Tabs", paste(name, ".tex", sep = "")))
}
