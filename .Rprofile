# Also source the user's own .Rprofile if it exists
if (file.exists("~/.Rprofile")) {
    source("~/.Rprofile")
}

# Formatting for styler at the project level:
# 4-tab indent; use = instead of <-
options(languageserver.formatting_style = function(options) {
    style <- styler::tidyverse_style(indent_by = 4)
    style$token$force_assignment_op <- NULL
    style
})
