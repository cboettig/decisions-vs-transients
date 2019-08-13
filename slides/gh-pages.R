library(fs)
library(here)

fs::dir_create(here("docs"))

slides <- here("slides/solarized.html")
#fs::link_create(slides, here("docs/index.html"))
fs::file_copy(slides, here("docs/index.html"), overwrite = TRUE)
fs::link_create(here("slides/img"), here("docs/img"))
fs::link_create(here("slides/libs"), here("docs/libs"))
fs::link_create(here("slides/solarized_files"), here("docs/solarized_files"))
fs::link_create(here("slides/solarized-light.css"), here("docs/solarized-light.css"))
