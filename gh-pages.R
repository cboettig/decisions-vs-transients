library(fs)
library(here)

fs::dir_create(fs::path_rel("docs"))

slides <- fs::path_rel("slides/solarized.html")
fs::link_create(slides, fs::path_rel("docs/index.html"))
#fs::file_copy(slides, fs::path_rel("docs/index.html"), overwrite = TRUE)
fs::link_create(fs::path_rel("slides/img"), fs::path_rel("docs/img"))
fs::link_create(fs::path_rel("slides/libs"), fs::path_rel("docs/libs"))
fs::link_create(fs::path_rel("slides/solarized_files"), fs::path_rel("docs/solarized_files"))
fs::link_create(fs::path_rel("slides/solarized-light.css"), fs::path_rel("docs/solarized-light.css"))
