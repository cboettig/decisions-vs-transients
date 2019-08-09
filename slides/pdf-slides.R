#pagedown::chrome_print(here::here("slides/boettiger.Rmd"), timeout=3000)



library(webshot)
install_phantomjs()
file_name <- paste0("file://", here::here("slides/boettiger.Rmd"))
webshot(file_name, "slides.pdf")


