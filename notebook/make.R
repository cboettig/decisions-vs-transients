library(callr)


p1 <- callr::r_bg(function() rmarkdown::render("notebook/ghost-sims.Rmd") )

#  needs to wait for p1 to complete
p2 <- callr::r_bg(function() rmarkdown::render("notebook/ghost-fit.Rmd") )

# greta tasks probably cannot run in parallel given current memory settings
p3 <- callr::r_bg(function() rmarkdown::render("notebook/discrete-sims.Rmd") )


callr::poll(list(p1, p2), -1)


p1$read_output_lines()
p1$get_result()
p1$get_exit_status()
