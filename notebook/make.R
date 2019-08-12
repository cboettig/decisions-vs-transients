library(callr)


p1 <- callr::r_bg(function() rmarkdown::render("notebook/ghost-sims.Rmd") )

#  needs to wait for p1 to complete
p2 <- callr::r_bg(function() rmarkdown::render("notebook/ghost-fit.Rmd") )

# greta tasks probably cannot run in parallel given current memory settings
p3 <- callr::r_bg(function() rmarkdown::render("notebook/discrete-sims.Rmd") )



p4 <- callr::r_bg(function() rmarkdown::render("notebook/may-outbreak-ghost.Rmd") )
p5 <- callr::r_bg(function() rmarkdown::render("notebook/may-outbreak-ghost-2.Rmd") )


callr::poll(list(p4,p5), -1)


p4$read_output_lines()
p4$get_result()
p4$get_exit_status()
