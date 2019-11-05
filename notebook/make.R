library(callr)

p4 <- callr::r_bg(function() rmarkdown::render(here::here("notebook/complete-discrete-version.Rmd"))
                  #env = c(LD_PRELOAD="libnvblas.so")
)

p1 <- callr::r_bg(function() rmarkdown::render("notebook/greta-gp.Rmd") )

p3 <- callr::r_bg(function() rmarkdown::render(here::here("manuscript/appendix/appendix.Rmd")) )






# greta tasks probably cannot run in parallel given current memory settings



p4 <- callr::r_bg(function() rmarkdown::render("notebook/may-outbreak-ghost.Rmd"),
                  env = c(LD_PRELOAD="libnvblas.so"))


p5 <- callr::r_bg(function() rmarkdown::render("notebook/may-outbreak-ghost-2.Rmd") )


callr::poll(list(p4), -1)


p4$read_output_lines()
p4$get_result()
p4$get_exit_status()
