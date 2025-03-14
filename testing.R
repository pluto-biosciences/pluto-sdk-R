
install.packages("devtools")
devtools::load_all()


Sys.setenv(PLUTO_ENV="prod")
pluto_login("0cc3b4f2a559301de2e7e40a26fee82a76b55f51")
api_token <- "0cc3b4f2a559301de2e7e40a26fee82a76b55f51"
experiment_id <- "PLX276265"
display_file_path <- "test.html"
file.exists("test.html")  # Should return TRUE if the file exists

pluto_login(api_token)

external_analysis <- pluto_add_experiment_plot(
  experiment_id = experiment_id,
  display_file_path = "test.html",
  analysis_name = "test 12",
  plot_methods = "methods test"
)

