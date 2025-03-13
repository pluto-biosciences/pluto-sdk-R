
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
  analysis_name = "test",
  plot_methods = "methods test"
)


# Then try to create the analysis manually
url_path <- paste0("lab/experiments/", experiment_id, "/analyses/")
body_data <- list(
  analysis_type = "external",
  name = "My Analysis",
  origin = "R",
  display_file_id = uploaded_plot$upload_session_uuid,
  results_file_id = "",
  script_file_id = "",
  methods = ""
)
resp_obj <- pluto_POST(url_path, body_data)
print(resp_obj)

# Try uploading with more detailed error handling
tryCatch({
  analysis_result <- pluto_add_experiment_plot(
    experiment_id = experiment_id,
    display_file_path = file_path,
    analysis_name = "My Analysis"
  )
  print("Success!")
  print(analysis_result)
}, error = function(e) {
  print(paste("Error:", e$message))
})


upload_result <- pluto_upload(experiment_id, display_file_path)
pluto_add_experiment_plot(experiment_id, display_file_path)

analysis_result <- create_analysis(
  experiment_id = experiment_id,
  upload_session_uuid = upload_result$upload_session_uuid,
  analysis_name = "Test External Analysis",
  methods = "Test methods description"
)
analysis_name <- "External"
methods <- ""

