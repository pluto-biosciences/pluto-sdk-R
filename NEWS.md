# pluto 0.1.0

## Minor improvements

- Simplified API request syntax for increased performance and improved maintainability
- Added new functions `pluto_create_external_plot()` and `pluto_update_external_plot()` to support simplified endpoints for creating and updating external plots
- Updated `pluto_add_experiment_plot()` and `pluto_update_external_plot()` to use the new simplified endpoints and support script file uploads, while maintaining backward compatibility with the old parameter names (`analysis_name` and `plot_methods`)

# pluto 0.0.1

## Major changes

- Added family of `pluto_get_*()` functions to retrieve data from the Pluto API

- Added family of `pluto_read_*()` functions to read Pluto data directly into a data.frame

- Added function `pluto_add_experiment_plot()` to upload HTML and image files to an Experiment in Pluto
