library(targets)

source("common.R")

# load & combine pipelines
...targets <- raveio::load_targets(
  "make-notch-filter.R"
)


# manually hard-wire two targets
...targets$apply_notch_filter$command$deps <- unique(c(
  ...targets$apply_notch_filter$command$deps,
  "preprocess_tool"
))

...targets
