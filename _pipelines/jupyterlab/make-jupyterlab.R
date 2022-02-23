library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        load_yaml(settings_path)
    }), deps = "settings_path"), input_force = targets::tar_target_raw("force", 
    quote({
        settings[["force"]]
    }), deps = "settings"), input_port = targets::tar_target_raw("port", 
    quote({
        settings[["port"]]
    }), deps = "settings"), input_host = targets::tar_target_raw("host", 
    quote({
        settings[["host"]]
    }), deps = "settings"), input_token = targets::tar_target_raw("token", 
    quote({
        settings[["token"]]
    }), deps = "settings"), check_jupyter_alive = targets::tar_target_raw(name = "jupyter_list", 
    command = quote({
        {
            source("R/status.R")
            jupyter_list <- jupyter_server_status(port = port, 
                force = force)
            jupyter_list$alive
        }
        return(jupyter_list)
    }), deps = c("port", "force"), cue = targets::tar_cue("always"), 
    pattern = NULL, iteration = "list"), start_jupyter = targets::tar_target_raw(name = "jupyter_url", 
    command = quote({
        {
            if (!jupyter_list$alive) {
                source("R/status.R")
                rpymat::jupyter_launch(workdir = raveio::raveio_getopt("data_dir"), 
                  host = host, port = port, open_browser = FALSE, 
                  async = TRUE, token = token, use_rs = FALSE)
                settings <- raveio::load_yaml("settings.yaml")
                settings$force <- FALSE
                raveio::save_yaml(settings, "settings.yaml")
                Sys.sleep(3)
                jupyter_list <- jupyter_server_status(port = port, 
                  force = FALSE)
            }
            instance <- jupyter_list$instances[jupyter_list$instances$port == 
                port]
            jupyter_url <- sprintf("http://%s:%s/jupyter/lab?token=%s", 
                instance$host, instance$port, instance$token)
            jupyter_url
        }
        return(jupyter_url)
    }), deps = c("jupyter_list", "host", "port", "token"), cue = targets::tar_cue("always"), 
    pattern = NULL, iteration = "list"))
