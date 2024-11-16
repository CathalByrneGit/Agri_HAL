#' Start an Ollama Server 
#'
#' Serves an Ollama server which can be used as an api
#'
#' @return a new ollama server process
#' @example \dontrun{
#' # Example: Start the server
#' ollama_server <- start_ollama_server()
#' # Check if the server is running
#' ollama_server$is_alive()
#' }
#' @export
start_ollama_server <- function() {
  # Run the ollama serve command in a separate process
  process <- processx::process$new(
    command = "ollama",
    args = c("serve"),
    stdout = "|", # Redirect output to capture it
    stderr = "|"
  )
  return(process) # Return the process object to manage it
}



#' Stop an Ollama Server 
#'
#' Kill an Ollama server.
#'
#' @return null
#' @example \dontrun{
#' # Example: Start the server
#' ollama_server <- start_ollama_server()
#' 
#' # Check if the server is running
#' ollama_server$is_alive()
#' 
#' # stop server
#' stop_ollama_server(ollama_server)
#' 
#' }
#' @export
stop_ollama_server <- function(server_process) {
  if (!server_process$is_alive()) {
    message("Server is not running.")
  } else {
    server_process$kill()
    message("Ollama server stopped.")
  }
}

