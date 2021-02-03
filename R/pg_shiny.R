#' Run Web-Based Demonstration
#'
#' Call Shiny to run a web-based application for the demonstration of entropy
#' balancing
#'
#'
#'
#' @examples
#' if(interactive()){
#' pg_demo()}
#'
#' @export
#'
pg_demo <- function() {
    req.pkgs        <- c("shiny", "shinydashboard", "ggplot2")
    chk.uninstalled <- sapply(req.pkgs, function(x) {
        !requireNamespace(x, quietly = TRUE)
    })
    chk.inx         <- which(chk.uninstalled)

    if (0 < length(chk.inx)) {
        msg <- paste("For the GUI to work, please install ",
        ifelse(1 < length(chk.inx), "packages ", "package "),
        paste(req.pkgs[chk.inx], collapse = ", "),
        " by \n install.packages(",
        paste(paste("'", req.pkgs[chk.inx], "'", sep = ""), collapse = ", "),
        ") \n  ",
        sep = "");
        stop(msg, call. = FALSE);
    }


    appDir <- system.file("shiny", package = "pgrwe")
    if (appDir == "") {
        stop("Could not find Shiny directory. Try re-installing `cava`.",
        call. = FALSE)
    }


    shiny::runApp(appDir, display.mode = "normal");
}
