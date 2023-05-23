#' checkpaqs
#'
#' The function checks whether the packages have been loaded in the session or not. Also, before that, it checks if the packages are installed or not and installs them if they are not.
#' @param libs c-type vector compound by the names of the libraries to be loaded.
#' @export
#' @examples 
#' checkpaqs("dplyr")
checkpaqs <- function(libs){
    pLog("Checking if the packages ", paste(libs, collapse = ", "), "are installed and loaded.")
    for (libreria in libs)
    {
        if (!any(libreria %in% installed.packages()[, 1]))
        {
            pLog("'", libreria, "' is not installed. Trying to install it now...");
            install.packages(libreria)
            if (!any(libreria %in% installed.packages()[, 1]))
            {
                pLog("For some reason the library could not be installed. Check Internet connection or the if the package's name ('", libreria, "') is well writen.")
                return(libreria)
            }
        } else
        {
            pLog("'", libreria, "' already installed.'")
        }
        pLog("Loading '", libreria, "'...")
        library(package = libreria, character.only = T)
    };
    pLog("Packages checked and loaded.")
}