sourceDirectory <- function(path){
  if(!dir.exists(path)){
    warning(paste0(path," is not a valid path"))
    return(NULL)
  }


env <- parent.frame()
files <- list.files(path = path, pattern = ".*\\.R", all.files = FALSE, full.names = TRUE, recursive = FALSE)

for (f in files) {
  tryCatch({
    source(f,local = env)
    cat(f," is sourced.")
  },
  error = function(condition){
    message("Failed loading the following file \" ",f,"\" .")
    message(condition)
  }
  )
}
}











