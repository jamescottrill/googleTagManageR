is_default_project <- function(){
  getOption("googleAuthR.client_id") %in% c("301891352590-qnhjsr3nblisijpb2jssbnclujqepuci.apps.googleusercontent.com")
}

default_project_message <- function(){

  if(is_default_project()){
    myMessage("Default Google Project for googleTagManageR is set.  \n This is shared with all googleTagManageR users. \n If making a lot of API calls, please: \n visit: https://bit.ly/2Evk6hn \n for instructions on setting your own Google Project \n",
              level = 3)

  }


}

# custom error messages for googleTagManageR
# otherwise googleAuthR handles them
custom_error <- function(err){
  if(grepl("insufficient tokens for quota",err$message)){
    default_project_message()
    stop("The Google Project ", getOption("googleAuthR.client_id") ," has run out of quota.", call. = FALSE)
  } else {
    stop(err$message, call. = FALSE)
  }
}
