pipelineid <- "3504965832666975645900460114637"
user_code <- "51B03"
api_token <- "MDWN53XHYZJ3RW87X0XHTN9502GNCQZ92M0K6HVG3W7J70XH8Y"


get_pipeline_report <- function(user_code, api_token, pipelineid) {
  if (missing(user_code)) {
    warning("Please add a valid user code")
  } else if (missing(api_token)) {
    warning("Please add a valid API token")
  } else if (missing(pipelineid)) {
    warning("Please add a valid pipeline ID")
  } else
    tryCatch({
        lacrm_url <- "https://api.lessannoyingcrm.com"

        r <- httr::GET(lacrm_url, query = list(
          UserCode = user_code,
          APIToken = api_token,
          Function = 'GetPipelineReport',
          Parameters = paste0('{"PipelineId":','"', pipelineid, '"', '}')
        )
        )

        contenido <- httr::content(r, "text")
        contenido <- jsonlite::fromJSON(contenido,
                                        simplifyVector = TRUE)
        contenido <- as.data.frame(contenido)
        contenido <- jsonlite::flatten(contenido)

        # Limpiar nombres del data frame
        contenido <- janitor::clean_names(contenido)

        # Limpieza PHONE ----------------------------------------------------------
        # Cambio de listas vacias por data.frame similar con
        # estructura igual a listas con entradas
        for (i in 1:nrow(contenido)) {
          if(typeof(contenido$result_phone[[i]]) == "logical") {
            contenido$result_phone[i] <- (data.frame("Text" = NA, "Type" = NA, "Clean" = NA, "TypeId" = NA))
          } else if (length(contenido$result_phone[[i]]$Text) == 0) {
            contenido$result_phone[i]  <- (data.frame("Text" = NA, "Type" = NA, "Clean" = NA, "TypeId" = NA))
          }
        }



        contenido$result_phone[5][(length(contenido$result_phone[[5]]$Text) == 0)]


        # Aplanar lista uniforme y adjuntarla con dataframe completo por posicion
        phone <- do.call(rbind.data.frame, contenido$result_phone)
        phone <- phone %>%
          select(Text, Type) %>%
          select(phone_numer = Text, phone_type = Type)


  return(r)
}

