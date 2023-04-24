
### log_info wrapper
###  MJ / GPT-4, 2023-04-12


### CHANGELOG:
#
#   - log_this_set initialize logging with a given log_level held @ LogClass (R6) [2022-04-24]
#   - logger_color.R integrated



library(logger)
library(glue)
library(R6)

library(crayon)
library(stringr)


### copied from logger_color

cat_color <- function(msg, font_color="black", bg_color=NULL) {
  
  color <- make_style(font_color)
  
  if (!is.null(bg_color))
    
    color <- crayon::combine_styles(color, make_style(bg_color, bg=TRUE))
  
  color_msg <- paste0(color(msg), crayon::reset(''))
  
  cat(color_msg)
  
}
# cat_color("Test")
# cat_color("Test", bg_color="white")
# cat_color("Color message", "white", "black")


# modified grayscale_by_log_level function
# reference: https://github.com/daroczig/logger/blob/master/R/color.R
clear_message <- function(msg, level) {
  
  color <- switch(
    attr(level, 'level'),
    'FATAL'   = crayon::make_style('black'),         # changed from gray100
    'ERROR'   = crayon::make_style('tomato3'),       # changed from gray90
    'WARN'    = crayon::make_style('darkgoldenrod'), # changed from gray80
    'SUCCESS' = crayon::make_style('gray70'),
    'INFO'    = crayon::make_style('gray60'),
    'DEBUG'   = crayon::make_style('gray50'),
    'TRACE'   = crayon::make_style('gray40'),
    stop('Unknown log level')
  )
  
  paste0(color(msg), crayon::reset(''))
  
}

# modified layout_glue_colors function
# reference: https://github.com/daroczig/logger/blob/master/R/layouts.R
clear_layout <- layout_glue_generator(
  format = paste(
    '{crayon::bold(colorize_by_log_level(str_pad(level, 5, "right"), levelr))}',  # level text width fixed at 5
    '[{crayon::italic(format(time, "%Y-%m-%d %H:%M:%S"))}]',
    '{clear_message(msg, levelr)}'))                                              # clear_message replaced grayscale_by_log_level


if (!exists("log_layout")) {
  
  library(logger)
  log_layout(clear_layout)
  log_threshold(TRACE)
  
}


### global logging class

LogClass <- R6Class('LogClass',
                    
  private = list(
    
    log_level = NA
    
  ),
  
  public = list(
    
    allowed_levels = c('none', 'info', 'warn', 'error'),
    
    set = function( log_level ) {
      
      if (log_level %in% self$allowed_levels) {
      
        private$log_level <- log_level
      
      } else {
        
        cat(glue("Logging level [{log_level}] not accepted. Allowed levels: {paste(allowed_levels, collapse=', ')}."))
        
      }
      
    },
    
    get = function() {
      
      return( private$log_level)
      
    }
    
  )
                    
)



log_this_set <- function( log_level ) {
  
  logging <<- LogClass$new()
  logging$set(log_level)
  
}
# log_this_set('info')



log_this <- function(message, leading_empty_lines=0) {
    stopifnot(exists('logging'))
  
    switch(logging$get(),
           info  = logger::log_info(message),
           warn  = logger::log_warn(message),
           error = logger::log_error(message),
           none  = {}, # do nothing
           {
             warning(glue::glue("Unregognized logging level [{level}] specified. Defaulting to 'info'."))
             logger::log_info(message)
           })
}
# log_this('log message')
