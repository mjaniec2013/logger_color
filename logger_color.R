
### Custom log messages colors with logger
#   logger package: https://github.com/daroczig/logger



library(logger)
library(crayon)
library(stringr)

# utility function for testing colors used in 'clear_message'
# prints a colored message; both font and backgound colors can be defined
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf



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


# wrapped colors.R
# reference: https://github.com/daroczig/logger/blob/master/demo/colors.R
logger_demo <- function(this_layout) {

  log_layout(this_layout)
  log_threshold(TRACE)
  
  log_info('Starting the script...')
  log_debug('This is the second line')
  log_trace('That is being placed right after the first one.')
  log_warn('Some errors might come!')
  log_error('This is a problem')
  log_debug('Getting an error is usually bad')
  log_error('This is another problem')
  log_fatal('The last problem.')
  
}



logger_demo( clear_layout )

