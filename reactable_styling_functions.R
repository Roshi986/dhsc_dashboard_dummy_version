# Function for adding bars to the cells
# This is passed into argument 'cell' in reactable::colDef() and should comprise of a value, a row index and the column name
# Here, we need the value and the column name. The latter is used to scale the data so as to determing the width of the bars for each value
# There's no need to supply values to the function's arguments. It's by default set up to take the three arguments above, in the order they were mentioned
reactive_table_bars_standard <- function(value, index, scaling_variable) {
  if (is.numeric(value)) {
    value[!is.finite(value)] <- 0
    width <- paste0(abs(value) * 100 / 
      max(abs(table_data[[scaling_variable]]), na.rm = TRUE), "%") # Hard-code here the data frame name to be subsetted
    value <- format(value, big.mark = ",")
    value <- format(value, width = 9, justify = "left")
    
    # A bar will accompany the value for a visual feel of the magnitude
    # We need two bars: one whose length is the value (front bar); and one with 100% length in the background. The second bar helps the user understand how big/small a value is
    # The front bar is a div inside the background bar. The latter has 0.5 opacity so it doesn't hide the former
    # Placing the front bar div inside the background bar div, instead of the other way around, fixes the length of the bars when a user may expand the column to the right to reveal the whole column name
    # Doing it the other way around would result in the front bar being stretched more than 100% of the value as the user expands the column, resulting in a horizontally stacked bar that makes no sense
    bar <- div(
      class = "bar-chart",
      style = list(marginRight = "6px"),
      div(
        class = "bar_back", 
        style = list(width = 100, backgroundColor = '#CCC', opacity = 0.5),
        div(
          class = "bar_front", 
          style = list(width = width, backgroundColor = '#00ad93')
        )
      )
    )
    
    div(
      class = "bar-cell", 
      value, bar
    )
  } else {
    value
  }
}

# Same function as above, but bars have rounded corners
reactive_table_bars_rounded <- function(value, index, scaling_variable) {
  if (is.numeric(value)) {
    value[!is.finite(value)] <- 0
    width <- paste0(abs(value) * 100 / 
                      max(abs(table_data[[scaling_variable]]), na.rm = TRUE), "%") # Hard-code here the data frame name to be subsetted
    value <- format(value, big.mark = ",")
    value <- format(value, width = 9, justify = "left")
    
    # A bar will accompany the value for a visual feel of the magnitude
    # We need two bars: one whose length is the value (front bar); and one with 100% length in the background. The second bar helps the user understand how big/small a value is
    # The front bar is a div inside the background bar. The latter has 0.5 opacity so it doesn't hide the former
    # Placing the front bar div inside the background bar div, instead of the other way around, fixes the length of the bars when a user may expand the column to the right to reveal the whole column name
    # Doing it the other way around would result in the front bar being stretched more than 100% of the value as the user expands the column, resulting in a horizontally stacked bar that makes no sense
    bar <- div(
      class = "bar-chart",
      style = list(marginRight = "6px"),
      div(
        class = "bar_back", 
        style = list(width = 100, backgroundColor = '#CCC', opacity = 0.5, borderRadius = '8px'),
        div(
          class = "bar_front", 
          style = list(width = width, backgroundColor = '#00ad93', borderRadius = '8px')
        )
      )
    )
    
    div(
      class = "bar-cell", 
      span(class = "number", value), 
      bar
    )
  } else {
    value
  }
}

# Function for (i) naming all columns; and (ii) hiding the non-sticky columns under the sticky ones when scrolling to the right
# The idea is to align each sticky column to the left by a multiple of 100 while setting the z-index to 1 to keep each sticky column above all non-sticky ones when scrolling to the right
# For instance, the first sticky columns is aligned to the left at position (1 - 1) * 100 = 0, the second at (2 - 1) * 100 etc.
# If a column isn't sticky, nothing happens.
reactive_table_format_columns <- function(column_names_in_data_frame, 
  column_names_in_reactive_table, sticky_columns, columns_with_round_bars, ...) {
  re <- column_names_in_reactive_table %>%
    lapply(
      function(x) {
        sticky_columns_style <- 
          if (!is.null(sticky_columns) & x %in% sticky_columns) {
            col_position <- which(sticky_columns %in% x)
            list(position = "sticky", left = (col_position - 1) * 100, 
              background = "#fff", borderRight = "0px", zIndex = 1)
          } else {
            NULL
          }
        
        colDef(
          name = x,
          style = sticky_columns_style,
          headerStyle = sticky_columns_style,
          cell = 
            if (x %in% columns_with_round_bars) {
              reactive_table_bars_standard
            } else {
              reactive_table_bars_rounded
            },
          ...
        )
      }
    )
  names(re) <- column_names_in_data_frame
  return(re)
}
