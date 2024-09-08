# Data Tables ------------------------------------------------------------------
# Completely Optional Rendering Method For Dashboad
# and Easy of Visualisation
#' Utility Javascript Functions
#' @name render_utils
#' @export
js_first_to_last = function() {
  reactable::JS('function(values, rows) {
              let s = values;
              let f = values[0];
              let l = values[s.length - 1];
              rn = f.concat(" to ",l);
              return rn
            }
   ')
}
#' @rdname render_utils
#' @export
js_first_value=function() {
  reactable::JS('function(values, rows) {
          let s = values;
          let f = values[0];
          return f
     }')
}
#' @rdname render_utils
#' @export
js_last_value=function() {
  reactable::JS('function(values, rows) {
        let s = values;
        let l = values[s.length - 1];
        return l
      }
     ')
}
#' Generic Method for Rendering Data Table
#' @export
render = function(x, ...) UseMethod('render',x)

#' Ploting an auditLog object
#' @param error_color Color code for when something is not consistent
#' @param id.name,from.name,to.name,logtime.name Column Name to Display for Object
#' @param column_defs Additional Column Definition to pass to `reactable::colDef`
#' @param ... any other column definition pass to `reactable`
#' @export
render.auditLog = function(
    x,
    error_color='red',
    id.name='Object ID',
    from.name = 'Old State',
    to.name='New State',
    logtime.name='Logged',
    column_defs = list(),
    ...
    ) {
  if(
    !all(c(
      '.from_is_valid','.to_is_valid','.consistent'
    ) %in% names(x))
  ) {
    x = consistency(x)
  }
  if(x |> inherits('tbl_lazy')) warning("object is tbl_lazy! something maynot work")
  # trim down a few column
  d = x |>
    select(everything(), -starts_with('.'), .id, .from,.from_is_valid, .to,.to_is_valid, .logtime, .consistent)
  d_simp = select(d, -.from_is_valid, -.to_is_valid,-.consistent)

  # Default Error Flagging Out
  DefaultColList = list(
    .id=reactable::colDef(
      name=id.name,
      style=function(value,index) {
        if(d[index, '.consistent']==FALSE) return(sprintf('color: %s;',error_color))
      }
    ),
    .from=reactable::colDef(
      name = from.name,
      aggregate = js_first_value(),
      style=function(value, index) {
        if(d[index, '.from_is_valid']==FALSE) return(sprintf('color: %s;',error_color))
      }
    ),
    .to =reactable::colDef(
      name = to.name,
      aggregate=js_last_value(),
      style=function(value, index){
        if(d[index, '.to_is_valid']==FALSE) return(sprintf('color: %s;',error_color))
      }
    ),
    .logtime=reactable::colDef(
      name= logtime.name,
      aggregate=js_first_to_last()
    )
  )
  # Merge Custom Settings With Default One
  FinalColList = if(length(column_defs)==0) DefaultColList else purrr::list_merge(DefaultColList, column_defs)
  reactable::reactable(d_simp,groupBy='.id',columns=FinalColList,...)
}
