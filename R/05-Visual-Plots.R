#' Visualisations
#' @name plot_generic
#' @param df any data.frame
#' @param x,y x and y
#' @param fill fill color pass to ggplot
#' @param error one dimensional bar see details
#' @param facet add facet to dimensions
#' @param error_range a vector of 2 to plot lines, useful for showing when the first
#' and last error is detected
#' @details
#' If Error is Negative, then the plot treat is as minimum bound of error bar
#' If Error is Positive then this plot treat it as that of a maximum bound.
#'
#' Because so the term 'Error' may be misleading. This will be corrected at the
#' next version.
#' @export
plot_time =function(df,
                    x, y,
                    color,
                    facet,
                    type ='col',
                    error=NULL,
                    error_range = NA,
                    man_pal = c()
) {
  # Reserve these to to check NULL so facet and error is optional
  facet_quo = enquo(facet)
  error_quo = enquo(error)

  # Facet Options
  if (rlang::quo_is_null(facet_quo)) {
    facet_var <- vars()
  } else {
    facet_var <- vars(!!facet_quo)
  }

  # Require to compute
  if (rlang::quo_is_null(error_quo)) {
    add_errorbar_ = function() NULL
  } else {
    df = df |>
      mutate(
        .ymax=ifelse(!!error_quo < 0, {{y}}, {{y}} + !!error_quo),
        .ymin=ifelse(!!error_quo > 0, {{y}}, {{y}} + !!error_quo))
    add_errorbar_ = function() ggplot2::geom_errorbar(aes(ymin=.ymin, ymax=.ymax))
  }


  df |>
    ggplot2::ggplot(aes(x={{x}},y={{y}})) +
    {if(type=='col') {
      ggplot2::geom_col(aes(fill={{color}}))
    } else if(type=='line') {
      ggplot2::geom_line(aes(color={{color}}))
    }} +
    add_errorbar_() +
    ggplot2::geom_abline(aes(slope=0, intercept=0)) +
    {if(!is.na(error_range[1])) geom_vline(
      aes(xintercept = error_range[1]), color='red', linetype=2)
    } +
    {if(!is.na(error_range[2])) geom_vline(
      aes(xintercept = error_range[2]), color='red', linetype=1)
    } +
    ggplot2::facet_wrap(facet_var) +
    ggplot2::theme_bw() +
    {
      if(length(man_pal)==0) {
        if(type=='col'){
          ggplot2::scale_fill_brewer(palette ='Set2')
        } else if(type=='line'){
          ggplot2::scale_color_brewer(palette='Set2')
        }
      } else {
        if(type=='col') {
          ggplot2::scale_fill_manual(values=man_pal)
        } else if(type=='line'){
          ggplot2::scale_color_manual(values=man_pal)
        }
      }
    }
}


#' @rdname plot_generic
#' @export
plot.auditlogHistory=function(
    x,type='col',
    facet=quo(.state),
    color=quo(.state),
    ...) {
  x |>
    mutate(.state=as.factor(.state)) |>
    plot_time(
      x=.period,
      y=.history,
      type = type,
      error=.error,
      color=!!color,
      facet=!!facet,
      ...
    )
}
#' @rdname plot_generic
#' @export
plot.auditlogIncrement=function(
    x,type='col',
    facet=quo(.state),
    color=quo(.state),
    ...
    ) {
  x |>
    mutate(.state=as.factor(.state)) |>
    plot_time(
      x=.period,
      y=.increment,
      type = type,
      error=.error,
      color=!!color,
      facet=!!facet,
      ...
    )
}
