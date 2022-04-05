#' Draw parallel axes with a linear transform
#'
#' Draws two parallel axes. The lower axis shows the input to
#' the function without a scaled input, e.g. the input to the pattern book
#' function. The upper axis shows the input that goes into the scaling
#' before being handed off to the pattern-book function.
#'
#' `plot_scaled_input()` is for drawing pattern-book functions on
#' their natural unscaled input (plain "x"), but then adds a second scale
#' that shows the input to the scaled function.
#'
#' @details Only handles linear transformations: r*(x-x0)
#'
#'
#' @param min lower end of domain
#' @param max upper end of domain
#' @param r r parameter in linear scaling `r*(x-x0)`
#' @param x0 x0 parameter in linear scaling `r*(x-x0)`
#' @param nticks number of ticks on new axis (add_scale)
#' @param color color of new axis (e.g. "blue")
#' @param ftilde tilde expression as in `slice_plot()`
#' @param domain domain specifier as in `slice_plot()`
#'
#' @rdname parallel_axes
#'
#' @returns a ggplot2 object
#'
#' @examples
#' scale_shift(-40, 100, 9/5, 32, 10 )
#' plot_scaled_input(exp(x) ~ x, domain(x=c(-2,2)), 2.4, 1)
#'
#' @export
scale_shift <- function(min, max, r, x0=0, nticks=10,color="blue", ratio=0.1) {
  tick_height = (max-min)/5
  nudge = (max-min)/15
  Orig <- tibble::tibble(
    horiz = pretty(min:max,
                   n=nticks,
                   min.n=ceiling(.7*nticks)),
    vert = min,
    end = vert + tick_height/2,
  )

  New <- tibble::tibble(
    yvals = pretty(c(r*(min-x0), r*(max-x0)),
                   n=nticks,
                   min.n=ceiling(.7*nticks)),
    horiz = yvals/r + x0,
    vert = max,
    end = vert - tick_height/2,

  )

  gf_line(vert ~ horiz, data = Orig) %>%
    gf_text(vert ~ horiz, label = ~ as.character(horiz), vjust=1, nudge_y = -nudge) %>%
    gf_errorbar(end + vert ~ horiz, width=0) %>%
    gf_line(vert ~ horiz, data = New, color=color) %>%
    gf_text(vert ~ horiz, label = ~ as.character(yvals),
            data = New, vjust=0, nudge_y = nudge,
            color=color) %>%
    gf_errorbar(end + vert ~ horiz, width=0, data=New,
                color=color) %>%
    gf_theme(theme_void()) %>%
    gf_refine(coord_fixed(ratio = ratio)) %>%
    gf_lims(
      y= extendrange(
        r=c(min-tick_height, max+tick_height),
        1))
}

#' @rdname parallel_axes
#' @export
plot_scaled_input <- function(ftilde, domain, r, x0, nticks=10, color="blue") {
  F <- makeFun(ftilde)
  Pts <- tibble::tibble(
    xpts = seq(domain[[1]][1], domain[[1]][2], length=500),
    ypts = F(xpts)
  )
  new_range <- range(Pts$ypts)
  yplace <- new_range[1] + 0.02*diff(new_range)
  tick_height <- 0.05*diff(new_range)
  nudge <- 1*tick_height
  New <- tibble::tibble(
    yvals = pretty(r*(range(Pts$xpts)-x0),
                   n=nticks,
                   min.n=ceiling(.7*nticks)),
    horiz = yvals/r + x0,
    vert = yplace,
    end = vert + tick_height/2,
  )

  gf_line(vert ~ horiz, data = New, color=color) %>%
    gf_text(vert ~ horiz, label = ~ as.character(yvals),
            data = New, vjust=0, nudge_y = nudge,
            color=color) %>%
    gf_errorbar(end + vert ~ horiz, width=0, data=New,
                color=color) %>%
    slice_plot(ftilde, domain) %>%
    gf_labs(x = "Input after scaling")
}

