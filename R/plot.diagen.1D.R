`plot.diagen.1D` <-
function (x, value, d.lim = range(x), v.lim = c(0, 1.2 * max(value)), 
    d.label = "depth", v.label = "value", lwd = 3, type = "l", 
    ...) 
{
    matplot(value, x, lwd = lwd, type = type, ylim = d.lim[2:1], 
        xlim = v.lim, xlab = "", ylab = d.label, main = v.label, 
        axes = FALSE, ...)
    axis(pos = par()$xaxp[1], side = 2, lwd = 1)
    axis(pos = par()$yaxp[2], side = 3, lwd = 1)
    if (par()$yaxp[2] != 0) 
        abline(h = 0)
}
