`setup.plot.info` <-
function (SL, RL, depth.lim = NULL, depth.unit = "", spec.unit = "", 
    reac.unit = "") 
{
    plot.info <- list(SL = NA, RL = NA)
    plot.info$SL <- vector(mode = "list", length = length(SL))
    names(plot.info$SL) <- names(SL)
    plot.info$SL <- initialise.plot.info(plot.info$SL, d.unit = depth.unit, 
        v.unit = spec.unit)
    plot.info$RL <- vector(mode = "list", length = length(RL))
    names(plot.info$RL) <- names(RL)
    plot.info$RL <- initialise.plot.info(plot.info$RL, d.unit = depth.unit, 
        v.unit = reac.unit)
    return(plot.info)
}
