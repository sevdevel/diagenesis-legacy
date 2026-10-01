`initialise.plot.info` <-
function (info, depth.lim = NULL, d.unit = "", v.unit = "") 
{
    for (i in 1:length(info)) {
        info[[i]]$visible <- TRUE
        info[[i]]$depth.lim <- depth.lim
        info[[i]]$value.lim <- NULL
        info[[i]]$depth.fac <- 1
        info[[i]]$value.fac <- 1
        info[[i]]$depth.label <- paste("depth", d.unit)
        info[[i]]$value.label <- paste(names(info)[i], v.unit)
        info[[i]]$data.plot <- FALSE
        info[[i]]$data.depth <- NULL
        info[[i]]$data.value <- NULL
    }
    return(info)
}
