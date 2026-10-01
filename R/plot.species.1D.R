`plot.species.1D` <-
function (sim.list, plot.info) 
{
    grid <- sim.list[[1]]$PL$grid
    n.spec <- length(sim.list[[1]]$SL)
    n.sim <- length(sim.list)
    spec.info <- plot.info$SL
    spec.names <- names(spec.info)
    for (i in 1:n.spec) {
        if (is.null(spec.info[[spec.names[i]]]$depth.lim)) 
            spec.info[[spec.names[i]]]$depth.lim <- spec.info[[spec.names[i]]]$depth.fac * 
                c(grid$x.up, grid$x.down)
        if (is.null(spec.info[[spec.names[i]]]$value.lim)) {
            max.value <- 0
            for (j in 1:n.sim) max.value <- max(max.value, sim.list[[j]]$SL[[spec.names[i]]]$C)
            spec.info[[spec.names[i]]]$value.lim <- c(0, 1.25 * 
                max(spec.info[[spec.names[i]]]$data.value, spec.info[[spec.names[i]]]$value.fac * 
                  max.value, na.rm = TRUE))
        }
    }
    for (i in 1:n.spec) {
        if (spec.info[[spec.names[i]]]$visible) {
            depth <- spec.info[[spec.names[i]]]$depth.fac * grid$x.mid
            value <- spec.info[[spec.names[i]]]$value.fac * sim.list[[1]]$SL[[spec.names[i]]]$C
            d.lim <- spec.info[[spec.names[i]]]$depth.lim
            v.lim <- spec.info[[spec.names[i]]]$value.lim
            d.label <- spec.info[[spec.names[i]]]$depth.label
            v.label <- spec.info[[spec.names[i]]]$value.label
            plot.diagen.1D(depth, value = value, d.lim = d.lim, 
                v.lim = v.lim, d.label = d.label, v.label = v.label, 
                col = sim.list[[1]]$sim.color)
            if (n.sim > 1) 
                for (j in 2:n.sim) {
                  depth <- spec.info[[spec.names[i]]]$depth.fac * 
                    grid$x.mid
                  value <- spec.info[[spec.names[i]]]$value.fac * 
                    sim.list[[j]]$SL[[spec.names[i]]]$C
                  lines.diagen.1D(depth, value = value, col = sim.list[[j]]$sim.color)
                }
            if (spec.info[[spec.names[i]]]$data.plot) 
                points.diagen.1D(spec.info[[spec.names[i]]]$data.depth, 
                  value = spec.info[[spec.names[i]]]$data.value, 
                  col = sim.list[[1]]$data.color)
        }
    }
}
