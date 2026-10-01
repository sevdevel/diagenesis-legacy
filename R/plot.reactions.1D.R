`plot.reactions.1D` <-
function (sim.list, plot.info) 
{
    grid <- sim.list[[1]]$PL$grid
    n.reac <- length(sim.list[[1]]$RL)
    n.sim <- length(sim.list)
    reac.info <- plot.info$RL
    reac.names <- names(reac.info)
    for (i in 1:n.reac) {
        if (is.null(reac.info[[reac.names[i]]]$depth.lim)) 
            reac.info[[reac.names[i]]]$depth.lim <- reac.info[[reac.names[i]]]$depth.fac * 
                c(grid$x.up, grid$x.down)
        if (is.null(reac.info[[reac.names[i]]]$value.lim)) {
            min.value <- 0
            for (j in 1:n.sim) min.value <- min(min.value, sim.list[[j]]$RL[[reac.names[i]]]$R)
            v1 <- 1.25 * min(reac.info[[reac.names[i]]]$data.value, 
                reac.info[[reac.names[i]]]$value.fac * min.value, 
                na.rm = TRUE)
            max.value <- 0
            for (j in 1:n.sim) max.value <- max(max.value, sim.list[[j]]$RL[[reac.names[i]]]$R)
            v2 <- 1.25 * max(reac.info[[reac.names[i]]]$data.value, 
                reac.info[[reac.names[i]]]$value.fac * max.value, 
                na.rm = TRUE)
            reac.info[[reac.names[i]]]$value.lim <- c(v1, v2)
        }
    }
    for (i in 1:n.reac) {
        if (reac.info[[reac.names[i]]]$visible) {
            depth <- reac.info[[reac.names[i]]]$depth.fac * grid$x.mid
            value <- reac.info[[reac.names[i]]]$value.fac * sim.list[[1]]$RL[[reac.names[i]]]$R
            d.lim <- reac.info[[reac.names[i]]]$depth.lim
            v.lim <- reac.info[[reac.names[i]]]$value.lim
            d.label <- reac.info[[reac.names[i]]]$depth.label
            v.label <- reac.info[[reac.names[i]]]$value.label
            plot.diagen.1D(depth, value = value, d.lim = d.lim, 
                v.lim = v.lim, d.label = d.label, v.label = v.label, 
                col = sim.list[[1]]$sim.color)
            if (n.sim > 1) 
                for (j in 2:n.sim) {
                  depth <- reac.info[[reac.names[i]]]$depth.fac * 
                    grid$x.mid
                  value <- reac.info[[reac.names[i]]]$value.fac * 
                    sim.list[[j]]$RL[[reac.names[i]]]$R
                  lines.diagen.1D(depth, value = value, col = sim.list[[j]]$sim.color)
                }
            if (reac.info[[reac.names[i]]]$data.plot) 
                points.diagen.1D(reac.info[[reac.names[i]]]$data.depth, 
                  value = reac.info[[reac.names[i]]]$data.value, 
                  col = sim.list[[1]]$data.color)
        }
    }
}
