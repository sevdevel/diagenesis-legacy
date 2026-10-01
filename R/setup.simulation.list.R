`setup.simulation.list` <-
function (out, PL, model) 
{
    simulation.list <- vector(mode = "list")
    for (i in 1:nrow(out)) {
        N <- PL$grid$N
        SV <- matrix(nrow = N, ncol = PL$N.var, data = out[i, 
            2:(PL$N.var * N + 1)])
        dimnames(SV) <- list((1:N), PL$var.names)
        simulation.list[[i]] <- model(t = out[i, 1], state = SV, 
            parameters = PL, summary.call = TRUE)
        simulation.list[[i]]$time <- out[i, 1]
    }
    class(simulation.list) <- "simulation.list"
    return(simulation.list)
}
