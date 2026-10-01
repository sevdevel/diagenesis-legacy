`initialise.simulation.progress` <-
function (var.names) 
{
    simulation.progress <- matrix(nrow = 0, ncol = (2 * length(var.names) + 
        1))
    colnames(simulation.progress) <- c("Time", paste("ACC", var.names), 
        paste("RCC", var.names))
    return(simulation.progress)
}
