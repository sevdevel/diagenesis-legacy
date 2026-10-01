`screenprint.simulation.summary` <-
function (sim, F.fac) 
{
    if (sim$SL[[1]]$geometry == "1D") {
        rate.names <- c("flux.up", "flux.down", "int.reac", "int.irr", 
            "int.ddt", "deficit")
        N.rate <- length(rate.names)
        N.spec <- length(sim$SL)
        N.reac <- length(sim$RL)
        N.elt <- length(sim$EL)
        spec.summary <- data.frame(matrix(nrow = length(sim$SL), 
            ncol = N.rate), row.names = names(sim$SL))
        names(spec.summary) <- rate.names
        reac.summary <- data.frame(matrix(nrow = N.reac, ncol = 1), 
            row.names = names(sim$RL))
        names(reac.summary) <- c("R.int")
        elt.summary <- data.frame(matrix(nrow = length(sim$EL), 
            ncol = N.rate), row.names = names(sim$EL))
        names(elt.summary) <- rate.names
        for (i in 1:N.spec) {
            spec.summary[i, "flux.up"] <- F.fac * sim$SL[[i]]$flux.up
            spec.summary[i, "flux.down"] <- F.fac * sim$SL[[i]]$flux.down
            spec.summary[i, "int.reac"] <- F.fac * sim$SL[[i]]$int.reac
            spec.summary[i, "int.irr"] <- F.fac * sim$SL[[i]]$int.irr
            spec.summary[i, "int.ddt"] <- F.fac * sim$SL[[i]]$int.ddt
            spec.summary[i, "deficit"] <- F.fac * sim$SL[[i]]$deficit
        }
        print("", quote = FALSE)
        print("SPECIES SUMMARY", quote = FALSE)
        print(spec.summary, digits = 2)
        for (i in 1:N.reac) reac.summary[i, ] <- F.fac * sim$RL[[i]]$R.int
        print("", quote = FALSE)
        print("REACTION SUMMARY", quote = FALSE)
        print(reac.summary, digits = 2)
        for (i in 1:N.elt) {
            elt.summary[i, "flux.up"] <- F.fac * sim$EL[[i]]$flux.up
            elt.summary[i, "flux.down"] <- F.fac * sim$EL[[i]]$flux.down
            elt.summary[i, "int.reac"] <- F.fac * sim$EL[[i]]$int.reac
            elt.summary[i, "int.irr"] <- F.fac * sim$EL[[i]]$int.irr
            elt.summary[i, "int.ddt"] <- F.fac * sim$EL[[i]]$int.ddt
            elt.summary[i, "deficit"] <- F.fac * sim$EL[[i]]$deficit
        }
        print("", quote = FALSE)
        print("ELEMENT SUMMARY", quote = FALSE)
        print(elt.summary, digits = 2)
    }
    if (sim$SL[[1]]$geometry == "2D") {
        rate.names <- c("f.up", "f.down", "f.left", "f.right", 
            "int.reac", "int.irr", "int.ddt", "deficit")
        N.rate <- length(rate.names)
        N.spec <- length(sim$SL)
        N.reac <- length(sim$RL)
        N.elt <- length(sim$EL)
        spec.summary <- data.frame(matrix(nrow = N.spec, ncol = N.rate), 
            row.names = names(sim$SL))
        names(spec.summary) <- rate.names
        reac.summary <- data.frame(matrix(nrow = N.reac, ncol = 1), 
            row.names = names(sim$RL))
        names(reac.summary) <- c("R.int")
        elt.summary <- data.frame(matrix(nrow = length(sim$EL), 
            ncol = N.rate), row.names = names(sim$EL))
        names(elt.summary) <- rate.names
        for (i in 1:N.spec) {
            spec.summary[i, "f.up"] <- F.fac * sim$SL[[i]]$f.up
            spec.summary[i, "f.down"] <- F.fac * sim$SL[[i]]$f.down
            spec.summary[i, "f.left"] <- F.fac * sim$SL[[i]]$f.left
            spec.summary[i, "f.right"] <- F.fac * sim$SL[[i]]$f.right
            spec.summary[i, "int.reac"] <- F.fac * sim$SL[[i]]$int.reac
            spec.summary[i, "int.irr"] <- F.fac * sim$SL[[i]]$int.irr
            spec.summary[i, "int.ddt"] <- F.fac * sim$SL[[i]]$int.ddt
            spec.summary[i, "deficit"] <- F.fac * sim$SL[[i]]$deficit
        }
        print("", quote = FALSE)
        print("SPECIES SUMMARY", quote = FALSE)
        print(spec.summary, digits = 2)
        for (i in 1:N.reac) reac.summary[i, ] <- F.fac * sim$RL[[i]]$R.int
        print("", quote = FALSE)
        print("REACTION SUMMARY", quote = FALSE)
        print(reac.summary, digits = 2)
        for (i in 1:N.elt) {
            elt.summary[i, "f.up"] <- F.fac * sim$EL[[i]]$f.up
            elt.summary[i, "f.down"] <- F.fac * sim$EL[[i]]$f.down
            elt.summary[i, "f.left"] <- F.fac * sim$EL[[i]]$f.left
            elt.summary[i, "f.right"] <- F.fac * sim$EL[[i]]$f.right
            elt.summary[i, "int.reac"] <- F.fac * sim$EL[[i]]$int.reac
            elt.summary[i, "int.irr"] <- F.fac * sim$EL[[i]]$int.irr
            elt.summary[i, "int.ddt"] <- F.fac * sim$EL[[i]]$int.ddt
            elt.summary[i, "deficit"] <- F.fac * sim$EL[[i]]$deficit
        }
        print("", quote = FALSE)
        print("ELEMENT SUMMARY", quote = FALSE)
        print(elt.summary, digits = 2)
    }
}
