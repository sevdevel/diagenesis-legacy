`screenprint.simulation.progress` <-
function (fc, t, SV, ACC, RCC, var.names) 
{
    negative.SV <- rep(FALSE, ncol(SV))
    for (i in 1:ncol(SV)) if (min(SV[, i]) < 0) 
        negative.SV[i] <- TRUE
    rms.ACC <- sqrt(sum(ACC^2)/length(ACC))
    max.index <- which(RCC == max(RCC))[1]
    print(paste(fc, "Time =", string.time(t), "rms ACC =", signif(rms.ACC, 
        3), "max RCC =", signif(max(RCC), 3), "Spec =", var.names[max.index]))
    if (sum(negative.SV) > 0) 
        print(paste("Warning negative concentrations:", paste(var.names[negative.SV], 
            collapse = ", ")))
}
