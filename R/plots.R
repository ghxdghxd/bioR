#' qq plot
#' @export

ggQQplot <- function(pvalues, title = NULL, random = NULL){
    pvalues = sort(na.omit(as.numeric(pvalues)))
    pvalues = cbind(op = pvalues, ep = ppoints(length(pvalues)))
    if(!is.null(random)){
        pvalues = pvalues[sample(1:nrow(pvalues), random), ]
    }
    lab <- quantile(pvalues[,"op"], 0.475)/quantile(pvalues[,"ep"], 0.475)
    pvalues <- -log10(pvalues)
    maxP = ceiling(max(pvalues))
    if(is.null(title)){
        title = ""
    }
    p = ggplot(as.data.frame(pvalues)) + 
        geom_point(aes(ep, op), size = 1) + 
        scale_x_continuous(expand = c(0, 0), limits = c(0, maxP)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0, maxP)) +
        geom_abline(intercept = 0, slope = 1, alpha = 1) +
        labs(x = "Expected (-log10(P))", y = "Observed (-log10(P))", title = title) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5, size = 15),
            axis.text = element_text(hjust = 0, size = 10),
             plot.margin=unit(c(1,2,1,1),"lines")) +
        annotate("text", x = 0.7 * maxP, y = 0.2 * maxP, size = 5,
            label = paste0("lambda = ", signif(lab, 3)))
    return(p)
}
