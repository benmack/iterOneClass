#'@export
get_full_pred_vector <- function(ioccObj){
    
    pred_iocc_all = (ioccObj$validCells*0 + min(ioccObj$pred)) - abs(min(ioccObj$pred))*.05
    pred_iocc_all = jitter(pred_iocc_all)
    pred_iocc_all[ioccObj$validCells %in% ioccObj$idx] <- ioccObj$pred
    return(pred_iocc_all)
}
