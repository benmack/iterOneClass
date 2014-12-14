#'@export
get_full_pred_vector <- function(ioccObj){
    pred_iocc_all = (pred*0 + min(ioccObj$pred)) - 
    abs(min(ioccObj$pred))*.05
    pred_iocc_all = jitter(pred_iocc_all)
    pred_iocc_all[ioccObj$pred_neg==0] = ioccObj$pred
    return(pred_iocc_all)
}
