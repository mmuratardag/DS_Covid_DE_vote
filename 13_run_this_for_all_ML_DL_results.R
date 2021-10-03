
# should take < 8 minutes with 7 cores
source("01_ML.R")
gdata::keep(ML_op_auc, rf_pred_roc_auc, sure = T)
source("02_DL.R")
gdata::keep(ML_op_auc, rf_pred_roc_auc, DL_op, sure = T)
source("05_ML_w_class.R")
gdata::keep(ML_op_auc, rf_pred_roc_auc, DL_op,
            ML_op_auc_class, rf_pred_roc_auc_class, sure = T)
source("06_DL_w_class.R")
gdata::keep(ML_op_auc, rf_pred_roc_auc, DL_op,
            ML_op_auc_class, rf_pred_roc_auc_class, DL_op_class, sure = T)
source("08_ML_w_profile.R")
gdata::keep(ML_op_auc, rf_pred_roc_auc, DL_op,
            ML_op_auc_class, rf_pred_roc_auc_class, DL_op_class,
            ML_op_auc_prof, rf_pred_roc_auc_prof, sure = T)
source("09_DL_w_profile.R")
gdata::keep(ML_op_auc, rf_pred_roc_auc, DL_op,
            ML_op_auc_class, rf_pred_roc_auc_class, DL_op_class,
            ML_op_auc_prof, rf_pred_roc_auc_prof, DL_op_profile, sure = T)

gridExtra::grid.arrange(ML_op_auc, rf_pred_roc_auc, DL_op,
                        ML_op_auc_class, rf_pred_roc_auc_class, DL_op_class,
                        ML_op_auc_prof, rf_pred_roc_auc_prof, DL_op_profile,
                        nrow = 3, ncol = 3,
                        top = "Model Evaluation Metrics")

# rm(list = ls(all.names = T))