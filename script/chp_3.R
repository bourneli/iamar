library('MVA')
demo('Ch-PCA')

# chp 3.4
blood_pcacov <- princomp(covmat = blood_cov)
summary(blood_pcacov, loadings = TRUE)
plot(blood_pcacov)

blood_pcacor <- princomp(covmat = blood_corr)
summary(blood_pcacor, loadings = TRUE)
plot(blood_pcacor)
