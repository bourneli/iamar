library('MVA')
demo("Ch-MVA") # generate data


 head(measure)
 head(pottery)
 head(exam)
 head(USairpollution)
 
 cov(measure[,c('chest','waist','hips')])
 cov(subset(measure, gender == 'female')[,c('chest','waist','hips')])
 cov(subset(measure, gender == 'male')[,c('chest','waist','hips')])
 
 cor(measure[,c('chest','waist','hips')])
 cor(subset(measure, gender == 'female')[,c('chest','waist','hips')])
 cor(subset(measure, gender == 'male')[,c('chest','waist','hips')])
 
 
 dist(scale(measure[, c("chest", "waist", "hips")], center = FALSE))
 
 
 
 