library('MVA')
demo('Ch-MDS')

# 4.4.2
(D <- dist(X))
cmdscale(D, k = 9, eig = TRUE)
max(abs(dist(X) - dist(cmdscale(D, k = 5))))
max(abs(prcomp(X)$x) - abs(cmdscale(D, k = 5)))   # PCA与MDS的关系


X_m <- cmdscale(dist(X, method = "manhattan"), k = nrow(X) - 1, eig = TRUE)
(X_eigen <- X_m$eig)
cumsum(abs(X_eigen)) / sum(abs(X_eigen))
cumsum(X_eigen^2) / sum(X_eigen^2)

# 城市距离
(airline_mds <- cmdscale(airdist, k = 9, eig = TRUE))
airline_mds$points

(lam <- airline_mds$eig)
cumsum(abs(lam)) / sum(abs(lam))
cumsum(lam^2) / sum(lam^2)

# 古埃及人头颅
skulls_var <- tapply(1:nrow(skulls), skulls$epoch,
                     function(i) var(skulls[i,-1]))
S <- 0
for (v in skulls_var) S <- S + 29 * v
(S <- S / 149)



