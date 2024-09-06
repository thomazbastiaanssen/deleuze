# # # imputeproof <- function(x){
# # #   x.dsq <- dist(x, method = "eucludean")^2
# # # }
# # # 
# # # 
# # 
# # adjusted_aitchison <- function(x, x.clr = NULL, x.dist = NULL){
# #   #clr transform
# #   if(is.null(x.clr)){
# #     x.clr = deleuze:::clr(x)
# #     }
# #   #compute aitchison distance
# #   if(is.null(x.dist)){
# #     x.dist = dist(t(x.clr), method = "euclidean")
# #   }
# #   # if(attr(x.dist, "method") == "euclidean"){
# #   #   x.dist = x.dist^2
# #   # }
# #   # 
# #   #determine imputed values
# #   iv <- imputed_values(x.clr)
# #     
# #   #count shared zeroes
# #   sz <- shared_zeroes(x = x.clr)
# #   
# #   #apply(X = combn(iv, 2), MARGIN = 2, FUN = prod)
# #   #calculate overshoot
# #   x.dist - sqrt(
# #     dist(rbind(
# #     (
# #       (sz * (combn(iv, 2)[1,])) 
# #      ),
# #       
# #       (
# #         (sz * (combn(iv, 2)[2,])) 
# #       )
# #     ) ) / (nrow(x)-sz)
# # 
# #   )
# #   x.dist - sqrt(
# #     (
# #       (sz * (
# #         (combn(iv, 2)[1,]) - (combn(iv, 2)[2,])))^2 / (nrow(x)-sz))^2
# #   )
# #     
# #       
# # 
# #   
# #   # 
# #   # sqrt(
# #   #   
# #   #   length(cs[-i]) * (sum(cs[i]) / length(cs[-i]) )^2
# #   #   
# #   # )
# #    return(x.dist)
# # }
# # 
# # 
# # 
# # shared_zeroes <- function(x){
# #   x.cp <- crossprod(apply(x, 2, function(x) x == min(x)))
# #   x.cp[upper.tri(x.cp)]
# # }
# # 
# # imputed_values <- function(x){
# #   apply(x, 2, min)
# # }
# # 
# # pw_overdist <- function(x){
# #   c(dist(x))^2
# # }
# # 
# # library(tidyverse)
# # 
# # x <- deleuze::getTableMeans(volatility::vola_genus_table[1:10,c(1, 2)], CLR_transformed = FALSE)
# # y <- deleuze:::clr(x) %>% 
# #   as.data.frame %>% 
# #   filter(!(Validation_Pre_Control_1 == min(Validation_Pre_Control_1) & 
# #              Validation_Pre_Control_2 == min(Validation_Pre_Control_2))) %>% 
# #   as.matrix()
# # 
# # #all.equal(rownames(x), row.names(y))
# # z <- x %>% 
# #   as.data.frame %>% 
# #   filter(!(Validation_Pre_Control_1   == min(Validation_Pre_Control_1) & 
# #              Validation_Pre_Control_2 == min(Validation_Pre_Control_2))) %>% 
# #   deleuze:::clr()
# # 
# # adjusted_aitchison(x = x) 
# # 
# # dist(t(apply(y, MARGIN = 2, FUN = function(x) x - mean(x)) ))
# # dist(t(y))
# # dist(t(z))
# # 
# # 
# # 
# # #pairwise_squared_diff
# # # 
# # # x.exp <- dclr(x)
# # # x.dist <- dist(t(x.exp), method = "euclidean")
# # # a = shared_zeroes(x)
# # # b = imputed_values(x.exp)
# # # cc = pw_overdist(b)
# # # 
# # # cc * a
# # 
# # #define still, see volatility 
# # #dist2vect 
# # 
# # #and then, with diust method <- euclidean squared dist: 
# # #   
# # # dist(x.exp) - vect2dist(dist2vect(b) * dist2vect(a))
# # # 
# # # 
# # # dist(4:10)
# # # 
# # # c(a)
# # # 
# # 
# # # counts <- data.frame(a = 1:10, b = (1:10)^2, c = (1:10)^3)
# # # counts <- rbind(counts, c(0.5, 0.5, 10))
# # # x <- as.matrix(counts)
# # # 
# # # microbenchmark::microbenchmark(aa.raw  = adjusted_aitchison(x = x),
# # #                                aa.clr  = adjusted_aitchison(x = x, x.clr = x.exp),
# # #                                aa.dist = adjusted_aitchison(x = x, x.dist = x.dist),
# # #                                aa.full = adjusted_aitchison(x = x, x.clr = x.exp, x.dist = x.dist),
# # #                                check = "equivalent",
# # #                                times = 1000)
# # # 
# # # raw     = dist(t(deleuze:::clr(x)))
# # # adjusted_aitchison(x = x)
# # # 
# # 
# # z = deleuze::getTableMeans(volatility::vola_genus_table[,1:2] %>% 
# #                              as.data.frame %>% 
# #                              filter(Validation_Pre_Control_1 != min(Validation_Pre_Control_1) & 
# #                                       Validation_Pre_Control_2 != min(Validation_Pre_Control_2)), 
# #                            CLR_transformed = FALSE)
# # 
# # 
# # dist(t(deleuze:::clr(z)))
# # 
# # 
# # dist(t(deleuze:::clr(x)[-11,]))  - ((colSums(deleuze:::clr(x)[11,,drop=FALSE])^2)/
# #                                       nrow(deleuze:::clr(x)[-11,, drop = FALSE]))
# # 
# # 
# # 
# # sum(cs[i])^2 / length(cs[-i])
# # 
# # 
# # sweep(deleuze:::clr(x)[-11,], MARGIN = 2, STATS = colMeans(deleuze:::clr(x)[-11,]), FUN = "-") 
# # deleuze:::clr(x[-11,])
# # 
# # 
# # library(deleuze)
# # library(tidyverse)
# # 
# i = c(1, 3, 5)
# counts <- data.frame(a = 1:10, b = (1:10)^2, c = (1:10)^3)
# counts <- rbind(counts, c(0.5, 0.5, 10))
# x <- as.matrix(counts)
# countsi <- counts[-i,]
# x.exp  <- deleuze:::clr(counts, cols_as_features = FALSE)
# x.expi <- deleuze:::clr(countsi, cols_as_features = FALSE)
# x.dist <- dist(t(x.exp), method = "euclidean")
# 
# x.disti <- dist(t(x.expi), method = "euclidean")
# 
# adjusted_aitchison(x = x)
# dist(t(deleuze:::clr(x[-11,])))
# # 
# # 
# # x.exp2.i <- x.exp[-i,] + (x.exp[i,])/nrow(x.exp[-i,])
# # x.dist2.i <- dist(t(x.exp2.i), method = "euclidean")
# # 
# # x.dist2.i[1] <- sqrt((x.dist2.i[1]^2) - sum((x.exp[i,1] - x.exp[i,2])^2))
# # x.dist2.i
# # x.disti
# # 
# # x.exp2.i
# # x.exp
# # a = x.exp1[-i,1] - x.exp1[-i,2]
# # b = x.exp2.i[,1] - x.exp2.i[,2]
# # 
# # 
# # 
# # 
# # clr <- function(x) log(x) - mean(log(x))
# # 
# # s <- sample(1:100, size = 100, replace = TRUE)
# # i =  sample(1:100, 10, replace = FALSE)
# # 
# # s <- c(1, 1, 1, 2, 6, 10, 100)
# # i <- c(1, 3, 5) 
# # 
# # 
# # cs  <- clr(s)
# # csi <- clr(s[-i])
# # 
# # csii <- cs[-i] +  sum(cs[i])/length(cs[-i])
# # 
# # (csi - csii)^2
# # 
# # (
# #   csi - (cs[-i] + sum(cs[i])/length(cs[-i]))
# #   )^2
# # 
# # sum((csi - cs[-i])^2)
# # 
# # cs[-i]
# # sum((csi - cs[-i])^2)
# # sum(
# #   rep( 
# #     cs[i] / length(cs[-i]), length(cs[-i]) 
# #     )^2
# # )
# # 
# # dist.i      <- dist(rbind(csi, cs[-i]))
# # overshoot.i <- sqrt(
# # 
# #     length(cs[-i]) * 
# #       (sum(cs[i])^2) /  
# #       
# #       (length(cs[-i]) ^2)
# #     
# #   )
# # 
# # sqrt(
# #   
# #   length(cs[-i]) * 
# #     
# #     
# #     sum(cs[i])^2 / length(cs[-i])^2
# #   
# # )
# # 
# # sqrt(
# #   sum(cs[i])^2 / length(cs[-i])
# # )
# # 
# # 
# # data.frame(a = -0.9, 
# #            b = -0.3, 
# #            x = rnorm(1000, mean = 0, sd = 3), 
# #            y = rnorm(1000, mean = 0, sd = 3), 
# #            batch = rep(letters[1:10], each = 10)) %>% 
# #   mutate(x = x - mean(x), 
# #          y = y - mean(y)) %>% 
# #   group_by(batch) %>% 
# #   mutate(x_a = x + a, 
# #          y_b = y + b) %>% 
# #   mutate(d_xy0 =  sqrt((x - y)^2),
# #          d_xy1 =  sqrt(((x + a)- (y + b))^2) 
# #          ) %>% 
# #   ungroup() %>% 
# #   mutate(er = d_xy0 - d_xy1) %>% 
# #   pivot_longer(c(d_xy1, er, x, y)) %>% 
# #   
# #   # mutate(xx = 1:1000,
# #   #        yy = 1:1000
# #   # ) %>% 
# #   # 
# #   ggplot()+
# #   aes(x = d_xy0, y = value) +
# #   facet_grid(name~batch, scales = "free") + 
# #   
# #   #aes(x = xx, y = yy) +
# #   geom_point()
# # #   
# 
# microbenchmark::microbenchmark(a = dist(d), 
#                                b = ragged_dist(d))
