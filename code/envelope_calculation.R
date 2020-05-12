# table

# to all destinations (no alpha uncertainty)
(pre_lock_w <- 196907 * c(1.50,0.963,1.53)*10^-3*1.49 )
(post_lock_w <- 39889 * c(4.96,3.06,9.46)*10^-3*1.49 )
(exp_w <- pre_lock_w + post_lock_w)
(pre_lock_c <- 7137041 * c(0.211,0.136,0.216)*10^-3*1.49 )
(post_lock_c <- 2870374 * c(0.903,0.573,1.23)*10^-3*1.49 )
(exp_c <- pre_lock_c + post_lock_c)
(all_exp <- exp_w[order(exp_w)] + exp_c[order(exp_c)]  )
# to africa
(pre_lock_w <- 898 * c(1.50,0.963,1.53)*10^-3*1.49 )
(post_lock_w <- 365 * c(4.96,3.06,9.46)*10^-3*1.49 )
(exp_w <- pre_lock_w + post_lock_w)
(pre_lock_c <- 132021 * c(0.211,0.136,0.216)*10^-3*1.49 )
(post_lock_c <- 61454 * c(0.903,0.573,1.23)*10^-3*1.49 )
(exp_c <- pre_lock_c + post_lock_c)

# to all destinations (with alpha uncertainty)
(pre_lock_w <- 196907 * c(1.50,0.963,1.53)*10^-3*c(1.49,1.23,1.80) )
(post_lock_w <- 39889 * c(4.96,3.06,9.46)*10^-3*c(1.49,1.23,1.80) )
(exp_w <- pre_lock_w + post_lock_w)
(pre_lock_c <- 7137041 * c(0.211,0.136,0.216)*10^-3*c(1.49,1.23,1.80) )
(post_lock_c <- 2870374 * c(0.903,0.573,1.23)*10^-3*c(1.49,1.23,1.80) )
(exp_c <- pre_lock_c + post_lock_c)
(all_exp <- exp_w[order(exp_w)] + exp_c[order(exp_c)]  )
# to africa
(pre_lock_w <- 898 * c(1.50,0.963,1.53)*10^-3*c(1.49,1.23,1.80) )
(post_lock_w <- 365 * c(4.96,3.06,9.46)*10^-3*c(1.49,1.23,1.80) )
(exp_w <- pre_lock_w + post_lock_w)
(pre_lock_c <- 132021 * c(0.211,0.136,0.216)*10^-3*c(1.49,1.23,1.80) )
(post_lock_c <- 61454 * c(0.903,0.573,1.23)*10^-3*c(1.49,1.23,1.80) )
(exp_c <- pre_lock_c + post_lock_c)
(all_exp <- exp_w[order(exp_w)] + exp_c[order(exp_c)]  )

# alpha uncertainty: 1.46
# prevalence uncertainty: 2.1
p_high <- c( 1.53, 9.46,0.216,1.23)
p_low <- c( 0.963, 3.06,0.136,0.573)
(p_high/p_low )%>% mean()
# expected uncertainty in final expected counts: 3.1
# uncertainty in final expected counts: 2.9 and 2.88

