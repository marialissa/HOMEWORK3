 #item 3
        x <- c(0.99, 2.31, 10.85, 6.15, 10.81, 3.72, 5.75, 4.15, 9.27, 7.84, 2.31, 10.85, 6.15, 1.81, 3.72, 5.75, 10.40, 10.04, 4.15, 9.27)
        sum_x <- sum(x)
        n_x <- length(x)
        mle_x <- n_x/sum_x
        
        cat("O somatorio dos dados eh ", sum_x, "\n")
        cat("O numero de amostras eh ", n_x, "\n")
        cat("O estimador MLE eh ", mle_x, "\n")
    
    #item 4
    
        lambda <- seq(0, 1, length.out = 300)
        log_lik_x <- n_x*log(lambda) - lambda*sum_x
        plot(lambda, log_lik_x)
    
        log_lik_max <- n_x*log(mle_x) - mle_x*sum_x
    
        points(mle_x, log_lik_max, col = "blue", pch = 19, cex = 2)
    
    
    #item 5
        #(a)
            ev_x <- 1/mle_x
            cat("O tempo esperado de vida de um computador eh de ", ev_x, "\n")
    
        #(b)
            p_gt_5 <- exp(-0.1583657*5)
            cat("A probabilidade de um computador viver mais que 5 anos eh ", p_gt_5, "\n") 
