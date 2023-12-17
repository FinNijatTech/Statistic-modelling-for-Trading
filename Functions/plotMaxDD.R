# plots maximum drawdown

plotMaxDD <- function(x) {
  x_ <- coredata(x)
  plot(x_, type = "l")
  mdd_ <- maxdrawdown(x_)
  segments(time(x_)[mdd_$from], x_[mdd_$from],
           time(x_)[mdd_$to], x_[mdd_$from], col = "grey")
		segments(time(x_)[mdd_$from], x_[mdd_$to],
				 time(x_)[mdd_$to], x_[mdd_$to], col = "grey")
		mid <- time(x_)[(mdd_$from + mdd_$to)/2]
		arrows(mid, x_[mdd_$from], mid, x_[mdd_$to], col = "red")
	}
