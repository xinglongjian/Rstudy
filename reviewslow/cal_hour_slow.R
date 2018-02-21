#' Title
#'
#' @return
#' @export
#'
#' @examples
cal_review_slow <- function() {
  hours = list(0:23)
  #dataframe = data.frame(row.names = c('Time','lt1','gt1le3','gt3le5','gt5le10','big10'))
  rlist <- list()
  Time <- c()
  lt1_list <- c()
  gt1le3_list <- c()
  gt3le5_list <- c()
  gt5le10_list <- c()
  big10_list <- c()
  #--------------
  allNum = length(trace_result_sort$X1)
  all_lt1 = round(length(trace_result_sort[trace_result_sort$X2 <= 1000, ]$X2) /
                    allNum, 2)
  all_gt1le3 = round(length(trace_result_sort[trace_result_sort$X2 > 1000 &
                                                trace_result_sort$X2 <= 3000, ]$X2) / allNum, 2)
  all_gt3le5 = round(length(trace_result_sort[trace_result_sort$X2 > 3000 &
                                                trace_result_sort$X2 <= 5000, ]$X2) / allNum, 2)
  all_gt5le10 = round(length(trace_result_sort[trace_result_sort$X2 > 5000 &
                                                 trace_result_sort$X2 <= 10000, ]$X2) / allNum, 2)
  all_gt10 = round(length(trace_result_sort[trace_result_sort$X2 > 10000, ]$X2) /
                     allNum, 2)
  print(c(all_lt1, all_gt1le3, all_gt3le5, all_gt5le10, all_gt10))
  for (h in 0:23) {
    Time <- c(Time, h)
    hourResult0 = trace_result_sort[trace_result_sort$X1 == h, ]
    allNum0 = length(hourResult0$X1)
    # print(allNum0)
    # print(length(hourResult0[hourResult0$X2<=1000,]$X2))
    # print(length(hourResult0[hourResult0$X2>1000 & hourResult0$X2<=3000,]$X2))
    # print(length(hourResult0[hourResult0$X2>3000 & hourResult0$X2<=5000,]$X2))
    # print(length(hourResult0[hourResult0$X2>5000 & hourResult0$X2<=10000,]$X2))
    # print(length(hourResult0[hourResult0$X2>10000,]$X2))
    lt1 = round(length(hourResult0[hourResult0$X2 <= 1000, ]$X2) / allNum0, 2)
    lt1_list <- c(lt1_list, lt1)
    gt1le3 = round(length(hourResult0[hourResult0$X2 > 1000 &
                                        hourResult0$X2 <= 3000, ]$X2) / allNum0, 2)
    gt1le3_list <- c(gt1le3_list, gt1le3)
    gt3le5 = round(length(hourResult0[hourResult0$X2 > 3000 &
                                        hourResult0$X2 <= 5000, ]$X2) / allNum0, 2)
    gt3le5_list <- c(gt3le5_list, gt3le5)
    gt5le10 = round(length(hourResult0[hourResult0$X2 > 5000 &
                                         hourResult0$X2 <= 10000, ]$X2) / allNum0, 2)
    gt5le10_list <- c(gt5le10_list, gt5le10)
    big10 = round(length(hourResult0[hourResult0$X2 > 10000, ]$X2) / allNum0, 2)
    big10_list <- c(big10_list, big10)
    
  }
  review_frame <-
    data.frame(Time,
               lt1_list,
               gt1le3_list,
               gt3le5_list,
               gt5le10_list,
               big10_list)
  w = review_frame
  colnames(w)[1] <- 'Time'
  colnames(w)[2] <- 'le1'
  colnames(w)[3] <- 'gt1le3'
  colnames(w)[4] <- 'gt3le5'
  colnames(w)[5] <- 'gt5le10'
  colnames(w)[6] <- 'gt10'
  #----------------------------多条折线图-------------------
  # test_data_long <- melt(w, id="Time")
  # ggplot(data = test_data_long,mapping = aes(x=Time,y=value,colour=variable))+ylim(0,1)+geom_line()
  
  #===========================堆积图===========================
  t <- 0
  num <- 0
  group <- ""
  
  for (i in 0:23) {
    t <- c(t, rep(i, ncol(w) - 1))
    group <- c(group, names(w[, -1]))
    num <- c(num, as.matrix(w[, -1])[i + 1, ])
  }
  data <- data.frame(Time = t,
                     CrTime = group,
                     Ratio = num)
  data = data[-1, ]
  data
  data$CrTime = factor(data$CrTime,
                       levels = c('gt10', 'gt5le10', 'gt3le5', 'gt1le3', 'le1'))
  #ggplot(data = data, mapping = aes(x = factor(Time), y = Ratio,fill = CrTime)) + geom_bar(stat = 'identity', position = 'dodge')
  ggplot(data = data,
         mapping = aes(x = Time, y = Ratio, fill = CrTime)) + geom_bar(stat = 'identity', position = 'stack')
  #ggplot(data,aes(x=Time,y=Ratio,group=CrTime,fill=CrTime))+geom_area(position = 'fill')
}
