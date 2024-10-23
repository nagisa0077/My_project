guess_number <- function(){
  choosed_number <- sample(1:100, 1)
  i <- 1
  
  repeat {
    input <- readline("1~100，輸入一個數字：")
    input_number <- as.numeric(input)
    
    if (input_number > choosed_number) {
      print(paste("大了，這是第", i, "次"))
    } else if (input_number < choosed_number) {
      print(paste("小了，這是第", i, "次"))
    } else {
      print("猜對了！")
      print(paste("你一共猜了", i, "次"))
      break
    }
    
    i <- i + 1
    if (i > 6) {
      print(paste("遊戲結束！正確的數字是：", choosed_number))
      break
    }
  }
}

guess_number()

