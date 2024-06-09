guess_num <- function(){
  generate_unique_number <- function(){
    repeat {
      num <- sample(1000:9999, 1)
      num_str <- as.character(num)
      num_vec <- as.numeric(strsplit(num_str, "")[[1]])
      if(length(unique(num_vec)) == 4) {
        return(num_vec)
      }
    }
  }
  
  # 生成答案，確保每個數字都不一樣
  ans <- generate_unique_number()
  
  repeat {
    # 輸入
    num_input <- readline("輸入一個四位數字（每個數字都不一樣）：")
    
    # 檢查輸入的數字是否有效
    if(nchar(num_input) != 4 || length(unique(strsplit(num_input, "")[[1]])) != 4) {
      print("輸入的數字無效，請確保每個數字都不一樣且為四位數。")
      next
    }
    
    # 拆分字串，並轉換成數字向量
    num <- as.numeric(strsplit(num_input, "")[[1]])
    
    # 計算A和B的數量
    A <- 0
    B <- 0
    for(i in 1:4){
      if(num[i] == ans[i]){
        A <- A + 1
      } else if(num[i] %in% ans){
        B <- B + 1
      }
    }
    
    # 檢查是否答對
    if(A == 4){
      print("恭喜答對")
      break
    } else {
      message <- paste(A, "A", B, "B")
      print(message)
    }
  }
}

guess_num()
