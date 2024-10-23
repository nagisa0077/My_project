park <- function() {
  # 初始化停車場列表
  park_tab <- list(numberOFcar = character(), time = as.POSIXct(character()))
  
  while (TRUE) {
    inp <- readline(prompt = "請輸入車牌號碼: ")
    
    if (toupper(inp) == "Q") {
      print("系統終止")
      break
    } else if (inp %in% park_tab$numberOFcar) {
      loc <- which(park_tab$numberOFcar == inp)
      ext_time <- Sys.time()
      ent_time <- park_tab$time[loc]
      prnt <- difftime(ext_time, ent_time, units = "secs")
      print(paste("離開時間為:", ext_time))
      print(paste("總共停了", as.numeric(prnt), "秒"))
      if((round(as.numeric(prnt))/3600)*20 > 200){
        print(paste("共", 200, "元"))
      }else if((round(as.numeric(prnt))/60) < 30){
        print(paste("共", 0, "元"))
      }else{
        print(paste("共", (round(as.numeric(prnt))/3600)*20, "元"))
      }
      
      park_tab$numberOFcar <- park_tab$numberOFcar[-loc]
      park_tab$time <- park_tab$time[-loc]
    } else {
      park_tab$numberOFcar <- c(park_tab$numberOFcar, inp)
      park_tab$time <- c(park_tab$time, Sys.time())
    }
  }
}

park()

