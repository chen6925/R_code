library(ggplot2)
data_word <- read.csv("C:/word.csv", header = FALSE)  
leave <- 0
while(leave != 1)
{
  r_length <- as.numeric(nrow(data_word))
  time <- 0
  mode <- as.numeric(readline(prompt = "1.電腦猜2.玩家猜3.離開遊戲:"))
  if(mode == 1)
  {
    amount <- as.numeric(readline(prompt = "請輸入字數:"))
    a <- nchar(as.character(data_word[1:r_length,1])) == amount
    complete <- 0
    lock <- 0
    do <- 1
    h_answer <-  letters
    letter_num <- numeric(26)
    answer <- readline(prompt = "目前答案:")
    answer_v <- strsplit(answer,"")[[1]]
    num_answer <- 0
    while(complete < amount)
    {
      num_answer <- 0
      complete <- 0
      for(i in 1:r_length)
      {
        if(a[i] == TRUE)
        {
          word <- unique(strsplit(data_word[i,1],"")[[1]])
          word_all <- strsplit(data_word[i,1],"")[[1]]
          for(k in 1:amount)
          {
            if(answer_v[k] != "-" && answer_v[k] == word_all[k] && do == 1)
            {
              do <- 1
            }
            else if(answer_v[k] != "-" && answer_v[k] != word_all[k])
            {
              do <- 0
            }
          }
          if(do == 1)
          {
            num_answer <- num_answer + 1
            record <- i
            for(j in 1:length(word))
            {
              where <- which(word[j] == letters)
              add <- 1
              if(h_answer[where] == TRUE)
              {
                add <- 0
                if(answer_v[j] == "-")
                {
                  break
                }
              }
              if(add == 1)
              {
                letter_num[where] = letter_num[where] + 1
              }
            }
          }
          do <- 1
        }
      }
      picture_v <- sort(letter_num,decreasing = TRUE)
      picture_t <- c(1:10)
      for(i in 1:10)
      {
        break_1 <- 0
        for(j in 1:26)
        {
          if(picture_v[i] == letter_num[j])
          {
            picture_t[i] = letters[j]
            break_1 <- 1
            for(k in 1:10)
            {
              if(picture_t[k] == picture_t[i] && k < i)
              {
                break_1 <- 0
              }
            }
          }
          if(break_1 == 1)
          {
            break
          }
        }
      }
      d <- data.frame(letter = picture_t[1:10],num = picture_v[1:10])
      p <- ggplot(data = d, aes(x = letter, y = num)) + geom_bar(stat = "identity")
      print(p)
      if(num_answer != 1)
      {
        biggest <- which.max(letter_num)
        cat(sprintf("電腦猜:%s",letters[biggest]))
        time <- time + 1
        h_answer[biggest] <- TRUE 
        answer <- readline(prompt = "目前答案:")
      }
      else
      {
        cat(sprintf("答案是:%s\n",data_word[record,1]))
        break
      }
      for(j in 1:length(word))
      {
        letter_num[1:26] <-  0
      }
      answer_v <- strsplit(answer,"")[[1]]
      lock <- 0
      for(i in 1:amount)
      {
        if(answer_v[i] != "-")
        {
          complete <- complete + 1
        }
      }
    }
  }
  if(mode == 2)
  {
    time <- 0
    random <- floor(runif(1,min = 1,max = r_length))
    answer <- data_word[random,1]
    a <- nchar(as.character(answer))
    word <- strsplit(answer,"")[[1]]
    answer_now <- c(1:a)
    for(i in 1:a)
    {
      cat("-")
      answer_now[i] <- "-"
    }
    cat("\n")
    all <- "-" 
    while(all != answer)
    {
      guess <- readline(prompt = "玩家猜:")
      time <- time + 1
      for(i in 1:a)
      {
        if(word[i] == guess)
        {
          answer_now[i] <- guess
        }
      }
      all <- paste(answer_now, collapse = "")
      cat(all)
    }
  }
  if(mode == 3)
  {
    leave <- 1
  }
  if(leave != 1)
  {
    cat(sprintf("遊戲結束 猜題次數:%d",time))
  }
}