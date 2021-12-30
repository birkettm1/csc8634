helper.function <- function()
{
  return(1)
}

df.examine = function(df){
  colnames(df)
  summary(df)
  dim(df)
  str(df)
}

df.view = function(df){
  view(df)  
}

df.headtail = function(df){
  head(df, n = 10)
  tail(df, n = 5)
}

examine.all.rows = function(data){
  print(data,n=nrow(data))
}

as.percent = function(big, small){
  x = (small / 100)
  y = (x * big)
  return (as.integer(y))
}

plot.continuous = function(df, column){
  title <- paste("Use of", column, sep=" ")
  
  ggplot(data=df, aes(!!sym(column), y=n, group=1)) +
    geom_line() +
    labs(title=title, y="Count", x = column) + 
    theme_bw() + 
    scale_fill_brewer(palette="PuBu")
}

plot.qq = function(df, column){
  title <- paste("QQ for", column, sep=" ")
  ggplot(df, aes(sample=!!sym(column))) +
    stat_qq() +
    labs(title=title) + 
    theme_bw() + 
    scale_fill_brewer(palette="PuBu")
}

#plots
plot.answers = function(data, column){
  
  #create title
  title <- paste("Answers by", column, sep=" ")
  
  #join the answers data
  df = left_join(data, dfAnswers, by = c("learner_id" = "learner_id"))
  df <- select(df, learner_id, column, quiz_question, correct)
  
  #do some cleaning
  df <- df %>% drop_na(!!sym(column))
  df <- df %>% drop_na(column)
  df <- df %>% drop_na(correct)
  df <- filter(df, !!sym(column) != "Unknown")
  
  ggplot(data = df, 
         aes(fill=correct, x = !!sym(column))) +
    geom_bar() +
    labs(title=title, y="Count", x = "Archetype") + 
    theme_bw() + 
    scale_fill_brewer(palette="PuBu") +
    theme(axis.text.x = element_text(angle = 90))
}

#function to plot progress in completed steps by categorical
plot.progress = function(data, column) {
  
  #create title
  title <- paste("Steps Complete by", column, sep=" ")
  
  #join on to the step data
  df <- left_join(data, dfStep, by = c("learner_id" = "learner_id"))
  df <- select(df, learner_id, step, column, isComplete)
  
  #do some cleaning
  df <- df %>% drop_na(!!sym(column))
  df <- df %>% drop_na(column)
  df <- df %>% drop_na(isComplete)
  df <- filter(df, !!sym(column) != "Unknown")
  
  #summary(df)
  ggplot(data = df, 
         aes(fill=isComplete, x = !!sym(column), y=step)) +
    geom_bar(stat="identity") +
    labs(title=title, y="Count", x = column) + 
    theme_bw() + 
    scale_fill_brewer(palette="PuBu", name="Step Complete") + 
    theme(axis.text.x = element_text(angle = 90))
}


#function to plot an enrollment with categorical data
plot.enrollment = function(data, column){
  
  #create title
  title <- paste("Students by", column, sep=" ")
  
  #tidy data
  df <- data %>% drop_na(all_of(column))
  df <- filter(df, !!sym(column) != "Unknown")
  
  #create the plot
  ggplot(data = df, aes_string(x = column)) +
    geom_bar() +
    labs(title= title, y="Count", x = column) +  
    #theme_bw() + 
    scale_fill_brewer(palette="PuBu") + 
    theme(axis.text.x = element_text(angle = 90))
}




