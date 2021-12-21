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


#create the flow chart
create.erd.flowchart <- function()
{
  grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      tab8 [label = '@@8']

      # edge definitions with the node IDs
      tab1 -> tab2;
      tab2 -> tab3;
      tab2 -> tab4;
      tab2 -> tab5;
      tab5 -> tab6;
      tab5 -> tab7;
      tab5 -> tab8;
      }

      [1]: 'Archtype'
      [2]: 'Enrollments'
      [3]: 'Team Member'
      [4]: 'Leaving Survey'
      [5]: 'Step Activity'
      [6]: 'Question Response'
      [7]: 'Video Stats'
      [8]: 'Sentiment Survey'
      ")
}

create.i2.erd.flowchart <- function()
{
  grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']

      # edge definitions with the node IDs
      tab1 -> tab3;
      tab2 -> tab3;
      tab3 -> tab4;
      tab3 -> tab5;
      tab3 -> tab6;
      tab3 -> tab7;
      }

      [1]: 'Person'
      [2]: 'Student Info'
      [3]: 'Step'
      [4]: 'Answers'
      [5]: 'Video By Device (and pivot)'
      [6]: 'Video Views (and pivot)'
      [7]: 'Video By Location (and pivot)'
      [8]: 'Sentiment Survey'
      ")
  
}





