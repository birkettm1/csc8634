library('ProjectTemplate')
load.project()

for (dataset in project.info$data)
{
  message(paste('Showing top 5 rows of', dataset))
  print(head(get(dataset)))
}

examine.df(gpu)
examine.df(task.x.y)
examine.df(application.checkpoints)