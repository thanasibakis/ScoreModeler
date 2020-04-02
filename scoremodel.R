score.plot <- function(density.func, score.limits, my.score = 0)
{
  p <- ggplot(score.limits, aes(x = Score)) +
    stat_function(fun = density.func) +
    ylab("Density") +
    theme_minimal() +
    stat_function(
      fun = density.func,
      geom = "area",
      fill = "light blue",
      alpha = 0.4,
      xlim = c(0, my.score)
    )

  if (my.score == 0)
    return(p)

  p + geom_vline(xintercept = my.score,
                 color = "blue",
                 alpha = 0.3)
}


score.percentile <- function(density.func, my.score = 0)
{
  integrate(density.func, 0, my.score)$value * 100
}


score.model <- function(max.score, mean.score, sd.score, ec.possible = FALSE)
{
  if (!ec.possible)
  {
    scaled.mean = mean.score / max.score
    scaled.var = sd.score ^ 2 / max.score ^ 2

    a = scaled.mean * (scaled.mean * (1 - scaled.mean) / scaled.var - 1)
    b = (1 - scaled.mean) * (scaled.mean * (1 - scaled.mean) / scaled.var - 1)

    density.func = function(x)
      dbeta(x / max.score, a, b) / max.score
    score.limits = data.frame(Score = c(0, max.score))
  }

  else
  {
    density.func = function(x)
      dnorm(x, mean.score, sd.score)
    score.limits = data.frame(Score = c(0, max.score + sd.score))
  }

  return(list(density.func, score.limits))
}