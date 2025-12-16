# Mathematik für Informatik 4: Übung 9
# Author: itsrye.dev
# Year: 2025

euler_plot <- function(f, x0, y0, h, n) {
  # Startwerte
  x <- numeric(n + 1)
  y <- numeric(n + 1)
  
  df <- data.frame(c = numeric(), x = numeric(), y = numeric())
  
  x[1] <- x0
  y[1] <- y0
  
  # Explizites Euler Verfahren
  for (i in 1:n) {
    y[i + 1] <- y[i] + h * f(x[i], y[i])
    x[i + 1] <- x[i] + h
    new_row <- data.frame(c = i, x = x[i+1], y = y[i+1])
    df <- rbind(df, new_row)
  }
  
  plot(
    x, y,
    type = "o",
    pch = 16,
    col = "blue",
    xlab = "x",
    ylab = "y",
    main = "Euler Verfahren"
  )
  
  grid()
  print(df)
}


# Explizites Euler Verfahren; von Rye geschrieben
eulerStreckenzug <- function(func, x0, y0, h, n){
  x <- x0
  y <- y0
  for (i in 1:n) {
    y_neu <- y + h * func(x,y)
    x_neu <- x +h
    x <- x_neu
    y <- y_neu
    cat(sprintf("Zwischenergebnis %d: x=%g, y=%g\n", i, x,y))
  }
}

f1b <- function(x,y){
  x + y
}

eulerStreckenzug(f1b, 0, 1, 0.01, 10)
euler_plot(f1b, 0, 1, 0.35, 100)

# Aus dem Übungsblatt
f1 <- function(x,y){
  y + exp(x)
}

eulerStreckenzug(f1, 0, 1, 0.05, 10)
euler_plot(f1, 0, 1, 0.05, 100)
