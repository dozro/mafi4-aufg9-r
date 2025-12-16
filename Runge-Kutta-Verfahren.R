# Mathematik für Informatik 4: Übung 9
# Author: itsrye.dev
# Year: 2025

runge_kutta <- function(f, x0, y0, h, n) {
  x <- numeric(n + 1)
  y <- numeric(n + 1)
  
  df <- data.frame(c = numeric(), x = numeric(), y = numeric(), k1 = numeric(), k2 = numeric(), k3 = numeric(), k4 = numeric())
  
  x[1] <- x0
  y[1] <- y0
  
  for(i in 1:n ){
    k1 <- h * f(x[i], y[i])
    k2 <- h * f(x[i] + h/2, y[i] + k1/2)
    k3 <- h * f(x[i] + h/2, y[i] + k2/2)
    k4 <- h * f(x[i]+h, y[i]+k3)
    
    y[i+1] = y[i] + (1/6)*(k1 + 2 * k2 + 2 * k3 + k4)
    x[i+1] = x[i] + h
    
    new_row <- data.frame(c = i, x = x[i+1], y = y[i+1], k1 = k1, k2 = k2, k3 = k3, k4 = k4)
    df <- rbind(df, new_row)
  }
  
  # Zeichnen
  plot(
    x, y,
    type = "o",
    pch = 16,
    col = "blue",
    xlab = "x",
    ylab = "y",
    main = "Runge Kutta Verfahren"
  )
  grid()
  
  print(df)
}

# Aus dem Übungsblatt
f1 <- function(x,y){
  y + exp(x)
}
runge_kutta(f1, 0, 1, 0.05, 25)

# weitere Beispiele

f2 <- function(x,y){
  y
}

runge_kutta(f2, 0, 2, 0.05, 25)

f3 <- function(x,y){
  sin(x)
}

runge_kutta(f3, 0, 0, 0.5, 25)

# weitere Aufgaben aus dem Übungsblatt

f4 <- function(x,y){
  y^2 + 3*x
}
runge_kutta(f4, 0, 1, 0.1, 4)

