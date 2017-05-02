
rts1_sv <- function(input, output, session, values) {
  output$rtsplot1 <- shiny::renderPlot( {
    shiny::req(input$N)
    shiny::req(input$sg1)
    shiny::req(input$sigmaG2)
    shiny::req(input$sigmaE2)

    N <- input$N
    sg1 <- input$sg1
    sigmaG2 <- input$sigmaG2
    sigmaE2 <- input$sigmaE2

    r <- seq(1, floor(N / sg1), 1)
    g <- floor(N / r)
    alpha <- sg1 / g
    z <- dnorm(qnorm(1 - alpha))
    i <- z / alpha
    rho <- sqrt(sigmaG2 / (sigmaG2 + sigmaE2 / r))
    R <- i * rho

    # draw the plot
    plot(r, R, xlab = "Number of replications", ylab = "Response to selection", type = "b")
    points(r[match(max(R), R)], max(R), col = "red", pch = 18)
    mtext(paste("Optimum number of replications = ", r[match(max(R), R)]), line = 2.9)
    mtext(paste("Number of genotypes at optimum = ", g[match(max(R), R)]), line = 1.7)
    mtext(paste("Response to selection at optimum = ", round(max(R), 2)), line = 0.5)
  })
}

rts2_sv <- function(input, output, session, values) {
output$rtsplot2 <- shiny::renderPlot( {
  shiny::req(input$N)
  shiny::req(input$sg1)
  shiny::req(input$sigmaG2)
  shiny::req(input$sigmaE2)
  shiny::req(input$sigmaGL2)
  shiny::req(input$k)

  N <- input$N
  k <- input$k
  sg1 <- input$sg1
  sigmaG2 <- input$sigmaG2
  sigmaGL2 <- input$sigmaGL2
  sigmaE2 <- input$sigmaE2

  r <- seq(1, floor(N / sg1 / k), 1)
  g <- floor(N / k / r)
  alpha <- sg1 / g
  z <- dnorm(qnorm(1 - alpha))
  i <- z / alpha
  rho <- sqrt(sigmaG2 / (sigmaG2 + sigmaGL2 / k + sigmaE2 / k / r))
  R <- i * rho

  # Draw the plot
  plot(r, R, xlab = "Number of replications", ylab = "Response to selection", type = "b")
  points(r[match(max(R), R)], max(R), col = "red", pch = 18)
  mtext(paste("Optimum number of replications = ", r[match(max(R), R)]), line = 2.9)
  mtext(paste("Number of genotypes at optimum = ", g[match(max(R), R)]), line = 1.7)
  mtext(paste("Response to selection at optimum = ", round(max(R), 2)), line = 0.5)
})
}


rts3_sv <- function(input, output, session, values){
  output$rtsplot3 <- shiny::renderPlot( {
    shiny::req(input$N)
    shiny::req(input$sg1)
    shiny::req(input$sigmaG2)
    shiny::req(input$sigmaE2)
    shiny::req(input$sigmaGL2)
    shiny::req(input$k)
    shiny::req(input$q)
    shiny::req(input$sigmaGY2)
    shiny::req(input$sigmaGLY2)
    shiny::req(input$sigmaE2)

    N <- input$N
    k <- input$k
    q <- input$q
    sg1 <- input$sg1
    sigmaG2 <- input$sigmaG2
    sigmaGL2 <- input$sigmaGL2
    sigmaGY2 <- input$sigmaGY2
    sigmaGLY2 <- input$sigmaGLY2
    sigmaE2 <- input$sigmaE2

    r <- seq(1, floor(N / sg1 / k / q), 1)
    g <- floor(N / k / q / r)
    alpha <- sg1 / g
    z <- dnorm(qnorm(1 - alpha))
    i <- z / alpha
    rho <- sqrt(sigmaG2 / (sigmaG2 + sigmaGL2 / k + sigmaGY2 / q + sigmaGLY2 / k / q + sigmaE2 / k / q / r))
    R <- i * rho

    # Draw the plot
    plot(r, R, xlab = "Number of replications", ylab = "Response to selection", type = "b")
    points(r[match(max(R), R)], max(R), col = "red", pch = 18)
    mtext(paste("Optimum number of replications = ", r[match(max(R), R)]), line = 2.9)
    mtext(paste("Number of genotypes at optimum = ", g[match(max(R), R)]), line = 1.7)
    mtext(paste("Response to selection at optimum = ", round(max(R), 2)), line = 0.5)
  })

}


rts4_sv <- function(input, output, session, values){
  output$rtstable4 <- shiny::renderTable( {

    g <- input$g
    k1 <- input$k1
    r1 <- input$r1
    sg1 <- input$sg1
    k2 <- input$k2
    r2 <- input$r2
    sg2 <- input$sg2
    sigmaG2 <- input$sigmaG2
    sigmaGL2 <- input$sigmaGL2
    sigmaGY2 <- input$sigmaGY2
    sigmaGLY2 <- input$sigmaGLY2
    sigmaE2 <- input$sigmaE2

    # first step

    alpha1 <- sg1 / g
    x1 <- qnorm(1 - alpha1) # truncation point on the N(0,1) for the selected fraction
    z1 <- dnorm(x1)
    i1 <- z1 / alpha1
    rho1 <- sqrt(sigmaG2 / (sigmaG2 + sigmaGL2 / k1 + sigmaGY2 + sigmaGLY2 / k1 + sigmaE2 / k1 / r1))
    R1 <- i1 * rho1

    # second step

    alpha2 <- sg2 / sg1
    rho2 <- sqrt(sigmaG2 / (sigmaG2 + sigmaGL2 / k2 + sigmaGY2 + sigmaGLY2 / k2 + sigmaE2 / k2 / r2))

    # both togheter

    alpha <- alpha1 * alpha2
    rho <- rho1 * rho2
    int <- function(x) {
      1 / sqrt(2 * pi) * exp(-x^2 / 2) * pnorm((x1 - rho * x) / (sqrt(1 - rho^2)), lower.tail = FALSE)
    }
    f <- function(t) {
      integrate(int, t, Inf)$value - alpha
    }
    x2 <- uniroot(f, c(0, 20))$root # truncation point on the N(0,1) for the selected fraction
    z2 <- dnorm(x2)
    a <- (x1 - rho * x2) / sqrt(1 - rho^2)
    b <- (x2 - rho * x1) / sqrt(1 - rho^2)
    I1 <- 1 - pnorm(a)
    I2 <- 1 - pnorm(b)
    R2 <- (rho1 * z1 * I2 + rho2 * z2 * I1) / alpha
    salida <- data.frame(row.names = c("1st step =", "2nd step ="))
    salida$x <- c(x1, x2)
    salida$Ru <- c(R1, R2)
    salida

  })
}

#' rts_sv
#'
#' @param input shiny
#' @param output shiny
#' @param session shiny
#' @param values shiny
#'
#' @return shiny
#' @export
rts_sv <- function(input, output, session, values){
  rts1_sv(input, output, session, values)
  rts2_sv(input, output, session, values)
  rts3_sv(input, output, session, values)
  #rts4_sv(input, output, session, values)
}
