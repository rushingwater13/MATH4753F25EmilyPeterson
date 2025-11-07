# app.R
library(shiny)
library(ggplot2)
library(MASS)        # fitdistr
library(stats)
library(reshape2)

# Helper: Beta MLE via optim
mle_beta <- function(x){
  eps <- 1e-8
  x <- pmin(pmax(x, eps), 1 - eps)   # clip to (0,1)
  negll <- function(par){
    a <- par[1]; b <- par[2]
    if (a <= 0 || b <= 0) return(1e20)
    -sum(dbeta(x, a, b, log = TRUE))
  }
  res <- optim(par = c(1,1), fn = negll, method = "L-BFGS-B", lower = c(1e-8,1e-8), control=list())
  list(par = res$par, value = res$value, converged = res$convergence == 0, hessian = tryCatch(res$hessian, error=function(e) NULL))
}

# Compute log-likelihood for named distributions
loglik_of <- function(dist, x, par){
  if (dist == "Normal") {
    mu <- par["mean"]; sd <- par["sd"]
    sum(dnorm(x, mean=mu, sd=sd, log=TRUE))
  } else if (dist == "Exponential") {
    rate <- par["rate"]
    sum(dexp(x, rate=rate, log=TRUE))
  } else if (dist == "Poisson") {
    lambda <- par["lambda"]
    sum(dpois(x, lambda=lambda, log=TRUE))
  } else if (dist == "Gamma") {
    shape <- par["shape"]; rate <- par["rate"]
    sum(dgamma(x, shape=shape, rate=rate, log=TRUE))
  } else if (dist == "Weibull") {
    shape <- par["shape"]; scale <- par["scale"]
    sum(dweibull(x, shape=shape, scale=scale, log=TRUE))
  } else if (dist == "Beta") {
    a <- par["a"]; b <- par["b"]
    sum(dbeta(x, a, b, log=TRUE))
  } else NA
}

ui <- fluidPage(
  titlePanel("MLE demonstration for univariate distributions"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_mode", "Data input",
                   choices = c("Simulate data" = "sim", "Upload CSV (1 column)" = "upload")),
      conditionalPanel(
        "input.data_mode == 'sim'",
        selectInput("sim_dist", "Distribution to simulate",
                    choices = c("Normal","Exponential","Poisson","Gamma","Weibull","Beta")),
        numericInput("sim_n", "Sample size", value = 200, min = 10),
        # params - show as text inputs for flexibility
        uiOutput("sim_params"),
        actionButton("gen", "Generate")
      ),
      conditionalPanel(
        "input.data_mode == 'upload'",
        fileInput("file", "Upload CSV (single numeric column)", accept = c(".csv",".txt")),
        checkboxInput("round_for_pois", "Round uploaded values for Poisson fit (if needed)", value = TRUE)
      ),
      hr(),
      selectInput("fit_dist", "Distribution to fit (MLE)", 
                  choices = c("Normal","Exponential","Poisson","Gamma","Weibull","Beta")),
      checkboxInput("do_boot", "Compute bootstrap CIs (slow)", value = FALSE),
      conditionalPanel("input.do_boot == true",
                       numericInput("nboot","Bootstrap resamples", value = 200, min=20, max=2000, step=10)),
      actionButton("fit", "Fit (MLE)"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 verbatimTextOutput("fit_summary")),
        tabPanel("Plot",
                 plotOutput("main_plot", height = "420px"),
                 plotOutput("qq_plot", height = "320px")),
        tabPanel("Data",
                 tableOutput("head_data"))
      ),
      width = 9
    )
  )
)

server <- function(input, output, session){
  # dynamic UI for simulation params
  output$sim_params <- renderUI({
    switch(input$sim_dist,
           "Normal" = tagList(numericInput("p1", "mean", value = 0), numericInput("p2", "sd", value = 1, min=1e-6)),
           "Exponential" = numericInput("p1", "rate", value = 1, min=1e-8),
           "Poisson" = numericInput("p1", "lambda", value = 3, min=1e-8),
           "Gamma" = tagList(numericInput("p1","shape", value = 2, min=1e-8), numericInput("p2","rate", value = 1, min=1e-8)),
           "Weibull" = tagList(numericInput("p1","shape", value = 2, min=1e-8), numericInput("p2","scale", value = 1, min=1e-8)),
           "Beta" = tagList(numericInput("p1","a (alpha)", value = 2, min=1e-8), numericInput("p2","b (beta)", value = 5, min=1e-8))
    )
  })
  
  data_r <- reactiveVal(NULL)
  observeEvent(input$gen, {
    n <- input$sim_n
    switch(input$sim_dist,
           "Normal" = { x <- rnorm(n, mean = input$p1, sd = input$p2) },
           "Exponential" = { x <- rexp(n, rate = input$p1) },
           "Poisson" = { x <- rpois(n, lambda = input$p1) },
           "Gamma" = { x <- rgamma(n, shape = input$p1, rate = input$p2) },
           "Weibull" = { x <- rweibull(n, shape = input$p1, scale = input$p2) },
           "Beta" = { x <- rbeta(n, shape1 = input$p1, shape2 = input$p2) }
    )
    data_r(data.frame(x = as.numeric(x)))
  })
  
  # read uploaded
  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch({
      read.csv(input$file$datapath, header = TRUE)
    }, error = function(e){
      tryCatch(read.table(input$file$datapath, header = TRUE, sep = ",", fill=TRUE), error = function(e2) NULL)
    })
    if (is.null(df)) {
      showModal(modalDialog(title="Error", "Could not read file. Make sure it's a CSV or plain text table."))
      return()
    }
    # try to find first numeric column
    nums <- sapply(df, is.numeric)
    if(!any(nums)){
      # try to coerce first column
      df[[1]] <- as.numeric(df[[1]])
      if(all(is.na(df[[1]]))) {
        showModal(modalDialog(title="Error", "No numeric column found in uploaded file."))
        return()
      }
    }
    col <- which(nums)[1]
    if(is.na(col)) col <- 1
    data_r(data.frame(x = as.numeric(df[[col]])))
  })
  
  output$head_data <- renderTable({
    d <- data_r()
    if (is.null(d)) return()
    head(d, 30)
  })
  
  # Fitting action
  fit_res <- eventReactive(input$fit, {
    dfr <- data_r()
    if (is.null(dfr)) {
      showModal(modalDialog(title="No data", "No data available. Simulate or upload first."))
      return(NULL)
    }
    x <- dfr$x
    fitdist <- input$fit_dist
    
    # For Poisson, force integers (round) if user opted
    if (fitdist == "Poisson" && input$data_mode == "upload" && input$round_for_pois) {
      x <- round(x)
    }
    
    # Some dists require positive support
    if (fitdist %in% c("Exponential","Gamma","Weibull") && any(x <= 0)) {
      showModal(modalDialog(title="Domain error", paste0("Selected distribution ", fitdist, " requires x > 0. Data has <= 0 values.")))
      return(NULL)
    }
    if (fitdist == "Beta"){
      # beta requires (0,1)
      if (any(x <= 0 | x >= 1)){
        showModal(modalDialog(title="Domain error", "Beta distribution requires data in (0,1). Either rescale your data or choose another distribution."))
        return(NULL)
      }
    }
    
    res <- list()
    if (fitdist == "Normal"){
      fr <- fitdistr(x, "normal")
      params <- as.list(fr$estimate); names(params) <- c("mean","sd")
      se <- fr$sd
      names(se) <- names(params)
      loglik <- loglik_of("Normal", x, params)
      k <- 2
    } else if (fitdist == "Exponential"){
      # MASS::fitdistr for exponential returns rate
      fr <- fitdistr(x, "exponential")
      params <- list(rate = unname(fr$estimate))
      se <- fr$sd; names(se) <- names(params)
      loglik <- loglik_of("Exponential", x, params)
      k <- 1
    } else if (fitdist == "Poisson"){
      # lambda = sample mean (MLE)
      lambda_hat <- mean(x)
      params <- list(lambda = lambda_hat)
      # se approx sqrt(lambda/n)
      se <- sqrt(lambda_hat / length(x))
      names(se) <- "lambda"
      loglik <- loglik_of("Poisson", x, params)
      k <- 1
    } else if (fitdist == "Gamma"){
      fr <- fitdistr(x, "gamma")
      params <- as.list(fr$estimate); names(params) <- c("shape","rate")
      se <- fr$sd; names(se) <- names(params)
      loglik <- loglik_of("Gamma", x, params)
      k <- 2
    } else if (fitdist == "Weibull"){
      # MASS::fitdistr supports "weibull"
      fr <- fitdistr(x, "weibull")
      # MASS returns shape and scale (named "shape" "scale")
      params <- as.list(fr$estimate); names(params) <- c("shape","scale")
      se <- fr$sd; names(se) <- names(params)
      loglik <- loglik_of("Weibull", x, params)
      k <- 2
    } else if (fitdist == "Beta"){
      fr <- mle_beta(x)
      if (!fr$converged) {
        showModal(modalDialog(title="Convergence warning", "Beta MLE did not converge."))
      }
      params <- list(a = fr$par[1], b = fr$par[2])
      # approximate SE from optim's hessian if available
      se <- c(NA, NA)
      names(se) <- c("a","b")
      if (!is.null(fr$hessian) && !any(is.na(fr$hessian))){
        covmat <- tryCatch(solve(fr$hessian), error = function(e) NULL)
        if (!is.null(covmat)) se <- sqrt(diag(covmat))
        names(se) <- c("a","b")
      }
      loglik <- -fr$value
      k <- 2
    } else {
      return(NULL)
    }
    
    aic <- 2*k - 2*loglik
    
    # optional bootstrap
    boot_ci <- NULL
    if (input$do_boot){
      nboot <- min(2000, max(20, as.integer(input$nboot)))
      B <- nboot
      bs_pars <- replicate(B, {
        xb <- sample(x, replace=TRUE)
        # quick fit inside bootstrap according to dist
        if (fitdist == "Normal"){
          frb <- fitdistr(xb, "normal")
          unname(frb$estimate)
        } else if (fitdist == "Exponential"){
          frb <- fitdistr(xb, "exponential")
          unname(frb$estimate)
        } else if (fitdist == "Poisson"){
          c(mean(xb))
        } else if (fitdist == "Gamma"){
          frb <- fitdistr(xb, "gamma")
          unname(frb$estimate)
        } else if (fitdist == "Weibull"){
          frb <- fitdistr(xb, "weibull")
          unname(frb$estimate)
        } else if (fitdist == "Beta"){
          frb <- tryCatch(mle_beta(xb)$par, error=function(e) c(NA,NA))
          frb
        } else c(NA)
      }, simplify = "array")
      # make into matrix
      bs_pars <- t(bs_pars)
      # compute 95% percentile CI for each parameter
      cis <- apply(bs_pars, 2, function(col) {
        if(all(is.na(col))) return(c(NA,NA))
        quantile(col, c(0.025,0.975), na.rm=TRUE)
      })
      boot_ci <- cis
      colnames(boot_ci) <- names(params)
    }
    
    list(dist = fitdist, params = params, se = se, loglik = as.numeric(loglik),
         aic = aic, n = length(x), data = x, boot_ci = boot_ci)
  })
  
  output$fit_summary <- renderPrint({
    res <- fit_res()
    if (is.null(res)) return("No fit yet.")
    cat("Distribution fitted:", res$dist, "\n\n")
    cat("Sample size:", res$n, "\n")
    cat("Log-likelihood:", format(res$loglik, digits=6), "\n")
    cat("AIC:", format(res$aic, digits=6), "\n\n")
    cat("Parameter estimates (MLE):\n")
    print(res$params)
    cat("\nApprox. standard errors:\n")
    print(res$se)
    if (!is.null(res$boot_ci)){
      cat("\nBootstrap 95% percentile CIs:\n")
      print(round(res$boot_ci,4))
    }
  })
  
  output$main_plot <- renderPlot({
    res <- fit_res()
    if (is.null(res)) return(NULL)
    x <- res$data
    df <- data.frame(x = x)
    dist <- res$dist
    
    if (dist == "Poisson"){
      # bar plot of counts + fitted pmf overlay
      tab <- as.data.frame(table(factor(x)))
      colnames(tab) <- c("x","count")
      tab$xnum <- as.numeric(as.character(tab$x))
      maxx <- max(tab$xnum, ceiling(qpois(0.999, lambda=res$params$lambda)))
      ggplot(tab, aes(x = xnum, y = count)) +
        geom_col(fill = "lightblue", color="black") +
        stat_function(fun = function(k) {
          # convert pmf to expected counts
          dpois(k, lambda=res$params$lambda) * length(x)
        }, args = list(), n = maxx+1, geom = "line") +
        labs(x = "x (counts)", y = "Frequency", title = paste("Poisson fit: lambda =", round(res$params$lambda,4)))
    } else {
      p <- ggplot(df, aes(x = x)) + geom_histogram(aes(y = ..density..), bins = 30, color="black", fill="lightgray")
      # overlay fitted density
      xs <- seq(min(x), max(x), length.out = 400)
      dens <- switch(dist,
                     "Normal" = dnorm(xs, mean=res$params$mean, sd=res$params$sd),
                     "Exponential" = dexp(xs, rate=res$params$rate),
                     "Gamma" = dgamma(xs, shape=res$params$shape, rate=res$params$rate),
                     "Weibull" = dweibull(xs, shape=res$params$shape, scale=res$params$scale),
                     "Beta" = dbeta(xs, shape1=res$params$a, shape2=res$params$b),
                     rep(NA, length(xs))
      )
      p + geom_line(data = data.frame(x=xs,y=dens), aes(x=x,y=y), size=1) +
        labs(title = paste("Histogram with fitted", dist, "density"))
    }
  })
  
  output$qq_plot <- renderPlot({
    res <- fit_res()
    if (is.null(res)) return(NULL)
    x <- sort(res$data)
    n <- length(x)
    ppoints <- ppoints(n)
    dist <- res$dist
    
    theo_q <- switch(dist,
                     "Normal" = qnorm(ppoints, mean=res$params$mean, sd=res$params$sd),
                     "Exponential" = qexp(ppoints, rate=res$params$rate),
                     "Poisson" = qpois(ppoints, lambda=res$params$lambda),
                     "Gamma" = qgamma(ppoints, shape=res$params$shape, rate=res$params$rate),
                     "Weibull" = qweibull(ppoints, shape=res$params$shape, scale=res$params$scale),
                     "Beta" = qbeta(ppoints, shape1=res$params$a, shape2=res$params$b)
    )
    qqdf <- data.frame(sample = x, theoretical = theo_q)
    ggplot(qqdf, aes(x=theoretical, y=sample)) +
      geom_point() + geom_abline(slope=1, intercept=0, linetype=2) +
      labs(title=paste("Q-Q plot vs", res$dist), x = "Theoretical quantiles", y = "Sample quantiles")
  })
}

shinyApp(ui, server)
