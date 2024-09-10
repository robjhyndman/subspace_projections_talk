component <- function(mat,
                      component = c("PCA", "normal", "uniform", "PCA_normal", "PCA_uniform", "ortho_normal",
                                    "PCAscaled_normal", "PCAcentred_normal"),
                      p = ncol(mat)){
  component <- match.arg(component)
  rnorm_w_once <- function(n){
    a <- rnorm(n)
    a/sqrt(sum(a^2))
  }
  runif_w_once <- function(n){
    a <- runif(n, min = -1, max = 1)
    a/sqrt(sum(a^2))
  }
  comp_names <- paste0("Component", seq_len(p))
  comp_fn <- switch(
    component,
    "PCA_normal" = ,
    "PCA" = function(mat, p) {
      pca <- stats::prcomp(mat, center = FALSE, scale. = FALSE)
      n_pca <- min(ncol(mat), nrow(mat), p)
      rotation <- pca$rotation[,seq_len(n_pca)]
      if(p>n_pca){

        rr <- replicate(p-n_pca, rnorm_w_once(ncol(mat)))
        # rownames(rr) <- colnames(mat)
        colnames(rr) <- paste0("RC_norm", seq_len(ncol(rr)))
        rotation <- cbind(rotation, rr)
      }
      rownames(rotation) <- colnames(mat)
      x <- mat %*% rotation
      list(x = x, Phi = t(rotation), sdev = pca$sdev)
    },
    "PCAscaled_normal" = function(mat, p) {
      pca <- stats::prcomp(mat, center = TRUE, scale. = TRUE)
      n_pca <- min(ncol(mat), nrow(mat), p)
      rotation <- pca$rotation[,seq_len(n_pca)]
      if(p>n_pca){

        rr <- replicate(p-n_pca, rnorm_w_once(ncol(mat)))
        # rownames(rr) <- colnames(mat)
        colnames(rr) <- paste0("RC_norm", seq_len(ncol(rr)))
        rotation <- cbind(rotation, rr)
      }
      rownames(rotation) <- colnames(mat)
      x <- mat %*% rotation
      list(x = x, Phi = t(rotation), sdev = pca$sdev)
    },
    "PCAcentred_normal" = function(mat, p) {
      pca <- stats::prcomp(mat, center = TRUE, scale. = FALSE)
      n_pca <- min(ncol(mat), nrow(mat), p)
      rotation <- pca$rotation[,seq_len(n_pca)]
      if(p>n_pca){

        rr <- replicate(p-n_pca, rnorm_w_once(ncol(mat)))
        # rownames(rr) <- colnames(mat)
        colnames(rr) <- paste0("RC_norm", seq_len(ncol(rr)))
        rotation <- cbind(rotation, rr)
      }
      rownames(rotation) <- colnames(mat)
      x <- mat %*% rotation
      list(x = x, Phi = t(rotation), sdev = pca$sdev)
    },
    "PCA_uniform" = function(mat, p) {
      pca <- stats::prcomp(mat, center = FALSE, scale. = FALSE)
      n_pca <- min(ncol(mat), nrow(mat), p)
      rotation <- pca$rotation[,seq_len(n_pca)]
      if(p>n_pca){

        rr <- replicate(p-n_pca, runif_w_once(ncol(mat)))
        colnames(rr) <- paste0("RC_unif", seq_len(ncol(rr)))
        rotation <- cbind(rotation, rr)
      }
      rownames(rotation) <- colnames(mat)
      x <- mat %*% rotation
      list(x = x, Phi = t(rotation), sdev = pca$sdev)
    },
    "normal" = function(mat, p){
      rotation <- replicate(p, rnorm_w_once(ncol(mat)))
      colnames(rotation) <- paste0("RC_norm", seq_len(p))
      rownames(rotation) <- colnames(mat)
      x <- mat %*% rotation
      list(x = x, Phi = t(rotation))
    },
    "uniform" = function(mat, p){
      rotation <- replicate(p, runif_w_once(ncol(mat)))
      colnames(rotation) <- paste0("RC_unif", seq_len(p))
      rownames(rotation) <- colnames(mat)
      x <- mat %*% rotation
      list(x = x, Phi = t(rotation))
    },
    "ortho_normal" = function(mat, p){
      rotation <- pracma::randortho(ncol(mat))[,seq_len(min(ncol(mat),p))]
      colnames(rotation) <- paste0("OrC", seq_len(ncol(rotation)))
      if(p>ncol(mat)){

        rr <- replicate(p-ncol(mat), rnorm_w_once(ncol(mat)))
        colnames(rr) <- paste0("RC_norm", seq_len(ncol(rr)))
        rotation <- cbind(rotation, rr)
      }
      rownames(rotation) <- colnames(mat)
      x <- mat %*% rotation
      list(x = x, Phi = t(rotation))
    },
  )
  comp_fn(mat, p)
}
