# By default, group-level coefficients within a grouping factor are assumed to be correlated. 
# Correlations can be set to zero by using the (coefs || group) syntax 

brms::make_stancode(use ~ urban||district,
                    data = df, 
                    family = bernoulli(link = "logit"))

brms::make_stancode(use ~ urban|district,
                    data = df, 
                    family = bernoulli(link = "logit"))

### 下記の関数でgroup level effectsのcorrelationが計算される.

# functions {
#   /* compute correlated group-level effects
#   * Args:
#     *   z: matrix of unscaled group-level effects
#   *   SD: vector of standard deviation parameters
#   *   L: cholesky factor correlation matrix
#   * Returns:
#     *   matrix of scaled group-level effects
#   */
#     matrix scale_r_cor(matrix z, vector SD, matrix L) {
#       // r is stored in another dimension order than z
#       return transpose(diag_pre_multiply(SD, L) * z);
#     }
# }