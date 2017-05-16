# devtools::use_package('entropy')
#' Normalized Mutual information estimator
#'
#' Function used to estimate the Normalized Mutual Information
#'
#' In probability theory and information theory, the mutual information (MI)
#'  of two random variables is a measure of the mutual dependence between the
#'  two variables. More specifically, it quantifies the "amount of information"
#'  (in units such as bits) obtained about one random variable, through the
#'  other random variable. The concept of mutual information is intricately
#'  linked to that of entropy of a random variable, a fundamental notion in
#'  information theory, that defines the "amount of information" held in a
#'  random variable.
#'
#' @export
#' @param imA,imB cimg objects
#' @param binsA,binsB numerical values indicatin the number of discrete bins for each image
#' @return the NMI value for imA and imB
#' @examples
#' imA <- ground[[1]]
#' imB <- ground[[2]]
#' imC <- ground[[20]]
#' NMI(imA, imB)
#' NMI(imA, imC)
#' entropy <- sapply(ground, NMI, imB = ground[[1]])
#' plot(entropy)
#' lines(entropy)
NMI <- function(imA, imB, binsA = 10, binsB = 10){

  imA[imB == 0] <- 0
  imB[imA == 0] <- 0

  p_AB <- entropy::discretize2d(imA, imB, binsA, binsB)
  p_A <- rowSums(p_AB)
  p_B <- colSums(p_AB)

  H_A <- entropy::entropy(p_A)
  H_B <- entropy::entropy(p_B)
  H_AB <- entropy::entropy(p_AB)

  return(log2((H_A + H_B)/H_AB))
}
