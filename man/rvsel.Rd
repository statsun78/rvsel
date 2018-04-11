\name{rvsel}
\alias{rvsel}
\title{Rare variant selection procedure}
\description{
    When a gene or a genetic region is significantly associated with a
    disease/trait, the rvsel procedure can distinguish causal (risk or
    protective) rare variants from noncausal rare variants located within
    the same gene or the same genetic region.  \cr
    \cr
    The most outcome-related rare variants are selected within a gene or a
    genetic region, considering all possible combinations of rare variants.
    First, genetic data of each individual is combined into one dimensional
    numeric vector based on one of the following methods; a weighted linear
    combination of a subset of rare variants, an adaptive weighted linear
    combination of a subset of rare variants and a combined multivariate and
    collapsing method.\cr
    \cr
    Next, one of the selection procedures such as exhaustive search, forward
    selection, backward selection, and forward based both risk and protective 
    search is conducted to identify causal rare variants within a gene or a 
    genetic region. \cr
    }
\usage{rvsel(x,y,cx=NULL,weight=NULL,family=c("gaussian","binomial"),method=
    c("asum","sum","cmc"),selection=c("exhaustive","forward","backward","Fsel"),
    ad.alpha=0.1,lambda=0)
    }
\arguments{
    \item{x}{The number of genetic mutations with \eqn{n} samples and \eqn{p}
        variants, where \eqn{x=0,1,} or 2. It should be a \eqn{n} x \eqn{p}
        matrix and \eqn{p>1}.}
    \item{y}{A phenotype outcome is coded as 1 for cases and 0 for controls if
        the phenotype is case-control binary data. Otherwise, it is considered
        as a quantitative outcome.}
    \item{cx}{Covariates such as gender and age. It should be a \eqn{n} x
        \eqn{m} matrix, where \eqn{m} is the number of covariates.}
    \item{weight}{User defined weights for \eqn{p} variants. It should be
        the \eqn{p}-dimensional vector. Default is 1.}
    \item{family}{A type of phenotype data. "\code{binomial}" is for
        a case-control binary outcome and "\code{gaussian}" for a quantitative
        outcome. Default is "\code{gaussian}". }
    \item{method}{A way to combine genetic data. "\code{sum}" conducts a
       weighted linear combinations of rare variants, "\code{asum}" first
       conducts a pre-screening of potential protective variants via a marginal
       association test. If a potential protective variant is detected, the
       coding of \code{x} for the variant is flipped out and then a weighted
       linear combinations of rare variants is performed. "\code{cmc}" conducts
       a combined multivariate and collapsing method. Default is "\code{asum}".
       See details.}
    \item{selection}{A type of selection procedure. "\code{exhaustive}" performs
       a complete search of the power set of the subset of all rare variants,
       where the combination of the most outcome-related variants is finally
       selected. "\code{forward}" conducts a forward based selection, where
       the most outcome-related variant is sequentially added from a null
       model. "\code{backward}" performs a backward based selection, where the
       most insignificant variant is sequentially removed from the full model.
       "\code{Fsel}" is also based on a forward selection procedure, but it
       considers both cases of "\code{x}" and the flipped coding of "\code{x}"
       so it can detect protective variants as well as risk variants without
       a pre-screening test of "\code{asum}". When "\code{Fsel} is selected, 
       a weighted linear combination of rare variants is conducted, regardless
       of the choice of "\code{method}". See details.}
    \item{ad.alpha}{A significance level of a marginal association test
       to detect potential protective variants when "\code{method}" is
       "\code{asum}". Default is 0.1.}
    \item{lambda}{A tuning parameter value used for a stopping rule, when
       "\code{selection}" is "\code{forward}", "\code{backward}" or
       "\code{Fsel}". A larger value of "\code{lambda}" induces a smaller model
       selection, where false positives can be reduced down while some true
       positives are missed. Default is 0.}
       }
\details{
       The method "\code{sum}" employs a weighted linear combination of the 
       subset of \eqn{p} rare variants to combine the rare variants. The weighted
       linear combination of the \eqn{i}th individual is 
       defined as \deqn{z_i=\sum_{j=1}^p \xi_j w_j x_{ij},}
       where \eqn{\xi_j=1} if the \eqn{j}th variant is included in a model, 
       otherwise \eqn{\xi_j=0}. \eqn{w_j} is a user defined weight of the
       \eqn{j}th variant. The method "\code{asum}" replace \eqn{x_{ij}} 
       by \eqn{x_{ij}^*}, where \eqn{x_{ij}^*=1-x_{ij}} if the \eqn{j}th variant
       is potentially protective variant. Otherwise, \eqn{x_{ij}^*=x_{ij}}. 
       If the p-value of an marginal association test between the \eqn{j}th
       variant and a phenotype outcome is less than "\code{ad.alpha}" and they
       have a negative relationship, the \eqn{j}th variant is considered as 
       potentially protective. The method "\code{cmc}" combines the \eqn{p} rare
       variants such as \deqn{z_i=I\left(\sum_{j=1}^p \xi_j x_{ij} > 0\right),} 
       where \eqn{I(\cdot)} is an indicator function. \cr       
       \cr       
       The selection procedure "\code{exhaustive}" generates \eqn{2^p-1} subsets of 
       the power set of \eqn{p} rare variants, where an empty set is excluded
       since the selection procedure assumes that at least one variant is causal.        
       The best combination of rare variants among the \eqn{2^p-1} subsets
       that can maximize the association with a phenotype outcome is selected as 
       a final model. When \eqn{p} is relatively large, computational time of 
       "\code{exhaustive}" is exponentially increased. Either "\code{forward}"
       or "\code{backward}" selection is desirable for a relatively large \eqn{p}.
       \code{Fsel} is a different selection procedure from others based on a 
       weighted linear combination. It defines a weighted linear combination
       of the subset of \eqn{p} rare variants as 
       \deqn{z_i=\sum_{j=1}^p \xi_j w_j x_{ij}^*,} where 
       \eqn{\xi_j=1}, \eqn{-1} or \eqn{0} if the \eqn{j}th variant is 
       risk, protective or noncausal variant, respectively. Also, 
       \eqn{x_{ij}^*=-(1-x_{ij})} if the \eqn{j}th variant is protective, otherwise,
       \eqn{x_{ij}^*=x_{ij}}. \code{Fsel} can be performed based only on forward 
       selection procedure. 
       }
\value{
    \item{model}{Types of "\code{family}", "\code{method}" and "\code{selection}"
       used in analysis}
    \item{selection}{The selection result of \eqn{p} x 2 matrix. In the first
       column indicator values of variants are displayed where 1 for selected
       variants and 0 for unselected variants. In the second column the weights
       of individual  variants used are displayed. }
    \item{score}{The largest sample correlation between the combined genotypes
       and a phenotypic outcome, which can be replaced by a regression residual
       if a covariate exists).}
    \item{sequence}{When "\code{selection}" is "\code{forward}",
       "\code{backward}" or "\code{Fsel}", the sequence of selected variants
       are listed.     }
     }
\references{
    S. Kim, K. Lee, and H. Sun (2015)
    \emph{Statistical Selection Strategy for Risk and Protective Rare Variants
    Associated with Complex Traits}, Journal of Computational Biology 22(11),
    1034--1043 \cr
    \cr
    H. Sun and S. Wang (2014)
    \emph{A Power Set Based Statistical Selection Procedure to Locate
    Susceptible Rare Variants Associated with Complex Traits with Sequencing
    Data}, Bioinformatics 30(16), 2317--2323 \cr
    }
\author{Hokeun Sun <hsun@pusan.ac.kr>}
\examples{
   # Generate simulation data
    n <- 2000
    p <- 10
    MAF <- runif(p,0.001,0.01)
    geno.prob <- rbind((1-MAF)^2,2*(1-MAF)*MAF,MAF^2)
    x <- apply(geno.prob,2,function(x) sample(0:2,n,prob=x,replace=TRUE))
    cx <- cbind(rnorm(n),sample(0:1,n,replace=TRUE))
    beta <- c(rep(1,4),rep(0,6))
    y <- cx \%*\% c(0.5,0.5)+ x \%*\% beta+rnorm(n)
    
    # method = 'asum' and selection = 'exhaustive' 
    g <- rvsel(x,y,cx=cx)
    
    # selection = 'Fsel' 
    g <- rvsel(x,y,cx=cx,selection="Fsel")
    
   # Both risk and protective variants are present
    n <- 2000
    p <- 10
    MAF <- runif(p,0.001,0.01)
    geno.prob <- rbind((1-MAF)^2,2*(1-MAF)*MAF,MAF^2)
    x <- apply(geno.prob,2,function(x) sample(0:2,n,prob=x,replace=TRUE))
    cx <- cbind(rnorm(n),sample(0:1,n,replace=TRUE))
    beta <- c(rep(1,2),rep(-1,2), rep(0,6))
    y <- cx \%*\% c(0.5,0.5)+ x \%*\% beta+rnorm(n)
    
    # method = 'cmc' and selection = 'exhaustive' 
    g <- rvsel(x,y,cx=cx,method="cmc")
    
    # selection = 'Fsel' 
    g <- rvsel(x,y,cx=cx,selection="Fsel")
    
    # A big gene simulation 
    n <- 2000
    p <- 50
    MAF <- runif(p,0.001,0.01)
    geno.prob <- rbind((1-MAF)^2,2*(1-MAF)*MAF,MAF^2)
    x <- apply(geno.prob,2,function(x) sample(0:2,n,prob=x,replace=TRUE))
    cx <- cbind(rnorm(n),sample(0:1,n,replace=TRUE))
    beta <- c(rep(1,8),rep(0,42))
    y <- cx \%*\% c(0.5,0.5)+ x \%*\% beta+rnorm(n)
    
    # method = 'asum' and selection = 'forward' 
    \dontrun{g <- rvsel(x,y,cx=cx,selection="forward")}
    
    # selection = 'Fsel' 
    \dontrun{g <- rvsel(x,y,cx=cx,selection="Fsel", lambda=0.01)}
 }




