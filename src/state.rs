//! Variable state of the algorithm and updating of that state.

use nalgebra::DVector;

use crate::matrix::{CovarianceMatrix, PosDefCovError, SquareMatrix};
use crate::parameters::Parameters;
use crate::sampling::EvaluatedPoint;
use rayon::prelude::*;

/// Stores the variable state of the algorithm and handles updating it
#[derive(Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct State {
    /// The number of generations that have been fully completed
    generation: usize,
    /// The distribution mean
    mean: DVector<f64>,
    /// The distribution covariance matrix
    cov: CovarianceMatrix,
    /// The distribution step size
    sigma: f64,
    /// Evolution path of the mean used to update the covariance matrix
    path_c: DVector<f64>,
    /// Evolution path of the mean used to update the step size
    path_sigma: DVector<f64>,
    /// The last time the eigendecomposition was updated, in function evals
    last_eigen_update_evals: usize,
}

impl State {
    /// Initializes the variable state of the algorithm
    pub fn new(initial_mean: DVector<f64>, initial_sigma: f64) -> Self {
        let dim = initial_mean.len();
        let mean = initial_mean;
        let cov = CovarianceMatrix::new(dim);
        let sigma = initial_sigma;
        let path_c = DVector::zeros(dim);
        let path_sigma = DVector::zeros(dim);

        Self {
            generation: 0,
            mean,
            cov,
            sigma,
            path_c,
            path_sigma,
            last_eigen_update_evals: 0,
        }
    }

    /// Updates the variable state using the provided sampled individuals
    pub fn update(
        &mut self,
        current_function_evals: usize,
        params: &Parameters,
        individuals: &[EvaluatedPoint],
    ) -> Result<(), PosDefCovError> {
        let dim = params.dim();
        let mu = params.mu();
        let mu_eff = params.mu_eff();
        let cc = params.cc();
        let c1 = params.c1();
        let cs = params.cs();
        let cmu = params.cmu();
        let cm = params.cm();
        let damp_s = params.damp_s();

        // Calculate new mean through weighted recombination
        // Only the mu best individuals are used even if there are lambda weights
        let yw = individuals
            .iter()
            .take(mu)
            .enumerate()
            .map(|(i, p)| p.unscaled_step() * params.weights()[i])
            .sum::<DVector<f64>>();
        self.mean = &self.mean + &(cm * self.sigma * &yw);

        // Update evolution paths
        let sqrt_inv_c = self.cov.sqrt_inv();

        self.path_sigma =
            (1.0 - cs) * &self.path_sigma + (cs * (2.0 - cs) * mu_eff).sqrt() * sqrt_inv_c * &yw;

        // Expectation of N(0, I)
        let chi_n = (dim as f64).sqrt()
            * (1.0 - 1.0 / (4.0 * dim as f64) + 1.0 / (21.0 * dim.pow(2) as f64));

        let hs = if (self.path_sigma.magnitude()
            / (1.0 - (1.0 - cs).powi(2 * (self.generation as i32 + 1))).sqrt())
            < (1.4 + 2.0 / (dim as f64 + 1.0)) * chi_n
        {
            1.0
        } else {
            0.0
        };

        self.path_c = (1.0 - cc) * &self.path_c + hs * (cc * (2.0 - cc) * mu_eff).sqrt() * &yw;

        // Update step size
        self.sigma *= ((cs / damp_s) * ((self.path_sigma.magnitude() / chi_n) - 1.0)).exp();

        // Update covariance matrix

        // Calculates the weighted contribution of each individual to the rank-mu update
        let map_weights = |(i, w): (usize, f64)| {
            // Scale negative weights to maintain positive definiteness of cov
            let mut wc = w;
            if w < 0.0 {
                wc *= dim as f64
                    / (sqrt_inv_c * individuals[i].unscaled_step())
                        .magnitude()
                        .powi(2);
            }

            wc * individuals[i].unscaled_step() * individuals[i].unscaled_step().transpose()
        };
        let rank_mu_update = if params.parallel_update() {
            rank_mu_update_parallel(params.weights().as_slice(), map_weights, || {
                SquareMatrix::zeros(dim, dim)
            })
        } else {
            rank_mu_update(params.weights().as_slice(), map_weights)
        };

        let delta_hs = (1.0 - hs) * cc * (2.0 - cc);
        let cov_new = (1.0 + c1 * delta_hs - c1 - cmu * params.weights().iter().sum::<f64>())
            * self.cov.cov()
            + c1 * &self.path_c * self.path_c.transpose()
            + cmu * rank_mu_update;

        // Update eigendecomposition occasionally (updating every generation is unnecessary and
        // inefficient for high dim)
        let evals_per_eigen = self.evals_per_eigen_update(params);
        let do_eigen_update =
            current_function_evals >= self.last_eigen_update_evals + evals_per_eigen;

        self.cov.set_cov(cov_new, do_eigen_update)?;

        if do_eigen_update {
            self.last_eigen_update_evals = current_function_evals;
        }

        self.generation += 1;

        Ok(())
    }

    pub fn generation(&self) -> usize {
        self.generation
    }

    pub fn mean(&self) -> &DVector<f64> {
        &self.mean
    }

    pub fn cov(&self) -> &SquareMatrix<f64> {
        self.cov.cov()
    }

    pub fn cov_eigenvectors(&self) -> &SquareMatrix<f64> {
        self.cov.eigenvectors()
    }

    pub fn cov_sqrt_eigenvalues(&self) -> &SquareMatrix<f64> {
        self.cov.sqrt_eigenvalues()
    }

    /// Returns the transform of the covariance matrix (`B * D`)
    pub fn cov_transform(&self) -> &SquareMatrix<f64> {
        self.cov.transform()
    }

    /// Returns the current axis ratio of the distribution
    pub fn axis_ratio(&self) -> f64 {
        let diag = self.cov.sqrt_eigenvalues().diagonal();
        diag.max() / diag.min()
    }

    pub fn sigma(&self) -> f64 {
        self.sigma
    }

    pub fn path_c(&self) -> &DVector<f64> {
        &self.path_c
    }

    /// Returns how many function evals should pass before updating the eigendecomposition
    pub fn evals_per_eigen_update(&self, params: &Parameters) -> usize {
        (0.5 * params.dim() as f64 * params.lambda() as f64
            / ((params.c1() + params.cmu()) * params.dim().pow(2) as f64))
            .ceil() as usize
    }

    // These methods are only used for setting up termination tests
    #[cfg(test)]
    pub fn mut_generation(&mut self) -> &mut usize {
        &mut self.generation
    }

    #[cfg(test)]
    pub fn mut_cov(&mut self) -> &mut CovarianceMatrix {
        &mut self.cov
    }

    #[cfg(test)]
    pub fn mut_mean(&mut self) -> &mut DVector<f64> {
        &mut self.mean
    }

    #[cfg(test)]
    pub fn mut_sigma(&mut self) -> &mut f64 {
        &mut self.sigma
    }
}

/// Calculates the rank-mu update term (ignoring cmu)
fn rank_mu_update<F>(weights: &[f64], map_weights: F) -> SquareMatrix<f64>
where
    F: Fn((usize, f64)) -> SquareMatrix<f64>,
{
    weights.iter().cloned().enumerate().map(map_weights).sum()
}

/// Like `rank_mu_update`, but uses parallel iterators instead
fn rank_mu_update_parallel<F, I>(weights: &[f64], map_weights: F, identity: I) -> SquareMatrix<f64>
where
    F: Send + Sync + Fn((usize, f64)) -> SquareMatrix<f64>,
    I: Send + Sync + Fn() -> SquareMatrix<f64>,
{
    weights
        .par_iter()
        .cloned()
        .enumerate()
        .map(map_weights)
        .reduce(identity, |a, b| a + b)
}
