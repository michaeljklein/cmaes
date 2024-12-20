//! Types related to matrix math.

use nalgebra::base::VecStorage;
use nalgebra::Dyn;

pub type SquareMatrix<T> = nalgebra::SquareMatrix<T, Dyn, VecStorage<T, Dyn, Dyn>>;

/// A symmetric square matrix that stores and updates its eigendecomposition and inverse square root
/// (`C^(-1/2)`)
#[derive(Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CovarianceMatrix {
    /// Covariance matrix
    cov: SquareMatrix<f64>,
    /// Normalized eigenvectors, forming an orthonormal basis of the matrix (`B`)
    eigenvectors: SquareMatrix<f64>,
    /// Diagonal matrix containing the square roots of the eigenvalues, which are the
    /// scales of the basis axes (`D`)
    sqrt_eigenvalues: SquareMatrix<f64>,
    /// The inverse square root of the matrix (`C^(-1/2)`)
    sqrt_inv: SquareMatrix<f64>,
    /// The transform to the normal distribution represented by the matrix (`B * D`)
    transform: SquareMatrix<f64>,
}

impl CovarianceMatrix {
    /// Returns an identity `CovarianceMatrix`
    pub fn new(dim: usize) -> Self {
        Self {
            cov: SquareMatrix::identity(dim, dim),
            eigenvectors: SquareMatrix::identity(dim, dim),
            sqrt_eigenvalues: SquareMatrix::identity(dim, dim),
            sqrt_inv: SquareMatrix::identity(dim, dim),
            transform: SquareMatrix::identity(dim, dim),
        }
    }

    pub fn cov(&self) -> &SquareMatrix<f64> {
        &self.cov
    }

    /// Updates the covariance matrix, symmetrizes it, and updates the eigendecomposition if
    /// `update_eigen` is true
    ///
    /// Returns `Err` if the matrix is not positive-definite
    pub fn set_cov(
        &mut self,
        new: SquareMatrix<f64>,
        update_eigen: bool,
    ) -> Result<(), PosDefCovError> {
        self.cov = new;
        // Ensure symmetry
        self.cov.fill_lower_triangle_with_upper_triangle();

        if update_eigen {
            self.update_eigendecomposition()?;
        }

        Ok(())
    }

    /// Updates the eigendecomposition
    ///
    /// Returns `Err` if the matrix is not positive-definite
    fn update_eigendecomposition(&mut self) -> Result<(), PosDefCovError> {
        #[cfg(any(
            feature = "openblas",
            feature = "netlib",
            feature = "accelerate",
            feature = "intel-mkl"
        ))]
        let mut eigen =
            nalgebra_lapack::SymmetricEigen::try_new(self.cov.clone()).ok_or(PosDefCovError)?;

        #[cfg(not(any(
            feature = "openblas",
            feature = "netlib",
            feature = "accelerate",
            feature = "intel-mkl"
        )))]
        let mut eigen =
            nalgebra::SymmetricEigen::try_new(self.cov.clone(), 1e-20, 0).ok_or(PosDefCovError)?;

        for mut col in eigen.eigenvectors.column_iter_mut() {
            col.normalize_mut();
        }

        if eigen.eigenvalues.iter().any(|x| *x <= 0.0) {
            return Err(PosDefCovError);
        }

        self.eigenvectors = eigen.eigenvectors;
        self.sqrt_eigenvalues = SquareMatrix::from_diagonal(&eigen.eigenvalues.map(|x| x.sqrt()));
        self.sqrt_inv = &self.eigenvectors
            * self
                .sqrt_eigenvalues
                .map(|d| if d > 0.0 { 1.0 / d } else { d })
            * self.eigenvectors.transpose();
        self.transform = &self.eigenvectors * &self.sqrt_eigenvalues;

        Ok(())
    }

    pub fn eigenvectors(&self) -> &SquareMatrix<f64> {
        &self.eigenvectors
    }

    pub fn sqrt_eigenvalues(&self) -> &SquareMatrix<f64> {
        &self.sqrt_eigenvalues
    }

    pub fn sqrt_inv(&self) -> &SquareMatrix<f64> {
        &self.sqrt_inv
    }

    /// Returns the transform of the matrix (`B * D`)
    pub fn transform(&self) -> &SquareMatrix<f64> {
        &self.transform
    }
}

/// The covariance matrix is not positive definite
#[derive(Clone, Debug)]
pub struct PosDefCovError;

#[cfg(test)]
mod tests {
    use assert_approx_eq::assert_approx_eq;

    use super::*;

    #[test]
    fn test_update_eigendecomposition() {
        let mut cov = CovarianceMatrix::new(2);
        cov.set_cov(
            SquareMatrix::from_iterator(2, 2, [3.0, 1.5, 1.5, 2.0]),
            false,
        )
        .unwrap();

        // The eigendecomposition hasn't been updated yet
        assert_eq!(cov.eigenvectors, SquareMatrix::identity(2, 2));
        assert_eq!(cov.sqrt_eigenvalues, SquareMatrix::identity(2, 2));

        cov.update_eigendecomposition().unwrap();

        let reconstructed =
            cov.eigenvectors.clone() * cov.sqrt_eigenvalues.pow(2) * cov.eigenvectors.transpose();

        for x in (reconstructed - &cov.cov).iter() {
            assert_approx_eq!(x, 0.0);
        }

        // Non-positive-definite matrices should return Err
        cov.set_cov(
            SquareMatrix::from_iterator(2, 2, [3.0, 5.0, 5.0, 2.0]),
            false,
        )
        .unwrap();
        assert!(cov.update_eigendecomposition().is_err());
    }
}
