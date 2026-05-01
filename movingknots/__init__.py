"""Python/JAX implementation of moving knots surface regression."""

__version__ = "0.0.0"

from movingknots.basis import design_matrix, thinplate_additive_basis, thinplate_surface_basis
from movingknots.data import data_partition, make_knots, set_crossvalid, std_data
from movingknots.fit import (
    cross_validate_gaussian_vi,
    evaluate_gaussian_fit,
    fit_fixed_knots_gaussian_vi,
    fit_full_gaussian_vi,
    fit_gaussian_vi,
    fit_free_additive_knots_gaussian_vi,
    fit_free_surface_knots_gaussian_vi,
    fit_marginal_gaussian_vi,
    full_gaussian_log_prob,
    full_gaussian_log_prob_components,
    gaussian_log_predictive_samples,
    gaussian_lpds,
    marginal_fit_beta_posterior,
    marginal_fit_log_predictive_density,
    marginal_fit_predictive_moments,
    marginal_gaussian_log_prob,
    marginal_gaussian_log_prob_components,
    predict_mean,
    predict_samples,
    predict_summary,
    summarize_log_predictive_samples,
    summarize_fit,
)
from movingknots.model import (
    coefficient_prior_covariance,
    conditional_gaussian_beta_posterior,
    gaussian_log_likelihood,
    log_joint,
    make_p_matrices,
    marginal_gaussian_log_likelihood,
)
from movingknots.parameters import (
    GaussianParameterSchema,
    MarginalGaussianParameterSchema,
    build_gaussian_parameter_schema,
    build_marginal_gaussian_parameter_schema,
    pack_gaussian_parameters,
    pack_marginal_gaussian_parameters,
    unpack_gaussian_parameters,
    unpack_marginal_gaussian_parameters,
)
from movingknots.priors import inverse_wishart_logpdf, log_prior, normal_logpdf
from movingknots.utils import rdist, vech, vech_to_matrix
from movingknots.variational import (
    elbo,
    fit_mean_field,
    init_mean_field,
    mean_field_logpdf,
    sample_mean_field,
)
