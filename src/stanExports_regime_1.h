// Generated by rstantools.  Do not edit by hand.

/*
    path is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    path is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with path.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#ifndef USE_STANC3
#define USE_STANC3
#endif
#include <rstan/rstaninc.hpp>
// Code generated by stanc v2.26.1-4-gd72b68b7-dirty
#include <stan/model/model_header.hpp>
namespace model_regime_1_namespace {
inline void validate_positive_index(const char* var_name, const char* expr,
                                    int val) {
  if (val < 1) {
    std::stringstream msg;
    msg << "Found dimension size less than one in simplex declaration"
        << "; variable=" << var_name << "; dimension size expression=" << expr
        << "; expression value=" << val;
    std::string msg_str(msg.str());
    throw std::invalid_argument(msg_str.c_str());
  }
}
inline void validate_unit_vector_index(const char* var_name, const char* expr,
                                       int val) {
  if (val <= 1) {
    std::stringstream msg;
    if (val == 1) {
      msg << "Found dimension size one in unit vector declaration."
          << " One-dimensional unit vector is discrete"
          << " but the target distribution must be continuous."
          << " variable=" << var_name << "; dimension size expression=" << expr;
    } else {
      msg << "Found dimension size less than one in unit vector declaration"
          << "; variable=" << var_name << "; dimension size expression=" << expr
          << "; expression value=" << val;
    }
    std::string msg_str(msg.str());
    throw std::invalid_argument(msg_str.c_str());
  }
}
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using std::pow;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::model_base_crtp;
using stan::model::rvalue;
using stan::model::cons_list;
using stan::model::index_uni;
using stan::model::index_max;
using stan::model::index_min;
using stan::model::index_min_max;
using stan::model::index_multi;
using stan::model::index_omni;
using stan::model::nil_index_list;
using namespace stan::math;
using stan::math::pow; 
stan::math::profile_map profiles__;
static int current_statement__= 0;
static const std::vector<string> locations_array__ = {" (found before start of program)",
                                                      " (in 'regime_1', line 9, column 2 to column 12)",
                                                      " (in 'regime_1', line 10, column 2 to column 24)",
                                                      " (in 'regime_1', line 13, column 2 to column 23)",
                                                      " (in 'regime_1', line 17, column 18 to column 41)",
                                                      " (in 'regime_1', line 17, column 4 to column 41)",
                                                      " (in 'regime_1', line 16, column 9 to line 18, column 3)",
                                                      " (in 'regime_1', line 15, column 18 to column 38)",
                                                      " (in 'regime_1', line 15, column 4 to column 38)",
                                                      " (in 'regime_1', line 14, column 21 to line 16, column 3)",
                                                      " (in 'regime_1', line 14, column 2 to line 18, column 3)",
                                                      " (in 'regime_1', line 26, column 2 to column 20)",
                                                      " (in 'regime_1', line 29, column 4 to column 55)",
                                                      " (in 'regime_1', line 28, column 17 to line 30, column 3)",
                                                      " (in 'regime_1', line 28, column 2 to line 30, column 3)",
                                                      " (in 'regime_1', line 21, column 2 to column 28)",
                                                      " (in 'regime_1', line 22, column 2 to column 31)",
                                                      " (in 'regime_1', line 23, column 2 to column 29)",
                                                      " (in 'regime_1', line 2, column 2 to column 17)",
                                                      " (in 'regime_1', line 3, column 2 to column 17)",
                                                      " (in 'regime_1', line 4, column 8 to column 9)",
                                                      " (in 'regime_1', line 4, column 2 to column 20)",
                                                      " (in 'regime_1', line 5, column 2 to column 25)",
                                                      " (in 'regime_1', line 6, column 8 to column 9)",
                                                      " (in 'regime_1', line 6, column 2 to column 24)",
                                                      " (in 'regime_1', line 13, column 8 to column 9)",
                                                      " (in 'regime_1', line 26, column 9 to column 10)"};
#include <stan_meta_header.hpp>
class model_regime_1 final : public model_base_crtp<model_regime_1> {
private:
  int T;
  int K;
  std::vector<double> x_t;
  int est_sigma;
  std::vector<double> sigma_t;
 
public:
  ~model_regime_1() { }
  
  inline std::string model_name() const final { return "model_regime_1"; }
  inline std::vector<std::string> model_compile_info() const noexcept {
    return std::vector<std::string>{"stanc_version = stanc3 v2.26.1-4-gd72b68b7-dirty", "stancflags = "};
  }
  
  
  model_regime_1(stan::io::var_context& context__,
                 unsigned int random_seed__ = 0,
                 std::ostream* pstream__ = nullptr) : model_base_crtp(0) {
    using local_scalar_t__ = double ;
    boost::ecuyer1988 base_rng__ = 
        stan::services::util::create_rng(random_seed__, 0);
    (void) base_rng__;  // suppress unused var warning
    static const char* function__ = "model_regime_1_namespace::model_regime_1";
    (void) function__;  // suppress unused var warning
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      int pos__;
      pos__ = std::numeric_limits<int>::min();
      
      pos__ = 1;
      current_statement__ = 18;
      context__.validate_dims("data initialization","T","int",
          context__.to_vec());
      T = std::numeric_limits<int>::min();
      
      current_statement__ = 18;
      T = context__.vals_i("T")[(1 - 1)];
      current_statement__ = 18;
      current_statement__ = 18;
      check_greater_or_equal(function__, "T", T, 1);
      current_statement__ = 19;
      context__.validate_dims("data initialization","K","int",
          context__.to_vec());
      K = std::numeric_limits<int>::min();
      
      current_statement__ = 19;
      K = context__.vals_i("K")[(1 - 1)];
      current_statement__ = 19;
      current_statement__ = 19;
      check_greater_or_equal(function__, "K", K, 1);
      current_statement__ = 20;
      validate_non_negative_index("x_t", "T", T);
      current_statement__ = 21;
      context__.validate_dims("data initialization","x_t","double",
          context__.to_vec(T));
      x_t = std::vector<double>(T, std::numeric_limits<double>::quiet_NaN());
      
      current_statement__ = 21;
      assign(x_t, nil_index_list(), context__.vals_r("x_t"),
        "assigning variable x_t");
      current_statement__ = 22;
      context__.validate_dims("data initialization","est_sigma","int",
          context__.to_vec());
      est_sigma = std::numeric_limits<int>::min();
      
      current_statement__ = 22;
      est_sigma = context__.vals_i("est_sigma")[(1 - 1)];
      current_statement__ = 22;
      current_statement__ = 22;
      check_greater_or_equal(function__, "est_sigma", est_sigma, 0);
      current_statement__ = 23;
      validate_non_negative_index("sigma_t", "T", T);
      current_statement__ = 24;
      context__.validate_dims("data initialization","sigma_t","double",
          context__.to_vec(T));
      sigma_t = std::vector<double>(T, std::numeric_limits<double>::quiet_NaN());
      
      current_statement__ = 24;
      assign(sigma_t, nil_index_list(), context__.vals_r("sigma_t"),
        "assigning variable sigma_t");
      current_statement__ = 25;
      validate_non_negative_index("sigmas", "T", T);
      current_statement__ = 26;
      validate_non_negative_index("log_lik", "T", T);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    num_params_r__ = 0U;
    
    try {
      num_params_r__ += 1;
      num_params_r__ += 1;
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
  }
  template <bool propto__, bool jacobian__, typename VecR, typename VecI, stan::require_vector_like_t<VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline stan::scalar_type_t<VecR> log_prob_impl(VecR& params_r__,
                                                 VecI& params_i__,
                                                 std::ostream* pstream__ = nullptr) const {
    using T__ = stan::scalar_type_t<VecR>;
    using local_scalar_t__ = T__;
    T__ lp__(0.0);
    stan::math::accumulator<T__> lp_accum__;
    static const char* function__ = "model_regime_1_namespace::log_prob";
(void) function__;  // suppress unused var warning
    stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      local_scalar_t__ mu_k;
      mu_k = DUMMY_VAR__;
      
      current_statement__ = 1;
      mu_k = in__.scalar();
      local_scalar_t__ sigma_k;
      sigma_k = DUMMY_VAR__;
      
      current_statement__ = 2;
      sigma_k = in__.scalar();
      current_statement__ = 2;
      if (jacobian__) {
        current_statement__ = 2;
        sigma_k = stan::math::lb_constrain(sigma_k, 0, lp__);
      } else {
        current_statement__ = 2;
        sigma_k = stan::math::lb_constrain(sigma_k, 0);
      }
      std::vector<local_scalar_t__> sigmas;
      sigmas = std::vector<local_scalar_t__>(T, DUMMY_VAR__);
      
      current_statement__ = 10;
      if (logical_eq(est_sigma, 1)) {
        current_statement__ = 8;
        for (int i = 1; i <= T; ++i) {
          current_statement__ = 7;
          assign(sigmas, cons_list(index_uni(i), nil_index_list()), sigma_k,
            "assigning variable sigmas");}
      } else {
        current_statement__ = 5;
        for (int i = 1; i <= T; ++i) {
          current_statement__ = 4;
          assign(sigmas, cons_list(index_uni(i), nil_index_list()),
            sigma_t[(i - 1)], "assigning variable sigmas");}
      }
      {
        current_statement__ = 15;
        lp_accum__.add(student_t_lpdf<propto__>(mu_k, 3, 0, 3));
        current_statement__ = 16;
        lp_accum__.add(student_t_lpdf<propto__>(sigma_k, 3, 0, 1));
        current_statement__ = 17;
        lp_accum__.add(normal_lpdf<propto__>(x_t, mu_k, sigmas));
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    lp_accum__.add(lp__);
    return lp_accum__.sum();
    } // log_prob_impl() 
    
  template <typename RNG, typename VecR, typename VecI, typename VecVar, stan::require_vector_like_vt<std::is_floating_point, VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr, stan::require_std_vector_vt<std::is_floating_point, VecVar>* = nullptr>
  inline void write_array_impl(RNG& base_rng__, VecR& params_r__,
                               VecI& params_i__, VecVar& vars__,
                               const bool emit_transformed_parameters__ = true,
                               const bool emit_generated_quantities__ = true,
                               std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    vars__.resize(0);
    stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
    static const char* function__ = "model_regime_1_namespace::write_array";
(void) function__;  // suppress unused var warning
    (void) function__;  // suppress unused var warning
    double lp__ = 0.0;
    (void) lp__;  // dummy to suppress unused var warning
    stan::math::accumulator<double> lp_accum__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      double mu_k;
      mu_k = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 1;
      mu_k = in__.scalar();
      double sigma_k;
      sigma_k = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 2;
      sigma_k = in__.scalar();
      current_statement__ = 2;
      sigma_k = stan::math::lb_constrain(sigma_k, 0);
      std::vector<double> sigmas;
      sigmas = std::vector<double>(T, std::numeric_limits<double>::quiet_NaN());
      
      vars__.emplace_back(mu_k);
      vars__.emplace_back(sigma_k);
      if (logical_negation((primitive_value(emit_transformed_parameters__) ||
            primitive_value(emit_generated_quantities__)))) {
        return ;
      } 
      current_statement__ = 10;
      if (logical_eq(est_sigma, 1)) {
        current_statement__ = 8;
        for (int i = 1; i <= T; ++i) {
          current_statement__ = 7;
          assign(sigmas, cons_list(index_uni(i), nil_index_list()), sigma_k,
            "assigning variable sigmas");}
      } else {
        current_statement__ = 5;
        for (int i = 1; i <= T; ++i) {
          current_statement__ = 4;
          assign(sigmas, cons_list(index_uni(i), nil_index_list()),
            sigma_t[(i - 1)], "assigning variable sigmas");}
      }
      if (emit_transformed_parameters__) {
        for (int sym1__ = 1; sym1__ <= T; ++sym1__) {
          vars__.emplace_back(sigmas[(sym1__ - 1)]);}
      } 
      if (logical_negation(emit_generated_quantities__)) {
        return ;
      } 
      Eigen::Matrix<double, -1, 1> log_lik;
      log_lik = Eigen::Matrix<double, -1, 1>(T);
      stan::math::fill(log_lik, std::numeric_limits<double>::quiet_NaN());
      
      current_statement__ = 14;
      for (int n = 1; n <= T; ++n) {
        current_statement__ = 12;
        assign(log_lik, cons_list(index_uni(n), nil_index_list()),
          normal_lpdf<false>(x_t[(n - 1)], mu_k, sigmas[(n - 1)]),
          "assigning variable log_lik");}
      for (int sym1__ = 1; sym1__ <= T; ++sym1__) {
        vars__.emplace_back(log_lik[(sym1__ - 1)]);}
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    } // write_array_impl() 
    
  template <typename VecVar, typename VecI, stan::require_std_vector_t<VecVar>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline void transform_inits_impl(const stan::io::var_context& context__,
                                   VecI& params_i__, VecVar& vars__,
                                   std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    vars__.clear();
    vars__.reserve(num_params_r__);
    
    try {
      int pos__;
      pos__ = std::numeric_limits<int>::min();
      
      pos__ = 1;
      double mu_k;
      mu_k = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 1;
      mu_k = context__.vals_r("mu_k")[(1 - 1)];
      double sigma_k;
      sigma_k = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 2;
      sigma_k = context__.vals_r("sigma_k")[(1 - 1)];
      double sigma_k_free__;
      sigma_k_free__ = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 2;
      sigma_k_free__ = stan::math::lb_free(sigma_k, 0);
      vars__.emplace_back(mu_k);
      vars__.emplace_back(sigma_k_free__);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    } // transform_inits_impl() 
    
  inline void get_param_names(std::vector<std::string>& names__) const {
    
    names__.clear();
    names__.emplace_back("mu_k");
    names__.emplace_back("sigma_k");
    names__.emplace_back("sigmas");
    names__.emplace_back("log_lik");
    } // get_param_names() 
    
  inline void get_dims(std::vector<std::vector<size_t>>& dimss__) const {
    dimss__.clear();
    dimss__.emplace_back(std::vector<size_t>{});
    
    dimss__.emplace_back(std::vector<size_t>{});
    
    dimss__.emplace_back(std::vector<size_t>{static_cast<size_t>(T)});
    
    dimss__.emplace_back(std::vector<size_t>{static_cast<size_t>(T)});
    
    } // get_dims() 
    
  inline void constrained_param_names(
                                      std::vector<std::string>& param_names__,
                                      bool emit_transformed_parameters__ = true,
                                      bool emit_generated_quantities__ = true) const
    final {
    
    param_names__.emplace_back(std::string() + "mu_k");
    param_names__.emplace_back(std::string() + "sigma_k");
    if (emit_transformed_parameters__) {
      for (int sym1__ = 1; sym1__ <= T; ++sym1__) {
        {
          param_names__.emplace_back(std::string() + "sigmas" + '.' + std::to_string(sym1__));
        }}
    }
    
    if (emit_generated_quantities__) {
      for (int sym1__ = 1; sym1__ <= T; ++sym1__) {
        {
          param_names__.emplace_back(std::string() + "log_lik" + '.' + std::to_string(sym1__));
        }}
    }
    
    } // constrained_param_names() 
    
  inline void unconstrained_param_names(
                                        std::vector<std::string>& param_names__,
                                        bool emit_transformed_parameters__ = true,
                                        bool emit_generated_quantities__ = true) const
    final {
    
    param_names__.emplace_back(std::string() + "mu_k");
    param_names__.emplace_back(std::string() + "sigma_k");
    if (emit_transformed_parameters__) {
      for (int sym1__ = 1; sym1__ <= T; ++sym1__) {
        {
          param_names__.emplace_back(std::string() + "sigmas" + '.' + std::to_string(sym1__));
        }}
    }
    
    if (emit_generated_quantities__) {
      for (int sym1__ = 1; sym1__ <= T; ++sym1__) {
        {
          param_names__.emplace_back(std::string() + "log_lik" + '.' + std::to_string(sym1__));
        }}
    }
    
    } // unconstrained_param_names() 
    
  inline std::string get_constrained_sizedtypes() const {
    stringstream s__;
    s__ << "[{\"name\":\"mu_k\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"sigma_k\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"sigmas\",\"type\":{\"name\":\"array\",\"length\":" << T << ",\"element_type\":{\"name\":\"real\"}},\"block\":\"transformed_parameters\"},{\"name\":\"log_lik\",\"type\":{\"name\":\"vector\",\"length\":" << T << "},\"block\":\"generated_quantities\"}]";
    return s__.str();
    } // get_constrained_sizedtypes() 
    
  inline std::string get_unconstrained_sizedtypes() const {
    stringstream s__;
    s__ << "[{\"name\":\"mu_k\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"sigma_k\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"sigmas\",\"type\":{\"name\":\"array\",\"length\":" << T << ",\"element_type\":{\"name\":\"real\"}},\"block\":\"transformed_parameters\"},{\"name\":\"log_lik\",\"type\":{\"name\":\"vector\",\"length\":" << T << "},\"block\":\"generated_quantities\"}]";
    return s__.str();
    } // get_unconstrained_sizedtypes() 
    
  
    // Begin method overload boilerplate
    template <typename RNG>
    inline void write_array(RNG& base_rng,
                            Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                            Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                            const bool emit_transformed_parameters = true,
                            const bool emit_generated_quantities = true,
                            std::ostream* pstream = nullptr) const {
      std::vector<double> vars_vec(vars.size());
      std::vector<int> params_i;
      write_array_impl(base_rng, params_r, params_i, vars_vec,
          emit_transformed_parameters, emit_generated_quantities, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i) {
        vars.coeffRef(i) = vars_vec[i];
      }
    }
    template <typename RNG>
    inline void write_array(RNG& base_rng, std::vector<double>& params_r,
                            std::vector<int>& params_i,
                            std::vector<double>& vars,
                            bool emit_transformed_parameters = true,
                            bool emit_generated_quantities = true,
                            std::ostream* pstream = nullptr) const {
      write_array_impl(base_rng, params_r, params_i, vars, emit_transformed_parameters, emit_generated_quantities, pstream);
    }
    template <bool propto__, bool jacobian__, typename T_>
    inline T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
                       std::ostream* pstream = nullptr) const {
      Eigen::Matrix<int, -1, 1> params_i;
      return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
    }
    template <bool propto__, bool jacobian__, typename T__>
    inline T__ log_prob(std::vector<T__>& params_r,
                        std::vector<int>& params_i,
                        std::ostream* pstream = nullptr) const {
      return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
    }
  
    inline void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream = nullptr) const final {
      std::vector<double> params_r_vec(params_r.size());
      std::vector<int> params_i;
      transform_inits_impl(context, params_i, params_r_vec, pstream);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i) {
        params_r.coeffRef(i) = params_r_vec[i];
      }
    }
    inline void transform_inits(const stan::io::var_context& context,
                                std::vector<int>& params_i,
                                std::vector<double>& vars,
                                std::ostream* pstream = nullptr) const final {
      transform_inits_impl(context, params_i, vars, pstream);
    }        
};
}
using stan_model = model_regime_1_namespace::model_regime_1;
#ifndef USING_R
// Boilerplate
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
stan::math::profile_map& get_stan_profile_data() {
  return model_regime_1_namespace::profiles__;
}
#endif
#endif
