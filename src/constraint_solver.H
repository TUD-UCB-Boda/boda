#ifndef BODA_CONSTRAINT_SOLVER_H
#define BODA_CONSTRAINT_SOLVER_H

#include "cnn_op.H"

namespace boda {
    typedef std::vector<op_tune_t> search_space;
    typedef std::vector<uint32_t> tuning_param_vec;

    struct solver_t {
        uint64_t max_wg_sz;
        uint64_t max_output;
        tuning_param_vec MN_vec;

        uint64_t next_two_exp(uint64_t output){
            uint64_t exp = 1;
            while(exp <= output) {
                exp = exp * 2;
            }
            return exp;
        }

        bool constraint_1(uint32_t Mb, uint32_t Nb) {
          bool b1 = ((Mb * Nb) % 16) == 0;
          bool b2 = (Mb * Nb) <= max_wg_sz;
          return (b1 & b2);
        }

        bool constraint_2(uint32_t Mb, uint32_t Nb, uint32_t Mt, uint32_t Nt) {
          uint64_t total_output = Mb * Nb * Mt * Nt;
          bool b1 = total_output <= max_output;
          bool b2 = (Mt * Nt) <= (Mb * Nb);
          return (b1 & b2);
        }

        bool constraint_3(uint32_t pels, uint32_t out_chan, uint32_t vw){
          bool b1 = (pels % vw == 0);
          bool b2 = (out_chan % vw == 0);
          return (b1 & b2);
        }
    };

    //define a constraint satisfaction problem by creating a problem_t and passing required variables and constraints
    struct convolution_solver_t : solver_t{
        search_space get_conv_search_space();
        convolution_solver_t(uint64_t max_wg_sz_, uint64_t output){
            max_wg_sz = max_wg_sz_;
            max_output = next_two_exp(output);
            for(uint32_t dim = 1; dim <= max_wg_sz_; dim = dim * 2) {
                MN_vec.push_back(dim);
            }
        }
    };

    struct simd_convolution_solver_t :solver_t {
        dims_t no_dims;
        tuning_param_vec Vw_vec;

        search_space get_simd_conv_search_space();
        simd_convolution_solver_t(uint64_t max_wg_sz_, uint64_t output, dims_t no_dims_){
          no_dims = no_dims_;
          max_wg_sz = max_wg_sz_;
          max_output = next_two_exp(output);
          for(uint32_t dim = 1; dim <= max_wg_sz_; dim = dim * 2) {
            MN_vec.push_back(dim);
          }
          Vw_vec.push_back(2);
          Vw_vec.push_back(4);
          Vw_vec.push_back(8);
          Vw_vec.push_back(16);
        }
    };

}

#endif //BODA_CONSTRAINT_SOLVER_H