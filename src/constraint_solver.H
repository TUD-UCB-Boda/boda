#ifndef BODA_CONSTRAINT_SOLVER_H
#define BODA_CONSTRAINT_SOLVER_H

#include "cnn_op.H"

namespace boda {
    typedef std::vector<op_tune_t> search_space;
    typedef std::vector<uint32_t> tuning_param_vec;

    //define a constraint satisfaction problem by creating a problem_t and passing required variables and constraints
    struct convolution_solver_t {
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

        search_space get_conv_search_space();
        bool constraint_1(uint32_t Mb, uint32_t Nb);
        bool constraint_2(uint32_t Mb, uint32_t Nb, uint32_t Mt, uint32_t Nt);
        convolution_solver_t(uint64_t max_wg_sz_, uint64_t output){
            max_wg_sz = max_wg_sz_;
            max_output = next_two_exp(output);
            for(uint32_t dim = 1; dim <= max_wg_sz_; dim = dim * 2) {
                MN_vec.push_back(dim);
            }
        }
    };

}

#endif //BODA_CONSTRAINT_SOLVER_H