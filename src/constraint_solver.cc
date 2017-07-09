#include"constraint_solver.H"

namespace boda{
    bool convolution_solver_t::constraint_1(uint32_t Mb, uint32_t Nb) {
        bool b1 = ((Mb * Nb) % 16) == 0;
        bool b2 = (Mb * Nb) <= max_wg_sz;
        return (b1 & b2);
    }

    bool convolution_solver_t::constraint_2(uint32_t Mb, uint32_t Nb, uint32_t Mt, uint32_t Nt) {
        uint64_t total_output = Mb * Nb * Mt * Nt;
        bool b1 = total_output <= max_output;
        bool b2 = (Mt * Nt) <= (Mb * Nb);
        return (b1 & b2);
    }

    search_space convolution_solver_t::get_conv_search_space() {
        search_space tuning_params;

        for(uint32_t mb : MN_vec){
            for(uint32_t nb : MN_vec){
                if(constraint_1(mb, nb)) {
                    for (uint32_t mt : MN_vec) {
                        for (uint32_t nt : MN_vec) {
                            if(constraint_2(mb, nb, mt, nt)){
                                op_tune_t op_tune;
                                op_tune.MNb.d[0] = mb;
                                op_tune.MNb.d[1] = nb;
                                op_tune.MNt.d[0] = mt;
                                op_tune.MNt.d[1] = nt;

                                op_tune.use_be = "";
                                op_tune.use_culibs = 0;
                                op_tune.Kb = 8;
                                op_tune.use_local_mem = 1;
                                op_tune.prof_variant = 0;
                                op_tune.vw = 8;
                                op_tune.k1conv = 0;
                                op_tune.tconv = 0;
                                op_tune.tconv_max_ksz.d[0] = 11;
                                op_tune.tconv_max_ksz.d[1] = 11;
                                op_tune.ipconv = 0;
                                tuning_params.push_back(op_tune);
                            }
                        }
                    }
                }
            }
        }

        return tuning_params;
    }
}