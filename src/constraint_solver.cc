#include"constraint_solver.H"
#include "gbt_tile.H"

namespace boda{

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

    search_space simd_convolution_solver_t::get_simd_conv_search_space() {
        search_space tuning_params;

        for(uint32_t mb : MN_vec){
            for(uint32_t nb : MN_vec){
                if(constraint_1(mb, nb)) {
                    for (uint32_t mt : MN_vec) {
                        for (uint32_t nt : MN_vec) {
                            if(constraint_2(mb, nb, mt, nt)){
                                uint32_t const out_ix_sz = no_dims.dims_prod();
                                uint32_t const pels_sz = out_ix_sz / no_dims.dsz("chan");
                                gbt_tile_t gbt;
                                gbt.init(u32_pt_t(mt,nt), mb*nb, u32_pt_t(pels_sz, no_dims.dsz("chan")));
                                for(uint32_t vw : Vw_vec){
                                    if(constraint_3(gbt.mn_per_thr.d[0], gbt.mn_per_thr.d[1], vw)){
                                        op_tune_t op_tune;
                                        op_tune.MNb.d[0] = mb;
                                        op_tune.MNb.d[1] = nb;
                                        op_tune.MNt.d[0] = mt;
                                        op_tune.MNt.d[1] = nt;

                                        op_tune.use_be = "";
                                        op_tune.use_culibs = 0;
                                        op_tune.Kb = 1;
                                        op_tune.use_local_mem = 2;
                                        op_tune.prof_variant = 0;
                                        op_tune.vw = vw;
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
            }
        }

        return tuning_params;
    }
}