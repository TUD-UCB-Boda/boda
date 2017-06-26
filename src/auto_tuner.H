#ifndef BODA_AUTO_TUNER_H
#define BODA_AUTO_TUNER_H

#include"build_info.H"
#include"timers.H"
#include<boost/filesystem.hpp>
#include<boost/lexical_cast.hpp>
#include"lexp.H"
#include"rtc_func_gen.H"
#include"rtc_compute.H"
#include"cnn_op.H"
#include"op-tuner.H"
#include"conv_util.H"
#include"comp_util.H"
#include<iostream>
#include<sstream>

#include<vector>

namespace  boda
{
typedef shared_ptr< dims_t > p_dims_t;

void run_xpose( p_op_base_t const & anno_op, rtc_codegen_t & codegen, string const & xpose_func_name,
                string const &out_an_and_vn, string const &in_an_and_vn );

struct rtc_var_holder_t {
  rtc_codegen_t & codegen;
  vect_string vns;
  rtc_var_holder_t( rtc_codegen_t & codegen_ ) : codegen(codegen_) {}
  void create( string const & vn, dims_t const & dims ) { vns.push_back( vn ); return codegen.rtc->create_var_with_dims( vn, dims ); }
  ~rtc_var_holder_t( void ) {
    for( vect_string::const_iterator i = vns.begin(); i != vns.end(); ++i ) { codegen.rtc->release_var( *i ); }
    codegen.rtc->release_per_call_id_data();
    codegen.gc_clear();
  }
};

prc_ret_t profile_rcg_call( p_op_base_t const & anno_op, rtc_codegen_t & codegen,
                            p_op_base_t const & in_gen_op_orig, map_str_p_nda_t * const outs,
                            uint32_t const & run_iter, bool const & include_ins_in_outs );

p_conv_op_base_t make_p_conv_op_base_t_init_and_check_unused_from_lexp( p_lexp_t const & lexp, nesi_init_arg_t * const nia );

struct ops_be_t {
  string rtcn;
  p_rtc_compute_t rtc;
  p_rtc_codegen_t codegen;
};
typedef shared_ptr< ops_be_t > p_ops_be_t;
typedef map< string, ops_be_t > map_str_ops_be_t;

struct auto_tuner_t : virtual public nesi // NESI(help="Tools for CNN autotuning")
{
  virtual cinfo_t const * get_cinfo( void ) const; // required declaration for NESI support
  // FIXME: we should use a map_str_p_rtc_compute_t here, but NESI doesn't support that yet. might work with manual typedef initally?
  vect_p_rtc_compute_t rtcs; //help="list of compute backends to use"
  vect_string rtcns; //help="list of names of compute backends (must have same # of elements as --rtcs option"

  rtc_compile_opts_t compile_opts; //default="()",help="runtime compilation options"

  long max_smem_sz;
  long max_wg_sz;

  std::vector<op_tune_t> op_tunes; //gets filled in init()
  double best_time;
  op_tune_t best_opt;

  uint32_t run_iter = 1; //default="1",help="re-run op to profile this many times (for power testing)"

  p_op_base_t gen_data; //help="test-pattern data generation parameters (if not provided, inputs will be zeros)"

  double mrd_toler = 2e-4; //default="2e-4",help="maximum maximum-absolute-difference over which a failure is declared"
  map_str_double func_mrd_toler; //default="()",help="per-function custom maximum maximum-absolute-differences over which a failure is declared (overrides mrd_toler per-function if specified"
  uint32_t max_err = 10; //default="10",help="print at most this many differing elems"

  map_str_ops_be_t ops_bes;

  string kg_tune_tag; //help="use the tune with this tag for writing known-good digests and as the lhs for full-data live comparisons agains all tunes (inclusing itself).", req=1

  uint32_t write_kg_digest = 1; //default="1", help="if non-zero, write results of op_tune with tag equal to the value of the kg_tune_tag option as known-good nda digests into output wisdom. if zero, will copy known-good digests from input wisdom if available."

  uint32_t write_runs = 0; //default="0",help="if non-zero, write run data in output wisdom. will merge into existing runs if present (overwriting duplicates). if zero, output wisdom will have no runs (perhaps only known-good digests)"

  p_rtc_codegen_t & get_codegen_for_op_tune( op_tune_t const & op_tune ) {
    assert_st( !ops_bes.empty() );
    if( op_tune.use_be.empty() ) { return ops_bes.begin()->second.codegen; }
    return must_find( ops_bes, op_tune.use_be ).codegen;
  }

  void init();
  op_tune_t auto_tuning(nesi_init_arg_t * nia, p_conv_op_base_t anno_op);
};

p_rtc_compute_t make_p_rtc_compute_t_init_and_check_unused_from_lexp( p_lexp_t const & lexp, nesi_init_arg_t * const nia );
void on_op_err( std::ostream & out, bool & op_seen_errs, uint32_t const & op_ix, p_op_base_t const & op );

}

#endif //BODA_AUTO_TUNER_H