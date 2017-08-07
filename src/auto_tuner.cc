#include"nesi.H"
#include"has_main.H"
#include"CL/cl.h"
#include<stdio.h>
#include"gbt_tile.H"
#include"auto_tuner.H"
#include"constraint_solver.H"

namespace  boda{
  void run_xpose( p_op_base_t const & anno_op, rtc_codegen_t & codegen, string const & xpose_func_name,
                  string const &out_an_and_vn, string const &in_an_and_vn )  {
    p_rcg_func_call_t rfc = codegen.gen_func_override_func_name( xpose_func_name, *anno_op,
                                                                 map_str_rtc_arg_t{{out_an_and_vn,out_an_and_vn},{in_an_and_vn,in_an_and_vn}});
    codegen.run_func( *rfc );
  }



  prc_ret_t profile_rcg_call( p_op_base_t const & anno_op, rtc_codegen_t & codegen,
                              p_op_base_t const & in_gen_op_orig, map_str_p_nda_t * const outs,
                              uint32_t const & run_iter, bool const & include_ins_in_outs )
  {
    rtc_var_holder_t rvh( codegen );
    timer_t t("profile_rcg_call");
    string const anno_op_func_name = anno_op->get_func_name();
    p_rcg_func_call_t rfc = codegen.gen_func( *anno_op, map_str_rtc_arg_t() ); // FIXME: not passing in args here. yet?
    p_rtc_call_gen_t const & rcg = rfc->rcg;
    map_str_rtc_arg_t & arg_map = rfc->arg_map;
    for( vect_arg_decl_t::multi_iter i = rcg->rtc_func_template->arg_decls.multi_begin( &rcg->op ); !i.at_end(); ++i ) {
      if( i.ad().io_type == "REF" ) {
        continue;
      }
      //if( i.vn() == "cucl_arg_info" ) { continue; } // FIXME: not-too-nice special case for cucl_arg_info argument
      if( i.ad().loi.v == 0 ) { // FIXME: not-too-nice special case for flags
        if( i.vn() == "flags" ) {
          must_insert( arg_map, "flags", make_scalar_nda(uint32_t(0)) );
          continue;
        }
      }
      dims_t const & func_dims = rcg->get_arg_dims_by_name( i.vn() );
      if( func_dims == make_null_dims_t() ) {
        continue;
      } // NULL case -- ignore
      // FIXME: overwrite dims. yeah, this doesn't feel too right ... hmm. see comments in gen_func()
      arg_map[ i.vn() ] = i.vn();
    }
    // FIXME: horrible: some kernels take a scalar uint32_t flags, and we know 0 is 'normal-mode'. so we set it here,
    // for all ops, and hope that's okay.

    //printf( "run: i->rtc_func_name=%s\n", str(rcg->gen_fn).c_str() );
    for( map_str_rtc_arg_t::const_iterator j = arg_map.begin(); j != arg_map.end(); ++j ) {
      if( j->second.is_var() ) {
        rvh.create( j->second.n, anno_op->get_dims( j->first ) );
      }
    }
    if( in_gen_op_orig ) {
      for( vect_arg_decl_t::multi_iter i = rcg->rtc_func_template->arg_decls.multi_begin( &rcg->op ); !i.at_end(); ++i ) {
        p_op_base_t in_gen_op = make_shared<op_base_t>( *in_gen_op_orig );
        if( i.ad().io_type != "IN" ) {
          continue;
        }
        //if( i.vn() == "cucl_arg_info" ) { continue; } // FIXME: not-too-nice special case for cucl_arg_info argument
        if( i.ad().loi.v == 0 ) {
          continue;
        } // FIXME: not-too-nice special case for scalars ... better be const.
        // note: gen_data variant choice based on gen type and op type (*not* op func_name)
        in_gen_op->set_func_name( in_gen_op->get_type()+"_"+anno_op->get_type()+"_"+i.vn() );
        dims_t const & in_dims = anno_op->get_dims( i.vn() );
        string const ref_in_dims_name = i.vn()+"_ref";
        dims_t const & ref_in_dims = anno_op->has(ref_in_dims_name)?anno_op->get_dims(ref_in_dims_name):in_dims;
        in_gen_op->set_dims( i.vn(), ref_in_dims );
        string gen_vn = i.vn();
        if( in_dims != ref_in_dims ) {
          gen_vn += "_ref";
          rvh.create( gen_vn, ref_in_dims );
        }
        p_rcg_func_call_t rfc_in_gen = codegen.gen_func( *in_gen_op, map_str_rtc_arg_t{{i.vn(),gen_vn}} );
        codegen.run_func( *rfc_in_gen );
        // check if xpose needed:
        if( include_ins_in_outs && outs ) {
          must_insert( *outs, gen_vn, codegen.rtc->create_nda_from_var( gen_vn ) );
        }
        if( gen_vn != i.vn() ) {
          // FIXME: some ugly, cut-n-paste, brittle stuff here ... but it's pending more global cleanup.
          string xpose_op = anno_op_func_name+"_xpose_"+i.vn();
          // FIXME: sigh.
          if( ( i.vn() == "filts" ) && is_k1_or_t_or_reg_conv(anno_op->get_func_name())) {
            xpose_op = "xpose_filts";
          }
          run_xpose( anno_op, codegen, xpose_op, gen_vn, i.vn() );
          if( include_ins_in_outs && outs ) {
            must_insert( *outs, i.vn(), codegen.rtc->create_nda_from_var( i.vn() ) );
          }
        }
      }
    }

    uint32_t call_id = uint32_t_const_max;
    prc_ret_t ret{make_shared<op_base_t>(rfc->rcg->op), NAN};

    for( uint32_t i = 0; i != run_iter; ++i ) {
      call_id = codegen.run_func( *rfc );
    }

    // FIXME: xpose of OUTs is semi-dup'd with "IN"/gen_data handling above
    for( vect_arg_decl_t::multi_iter i = rcg->rtc_func_template->arg_decls.multi_begin( &rcg->op ); !i.at_end(); ++i ) {
      if( !endswith( i.ad().io_type, "OUT" ) ) { continue; }
      dims_t const & out_dims = anno_op->get_dims( i.vn() );
      string const ref_out_dims_name = i.vn()+"_ref";
      dims_t const & ref_out_dims = anno_op->has(ref_out_dims_name)?anno_op->get_dims(ref_out_dims_name):out_dims;
      string gen_vn = i.vn();
      if( out_dims != ref_out_dims ) {
        gen_vn += "_ref";
        rvh.create( gen_vn, ref_out_dims );
      }
      if( gen_vn != i.vn() ) {
        run_xpose( anno_op, codegen, anno_op_func_name+"_xpose_"+i.vn(), gen_vn, i.vn() );
      }
      if( outs ) {
        must_insert( *outs, i.vn(), codegen.rtc->create_nda_from_var( gen_vn ) );
      }
    }
    codegen.rtc->finish_and_sync();
    double const rfc_dur = codegen.rtc->get_dur( call_id, call_id ); // get call duration in msecs
    ret.rt_secs = rfc_dur / 1000.0; // convert msecs to secs
    return ret;
  }


  void on_op_err( std::ostream & out, bool & op_seen_errs, uint32_t const & op_ix, p_op_base_t const & op ) {
    // if first err for this op, print out op
    if( !op_seen_errs ) {
      out << "-----\n errors for op_ix=" << str(op_ix) << " op='" << str( op ) << "'\n";
      op_seen_errs = 1;
    }
  }

  void auto_tuner_t::init(string rtc_be, nesi_init_arg_t * nia, op_tune_t kg_op_tune_t_) {
    rtc = make_p_rtc_compute_t_init_and_check_unused_from_lexp( parse_lexp( rtc_be ), nia ); //FIXME: currently passing 0, but has to be NESI
    rtc->init();
    codegen = make_shared<rtc_codegen_t>();
    codegen->init( rtc, make_cnn_custom_codegen_t(), compile_opts );
    dev_info = rtc->get_device_info();

    bool const enable_prof = 0;

    op_tunes.push_back(kg_op_tune_t_);
  }

  op_tune_t auto_tuner_t::auto_tuning(p_conv_op_base_t anno_op, uint32_t print) {
    //set up search space for tuning parameters
    dims_t in_dims = anno_op->get_dims("in");
    uint32_t in_x = (in_dims.get_dim_by_name("x"))->sz;
    uint32_t in_y = (in_dims.get_dim_by_name("y"))->sz;
    convolution_solver_t conv_solver = convolution_solver_t(dev_info.wg_sz, in_x * in_y);
    search_space space = conv_solver.get_conv_search_space();
    op_tunes.insert(op_tunes.end(), space.begin(), space.end());

    int comp_errs = 0;

    p_ostream out = p_ostream( &std::cout, null_deleter<std::ostream>() );
    p_ostream wout = p_ostream();
    p_istream win;

    uint32_t num_mad_fail = 0;
    uint32_t kg_wix = uint32_t_const_max;

    //tune and profile operation
    bool op_seen_errs = 0; // we only print out op info if there are errors, and then only once across all tunes.
    p_op_wisdom_t op_wisdom_in = win ? read_next_wisdom( win ) : p_op_wisdom_t();
    p_op_wisdom_t op_wisdom_out = make_shared< op_wisdom_t >();

    p_map_str_p_nda_t vs_kg; // we compare all runs against the known-good run, whose results will be stored here
    std::ostringstream tuning_info;

    //profile each tuning parameter option we want to try, runs 2 times, one for known good and one for test_op_tune
    for( int wix = 0; wix < op_tunes.size(); ++wix ) { //FIXME: pass multiple op_tunes instead of just one
      p_conv_op_base_t op_copy = std::make_shared<conv_op_base_t>(*anno_op); //we have to make a copy so anno_op doesn't get annotated twice
      op_tune_t op_tune;
      op_tune = op_tunes.at(wix); //get op_tune from op_tunes we want to use

      string const plat_tag = codegen->rtc->get_plat_tag();
      p_map_str_p_nda_t vsi;
      prc_ret_t prc_ret{0,NAN};
      std::ostringstream err; // one-line-only, goes to wisdom and text output; non-empty if any error
      std::ostringstream err_extra; // can be multi-line, goes to only to text output, may be empty even if error (if no extra info)

      try {
        add_cnn_codegen_annotations(op_copy.get(), op_tune, 0);
      }
      catch( unsup_exception const & us_exp ) {
        std::cout << string("annotation failure: ") + us_exp.what();
      }

      if( err.str().empty() ) {
        if( gen_data ) {
          assert_st( gen_data->get_type() == "gen_data" );
        } // FIXME: remove assert after fixing existing usages
        vsi = make_shared<map_str_p_nda_t>();
        try {
          prc_ret = profile_rcg_call( op_copy, *codegen, gen_data, vsi.get(), run_iter, 0 );
          //saving best time and op_tune
          if(wix == 0){
            tuning_info << "\tbaseline: " << prc_ret.rt_secs << "\n";
            best_time = prc_ret.rt_secs;
            best_opt = op_tune;
          }
          else if(prc_ret.rt_secs < best_time){
            best_time = prc_ret.rt_secs;
            best_opt = op_tune;
          }
        }
        catch(
            unsup_exception const & ue ) { err << "profile call failure: " << ue.what();
        }
      }
      if( wix == 0 ) { // if this is the to-use-as-known-good op_tune, store it's results in vs1, and maybe write its digest.
        if( !err.str().empty() ) {
          err << strprintf( "known-good op_tune (kg_tune_tag=%s) failed. Can't write digests or do live comparisons.",
                            kg_tune_tag.c_str() );
        }
        else {
          vs_kg = vsi;
          if( write_kg_digest ) {
            for( map_str_p_nda_t::const_iterator i = vs_kg->begin(); i != vs_kg->end(); ++i ) {
              size_t const digest_seed = std::hash<string>()(i->first); // FIXME: make better seed by including op/op_tune/???
              op_wisdom_out->kgs.push_back( pair_str_p_nda_digest_t( i->first, nda_digest_t::make_from_nda( i->second, digest_seed ) ) );
            }
          }
        }
      }

      if( err.str().empty() ) { // if tune ran without error, do full-data compare
        double vmt = get( func_mrd_toler, prc_ret.op->get_func_name(), mrd_toler );
        // full-data compare
        if( vs_kg ) { // note: can only be only false if known-good run failed
          vect_string const vns_kg = get_keys( *vs_kg );
          vect_string const vns_wix = get_keys( *vsi );

          if( vns_kg != vns_wix ) {
            rt_err( strprintf( "reg/comp out var set mismatch: vns_kg=%s vns[%s]=%s\n",
                               str(vns_kg).c_str(), str(wix).c_str(), str(vns_wix).c_str() ) );
          }
          comp_vars( &err, num_mad_fail, vmt, 0, 0, max_err, vns_kg, vs_kg, vsi );
        }
      }

      if( !err.str().empty() ) {
        comp_errs++;
      }
    }

    if(print) {
      tuning_info << "\tbest parameters: " << str(best_opt).c_str() << " " << best_time << "\n";
      tuning_info << "\tsearch space size: " << op_tunes.size() << ", errors: " << comp_errs << "\n";
      std::cout << tuning_info.str() << (anno_op->get_conv_dims_info()).str() << "\n";
    }

    out->flush();

    if( wout ) {
      if( !write_runs ) {
        op_wisdom_out->wisdoms.clear();
      }
      if( !write_kg_digest ) {
        if( op_wisdom_in ) {
          op_wisdom_out->kgs = op_wisdom_in->kgs;
        }
      }
      write_op_wisdom( *op_wisdom_out, *wout );
      wout->flush();
    }

    rtc->finish_and_sync();

    assert_st(!num_mad_fail);

    return best_opt;
  }

  struct auto_tuner_tester_t : virtual public nesi, public has_main_t // NESI(help="get OpenCL/CUDA device informations",
    // bases=["has_main_t"], type_id="device_information" )
  {
      p_rtc_compute_t rtc;
      rtc_device_info_t dev_info;

    virtual cinfo_t const * get_cinfo( void ) const; // required declaration for NESI support

    virtual void main(nesi_init_arg_t * nia) {
      rtc = make_p_rtc_compute_t_init_and_check_unused_from_lexp( parse_lexp( "(be=ocl)" ), nia );
      rtc->init();

      dev_info = rtc->get_device_info();
      printf("%s %d %d\n", dev_info.device_name.c_str(), dev_info.wg_sz, dev_info.mem_sz);
    }
  };

  #include"gen/auto_tuner.H.nesi_gen.cc"
  #include"gen/auto_tuner.cc.nesi_gen.cc"
}