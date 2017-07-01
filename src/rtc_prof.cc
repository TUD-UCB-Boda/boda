// Copyright (c) 2015, Matthew W. Moskewicz <moskewcz@alumni.princeton.edu>; part of Boda framework; see LICENSE
#include"boda_tu_base.H"
#include"build_info.H"
#include"timers.H"
#include<boost/filesystem.hpp>
#include<boost/lexical_cast.hpp>
#include"lexp.H"
#include"has_main.H"
#include"rtc_func_gen.H"
#include"rtc_compute.H"
#include"cnn_op.H"
#include"op-tuner.H"
#include"conv_util.H"
#include"comp_util.H"
#include<iostream>
#include<sstream>
#include"auto_tuner.H"

namespace boda 
{
  struct ops_prof_t : virtual public nesi, public has_main_t // NESI(help="profile set of operations across backends and tuning params",
			 // bases=["has_main_t"], type_id="ops-prof" )

  {
    virtual cinfo_t const * get_cinfo( void ) const; // required declaration for NESI support
    p_filename_t out_fn; //NESI(help="output file (output goes to stdout if not specified)")
    p_filename_t wisdom_out_fn; //NESI(help="wisdom output file (wisdom not output if not specified)")
    p_filename_t wisdom_in_fn; //NESI(help="wisdom input file (to add to, may contain known-good results for checking)")

    // FIXME: we should use a map_str_p_rtc_compute_t here, but NESI doesn't support that yet. might work with manual typedef initally?
    vect_p_rtc_compute_t rtcs; //NESI(help="list of compute backends to use")
    vect_string rtcns; //NESI(help="list of names of compute backends (must have same # of elements as --rtcs option")

    rtc_compile_opts_t compile_opts; // NESI(default="()",help="runtime compilation options")
    filename_t ops_fn; //NESI(default="%(boda_test_dir)/ops-prof/ops-prof-debug.txt",help="file to read ops from")

    map_str_op_tune_t op_tunes; //NESI(default="()",help="tuning parameters / options")

    uint32_t run_iter; //NESI(default="1",help="re-run op to profile this many times (for power testing)")

    p_op_base_t gen_data; //NESI(help="test-pattern data generation parameters (if not provided, inputs will be zeros)")

    double mrd_toler; //NESI(default="2e-4",help="maximum maximum-absolute-difference over which a failure is declared")
    map_str_double func_mrd_toler; //NESI(default="()",help="per-function custom maximum maximum-absolute-differences over which a failure is declared (overrides mrd_toler per-function if specified")
    uint32_t max_err; //NESI(default="10",help="print at most this many differing elems")

    map_str_ops_be_t ops_bes;

    string kg_tune_tag; //NESI(help="use the tune with this tag for writing known-good digests and as the lhs for full-data live comparisons agains all tunes (inclusing itself).", req=1 )

    uint32_t write_kg_digest; //NESI(default="1", help="if non-zero, write results of op_tune with tag equal to the value of the kg_tune_tag option as known-good nda digests into output wisdom. if zero, will copy known-good digests from input wisdom if availible." )

    uint32_t write_runs; //NESI(default="0",help="if non-zero, write run data in output wisdom. will merge into existing runs if present (overwriting duplicates). if zero, output wisdom will have no runs (perhaps only known-good digests)" )

    uint32_t skip_ops; //NESI(default="0",help="skip this many ops (for debugging/testing large op lists with multiple)" )

    p_rtc_codegen_t & get_codegen_for_op_tune( op_tune_t const & op_tune ) {
      assert_st( !ops_bes.empty() );
      if( op_tune.use_be.empty() ) { return ops_bes.begin()->second.codegen; }
      return must_find( ops_bes, op_tune.use_be ).codegen;
    }

    virtual void main( nesi_init_arg_t * nia );
  };

  void ops_prof_t::main( nesi_init_arg_t * nia ) {
    p_ostream out = out_fn ? ofs_open( *out_fn ) : p_ostream( &std::cout, null_deleter<std::ostream>() );
    p_ostream wout = wisdom_out_fn ? ofs_open( *wisdom_out_fn ) : p_ostream();
    p_istream win;
    if( wisdom_in_fn ) {
      // if non-existant, don't load, but do complain
      if( boost::filesystem::is_regular_file( wisdom_in_fn->exp ) ) { win = ifs_open( *wisdom_in_fn ); } 
      else { (*out) << "warning: specified input wisdom file could not be loaded; ignoring. known-good testing disabled.\n"; } // 
    }

    // by default, add all enabled/availible backends
    if( rtcs.size() != rtcns.size() ) { rt_err( strprintf( "must specific the same # of rtcs and rtcns, but rtcs.size()=%s and rtcns.size()=%s\n", str(rtcs.size()).c_str(), str(rtcns.size()).c_str() ) ); }
    if( rtcs.empty() ) {
      if( is_feature_enabled("nvrtc") ) {
        ops_be_t ops_be{ "nvrtc", make_p_rtc_compute_t_init_and_check_unused_from_lexp( parse_lexp( "(be=nvrtc)" ), nia ) };
        must_insert( ops_bes, ops_be.rtcn, ops_be );
      }
      if( is_feature_enabled("opencl") ) { 
        ops_be_t ops_be{ "ocl", make_p_rtc_compute_t_init_and_check_unused_from_lexp( parse_lexp( "(be=ocl)" ), nia ) };
        must_insert( ops_bes, ops_be.rtcn, ops_be );
      }
    } else { // otherwise, use exactly/only the specified backends
      for( uint32_t i = 0; i != rtcs.size(); ++i ) { must_insert( ops_bes, rtcns[i], ops_be_t{rtcns[i],rtcs[i]} ); }
    }
    // init backends
    bool const enable_prof = 0;
    for( map_str_ops_be_t::iterator i = ops_bes.begin(); i != ops_bes.end(); ++i ) {
      ops_be_t & ops_be = i->second;
      ops_be.rtc->init();
      ops_be.codegen = make_shared<rtc_codegen_t>();
      ops_be.codegen->init( ops_be.rtc, make_cnn_custom_codegen_t(), compile_opts );
      if( enable_prof ) { ops_be.rtc->profile_start(); }
    }

    uint32_t num_mad_fail = 0;
    p_istream ops = ifs_open( ops_fn );
    string line;
    uint32_t kg_wix = uint32_t_const_max;
    for( uint32_t op_ix = 0; !ifs_getline( ops_fn.exp, ops, line ); op_ix++ ) { // op_ix is for printouts, helpful to run specific op later
      bool op_seen_errs = 0; // we only print out op info if there are errors, and then only once across all tunes.
      p_op_wisdom_t op_wisdom_in = win ? read_next_wisdom( win ) : p_op_wisdom_t();
      p_op_wisdom_t op_wisdom_out = make_shared< op_wisdom_t >();
      op_wisdom_out->op = make_p_op_base_t_init_and_check_unused_from_lexp( parse_lexp( line ), 0 ); 
      if( skip_ops ) { --skip_ops; continue; } // note: we already read from op_wisdom_in ... probably what we want?
      if( op_wisdom_in && (*op_wisdom_in->op != *op_wisdom_out->op) ) {
        rt_err( strprintf( "op mismatch between input wisdom and ops-list (to output): op_wisdom_in->op=%s op_wisdom_out=%s", 
                           str(op_wisdom_in->op).c_str(), str(op_wisdom_out->op).c_str() ) ); 
      }
      vect_uint32_t out_wixs_to_run; // will contain all wixs specified on the command line, with the known-good wix first
      {
        uint32_t wix = 0;
        for( map_str_op_tune_t::const_iterator i = op_tunes.begin(); i != op_tunes.end(); ++i, ++wix ) {
          op_wisdom_out->wisdoms.push_back( p_op_tune_wisdom_t( new op_tune_wisdom_t{make_shared<op_tune_t>(i->second)} ) );
          if( i->first == kg_tune_tag ) { kg_wix = wix; }
          else{ out_wixs_to_run.push_back( wix ); }
        }
        assert_st( kg_wix != uint32_t_const_max );
        out_wixs_to_run.insert( out_wixs_to_run.begin(), kg_wix );
      }
      // FIXME: quadratic in in*out tunes ... just need local map to fix.
      if( op_wisdom_in && write_runs ) // merge in input runs (if any)
      { 
        for( uint32_t i = 0; i != op_wisdom_in->wisdoms.size(); ++i ) {
          string const ot_str_in = str(op_wisdom_in->wisdoms[i]->op_tune);
          bool merged_in = 0;
          for( uint32_t j = 0; j != op_wisdom_out->wisdoms.size(); ++j ) {
            string const ot_str_out = str(op_wisdom_out->wisdoms[j]->op_tune);
            if( ot_str_in == ot_str_out ) { // matching, merge in runs
              assert_st( op_wisdom_out->wisdoms[j]->runs.empty() );
              op_wisdom_out->wisdoms[j]->runs = op_wisdom_in->wisdoms[i]->runs;
              merged_in = 1;
            }
          }
          if( !merged_in ) { 
            // FIXME: make not fatal? ignore? or just add runs to output (but don't otherwise run this tune?)
            // rt_err( strprintf( "input-wisdom op_tune=%s is not to be run", str(ot_str_in).c_str() ) );
            op_wisdom_out->wisdoms.push_back( op_wisdom_in->wisdoms[i] ); // preserve data, but don't run tune
          }

        }
      }
      p_map_str_p_nda_t vs_kg; // we compare all runs against the known-good run, whose results will be stored here
      for( vect_uint32_t::const_iterator wix = out_wixs_to_run.begin(); wix != out_wixs_to_run.end(); ++wix ) {
        p_op_tune_wisdom_t & op_tune_wisdom = op_wisdom_out->wisdoms[*wix];
        op_tune_t const & op_tune = *op_tune_wisdom->op_tune;
        // generate boda variant according to tuning params (just opt and t_tile_sz currently)
        p_conv_op_base_t anno_op = make_shared<conv_op_base_t>( *op_wisdom_out->op );
        p_rtc_codegen_t const & codegen = get_codegen_for_op_tune( op_tune );
        string const plat_tag = codegen->rtc->get_plat_tag();
        p_map_str_p_nda_t vsi;
        prc_ret_t prc_ret{0,NAN};
        std::ostringstream err; // one-line-only, goes to wisdom and text output; non-empty if any error
        std::ostringstream err_extra; // can be multi-line, goes to only to text output, may be empty even if error (if no extra info)
        try { add_codegen_annotations( anno_op, op_tune, 0 ); }
        catch( unsup_exception const & us_exp ) {
          err << string("annotation failure: ") + us_exp.what();
        }
        if( err.str().empty() ) {
          if( gen_data ) { assert_st( gen_data->get_type() == "gen_data" ); } // FIXME: remove assert after fixing existing usages
          vsi = make_shared<map_str_p_nda_t>();
          try { prc_ret = profile_rcg_call( anno_op, *codegen, gen_data, vsi.get(), run_iter, 0 ); }
          catch( unsup_exception const & ue ) { err << "profile call failure: " << ue.what(); }
        }
        if( (*wix) == kg_wix ) { // if this is the to-use-as-known-good op_tune, store it's results in vs1, and maybe write its digest.
          assert_st( wix == out_wixs_to_run.begin() ); // should be first run
          if( !err.str().empty() ) { 
            err << strprintf( "known-good op_tune (kg_tune_tag=%s) failed. Can't write digests or do live comparisons.",
                              kg_tune_tag.c_str() ); 
          } else {
            vs_kg = vsi;
            if( write_kg_digest ) {
              for( map_str_p_nda_t::const_iterator i = vs_kg->begin(); i != vs_kg->end(); ++i ) {
                size_t const digest_seed = std::hash<string>()(i->first); // FIXME: make better seed by including op/op_tune/???
                op_wisdom_out->kgs.push_back( pair_str_p_nda_digest_t( i->first, nda_digest_t::make_from_nda( i->second, digest_seed ) ) );
              }
            }
          }
        }
          
        if( err.str().empty() ) { // if tune ran without error, do compares
          double vmt = get( func_mrd_toler, prc_ret.op->get_func_name(), mrd_toler );
          // FIXME: here's a nice little hack for the winograd cases. sigh. when/if we split up all the cudnn algos
          // using a tuning parameter, maybe we can fix this better?
          if( (prc_ret.op->get_func_name() == "cudnn_conv") && ( get_xy_dims( op_wisdom_out->op->get_dims("kern_sz") ) == u32_pt_t{3,3} ) ) {
            vmt = 2e-3;
          }
          // full-data compare
          if( vs_kg ) { // note: can only be only false if known-good run failed
            vect_string const vns_kg = get_keys( *vs_kg );
            vect_string const vns_wix = get_keys( *vsi );
            if( vns_kg != vns_wix ) { rt_err( strprintf( "reg/comp out var set mismatch: vns_kg=%s vns[%s]=%s\n", 
                                                         str(vns_kg).c_str(), str(*wix).c_str(), str(vns_wix).c_str() ) ); }
            comp_vars( &err, num_mad_fail, vmt, 0, 0, max_err, vns_kg, vs_kg, vsi );
          }
          // digest compare
          if( op_wisdom_in ) {
            assert_st( op_wisdom_in->kgs.size() == vsi->size() );
            uint32_t vix = 0;
            for( map_str_p_nda_t::const_iterator i = vsi->begin(); i != vsi->end(); ++i, ++vix ) {
              assert_st( op_wisdom_in->kgs[vix].first == i->first ); // should be same var name
              p_nda_digest_t const & kg_digest = op_wisdom_in->kgs[vix].second;
              size_t const digest_seed = std::hash<string>()(i->first); // FIXME: make better seed by including op/op_tune/???
              p_nda_digest_t digest = nda_digest_t::make_from_nda( i->second, digest_seed );
              string const comp_res = kg_digest->mrd_comp( digest, vmt );
              if( !comp_res.empty() ) { 
                err << (i->first) + " digest mrd_comp() failure vs stored digest in '"+wisdom_in_fn->in+"'";
                err_extra << "comp_res:\n" + comp_res + "\n";
              }
            }
          } else { // no input wisdom, so no digest compute
            err << "digest mrd_comp() vs '"+str(op_tune)+"' skipped, no input wisdom availible";
          }
        }
        op_tune_wisdom->runs[plat_tag] = op_run_t{plat_tag,prc_ret.op,prc_ret.rt_secs,err.str()};
        if( !err.str().empty() ) { 
          on_op_err( *out, op_seen_errs, op_ix, op_wisdom_out->op );
          (*out) << "--  comp fail for op_tune='" + str(op_tune) + "'\n" << err.str() << "\n" << err_extra.str();
        }
      }      
      out->flush();
      if( wout ) { 
        if( !write_runs ) { op_wisdom_out->wisdoms.clear(); }
        if( !write_kg_digest ) { if( op_wisdom_in ) { op_wisdom_out->kgs = op_wisdom_in->kgs; } }
        write_op_wisdom( *op_wisdom_out, *wout ); 
        wout->flush();
      }
    }
      
    for( map_str_ops_be_t::iterator i = ops_bes.begin(); i != ops_bes.end(); ++i ) {
      ops_be_t & ops_be = i->second;
      if( enable_prof ) { ops_be.rtc->profile_stop(); }
      ops_be.rtc->finish_and_sync(); 
    }
  
    if( !num_mad_fail ) { (*out) << strprintf( "***ALL IS WELL***\n" ); }
    else { (*out) << strprintf( "***MAD FAILS*** num_mad_fail=%s\n", str(num_mad_fail).c_str() ); }

  }

  void add_to_with_prefix( vect_pair_str_str & out, vect_pair_str_str const & in, pair_str_str const & prefix ) {
    for( vect_pair_str_str::const_iterator i = in.begin(); i != in.end(); ++i ) {
      out.push_back( {prefix.first+i->first,prefix.second+i->second} );
    }
  }

  void emit_clis( p_ostream & out, vect_pair_str_str const & run_bases, vect_pair_str_str const & op_tunes ) {
    for( vect_pair_str_str::const_iterator i = run_bases.begin(); i != run_bases.end(); ++i ) {
      pair_str_str cli = *i;
      // note: could use %(test_name) in wisdom-in-fn instead of cli.first, but then resultant cli can't be run outside test framework ...
      cli.second += " --wisdom-in-fn='%(boda_test_dir)/good_tr/"+cli.first+"/wisdom.wis'"; 
      cli.second += " --op-tunes='(";
      for( vect_pair_str_str::const_iterator j = op_tunes.begin(); j != op_tunes.end(); ++j ) {
        cli.second += string((j==op_tunes.begin())?"":",") + j->first +"=("+j->second+")";
      }
      cli.second += ")'";
      (*out) << strprintf( "<li ignore_missing_outputs=\"1\" test_name=\"%s\" cli_str=\"%s\" />\n", str(cli.first).c_str(), str(cli.second).c_str() );
    }
  }

  void gen_ops_prof_tests( p_ostream & out, bool const & run_slow ) {
    bool const output_wisdom = 1;
    vect_pair_str_str op_tune_sgemm_bases = { {"def", ""},
                                              {"4-16-4-lm0","MNt=4:4,MNb=16:16,Kb=4,use_local_mem=0"},
                                              {"4-16-4-lm2-vw4","MNt=4:4,MNb=16:16,Kb=4,use_local_mem=2,vw=4"},
                                              {"4-16-4-lm3-vw4","MNt=4:4,MNb=16:16,Kb=4,use_local_mem=3,vw=4" } };
    // FIXME: enable_write_xpose=1 goes where? doesn't much matter for single ops? 
    // FIXME: enable_bconv=1 goes where? not part of per-op flow currently ... maybe it should be though.
    // FIXME: note: can't test per-op against caffe here -- should be enable that somehow? it's covered by test_compute_multi, of course ..
    vect_pair_str_str op_tune_conv_bases = { {"def",""},
                                             {"opt","k1conv=1,tconv=1"}, // general op_tune for titan X
                                             {"AMD1", // guess1 at tune for AMD/GCN
                                              "use_culibs=0,MNt=4:4,MNb=8:8,k1conv=1,tconv=1"},
                                             {"AMD2", // guess2 at tune for AMD/GCN
                                              "use_culibs=0,MNt=4:4,MNb=8:8,tconv=1"},
                                             {"AMD3", // guess3 at tune for AMD/GCN
                                              "use_culibs=0,MNt=4:4,MNb=8:8,k1conv=1"},
                                             {"AMD4", // guess4 at tune for AMD/GCN
                                              "use_culibs=0,MNt=4:4,MNb=8:8"},
    };
    vect_pair_str_str op_tunes_sgemm;
    vect_pair_str_str op_tunes_conv;
    string kg_tune_tag;
    if( is_feature_enabled("opencl") ) { 
      if( kg_tune_tag.empty() ) { kg_tune_tag = "ocl-def"; }
      add_to_with_prefix( op_tunes_sgemm, op_tune_sgemm_bases, {"ocl-","use_be=ocl,"} );
      add_to_with_prefix( op_tunes_conv, op_tune_conv_bases, {"ocl-","use_be=ocl,"} );
      // FIXME/NOTE: (some?) vector widths don't work with the nvrtc backend (only opencl) currently, due to syntax
      // issues / language support issues. revisit/investigate?
      op_tunes_conv.push_back( pair_str_str{"8-16-1-lm2-vm8",  // general tune for SD820 
            "use_be=ocl,MNt=8:8,MNb=16:16,k1conv=1,tconv=0,Kb=1,use_local_mem=2,vw=8"} );
    }
    if( is_feature_enabled("nvrtc") ) { 
      if( kg_tune_tag.empty() ) { kg_tune_tag = "nvrtc-def"; }
      add_to_with_prefix( op_tunes_sgemm, op_tune_sgemm_bases, {"nvrtc-","use_be=nvrtc,"} );
      op_tunes_sgemm.push_back( {"culibs","use_be=nvrtc,use_culibs=1"} ); 
      add_to_with_prefix( op_tunes_conv, op_tune_conv_bases, {"nvrtc-","use_be=nvrtc,"} );
      op_tunes_conv.push_back( {"culibs","use_be=nvrtc,use_culibs=1"} ); 
    }
    if( kg_tune_tag.empty() ) { 
      rt_err( "no known-good tune tag set; can't generate ops-prof test command lines. are no operation-level backends enabled? i.e. both OpenCL and nvrtc are disabled?" ); 
    }
    string cli_base = "boda ops-prof --out-fn='%(boda_output_dir)/cnn_op_info.txt' --kg-tune-tag=" + kg_tune_tag;
    cli_base += " --func-mrd-toler='(cudnn_conv=4e-4)'"; // FIXME: should be 4e-4 for non-winograd cudnn_conv, but 2e-3 for winograd ones.
    if( output_wisdom ) { cli_base += " --wisdom-out-fn='%(boda_output_dir)/wisdom.wis'"; }
    string const sgemm_ops = " --ops-fn='%(boda_test_dir)/sgemm-ops-debug.txt'";
    string const cnn_ops = " --ops-fn='%(boda_test_dir)/conv-ops-debug.txt'";
    string const gen_data_mode_600 = " --gen-data='(str_vals=(type=gen_data),nda_vals=(vi=(tn=float,v=0.0),mode=(tn=uint32_t,v=600)))'";
    string const gen_data_mode_5 = " --gen-data='(str_vals=(type=gen_data),nda_vals=(vi=(tn=float,v=0.0),mode=(tn=uint32_t,v=5)))'";
    vect_pair_str_str run_bases_sgemm;
    run_bases_sgemm.push_back( {"sgemm-gen600", cli_base + sgemm_ops + gen_data_mode_600 } );
    run_bases_sgemm.push_back( {"sgemm-gen5", cli_base + sgemm_ops + gen_data_mode_5 } );
    vect_pair_str_str run_bases_conv;
    run_bases_conv.push_back( {"conv-gen5", cli_base + cnn_ops + gen_data_mode_5} );
    if( run_slow ) {
      run_bases_conv.push_back( {"conv-full-gen5", cli_base + " --ops-fn='%(boda_test_dir)/conv-ops-1-5-20-nin-alex-gn.txt'" +gen_data_mode_5});
    }
    run_bases_conv.push_back( {"conv-debug", cli_base + " --ops-fn='%(boda_test_dir)/conv-ops-debug-tmp.txt'" +gen_data_mode_5});
    (*out) << "<root>\n";
    emit_clis( out, run_bases_sgemm, op_tunes_sgemm );
    emit_clis( out, run_bases_conv, op_tunes_conv );
    (*out) << "</root>\n";                                
  }

  // normally, the output of this mode is generated automatically by a magic filename-based hack in
  // test_cmds_t. however, this mode is provided as a way to generate the file without going though test_cmds_t ...
  struct gen_ops_prof_tests_t : virtual public nesi, public has_main_t // NESI( help="generate list of ops-prof tests",
			// bases=["has_main_t"], type_id="gen_ops_prof_tests")
  {
    filename_t out_fn; //NESI(default="gen_ops_prof_tests.xml",help="output: xml list of tests.")
    virtual cinfo_t const * get_cinfo( void ) const; // required declaration for NESI support    
    virtual void main( nesi_init_arg_t * nia ) {
      p_ostream out = ofs_open( out_fn.exp );
      gen_ops_prof_tests( out, 1 );
    }
  };


#include"gen/rtc_prof.cc.nesi_gen.cc"

}
