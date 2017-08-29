// Copyright (c) 2015, Matthew W. Moskewicz <moskewcz@alumni.princeton.edu>; part of Boda framework; see LICENSE
#include"boda_tu_base.H"
#include"geom_prim.H"
#include"img_io.H"
#include"str_util.H"
#include"data-to-img.H"
#include"data-stream.H"

namespace boda 
{
  
  struct data_to_img_raw_t : virtual public nesi, public data_to_img_t // NESI(help="convert data blocks (containing raw video frames) to images",
                           // bases=["data_to_img_t"], type_id="raw")
  {
    virtual cinfo_t const * get_cinfo( void ) const; // required declaration for NESI support
    uint32_t verbose; //NESI(default="0",help="verbosity level (max 99)")
    u32_box_t crop; //NESI(default="0:0:0:0",help="crop pels from sides")
    string img_fmt; //NESI(req=1,help="image format; valid values '16u-grey', '32f-grey', '16u-RGGB' ")
    string meta; //NESI(default="nda",help="if image has a natural type, specify it here (default is generic nda)")
    string tag; //NESI(default="stream",help="descriptive short string tag for type of stream: lidar, radar, camera, ...")
    uint32_t level_adj; //NESI(default=1,help="if non-zero, adjust levels with filt_alpha LPF sliding window on per-frame min/max. otherwise (if 0), make some random assumptions about levels: 12-bit for 16u-grey/RGGB, direct-cast-to-uint8_t for 32f")
    uint32_t invert_intensity; //NESI(default=0,help="if non-zero, for greyscale outputs only, map [data_min,data_max] to [1,0] (instead of to [0,1])")
    uint32_t level_adj_log_for_float_data; //NESI(default=0,help="if non-zero, level_adj will use for log-scale normalization for float data values")
    float level_filt_alpha; //NESI(default=.9,help="LPF alpha constant for sliding window level adjustment")

    float rgb_levs_filt_min;
    float rgb_levs_filt_rng; 
    float rgb_levs_frame_min;
    float rgb_levs_frame_max;

    uint32_t grey_to_pel_maybe_inv( uint8_t const & gv ) { return invert_intensity ? grey_to_pel( 255 - gv ) : grey_to_pel( gv ); }
    
    // internal state and buffers
    u32_pt_t cur_frame_sz;
    zi_uint64_t frame_sz_bytes;
    p_img_t frame_buf;
    i32_pt_t samp_pt;
    uint32_t get_bytes_per_pel( void ) {
      if( startswith(img_fmt, "32") ) { return 4; }
      if( startswith(img_fmt, "24") ) { return 3; }
      if( startswith(img_fmt, "16") ) { return 2; }
      rt_err( "can't determine bytes-per-pel: unknown img_fmt: " + img_fmt );
    }
    virtual string data_block_get_meta( data_block_t const & db ) { return meta; }
    virtual string get_stream_tag( void ) { return tag; }

    virtual void set_samp_pt( i32_pt_t const & samp_pt_ ) { samp_pt = samp_pt_; }

    void set_frame_sz( u32_pt_t const & sz ) {
      if( cur_frame_sz == sz ) { return; } // already set correctly
      cur_frame_sz = sz;
      frame_sz_bytes.v = cur_frame_sz.dims_prod() * get_bytes_per_pel();
      frame_buf = make_shared< img_t >();
      assert_st( cur_frame_sz.both_dims_gt( crop.bnds_sum() ) );
      frame_buf->set_sz_and_alloc_pels( cur_frame_sz - crop.bnds_sum() );
    }
    
    virtual void data_to_img_init( nesi_init_arg_t * const nia ) {
      samp_pt = i32_pt_t(-1,-1); // invalid/sentinel value to suppress samp_pt prinouts
      rgb_levs_filt_min = float_const_min;
      rgb_levs_filt_rng = 0; 
    }
    virtual p_img_t data_block_to_img( data_block_t const & db ) {
      if( level_adj && (rgb_levs_filt_min == float_const_min) ) { // init level filt on first frame
        data_block_to_img_inner( db ); // note: writes (maybe-garbage) values to frame_buf which are unused, but sets frame levels
        rgb_levs_filt_min = rgb_levs_frame_min;
        float const rgb_levs_frame_rng = rgb_levs_frame_max - rgb_levs_frame_min;
        rgb_levs_filt_rng = rgb_levs_frame_rng;
      }
      return data_block_to_img_inner( db );
    }

    // return false if end-of-stream, true otherwise
    bool maybe_set_per_block_frame_sz( data_block_t const & db ) {
      if( !db.valid() ) { return false; } // end-of-stream: return failure      
      // set frame size based on nda dims
      if( !db.nda ) { rt_err( "data_to_img_raw: frame_sz not set, but data block didn't have nda_dims set. can't determine image dims." ); }
      dim_t const * const c_dim = db.nda->dims.get_dim_by_name("c");
      uint32_t const chans = c_dim ? c_dim->sz : 1;
      set_frame_sz( get_xy_dims( db.nda->dims ) );
      if( (db.nda->dims.tsz() * chans) != get_bytes_per_pel() ) { rt_err( strprintf( "nda dims / pel format byte size mismatch: db.nda_dims.tsz()=%s but get_bytes_per_pel()=%s\n", str(db.nda->dims.tsz()).c_str(), str(get_bytes_per_pel()).c_str() ) ); }

      if( db.sz() != frame_sz_bytes.v ) {
        rt_err( strprintf( "error: can't convert data block to string, had db.sz=%s but frame_sz_bytes.v=%s\n",
                           str(db.sz()).c_str(), str(frame_sz_bytes.v).c_str() ) );
      }
      return 1;
    }
    
    p_img_t data_block_to_img_inner( data_block_t const & db ) {
      if( !maybe_set_per_block_frame_sz( db ) ) { return p_img_t(); } // if no data block, return no (null) img_t
      u32_pt_t const & img_sz = frame_buf->sz;
      rgb_levs_frame_min = float_const_max;
      rgb_levs_frame_max = float_const_min;
      // copy and convert frame data
      if( 0 ) {
      } else if( img_fmt == "16u-RGGB" ) {
        uint16_t const * const rp_frame = (uint16_t const *)(db.d());
        for( uint32_t d = 0; d != 2; ++d ) {
          assert_st( !(cur_frame_sz.d[d]&1) );
          assert_st( !(crop.p[0].d[d]&1) );
          assert_st( !(crop.p[1].d[d]&1) );
        }
        for( uint32_t y = 0; y < img_sz.d[1]; y += 2 ) {
          uint32_t const src_y = crop.p[0].d[1] + y;
          uint16_t const * const src_data = rp_frame + (src_y)*cur_frame_sz.d[0];
          uint16_t const * const src_data_yp1 = rp_frame + (src_y+1)*cur_frame_sz.d[0];
          uint32_t * const dest_data = frame_buf->get_row_addr( y );
          uint32_t * const dest_data_yp1 = frame_buf->get_row_addr( y+1 );
          for( uint32_t x = 0; x < img_sz.d[0]; x += 2 ) {
            uint32_t const src_x = crop.p[0].d[0] + x;
            if( int32_t(x|1) == (samp_pt.d[0]|1) && int32_t(y|1) == (samp_pt.d[1]|1)  ) {
              printf( "\nx,y = %s,%s  --  %s %s\n                   %s %s\n",
                      str(uint32_t(x)).c_str(), str(uint32_t(y)).c_str(),
                      str(uint32_t(src_data[x])).c_str(), str(uint32_t(src_data[x+1])).c_str(),
                      str(uint32_t(src_data_yp1[x])).c_str(), str(uint32_t(src_data_yp1[x+1])).c_str() );
            }
            uint16_t rgb[3]; // as r,g,b
            // set raw values first
            rgb[0] = src_data[src_x+1];
            rgb[1] = (src_data[src_x] + src_data_yp1[src_x+1]) >> 1;
            rgb[2] = src_data_yp1[src_x];            
            if( level_adj ) {
              for( uint32_t d = 0; d != 3; ++d ) {
                min_eq( rgb_levs_frame_min, float(rgb[d]) );
                max_eq( rgb_levs_frame_max, float(rgb[d]) );
                rgb[d] = clamp( (float(rgb[d]) - rgb_levs_filt_min) * (float(uint8_t_const_max) + 1.0f) / rgb_levs_filt_rng, 0.0f, float(uint8_t_const_max) );
              }          
            } else { // hard-coded level adj
              rgb[0] >>= 4;
              rgb[1] >>= 4;
              rgb[2] >>= 4;
            }
            uint32_t const pel = rgba_to_pel(rgb[0],rgb[1],rgb[2]);
            dest_data[x] = pel;  dest_data[x+1] = pel;
            dest_data_yp1[x] = pel;  dest_data_yp1[x+1] = pel;
          }
        }
      } else if( img_fmt == "16u-grey" ) {
        for( uint32_t y = 0; y < img_sz.d[1]; ++y ) {
          uint32_t const src_y = crop.p[0].d[1] + y;
          uint16_t const * const src_data = (uint16_t const *)(db.d()) + src_y*cur_frame_sz.d[0];
          uint32_t * const dest_data = frame_buf->get_row_addr( y );
          for( uint32_t x = 0; x < img_sz.d[0]; ++x ) {
            uint32_t const src_x = crop.p[0].d[0] + x;
            uint16_t gv = src_data[src_x];
            if( level_adj ) {
              min_eq( rgb_levs_frame_min, float(gv) );
              max_eq( rgb_levs_frame_max, float(gv) );
              gv = clamp( (float(gv) - rgb_levs_filt_min) * (float(uint8_t_const_max) + 1.0f) / rgb_levs_filt_rng, 0.0f, float(uint8_t_const_max) );
            } else { gv >>= 4; } // hard-coded assume 12-bits
            dest_data[x] = grey_to_pel_maybe_inv( gv ); 
          }
        }
      } else if( img_fmt == "24u-RGB" ) {
        for( uint32_t y = 0; y < img_sz.d[1]; ++y ) {
          uint32_t const src_y = crop.p[0].d[1] + y;
          uint8_t const * const src_data = (uint8_t const *)(db.d()) + src_y*cur_frame_sz.d[0]*get_bytes_per_pel();
          uint32_t * const dest_data = frame_buf->get_row_addr( y );
          for( uint32_t x = 0; x < img_sz.d[0]; ++x ) {
            uint32_t const src_x = (crop.p[0].d[0] + x)*get_bytes_per_pel();
            dest_data[x] = rgba_to_pel( src_data[src_x+2], src_data[src_x+1], src_data[src_x] ); 
          }
        }
      } else if( img_fmt == "32f-grey" ) {
        for( uint32_t y = 0; y < img_sz.d[1]; ++y ) {
          uint32_t const src_y = crop.p[0].d[1] + y;
          float const * const src_data = ((float const *)db.d()) + (src_y*cur_frame_sz.d[0]);
          uint32_t * const dest_data = frame_buf->get_row_addr( y );
          for( uint32_t x = 0; x < img_sz.d[0]; ++x ) {
            uint32_t const src_x = crop.p[0].d[0] + x;
            float gv = src_data[src_x];
            if( verbose ) { printf( " %s", str(gv).c_str() ); }
            if( level_adj ) {
              min_eq( rgb_levs_frame_min, gv );
              max_eq( rgb_levs_frame_max, gv );
              if( level_adj_log_for_float_data ) {
                gv = logf(gv - rgb_levs_filt_min + 1.0f) * (float(uint8_t_const_max) + 1.0f) / logf( rgb_levs_filt_rng + 1.0f );
              } else { gv = (gv - rgb_levs_filt_min) * (float(uint8_t_const_max) + 1.0f) / rgb_levs_filt_rng; }
              clamp_eq( gv, 0.0f, float(uint8_t_const_max) );
            } else { } // hard-coded assume already in [0,256)
            if( verbose ) { printf( "=%s", str(gv).c_str() ); }
            dest_data[x] = grey_to_pel_maybe_inv( gv ); 
          }
          if( verbose ) { printf( "\n" ); }
        }
      } else { rt_err( "can't decode frame: unknown img_fmt: " + img_fmt ); }
      // update level filter
      if( level_adj ) {
        float const rgb_levs_frame_rng = rgb_levs_frame_max - rgb_levs_frame_min;
        if( verbose ) {
         printf( "rgb_levs_filt_rng=%s rgb_levs_filt_min=%s\n", str(rgb_levs_filt_rng).c_str(), str(rgb_levs_filt_min).c_str() );
         printf( "rgb_levs_frame_min=%s rgb_levs_frame_max=%s\n", str(rgb_levs_frame_min).c_str(), str(rgb_levs_frame_max).c_str() );
        }
        rgb_levs_filt_rng *= level_filt_alpha;
        rgb_levs_filt_rng += (1.0 - level_filt_alpha)*rgb_levs_frame_rng;
        rgb_levs_filt_min *= level_filt_alpha;
        rgb_levs_filt_min += (1.0 - level_filt_alpha)*rgb_levs_frame_min;
      }
      return frame_buf;
    }
    string data_block_to_str( data_block_t const & db ) {
      if( !maybe_set_per_block_frame_sz( db ) ) { return string(); } // if no data block, return no (empty) string
      string ret;
      u32_pt_t const & img_sz = frame_buf->sz;
      ret += img_fmt + " " + str(img_sz) + "\n";
      // copy and convert frame data
      if( 0 ) {
      } else if( img_fmt == "16u-RGGB" || img_fmt == "16u-grey" ) {
        uint16_t const * const rp_frame = (uint16_t const *)(db.d());
        for( uint32_t d = 0; d != 2; ++d ) {
          assert_st( !(cur_frame_sz.d[d]&1) );
          assert_st( !(crop.p[0].d[d]&1) );
          assert_st( !(crop.p[1].d[d]&1) );
        }
        for( uint32_t y = 0; y < img_sz.d[1]; y += 1 ) {
          uint32_t const src_y = crop.p[0].d[1] + y;
          uint16_t const * const src_data = rp_frame + (src_y)*cur_frame_sz.d[0];
          //uint32_t * const dest_data = frame_buf->get_row_addr( y );
          for( uint32_t x = 0; x < img_sz.d[0]; x += 1 ) {
            uint32_t const src_x = crop.p[0].d[0] + x;
            uint16_t v = src_data[src_x];
            ret += " " + str(v);
          }
          ret += "\n";
        }
      } else if( img_fmt == "32f-grey" ) {
        for( uint32_t y = 0; y < img_sz.d[1]; ++y ) {
          uint32_t const src_y = crop.p[0].d[1] + y;
          float const * const src_data = ((float const *)db.d()) + (src_y*cur_frame_sz.d[0]);
          //uint32_t * const dest_data = frame_buf->get_row_addr( y );
          for( uint32_t x = 0; x < img_sz.d[0]; ++x ) {
            uint32_t const src_x = crop.p[0].d[0] + x;
            float gv = src_data[src_x];
            ret += " " + str(gv);
          }
          ret += "\n";
        }
      } else { rt_err( "can't decode frame: unknown img_fmt: " + img_fmt ); }
      return ret;
    }
    virtual p_nda_t data_block_to_nda( data_block_t const & db ) {
      if( !maybe_set_per_block_frame_sz( db ) ) { return p_nda_t(); } // if no data block, return no (null) nda_t
      u32_pt_t const & img_sz = frame_buf->sz;
      p_nda_t ret;
      if( crop.p[0].d[0] || crop.p[1].d[0] ) { rt_err( "TODO: non-zero x-coord crop for to_nda (needs copy and/or padded/strided nda support)" ); }
      if( img_fmt == "32f-grey" ) {
        float const * const src_data = ((float const *)db.d()) + (crop.p[0].d[1]*cur_frame_sz.d[0]);
        ret = make_shared<nda_t>( dims_t{ vect_uint32_t{img_sz.d[1],img_sz.d[0]}, "float" }, (void*)src_data );
        assert_st( (uint8_t *)src_data + ret->dims.bytes_sz() <= (uint8_t *)db.d() + db.sz() );
      } else {
        rt_err( strprintf( "img_fmt=%s not supported for to_nda()", str(img_fmt).c_str() ) );
      }
      return ret;
    }

    
  };

  struct data_to_img_null_t : virtual public nesi, public data_to_img_t // NESI(help="consume data blocks and return nothing (null images)",
                           // bases=["data_to_img_t"], type_id="null")
  {
    virtual cinfo_t const * get_cinfo( void ) const; // required declaration for NESI support
    uint32_t verbose; //NESI(default="0",help="verbosity level (max 99)")

    virtual void set_samp_pt( i32_pt_t const & samp_pt_ ) { }    
    virtual void data_to_img_init( nesi_init_arg_t * const nia ) { }
    virtual p_img_t data_block_to_img( data_block_t const & db ) {
      if( verbose ) { printf( "data_to_img_null: db=%s\n", db.info_str().c_str() ); }
      return p_img_t();
    } 
    virtual string data_block_to_str( data_block_t const & db ) { return "<data_to_img_null_t:to-strnot-implemented>"; } 
    virtual string data_block_get_meta( data_block_t const & db ) { return string("null"); }
    virtual p_nda_t data_block_to_nda( data_block_t const & db ) { return p_nda_t(); }
    virtual string get_stream_tag( void ) { return string("null"); }
   
  };

  struct data_to_img_lidar_t : virtual public nesi, public data_to_img_t // NESI(help="consume velodyne lidar data blocks (packets) and return nothing (null images)",
                           // bases=["data_to_img_t"], type_id="lidar")
  {
    virtual cinfo_t const * get_cinfo( void ) const; // required declaration for NESI support
    uint32_t verbose; //NESI(default="0",help="verbosity level (max 99)")

    virtual void set_samp_pt( i32_pt_t const & samp_pt_ ) { }    
    virtual void data_to_img_init( nesi_init_arg_t * const nia ) {
    }
    virtual p_img_t data_block_to_img( data_block_t const & db ) {
      if( verbose ) { printf( "data_to_img_lidar: db=%s\n", db.info_str().c_str() ); }
      return p_img_t();
    }
    virtual string data_block_to_str( data_block_t const & db ) { return "<data_to_img_lidar_t:to-str-not-implemented>"; } 
    virtual string data_block_get_meta( data_block_t const & db ) { return string("lidar"); }
    // FIXME: plan is to remove the following after pipe-based-img-conv stuff is added ... 'to_nda' will always be just db.nda in all cases.
    virtual p_nda_t data_block_to_nda( data_block_t const & db ) { 
      if( !db.nda ) { rt_err( "<unsurprising internal error: data_to_img_lidar expected nda to be set, but it wasn't>" ); }
      return db.nda;
    }
    virtual string get_stream_tag( void ) { return string("lidar"); }
  };

#include"gen/data-to-img.H.nesi_gen.cc"
#include"gen/data-to-img.cc.nesi_gen.cc"

}
