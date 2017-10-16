// Copyright (c) 2017, Matthew W. Moskewicz <moskewcz@alumni.princeton.edu>; part of Boda framework; see LICENSE
#include"boda_tu_base.H"
#include"has_main.H"
#include"str_util.H"
#include"data-stream.H"
#include"nesi.H"
extern "C" {
#include"libavformat/avformat.h"
}

namespace boda 
{

  struct data_stream_ffmpeg_src_t : virtual public nesi, public data_stream_t // NESI(
                                    // help="parse file with ffmpeg (libavformat,...) output one block per raw video frame",
                                    // bases=["data_stream_t"], type_id="ffmpeg-src")
  {
    virtual cinfo_t const * get_cinfo( void ) const; // required declaration for NESI support
    
    filename_t fn; //NESI(req=1,help="input filename")
    p_dims_t out_dims; // NESI(help="set dims of output to this. size must match data size. if not set, will typically emit 'bytes' (1D array of uint8_t, with one dim named 'v'), but default may depend on specific file reader.")
    uint32_t stream_index; //NESI(default="0",help="ffmpeg stream index from which to extract frames from")

    virtual string get_pos_info_str( void ) { return string( "ffmpeg-src: pos info TODO" ); }

    virtual bool seek_to_block( uint64_t const & frame_ix ) { return false; }

    AVFormatContext *ic;

    data_stream_ffmpeg_src_t( void ) : ic( 0 ) { }
    
    virtual void data_stream_init( nesi_init_arg_t * const nia ) {
      ic = avformat_alloc_context();
      if (!ic) { rt_err( "avformat_alloc_context() failed" ); }
      //ic->interrupt_callback.callback = decode_interrupt_cb;
      //ic->interrupt_callback.opaque = is;
      AVDictionary * format_opts = NULL;
      // note: by default, ffplay sets "scan_all_pmts" to 1 here. but, perhaps we can ignore that, since it's unclear if
      // it's relevant to us -- seems only to be for mpegts containers, and then only sometimes relevant?
      string const ffmpeg_url = "file:" + fn.exp;
      //AVInputFormat *iformat;
      int err;
      err = avformat_open_input(&ic, ffmpeg_url.c_str(), NULL, &format_opts);
      if( err < 0 ) { rt_err( strprintf( "avformat_open_input failed for ffmpeg_url=%s\n", str(ffmpeg_url).c_str() ) ); }
      // note: here, we could check that all options were consumed. but, we're not setting any, so why bother. see the
      // relevant check in ffplay.c
      err = avformat_find_stream_info(ic, NULL);
      if( err < 0 ) { printf( "warning: avformat_find_stream_info() failed for ffmpeg_url=%s\n", str(ffmpeg_url).c_str() ); }

      if( !( stream_index < ic->nb_streams ) ) {
        rt_err( strprintf( "user requested (zero-based) stream_index=%s, but ffmpeg says there are only ic->nb_streams=%s streams.\n",
                           str(stream_index).c_str(), str(ic->nb_streams).c_str() ) );
      }
    
      for( uint32_t i = 0; i != ic->nb_streams; ++i ) {
        // FIXME: for no obvious reason, av_dump_format() seems to print nothing -- maybe an stdio/iostreams or other
        // C++-and-ffmpeg issue?
#if 0
        printf( "av_dump_format for stream: i=%s\n", str(i).c_str() );
        av_dump_format(ic, i, ffmpeg_url.c_str(), 0);
#endif
        ic->streams[stream_index]->discard = ( i == stream_index ) ? AVDISCARD_DEFAULT : AVDISCARD_ALL;
      }

      AVStream * const vid_st = ic->streams[stream_index];
      // FIXME/NOTE: it seems we could use either a direct check on vid_st_type or avformat_match_stream_specifier here. hmm.
      // AVMediaType vid_st_type = vid_st->codecpar->codex_type;
      int const avmss_ret = avformat_match_stream_specifier( ic, vid_st, "v" );
      assert_st( avmss_ret >= 0 );
      if( avmss_ret == 0 ) { rt_err( strprintf( "stream stream_index=%s is not a video stream", str(stream_index).c_str() ) ); }
    
    }
    
    virtual data_block_t proc_block( data_block_t const & db ) {
      assert_st( ic );
      data_block_t ret = db;
      AVPacket pkt;
      int const err = av_read_frame(ic, &pkt);
      if( err < 0 ) { return ret; }
      assert_st( (uint32_t)pkt.stream_index == stream_index ); // AVDISCARD_ALL setting for other streams in init() should guarentee this
      ret.nda = make_shared<nda_t>( dims_t{ vect_uint32_t{uint32_t(pkt.size)}, vect_string{ "v" }, "uint8_t" } );
      std::copy( pkt.data, pkt.data + pkt.size, (uint8_t *)ret.d() );
      if( out_dims ) { assert_st( ret.nda ); ret.nda->reshape( *out_dims ); }
      return ret;
    }
  };
  
#include"gen/data-stream-ffmpeg.cc.nesi_gen.cc"

}
