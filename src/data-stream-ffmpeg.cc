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
#if 0
/* this thread gets the stream from the disk or the network */
static int read_thread(void *arg)
{
    VideoState *is = arg;
    int err, i, ret;
    int st_index[AVMEDIA_TYPE_NB];
    AVPacket pkt1, *pkt = &pkt1;
    int64_t stream_start_time;
    int pkt_in_play_range = 0;
    AVDictionaryEntry *t;
    SDL_mutex *wait_mutex = SDL_CreateMutex();
    int scan_all_pmts_set = 0;
    int64_t pkt_ts;

    
    if (!wait_mutex) {
        av_log(NULL, AV_LOG_FATAL, "SDL_CreateMutex(): %s\n", SDL_GetError());
        ret = AVERROR(ENOMEM);
        goto fail;
    }

    memset(st_index, -1, sizeof(st_index));
    is->last_video_stream = is->video_stream = -1;
    is->last_audio_stream = is->audio_stream = -1;
    is->last_subtitle_stream = is->subtitle_stream = -1;
    is->eof = 0;
#endif

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
#if 0    
    if (find_stream_info) {
        AVDictionary **opts = setup_find_stream_info_opts(ic, codec_opts);
        int orig_nb_streams = ic->nb_streams;

        err = avformat_find_stream_info(ic, opts);

        for (i = 0; i < orig_nb_streams; i++)
            av_dict_free(&opts[i]);
        av_freep(&opts);

        if (err < 0) {
            av_log(NULL, AV_LOG_WARNING,
                   "%s: could not find codec parameters\n", is->filename);
            ret = -1;
            goto fail;
        }
    }

    if (ic->pb)
        ic->pb->eof_reached = 0; // FIXME hack, ffplay maybe should not use avio_feof() to test for the end

    if (seek_by_bytes < 0)
        seek_by_bytes = !!(ic->iformat->flags & AVFMT_TS_DISCONT) && strcmp("ogg", ic->iformat->name);

    is->max_frame_duration = (ic->iformat->flags & AVFMT_TS_DISCONT) ? 10.0 : 3600.0;

    if (!window_title && (t = av_dict_get(ic->metadata, "title", NULL, 0)))
        window_title = av_asprintf("%s - %s", t->value, input_filename);

    /* if seeking requested, we execute it */
    if (start_time != AV_NOPTS_VALUE) {
        int64_t timestamp;

        timestamp = start_time;
        /* add the stream start time */
        if (ic->start_time != AV_NOPTS_VALUE)
            timestamp += ic->start_time;
        ret = avformat_seek_file(ic, -1, INT64_MIN, timestamp, INT64_MAX, 0);
        if (ret < 0) {
            av_log(NULL, AV_LOG_WARNING, "%s: could not seek to position %0.3f\n",
                    is->filename, (double)timestamp / AV_TIME_BASE);
        }
    }
    is->realtime = is_realtime(ic);
#endif
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
    
#if 0
    for (i = 0; i < ic->nb_streams; i++) {
        AVStream *st = ic->streams[i];
        enum AVMediaType type = st->codecpar->codec_type;
        st->discard = AVDISCARD_ALL;
        if (type >= 0 && wanted_stream_spec[type] && st_index[type] == -1)
            if (avformat_match_stream_specifier(ic, st, wanted_stream_spec[type]) > 0)
                st_index[type] = i;
    }
    for (i = 0; i < AVMEDIA_TYPE_NB; i++) {
        if (wanted_stream_spec[i] && st_index[i] == -1) {
            av_log(NULL, AV_LOG_ERROR, "Stream specifier %s does not match any %s stream\n", wanted_stream_spec[i], av_get_media_type_string(i));
            st_index[i] = INT_MAX;
        }
    }

    if (!video_disable)
        st_index[AVMEDIA_TYPE_VIDEO] =
            av_find_best_stream(ic, AVMEDIA_TYPE_VIDEO,
                                st_index[AVMEDIA_TYPE_VIDEO], -1, NULL, 0);
    if (!audio_disable)
        st_index[AVMEDIA_TYPE_AUDIO] =
            av_find_best_stream(ic, AVMEDIA_TYPE_AUDIO,
                                st_index[AVMEDIA_TYPE_AUDIO],
                                st_index[AVMEDIA_TYPE_VIDEO],
                                NULL, 0);
    if (!video_disable && !subtitle_disable)
        st_index[AVMEDIA_TYPE_SUBTITLE] =
            av_find_best_stream(ic, AVMEDIA_TYPE_SUBTITLE,
                                st_index[AVMEDIA_TYPE_SUBTITLE],
                                (st_index[AVMEDIA_TYPE_AUDIO] >= 0 ?
                                 st_index[AVMEDIA_TYPE_AUDIO] :
                                 st_index[AVMEDIA_TYPE_VIDEO]),
                                NULL, 0);

    is->show_mode = show_mode;
    if (st_index[AVMEDIA_TYPE_VIDEO] >= 0) {
        AVStream *st = ic->streams[st_index[AVMEDIA_TYPE_VIDEO]];
        AVCodecParameters *codecpar = st->codecpar;
        AVRational sar = av_guess_sample_aspect_ratio(ic, st, NULL);
        if (codecpar->width)
            set_default_window_size(codecpar->width, codecpar->height, sar);
    }

    /* open the streams */
    if (st_index[AVMEDIA_TYPE_AUDIO] >= 0) {
        stream_component_open(is, st_index[AVMEDIA_TYPE_AUDIO]);
    }

    ret = -1;
    if (st_index[AVMEDIA_TYPE_VIDEO] >= 0) {
        ret = stream_component_open(is, st_index[AVMEDIA_TYPE_VIDEO]);
    }
    if (is->show_mode == SHOW_MODE_NONE)
        is->show_mode = ret >= 0 ? SHOW_MODE_VIDEO : SHOW_MODE_RDFT;

    if (st_index[AVMEDIA_TYPE_SUBTITLE] >= 0) {
        stream_component_open(is, st_index[AVMEDIA_TYPE_SUBTITLE]);
    }

    if (is->video_stream < 0 && is->audio_stream < 0) {
        av_log(NULL, AV_LOG_FATAL, "Failed to open file '%s' or configure filtergraph\n",
               is->filename);
        ret = -1;
        goto fail;
    }

    if (infinite_buffer < 0 && is->realtime)
        infinite_buffer = 1;

    for (;;) {
        if (is->abort_request)
            break;
        if (is->paused != is->last_paused) {
            is->last_paused = is->paused;
            if (is->paused)
                is->read_pause_return = av_read_pause(ic);
            else
                av_read_play(ic);
        }
#if CONFIG_RTSP_DEMUXER || CONFIG_MMSH_PROTOCOL
        if (is->paused &&
                (!strcmp(ic->iformat->name, "rtsp") ||
                 (ic->pb && !strncmp(input_filename, "mmsh:", 5)))) {
            /* wait 10 ms to avoid trying to get another packet */
            /* XXX: horrible */
            SDL_Delay(10);
            continue;
        }
#endif
        if (is->seek_req) {
            int64_t seek_target = is->seek_pos;
            int64_t seek_min    = is->seek_rel > 0 ? seek_target - is->seek_rel + 2: INT64_MIN;
            int64_t seek_max    = is->seek_rel < 0 ? seek_target - is->seek_rel - 2: INT64_MAX;
// FIXME the +-2 is due to rounding being not done in the correct direction in generation
//      of the seek_pos/seek_rel variables

            ret = avformat_seek_file(is->ic, -1, seek_min, seek_target, seek_max, is->seek_flags);
            if (ret < 0) {
                av_log(NULL, AV_LOG_ERROR,
                       "%s: error while seeking\n", is->ic->filename);
            } else {
                if (is->audio_stream >= 0) {
                    packet_queue_flush(&is->audioq);
                    packet_queue_put(&is->audioq, &flush_pkt);
                }
                if (is->subtitle_stream >= 0) {
                    packet_queue_flush(&is->subtitleq);
                    packet_queue_put(&is->subtitleq, &flush_pkt);
                }
                if (is->video_stream >= 0) {
                    packet_queue_flush(&is->videoq);
                    packet_queue_put(&is->videoq, &flush_pkt);
                }
                if (is->seek_flags & AVSEEK_FLAG_BYTE) {
                   set_clock(&is->extclk, NAN, 0);
                } else {
                   set_clock(&is->extclk, seek_target / (double)AV_TIME_BASE, 0);
                }
            }
            is->seek_req = 0;
            is->queue_attachments_req = 1;
            is->eof = 0;
            if (is->paused)
                step_to_next_frame(is);
        }
        if (is->queue_attachments_req) {
            if (is->video_st && is->video_st->disposition & AV_DISPOSITION_ATTACHED_PIC) {
                AVPacket copy = { 0 };
                if ((ret = av_packet_ref(&copy, &is->video_st->attached_pic)) < 0)
                    goto fail;
                packet_queue_put(&is->videoq, &copy);
                packet_queue_put_nullpacket(&is->videoq, is->video_stream);
            }
            is->queue_attachments_req = 0;
        }

        /* if the queue are full, no need to read more */
        if (infinite_buffer<1 &&
              (is->audioq.size + is->videoq.size + is->subtitleq.size > MAX_QUEUE_SIZE
            || (stream_has_enough_packets(is->audio_st, is->audio_stream, &is->audioq) &&
                stream_has_enough_packets(is->video_st, is->video_stream, &is->videoq) &&
                stream_has_enough_packets(is->subtitle_st, is->subtitle_stream, &is->subtitleq)))) {
            /* wait 10 ms */
            SDL_LockMutex(wait_mutex);
            SDL_CondWaitTimeout(is->continue_read_thread, wait_mutex, 10);
            SDL_UnlockMutex(wait_mutex);
            continue;
        }
        if (!is->paused &&
            (!is->audio_st || (is->auddec.finished == is->audioq.serial && frame_queue_nb_remaining(&is->sampq) == 0)) &&
            (!is->video_st || (is->viddec.finished == is->videoq.serial && frame_queue_nb_remaining(&is->pictq) == 0))) {
            if (loop != 1 && (!loop || --loop)) {
                stream_seek(is, start_time != AV_NOPTS_VALUE ? start_time : 0, 0, 0);
            } else if (autoexit) {
                ret = AVERROR_EOF;
                goto fail;
            }
        }
        ret = av_read_frame(ic, pkt);
        if (ret < 0) {
            if ((ret == AVERROR_EOF || avio_feof(ic->pb)) && !is->eof) {
                if (is->video_stream >= 0)
                    packet_queue_put_nullpacket(&is->videoq, is->video_stream);
                if (is->audio_stream >= 0)
                    packet_queue_put_nullpacket(&is->audioq, is->audio_stream);
                if (is->subtitle_stream >= 0)
                    packet_queue_put_nullpacket(&is->subtitleq, is->subtitle_stream);
                is->eof = 1;
            }
            if (ic->pb && ic->pb->error)
                break;
            SDL_LockMutex(wait_mutex);
            SDL_CondWaitTimeout(is->continue_read_thread, wait_mutex, 10);
            SDL_UnlockMutex(wait_mutex);
            continue;
        } else {
            is->eof = 0;
        }
        /* check if packet is in play range specified by user, then queue, otherwise discard */
        stream_start_time = ic->streams[pkt->stream_index]->start_time;
        pkt_ts = pkt->pts == AV_NOPTS_VALUE ? pkt->dts : pkt->pts;
        pkt_in_play_range = duration == AV_NOPTS_VALUE ||
                (pkt_ts - (stream_start_time != AV_NOPTS_VALUE ? stream_start_time : 0)) *
                av_q2d(ic->streams[pkt->stream_index]->time_base) -
                (double)(start_time != AV_NOPTS_VALUE ? start_time : 0) / 1000000
                <= ((double)duration / 1000000);
        if (pkt->stream_index == is->audio_stream && pkt_in_play_range) {
            packet_queue_put(&is->audioq, pkt);
        } else if (pkt->stream_index == is->video_stream && pkt_in_play_range
                   && !(is->video_st->disposition & AV_DISPOSITION_ATTACHED_PIC)) {
            packet_queue_put(&is->videoq, pkt);
        } else if (pkt->stream_index == is->subtitle_stream && pkt_in_play_range) {
            packet_queue_put(&is->subtitleq, pkt);
        } else {
            av_packet_unref(pkt);
        }
    }

    ret = 0;
 fail:
    if (ic && !is->ic)
        avformat_close_input(&ic);

    if (ret != 0) {
        SDL_Event event;

        event.type = FF_QUIT_EVENT;
        event.user.data1 = is;
        SDL_PushEvent(&event);
    }
    SDL_DestroyMutex(wait_mutex);
    return 0;
}
#endif
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
