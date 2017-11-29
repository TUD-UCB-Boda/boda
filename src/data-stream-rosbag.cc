// Copyright (c) 2015, Matthew W. Moskewicz <moskewcz@alumni.princeton.edu>; part of Boda framework; see LICENSE
#include"boda_tu_base.H"
#include"has_main.H"
#include"str_util.H"
#include"data-stream.H"
#include"timers.H"
#include"img_io.H"

#include <ros/ros.h>
#include <rosbag/bag.h>
#include <rosbag/view.h>

// #include <message_filters/subscriber.h>
// #include <message_filters/time_synchronizer.h>

#include <sensor_msgs/Image.h>
#include <sensor_msgs/CompressedImage.h>
#include <sensor_msgs/image_encodings.h>
#include <sensor_msgs/PointCloud2.h>
// #include <sensor_msgs/CameraInfo.h>

namespace boda 
{
  using sensor_msgs::PointField;
  //note: no need to use a vect of PointField directly; we just use the one in our temp PointCloud2
  //typedef vector< PointField > vect_PointField;
  using sensor_msgs::PointCloud2;

  typedef shared_ptr< rosbag::View > p_rosbag_view;
  typedef rosbag::MessageInstance message_instance_t;
  typedef std::deque< message_instance_t > deque_message_instance_t;
  typedef vector< deque_message_instance_t > vect_deque_message_instance_t;
  
  // if there are multiple topics, the first topic will be considered the 'primary' topic, and one data block will be
  // emitted per message on that topic. other topics will be synced to the primary topic by choosing the message from
  // each other topic closest in time to the primary topic message. in general, this means that messages on non-primary
  // topics can be dropped or stuttered. note that for some types of messages, however, multiple messages may instead be
  // merged together, such that some set of non-primary topic messages (all near in time to the primary message) are
  // emitted. in particular, one case is that, for some secondary topics, all messages will be emitted exactly once,
  // attached to the primary topic message that they are closest to.
  struct data_stream_rosbag_src_t : virtual public nesi, public data_stream_t // NESI(help="parse mxnet-brick-style-serialized data stream into data blocks",
                                    // bases=["data_stream_t"], type_id="rosbag-src")
  {
    virtual cinfo_t const * get_cinfo( void ) const; // required declaration for NESI support

    filename_t fn; //NESI(req=1,help="input filename")
    vect_string topics; //NESI(req=1,help="list of topics to read from bag")
    
    rosbag::Bag bag;
    p_rosbag_view view;
    rosbag::View::iterator vi;

    vect_deque_message_instance_t msg_deques; // deques for storing non-primary messages
    map_str_uint32_t topic_to_ix;
    
    virtual string get_pos_info_str( void ) { return strprintf( "rosbag: status <TODO>" ); }

    // returns false if we get to end-of-stream before finding a primary msg, otherwise returns true
    bool read_up_to_primary_topic_msg( void ) {
      // read messages until we get to a primary topic message.
      while( 1 ) {
        if( vi == view->end() ) { return false; }
        rosbag::MessageInstance const & msg = *vi;
        if( msg.getTopic() == topics[0] ) { return true; }
        // otherwise process/store secondary topic msg
        // TODO
        ++vi;
      }
    }

    void msg_to_db( bool const & is_primary, data_block_t & ret, message_instance_t const & msg ) {
      printf( "msg.getDataType()=%s\n", str(msg.getDataType()).c_str() );
      if( msg.getDataType() == "sensor_msgs/Image" ) {
        sensor_msgs::Image::ConstPtr img = msg.instantiate<sensor_msgs::Image>();
        //printf( "img->height=%s img->width=%s img->encoding=%s\n", str(img->height).c_str(), str(img->width).c_str(), str(img->encoding).c_str() );
        if( img->encoding != "bayer_bggr8" ) { rt_err( "unsupported image encoding in rosbag: " + img->encoding ); }
        
        assert_st( (img->height * img->step) == img->data.size() );
        p_nda_uint8_t img_nda = make_shared<nda_uint8_t>( dims_t{ vect_uint32_t{uint32_t(img->height), uint32_t(img->width)}, vect_string{ "y","x" },"uint8_t" }); // note: for now, always in in bggr format ...
        // copy image data to packed nda. FIXME: if we had nda padding, we could borrow, or at least copy in one
        // block. also if we checked for the un-padded case, we could do similar for that case at least.
        for( uint32_t y = 0; y != img->height; ++y ) { 
          uint8_t const * rb = &img->data[img->step*y];
          std::copy( rb, rb + img->width, &img_nda->at1(y) );
        }
        ret.nda = img_nda;
        ret.meta = "image";
        ret.tag = "rosbag:"+msg.getTopic();
        ros::Time const msg_time = msg.getTime();
        ret.timestamp_ns = secs_and_nsecs_to_nsecs_signed( msg_time.sec, msg_time.nsec );
        uint64_t const img_ts = secs_and_nsecs_to_nsecs_signed( img->header.stamp.sec, img->header.stamp.nsec );
        printf( "ret.timestamp_ns=%s img_ts=%s\n", str(ret.timestamp_ns).c_str(), str(img_ts).c_str() );
      }
    }
    
    virtual data_block_t proc_block( data_block_t const & db ) {
      data_block_t ret = db;
      if( vi == view->end() ) { return ret; }
      ret.subblocks = make_shared<vect_data_block_t>(topics.size());
      // we should be at a primary topic message, so consume it
      rosbag::MessageInstance const prim_msg = *vi;
      ++vi;
      assert_st( prim_msg.getTopic() == topics[0] );
      read_up_to_primary_topic_msg(); // read up to *next* primary topic msg (if there is one)

      data_block_t pdb = db;
      msg_to_db( 1, pdb, prim_msg );
      ret.subblocks->at(0) = pdb;
      ret.timestamp_ns = ret.subblocks->at(0).timestamp_ns; // use primary timestamp as timeframe timestamp
      
      for( uint32_t i = 1; i != topics.size(); ++i ) {
        data_block_t sdb = db;
        //msg_to_db( 0, sdb, prim_msg );
        ret.subblocks->at(i) = sdb;
      }

      return ret;
    }
    virtual void data_stream_init( nesi_init_arg_t * nia ) {
      if( topics.empty() ) { rt_err( "rosbag-src: must specify at least one topic (first will be primary)" ); }
      bag.open( fn.exp, rosbag::bagmode::Read );
      view = make_shared< rosbag::View >( bag, rosbag::TopicQuery(topics) );
      vi = view->begin();
      for( uint32_t i = 0; i != topics.size(); ++i ) { must_insert( topic_to_ix, topics[i], i ); }
      read_up_to_primary_topic_msg();
    }
  };

  struct pc2_point {
    float x,y,z,intensity;
    uint16_t ring;
  };

  float const meters_to_feet = 3.28084;
  
  struct data_stream_rosbag_sink_t : virtual public nesi, public data_stream_t // NESI(help="parse mxnet-brick-style-serialized data stream into data blocks",
                                     // bases=["data_stream_t"], type_id="rosbag-sink")
  {
    virtual cinfo_t const * get_cinfo( void ) const; // required declaration for NESI support

    filename_t fn; //NESI(req=1,help="output filename")
    uint32_t verbose; //NESI(default="0",help="verbosity level (max 99)")
    uint32_t append_mode; //NESI(default="0",help="if 1, open bag for append. otherwise, open for writing.")
    uint32_t rot_90; //NESI(default="0",help="if 1, rotate data 90 CW (x_ros=y_in; y_ros=-x_in).")
    uint32_t compress_images_as_jpeg; //NESI(default="0",help="if 1, compress images as jpeg. FIXME/NOTE: can't seem to view the compressed images in rviz, so maybe not such a usefull option currently.")
    float scale_xy; //NESI(default="1.0",help="scale xy points by this value")

    string frame_id; //NESI(default="base_link",help="for output msg headers, what frame id to use")

    vect_string topics; //NESI(req=1,help="list of topics to write to bag, one per sub-block. to omit a topic, use an empty name.")
    rosbag::Bag bag;

    std_msgs::Header msg_header; // used as template/temporary.
    PointCloud2 pc2; // used as template/temporary.
    
    virtual string get_pos_info_str( void ) { return strprintf( "rosbag: status <TODO>" ); }

    virtual data_block_t proc_block( data_block_t const & db ) {
      if( topics.size() != db.num_subblocks() ) {
        rt_err( strprintf( "topics.size()=%s must equal db.num_subblocks()=%s\n",
                           str(topics.size()).c_str(), str(db.num_subblocks()).c_str() ) );
      }
      if( verbose ) { printf( "rosbag-sink: db.info_str()=%s\n", db.info_str().c_str() ); }
      
      assert_st( db.has_subblocks() );
      for( uint32_t i = 0; i != db.subblocks->size(); ++i ) { write_db_to_bag( db.subblocks->at(i), topics.at(i) ); }
      return db;
    }
    void write_db_to_bag( data_block_t const & db, string const & topic ) {
      if( topic.empty() ) { return; } // skip if directed to do so
      ros::Time ros_ts;
      ros_ts.fromNSec( db.timestamp_ns ); // note: we'll use this for both the 'recv' and 'header' timestamp for our gen'd message
      msg_header.seq = db.frame_ix;
      msg_header.stamp = ros_ts;
      msg_header.frame_id = frame_id;

      if( 0 ) { }
      else if( startswith( db.meta, "image" ) || startswith( db.meta, "IMAGEDATA" ) ) {
        if( !db.as_img ) { rt_err( "rosbag-sink: image: expected as_img to be non-null" ); }
        if( compress_images_as_jpeg ) {
          sensor_msgs::CompressedImagePtr img = boost::make_shared< sensor_msgs::CompressedImage >();
          img->header = msg_header;
          img->format = "jpeg";
          p_uint8_with_sz_t img_jpeg = db.as_img->to_jpeg();
          img->data.resize( img_jpeg.sz );
          std::copy( img_jpeg.get(), img_jpeg.get() + img_jpeg.sz, &img->data[0] );
          bag.write( topic + "/compressed", ros_ts, img );
        } else {
          sensor_msgs::ImagePtr img = boost::make_shared< sensor_msgs::Image >();
          img->header = msg_header;
          img->width = db.as_img->sz.d[0];
          img->height = db.as_img->sz.d[1];
          img->encoding = sensor_msgs::image_encodings::RGBA8;
          img->step = db.as_img->row_pitch;
          assert_st( (img->height * img->step) == db.as_img->sz_raw_bytes() );
          img->data.resize( db.as_img->sz_raw_bytes() );
          std::copy( db.as_img->pels.get(), db.as_img->pels.get() + db.as_img->sz_raw_bytes(), &img->data[0] );
          bag.write( topic, ros_ts, img );
        }
      } else if( startswith( db.meta, "pointcloud" ) ) {
        //sensor_msgs::PointCloud2Ptr pc2 = boost::make_shared< sensor_msgs::PointCloud2 >();
        pc2.header = msg_header;

        // p_nda_float_t xyz_nda = make_shared<nda_float_t>( dims_t{ dims_t{ { xy_sz.d[1], xy_sz.d[0], 3 }, {"y","x","xyz"}, "float" }} );
        assert_st( db.nda );
        p_nda_float_t xyz_nda = make_shared< nda_float_t >( db.nda );
        u32_pt_t xy_sz = get_xy_dims( xyz_nda->dims );

        pc2.width = xy_sz.d[0];
        pc2.height = xy_sz.d[1];
        pc2.row_step = pc2.point_step * pc2.width;
        pc2.data.resize( pc2.row_step * pc2.height, 0 );

        pc2_point pt;
        uint8_t * out = &pc2.data[0];
        for( uint32_t y = 0; y != xy_sz.d[1] ; ++y ) {
          for( uint32_t x = 0; x != xy_sz.d[0] ; ++x ) {
            float * const xyz = &xyz_nda->at2(y,x);
            pt.x = xyz[0]; pt.y = xyz[1]; pt.z = xyz[2]; pt.intensity = 50; pt.ring = y;
            if( rot_90 ) { std::swap(pt.x,pt.y); pt.y = -pt.y; }
            if( 1 ) { pt.x *= scale_xy; pt.y *= scale_xy; }
            std::copy( (uint8_t const *)&pt, (uint8_t const *)&pt + sizeof(pt), out );
            out += pc2.point_step;
          }
        }

        bag.write( topic, ros_ts, pc2 );

        // NOTE: ros axis conventions:
        // in relation to body (i.e. 'body_link'); colors in ()s are as in rviz TF view (best guess currently)
        //   x forward (red)
        //   y left (green)
        //   z up (blue)

#if 0
        // FIXME/NOTE: this untested/unused code is from an SO post about writing transforms to a bag, and seems
        // plausible. if we want to use our own frame (i.e. the camera frame), this might be one way to do it.
        // see: https://answers.ros.org/question/65556/write-a-tfmessage-to-bag-file/
        geometry_msgs::TransformStamped msg;
        msg.header = msg_header; 
        msg.child_frame_id = msg_header.frame_id;
        msg.header.frame_id = base_frame_id; // FIXME: set base frame to proper frame here
        msg.transform.translation.x = ???; // FIXME: set xform properly
        tf::tfMessage message;
        message.transforms.push_back( msg );
        bag.write("tf", ros_ts, message); // MWM FIXME: use leading slash for tf topic, i.e. '/tf'?
#endif
        
      } else { rt_err( "rosbag-sink: unhandled db with meta=" + db.meta ); }
    }      
    
    virtual void data_stream_init( nesi_init_arg_t * nia ) {
      bag.open( fn.exp, append_mode ? rosbag::bagmode::Append : rosbag::bagmode::Write );
      uint32_t offset = 0;
      vect_string float_fields = {"x","y","z","intensity"}; // note: float type = 7
      PointField pf;
      pf.count = 1;
      pf.datatype = 7;
      for( vect_string::const_iterator i = float_fields.begin(); i != float_fields.end(); ++i ) {
        pf.name = *i;
        pf.offset = offset;
        pc2.fields.push_back( pf );
        offset += sizeof(float);
      }
      // ring, type: uint16_t=4
      pf.name = "ring";
      pf.datatype = 4;
      pf.offset = offset;
      offset += sizeof( uint16_t );
      offset += 2; // FIXME: should we be adding this padding?
      pc2.fields.push_back( pf );

      pc2.is_bigendian = 0;
      pc2.point_step = offset;
      // pc2.row_step --> set per-msg to pc2.point_step * width (although probably constant across msgs)
      // pc2.data --> set per-msg 
      pc2.is_dense = 1;
      
    }
  };
  
#include"gen/data-stream-rosbag.cc.nesi_gen.cc"

}