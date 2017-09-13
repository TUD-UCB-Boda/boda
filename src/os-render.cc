// Copyright (c) 2015, Matthew W. Moskewicz <moskewcz@alumni.princeton.edu>; part of Boda framework; see LICENSE
#include"boda_tu_base.H"
#include"os-render.H"
#include"img_io.H"
#include"has_main.H"
#include"str_util.H"
#include"rand_util.H"
#include"data-stream-velo.H"

#include"GL/glew.h"
#define GLAPI extern 
#include"GL/osmesa.h"

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtx/transform.hpp>

using namespace glm;

#include"ext/shader.hpp"


namespace boda 
{
  void _check_gl_error( char const * const tag, char const * const file, int const line ) {
    while( 1 ) {
      GLenum const err = glGetError();
      if( err== GL_NO_ERROR ) { break; }
      string error;
      switch(err) {
      case GL_INVALID_OPERATION:      error="INVALID_OPERATION";      break;
      case GL_INVALID_ENUM:           error="INVALID_ENUM";           break;
      case GL_INVALID_VALUE:          error="INVALID_VALUE";          break;
      case GL_OUT_OF_MEMORY:          error="OUT_OF_MEMORY";          break;
      case GL_INVALID_FRAMEBUFFER_OPERATION:  error="INVALID_FRAMEBUFFER_OPERATION";  break;
      }
      printf( "error: tag=%s GL_%s file=%s line=%s\n", tag, error.c_str(), file, str(line).c_str() );
    }
    // rt_err( "one or more GL errors, aborting" );
  }
#define check_gl_error(tag) _check_gl_error(tag,__FILE__,__LINE__)

  
  string const vertex_shader_code_str = R"xxx(
#version 330 core

// Input vertex data, different for all executions of this shader.
layout(location = 0) in vec3 vertexPosition_modelspace;

// Output data ; will be interpolated for each fragment.
out vec3 fragmentColor;
// Values that stay constant for the whole mesh.
uniform mat4 MVP;

void main(){	
  // Output position of the vertex, in clip space : MVP * position
  gl_Position =  MVP * vec4(vertexPosition_modelspace,1);
  fragmentColor = vec3(.4,.4,.4);
}

)xxx";

  string const fragment_shader_code_str = R"xxx(
#version 330 core

// Interpolated values from the vertex shaders
in vec3 fragmentColor;

// Ouput data
out vec3 color;

void main(){

	// Output color = color specified in the vertex shader, 
	// interpolated between all 3 surrounding vertices
	color = fragmentColor;

})xxx";
  
  struct data_to_img_pts_t : virtual public nesi, public data_stream_t // NESI(help="annotate data blocks (containing point cloud data) with image representations (in as_img field of data block). returns annotated data block.",
                           // bases=["data_stream_t"], type_id="add-img-pts")
  {
    virtual cinfo_t const * get_cinfo( void ) const; // required declaration for NESI support
    filename_t cloud_vertex_shader_fn; //NESI(default="%(boda_dir)/shaders/cloud-vertex.glsl",help="point cloud vertex shader filename")
    p_filename_t velo_cfg; //NESI(help="xml config filename (optional; will try to read from stream if not present. but note, do need config somehow, from stream or file!")
    uint32_t verbose; //NESI(default="0",help="verbosity level (max 99)")
    u32_pt_t disp_sz; //NESI(default="600:300",help="X/Y per-stream-image size")
    double cam_scale; //NESI(default="1.0",help="scale camera pos by this amount")
    float start_z; //NESI(default="50.0",help="starting z value for camera")

    uint32_t grid_cells; //NESI(default="10",help="number of X/Y grid cells to draw")
    float grid_cell_sz; //NESI(default="10.0",help="size of each grid cell")
    
    
    p_img_t frame_buf;

    OSMesaContext ctx;
    GLuint programID;
    GLuint mvp_id;
    
    GLuint cloud_programID;
    GLuint cloud_mvp_id;
    GLuint cloud_hbins_id;
    GLuint cloud_lasers_id;
    //GLuint mode_uid;

    glm::mat4 MVP;
    
    float cam_pos[3];
    float cam_rot[3];

    boost::random::mt19937 gen;
    
    virtual void set_opt( data_stream_opt_t const & opt ) {
      if( opt.name == "camera-pos-rot" ) {
        if( (!opt.val.get()) || (opt.val->dims.tn != "float") || (opt.val->elems_sz() != 6) ) {
          rt_err( "add-img-pts: couldn't parse camera-pos opt" );
        }
        nda_T<float> cam_pos_rot( opt.val );
        for( uint32_t i = 0; i != 3; ++i ) {
          cam_pos[i] = cam_pos_rot.at2(0,i) * cam_scale;
          cam_rot[i] = glm::radians(cam_pos_rot.at2(1,i));
        }
        cam_pos[2] += start_z; 
        //printf( "cam_pos[0]=%s cam_pos[1]=%s cam_pos[2]=%s\n", str(cam_pos[0]).c_str(), str(cam_pos[1]).c_str(), str(cam_pos[2]).c_str() );
        //printf( "cam_rot[0]=%s cam_rot[1]=%s cam_rot[2]=%s\n", str(cam_rot[0]).c_str(), str(cam_rot[1]).c_str(), str(cam_rot[2]).c_str() );
      }
      
    }

    p_nda_float_t grid_pts;
    GLuint grid_pts_buf;

    void draw_grid( void ) {
      glUseProgram(programID);
      glUniformMatrix4fv(mvp_id, 1, GL_FALSE, &MVP[0][0]);
      glEnableVertexAttribArray(0);
      glBindBuffer(GL_ARRAY_BUFFER, grid_pts_buf);
      glVertexAttribPointer( 0, 3, GL_FLOAT, GL_FALSE, 0, (void *) 0 ); 
      glDrawArrays(GL_LINES, 0, grid_pts->elems_sz() );
      glDisableVertexAttribArray(0);
    }

    void init_grid( void ) {
      float const half = grid_cells * grid_cell_sz / 2.0f;
      grid_pts = make_shared<nda_float_t>( dims_t{ {grid_cells+1,2,2,3}, {"cell","xy","be","d"}, "float" } );
      for( uint32_t cell = 0; cell != grid_cells+1; ++cell ) {
        for( uint32_t xy = 0; xy != 2; ++xy ) {
          for( uint32_t d = 0; d != 2; ++d ) {
            float x = d ? half : -half;
            float y = (cell*grid_cell_sz - half);
            if( xy ) { std::swap(x,y); }
            (glm::vec3 &)grid_pts->at3(cell,xy,d) = glm::vec3( x, y, 0.0f );
          }
        }
      }
      glGenBuffers(1, &grid_pts_buf);
      glBindBuffer(GL_ARRAY_BUFFER, grid_pts_buf );
      glBufferData(GL_ARRAY_BUFFER, grid_pts->dims.bytes_sz(), grid_pts->rp_elems(), GL_STATIC_DRAW);
      glBindBuffer(GL_ARRAY_BUFFER, 0 );
      glLineWidth( 2.0f );
    }

    GLuint cloud_pts_buf;
    GLuint cloud_lut_buf;
    GLuint cloud_lut_tex;
    GLint cloud_lut_tex_id;
    
    void draw_cloud( data_block_t const & db ) {
      p_nda_t const & nda = db.nda;
      glBindBuffer(GL_ARRAY_BUFFER, cloud_pts_buf );
      glBufferData(GL_ARRAY_BUFFER, nda->dims.bytes_sz(), nda->rp_elems(), GL_STREAM_DRAW);

      glUseProgram(cloud_programID);

      glUniformMatrix4fv(cloud_mvp_id, 1, GL_FALSE, &MVP[0][0]);
      if( nda->dims.sz() != 2 ) {
        rt_err( strprintf( "expected 2D-array for point cloud, but had nda->dims=%s\n", str(nda->dims).c_str() ) ); 
      }
      glUniform1ui(cloud_lasers_id, nda->dims.dims(0) );
      glUniform1ui(cloud_hbins_id, nda->dims.dims(1) );
      
      glEnableVertexAttribArray(0);
      glBindBuffer(GL_ARRAY_BUFFER, cloud_pts_buf);
      glVertexAttribPointer( 0, 1, GL_UNSIGNED_SHORT, GL_FALSE, 0, (void *) 0 );

      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_BUFFER, cloud_lut_tex);
      glUniform1i(cloud_lut_tex_id, 0);
      
      glDrawArrays(GL_POINTS, 0, nda->elems_sz() );
      glDisableVertexAttribArray(0);
    }

    void init_cloud( void ) {
      glGenBuffers(1, &cloud_pts_buf);
      glGenBuffers(1, &cloud_lut_buf);
      glGenTextures(1, &cloud_lut_tex);
    }

    void bind_laser_corrs( void ) {
      assert_st( laser_corrs.size() == 64 );
      glBindBuffer(GL_TEXTURE_BUFFER, cloud_lut_buf);
      glBufferData(GL_TEXTURE_BUFFER, laser_corrs.size()*sizeof(laser_corrs[0]), laser_corrs.data(), GL_STATIC_DRAW);
      glBindBuffer(GL_TEXTURE_BUFFER, 0);
      
      glBindTexture(GL_TEXTURE_BUFFER, cloud_lut_tex);
      glTexBuffer(GL_TEXTURE_BUFFER, GL_R32F, cloud_lut_buf);

      if( velo_cfg.get() ) { read_velo_config( *velo_cfg, laser_corrs ); bind_laser_corrs(); }
      
    }

    vect_laser_corr_t laser_corrs;
    
    virtual void data_stream_init( nesi_init_arg_t * const nia ) {
      
      for( uint32_t i = 0; i != 3; ++i ) { cam_pos[i] = 0.0f; cam_rot[i] = 0.0f; }
      frame_buf = make_shared< img_t >();
      frame_buf->set_sz_and_alloc_pels( disp_sz );
      int const osmesa_attrs[] = {
        OSMESA_FORMAT, OSMESA_RGBA,
        OSMESA_DEPTH_BITS, 16,
        OSMESA_STENCIL_BITS, 0,
        OSMESA_ACCUM_BITS, 0,
        OSMESA_PROFILE, OSMESA_CORE_PROFILE,
        OSMESA_CONTEXT_MAJOR_VERSION, 3,
        OSMESA_CONTEXT_MINOR_VERSION, 3,
        0 };
    
//      ctx = OSMesaCreateContextExt( OSMESA_RGBA, 16, 0, 0, NULL );
       ctx = OSMesaCreateContextAttribs( osmesa_attrs, NULL );
      if (!ctx) { rt_err("OSMesaCreateContext failed!"); }
      
      if (!OSMesaMakeCurrent( ctx, frame_buf->get_row_addr(0), GL_UNSIGNED_BYTE, frame_buf->sz.d[0], frame_buf->sz.d[1] )) {
        rt_err("OSMesaMakeCurrent failed.\n");
      }

      // Initialize GLEW
      glewExperimental = true; // Needed for core profile
      if (glewInit() != GLEW_OK) { rt_err( "Failed to initialize GLEW" ); }

      
      {
        int z, s, a;
        glGetIntegerv(GL_DEPTH_BITS, &z);
        glGetIntegerv(GL_STENCIL_BITS, &s);
        glGetIntegerv(GL_ACCUM_RED_BITS, &a);
        printf("Depth=%d Stencil=%d Accum=%d\n", z, s, a);
        printf( "GL_VERSION: %s\nGL_VENDOR: %s\nGL_RENDERER: %s\n", glGetString(GL_VERSION), glGetString(GL_VENDOR), glGetString(GL_RENDERER) );
        
      }

      // Dark blue background
      glClearColor(0.0f, 0.0f, 0.3f, 0.0f);

      // Enable depth test
      glEnable(GL_DEPTH_TEST);
      glEnable(GL_PROGRAM_POINT_SIZE);
      // Accept fragment if it closer to the camera than the former one
      glDepthFunc(GL_LESS); 


      GLuint VertexArrayID;
      glGenVertexArrays(1, &VertexArrayID);
      glBindVertexArray(VertexArrayID);
      
      // Create and compile our GLSL program from the shaders
      programID = LoadShaders( vertex_shader_code_str, fragment_shader_code_str );
      printf( "programID=%s\n", str(programID).c_str() );
      mvp_id = glGetUniformLocation(programID, "MVP");
      
      cloud_programID = LoadShaders( *read_whole_fn( cloud_vertex_shader_fn ), fragment_shader_code_str );
      cloud_mvp_id = glGetUniformLocation(cloud_programID, "MVP");
      cloud_lasers_id = glGetUniformLocation(cloud_programID, "lasers");
      cloud_hbins_id = glGetUniformLocation(cloud_programID, "hbins");
      cloud_lut_tex_id = glGetUniformLocation(cloud_programID, "lut_tex");
      
      printf( "cloud_programID=%s\n", str(cloud_programID).c_str() );

      init_grid();
      init_cloud();
      check_gl_error( "init" );
    }
    
    virtual data_block_t proc_block( data_block_t const & db ) {
      if( !db.nda.get() ) { rt_err( "add-img-pts: expected nda data in block, but found none." ); }
      if( db.has_subblocks() ) {
        for( uint32_t i = 0; i != db.subblocks->size(); ++i ) {
          data_block_t const & sdb = db.subblocks->at(i);
          if( sdb.meta == "lidar-corrections" ) {
            if( laser_corrs.empty() ) {
              p_nda_t const & laser_corrs_nda = sdb.nda;
              assert_st( laser_corrs_nda->dims.sz() == 2 );
              assert_st( laser_corrs_nda->dims.strides(0)*sizeof(float) == sizeof(laser_corr_t) );
              laser_corr_t const * laser_corr = (laser_corr_t const *)laser_corrs_nda->rp_elems();
              for( uint32_t i = 0; i != laser_corrs_nda->dims.dims(0); ++i ) {
                laser_corrs.push_back( *laser_corr );
                //printf( "i=%s (*laser_corr)=%s\n", str(i).c_str(), str((*laser_corr)).c_str() );
                ++laser_corr;
              }
              bind_laser_corrs();
            }
          }
          else {
            rt_err( strprintf( "os-render: unknown subblock with meta=%s\n", str(sdb.meta).c_str() ) ); // could maybe just skip/ignore
          }
        }
      }
      render_pts_into_frame_buf( db );
      data_block_t ret = db;
      ret.as_img = frame_buf;
      return ret;
    }

    virtual void render_pts_into_frame_buf( data_block_t const & db ) {
      check_gl_error( "preframe" );
      //mode_uid = glGetUniformLocation(programID, "mode");
      // Projection matrix : 45° Field of View, 4:3 ratio, display range : 0.1 unit <-> 100 units
      glm::mat4 Projection = glm::perspective(glm::radians(60.0f), float( frame_buf->sz.d[0] ) / float( frame_buf->sz.d[1] ), 0.1f, 1000.0f);
      // Or, for an ortho camera :
      //glm::mat4 Projection = glm::ortho(-10.0f,10.0f,-10.0f,10.0f,0.0f,100.0f); // In world coordinates
#if 1
      glm::mat4 R = glm::rotate(-cam_rot[0], glm::vec3(0.0f, 0.0f, 1.0f)) * glm::rotate(-cam_rot[1], glm::vec3(1.0f, 0.0f, 0.0f));
      glm::vec3 T = glm::vec3(0, 0, cam_pos[2]);   
      glm::vec3 position = glm::vec3(R * glm::vec4(T,0.0f)); 
      glm::vec3 m_direction = glm::vec3(0,0,0);
      glm::vec3 m_real_up = glm::vec3(0,1,0);
      glm::vec3 m_up = glm::vec3(R * glm::vec4(m_real_up, 0.0f)); 
      glm::mat4 TPan = glm::translate( glm::mat4(), glm::vec3(-cam_pos[0], -cam_pos[1], 0) );
      glm::mat4 View = TPan * glm::lookAt(position, m_direction, m_up);
#else
      // Camera matrix
      glm::mat4 View       = glm::lookAt(
        glm::vec3(4,3,3), // Camera is at (4,3,3), in World Space
        glm::vec3(0,0,0), // and looks at the origin
        glm::vec3(0,1,0)  // Head is up (set to 0,-1,0 to look upside-down)
                                         );
#endif
      // Model matrix : an identity matrix (model will be at the origin)
      glm::mat4 Model      = glm::mat4(1.0f);
      // Our ModelViewProjection : multiplication of our 3 matrices
      MVP = Projection * View * Model; // Remember, matrix multiplication is the other way around
      
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); // Clear the screen
      draw_grid();
      draw_cloud( db );
      glFinish();
      check_gl_error( "postframe" );

#if 0                
	// Cleanup VBO
	glDeleteBuffers(1, &vertexbuffer);
	glDeleteVertexArrays(1, &VertexArrayID);
	glDeleteProgram(programID);
#endif      
    }

    virtual string get_pos_info_str( void ) {      
      return strprintf( "data-to-img: disp_sz=%s", str(disp_sz).c_str() );
    }

    
  };

#include"gen/os-render.cc.nesi_gen.cc"

}
