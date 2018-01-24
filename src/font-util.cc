// Copyright (c) 2013-2014, Matthew W. Moskewicz <moskewcz@alumni.princeton.edu>; part of Boda framework; see LICENSE
#include"boda_tu_base.H"
#include"font-util.H"
#include"has_main.H"
#define STB_TRUETYPE_IMPLEMENTATION  // force following include to generate implementation
#include"ext/stb_truetype.h"


namespace boda
{

  typedef map< int, p_rendered_char_t > rendered_char_cache_t;
  
  struct ttf_font_render_t : public virtual nesi, public font_render_t // NESI(help="stb_truetype-based ttf-font-rendering to bitmaps", bases=["font_render_t"], type_id="ttf" )
  {
    virtual cinfo_t const * get_cinfo( void ) const; // required declaration for NESI support
    filename_t font_fn; //NESI(default="%(boda_dir)/fonts/DroidSansMono.ttf",help="ttf font filename")
    uint32_t pixel_height; //NESI(default=20,help="desired pixel height")

    p_string font_data;
    stbtt_fontinfo font;
    float scale;

    int ascent;
    int baseline;

    rendered_char_cache_t rcc;
    
    void lazy_init( void ) {
      if( font_data ) { return; }
      font_data = read_whole_fn( font_fn );
      uint8_t * const rp_font_data = (uint8_t * const)&font_data->at(0);
      int const font_offset = stbtt_GetFontOffsetForIndex(rp_font_data,0); // FIXME: doesn't take len of data, so presumably unsafe ...
      if( font_offset == -1 ) { rt_err( "stbtt_GetFontOffsetForIndex() failed" ); }
      int const ret = stbtt_InitFont( &font, rp_font_data, font_offset );
      if( ret == 0 ) { rt_err( "stbtt_InitFont() failed" ); }

      scale = stbtt_ScaleForPixelHeight(&font, pixel_height);
      stbtt_GetFontVMetrics(&font, &ascent,0,0);
      baseline = (int) (ascent*scale);

    }

    virtual p_rendered_char_t render_char( int8_t const & c ) {
      lazy_init();
      p_rendered_char_t & rc = rcc[c];
      if( !rc ) {
        rc = make_shared<rendered_char_t>();
        rc->bitmap = p_uint8_t( stbtt_GetCodepointBitmap(&font, scale, scale, c, &rc->w, &rc->h, &rc->xoff, &rc->yoff ), free );
        int advance, lsb;
        stbtt_GetCodepointHMetrics(&font, c, &advance, &lsb);
        rc->advance_pels = advance * scale;
        rc->lsb_pels = lsb * scale;
      }
      return rc;
    }
    // FIXME: cache glyph indexes for this?
    virtual int kern_advance_pels( int8_t const & c1, int8_t const & c2 ) {
      return scale * stbtt_GetCodepointKernAdvance(&font,c1,c2);
    }
  };

#if 0 // string render example
   while (text[ch]) {
      int advance,lsb,x0,y0,x1,y1;
      float x_shift = xpos - (float) floor(xpos);
      stbtt_GetCodepointHMetrics(&font, text[ch], &advance, &lsb);
      stbtt_GetCodepointBitmapBoxSubpixel(&font, text[ch], scale,scale,x_shift,0, &x0,&y0,&x1,&y1);
      stbtt_MakeCodepointBitmapSubpixel(&font, &screen[baseline + y0][(int) xpos + x0], x1-x0,y1-y0, 79, scale,scale,x_shift,0, text[ch]);
      // note that this stomps the old data, so where character boxes overlap (e.g. 'lj') it's wrong
      // because this API is really for baking character bitmaps into textures. if you want to render
      // a sequence of characters, you really need to render each bitmap to a temp buffer, then
      // "alpha blend" that into the working buffer
      xpos += (advance * scale);
      if (text[ch+1])
         xpos += scale*stbtt_GetCodepointKernAdvance(&font, text[ch],text[ch+1]);
      ++ch;
   }
#endif

  struct test_font_util_t : public virtual nesi, public has_main_t // NESI(help="test of stb_truetype/font-rendering", bases=["has_main_t"], type_id="test-font-util" )
  {
    virtual cinfo_t const * get_cinfo( void ) const; // required declaration for NESI support

    p_font_render_t font_renderer; //NESI(default="(be=ttf)",help="font renderer to use for test")

    string to_render; //NESI(default="a",help="string to render")
    virtual void main( nesi_init_arg_t * nia ) {
      printf( "test-font-util main() begins.\n" );

      for( string::const_iterator c = to_render.begin(); c != to_render.end(); ++c ) {
        p_rendered_char_t rc = font_renderer->render_char( *c );
        for (int j=0; j < rc->h; ++j) {
          for (int i=0; i < rc->w; ++i)
            putchar(" .:ioVM@"[rc->bitmap.get()[j*rc->w+i]>>5]);
          putchar('\n');
        }
      }
    }

  };

#include"gen/font-util.H.nesi_gen.cc"
#include"gen/font-util.cc.nesi_gen.cc"

}
