// Copyright (c) 2013-2014, Matthew W. Moskewicz <moskewcz@alumni.princeton.edu>; part of Boda framework; see LICENSE
#ifndef MWM_str_util
#define MWM_str_util

#include<string>
#include<vector>
#include<exception>
#include<sstream>
#include<stdio.h>
#include<string.h>

namespace boda
{
  string hex( string const & s );
  string unhex( string const & s );

  string cxx_demangle( string const & sym );

  double lc_str_d( char const * const s );
  uint32_t lc_str_u32( char const * const s );
  uint64_t lc_str_u64( char const * const s );
  int32_t lc_str_i32( char const * const s );
  int64_t lc_str_i64( char const * const s );
  
  double lc_str_d( std::string const & s );
  uint32_t lc_str_u32( std::string const & s );
  uint64_t lc_str_u64( std::string const & s );
  int32_t lc_str_i32( std::string const & s );
  int64_t lc_str_i64( std::string const & s );

  typedef std::vector< std::string > vect_string;

  std::string strprintf( char const * const format, ... ) __attribute__( (format(__printf__, 1, 2) ) );
  void printstr( std::string const & str );

  filename_t filename_t_printf( filename_t const & fn, ... ); // note: can't use gcc format-string checking here

  std::string xml_escape( std::string const & str );
  std::string shell_escape( std::string const & str );

  inline bool endswith( std::string const & s, std::string const & suf ) {
    if( s.size() < suf.size() ) { return false; }
    size_t const off( s.size() - suf.size() );
    for( size_t i = 0; i < suf.size(); i++ ) {
      if( suf[i] != s[i+off] ) { return false; }
    }
    return true;
  }
  inline bool startswith( std::string const & s, std::string const & pref ) {
    if( s.size() < pref.size() ) { return false; }
    for( size_t i = 0; i < pref.size(); i++ ) {
      if( pref[i] != s[i] ) { return false; }
    }
    return true;
  }
  string get_part_before( string const & s, string const & to_find );
  string get_part_after( string const & s, string const & to_find );
  string replace_chars_with_char( string const & s, string const & chars_to_find, char const rep_with );
  bool maybe_replace_str_with_str( string & s, string const & find_s, string const & rep_s );
  string strip_ending_chars( string const & s, string const & chars_to_strip );

  std::string join( vect_string const & vs, std::string const & sep );

  string as_py_str_list( vect_string const & vs ); // sticks \"...\" around strings, joins them with ',', and wraps in []s
  std::string as_pyid_fixme( string const & s ); // only a single use left, deprecated

  vect_string split( std::string const & s, char const sep );
  vect_string split_ws( std::string const & s );
  vect_string split_space_tab_colon( std::string const & s );

  string strip_ws( std::string const & s );
  void strip_ws_inplace( std::string & s );

  inline void strip_prefixes( std::string & s, std::string const & prefix ) {
    while( startswith(s,prefix) ) { s = std::string( s, prefix.size() ); }
  }
  
  inline void strip_suffs( std::string & s, std::string const & suff ) {
    while( endswith(s,suff) ) { s = s.substr( 0, s.size() - suff.size() ); }
  }
  inline bool maybe_strip_suffix( std::string & s, std::string const & suff ) {
    if( endswith(s,suff) ) { s = s.substr( 0, s.size() - suff.size() ); return 1; } return 0;
  }
  inline bool maybe_strip_prefix( std::string & s, std::string const & prefix ) {
    if( startswith(s,prefix) ) { s = s.substr( prefix.size(), string::npos ); return 1; } return 0;
  }

  template< typename T > inline std::ostream & operator << ( std::ostream & out, vector< T > const & v ) {
    for( typename vector< T >::const_iterator i = v.begin(); i != v.end(); ++i ) {
      if( i != v.begin() ) { out << " "; }
      out << (*i);
    }
    return out;
  }

  template< typename T > inline std::ostream & operator << ( std::ostream & out, set< T > const & v ) {
    for( typename set< T >::const_iterator i = v.begin(); i != v.end(); ++i ) {
      if( i != v.begin() ) { out << " "; }
      out << (*i);
    }
    return out;
  }

  template< typename T > inline std::ostream & operator << ( std::ostream & out, shared_ptr< T > const & v ) { return out << (*v); }

  template< typename T > inline std::string str(T const & i) { // convert T i to string
    std::stringstream s;
    s << i;
    return s.str();
  }

  // convert T i to string, but force uint8_t to wider/numeric type first
  template< typename T > inline std::string strN(T const & i) { return str(i); }
  template<> inline std::string strN( uint8_t const & i ) { return str(uint16_t(i)); }
  template<> inline std::string strN( int8_t const & i ) { return str(int16_t(i)); }

  
  struct string_scoped_prefixer_t {
    std::string & s;
    uint32_t const orig_sz; 
    string_scoped_prefixer_t( std::string & s_, std::string const & prefix ) : s(s_), orig_sz( s.size() ) { s += prefix; }
    ~string_scoped_prefixer_t( void ) { s.resize( orig_sz ); }
  };

  class str_exception : public std::exception {
  public:
    char const * const msg;
    str_exception( char const * const msg_ ) : msg(strdup(msg_)) { }
    str_exception( std::string const & msg_ ) : msg(strdup(msg_.c_str())) { }
    virtual char const * what() const throw() { return msg; }
  };

  // pretty printing (with units)
  std::string pp_val( double const & orig_v );

  std::string pp_secs( double const & v, bool const & verbose=0 );
  std::string pp_flops( double const & v, bool const & verbose=0 );
  std::string pp_bytes( double const & v, bool const & verbose=0 );
  std::string pp_bps( double const & v, bool const & verbose=0 );
  std::string pp_fpb( double const & v, bool const & verbose=0 );
  std::string pp_fps( double const & v, bool const & verbose=0 );
  std::string pp_fpspw( double const & v, bool const & verbose=0 );
  std::string pp_joules( double const & v, bool const & verbose=0 );

}

#endif // MWM_str_util
