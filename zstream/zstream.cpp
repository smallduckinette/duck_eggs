/* This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the WTFPLv2. See the LICENSE
 * file for more details. */

#include "zstream.h"

#include <stdexcept>
#include <zlib.h>

namespace
{
  const int chunk = 256 * 1024;
}

void compress(std::istream & input, std::ostream & output, int level, int window)
{
  unsigned char in[chunk];
  unsigned char out[chunk];
  z_stream strm;
  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;
  
  if(deflateInit2(&strm, 
                  Z_DEFAULT_COMPRESSION,
                  Z_DEFLATED,
                  window,
                  level,
                  Z_DEFAULT_STRATEGY) != Z_OK)
  {
    throw std::runtime_error("deflateInit");
  }
    
  int flush;

  do
  {
    input.read(reinterpret_cast<char*>(in), chunk);
    strm.avail_in = input.gcount();
    flush = (input.eof() || input.fail()) ? Z_FINISH : Z_NO_FLUSH;
    strm.next_in = in;
    do
    {
      strm.avail_out = chunk;
      strm.next_out = out;
      deflate(&strm, flush);
      output.write(reinterpret_cast<char*>(out), chunk - strm.avail_out);
            
    }
    while (strm.avail_out == 0);
  }
  while(flush != Z_FINISH);

  deflateEnd(&strm);
}

void decompress(std::istream & input, std::ostream & output, int window)
{
  unsigned char in[chunk];
  unsigned char out[chunk];
  z_stream strm;
  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;
  strm.avail_in = 0;
  strm.next_in = Z_NULL;
  if(inflateInit2(&strm,
                  window))
  {
    throw std::runtime_error("inflateInit");
  }
   
  int ret;
  do
  {
    input.read(reinterpret_cast<char*>(in), chunk);
    strm.avail_in = input.gcount();
    if(strm.avail_in == 0)
    {
      inflateEnd(&strm);
      throw std::runtime_error("unexpected end of stream");
    }
    strm.next_in = in;
    do
    {
      strm.avail_out = chunk;
      strm.next_out = out;
      ret = inflate(&strm, Z_NO_FLUSH);
      if(ret == Z_STREAM_ERROR ||
         ret == Z_NEED_DICT ||
         ret == Z_DATA_ERROR ||
         ret == Z_MEM_ERROR)
      {
        inflateEnd(&strm);
        throw std::runtime_error("inflate");
      }
      output.write(reinterpret_cast<char*>(out), chunk - strm.avail_out);
            
    }
    while(strm.avail_out == 0);
  }
  while(ret != Z_STREAM_END);
  inflateEnd(&strm);
}
