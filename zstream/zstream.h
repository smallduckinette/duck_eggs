/* This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the WTFPLv2. See the LICENSE
 * file for more details. */

#ifndef __ZSTREAM_ZTREAM_H__
#define __ZSTREAM_ZTREAM_H__

#include <iostream>

void compress(std::istream & input, 
              std::ostream & output, 
              int level = 8, 
              int window = 15);

void decompress(std::istream & input, std::ostream & output, int window = 15);

#endif
