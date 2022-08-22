#pragma once

#include "types.hpp"

i8* copy_str_to_buffer(const i8* buff, u64* len = 0);
i8 *read_from_file(const i8* file);
i8* copy_str(const i8* str);
u32 round_up(u32 num, u32 factor);
u32 round_down(u32 num, u32 factor);
