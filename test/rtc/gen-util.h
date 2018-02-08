CUCL_DEVICE float det_hash_rand(const uint32_t rv ) {
  uint32_t h = rv;
  h ^= h >> 16;
  h *= 0x85ebca6b;
  h ^= h >> 13;
  h *= 0xc2b2ae35;
  h ^= h >> 16;
  return FLOAT_CAST ( h ) * ( 10.0f / FLOAT_CAST ( U32_MAX ) ) - 5.0f;
}
