#include <blkmaker.h>
#include <gcrypt.h>
#include <libguile.h>
#include <stdbool.h>

static bool
my_sha256 (void *digest, const void *buffer, size_t length)
{
  gcry_md_hash_buffer (GCRY_MD_SHA256, digest, buffer, length);
  return true;
}

SCM
test ()
{
  return SCM_BOOL_T;
}

void
init_prospect (void)
{
  blkmk_sha256_impl = my_sha256;
  scm_c_define_gsubr ("test-prospect", 0, 0, 0, test);
}
