#include <assert.h>
#include <blkmaker.h>
#include <blkmaker_jansson.h>
#include <gcrypt.h>
#include <inttypes.h>
#include <libguile.h>

static const time_t simple_time_rcvd = 0x777;

static const char *
hash2hex (libblkmaker_hash_t hash)
{
  char *hex = (char *)malloc (64 + 1);

  hex[0] = 0;

  for (int i = 7; i >= 0; i--)
    {
      char add[9];
      sprintf (add, "%08" PRIx32, hash[i]);
      strcat (hex, add);
    }

  return hex;
}

static bool
my_sha256 (void *digest, const void *buffer, size_t length)
{
  gcry_md_hash_buffer (GCRY_MD_SHA256, digest, buffer, length);
  return true;
}

SCM
scm_merkle_root (SCM tmpl_scm)
{
  blktemplate_t *tmpl = scm_to_pointer (tmpl_scm);
  const char *h = hash2hex (tmpl->_mrklbranch);
  return scm_from_locale_string (h);
}

SCM
scm_prospect_extention ()
{
  return SCM_BOOL_T;
}

SCM
scm_get_data (SCM tmpl)
{
  blktemplate_t *tmpl_p = scm_to_pointer (tmpl);

  unsigned char data[80], hash[32];
  size_t datasz;
  unsigned int dataid;
  uint32_t nonce;

  datasz = blkmk_get_data (tmpl_p, data, sizeof (data), simple_time_rcvd, NULL,
                           &dataid);

  return scm_from_size_t (datasz);
}

SCM
scm_test_c ()
{
  blktemplate_t *tmpl;
  json_t *json;
  json_error_t error;
  const char *json_error;

  unsigned char data[80], hash[32];
  size_t datasz;
  unsigned int dataid;
  uint32_t nonce;

  blkmk_sha256_impl = my_sha256;

  tmpl = blktmpl_create ();

  assert (tmpl);

  json = json_load_file ("/home/mrosset/src/prospect/test-suite/data.json", 0,
                         &error);

  json_error = blktmpl_add_jansson (tmpl, json, simple_time_rcvd);
  json_decref (json);

  if (json_error)
    {
      assert (0 && "Error adding block template");
    }

  assert (blkmk_init_generation (tmpl, NULL, 0));

  assert (tmpl->cbtxn);
  assert (tmpl->txncount == 105);
  assert (tmpl->cbtxn->datasz == 64);

  datasz = blkmk_get_data (tmpl, data, sizeof (data), simple_time_rcvd, NULL,
                           &dataid);
  assert (datasz == 76);

  const char *prev_hex = hash2hex (tmpl->prevblk);
  const char *merkle = hash2hex (tmpl->_mrklbranch);

  // clang-format off
  assert (strcmp (prev_hex, "000000000000000000095302283803967a66414cd23b452ebea94e745d3abc8e") == 0);
  assert (strcmp (merkle, "ab52937526190b791f641a6c5c3b0c4ca78cfa35fb31398618787b49fbd2449a") == 0);
  // clang-format on

  return SCM_BOOL_T;
}

SCM
scm_mine (SCM tmpl)
{
  blktemplate_t *tmpl_p = scm_to_pointer (tmpl);
  return SCM_BOOL_T;
}

void
init_prospect (void)
{
  blkmk_sha256_impl = my_sha256;
  scm_c_define_gsubr ("prospect-extention?", 0, 0, 0, scm_prospect_extention);
  scm_c_define_gsubr ("merkle-root", 1, 0, 0, scm_merkle_root);
  scm_c_define_gsubr ("get-data", 1, 0, 0, scm_get_data);
  scm_c_define_gsubr ("test-c", 0, 0, 0, scm_test_c);
  scm_c_define_gsubr ("mine", 1, 0, 0, scm_mine);
}
