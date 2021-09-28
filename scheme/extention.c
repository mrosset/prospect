#include <assert.h>
#include <blkmaker.h>
#include <blkmaker_jansson.h>
#include <gcrypt.h>
#include <inttypes.h>
#include <libguile.h>

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

  // The json data file has an old received time, we must set the
  // received time to now.
  tmpl_p->_time_rcvd = time (NULL);

  datasz = blkmk_get_data (tmpl_p, data, sizeof (data), time (NULL), NULL,
                           &dataid);
  assert (datasz > 0);

  return scm_from_size_t (datasz);
}

static blktemplate_t *
template_from_file (const char *path)
{
  blktemplate_t *tmpl;
  json_t *json;
  json_error_t error;
  const char *json_error;
  tmpl = blktmpl_create ();

  assert (tmpl);

  json = json_load_file (path, 0, &error);

  json_error = blktmpl_add_jansson (tmpl, json, time (NULL));
  json_decref (json);

  if (json_error)
    {
      assert (0 && "Error adding block template");
    }

  assert (blkmk_init_generation (tmpl, NULL, 0));
  unsigned char data[80], hash[32];
  size_t datasz;
  unsigned int dataid;
  uint32_t nonce;

  assert (tmpl->cbtxn);
  assert (tmpl->txncount == 105);
  assert (tmpl->cbtxn->datasz == 64);

  datasz
      = blkmk_get_data (tmpl, data, sizeof (data), time (NULL), NULL, &dataid);

  assert (datasz == 76);

  return tmpl;
}

SCM
scm_c_test_mine (SCM json_str)
{
  blktemplate_t *tmpl;
  json_t *json;
  json_error_t error;
  const char *error_str;

  tmpl = blktmpl_create ();
  assert (tmpl);

  json = json_loads (scm_to_locale_string (json_str), 0, &error);
  assert (json);

  json = blktmpl_request_jansson (blktmpl_addcaps (tmpl), NULL);
  assert (json);

  // Change receive time to now
  /* tmpl->_time_rcvd = time (NULL); */

  error_str = blktmpl_add_jansson (tmpl, json, time (NULL));

  if (error_str)
    {
      printf (";; Error: %s\n", error_str);
      assert (0 && "Error adding block template");
    }

  assert (blkmk_init_generation (tmpl, NULL, 0));

  while (blkmk_time_left (tmpl, time (NULL)) && blkmk_work_left (tmpl))
    {
      unsigned char data[80], hash[32];
      size_t datasz;
      unsigned int dataid;
      uint32_t nonce;

      datasz = blkmk_get_data (tmpl, data, sizeof (data), time (NULL), NULL,
                               &dataid);
      assert (datasz >= 76 && datasz <= sizeof (data));

      // mine the right nonce this is iterating in native order, even
      // though SHA256 is big endian, because we don't implement
      // noncerange however, the nonce is always interpreted as big
      // endian, so we need to convert it as if it were big endian
      for (nonce = 0; nonce < 0xffffffff; ++nonce)
        {
          *(uint32_t *)(&data[76]) = nonce;
          assert (my_sha256 (hash, data, 80));
          assert (my_sha256 (hash, hash, 32));
          if (!*(uint32_t *)(&hash[28]))
            break;
          if (!(nonce % 0x1000))
            {
              printf ("0x%8" PRIx32 " hashes done...\r", nonce);
              fflush (stdout);
            }
        }
      printf ("Found nonce: 0x%8" PRIx32 " \n", nonce);
      nonce = ntohl (nonce);

      json = blkmk_submit_jansson (tmpl, data, dataid, nonce);
      assert (json);
    }
}

SCM
scm_c_test_hash (SCM path)
{
  blktemplate_t *tmpl = template_from_file (scm_to_locale_string (path));

  const char *prev_hex = hash2hex (tmpl->prevblk);
  const char *merkle = hash2hex (tmpl->_mrklbranch);

  // clang-format off
  assert (strcmp (prev_hex, "000000000000000000095302283803967a66414cd23b452ebea94e745d3abc8e") == 0);
  assert (strcmp (merkle, "ab52937526190b791f641a6c5c3b0c4ca78cfa35fb31398618787b49fbd2449a") == 0);
  // clang-format on

  return SCM_BOOL_T;
}

void
init_prospect (void)
{
  blkmk_sha256_impl = my_sha256;
  scm_c_define_gsubr ("prospect-extention?", 0, 0, 0, scm_prospect_extention);
  scm_c_define_gsubr ("merkle-root", 1, 0, 0, scm_merkle_root);
  scm_c_define_gsubr ("get-data", 1, 0, 0, scm_get_data);
  scm_c_define_gsubr ("test-c-hashes", 1, 0, 0, scm_c_test_hash);
  scm_c_define_gsubr ("test-c-mine", 1, 0, 0, scm_c_test_mine);
}
