/* test.c
 * Copyright (C) 2021 Michael Rosset <mike.rosset@gmail.com>
 *
 * This file is part of Prospect
 *
 * Prospect is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Prospect is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <assert.h>
#include <inttypes.h>

#include <gcrypt.h>

#include "blktemplate.h"
#include <blkmaker.h>
#include <blkmaker_jansson.h>

static const time_t simple_time_rcvd = 0x777;

static bool
my_sha256 (void *digest, const void *buffer, size_t length)
{
  gcry_md_hash_buffer (GCRY_MD_SHA256, digest, buffer, length);
  return true;
}

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

int
main ()
{
  blktemplate_t *tmpl;
  json_t *json;
  json_error_t error;
  const char *json_error;

  blkmk_sha256_impl = my_sha256;

  tmpl = blktmpl_create ();

  assert (tmpl);

  json = json_load_file ("/home/mrosset/src/prospect/test-suite/data.json", 0,
                         &error);

  if (!json)
    {
      fprintf (stderr, "ERROR: %s\n", error.text);
      assert (0 && "Error loading test data");
    }

  json_error = blktmpl_add_jansson (tmpl, json, simple_time_rcvd);
  json_decref (json);

  if (json_error)
    {
      fprintf (stderr, "Error adding block template: %s", json_error);
      assert (0 && "Error adding block template");
    }

  assert (blkmk_init_generation (tmpl, NULL, 0));

  assert (tmpl->cbtxn);
  assert (tmpl->txncount == 105);
  assert (tmpl->cbtxn->datasz == 64);

  unsigned char data[80], hash[32];
  size_t datasz;
  unsigned int dataid;
  uint32_t nonce;

  datasz = blkmk_get_data (tmpl, data, sizeof (data), simple_time_rcvd, NULL,
                           &dataid);

  assert (datasz == 76);

  const char *prev_hex = hash2hex (tmpl->prevblk);
  const char *merkle = hash2hex (tmpl->_mrklbranch);
  /* printf ("%s\n", hash2hex (*tmpl->_mrklbranch)); */

  // clang-format off
  assert (strcmp (prev_hex, "000000000000000000095302283803967a66414cd23b452ebea94e745d3abc8e") == 0);
  assert (strcmp (merkle, "ab52937526190b791f641a6c5c3b0c4ca78cfa35fb31398618787b49fbd2449a") == 0);
  // clang-format on
  /* printf ("DATA: %zu\n", datasz); */
  /* blkmk_get_mdata (tmpl, data, sizeof (data), simple_time_rcvd, &i16,
   * &cbtxn, &cbtxnsize, &cbextranonceoffset, &branchcount, &branches, 0,
   *                  true); */

  /* printf ("HASH: %s \n", branches); */
  /* printf ("%08" PRIx8 "\n", branches); */
  /* printf ("HASH: %08" PRIx32 " \n", branches); */

  return 0;
}
