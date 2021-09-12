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

#include <gcrypt.h>

#include "blktemplate.h"
#include <blkmaker.h>
#include <blkmaker_jansson.h>

static bool
my_sha256 (void *digest, const void *buffer, size_t length)
{
  gcry_md_hash_buffer (GCRY_MD_SHA256, digest, buffer, length);
  return true;
}

int
main_old ()
{
  blktemplate_t *tmpl;
  json_t *json;
  json_error_t error;
  const time_t simple_time_rcvd = 0x777;
  const char *err;

  tmpl = blktmpl_create ();

  assert (tmpl);
  /* json = blktmpl_request_jansson(blktmpl_addcaps(tmpl), NULL); */
  /* assert(json); */

  json = json_load_file ("../data.json", 0, &error);
  if (!json)
    {
      fprintf (stderr, "ERROR: %s\n", error.text);
      assert (0 && "Error loading test data");
    }

  err = blktmpl_add_jansson (tmpl, json, simple_time_rcvd);
  json_decref (json);

  if (err)
    {
      fprintf (stderr, "Error adding block template: %s", err);
      assert (0 && "Error adding block template");
    }

  uint8_t data[76], *cbtxn, *branches, extranonce[10];
  size_t cbextranonceoffset, cbtxnsize;
  size_t datasz;
  unsigned int dataid;
  int branchcount;
  int16_t i16;

  /* uint32_t nonce; */
  bool newcb = true;

  assert (blkmk_init_generation (tmpl, NULL, 0));
  /* assert (blkmk_init_generation3 (tmpl,
   *                                 "\x04"
   *                                 "test",
   *                                 5, &newcb)); */

  assert (tmpl->cbtxn);
  printf ("DATAZ %zu\n", tmpl->cbtxn->datasz);
  /* assert (tmpl->cbtxn->datasz == 64); */

  blkmk_get_mdata (tmpl, data, sizeof (data), simple_time_rcvd, &i16, &cbtxn,
                   &cbtxnsize, &cbextranonceoffset, &branchcount, &branches, 5,
                   false);
  /* printf ("branch count %d\n", branchcount); */
  /* datasz
   *   = blkmk_get_data (tmpl, data, sizeof (data), simple_time_rcvd, &i16,
   * &dataid); printf("%zu\n", datasz); */
  /* assert (datasz >= 76); */
  /* printf ("SIZE: %zu\n", datasz); */

  return 0;
}

static const char *
blktmpl_add_jansson_str (blktemplate_t *const tmpl, const char *const s,
                         const time_t time_rcvd)
{
  json_t *const j = json_loads (s, 0, NULL);
  assert (j);
  const char *const rv = blktmpl_add_jansson (tmpl, j, time_rcvd);
  json_decref (j);
  return rv;
}

static const time_t simple_time_rcvd = 0x777;

int
main ()
{
  blktemplate_t *tmpl = blktmpl_create ();
  uint8_t data[76], *cbtxn, *branches;
  size_t cbextranonceoffset, cbtxnsize;
  int branchcount;
  int16_t i16;

  // clang-format off
  assert(!blktmpl_add_jansson_str(tmpl, "{\"version\":3,\"height\":4,\"bits\":\"1d007fff\",\"curtime\":877,\"previousblockhash\":\"00000000a7777777a7777777a7777777a7777777a7777777a7777777a7777777\",\"coinbasevalue\":640,\"sigoplimit\":100,\"sizelimit\":1000,\"transactions\":[{\"data\":\"01000000019999999999999999999999999999999999999999999999999999999999999999aaaaaaaa00222222220100100000015100000000\",\"required\":true},{\"hash\":\"8eda1a8b67996401a89af8de4edd6715c23a7fb213f9866e18ab9d4367017e8d\",\"data\":\"01000000011c69f212e62f2cdd80937c9c0857cedec005b11d3b902d21007c932c1c7cd20f0000000000444444440100100000015100000000\",\"depends\":[1],\"fee\":12,\"required\":false,\"sigops\":4},{\"data\":\"01000000010099999999999999999999999999999999999999999999999999999999999999aaaaaaaa00555555550100100000015100000000\"}],\"coinbasetxn\":{\"data\":\"01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff07010404deadbeef333333330100100000015100000000\"},\"workid\":\"mywork\",\"mutable\":[\"submit/coinbase\",\"submit/truncate\",\"coinbase/append\"],\"expires\":99}", simple_time_rcvd));

  // clang-format on
  /* assert (blkmk_get_mdata (tmpl, data, sizeof (data), simple_time_rcvd,
   * &i16, &cbtxn, &cbtxnsize, &cbextranonceoffset,
   *                          &branchcount, &branches, 1, false)); */
  return 0;
}
