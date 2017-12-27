#include "erl_nif.h"
#include "murmur3.h"
#include <string.h>

extern ERL_NIF_TERM
murmur3_x86_32(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary in;
  ERL_NIF_TERM ret;
  int seed;
  unsigned char *hash;

  if (!enif_inspect_iolist_as_binary(env, argv[0], &in)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &seed)) {
    return enif_make_badarg(env);
  }

  hash = enif_make_new_binary(env, 4, &ret);
  MurmurHash3_x86_32(in.data, in.size, seed, hash);

  return ret;
}

extern ERL_NIF_TERM
murmur3_x86_128(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary in;
  ERL_NIF_TERM ret;
  int seed;
  unsigned char *hash;

  if (!enif_inspect_iolist_as_binary(env, argv[0], &in)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &seed)) {
    return enif_make_badarg(env);
  }

  hash = enif_make_new_binary(env, 16, &ret);
  MurmurHash3_x64_128(in.data, in.size, seed, hash);

  return ret;
}

extern ERL_NIF_TERM
murmur3_x64_128(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary in;
  ERL_NIF_TERM ret;
  int seed;
  unsigned char *hash;

  if (!enif_inspect_iolist_as_binary(env, argv[0], &in)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &seed)) {
    return enif_make_badarg(env);
  }

  hash = enif_make_new_binary(env, 16, &ret);
  MurmurHash3_x64_128(in.data, in.size, seed, hash);

  return ret;
}

static ErlNifFunc nif_funcs[] = {
  {"murmur3_x86_32", 2, murmur3_x86_32},
  {"murmur3_x86_128", 2, murmur3_x86_128},
  {"murmur3_x64_128", 2, murmur3_x64_128}
};

ERL_NIF_INIT(murmerl, nif_funcs, NULL, NULL, NULL, NULL);
