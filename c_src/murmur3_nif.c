#include "erl_nif.h"
#include "murmur3.h"
#include <string.h>

ERL_NIF_TERM
mk_atom(ErlNifEnv *env, const char *atom) {
  ERL_NIF_TERM ret;

  if (!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
    return enif_make_atom(env, atom);
  }

  return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv *env, const char *msg) {
  return enif_make_tuple(env, mk_atom(env, "error"), mk_atom(env, msg));
}

extern ERL_NIF_TERM
murmur3_x86_32(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary ret, in;
  int seed;

  if (!enif_inspect_iolist_as_binary(env, argv[0], &in)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &seed)) {
    return enif_make_badarg(env);
  }

  if (!enif_alloc_binary(4, &ret)) {
    mk_error(env, "failed_to_alloc_bin");
  }
  memset(ret.data, 0, 4);

  MurmurHash3_x86_32(in.data, in.size, seed, ret.data);

  return enif_make_binary(env, &ret);
}

extern ERL_NIF_TERM
murmur3_x86_128(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary ret, in;
  int seed;

  if (!enif_inspect_iolist_as_binary(env, argv[0], &in)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &seed)) {
    return enif_make_badarg(env);
  }

  if (!enif_alloc_binary(16, &ret)) {
    mk_error(env, "failed_to_alloc_bin");
  }
  memset(ret.data, 0, 16);

  MurmurHash3_x64_128(in.data, in.size, seed, ret.data);

  return enif_make_binary(env, &ret);
}

extern ERL_NIF_TERM
murmur3_x64_128(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary ret, in;
  int seed;

  if (!enif_inspect_iolist_as_binary(env, argv[0], &in)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &seed)) {
    return enif_make_badarg(env);
  }

  if (!enif_alloc_binary(16, &ret)) {
    mk_error(env, "failed_to_alloc_bin");
  }
  memset(ret.data, 0, 16);

  MurmurHash3_x86_128(in.data, in.size, seed, ret.data);

  return enif_make_binary(env, &ret);
}
static ErlNifFunc nif_funcs[] = {
  {"murmur3_x86_32", 2, murmur3_x86_32},
  {"murmur3_x86_128", 2, murmur3_x86_128},
  {"murmur3_x64_128", 2, murmur3_x64_128}
};

ERL_NIF_INIT(murmerl, nif_funcs, NULL, NULL, NULL, NULL);
