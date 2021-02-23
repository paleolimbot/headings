// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"

// wmm.cpp
std::string cpp_wmm_version();
extern "C" SEXP _headings_cpp_wmm_version() {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_wmm_version());
  END_CPP11
}
// wmm.cpp
SEXP cpp_wmm_read_cof(std::string filename_utf8);
extern "C" SEXP _headings_cpp_wmm_read_cof(SEXP filename_utf8) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_wmm_read_cof(cpp11::as_cpp<cpp11::decay_t<std::string>>(filename_utf8)));
  END_CPP11
}
// wmm.cpp
list cpp_wmm_extract(SEXP model_sexp, data_frame coords);
extern "C" SEXP _headings_cpp_wmm_extract(SEXP model_sexp, SEXP coords) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_wmm_extract(cpp11::as_cpp<cpp11::decay_t<SEXP>>(model_sexp), cpp11::as_cpp<cpp11::decay_t<data_frame>>(coords)));
  END_CPP11
}

extern "C" {
/* .Call calls */
extern SEXP _headings_cpp_wmm_extract(SEXP, SEXP);
extern SEXP _headings_cpp_wmm_read_cof(SEXP);
extern SEXP _headings_cpp_wmm_version();

static const R_CallMethodDef CallEntries[] = {
    {"_headings_cpp_wmm_extract",  (DL_FUNC) &_headings_cpp_wmm_extract,  2},
    {"_headings_cpp_wmm_read_cof", (DL_FUNC) &_headings_cpp_wmm_read_cof, 1},
    {"_headings_cpp_wmm_version",  (DL_FUNC) &_headings_cpp_wmm_version,  0},
    {NULL, NULL, 0}
};
}

extern "C" void R_init_headings(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
