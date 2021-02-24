// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"

// wmm-emm.cpp
std::string cpp_mm_version();
extern "C" SEXP _headings_cpp_mm_version() {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_mm_version());
  END_CPP11
}
// wmm-emm.cpp
SEXP cpp_mm_read_coef(std::string filename_utf8);
extern "C" SEXP _headings_cpp_mm_read_coef(SEXP filename_utf8) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_mm_read_coef(cpp11::as_cpp<cpp11::decay_t<std::string>>(filename_utf8)));
  END_CPP11
}
// wmm-emm.cpp
SEXP cpp_mm_read_coef_sv(std::string filename_utf8, std::string filename_sv_utf8);
extern "C" SEXP _headings_cpp_mm_read_coef_sv(SEXP filename_utf8, SEXP filename_sv_utf8) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_mm_read_coef_sv(cpp11::as_cpp<cpp11::decay_t<std::string>>(filename_utf8), cpp11::as_cpp<cpp11::decay_t<std::string>>(filename_sv_utf8)));
  END_CPP11
}
// wmm-emm.cpp
void cpp_mm_coalesce_for_emm2017(SEXP mutable_model_sexp, SEXP model_sexp, bool zero_sv);
extern "C" SEXP _headings_cpp_mm_coalesce_for_emm2017(SEXP mutable_model_sexp, SEXP model_sexp, SEXP zero_sv) {
  BEGIN_CPP11
    cpp_mm_coalesce_for_emm2017(cpp11::as_cpp<cpp11::decay_t<SEXP>>(mutable_model_sexp), cpp11::as_cpp<cpp11::decay_t<SEXP>>(model_sexp), cpp11::as_cpp<cpp11::decay_t<bool>>(zero_sv));
    return R_NilValue;
  END_CPP11
}
// wmm-emm.cpp
doubles cpp_mm_ellipsoidal_height(list coords, integers geoid_ints);
extern "C" SEXP _headings_cpp_mm_ellipsoidal_height(SEXP coords, SEXP geoid_ints) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_mm_ellipsoidal_height(cpp11::as_cpp<cpp11::decay_t<list>>(coords), cpp11::as_cpp<cpp11::decay_t<integers>>(geoid_ints)));
  END_CPP11
}
// wmm-emm.cpp
list cpp_mm_extract(SEXP model_sexp, list coords);
extern "C" SEXP _headings_cpp_mm_extract(SEXP model_sexp, SEXP coords) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_mm_extract(cpp11::as_cpp<cpp11::decay_t<SEXP>>(model_sexp), cpp11::as_cpp<cpp11::decay_t<list>>(coords)));
  END_CPP11
}

extern "C" {
/* .Call calls */
extern SEXP _headings_cpp_mm_coalesce_for_emm2017(SEXP, SEXP, SEXP);
extern SEXP _headings_cpp_mm_ellipsoidal_height(SEXP, SEXP);
extern SEXP _headings_cpp_mm_extract(SEXP, SEXP);
extern SEXP _headings_cpp_mm_read_coef(SEXP);
extern SEXP _headings_cpp_mm_read_coef_sv(SEXP, SEXP);
extern SEXP _headings_cpp_mm_version();

static const R_CallMethodDef CallEntries[] = {
    {"_headings_cpp_mm_coalesce_for_emm2017", (DL_FUNC) &_headings_cpp_mm_coalesce_for_emm2017, 3},
    {"_headings_cpp_mm_ellipsoidal_height",   (DL_FUNC) &_headings_cpp_mm_ellipsoidal_height,   2},
    {"_headings_cpp_mm_extract",              (DL_FUNC) &_headings_cpp_mm_extract,              2},
    {"_headings_cpp_mm_read_coef",            (DL_FUNC) &_headings_cpp_mm_read_coef,            1},
    {"_headings_cpp_mm_read_coef_sv",         (DL_FUNC) &_headings_cpp_mm_read_coef_sv,         2},
    {"_headings_cpp_mm_version",              (DL_FUNC) &_headings_cpp_mm_version,              0},
    {NULL, NULL, 0}
};
}

extern "C" void R_init_headings(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
