
#include <cpp11.hpp>
using namespace cpp11;
namespace writable = cpp11::writable;

#include <vector>

#include "GeomagnetismHeader.h"

class WMMMagneticModel {
public:
    WMMMagneticModel(MAGtype_MagneticModel* model): model_(model) {}
    WMMMagneticModel(int num_terms): model_(nullptr) {
        model_ = MAG_AllocateModelMemory(num_terms);
        if (model_ == nullptr) {
            stop("Failed to MAGtype_MagneticModel with %d terms", num_terms);
        }
    }

    MAGtype_MagneticModel* model() {
        return model_;
    }

    ~WMMMagneticModel() {
        if (model_ != nullptr) {
            MAG_FreeMagneticModelMemory(model_);
        }
    }

private:
    MAGtype_MagneticModel* model_;
};



[[cpp11::register]]
std::string cpp_wmm_version() {
    return VERSIONDATE_LARGE;
}

[[cpp11::register]]
SEXP cpp_wmm_read_coef(std::string filename_utf8) {
    // some peculiarities with the input...filename isn't modified and
    // the models array has a hard-to-replicate type
    MAGtype_MagneticModel *models_sane[1];
    models_sane[0] = nullptr;

    char* filename_buf = (char*) filename_utf8.c_str();

    int result = MAG_robustReadMagModels(filename_buf, (MAGtype_MagneticModel *(*)[]) &models_sane, 1);
    if (result == 0) {
        if (models_sane[0] != nullptr) {
            MAG_FreeMagneticModelMemory(models_sane[0]);
        }
        stop("Failed to open model coefficients '%s'", filename_utf8.c_str());
    }

    return external_pointer<WMMMagneticModel>(new WMMMagneticModel(models_sane[0]));
}

[[cpp11::register]]
doubles cpp_wmm_ellipsoidal_height(list coords, integers geoid_ints) {
    doubles lambda = coords["lambda"];
    doubles phi = coords["phi"];
    doubles height = coords["height"];

    if (geoid_ints.size() != 1038961) {
        stop("Expected `geoid` as an integer vector with length 1038961");
    }

    // geoid_ints are the float values scaled by 1000, but we need a float
    // buffer
    std::vector<float> geoid_floats(geoid_ints.size());
    for (R_xlen_t i = 0; i < geoid_ints.size(); i++) {
        geoid_floats[i] = geoid_ints[i] / 1000.0;
    }

    MAGtype_Ellipsoid ellipsoid;
    MAGtype_Geoid geoid;
    MAG_SetDefaults(&ellipsoid, &geoid);
    // double make sure the geoid *is* getting used here
    geoid.UseGeoid = 1;

    geoid.GeoidHeightBuffer = geoid_floats.data();
    geoid.Geoid_Initialized = 1;

    R_xlen_t size = lambda.size();
    writable::doubles height_ellipsoid(size);
    MAGtype_CoordGeodetic coord_geod;
    coord_geod.UseGeoid = 1;

    for (R_xlen_t i = 0; i < size; i++) {
        coord_geod.lambda = lambda[i];
        coord_geod.phi = phi[i];
        coord_geod.HeightAboveGeoid = height[i];

        // MAG_ConvertGeoidToEllipsoidHeight() crashes with NAs
        if (is_na(coord_geod.lambda) || 
              is_na(coord_geod.phi) || 
              is_na(coord_geod.HeightAboveGeoid)) {
            height_ellipsoid[i] = NA_REAL;
            continue;
        }

        MAG_ConvertGeoidToEllipsoidHeight(&coord_geod, &geoid);
        height_ellipsoid[i] = coord_geod.HeightAboveEllipsoid;
    }

    return height_ellipsoid;
}

[[cpp11::register]]
list cpp_wmm_extract(SEXP model_sexp, list coords) {
    external_pointer<WMMMagneticModel> model = model_sexp;

    doubles lambda = coords["lambda"];
    doubles phi = coords["phi"];
    doubles height = coords["height"];
    doubles year = coords["year"];

    if (model->model() == nullptr) {
        stop("`model` is nullptr");
    }

    int nMax = model->model()->nMax;
    WMMMagneticModel timed_model((nMax + 1) * (nMax + 2) / 2);

    R_xlen_t size = lambda.size();
    writable::doubles decl(size);
    writable::doubles incl(size);
    writable::doubles decl_err(size);
    writable::doubles incl_err(size);
    
    MAGtype_Ellipsoid ellipsoid;
    MAGtype_CoordSpherical coord_spherical;
    MAGtype_CoordGeodetic coord_geod;
    coord_geod.HeightAboveGeoid = NA_REAL;
    coord_geod.UseGeoid = 0;
    MAGtype_Date user_date;
    MAGtype_GeoMagneticElements elements, element_errors;
    MAGtype_Geoid geoid;

    MAG_SetDefaults(&ellipsoid, &geoid);
    // double make sure the geoid isn't getting used here
    geoid.UseGeoid = 0;

    for (R_xlen_t i = 0 ; i < size; i++) {
        coord_geod.lambda = lambda[i];
        coord_geod.phi = phi[i];
        coord_geod.HeightAboveEllipsoid = height[i];
        user_date.DecimalYear = year[i];

        MAG_GeodeticToSpherical(ellipsoid, coord_geod, &coord_spherical);
        MAG_TimelyModifyMagneticModel(user_date, model->model(), timed_model.model());
        MAG_Geomag(ellipsoid, coord_spherical, coord_geod, timed_model.model(), &elements);
        MAG_CalculateGridVariation(coord_geod, &elements);
        MAG_WMMErrorCalc(elements.H, &element_errors);

        decl[i] = elements.Decl;
        incl[i] = elements.Incl;
        decl_err[i] = element_errors.Decl;
        decl_err[i] = element_errors.Incl;
    }

    writable::list out = {decl, incl, decl_err, incl_err};
    out.names() = {"decl", "incl", "decl_err", "incl_err"};
    return out;
}
