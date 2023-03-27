# Corrections to SAPFLUXNET v.0.1.5
SAPFLUXNET version described in the SAPFLUXNET data paper and available in 
Zenodo.

## Structure 

* sfn_0_1_5_corrections.R:incorporates corrections on SFN data and metadata and
writes a new version of the database (0.1.6, RData files) in an output folder.
* sfn_0_1_5_csvs.R: writes RData files to csv folder and fixes issues with 
timezones in csv files
* sfn_0_1_6_checks.R: miscellaneous data checks of the corrected version.

## Implementation

This implementation assumes that the current project folder is at the same level
of the SFN version that is updating. In my filesystem:

```
SAPFLUXNET
└───DataVersions
│   └──0.1.5
│       │ csv
│       │ RData
│       └─sfn_0_1_5_corrections
│          │
│          └──output
│             │
│             └─0.1.6
│                 │
│                 └─ csv
|                 └─ RData
```

## Output

The code produces a new version stored in the folder 
output/0.1.6. It has the same
structure as the database in Zenodo.

Once updated version is finalised and checked, it can be copied to 
the DataVersions directory as 0.1.6 and can be used in 
further analyses.

## Changelog

Changes to version 0.1.5 will be documented here soon. For now, have a look at 
the comments in the sfn_0_1_5_corrections.R file.

* 27032022 - Added ESP_RIN stand basal area correction, not present in earlier 0.1.6 versions.
* 27032022 - Recalculated VPD in ESP_LAS based on ta and rh, as VPD supplied had a clear shift in the
time series, making it reach unreasonably high values.