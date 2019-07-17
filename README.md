# EDCAT
Computerized adaptive test for eating disorders.

The survey is specified by the files in the data directory, which were extracted from MultiCATinput_EPSI_09-21-18.RData in the demo Shiney app.

## PiLR Calculated Content API
PiLR EMA surveys can delegate their content to a service implement the API defined in
[KU Cat Design - Remote Calculation Card](https://docs.google.com/document/d/1fC8kag54Ttm9Yy0vm3oayHKyk5jLnvHw9e5MOqrkZJo).
The function pilrContentApi() implments the API when invoked via openCPU.

## Install on openCPU server
Install on openCPU.pilrhealth.com:

    TOKEN="<your github personal access token>"
    install() {
      sudo R_LIB='/usr/lib/R/site-library:/usr/lib/opencpu/library' Rscript --slave --no-save --no-restore-history -e "library(devtools) ; install_github(repo='$1', auth_token='$TOKEN')"
    }
    install philchalmers/mirt
    install philchalmers/mirtcat
    install MeiResearchLtd/EDCAT

## Testing with Curl

There are some sample JSON requests in the tests/testthat/ directory that were captured from the EMA
console. They can be submitted with the following command.

    curl -H"Content-Type":"application/json" -d @request.json \
          https://ocpu.pilrhealth.com/ocpu/library/EDCAT/R/pilrContentApi/json

## JSON to R Translation

openCPU's translation from JSON to R data structures is unpredictable and changes depending on the version of
openCPU. I have been unable to find any documentation.  It appears to use the jsonlite package, but not with
unknown settings.

So to create data for unit tests, use the curl command in the previous section, but replace the function
'pilrContentAPI' with 'dumper'.  This will return a string containain an R expression that constructs a list
of the input parameters as decoded by openCPU.
