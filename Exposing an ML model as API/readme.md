More often that not we build models offline and send reports to stakeholders suggesting actions to understake to tackle the same.
However, in ideal scenario we would like to expose the model as an API for consumption so that it can be invoked from anywhere/anytime.
In this project, I have put an example of GET and POST REST APIs using jupyter kernel gateway. You have to install jupyter kernel gateway
and expose the python notebook as an API. Link to the details can be found in: https://github.com/jupyter/kernel_gateway 
Command to expose any python notebook as an API (run this from command prompt): 
jupyter kernelgateway --KernelGatewayApp.api='kernel_gateway.notebook_http' --KernelGatewayApp.seed_uri='testnotebook.ipynb'

Please note that we can use other python frameworks like FLask and Django also to create REST end points, however, I have used jupyter kernel_gateway. Also security is a big aspect when we have public APIs, so we should always have some form of authentication parameters passed in the JSON header, but this is not handled by my code. 

1. Simple_GET_API.ipynb -- GET API it takes in the angle in degrees as a parameter and returns the corresponding radian value.
It can be invoked using the command: http://127.0.0.1:8889/convert?angle=60 from the browser or POSTMAN. 
2. POST_API_Predictions.ipynb - POST API to get the real time predictions when a JSON request with the feature set is provided. The code parses the JSON requests to create a dataframe, loads the model (saved as a pickle file in file server), runs the model through the datafranme and returns predictions.
This can be invoked from POSTMAN or through any code by passing the JSON request to the URL.
POST: http://127.0.0.1:8889/predictions
JSON request:
{"_data": {
                    "HR_DATE_PRO_ORCL_ES_ENG": null,
                    "HIGHEST_EDUC_LVL_ORCL_ES_ENG": "A",
                    "ORCL_ES_LINK": "TQ898",
                    "BIRTHDATE_ORCL_ES_ENG": "1945-04-17",
                    "EMPLID_ORCL_ES_ENG": "TQ898",
                    "FULL_PART_TIME_ORCL_ES_ENG": "F",
                    "ORCL_ES_LANGUAGE_CD_ENG": "ENG",
                    "PER_ORG_ORCL_ES_ENG": "EMP",
                    "CHANGE_PCT_ORCL_ES_ENG": 0,
                    "PORTAL_NAME": "PTSF_NONE",
                    "JOBCODE_ORCL_ES_ENG": "TQ006",
                    "PTSF_NAV_MODE": "1",
                    "SEX_ORCL_ES_ENG": "F",
                    "COMPRATE_ORCL_ES_ENG": 5000,
                    "MAR_STATUS_ORCL_ES_ENG": "S",
                    "HR_STATUS_ORCL_ES_ENG": "A",
                    "ORCL_ES_LAST_MODIFIED_DATE": "2004-05-05T07:00:00.000Z",
                    "LASTUPDDTTM_ORCL_ES_ENG": "2004-05-05T07:00:00.000Z",
                    "PTSF_SBO_NAME": "QE_LOCAL HR_EMPL_ATTR_DATA",
                    "ORCL_ES_TITLE_ENG": "TQ898",
                    "HR_DATE_CURRROLE_ORCL_ES_ENG": "2003-10-04",
                    "ORCL_ES_DESCRIPTION_ENG": "TQ898|A",
                    "EP_RATING_ORCL_ES_ENG": 0,
                    "HR_DATE_MGR_ORCL_ES_ENG": "1990-01-01"
  }}
