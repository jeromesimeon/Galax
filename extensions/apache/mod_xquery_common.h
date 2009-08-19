#if !(defined(_MOD_XQUERY_COMMON_H))
#define _MOD_XQUERY_COMMON_H

extern compiled_prolog cp_glob;
extern processing_context pc_glob;

#define BUFFER_LEN 1024

#define BEG_ENVELOPE  "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\">\n"
#define END_ENVELOPE  "</soapenv:Envelope>\n"

#define BEG_BODY "<soapenv:Body>\n"
#define END_BODY "</soapenv:Body>\n"

#define BEG_FAULT  "<soapenv:Fault>\n"
#define END_FAULT "</soapenv:Fault>\n"



char * get_user_data (request_rec *r);
char * build_xquery_response (request_rec *r, itemlist items, processing_context pc, prepared_prolog pp);
char * answer_soap_request (request_rec *r, itemlist items); 
char * get_response (request_rec *r, char *req_body, processing_context pc);

#define DEBUG

#endif
