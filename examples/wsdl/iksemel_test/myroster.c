#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <iksemel.h>


#define STACK_SIZE 8192 

iksparser *p;
iksfilter *filter;
iks *presence_tag = NULL;
int default_timeout = 1;
int had_bytes_in = 0;
iks *roster_node = NULL;

char s_err [1024];
int verbose = 0; /* if =1 then print XML stream to stderr */
int is_auth = 1; /* reset, if authorization fails */

void my_error (char *faultstring, char *detail) {
  s_err[0] = (char)0;
  strcpy (s_err, "<soapenv:Fault xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"><faultcode>Server</faultcode><faultstring>");
  strcat (s_err, faultstring);
  strcat (s_err, "</faultstring><faultactor>Jabber wrapper</faultactor><detail>");
  strcat (s_err, detail);
  strcat (s_err, "</detail></soapenv:Fault>");
}


iks* my_get_roster (iksparser *parser, int timeout) {
  int ret; 
  iks *x;

  x = iks_make_iq (IKS_TYPE_GET, IKS_NS_ROSTER);
  iks_insert_attrib (x, "id", "roster_0");
  iks_send (parser, x);
    
  while (timeout > 0) {
    ret = iks_recv (parser, 5);
    if (IKS_HOOK == ret) break;
    if (IKS_OK != ret) {
      my_error ("io error", "");
      return NULL;
    }
    timeout--;
    if (timeout < 0) {
      my_error ("network timeout", "");
      return NULL;
    }
  }

  return roster_node;
}


iks* my_get_presence (iksparser *parser, int timeout) {
  int ret; 
  iks *x = iks_new ("available");
  
  if (NULL == x) {
    my_error ("Not enough memory", "");
    return NULL;
  }
  else presence_tag = x;

  while (1) {
    had_bytes_in = 0;
    ret = iks_recv (parser, timeout);
    if (IKS_HOOK == ret) break;
    if (IKS_OK != ret) {
      my_error ("io error", "");
      return NULL;
    }
    if (!had_bytes_in)
      break; /* no input */
  }  
  presence_tag = NULL;
  /*
    avail = iks_string (iks_stack(x), x);
    iks_delete (x);
  */
  return x;
}


int stream_hook (void *user_data, int type, iks *node) {
  ikspak *pak;

  switch (type) {
  case IKS_NODE_NORMAL:
    pak = iks_packet (node);
    iks_filter_packet (filter, pak);
    
    /* if (presence_tag != NULL && !strcmp(iks_name(node), "presence")) 
       iks_insert_node (presence_tag, node); */
    break;
  }
  return IKS_OK;
}

void log_hook (void *user_data, const char* data, size_t size, int is_incoming) {
  if (is_incoming)
    had_bytes_in = 1;
  
  if (verbose) {
    if (is_incoming) 
      fprintf (stderr, "IN:");  
    else
      fprintf (stderr, "OUT:");
    fprintf (stderr, "[%s]\n", data);
  }
}

void usage() {
  puts ("Usage: myroster JID password");
}

int presence_recv (void *user_data, ikspak *pak) { 
  if (presence_tag != NULL)
    iks_insert_node (presence_tag, pak->x);
  return IKS_FILTER_EAT;
}

int auth_failed (void *user_data, ikspak *pak) {
  is_auth = 0;
  return IKS_FILTER_EAT;
}

int roster_recv (void *user_data, ikspak *pak) {
  roster_node = iks_find(pak->x, "query");
  return IKS_FILTER_EAT;
}


/* sample call: ./myroster onosen@localhost/galax onose */

char*
presence_information
(char *s_jid, char *s_passwd, int timeout, int debug) {

  iks *auth_block;
  ikstack *st;
  iksid* my_jid;
  char *msg = NULL;
  iks *msg_roster = NULL;
  iks *msg_presence = NULL;
  iks *response;

  verbose = debug;

  /* initialize variables */
  st = iks_stack_new(STACK_SIZE);
  my_jid = iks_id_new(st, s_jid);
	
  /* initialize stream */
  p = iks_stream_new (IKS_NS_CLIENT, NULL, stream_hook);
  iks_set_log_hook(p, log_hook);

  /* initialize filters */

  filter = iks_filter_new ();
  iks_filter_add_rule (filter, auth_failed, NULL,
		       IKS_RULE_TYPE, IKS_PAK_IQ,
		       IKS_RULE_SUBTYPE, IKS_TYPE_ERROR,
		       IKS_RULE_ID, "pthsock_client_auth_ID",
		       IKS_RULE_DONE);
  iks_filter_add_rule (filter, auth_failed, NULL,
		       IKS_RULE_TYPE, IKS_PAK_IQ,
		       IKS_RULE_SUBTYPE, IKS_TYPE_ERROR,
		       IKS_RULE_ID, "auth",
		       IKS_RULE_DONE);
  iks_filter_add_rule (filter, presence_recv, NULL,
		       IKS_RULE_TYPE, IKS_PAK_PRESENCE,
		       IKS_RULE_DONE);
  iks_filter_add_rule (filter, roster_recv, NULL,
		       IKS_RULE_TYPE, IKS_PAK_IQ,
		       IKS_RULE_SUBTYPE, IKS_TYPE_RESULT,
		       IKS_RULE_NS, "jabber:iq:roster",
		       IKS_RULE_DONE);

  /* Connect to server */
  if (iks_connect_tcp(p, my_jid->server, IKS_JABBER_PORT)) {
    my_error ("Connexion error", "");
    return s_err;
  }
  iks_recv(p, 1);
  
  /* Authentification */

  auth_block = iks_make_auth(my_jid, s_passwd, NULL);

  iks_send(p, auth_block);
  iks_recv(p, 1);  

  if (!is_auth) {
    my_error ("Authentification failure", "");
    return s_err;
  }

  /* Note: roster query must be sent before publishing presence */
  msg_roster = my_get_roster (p, 1);   

  if (NULL == msg_roster) {
    return s_err;
  }
  

  /* My presence */

  iks_send(p, iks_make_pres(IKS_SHOW_AVAILABLE,"Online")); 
  
    
  msg_presence = my_get_presence (p, timeout);
  if (msg_presence == NULL) {
    return s_err;
  }    

  response = iks_new ("response");
  iks_insert_node (response, msg_roster);
  iks_insert_node (response, msg_presence);
  /*  msg = strcat (msg, msg_presence); */
  msg = iks_string (iks_stack(response), response);

  iks_disconnect(p);	 
  iks_delete (response);
  iks_parser_delete (p);


  return msg;
}

/* test program */
int main (int argc, char *argv[])
{
  if (argc < 3) {
    usage();
    exit(1);
  }
  else
    puts(presence_information (argv[1], argv[2], 1, 0));
  return 0;
}
