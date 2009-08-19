#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<db.h>
#include<caml/callback.h>
#include<caml/memory.h>
#include<caml/alloc.h>
#include<assert.h>
//#define DEBUG
#include"smalldb.h"

/* Lower level interface so we can prevent double copying */
/* UNUSED */
value copy_outgoing_hack(struct outgoing_array * _c1)
{ CAMLparam0 ();
  CAMLlocal1(_v2);
  mlsize_t _c3;


  if ((*_c1).length == 0) {
    _v2 = Val_int(0); // the empty array
  } else {
    //   assert((*_c1).length > 0);
    if ((*_c1).length < Max_young_wosize) {
      _v2 = alloc_small((*_c1).length,0); 
      for (_c3 = 0; _c3 < (*_c1).length; _c3++) {
	Field(_v2,_c3) = Val_int((unsigned char)((*_c1).contents[_c3]));
      }
    } else {
      _v2 = alloc_shr((*_c1).length,0); 
      for (_c3 = 0; _c3 < (*_c1).length; _c3++) {
	initialize(&Field(_v2,_c3), 
		   Val_int((unsigned char)((*_c1).contents[_c3])));
      }
    } 
  }
  CAMLreturn(_v2);
}


entry * sec_index_func_list = NULL;

void dump_bytes(char* p, int len) {
  int i;
  for(i = 0; i < len; i++) {
    printf("%2X", p[i]);
  }
  printf("\n");
}

/* unsigned int xor_flags(int num, int opt[]) {
	unsigned int flg = 0;
	int i = 0;

	for(i; i<num; i++) {
		flg |= opt[i];
	}
	return flg;
	} */


int GalaxDB_Env_open(DB_ENV ** dbenvp, char * db_home, 
		     unsigned int flg, int mode) {
	int retval;
	//	unsigned int flg = 0;

	retval = db_env_create(dbenvp, 0);
	if(retval != 0) {
		#ifdef DEBUG
			printf("C: Error in creating Environment: %s\n", db_strerror(retval));
		#endif
		return retval;
	}

	// flg = xor_flags(num, opt);

	retval = (*dbenvp)->open((*dbenvp), db_home, flg, mode); 
	if(retval != 0) {
		#ifdef DEBUG
		printf("C: Error in opening Environment: %s\n", db_strerror(retval));
		#endif
		return retval;
	}
	return retval;
}

int GalaxDB_Env_close(DB_ENV * dbenvp) {
	int retval;

	retval = dbenvp->close(dbenvp, 0); 
	if(retval != 0) {
		#ifdef DEBUG
		printf("C: Error in closing Environment: %s\n", db_strerror(retval));
		#endif
	}
	return retval;
}

char * GalaxDBerror(int error) {
	char * retval;

	retval = db_strerror(error);
	#ifdef DEBUG
	printf("Error string obtained = %s\n",  retval);
	#endif
	fflush(stdout);
	return retval;
}



int GalaxDBopen_internal(DB **dbp, char * dbname, DBTYPE type, 
			 unsigned int open_flags, unsigned int set_flags, 
			 int buffsize, int bfixed, int fixed_length) {
	
	int retval, i;
	char * argument[2];

	#ifdef DEBUG
	printf("C: creating database : %s and %d size | open flags %d\n", dbname, buffsize,open_flags);
	fflush(stdout);
	#endif
	retval = db_create(dbp, NULL, open_flags);	
	if(retval != 0) {
		#ifdef DEBUG
		printf("C: Error in creating Database: %s\n", db_strerror(retval));
		fflush(stdout);
		#endif
		return retval;
	}

	#ifdef DEBUG
	printf("C: database created [name: %s|dbp: %X],  now setting flags = %u\n", dbname, *dbp, set_flags);
	fflush(stdout);
	#endif

	retval = (*dbp)->set_flags((*dbp), set_flags);
	if(retval != 0) {
		#ifdef DEBUG
		printf("C: Error in setting flags : %s\n", db_strerror(retval));
		fflush(stdout);
		#endif
		(*dbp)->err((*dbp), retval, "%s", "GalaxDB", dbname);	
		return retval;
	}

	#ifdef DEBUG
	printf("C: flag set now opening database\n");
	printf("C: Setting Cachesize: %d\n", buffsize);
	fflush(stdout);
	#endif
	
	retval = (*dbp)->set_cachesize((*dbp), 0, buffsize, 1); 
	if(retval != 0) {
		printf("C: Error in setting such a large cache\n");
		fflush(stdout);
		(*dbp)->err((*dbp), retval, "%s", "GalaxDB", dbname);	
		return retval;
	}
	
	if (bfixed) { 
	  if ((retval = (*dbp)->set_re_len(*dbp, fixed_length)) != 0) {
#ifdef DEBUG
	    printf("C: Error in setting to fixed length : %s\n", db_strerror(retval));
	    fflush(stdout);
#endif
	    (*dbp)->err(*dbp, retval, "%s", "GalaxDBrec_fixed_length [fixed]");
	    return retval;
	  }
	  
	  if((retval = (*dbp)->set_re_pad(*dbp, 0)) != 0) {
#ifdef DEBUG
	    printf("C: Error in setting padding : %s\n", db_strerror(retval));
	    fflush(stdout);
#endif
	    (*dbp)->err(*dbp, retval, "%s", "GalaxDBrec_fixed_length [pad]");
	    }	
	  #ifdef DEBUG
	  printf("C: Succeded in setting fixed size of %d\n", fixed_length);
	  fflush(stdout);
	  #endif 
	}

  

	retval = (*dbp)->open((*dbp), NULL, dbname, NULL, type, DB_CREATE, 0664);
	if(retval != 0) {
		(*dbp)->err((*dbp), retval, "%s", "GalaxDB", dbname);	
		return retval;
	}

	#ifdef DEBUG
	printf("C: Database %s opened \n", dbname);
	fflush(stdout);
	#endif

	return retval;
}

/*
 * Opens the BerkeleyDB database with specified name
 * If the database doesnot exists than creates it
 */
int GalaxDBopen(DB **dbp, char * dbname, DBTYPE type, 
		unsigned int open_flags, unsigned int set_flags, 
		int buffsize) {
  int false = 0; 
  return GalaxDBopen_internal(dbp, dbname, type, open_flags, set_flags, buffsize, false, 0) ;
}

/*
 *
 */
int GalaxDBopen_rec(DB **dbp, char * dbname, 		
		    unsigned int open_flags, unsigned int set_flags, 
		    int buffsize, int fixed, int fixed_length) {

  return GalaxDBopen_internal(dbp, dbname, DB_RECNO, open_flags, set_flags, buffsize, fixed, fixed_length);
}


int GalaxDBclose_no_sync_rec(DB *dbp) {
  int retval, i;


  retval = dbp->close(dbp, DB_NOSYNC);
  if(retval != 0) {
    dbp->err(dbp, retval, "%s", "GalaxDB");	
    return retval;
  }
  return retval;
}


/*
 *
 */
int GalaxDBclose_rec(DB *dbp) {
	return GalaxDBclose(dbp);
}

/*
 *
 */
int GalaxDBclose(DB *dbp) {
	int retval, i;
	entry * temp1, * temp2;

	retval = dbp->close(dbp, 0);
	if(retval != 0) {
		dbp->err(dbp, retval, "%s", "GalaxDB");	
		return retval;
	}

	// Remove all the entries associated with this DB * 
	if(sec_index_func_list != NULL) {
		#ifdef DEBUG
		printf("Going to remove entry from sec_index_func_list\n");
		#endif
		// Deletion in the front of link-list
		while(sec_index_func_list->dbp == dbp) {
			temp1 = sec_index_func_list;
			sec_index_func_list = temp1->next;
			temp1->next = NULL;
			#ifdef DEBUG
			printf("Removing entry %s from the head of the link-list \n", temp1->keyExtractFunc);
			#endif
			free(temp1->keyExtractFunc);
			free(temp1);	
			// List could have been empty by now so check
			if(sec_index_func_list == NULL) {
				#ifdef DEBUG
				printf("List is empty now !!!\n");
				#endif
				return retval;
			}
		}

		temp1 = sec_index_func_list;	
		while(temp1->next != NULL) {
			if(dbp == temp1->next->dbp) {
				#ifdef DEBUG
				printf("Removing Key extraction function for Secondry Index!!\n");
				#endif
				fflush(stdout);
				temp2 = temp1->next;	
				temp1->next = temp2->next;
				temp2->next = NULL;
				free(temp2->keyExtractFunc);
				free(temp2);
			}
			else {
				temp1= temp1->next;
			}
		}
	}
	return retval;
}

/*
 *
 */
int GalaxDBsync(DB *dbp) {
	int retval;

	retval = dbp->sync(dbp, 0);

	if(retval != 0) {
		dbp->err(dbp, retval, "%s", "GalaxDB");	
	}
	return retval;
}

int GalaxDBsync_rec(DB *dbp) {
		return GalaxDBsync(dbp);
}

int GalaxDBassociate(DB *pdbp, DB *sdbp, char * keyFunc, unsigned int flg) {
	int retval;
	entry * temp;

	//flg = xor_flags(num, opt);

	temp = (entry *) malloc(sizeof(entry));
	temp->dbp = sdbp;
	temp->keyExtractFunc = strdup(keyFunc);
	temp->next = sec_index_func_list;
	sec_index_func_list = temp;
	#ifdef DEBUG
	printf("C: associating %s \n", keyFunc);
	#endif
	retval = pdbp->associate(pdbp, NULL, sdbp, common_key_extract_func, flg);
	if(retval != 0) {
		pdbp->err(pdbp, retval, "%s", "GalaxDB");
	}
	
	return retval;
}

int GalaxDBassociate_to_rec(DB *pdbp, DB *sdbp, char * keyFunc, unsigned int flg) {
	int retval;
	entry * temp;


	temp = (entry *) malloc(sizeof(entry));
	temp->dbp = sdbp;
	temp->keyExtractFunc = strdup(keyFunc);
	temp->next = sec_index_func_list;
	sec_index_func_list = temp;
	#ifdef DEBUG
	printf("C: associating %s \n", keyFunc);
	fflush(stdout);
	#endif
	retval = pdbp->associate(pdbp, NULL, sdbp, common_key_extract_func_rec, flg);
	if(retval != 0) {
		pdbp->err(pdbp, retval, "%s", "GalaxDB");
	}
	
	return retval;
}

int GalaxDBassociate_rec(DB *pdbp, DB *sdbp, char * keyFunc, unsigned int flags) {
	return GalaxDBassociate(pdbp, sdbp, keyFunc, flags);
}

int GalaxDBassociate_with_rec(DB *pdbp, DB *sdbp, char * keyFunc, unsigned int flags) {
	return GalaxDBassociate_to_rec(pdbp, sdbp, keyFunc, flags);
}

int GalaxDBassociate_rec_with_rec(DB *pdbp, DB *sdbp, char * keyFunc, unsigned int flags) {
	return GalaxDBassociate_to_rec(pdbp, sdbp, keyFunc, flags);
}

int common_key_extract_func(DB * dbp, const DBT * key, const DBT * data, DBT * skey) {
	CAMLparam0();
	CAMLlocal1(result);

	int i;
	char * func_name = NULL, * key_string = NULL, * data_string = NULL, * sskey;
	entry * temp;
	value * cml_key_extract_func = NULL;

	key_string = (char *) malloc((key->size) * sizeof(char));
	data_string = (char *) malloc((data->size) * sizeof(char));

	strncpy(key_string, key->data, key->size);
	strncpy(data_string, data->data, data->size);

	#ifdef DEBUG
		printf("C: common_key_extract_func:\n\tkey_string = %s with size %i;\n\tdata_string = %s with size %i\n", key_string, key->size, data_string, data->size);
		fflush(stdout);
	#endif

	temp = sec_index_func_list;
	while(temp != NULL) {
		if(dbp == temp->dbp) {
			func_name=temp->keyExtractFunc;
			break;
		}
		temp = temp->next;
	}

	if(func_name == NULL) {
		#ifdef DEBUG
		printf("Error!!! no key extraction function for secondary index for this database found!!\n");
		#endif
		return(-1);
	}
	else {
		#ifdef DEBUG
		printf("The name of the function obtained = %s\n", func_name);
		#endif
		fflush(stdout);
	}

	apply_caml_closure(func_name, key_string, data_string, &sskey);

	#ifdef DEBUG
	printf("C:common_key_extract_func_rec : skey-created = %s\n", sskey); 
	fflush(stdout);
	#endif

	// Notify BerkeleyDB to free this memory after use
	skey->flags = DB_DBT_APPMALLOC;
	skey->data = malloc((strlen(sskey)+1) * sizeof(char));
	strncpy(skey->data, sskey, (strlen(sskey)+1));
	skey->size = strlen(sskey)+1;

	free(key_string);	
	free(data_string);	
	free(sskey);

	#ifdef DEBUG
	printf("C:FINISHED common_key_extract_func!\n");
	printf("C:WITH: skey = %s of size %i\n",(char *)skey->data,skey->size);
	#endif
	CAMLreturn(0);
}

void apply_caml_closure(char * func_name, char * key, char * data, char **skey_string) {
	CAMLparam0();
	CAMLlocal1(result);

	value * cml_key_extract_func = NULL;
	cml_key_extract_func = caml_named_value(func_name);

	result = callback2(*cml_key_extract_func, copy_string(key), copy_string(data));
	(*skey_string) = strdup(String_val(result));
	#ifdef DEBUG
	printf("C:apply_caml_closure = %s\n", (*skey_string)); 
	fflush(stdout);
	#endif
	CAMLreturn0;
}

int common_key_extract_func_rec(DB * dbp, const DBT * key, const DBT * data, DBT * skey) {
	int i;
	char * func_name = NULL, * data_string = NULL, * sskey = NULL;
	entry * temp;

	data_string = (char *) malloc((data->size) * sizeof(char));
	strncpy(data_string,data->data,data->size);

	#ifdef DEBUG
	printf("C: index_key_func_rec : data_string = %s with size %i\n", data_string, data->size);
	fflush(stdout);
	#endif

	temp = sec_index_func_list;
	while(temp != NULL) {
		if(dbp == temp->dbp) {
			func_name=temp->keyExtractFunc;
			break;
		}
		temp = temp->next;
	}

	if(func_name == NULL) {
		#ifdef DEBUG
		printf("Error!!! no key extraction function for secondary index for this database found!!\n");
		#endif
		return(-1);
	}

	apply_caml_closure_rec(func_name, data_string, &sskey);
	#ifdef DEBUG
	printf("C: skey-created = %s\n", sskey); 
	fflush(stdout);
	#endif

	// Notify BerkeleyDB to free the memory after use
	skey->flags = DB_DBT_APPMALLOC;
	skey->data = malloc((strlen(sskey)+1) * sizeof(char));
	strncpy(skey->data, sskey, (strlen(sskey)+1));
	skey->size = strlen(sskey)+1;

	free(data_string);
	free(sskey);

	#ifdef DEBUG
	printf("C:FINISHED!\n");
	fflush(stdout);
	#endif
	return 0;
}

void apply_caml_closure_rec(char * func_name, char * data, char **skey_string) {
	CAMLparam0();
	CAMLlocal1(result);

	value * cml_key_extract_func = NULL;
	cml_key_extract_func = caml_named_value(func_name);

	result = callback(*cml_key_extract_func, copy_string(data));
	(*skey_string) = strdup(String_val(result));
	CAMLreturn0;
}

/*
 *
 */
int GalaxDBput(DB *dbp, struct outgoing_array *k, struct outgoing_array *v, unsigned int flg) {
	int i=0, retval = 0;
	DBT key, data;

#ifdef DEBUG
	printf("Enterting DBput\n");
#endif       

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));

	key.data = k->contents;
	key.size = k->length;

	data.data = v->contents;
	data.size = v->length;


	#ifdef DEBUG
	printf("C:Put: key = %s and data = *%s*\n", key.data, data.data);
	printf("C:Put: keylen = %d and datalen = *%d*\n", key.size, data.size);
	printf("*");

	printf("\n\tkey: "); dump_bytes(key.data, key.size); 
	printf("\t data: "); dump_bytes(data.data, data.size);
	printf("*\n");
	fflush(stdout);
	#endif

	//	flg = xor_flags(num, opt); 

	retval = dbp->put(dbp, NULL, &key, &data, flg);	
	if(retval != 0) {
		dbp->err(dbp, retval, "%s", "GalaxDBput:");	
	}
	

	return retval;
}

/*
 *
 */
int GalaxDBget(DB *dbp, struct outgoing_array *k, struct outgoing_array* v, unsigned int flg) {
	int retval, i;
	DBT key, data;

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));

	key.data = k->contents;
	key.size = k->length;

#ifdef DEBUG
	printf("C: [get] Key = %s and Size of the key = %d from %X\n", key.data, key.size, dbp); 
	printf("\n\tkey: "); dump_bytes(key.data, key.size); 
	fflush(stdout);
#endif
	

	if((retval = dbp->get(dbp, NULL, &key, &data, flg))  != 0) {
#ifdef DEBUG
	  printf("C: GalaxDBget Error in reading the data: %d\n", retval);
	  fflush(stdout);
#endif
	  v->length   = 0;
	  return retval;
	}

	#ifdef DEBUG
	printf("C: BerkeleyDB returned: %s size = %d\n", (char*)data.data, data.size);
	fflush(stdout);
	#endif

	v->length   = data.size;
	v->contents = data.data;

	#ifdef DEBUG
	printf("C: GalaxDBget = length of string returning : %d\n", v->length);
	fflush(stdout);
	#endif
	return retval;
}

/*
 * Takes an already opened cursor (over a given DB) and preforms or continues the search
 */
int GalaxDBgetall_first(DBC *cursorp, 
			struct outgoing_array* k,
			struct outgoing_array* v) {
	int retval, i;
	DBT key, data;

	unsigned int flg = 0;	

#ifdef DEBUG
	printf("C: Curgetall_first:\n"); fflush(stdout);
#endif

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
        key.data = k->contents;
	key.size = k->length;
	// This means only DB_SET styles will work... 
	// we aren't passing flags right now anyway..
	

#ifdef DEBUG
	printf("[getall_first] Key = %s  and Size of the key = %d\n", 
	       k->contents, k->length); fflush(stdout);
	dump_bytes(k->contents, k->length);
#endif

	retval = cursorp->c_get(cursorp, &key, &data, DB_SET);	
	if(retval != 0) {
	  k->length = 0;
	  v->length = 0;
#ifdef DEBUG
	  printf ("[getall_first] Returned: %d\n", retval); 
#endif	  
	  return retval;
	}
	#ifdef DEBUG
	printf("C: BerkeleyDB returned: %s size = %d\n", (char*)data.data, data.size);
	fflush(stdout);
	#endif
	
	k->length   = key.size;
	k->contents = key.data;
	v->length   = data.size;
	v->contents = data.data;

	#ifdef DEBUG
	printf("C: GalaxDBgetall_first = length of string returning : %d\n", v->length);
	fflush(stdout);
	#endif
	return retval;
}



/*
 * Takes an already opened cursor (over a given DB) and preforms or continues the search
 */
int GalaxDBgetall_next(DBC *cursorp, 
		       struct outgoing_array* k,
		       struct outgoing_array* v) {
		
	int retval, i;
	DBT key, data;

	unsigned int flg = 0;	


	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));

        key.data = k->contents;
	key.size = k->length;

	data.data = v->contents; 
	data.size = v->length;

#ifdef DEBUG	
	printf("[getall_next] Key = %s and Size of the key = %d\n", 
	       k->contents, k->length); 
	fflush(stdout);
#endif

	retval = cursorp->c_get(cursorp, &key, &data, DB_NEXT_DUP);	
	if(retval != 0) {
#ifdef DEBUG
	  printf ("C: [getall_next] Berekely DB returned %d\n", retval);
	  fflush(stdout);
#endif 
	  //	  (*datalen) = 0;
	  k->length = 0;
	  v->length = 0;
	  return retval;
	}
	#ifdef DEBUG
	printf("C: [getall_next] BerkeleyDB returned: %s size = %d\n", (char*)data.data, data.size);
	fflush(stdout);
	#endif
	
/*	(*inkey)   = (char *) key.data;
	(*keylen)  = key.size;
	(*outdata) = (char *) data.data;
	(*datalen) = data.size;	*/

	k->length   = key.size;
	k->contents = key.data;
	v->length   = data.size;
	v->contents = data.data;
	

	#ifdef DEBUG
	//printf("C: GalaxDBget = length of string returning : %d\n", (*datalen));
	//fflush(stdout);
	#endif
	return retval;
}


int GalaxDBdel(DB *dbp, struct outgoing_array *k) {
	int retval;
	DBT key;
	unsigned int flg = 0;	

	memset(&key, 0, sizeof(key));

	key.data = k->contents;
	key.size = k->length;

	retval = dbp->del(dbp, NULL, &key, flg); 
	
	if(retval != 0) {
		dbp->err(dbp, retval, "%s", "GalaxDBdel:");	
	}

	GalaxDBsync(dbp);

	return retval;
}
/* BDB deletes all in a duplicate record */
int GalaxDBdelete_all(DB *dbp, struct outgoing_array* k) {
  return GalaxDBdel(dbp,k);
}
/* Here we need to use cursors because of BDBs implementation */
int GalaxDBdelete(DB *dbp, struct outgoing_array* k, struct outgoing_array *v) {  
  DBC* cursor;
  int retval;
  
  if ((retval = GalaxDBCursor(dbp, &cursor, 0)) != 0) { 
    dbp->err(dbp, retval, "%s", "GalaxDB delete");
    return retval;
  }
  
  // Set the cursor to the correct location.
  if ((retval = GalaxDBCurget_both(cursor, k, v, DB_GET_BOTH)) != 0) {
    dbp->err(dbp, retval, "%s", "GalaxDB delete [get_set]");
    return retval;
  }

#ifdef DEBUG
  printf("\t[set_range completed]\n");
#endif

  if ((retval = GalaxDBCurdel(cursor)) != 0) { 
    dbp->err(dbp, retval, "%s", "GalaxDB delete [curdel]");
    return retval;
  }

  if ((retval = GalaxDBCurClose(cursor)) != 0) { 
    dbp->err(dbp, retval, "%s", "GalaxDB close [curdel]");
    return retval;
  }

  return retval;
}

int GalaxDBput_rec(DB *dbp, unsigned int position, char * indata, int datalen, unsigned int flg) {
	int retval;
	DBT key, data;
	db_recno_t recno = position;

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));

	key.data = &recno;
	key.size = sizeof(recno);

	data.data = indata;
	data.size = datalen;

	//flg = xor_flags(num, opt);

	#ifdef DEBUG
	printf("C: [key] Inserting Record = %s of length %d at position = %u\n", (char*)key.data, key.size, recno);
	dump_bytes(key.data, key.size);
	printf("C: Inserting Record = %s of length %d at position = %u\n", (char*)data.data, data.size, recno);
	dump_bytes(data.data, data.size);
	if(dbp) { printf("C: [More] dbp: %x\n", dbp); }
	
	else { printf("dbp is NULL!\n"); }
	fflush(stdout);
	#endif
	retval = dbp->put(dbp, NULL, &key, &data, flg);	

	if(retval != 0) {
		dbp->err(dbp, retval, "%s", "GalaxDBput_rec");	
	}

	return retval;
}

int GalaxDBget_rec(DB *dbp, unsigned int position, struct outgoing_array* poa, unsigned int flg) {
	int retval, i;
	DBT key, data;
	db_recno_t recno = position;

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));

	key.data = &recno;
	key.size = sizeof(recno);

	retval = dbp->get(dbp, NULL, &key, &data, flg);	
	if(retval != 0) {
	  poa->length = -1;	  
	  return retval;
	}

	/* 	(*outdata) = (char *) data.data;
	   (*datalen) = data.size; */

	poa->contents = (char*) data.data;
	poa->length = data.size;


	#ifdef DEBUG
	printf("C: Record at position = %d Data = %s of length = %d\n", 
	       position, poa->contents, data.size);
	fflush(stdout);
	#endif
	
	return retval;
}

int GalaxDBdel_rec(DB *dbp, unsigned int position) {
	int retval;
	DBT key;
	unsigned int flg = 0;
	db_recno_t recno = position;

	memset(&key, 0, sizeof(key));

	key.data = &recno;
	key.size = sizeof(recno);

	#ifdef DEBUG
	printf("C: Deleting Record at postion = %d\n", position);
	#endif
	fflush(stdout);

	retval = dbp->del(dbp, NULL, &key, flg);	
	if(retval != 0) {
		dbp->err(dbp, retval, "%s", "GalaxDBdel_recl:");	
	}

	GalaxDBsync_rec(dbp);

	return retval;
}

/*
 * Cursor Operations
 */

int GalaxDBCursor(DB *dbp, DBC **cursorp, unsigned int flg) {
	int retval;

	#ifdef DEBUG
	printf("C: trying to get a cursor, flag = %d\n", flg);
	#endif
	fflush(stdout);

	retval = dbp->cursor(dbp, NULL, cursorp, flg);

	if(retval != 0) {
		dbp->err(dbp, retval, "%s", "GalaxDB");	
	}

	#ifdef DEBUG
	printf("C: Obtained a cursor [Cursor: %X| DB: %X]\n",  *cursorp,dbp);
	#endif
	fflush(stdout);

	return retval;
}

int GalaxDBCurClose(DBC *cursor) {
	int retval;
#ifdef DEBUG
	printf("C: CurClose:\n");	
#endif
	retval = cursor->c_close(cursor);

	if(retval != 0) {
		#ifdef DEBUG
		printf("CurClose: Error number %d in GalaxDB", retval);	
		#endif
	}
	return retval;
}

int GalaxDBCurput(DBC *cursor, char * inkey, int keylen, char * indata, int datalen, unsigned int flg) {
	int retval;
	DBT key, data;

	#ifdef DEBUG
	printf("C: Curput called placed key = %s and data = %s\n", inkey, indata);
	fflush(stdout);
	#endif
	
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));

	key.data = inkey;
	key.size = keylen;


	data.data = indata;
	data.size = datalen;


	//	flg = xor_flags(num, opt);

	retval = cursor->c_put(cursor, &key, &data, flg);
	if(retval != 0) {
		#ifdef DEBUG
		printf("CurClose: Error number %d in GalaxDB", retval);	
		#endif
		fflush(stdout);
	}
	return retval;
}

int GalaxDBCurget(DBC *cursor, struct outgoing_array *k, struct outgoing_array *v , unsigned int flg) {
	int retval, i;
	DBT key, data;

	#ifdef DEBUG
	printf("C: reading from cursor, [cursor: %X] flag = %d\n", cursor, flg);
	fflush(stdout);
	#endif

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));

        key.data = k->contents;
	key.size = k->length;

	data.data = v->contents; 
	data.size = v->length;

	retval = cursor->c_get(cursor, &key, &data, flg);	
	if(retval != 0) {
	  k->length = 0;
	  v->length = 0;
#ifdef DEBUG
		printf("C: Cursor error: %s [%d]\n", db_strerror(retval),retval);
#endif
		return retval;
	}

	k->length   = key.size;
	k->contents = key.data;
	v->length   = data.size;
	v->contents = data.data;

	#ifdef DEBUG
	printf("C: Key = %s %d and Data = %s %d\n",
	       k->contents,k->length,v->contents,v->length);
	#endif
	fflush(stdout);

	return retval;
}
/* 
   These two functions are necessary because the get, get_set and
   get_both cursor functions have different IDL signatures.  The
   default get has only outgoing parameters, while get_set has one
   incoming parameter and get_both has two incoming parameters.

   In the BDB interface, which one is used depends on which flags
   are passed. 
*/
int GalaxDBCurget_set(DBC *cursor, struct outgoing_array *k, struct outgoing_array *v , unsigned int flg) {
  if ((flg & (DB_SET | DB_SET_RANGE)) > 0) {
    return GalaxDBCurget(cursor, k, v, flg);
  } else {
#ifdef DEBUG
    printf("C: Cursor Using get_set without a correct flag set!\n");
#endif
    return -1; // We should define an error code...
  }
}

int GalaxDBCurget_both(DBC *cursor, struct outgoing_array *k, struct outgoing_array *v , unsigned int flg) {
  if ((flg & (DB_GET_BOTH_RANGE | DB_GET_BOTH)) > 0) {
    return GalaxDBCurget(cursor, k, v, flg);
  } else {
#ifdef DEBUG
    printf("C: Cursor Using get_both without the flag set!\n");
#endif
    return -1;
  }
}


int GalaxDBCurdel(DBC *cursor) {
	int retval;
	unsigned int flg = 0;	// ?
#ifdef DEBUG
	printf("C: CurDelete:\n"); fflush(stdout);
#endif
	retval = cursor->c_del(cursor, flg);
	if(retval != 0) {
		#ifdef DEBUG
		printf("CurClose: Error number %d in GalaxDB", retval);	
		#endif
		fflush(stdout);
	}

	return retval;
}

int GalaxDBCursor_rec(DB *dbp, DBC **cursorp, unsigned int flg) {
    int retval;

    retval = GalaxDBCursor(dbp, cursorp, flg);
    return retval;
}

int GalaxDBCurClose_rec(DBC *cursor) {
    int retval;
    retval = GalaxDBCurClose(cursor);
    return retval;
}

int GalaxDBCurput_rec(DBC *cursor, char * indata, int datalen, unsigned int flg) {
    int retval;
    DBT key, data;
    db_recno_t recno;


#ifdef DEBUG
    printf("C: Curput_rec called placed record = %s\n", indata);
#endif
    fflush(stdout);
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    key.data = &recno; 
    key.size = sizeof(db_recno_t); 

    data.data = indata;
    data.size = datalen;
    //data.size = strlen(indata) + 1;

    //flg = xor_flags(num, opt);

    retval = cursor->c_put(cursor, &key, &data, flg);
    if(retval != 0) {
		#ifdef DEBUG
        printf("CurClose: Error number %d in GalaxDB", retval);
		#endif
        fflush(stdout);
    }
    return retval;
}


int GalaxDBCurget_rec(DBC *cursor, struct outgoing_array *v, unsigned int flg) {
    int retval, i;
    DBT key, data;
	db_recno_t recno;
	//    unsigned int flg = 0;

#ifdef DEBUG
    printf("C: reading from cursor, flag = %d\n", flg);
#endif
    fflush(stdout);

    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    key.data = &recno; 
    key.size = sizeof(db_recno_t); 
    
    //    flg = xor_flags(num, opt);

    retval = cursor->c_get(cursor, &key, &data, flg);
    if(retval != 0) {
      v->length = 0;
      return retval;
    }
    v->length = data.size;
    v->contents = data.data;
    //    (*outdata) = (char *) data.data; 
    //(*datalen) = data.size;
#ifdef DEBUG
    printf("C: Record data and length  = %s %d\n", v->contents,v->length);
#endif
    fflush(stdout);

    return retval;
}

int GalaxDBCurget_set_rec(DBC *cursor, unsigned int position, struct outgoing_array *v, unsigned int flg) {
    int retval, i;
    DBT key, data;
    db_recno_t recno = position;

#ifdef DEBUG
    printf("C: reading from cursor, flag = %d\n", flg);
#endif
    fflush(stdout);

    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    key.data = &recno; 
    key.size = sizeof(db_recno_t);     

    retval = cursor->c_get(cursor, &key, &data, flg);
    if(retval != 0) {
      v->length = 0;
      return retval;
    }
    v->length = data.size;
    v->contents = data.data;
    
#ifdef DEBUG
    printf("C: Record data and length  = %s %d\n", v->contents,v->length);
#endif
    fflush(stdout);

    return retval;
}

int GalaxDBCurdel_rec(DBC *cursor) {
    int retval;

    retval = GalaxDBCurdel(cursor); 

    return retval;
}

int get_const_DB_NOTFOUND() { return DB_NOTFOUND; }
