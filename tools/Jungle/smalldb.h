#include<db.h>

struct outgoing_array { int length; char* contents;} ;

enum galaxDBflags {
    G_DB_AFTER = DB_AFTER,     
    G_DB_APPEND = DB_APPEND,     
    G_DB_BEFORE = DB_BEFORE,      
    G_DB_CACHED_COUNTS = DB_CACHED_COUNTS,
    //    DB_COMMIT, 
    G_DB_CONSUME = DB_CONSUME,       
    G_DB_CONSUME_WAIT = DB_CONSUME_WAIT,   
    G_DB_CURRENT = DB_CURRENT,   
    G_DB_FAST_STAT = DB_FAST_STAT,  
    G_DB_FIRST = DB_FIRST,       
    G_DB_GET_BOTH = DB_GET_BOTH,     
    G_DB_GET_BOTHC = DB_GET_BOTHC,     
    G_DB_GET_BOTH_RANGE = DB_GET_BOTH_RANGE, 
    G_DB_GET_RECNO = DB_GET_RECNO,    
    G_DB_JOIN_ITEM = DB_JOIN_ITEM,     
    G_DB_KEYFIRST = DB_KEYFIRST, 
    G_DB_KEYLAST = DB_KEYLAST,
    G_DB_LAST = DB_LAST,
    G_DB_NEXT = DB_NEXT,    
    G_DB_NEXT_DUP = DB_NEXT_DUP, 
    G_DB_NEXT_NODUP = DB_NEXT_NODUP,  
    G_DB_NODUPDATA = DB_NODUPDATA,  
    G_DB_NOOVERWRITE = DB_NOOVERWRITE, 
    G_DB_NOSYNC = DB_NOSYNC,   
    G_DB_POSITION = DB_POSITION,  
    //G_DB_POSITIONI = DB_POSITIONI,  
    G_DB_PREV = DB_PREV,        
    G_DB_PREV_NODUP = DB_PREV_NODUP,   
    G_DB_RECORDCOUNT = DB_RECORDCOUNT,   
    G_DB_SET = DB_SET,            
    G_DB_SET_LOCK_TIMEOUT = DB_SET_LOCK_TIMEOUT,
    G_DB_SET_RANGE = DB_SET_RANGE,
    G_DB_SET_RECNO = DB_SET_RECNO,    
    G_DB_SET_TXN_NOW = DB_SET_TXN_NOW,   
    G_DB_SET_TXN_TIMEOUT = DB_SET_TXN_TIMEOUT,
    G_DB_UPDATE_SECONDARY = DB_UPDATE_SECONDARY,
    G_DB_WRITECURSOR = DB_WRITECURSOR,
    G_DB_WRITELOCK = DB_WRITELOCK, 
    G_DB_MULTIPLE = DB_MULTIPLE,   
    G_DB_MULTIPLE_KEY = DB_MULTIPLE_KEY,
    G_DB_RMW = DB_RMW,          
    G_DB_AUTO_COMMIT = DB_AUTO_COMMIT,   
    G_DB_DIRTY_READ = DB_DIRTY_READ,     
    G_DB_CREATE = DB_CREATE,     
    G_DB_FORCE = DB_FORCE,       
    G_DB_NOMMAP = DB_NOMMAP,       
    G_DB_RDONLY = DB_RDONLY,        
    G_DB_RECOVER = DB_RECOVER,        
    G_DB_THREAD = DB_THREAD,         
    G_DB_TRUNCATE = DB_TRUNCATE,       
    G_DB_TXN_NOSYNC = DB_TXN_NOSYNC,     
    G_DB_USE_ENVIRON = DB_USE_ENVIRON,    
    G_DB_USE_ENVIRON_ROOT = DB_USE_ENVIRON_ROOT,
    G_DB_EXCL = DB_EXCL,
    //DB_CHKSUM_SHA1,    
    G_DB_DUP = DB_DUP,     
    G_DB_DUPSORT = DB_DUPSORT,  
    G_DB_ENCRYPT = DB_ENCRYPT,   
    G_DB_RECNUM = DB_RECNUM,     
    G_DB_RENUMBER = DB_RENUMBER,    
    G_DB_REVSPLITOFF = DB_REVSPLITOFF,  
    G_DB_SNAPSHOT = DB_SNAPSHOT,
    G_DB_INIT_LOCK = DB_INIT_LOCK,     
    G_DB_INIT_TXN = DB_INIT_TXN,       
    G_DB_INIT_LOG = DB_INIT_LOG        
};

typedef enum galaxDBflags flag_set;


/*
 * This structure stores the name of the Ocaml Function 
 * which would compute the key for the secondary index.
 */
typedef struct ent {
	DB *dbp;		
	char * keyExtractFunc;
	struct ent * next;
} entry;

/*
 * List of all the functions to compute secondary index
 */
extern entry * sec_index_func_list;

/*
 *
 *
 */

int GalaxDB_Env_open(DB_ENV ** dbenvp, char * db_home, unsigned int flg, int mode);
int GalaxDB_Env_close(DB_ENV * ebenvp);

char * GalaxDBerror(int error);
/*
 *	These are the database operations associated with
 *		- Btree database
 *		- Hash database
 */
int GalaxDBopen(DB **dbp, char * dbname, DBTYPE type, 
		unsigned int open_flags, unsigned int set_flags, 
		int dbbuffsize);

int GalaxDBclose(DB *dbp);
int GalaxDBsync(DB *dbp); 
int GalaxDBput(DB *dbp, struct outgoing_array *k, struct outgoing_array *v, unsigned int flg);
int GalaxDBget(DB *dbp, struct outgoing_array *k,  struct outgoing_array* v, unsigned int flg); 
int GalaxDBdel(DB *dbp, struct outgoing_array *k);
int GalaxDBdel_all(DB *dbp, struct outgoing_array *k);
int GalaxDBdelete(DB *dbp, struct outgoing_array *k, struct outgoing_array *v);

/* Record Operations */
/*
 *	These are the database operations associated with
 *		- Record Database 
 *		- Queue Database 
 */
int GalaxDBopen_rec(DB **dbp, char *dbname, 
		    unsigned int open_flags, unsigned int set_flags,
		    int dbbuffsize, int bfixed, int fixed_length);
int GalaxDBclose_rec(DB *dbp);
int GalaxDBclose_no_sync_rec(DB *dbp);
int GalaxDBsync_rec(DB *dbp); 
int GalaxDBput_rec(DB *dbp, unsigned int position, char * indata, int datalen, unsigned int flg);
//int GalaxDBget_rec(DB *dbp, unsigned int position, char **outdata, int * datalen, flag_set flgs); 
int GalaxDBget_rec(DB *dbp, unsigned int position, struct outgoing_array* oa, flag_set flgs); 
int GalaxDBdel_rec(DB *dbp, unsigned int position);

/* Associate a Secondary Index */

/* Associate a Btree/Hash Secondary Index with Btree/Hash Primary Database */
int GalaxDBassociate(DB *pdbp, DB *sdbp, char * keyExtractFunc, unsigned int flg); 
/* Associate a Record Secondary Index with Btree/Hash Primary Database */
int GalaxDBassociate_rec(DB *pdbp, DB *sdbp, char * keyExtractFunc, unsigned int flg); 
/* Associate a Btree/Hash Secondary Index with Record Primary Database */
int GalaxDBassociate_with_rec(DB *pdbp, DB *sdbp, char * keyExtractFunc, unsigned int flg); 
/* Associate a Record Secondary Index with Record Primary Database */
int GalaxDBassociate_rec_with_rec(DB *pdbp, DB *sdbp, char * keyExtractFunc, unsigned int flg); 

int common_key_extract_func(DB * dbp, const DBT * key, const DBT * data, DBT * skey); 
int common_key_extract_func_rec(DB * dbp, const DBT * key, const DBT * data, DBT * skey); 

void apply_caml_closure(char * func_name, char * key, char * data, char **skey_string); 
void apply_caml_closure_rec(char * func_name, char * data, char **skey_string); 

/* Cursor operations*/
/*
 *	These are the cursor operations associated with
 *		- Btree database
 *		- Hash database
 */
int GalaxDBCursor(DB *dbp, DBC **cursorp, unsigned int flg);
int GalaxDBCurClose(DBC *cursor);
int GalaxDBCurput(DBC *cursor, char * inkey, int keylen, char * indata, int datalen, unsigned int flg);
int GalaxDBCurget(DBC *cursor, struct outgoing_array* k, struct outgoing_array* v, unsigned int flg);
int GalaxDBCurdel(DBC *cursor);
int GalaxDBCurget_set (DBC *cursor, struct outgoing_array* k, struct outgoing_array* v, unsigned int flg);
int GalaxDBCurget_both(DBC *cursor, struct outgoing_array* k, struct outgoing_array* v, unsigned int flg);

/*
 *	These are the cursor operations associated with
 *		- Record Database 
 *		- Queue Database 
 */
int GalaxDBCursor_rec(DB *dbp, DBC **cursorp, unsigned int flg); 
int GalaxDBCurClose_rec(DBC *cursor); 
int GalaxDBCurput_rec(DBC *cursor, char * indata, int datalen, unsigned int flg); 
int GalaxDBCurget_rec(DBC *cursor, struct outgoing_array* oa, unsigned int flg); 
int GalaxDBCurget_rec_set(DBC *cursor, unsigned int position, struct outgoing_array* oa, unsigned int flg); 
int GalaxDBCurdel_rec(DBC *cursor); 

/* These are compatibility functions to get constants that
   are needed on the ml side. */
int get_const_DB_NOTFOUND();
