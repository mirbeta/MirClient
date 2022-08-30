{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2013                                        }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit odbccst;

interface

const
 SQL_ERROR = -1;
 SQL_SUCCESS = 0;
 SQL_SUCCESS_WITH_INFO = 1;
 SQL_NTS = -3;
 SQL_NULL_HSTMT = 0;
 SQL_CLOSE = 0;
 SQL_COMMIT = 0;
 SQL_DROP         = 1;
 SQL_UNBIND       = 2;
 SQL_RESET_PARAMS = 3;

 SQL_NEED_DATA = 99;

 SQL_PARAM_TYPE_UNKNOWN  =0;
 SQL_PARAM_INPUT         =1;
 SQL_PARAM_INPUT_OUTPUT  =2;
 SQL_RESULT_COL          =3;

 SQL_CHAR     = 1;
 SQL_NUMERIC  = 2;
 SQL_DECIMAL  = 3;
 SQL_INTEGER  = 4;
 SQL_SMALLINT = 5;
 SQL_FLOAT    = 6;
 SQL_REAL     = 7;
 SQL_DOUBLE   = 8;
 SQL_VARCHAR  = 12;

 SQL_DATE        =  9;
 SQL_TIME        = 10;
 SQL_TIMESTAMP   = 11;
 SQL_LONGVARCHAR = -1;
 SQL_BINARY      = -2;
 SQL_VARBINARY   = -3;
 SQL_LONGVARBINARY = -4;
 SQL_BIGINT      = -5;
 SQL_TINYINT     = -6;
 SQL_BIT         = -7;

 {constants for SQLGetInfo}

 SQL_INFO_FIRST          = 0;
 SQL_ACTIVE_CONNECTIONS  = 0;

 SQL_ACTIVE_STATEMENTS   = 1;
 SQL_DATA_SOURCE_NAME    = 2;
 SQL_DRIVER_HDBC         = 3;
 SQL_DRIVER_HENV         = 4;
 SQL_DRIVER_HSTMT        = 5;
 SQL_DRIVER_NAME         = 6;
 SQL_DRIVER_VER          = 7;
 SQL_FETCH_DIRECTION     = 8;
 SQL_ODBC_CONFORMANCE    = 9;
 SQL_ODBC_VER            = 10;

 SQL_ROW_UPDATES           = 11;
 SQL_SAG_CLI_CONFORMANCE   = 12;
 SQL_SERVER_NAME           = 13;
 SQL_SEARCH_PATTERN_ESCAPE = 14;
 SQL_SYNTAX_COMPATIBILITY  = 15;

 SQL_DATABASE_NAME         = 16;
 SQL_DBMS_NAME             = 17;
 SQL_DBMS_VER              = 18;
 SQL_ACCESSIBLE_TABLES     = 19;
 SQL_ACCESSIBLE_PROCEDURES = 20;

 SQL_COLLATION_SEQ             = 21;
 SQL_CONCAT_NULL_BEHAVIOR      = 22;
 SQL_CURSOR_COMMIT_BEHAVIOR    = 23;
 SQL_CURSOR_ROLLBACK_BEHAVIOR  = 24;
 SQL_DATA_SOURCE_READ_ONLY     = 25;
 SQL_DEFAULT_TXN_ISOLATION     = 26;
 SQL_EXPRESSIONS_IN_ORDERBY    = 27;
 SQL_IDENTIFIER_CASE           = 28;
 SQL_IDENTIFIER_QUOTE_CHAR     = 29;
 SQL_MAX_COLUMN_NAME_LEN       = 30;

 SQL_MAX_CURSOR_NAME_LEN       = 31;
 SQL_MAX_OWNER_NAME_LEN        = 32;
 SQL_MAX_PROCEDURE_NAME_LEN    = 33;
 SQL_MAX_QUALIFIER_NAME_LEN    = 34;
 SQL_MAX_TABLE_NAME_LEN        = 35;
 SQL_MULT_RESULT_SETS          = 36;
 SQL_MULTIPLE_ACTIVE_TXN       = 37;
 SQL_OUTER_JOINS               = 38;
 SQL_OWNER_TERM                = 39;
 SQL_PROCEDURE_TERM            = 40;

 SQL_QUALIFIER_NAME_SEPARATOR  = 41;
 SQL_QUALIFIER_TERM            = 42;
 SQL_SCROLL_CONCURRENCY        = 43;
 SQL_SCROLL_OPTIONS            = 44;
 SQL_TABLE_TERM                = 45;
 SQL_TXN_CAPABLE               = 46;
 SQL_USER_NAME                 = 47;

 SQL_CONVERT_FUNCTIONS         = 48;
 SQL_NUMERIC_FUNCTIONS         = 49;
 SQL_STRING_FUNCTIONS          = 50;

 SQL_SYSTEM_FUNCTIONS          = 51;
 SQL_TIMEDATE_FUNCTIONS        = 52;
 SQL_CONVERT_BIGINT            = 53;
 SQL_CONVERT_BINARY            = 54;
 SQL_CONVERT_BIT               = 55;
 SQL_CONVERT_CHAR              = 56;
 SQL_CONVERT_DATE              = 57;
 SQL_CONVERT_DECIMAL           = 58;
 SQL_CONVERT_DOUBLE            = 59;
 SQL_CONVERT_FLOAT             = 60;

 SQL_CONVERT_INTEGER           = 61;
 SQL_CONVERT_LONGVARCHAR       = 62;
 SQL_CONVERT_NUMERIC           = 63;
 SQL_CONVERT_REAL              = 64;
 SQL_CONVERT_SMALLINT          = 65;
 SQL_CONVERT_TIME              = 66;
 SQL_CONVERT_TIMESTAMP         = 67;
 SQL_CONVERT_TINYINT           = 68;
 SQL_CONVERT_VARBINARY         = 69;
 SQL_CONVERT_VARCHAR           = 70;
 SQL_CONVERT_LONGVARBINARY     = 71;
 SQL_TXN_ISOLATION_OPTION      = 72;
 SQL_INFO_LAST                 = 72;
 SQL_INFO_DRIVER_START         = 1000;

 SQL_DRIVER_HLIB           = 76;
 SQL_DRIVER_ODBC_VER       = 77;
 SQL_LOCK_TYPES            = 78;
 SQL_POS_OPERATIONS        = 79;
 SQL_POSITIONED_STATEMENTS = 80;
 SQL_GETDATA_EXTENSIONS    = 81;
 SQL_BOOKMARK_PERSISTENCE  = 82;
 SQL_STATIC_SENSITIVITY    = 83;
 SQL_FILE_USAGE            = 84;
 SQL_NULL_COLLATION        = 85;
 SQL_ALTER_TABLE           = 86;
 SQL_COLUMN_ALIAS          = 87;
 SQL_GROUP_BY              = 88;
 SQL_KEYWORDS              = 89;
 SQL_ORDER_BY_COLUMNS_IN_SELECT = 90;
 SQL_OWNER_USAGE             = 91;
 SQL_QUALIFIER_USAGE         = 92;
 SQL_QUOTED_IDENTIFIER_CASE  = 93;
 SQL_SPECIAL_CHARACTERS      = 94;
 SQL_SUBQUERIES              = 95;
 SQL_UNION                   = 96;
 SQL_MAX_COLUMNS_IN_GROUP_BY = 97;
 SQL_MAX_COLUMNS_IN_INDEX    = 98;
 SQL_MAX_COLUMNS_IN_ORDER_BY = 99;
 SQL_MAX_COLUMNS_IN_SELECT   = 100;
 SQL_MAX_COLUMNS_IN_TABLE    = 101;
 SQL_MAX_INDEX_SIZE          = 102;
 SQL_MAX_ROW_SIZE_INCLUDES_LONG = 103;
 SQL_MAX_ROW_SIZE            = 104;
 SQL_MAX_STATEMENT_LEN       = 105;
 SQL_MAX_TABLES_IN_SELECT    = 106;

 SQL_C_CHAR   = SQL_CHAR;
 SQL_C_LONG   = SQL_INTEGER;
 SQL_C_SHORT  = SQL_SMALLINT;
 SQL_C_FLOAT  = SQL_REAL;
 SQL_C_DOUBLE = SQL_DOUBLE;
 SQL_C_DATE   = SQL_DATE;

 SQL_FETCH_NEXT  =  1;
 SQL_FETCH_FIRST =  2;
 SQL_FETCH_LAST  =  3;
 SQL_FETCH_PRIOR =  4;
 SQL_FETCH_ABSOLUTE =   5;
 SQL_FETCH_RELATIVE =   6;
 SQL_FETCH_BOOKMARK =   8;

 SQL_DRIVER_NOPROMPT   = 0;
 SQL_DRIVER_COMPLETE   = 1;
 SQL_DRIVER_PROMPT     = 2;
 SQL_DRIVER_COMPLETE_REQUIRED  = 3;

 SQL_FILE_NOT_SUPPORTED = $0000;
 SQL_FILE_TABLE         = $0001;
 SQL_FILE_QUALIFIER     = $0002;

 ODBC_ADD_DSN    = 1;               // Add data source
 ODBC_CONFIG_DSN = 2;               // Configure (edit) data source
 ODBC_REMOVE_DSN = 3;               // Remove data source

 SQL_ACCESS_MODE =                        101;
 SQL_AUTOCOMMIT  =                        102;
 SQL_LOGIN_TIMEOUT =                      103;
 SQL_OPT_TRACE     =                      104;
 SQL_OPT_TRACEFILE =                      105;
 SQL_TRANSLATE_DLL =                      106;
 SQL_TRANSLATE_OPTION =                   107;
 SQL_TXN_ISOLATION    =                   108;
 SQL_CURRENT_QUALIFIER  =                 109;
 SQL_ODBC_CURSORS    =                    110;
 SQL_QUIET_MODE      =                    111;
 SQL_PACKET_SIZE     =                    112;
 SQL_CONN_OPT_MAX    =                    SQL_PACKET_SIZE;
 SQL_CONNECT_OPT_DRVR_START  =            1000;
 SQL_CONN_OPT_MIN   =                     SQL_ACCESS_MODE;

 SQL_MODE_READ_WRITE  =           0;
 SQL_MODE_READ_ONLY   =           1;
 SQL_MODE_DEFAULT     =         SQL_MODE_READ_WRITE;

 SQL_OK = [SQL_SUCCESS,SQL_SUCCESS_WITH_INFO];

 SQL_COMMAND_SIZE = 1023;
 SQL_DATA_SIZE = 255;
 SQL_MAX_COLUMNS = 255;

 SQL_POSITION = 0;
 SQL_REFRESH  = 1;
 SQL_UPDATE   = 2;
 SQL_DELETE   = 3;
 SQL_ADD      = 4;

 SQL_LOCK_NO_CHANGE = 0;
 SQL_LOCK_EXCLUSIVE = 1;
 SQL_LOCK_UNLOCK    = 2;

 SQL_QUERY_TIMEOUT               =        0;
 SQL_MAX_ROWS                    =        1;
 SQL_NOSCAN                      =        2;
 SQL_MAX_LENGTH                  =        3;
 SQL_ASYNC_ENABLE                =        4;
 SQL_BIND_TYPE                   =        5;
 SQL_CURSOR_TYPE                 =        6;
 SQL_CONCURRENCY                 =        7;
 SQL_KEYSET_SIZE                 =        8;
 SQL_ROWSET_SIZE                 =        9;
 SQL_SIMULATE_CURSOR             =       10;
 SQL_RETRIEVE_DATA               =       11;
 SQL_USE_BOOKMARKS               =       12;
 SQL_GET_BOOKMARK                =       13;
 SQL_ROW_NUMBER                  =       14;


{ SQL_CONCURRENCY options }
 SQL_CONCUR_READ_ONLY        =    1;
 SQL_CONCUR_LOCK             =    2;
 SQL_CONCUR_ROWVER           =    3;
 SQL_CONCUR_VALUES           =    4;
 SQL_CONCUR_DEFAULT          =  SQL_CONCUR_READ_ONLY;

{ SQL_CURSOR_TYPE options }
 SQL_CURSOR_FORWARD_ONLY        = 0;
 SQL_CURSOR_KEYSET_DRIVEN       = 1;
 SQL_CURSOR_DYNAMIC             = 2;
 SQL_CURSOR_STATIC              = 3;
 SQL_CURSOR_TYPE_DEFAULT        = SQL_CURSOR_FORWARD_ONLY;


{ SQLColAttributes defines }
 SQL_COLUMN_COUNT           = 0;
 SQL_COLUMN_NAME            = 1;
 SQL_COLUMN_TYPE            = 2;
 SQL_COLUMN_LENGTH          = 3;
 SQL_COLUMN_PRECISION       = 4;
 SQL_COLUMN_SCALE           = 5;
 SQL_COLUMN_DISPLAY_SIZE    = 6;
 SQL_COLUMN_NULLABLE        = 7;
 SQL_COLUMN_UNSIGNED        = 8;
 SQL_COLUMN_MONEY           = 9;
 SQL_COLUMN_UPDATABLE        =    10;
 SQL_COLUMN_AUTO_INCREMENT   =    11;
 SQL_COLUMN_CASE_SENSITIVE   =    12;
 SQL_COLUMN_SEARCHABLE       =    13;
 SQL_COLUMN_TYPE_NAME        =    14;
 SQL_COLUMN_TABLE_NAME       =    15;
 SQL_COLUMN_OWNER_NAME       =    16;
 SQL_COLUMN_QUALIFIER_NAME   =    17;
 SQL_COLUMN_LABEL            =    18;
 SQL_COLATT_OPT_MAX          =    SQL_COLUMN_LABEL;

{ SQLColAttributes subdefines for SQL_COLUMN_UPDATABLE }
 SQL_ATTR_READONLY            =   0;
 SQL_ATTR_WRITE               =   1;
 SQL_ATTR_READWRITE_UNKNOWN   =   2;


 SQL_MAX_MESSAGE_LENGTH  = 512;

 SQL_INDEX_ALL    =               1;
 SQL_ENSURE    =               1;


implementation

end.
