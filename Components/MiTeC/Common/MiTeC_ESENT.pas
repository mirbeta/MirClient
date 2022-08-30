{*******************************************************}
{                                                       }
{                MiTeC Common Routines                  }
{         Microsoft ESENT database API interface        }
{           (Extensible Storage Engine)                 }
{         Copyright (c) 2013 Michal Mutl                }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_ESENT;

{$ALIGN 4}
{$IFDEF UNICODE}
  {$DEFINE JET_UNICODE}
{$ENDIF}

{$DEFINE DYNAMIC_LINK}

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Math
     {$ELSE}
     Windows, SysUtils, Math
     {$ENDIF}
     ;

const
  {$IFDEF JET_UNICODE}
  Suffix = 'W';
  {$ELSE}
  Suffix = 'A';
  {$ENDIF}

  JET_VERSION_WIN2000   = $0500;  // Windows 2000
  JET_VERSION_WINXP     = $0501;  // Windows XP
  JET_VERSION_WIN2003   = $0502;  // Windows Server 2003
  JET_VERSION_WINVISTA  = $0600;  // Windows Vista / Windows Server 2008
  JET_VERSION_WIN7      = $0601;  // Windows 7
  JET_VERSION_WIN8      = $0602;  // Windows 8

  JET_VERSION = JET_VERSION_WINXP;

type
  long       = LongInt;
  PtrPointer = ^Pointer;
  USHORT     = Word;

type
  JET_ERR = type LongInt;

  JET_HANDLE    = type Pointer; { backup file handle }
  JET_INSTANCE  = type Pointer; { Instance Identifier }
  JET_SESID     = type Pointer;   { Session Identifier }
  JET_TABLEID   = type Pointer; { Table Identifier }
  {$IF JET_VERSION >= $0501}
  JET_LS        = type Pointer;   { Local Storage }
  {$IFEND}

  JET_COLUMNID  = type Cardinal; { Column Identifier }

  PJET_COLUMNID  = ^JET_COLUMNID;
  PJET_INSTANCE  = ^JET_INSTANCE;


  tagJET_INDEXID = record
    cbStruct  : Cardinal;
    rgbIndexId: array[0..SizeOf(Pointer)+SizeOf(Cardinal)+SizeOf(Cardinal)-1] of AnsiChar;
  end;
  JET_INDEXID    = tagJET_INDEXID;
  PJET_INDEXID   = ^JET_INDEXID;

  JET_DBID       = type Cardinal;   { Database Identifier }
  JET_OBJTYP     = type Cardinal;   { Object Type }
  JET_COLTYP     = type Cardinal;   { Column Type }
  JET_GRBIT      = type Cardinal;   { Group of Bits }

  JET_SNP        = type Cardinal;   { Status Notification Process }
  JET_SNT        = type Cardinal;   { Status Notification Type }
  JET_DATESERIAL = type Cardinal;  { JET_coltypDateTime format }
  {$IF JET_VERSION >= $0501}
  JET_CBTYP      = type Cardinal;  { Callback Types }
  {$IFEND}

  JET_PFNSTATUS = function(sesid: JET_SESID;
                           snp  : JET_SNP;
                           snt  : JET_SNT;
                           pv   : Pointer
                           ): JET_ERR;  stdcall;

  JET_PSTR   = PAnsiChar;   { ASCII string (char *) null terminated }
  JET_PCSTR  = PAnsiChar;   { const ASCII string (char *) null terminated }
  JET_PWSTR  = PWideChar;   { Unicode string (char *) null terminated }
  JET_PCWSTR = PWideChar;   { const Unicode string (char *) null terminated }

  JET_RSTMAP_A = record
    szDatabaseName   : PAnsiChar;
    szNewDatabaseName: PAnsiChar;
  end;
  PJET_RSTMAP_A = ^JET_RSTMAP_A;

  JET_RSTMAP_W = record
    szDatabaseName   : PWideChar;
    szNewDatabaseName: PWideChar;
  end;
  PJET_RSTMAP_W = ^JET_RSTMAP_W;

  {$IFDEF JET_UNICODE}
  JET_RSTMAP = JET_RSTMAP_W;
  {$ELSE}
  JET_RSTMAP = JET_RSTMAP_A;
  {$ENDIF}


  tagCONVERT_A = record
    szOldDll: PAnsiChar;
    case Byte of
      0: (fFlags: Cardinal);
      1: (fSchemaChangesOnly: Cardinal);  // one bit only used
  end;
  JET_CONVERT_A = tagCONVERT_A;
  PJET_CONVERT_A = ^JET_CONVERT_A;

 tagCONVERT_W = record
    szOldDll: PWideChar;
    case Byte of
      0: (fFlags: Cardinal);
      1: (fSchemaChangesOnly: Cardinal);  // one bit only used
  end;
  JET_CONVERT_W = tagCONVERT_W;
  PJET_CONVERT_W = ^JET_CONVERT_W;

  {$IFDEF JET_UNICODE}
  JET_CONVERT = JET_CONVERT_W;
  {$ELSE}
  JET_CONVERT = JET_CONVERT_A;
  {$ENDIF}


const
  // Online defragmentation options
  JET_bitDefragmentBatchStart          = $00000001;
  JET_bitDefragmentBatchStop           = $00000002;
  {$IF JET_VERSION >= $0501}
  JET_bitDefragmentAvailSpaceTreesOnly = $00000040; { only defrag AvailExt trees }
  {$IFEND}
  {$IF JET_VERSION >= $0601}
  JET_bitDefragmentNoPartialMerges     = $00000080; { don't do partial merges during OLD }
  JET_bitDefragmentBTree               = $00000100; { defrag one B-Tree (testing only) }
  {$IFEND}

{$IF JET_VERSION >= $0501}   { Callback-function types }
  JET_cbtypNull                    = $00000000;
  JET_cbtypFinalize                = $00000001;  { a finalizable column has gone to zero }
  JET_cbtypBeforeInsert            = $00000002;  { about to insert a record }
  JET_cbtypAfterInsert             = $00000004;  { finished inserting a record }
  JET_cbtypBeforeReplace           = $00000008;  { about to modify a record }
  JET_cbtypAfterReplace            = $00000010;  { finished modifying a record }
  JET_cbtypBeforeDelete            = $00000020;  { about to delete a record }
  JET_cbtypAfterDelete             = $00000040;  { finished deleting the record }
  JET_cbtypUserDefinedDefaultValue = $00000080;  { calculating a user-defined default }
  JET_cbtypOnlineDefragCompleted   = $00000100;  { a call to JetDefragment2 has completed }
  JET_cbtypFreeCursorLS            = $00000200;  { the Local Storage associated with a cursor must be freed }
  JET_cbtypFreeTableLS             = $00000400;  { the Local Storage associated with a table must be freed }

type
  { Callback-function prototype }
  JET_CALLBACK = function(sesid  : JET_SESID;
                          dbid   : JET_DBID;
                          tableid: JET_TABLEID;
                          cbtyp  : JET_CBTYP;
                          pvArg1 : Pointer;
                          pvArg2 : Pointer;
                          pvContext: Pointer;
                          ulUnused : Pointer
                          ): JET_ERR;  stdcall;

{$IFEND}

type
 { Status Notification Structures }
  JET_SNPROG = record      { Status Notification Progress }
    cbStruct  : Cardinal; { Size of this structure }
    cunitDone : Cardinal; { Number of units of work completed }
    cunitTotal: Cardinal; { Total number of units of work }
  end;

  JET_DBINFOUPGRADE = record
    cbStruct: Cardinal;
    cbFilesizeLow: Cardinal;   // file's current size (low DWORD)
    cbFilesizeHigh:  Cardinal;   // file's current size (high DWORD)
    cbFreeSpaceRequiredLow: Cardinal; // estimate of free disk space required for in-place upgrade (low DWORD)
    cbFreeSpaceRequiredHigh: Cardinal;// estimate of free disk space required for in-place upgrade (high DWORD)
    csecToUpgrade: Cardinal;   // estimate of time required, in seconds, for upgrade
    ulFlags: Cardinal;  // bits: 0=fUpgradable, 1=fAlreadyUpgraded
  end;


  JET_OBJECTINFO = record
    cbStruct: Cardinal;
    objtyp  : JET_OBJTYP;
    dtCreate: JET_DATESERIAL; //  Deprecated.
    dtUpdate: JET_DATESERIAL; //  Deprecated.
    grbit   : JET_GRBIT;
    flags   : Cardinal;
    cRecord : Cardinal;
    cPage   : Cardinal;
  end;

const
   { The following flags appear in the grbit field above }
  JET_bitTableInfoUpdatable  = $00000001;
  JET_bitTableInfoBookmark   = $00000002;
  JET_bitTableInfoRollback   = $00000004;

 { The following flags occur in the flags field above }

  JET_bitObjectSystem        = $80000000; // Internal use only
  JET_bitObjectTableFixedDDL = $40000000; // Table's DDL is fixed
  JET_bitObjectTableTemplate = $20000000; // Table's DDL is inheritable (implies FixedDDL)
  JET_bitObjectTableDerived  = $10000000; // Table's DDL is inherited from a template table
  {$IF JET_VERSION >= $0501}
  // used in conjunction with JET_bitObjectTableTemplate to disallow
  // fixed/var columns in derived tables (so that fixed/var columns
  // may be added to the template in the future)
  JET_bitObjectTableNoFixedVarColumnsInDerivedTables = $04000000;
  {$IFEND}

type
  JET_OBJECTLIST = record
    cbStruct: Cardinal;
    tableid: JET_TABLEID;
    cRecord: Cardinal;
    columnidcontainername: JET_COLUMNID;
    columnidobjectname: JET_COLUMNID;
    columnidobjtyp: JET_COLUMNID;
    columniddtCreate: JET_COLUMNID; //  XXX -- to be deleted
    columniddtUpdate: JET_COLUMNID; //  XXX -- to be deleted
    columnidgrbit: JET_COLUMNID;
    columnidflags: JET_COLUMNID;
    columnidcRecord: JET_COLUMNID; { Level 2 info }
    columnidcPage: JET_COLUMNID;  { Level 2 info }
  end;

const
  cObjectInfoCols = 9;
  cColumnInfoCols = 14;
  cIndexInfoCols  = 15;

type
  JET_COLUMNLIST = record
    cbStruct: Cardinal;
    tableid: JET_TABLEID;
    cRecord: Cardinal;
    columnidPresentationOrder: JET_COLUMNID;
    columnidcolumnname: JET_COLUMNID;
    columnidcolumnid: JET_COLUMNID;
    columnidcoltyp: JET_COLUMNID;
    columnidCountry: JET_COLUMNID;
    columnidLangid: JET_COLUMNID;
    columnidCp: JET_COLUMNID;
    columnidCollate: JET_COLUMNID;
    columnidcbMax: JET_COLUMNID;
    columnidgrbit: JET_COLUMNID;
    columnidDefault: JET_COLUMNID;
    columnidBaseTableName: JET_COLUMNID;
    columnidBaseColumnName: JET_COLUMNID;
    columnidDefinitionName: JET_COLUMNID;
  end;


  JET_COLUMNDEF = record
    cbStruct: Cardinal;
    columnid: JET_COLUMNID;
    coltyp: JET_COLTYP;
    wCountry: WORD;
    langid: WORD;
    cp: WORD;
    wCollate: WORD;       { Must be 0 }
    cbMax: Cardinal;
    grbit: JET_GRBIT;
  end;
  PJET_COLUMNDEF = ^JET_COLUMNDEF;
  

  JET_COLUMNBASE_A = record
    cbStruct: Cardinal;
    columnid: JET_COLUMNID;
    coltyp: JET_COLTYP;
    wCountry: WORD;
    langid: WORD;
    cp: WORD;
    wFiller: WORD;       { Must be 0 }
    cbMax: Cardinal;
    grbit: JET_GRBIT;
    szBaseTableName: array[0..256-1] of AnsiChar;
    szBaseColumnName: array[0..256-1] of AnsiChar;
  end;


  JET_COLUMNBASE_W = record
    cbStruct: Cardinal;
    columnid: JET_COLUMNID;
    coltyp: JET_COLTYP;
    wCountry: WORD;
    langid: WORD;
    cp: WORD;
    wFiller: WORD;       { Must be 0 }
    cbMax: Cardinal;
    grbit: JET_GRBIT;
    szBaseTableName: array[0..256-1] of WideChar;
    szBaseColumnName: array[0..256-1] of WideChar;
  end;


  {$IFDEF JET_UNICODE}
  JET_COLUMNBASE = JET_COLUMNBASE_W;
  {$ELSE}
  JET_COLUMNBASE = JET_COLUMNBASE_A;
  {$ENDIF}


  JET_INDEXLIST = record
    cbStruct: Cardinal;
    tableid: JET_TABLEID;
    cRecord: Cardinal;
    columnidindexname: JET_COLUMNID;
    columnidgrbitIndex: JET_COLUMNID;
    columnidcKey: JET_COLUMNID;
    columnidcEntry: JET_COLUMNID;
    columnidcPage: JET_COLUMNID;
    columnidcColumn: JET_COLUMNID;
    columnidiColumn: JET_COLUMNID;
    columnidcolumnid: JET_COLUMNID;
    columnidcoltyp: JET_COLUMNID;
    columnidCountry: JET_COLUMNID;
    columnidLangid: JET_COLUMNID;
    columnidCp: JET_COLUMNID;
    columnidCollate: JET_COLUMNID;
    columnidgrbitColumn: JET_COLUMNID;
    columnidcolumnname: JET_COLUMNID;
    columnidLCMapFlags: JET_COLUMNID;
  end;



 tag_JET_COLUMNCREATE_A = record  
    cbStruct: Cardinal;    // size of this structure (for future expansion)
    szColumnName: PAnsiChar;   // column name
    coltyp: JET_COLTYP;     // column type
    cbMax: Cardinal;     // the maximum length of this column (only relevant for binary and text columns)
    grbit: JET_GRBIT;     // column options
    pvDefault: Pointer;    // default value (NULL if none)
    cbDefault: Cardinal;    // length of default value
    cp: Cardinal;      // code page (for text columns only)
    columnid: JET_COLUMNID;    // returned column id
    err: JET_ERR;     // returned error code
  end;
  JET_COLUMNCREATE_A  = tag_JET_COLUMNCREATE_A;
  PJET_COLUMNCREATE_A = ^JET_COLUMNCREATE_A;

 tag_JET_COLUMNCREATE_W = record
    cbStruct: Cardinal;    // size of this structure (for future expansion)
    szColumnName: PWideChar;   // column name
    coltyp: JET_COLTYP;     // column type
    cbMax: Cardinal;     // the maximum length of this column (only relevant for binary and text columns)
    grbit: JET_GRBIT;     // column options
    pvDefault: Pointer;    // default value (NULL if none)
    cbDefault: Cardinal;    // length of default value
    cp: Cardinal;      // code page (for text columns only)
    columnid: JET_COLUMNID;    // returned column id
    err: JET_ERR;     // returned error code
  end;
  JET_COLUMNCREATE_W  = tag_JET_COLUMNCREATE_W;
  PJET_COLUMNCREATE_W = ^JET_COLUMNCREATE_W;

  {$IFDEF JET_UNICODE}
  JET_COLUMNCREATE = JET_COLUMNCREATE_W;
  {$ELSE}
  JET_COLUMNCREATE = JET_COLUMNCREATE_A;
  {$ENDIF}

{$IF JET_VERSION >= $0501} 
//  This is the information needed to create a column with a user-defined default. It should be passed in using
//  the pvDefault and cbDefault in a JET_COLUMNCREATE structure

 tag_JET_USERDEFINEDDEFAULT_A = record
    szCallback: PAnsiChar;
    pbUserData: PByte;
    cbUserData: Cardinal;
    szDependantColumns: PAnsiChar;
  end;
  JET_USERDEFINEDDEFAULT_A = tag_JET_USERDEFINEDDEFAULT_A;

 tag_JET_USERDEFINEDDEFAULT_W = record
 szCallback: PWideChar;
 pbUserData: PByte;
 cbUserData: Cardinal;
 szDependantColumns: PWideChar;
  end;
  JET_USERDEFINEDDEFAULT_W = tag_JET_USERDEFINEDDEFAULT_W;

  {$IFDEF JET_UNICODE}
  JET_USERDEFINEDDEFAULT = JET_USERDEFINEDDEFAULT_W;
  {$ELSE}
  JET_USERDEFINEDDEFAULT = JET_USERDEFINEDDEFAULT_A;
  {$ENDIF}

{$IFEND}

  tagJET_CONDITIONALCOLUMN_A = record
    cbStruct: Cardinal;    // size of this structure (for future expansion)
    szColumnName: PAnsiChar;   // column that we are conditionally indexed on
    grbit: JET_GRBIT;     // conditional column options
  end;
  JET_CONDITIONALCOLUMN_A  = tagJET_CONDITIONALCOLUMN_A;
  PJET_CONDITIONALCOLUMN_A = ^JET_CONDITIONALCOLUMN_A;

  tagJET_CONDITIONALCOLUMN_W = record
    cbStruct: Cardinal;    // size of this structure (for future expansion)
    szColumnName: PWideChar;   // column that we are conditionally indexed on
    grbit: JET_GRBIT;     // conditional column options
  end;
  JET_CONDITIONALCOLUMN_W  = tagJET_CONDITIONALCOLUMN_W;
  PJET_CONDITIONALCOLUMN_W = ^JET_CONDITIONALCOLUMN_W; 

  {$IFDEF JET_UNICODE}
  JET_CONDITIONALCOLUMN = JET_CONDITIONALCOLUMN_W;
  {$ELSE}
  JET_CONDITIONALCOLUMN = JET_CONDITIONALCOLUMN_A;
  {$ENDIF}

   tagJET_UNICODEINDEX = record
    lcid: Cardinal;
    dwMapFlags: Cardinal;
  end;
  JET_UNICODEINDEX  = tagJET_UNICODEINDEX;
  PJET_UNICODEINDEX = ^JET_UNICODEINDEX;

  {$IF JET_VERSION >= $0502}
  tagJET_TUPLELIMITS = record
    chLengthMin: Cardinal;
    chLengthMax: Cardinal;
    chToIndexMax: Cardinal;
    {$IF JET_VERSION >= $0600}
    cchIncrement: Cardinal;
    ichStart: Cardinal;
    {$IFEND}
  end;
  JET_TUPLELIMITS  = tagJET_TUPLELIMITS;
  PJET_TUPLELIMITS = ^JET_TUPLELIMITS;
  {$IFEND}

  {$IF JET_VERSION >= $0601}
  // This structure describes some of the hints we can give to a given B-tree, be it a
  // table, index, or the internal long values tree.
  tagJET_SPACEHINTS = record
    cbStruct: Cardinal;   // size of this structure
    ulInitialDensity: Cardinal; // density at (append) layout.
    cbInitial: Cardinal;   // initial size (in bytes).

    grbit: JET_GRBIT;    // Combination of one or more flags from
             //  JET_bitSpaceHints* flags
             //  JET_bitCreateHints* flags
             //  JET_bitRetrieveHints* flags
             //  JET_bitUpdateHints* flags
             //  JET_bitDeleteHints* flags
    ulMaintDensity: Cardinal;  // density to maintain at.
    ulGrowth: Cardinal;   // percent growth from:
             //   last growth or initial size (possibly rounded to nearest native JET allocation size).
    cbMinExtent: Cardinal;  // This overrides ulGrowth if too small.
    cbMaxExtent: Cardinal;  // This caps ulGrowth.
  end;
  JET_SPACEHINTS  = tagJET_SPACEHINTS;
  PJET_SPACEHINTS = ^JET_SPACEHINTS;
  {$IFEND}

  tagJET_INDEXCREATE_A = record
    cbStruct: Cardinal;    // size of this structure (for future expansion)
    szIndexName: PAnsiChar;   // index name
    szKey: PAnsiChar;     // index key definition
    cbKey: Cardinal;     //  length, in bytes, of szKey including the two terminating nulls
    grbit: JET_GRBIT;     // index options
    ulDensity: Cardinal;    // index density
    locale: record case Byte of
      0: (lcid: Cardinal);     // lcid for the index (if JET_bitIndexUnicode NOT specified)
      1: (pidxunicode: PJET_UNICODEINDEX);   // pointer to JET_UNICODEINDEX struct (if JET_bitIndexUnicode specified)
    end;
    varcols: record case Byte of
      0: (cbVarSegMac: Cardinal);   // maximum length of variable length columns in index key (if JET_bitIndexTupleLimits specified)
      {$IF JET_VERSION >= $0502}
      1: (ptuplelimits: PJET_TUPLELIMITS);   // pointer to JET_TUPLELIMITS struct (if JET_bitIndexTupleLimits specified)
      {$IFEND}
    end;
    rgconditionalcolumn: PJET_CONDITIONALCOLUMN_A; // pointer to conditional column structure
    cConditionalColumn: Cardinal;  // number of conditional columns
    err: JET_ERR;     // returned error code
    {$IF JET_VERSION >= $0600}
    cbKeyMost: Cardinal;    // size of key preserved in index, e.g. without truncation (if JET_bitIndexKeyMost specified)
    {$IFEND}
  end;
  JET_INDEXCREATE_A  = tagJET_INDEXCREATE_A;
  PJET_INDEXCREATE_A = ^JET_INDEXCREATE_A;

  tagJET_INDEXCREATE_W = record
    cbStruct: Cardinal;    // size of this structure (for future expansion)
    szIndexName: PWideChar;   // index name
    szKey: PWideChar;     // index key definition
    cbKey: Cardinal;     //  length, in bytes, of szKey including the two terminating nulls
    grbit: JET_GRBIT;     // index options
    ulDensity: Cardinal;    // index density
    locale: record case Byte of
      0: (lcid: Cardinal);     // lcid for the index (if JET_bitIndexUnicode NOT specified)
      1: (pidxunicode: PJET_UNICODEINDEX);   // pointer to JET_UNICODEINDEX struct (if JET_bitIndexUnicode specified)
    end;
    varcols: record case Byte of
      0: (cbVarSegMac: Cardinal);   // maximum length of variable length columns in index key (if JET_bitIndexTupleLimits specified)
      {$IF JET_VERSION >= $0502}
      1: (ptuplelimits: PJET_TUPLELIMITS);   // pointer to JET_TUPLELIMITS struct (if JET_bitIndexTupleLimits specified)
      {$IFEND}
    end;
    rgconditionalcolumn: PJET_CONDITIONALCOLUMN_W; // pointer to conditional column structure
    cConditionalColumn: Cardinal;  // number of conditional columns
    err: JET_ERR;     // returned error code
    {$IF JET_VERSION >= $0600}
    cbKeyMost: Cardinal;    // size of key preserved in index, e.g. without truncation (if JET_bitIndexKeyMost specified)
    {$IFEND}
  end;
  JET_INDEXCREATE_W  = tagJET_INDEXCREATE_W;
  PJET_INDEXCREATE_W = ^JET_INDEXCREATE_W;

  {$IFDEF JET_UNICODE}
  JET_INDEXCREATE = JET_INDEXCREATE_W;
  {$ELSE}
  JET_INDEXCREATE = JET_INDEXCREATE_A;
  {$ENDIF}

  {$IF JET_VERSION >= $0601}

  tagJET_INDEXCREATE2_A = record
    cbStruct: Cardinal;    // size of this structure (for future expansion)
    szIndexName: PAnsiChar;   // index name
    szKey: PAnsiChar;     // index key definition
    cbKey: Cardinal;     //  length, in bytes, of szKey including the two terminating nulls
    grbit: JET_GRBIT;     // index options
    ulDensity: Cardinal;    // index density
    locale: record case Byte of
      0: (lcid: Cardinal);     // lcid for the index (if JET_bitIndexUnicode NOT specified)
      1: (pidxunicode: PJET_UNICODEINDEX);   // pointer to JET_UNICODEINDEX struct (if JET_bitIndexUnicode specified)
    end;
    varcols: record case Byte of
      0: (cbVarSegMac: Cardinal);   // maximum length of variable length columns in index key (if JET_bitIndexTupleLimits specified)
      1: (ptuplelimits: PJET_TUPLELIMITS);   // pointer to JET_TUPLELIMITS struct (if JET_bitIndexTupleLimits specified)
    end;
    rgconditionalcolumn: PJET_CONDITIONALCOLUMN_A; // pointer to conditional column structure
    cConditionalColumn: Cardinal;  // number of conditional columns
    err: JET_ERR;     // returned error code
    cbKeyMost: Cardinal;    // size of key preserved in index, e.g. without truncation (if JET_bitIndexKeyMost specified)
    pSpacehints: PJET_SPACEHINTS;   // space allocation, maintenance, and usage hints
  end;
  JET_INDEXCREATE2_A  = tagJET_INDEXCREATE2_A;
  PJET_INDEXCREATE2_A = ^JET_INDEXCREATE2_A;

  tagJET_INDEXCREATE2_W = record
    cbStruct: Cardinal;    // size of this structure (for future expansion)
    szIndexName: PWideChar;   // index name
    szKey: PWideChar;     // index key definition
    cbKey: Cardinal;     //  length, in bytes, of szKey including the two terminating nulls
    grbit: JET_GRBIT;     // index options
    ulDensity: Cardinal;    // index density
    locale: record case Byte of
      0: (lcid: Cardinal);     // lcid for the index (if JET_bitIndexUnicode NOT specified)
      1: (pidxunicode: PJET_UNICODEINDEX);   // pointer to JET_UNICODEINDEX struct (if JET_bitIndexUnicode specified)
    end;
    varcols: record case Byte of
      0: (cbVarSegMac: Cardinal);   // maximum length of variable length columns in index key (if JET_bitIndexTupleLimits specified)
      1: (ptuplelimits: PJET_TUPLELIMITS);   // pointer to JET_TUPLELIMITS struct (if JET_bitIndexTupleLimits specified)
    end;
    rgconditionalcolumn: PJET_CONDITIONALCOLUMN_W; // pointer to conditional column structure
    cConditionalColumn: Cardinal;  // number of conditional columns
    err: JET_ERR;     // returned error code
    cbKeyMost: Cardinal;    // size of key preserved in index, e.g. without truncation (if JET_bitIndexKeyMost specified)
    pSpacehints: PJET_SPACEHINTS;   // space allocation, maintenance, and usage hints
  end;
  JET_INDEXCREATE2_W  = tagJET_INDEXCREATE2_W;
  PJET_INDEXCREATE2_W = ^JET_INDEXCREATE2_W;

  {$IFDEF JET_UNICODE}
  JET_INDEXCREATE2 = JET_INDEXCREATE2_W;
  {$ELSE}
  JET_INDEXCREATE2 = JET_INDEXCREATE2_A;
  {$ENDIF}
  {$IFEND}

  //
  //  Table Creation Structures
  //

  tagJET_TABLECREATE_A = record
  cbStruct: Cardinal;    // size of this structure (for future expansion)
  szTableName: PAnsiChar;   // name of table to create.
  szTemplateTableName: PAnsiChar; // name of table from which to inherit base DDL
  ulPages: Cardinal;    // initial pages to allocate for table.
  ulDensity: Cardinal;    // table density.
  rgcolumncreate: PJET_COLUMNCREATE_A;  // array of column creation info
  cColumns: Cardinal;    // number of columns to create
  rgindexcreate: PJET_INDEXCREATE_A;   // array of index creation info
  cIndexes: Cardinal;    // number of indexes to create
  grbit: JET_GRBIT;
  tableid: JET_TABLEID;    // returned tableid.
  cCreated: Cardinal;    // count of objects created (columns+table+indexes).
  end;
  JET_TABLECREATE_A = tagJET_TABLECREATE_A;

  tagJET_TABLECREATE_W = record
  cbStruct: Cardinal;    // size of this structure (for future expansion)
  szTableName: PWideChar;   // name of table to create.
  szTemplateTableName: PWideChar; // name of table from which to inherit base DDL
  ulPages: Cardinal;    // initial pages to allocate for table.
  ulDensity: Cardinal;    // table density.
  rgcolumncreate: PJET_COLUMNCREATE_W;  // array of column creation info
  cColumns: Cardinal;    // number of columns to create
  rgindexcreate: PJET_INDEXCREATE_W;   // array of index creation info
  cIndexes: Cardinal;    // number of indexes to create
  grbit: JET_GRBIT;
  tableid: JET_TABLEID;    // returned tableid.
  cCreated: Cardinal;    // count of objects created (columns+table+indexes).
  end;
  JET_TABLECREATE_W = tagJET_TABLECREATE_W;

  {$IFDEF JET_UNICODE}
  JET_TABLECREATE = JET_TABLECREATE_W;
  {$ELSE}
  JET_TABLECREATE = JET_TABLECREATE_A;
  {$ENDIF}

  {$IF JET_VERSION >= $0501}
  tagJET_TABLECREATE2_A = record
  cbStruct: Cardinal;    // size of this structure (for future expansion)
  szTableName: PAnsiChar;   // name of table to create.
  szTemplateTableName: PAnsiChar; // name of table from which to inherit base DDL
  ulPages: Cardinal;    // initial pages to allocate for table.
  ulDensity: Cardinal;    // table density.
  rgcolumncreate: PJET_COLUMNCREATE_A;  // array of column creation info
  cColumns: Cardinal;    // number of columns to create
  rgindexcreate: PJET_INDEXCREATE_A;   // array of index creation info
  cIndexes: Cardinal;    // number of indexes to create
  szCallback: PAnsiChar;   // callback to use for this table
  cbtyp: JET_CBTYP;     // when the callback should be called
  grbit: JET_GRBIT;
  tableid: JET_TABLEID;    // returned tableid.
  cCreated: Cardinal;    // count of objects created (columns+table+indexes+callbacks).
  end;
  JET_TABLECREATE2_A = tagJET_TABLECREATE2_A;

  tagJET_TABLECREATE2_W = record
  cbStruct: Cardinal;    // size of this structure (for future expansion)
  szTableName: PWideChar;   // name of table to create.
  szTemplateTableName: PWideChar; // name of table from which to inherit base DDL
  ulPages: Cardinal;    // initial pages to allocate for table.
  ulDensity: Cardinal;    // table density.
  rgcolumncreate: PJET_COLUMNCREATE_W;  // array of column creation info
  cColumns: Cardinal;    // number of columns to create
  rgindexcreate: PJET_INDEXCREATE_W;   // array of index creation info
  cIndexes: Cardinal;    // number of indexes to create
  szCallback: PWideChar;   // callback to use for this table
  cbtyp: JET_CBTYP;     // when the callback should be called
  grbit: JET_GRBIT;
  tableid: JET_TABLEID;    // returned tableid.
  cCreated: Cardinal;    // count of objects created (columns+table+indexes+callbacks).
  end;
  JET_TABLECREATE2_W = tagJET_TABLECREATE2_W;

  {$IFDEF JET_UNICODE}
  JET_TABLECREATE2 = JET_TABLECREATE2_W;
  {$ELSE}
  JET_TABLECREATE2 = JET_TABLECREATE2_A;
  {$ENDIF}

  {$IFEND}


  {$IF JET_VERSION >= $0601}
  tagJET_TABLECREATE3_A = record
    cbStruct: Cardinal;    // size of this structure (for future expansion)
    szTableName: PAnsiChar;   // name of table to create.
    szTemplateTableName: PAnsiChar; // name of table from which to inherit base DDL
    ulPages: Cardinal;    // initial pages to allocate for table.
    ulDensity: Cardinal;    // table density.
    rgcolumncreate: PJET_COLUMNCREATE_A;  // array of column creation info
    cColumns: Cardinal;    // number of columns to create
    rgindexcreate: PJET_INDEXCREATE2_A;   // array of index creation info
    cIndexes: Cardinal;    // number of indexes to create
    szCallback: PAnsiChar;   // callback to use for this table
    cbtyp: JET_CBTYP;     // when the callback should be called
    grbit: JET_GRBIT;
    pSeqSpacehints: PJET_SPACEHINTS;   // space allocation, maintenance, and usage hints for default sequential index
    pLVSpacehints: PJET_SPACEHINTS;   // space allocation, maintenance, and usage hints for Separated LV tree.
    cbSeparateLV: Cardinal;   // heuristic size to separate a intrinsic LV from the primary record

    tableid: JET_TABLEID;    // returned tableid.
    cCreated: Cardinal;    // count of objects created (columns+table+indexes+callbacks).
  end;
  JET_TABLECREATE3_A = tagJET_TABLECREATE3_A;

  tagJET_TABLECREATE3_W = record
    cbStruct: Cardinal;    // size of this structure (for future expansion)
    szTableName: PWideChar;   // name of table to create.
    szTemplateTableName: PWideChar; // name of table from which to inherit base DDL
    ulPages: Cardinal;    // initial pages to allocate for table.
    ulDensity: Cardinal;    // table density.
    rgcolumncreate: PJET_COLUMNCREATE_W;  // array of column creation info
    cColumns: Cardinal;    // number of columns to create
    rgindexcreate: PJET_INDEXCREATE2_W;   // array of index creation info
    cIndexes: Cardinal;    // number of indexes to create
    szCallback: PWideChar;   // callback to use for this table
    cbtyp: JET_CBTYP;     // when the callback should be called
    grbit: JET_GRBIT;
    pSeqSpacehints: PJET_SPACEHINTS;   // space allocation, maintenance, and usage hints for default sequential index
    pLVSpacehints: PJET_SPACEHINTS;   // space allocation, maintenance, and usage hints for Separated LV tree.
    cbSeparateLV: Cardinal;   // heuristic size to separate a intrinsic LV from the primary record
    tableid: JET_TABLEID;    // returned tableid.
    cCreated: Cardinal;    // count of objects created (columns+table+indexes+callbacks).
  end;
  JET_TABLECREATE3_W = tagJET_TABLECREATE3_W;

  {$IFDEF JET_UNICODE}
  JET_TABLECREATE3 = JET_TABLECREATE3_W;
  {$ELSE}
  JET_TABLECREATE3 = JET_TABLECREATE3_A;
  {$ENDIF}

  {$IFEND}

  {$IF JET_VERSION >= $0600}
  tagJET_OPENTEMPORARYTABLE = record
    cbStruct: Cardinal;    // size of this structure (for future expansion)
    prgcolumndef: PJET_COLUMNDEF;
    ccolumn: Cardinal;
    pidxunicode :PJET_UNICODEINDEX;
    grbit: JET_GRBIT;
    prgcolumnid: PJET_COLUMNID;
    cbKeyMost: Cardinal;
    cbVarSegMac: Cardinal;
    tableid: JET_TABLEID;
  end;
  JET_OPENTEMPORARYTABLE = tagJET_OPENTEMPORARYTABLE;
  {$IFEND}

  JET_RETINFO = record
    cbStruct: Cardinal;
    ibLongValue: Cardinal;
    itagSequence: Cardinal;
    columnidNextTagged: JET_COLUMNID;
  end;
  PJET_RETINFO = ^JET_RETINFO;

  JET_SETINFO = record
    cbStruct: Cardinal;
    ibLongValue: Cardinal;
    itagSequence: Cardinal;
  end;
  PJET_SETINFO = ^JET_SETINFO;

  JET_RECPOS = record
    cbStruct: Cardinal;
    centriesLT: Cardinal;
    centriesInRange: Cardinal;
    centriesTotal: Cardinal;
  end;

  JET_RECORDLIST = record
    cbStruct: Cardinal;
    tableid: JET_TABLEID;
    cRecord: Cardinal;
    columnidBookmark: JET_COLUMNID;
  end;

  JET_INDEXRANGE = record
    cbStruct: Cardinal;
    tableid: JET_TABLEID;
    grbit: JET_GRBIT;
  end;
  PJET_INDEXRANGE = ^JET_INDEXRANGE;

const
  JET_MAX_COMPUTERNAME_LENGTH = 15;

type
  JET_LOGTIME = packed record
    bSeconds: Byte;   // 0 - 59
    bMinutes: Byte;   // 0 - 59
    bHours  : Byte;   // 0 - 23
    bDay    : Byte;   // 1 - 31
    bMonth  : Byte;   // 1 - 12
    bYear   : Byte;   // current year - 1900
    flags1  : Byte;   // Bits: 0=fTimeIsUTC, rest unused
    bFiller2: Byte;
  end;

  {$IF JET_VERSION >= $0600}
  // the JET_BKLOGTIME is an extention of JET_LOGTIME to be used
  // in the JET_BKINFO structure. They should have the same size for
  // compatibility reasons
  JET_BKLOGTIME = packed record
    bSeconds: Byte;   // 0 - 59
    bMinutes: Byte;   // 0 - 59
    bHours  : Byte;   // 0 - 23
    bDay    : Byte;   // 1 - 31
    bMonth  : Byte;   // 1 - 12
    bYear   : Byte;   // current year - 1900
    flags1  : Byte;   // Bits: 0=fTimeIsUTC, rest unused
    flags2  : Byte;   // Bits: 0=fOSSnapshot, rest reserved
  end;
  {$IFEND}

  JET_LGPOS = packed record     // be casted to TIME
    ib: WORD;    // must be the last so that lgpos can
    isec: WORD;   // index of disksec starting logsec
    lGeneration: long; // generation of logsec
  end;

  JET_SIGNATURE = packed record
    ulRandom: Cardinal;   // a random number
    logtimeCreate: JET_LOGTIME;  // time db created, in logtime format
    szComputerName: array[0..JET_MAX_COMPUTERNAME_LENGTH] of AnsiChar; // where db is created
  end;

  JET_BKINFO = packed record
    lgposMark: JET_LGPOS;   // id for this backup
    logtimemark: record case Byte of
      0: ( logtimeMark: JET_LOGTIME  );
      {$IF JET_VERSION >= $0600}
      1: ( bklogtimeMark: JET_BKLOGTIME );
      {$IFEND}
    end;
    genLow: Cardinal;
    genHigh: Cardinal;
  end;

  JET_DBINFOMISC = record
    ulVersion: Cardinal;  // version of DAE the db created (see ulDAEVersion)
    ulUpdate: Cardinal;   // used to track incremental database format updates that are backward-compatible (see ulDAEUpdate)
    signDb: JET_SIGNATURE;   // (28 bytes) signature of the db (incl. creation time).
    dbstate: Cardinal;  // consistent/inconsistent state
    lgposConsistent: JET_LGPOS; // null if in inconsistent state
    logtimeConsistent: JET_LOGTIME; // null if in inconsistent state
    logtimeAttach: JET_LOGTIME; // Last attach time.
    lgposAttach: JET_LGPOS;
    logtimeDetach: JET_LOGTIME; // Last detach time.
    lgposDetach: JET_LGPOS;
    signLog: JET_SIGNATURE;  // (28 bytes) log signature for this attachments
    bkinfoFullPrev: JET_BKINFO; // Last successful full backup.
    bkinfoIncPrev: JET_BKINFO; // Last successful Incremental backup. Reset when bkinfoFullPrev is set
    bkinfoFullCur: JET_BKINFO; // current backup. Succeed if a corresponding pat file generated.
    fShadowingDisabled: Cardinal;
    fUpgradeDb: Cardinal;
    // NT version information. This is needed to decide if an index need
    // be recreated due to sort table changes.
    dwMajorVersion: Cardinal;  { OS version info        }
    dwMinorVersion: Cardinal;
    dwBuildNumber: Cardinal;
    lSPNumber: long;
    cbPageSize: Cardinal;   // database page size (0 = 4k pages)
  end;

  {$IF JET_VERSION >= $0600}
  JET_DBINFOMISC2 = record
    ulVersion: Cardinal;  // version of DAE the db created (see ulDAEVersion)
    ulUpdate: Cardinal;   // used to track incremental database format updates that are backward-compatible (see ulDAEUpdate)
    signDb: JET_SIGNATURE;   // (28 bytes) signature of the db (incl. creation time).
    dbstate: Cardinal;  // consistent/inconsistent state
    lgposConsistent: JET_LGPOS; // null if in inconsistent state
    logtimeConsistent: JET_LOGTIME; // null if in inconsistent state
    logtimeAttach: JET_LOGTIME; // Last attach time.
    lgposAttach: JET_LGPOS;
    logtimeDetach: JET_LOGTIME; // Last detach time.
    lgposDetach: JET_LGPOS;
    signLog: JET_SIGNATURE;  // (28 bytes) log signature for this attachments
    bkinfoFullPrev: JET_BKINFO; // Last successful full backup.
    bkinfoIncPrev: JET_BKINFO; // Last successful Incremental backup. Reset when bkinfoFullPrev is set
    bkinfoFullCur: JET_BKINFO; // current backup. Succeed if a corresponding pat file generated.
    fShadowingDisabled: Cardinal;
    fUpgradeDb: Cardinal;

    // NT version information. This is needed to decide if an index need
    // be recreated due to sort table changes.
    dwMajorVersion: Cardinal;  { OS version info        }
    dwMinorVersion: Cardinal;
    dwBuildNumber: Cardinal;
    lSPNumber: long;
    cbPageSize: Cardinal;   // database page size (0 = 4k pages)

    // new fields added on top of the above JET_DBINFOMISC
    genMinRequired: Cardinal;   // the minimum log generation required for replaying the logs. Typically the checkpoint generation
    genMaxRequired: Cardinal;   // the maximum log generation required for replaying the logs.
    logtimeGenMaxCreate: JET_LOGTIME; // creation time of the genMax log file

    ulRepairCount: Cardinal;   // number of times repair has been called on this database
    logtimeRepair: JET_LOGTIME;   // the date of the last time that repair was run
    ulRepairCountOld: Cardinal;  // number of times ErrREPAIRAttachForRepair has been called on this database before the last defrag

    ulECCFixSuccess: Cardinal;  // number of times a one bit error was fixed and resulted in a good page
    logtimeECCFixSuccess: JET_LOGTIME; // the date of the last time that a one bit error was fixed and resulted in a good page
    ulECCFixSuccessOld: Cardinal;  // number of times a one bit error was fixed and resulted in a good page before last repair

    ulECCFixFail: Cardinal;   // number of times a one bit error was fixed and resulted in a bad page
    logtimeECCFixFail: JET_LOGTIME;     // the date of the last time that a one bit error was fixed and resulted in a bad page
    ulECCFixFailOld: Cardinal;  // number of times a one bit error was fixed and resulted in a bad page before last repair

    ulBadChecksum: Cardinal;   // number of times a non-correctable ECC/checksum error was found
    logtimeBadChecksum: JET_LOGTIME;     // the date of the last time that a non-correctable ECC/checksum error was found
    ulBadChecksumOld: Cardinal;  // number of times a non-correctable ECC/checksum error was found before last repair
  end;
  {$IFEND}

  {$IF JET_VERSION >= $0601}
  JET_DBINFOMISC3 = record
    ulVersion: Cardinal;  // version of DAE the db created (see ulDAEVersion)
    ulUpdate: Cardinal;   // used to track incremental database format updates that are backward-compatible (see ulDAEUpdate)
    signDb: JET_SIGNATURE;   // (28 bytes) signature of the db (incl. creation time).
    dbstate: Cardinal;  // consistent/inconsistent state
    lgposConsistent: JET_LGPOS; // null if in inconsistent state
    logtimeConsistent: JET_LOGTIME; // null if in inconsistent state
    logtimeAttach: JET_LOGTIME; // Last attach time.
    lgposAttach: JET_LGPOS;
    logtimeDetach: JET_LOGTIME; // Last detach time.
    lgposDetach: JET_LGPOS;
    signLog: JET_SIGNATURE;  // (28 bytes) log signature for this attachments
    bkinfoFullPrev: JET_BKINFO; // Last successful full backup.
    bkinfoIncPrev: JET_BKINFO; // Last successful Incremental backup. Reset when bkinfoFullPrev is set
    bkinfoFullCur: JET_BKINFO; // current backup. Succeed if a corresponding pat file generated.
    fShadowingDisabled: Cardinal;
    fUpgradeDb: Cardinal;

    // NT version information. This is needed to decide if an index need
    // be recreated due to sort table changes.
    dwMajorVersion: Cardinal;  { OS version info        }
    dwMinorVersion: Cardinal;
    dwBuildNumber: Cardinal;
    lSPNumber: long;
    cbPageSize: Cardinal;   // database page size (0 = 4k pages)

    // new fields added on top of the above JET_DBINFOMISC
    genMinRequired: Cardinal;   // the minimum log generation required for replaying the logs. Typically the checkpoint generation
    genMaxRequired: Cardinal;   // the maximum log generation required for replaying the logs.
    logtimeGenMaxCreate: JET_LOGTIME; // creation time of the genMax log file

    ulRepairCount: Cardinal;   // number of times repair has been called on this database
    logtimeRepair: JET_LOGTIME;   // the date of the last time that repair was run
    ulRepairCountOld: Cardinal;  // number of times ErrREPAIRAttachForRepair has been called on this database before the last defrag

    ulECCFixSuccess: Cardinal;  // number of times a one bit error was fixed and resulted in a good page
    logtimeECCFixSuccess: JET_LOGTIME; // the date of the last time that a one bit error was fixed and resulted in a good page
    ulECCFixSuccessOld: Cardinal;  // number of times a one bit error was fixed and resulted in a good page before last repair

    ulECCFixFail: Cardinal;   // number of times a one bit error was fixed and resulted in a bad page
    logtimeECCFixFail: JET_LOGTIME;     // the date of the last time that a one bit error was fixed and resulted in a bad page
    ulECCFixFailOld: Cardinal;  // number of times a one bit error was fixed and resulted in a bad page before last repair

    ulBadChecksum: Cardinal;   // number of times a non-correctable ECC/checksum error was found
    logtimeBadChecksum: JET_LOGTIME;     // the date of the last time that a non-correctable ECC/checksum error was found
    ulBadChecksumOld: Cardinal;  // number of times a non-correctable ECC/checksum error was found before last repair

    // new fields added on top of the above JET_DBINFOMISC2
    genCommitted: Cardinal;   // the maximum log generation committed to the database. Typically the current log generation
  end;


  JET_DBINFOMISC4 = record
    ulVersion: Cardinal;  // version of DAE the db created (see ulDAEVersion)
    ulUpdate: Cardinal;   // used to track incremental database format updates that are backward-compatible (see ulDAEUpdate)
    signDb: JET_SIGNATURE;   // (28 bytes) signature of the db (incl. creation time).
    dbstate: Cardinal;  // consistent/inconsistent state
    lgposConsistent: JET_LGPOS; // null if in inconsistent state
    logtimeConsistent: JET_LOGTIME; // null if in inconsistent state
    logtimeAttach: JET_LOGTIME; // Last attach time.
    lgposAttach: JET_LGPOS;
    logtimeDetach: JET_LOGTIME; // Last detach time.
    lgposDetach: JET_LGPOS;
    signLog: JET_SIGNATURE;  // (28 bytes) log signature for this attachments
    bkinfoFullPrev: JET_BKINFO; // Last successful full backup.
    bkinfoIncPrev: JET_BKINFO; // Last successful Incremental backup. Reset when bkinfoFullPrev is set
    bkinfoFullCur: JET_BKINFO; // current backup. Succeed if a corresponding pat file generated.
    fShadowingDisabled: Cardinal;
    fUpgradeDb: Cardinal;

    // NT version information. This is needed to decide if an index need
    // be recreated due to sort table changes.
    dwMajorVersion: Cardinal;  { OS version info        }
    dwMinorVersion: Cardinal;
    dwBuildNumber: Cardinal;
    lSPNumber: long;
    cbPageSize: Cardinal;   // database page size (0 = 4k pages)

    // new fields added on top of the above JET_DBINFOMISC
    genMinRequired: Cardinal;   // the minimum log generation required for replaying the logs. Typically the checkpoint generation
    genMaxRequired: Cardinal;   // the maximum log generation required for replaying the logs.
    logtimeGenMaxCreate: JET_LOGTIME;   // creation time of the genMax log file

    ulRepairCount: Cardinal;   // number of times repair has been called on this database
    logtimeRepair: JET_LOGTIME;   // the date of the last time that repair was run
    ulRepairCountOld: Cardinal;  // number of times ErrREPAIRAttachForRepair has been called on this database before the last defrag

    ulECCFixSuccess: Cardinal;  // number of times a one bit error was fixed and resulted in a good page
    logtimeECCFixSuccess: JET_LOGTIME; // the date of the last time that a one bit error was fixed and resulted in a good page
    ulECCFixSuccessOld: Cardinal;  // number of times a one bit error was fixed and resulted in a good page before last repair

    ulECCFixFail: Cardinal;   // number of times a one bit error was fixed and resulted in a bad page
    logtimeECCFixFail: JET_LOGTIME;     // the date of the last time that a one bit error was fixed and resulted in a bad page
    ulECCFixFailOld: Cardinal;  // number of times a one bit error was fixed and resulted in a bad page before last repair

    ulBadChecksum: Cardinal;   // number of times a non-correctable ECC/checksum error was found
    logtimeBadChecksum: JET_LOGTIME;     // the date of the last time that a non-correctable ECC/checksum error was found
    ulBadChecksumOld: Cardinal;  // number of times a non-correctable ECC/checksum error was found before last repair

    // new fields added on top of the above JET_DBINFOMISC2
    genCommitted: Cardinal;   // the maximum log generation committed to the database. Typically the current log generation

    // new fields added on top of the above JET_DBINFOMISC3
    bkinfoCopyPrev: JET_BKINFO;   // Last successful Copy backup
    bkinfoDiffPrev: JET_BKINFO;   // Last successful Differential backup, reset when bkinfoFullPrev is set
  end;
  {$IFEND}

  {$IF JET_VERSION >= $0600}
  //  JET performance counters accumulated by thread
  //
  JET_THREADSTATS = record
    cbStruct: Cardinal;   //  size of this struct
    cPageReferenced: Cardinal; //  pages referenced
    cPageRead : Cardinal;  //  pages read from disk
    cPagePreread: Cardinal;  //  pages preread from disk
    cPageDirtied: Cardinal;  //  clean pages modified
    cPageRedirtied: Cardinal;  //  dirty pages modified
    cLogRecord : Cardinal;  //  log records generated
    cbLogRecord: Cardinal;  //  log record bytes generated
  end;

  JET_RSTINFO_A = record
    cbStruct: Cardinal;
    rgrstmap: PJET_RSTMAP_A;
    crstmap: long;
    lgposStop: JET_LGPOS;
    logtimeStop: JET_LOGTIME;
    pfnStatus: JET_PFNSTATUS;
  end;
  PJET_RSTINFO_A = ^JET_RSTINFO_A;

  JET_RSTINFO_W = record
    cbStruct: Cardinal;
    rgrstmap: PJET_RSTMAP_W;
    crstmap: long;
    lgposStop: JET_LGPOS;
    logtimeStop: JET_LOGTIME;
    pfnStatus: JET_PFNSTATUS;
  end;
  PJET_RSTINFO_W = ^JET_RSTINFO_W;

  {$IFDEF JET_UNICODE}
  JET_RSTINFO = JET_RSTINFO_W;
  {$ELSE}
  JET_RSTINFO = JET_RSTINFO_A;
  {$ENDIF}

  {$IFEND}


const
  {**********************************************************************}
  {**************************** JET CONSTANTS ***************************}
  {**********************************************************************}

  {$IF JET_VERSION >= $0501}
  JET_instanceNil  = JET_INSTANCE( not 0);
  {$IFEND}
  JET_sesidNil     = JET_SESID( not 0);
  JET_tableidNil   = JET_TABLEID(not 0);
  JET_bitNil       = JET_GRBIT(0);

  { Max length of a object/column/index/property name }

  {$IFNDEF JET_UNICODE}
  JET_cbNameMost    = 64;
  {$ELSE}
  JET_cbNameMost    = 128;
  {$ENDIF}

  { Max length of a "name.name.name..." construct }

  {$IFNDEF JET_UNICODE}
  JET_cbFullNameMost   = 255;
  {$ELSE}
  JET_cbFullNameMost   = 510;
  {$ENDIF}

  { Max size of long-value (LongBinary or LongText) column chunk }

  // const JET_cbColumnLVChunkMost  ( JET_cbPage - 82 ) to the following:
  // Get cbPage from GetSystemParameter.
  //  changed JET_cbColumnLVChunkMost reference to cbPage - JET_cbColumnLVPageOverhead

  JET_cbColumnLVPageOverhead  = 82;  // ONLY for small (<=8kiB) page, otherwise, query JET_paramLVChunkSizeMost


  { Max size of long-value (LongBinary or LongText) column default value }

  JET_cbLVDefaultValueMost = 255;

  { Max size of non-long-value column data }

  JET_cbColumnMost   = 255;

  { Max size of a sort/index key }

  {$IF JET_VERSION >= $0600}
  JET_cbKeyMost8KBytePage  = 2000;
  JET_cbKeyMost4KBytePage  = 1000;
  JET_cbKeyMost2KBytePage  = 500;
  JET_cbKeyMostMin   = 255;
  {$IFEND}

  {$IF JET_VERSION >= $0601}
  JET_cbKeyMost32KBytePage =  JET_cbKeyMost8KBytePage;
  JET_cbKeyMost16KBytePage = JET_cbKeyMost8KBytePage;
  JET_cbKeyMostMost    = JET_cbKeyMost32KBytePage;
  {$IFEND}

  JET_cbKeyMost    = 255;  // defunct constant retained for backward compatibility
  JET_cbLimitKeyMost   = 256;  // defunct constant retained for backward compatibility
  JET_cbPrimaryKeyMost  = 255;  // defunct constant retained for backward compatibility
  JET_cbSecondaryKeyMost  = 255;  // defunct constant retained for backward compatibility


  { Max size of a bookmark }

  JET_cbBookmarkMost   = 256;
  {$IF JET_VERSION >= $0601}
  JET_cbBookmarkMostMost  = JET_cbKeyMostMost;
  {$IFEND}

  { Max number of components in a sort/index key }

  {$IF JET_VERSION >= $0600}
  JET_ccolKeyMost    = 16;
  {$ELSE}
  JET_ccolKeyMost   = 12;
  {$IFEND}

  // maximum number of columns
  {$IF JET_VERSION >= $0501}
  JET_ccolMost   = $0000fee0;
  {$ELSE}
  JET_ccolMost   = $00007ffe;
  {$IFEND}
  JET_ccolFixedMost  = $0000007f;
  JET_ccolVarMost   = $00000080;
  JET_ccolTaggedMost   = ( JET_ccolMost - $000000ff );

  {$IF JET_VERSION >= $0501}
  JET_EventLoggingDisable  = 0;
  {$IF JET_VERSION >= $0601}
  JET_EventLoggingLevelMin = 1;
  JET_EventLoggingLevelLow = 25;
  JET_EventLoggingLevelMedium = 50;
  JET_EventLoggingLevelHigh = 75;
  {$IFEND}
  JET_EventLoggingLevelMax = 100;
  {$IFEND}

  // system paramters
  //
  //  NOTE:  the default values of these parameters used to be documented here.
  //  this can no longer be done because we now support multiple sets of default
  //  values as set by JET_paramConfiguration
  //
  // location parameters
  //
  JET_paramSystemPath      = 0; // path to check point file
  JET_paramTempPath      = 1; // path to the temporary database
  JET_paramLogFilePath      = 2; // path to the log file directory
  JET_paramBaseName      = 3; // base name for all DBMS object names
  JET_paramEventSource     = 4; // language independent process descriptor string

  // performance parameters
  //
  JET_paramMaxSessions     = 5; // maximum number of sessions
  JET_paramMaxOpenTables      = 6; // maximum number of open directories
             //   need 1 for each open table index,
             //   plus 1 for each open table with no indexes,
             //   plus 1 for each table with long column data,
             //   plus a few more.
             //  for 4.1, 1/3 for regular table, 2/3 for index
  JET_paramPreferredMaxOpenTables   = 7; // preferred maximum number of open directories
  {$IF JET_VERSION >= $0600}
  JET_paramCachedClosedTables    = 125; // number of closed tables to cache the meta-data for
  {$IFEND}
  JET_paramMaxCursors      = 8; // maximum number of open cursors
  JET_paramMaxVerPages     = 9; // maximum version store size in version pages
  JET_paramPreferredVerPages    = 63; // preferred version store size in version pages
  {$IF JET_VERSION >= $0501}
  JET_paramGlobalMinVerPages    = 81; // minimum version store size for all instances in version pages
  JET_paramVersionStoreTaskQueueMax  = 105; // maximum number of tasks in the task queue before start dropping the tasks
  {$IFEND}
  JET_paramMaxTemporaryTables    = 10; // maximum concurrent open temporary table/index creation
  JET_paramLogFileSize     = 11; // log file size in kBytes
  JET_paramLogBuffers      = 12; // log buffers in 512 byte units.
  JET_paramWaitLogFlush     = 13; // log flush wait time in milliseconds
  JET_paramLogCheckpointPeriod   = 14; // checkpoint period in sectors
  JET_paramLogWaitingUserMax    = 15; // maximum sessions waiting log flush
  JET_paramCommitDefault     = 16; // default grbit for JetCommitTransaction
  JET_paramCircularLog     = 17; // boolean flag for circular logging
  JET_paramDbExtensionSize    = 18; // database extension size in pages
  JET_paramPageTempDBMin     = 19;  // minimum size temporary database in pages
  JET_paramPageFragment     = 20; // maximum disk extent considered fragment in pages
  {$IF JET_VERSION >= $0600}
  JET_paramEnableFileCache    = 126; //  enable the use of the OS file cache for all managed files
  JET_paramVerPageSize     = 128; //  the version store page size
  JET_paramConfiguration     = 129; //  RESETs all parameters to their default for a given configuration
  JET_paramEnableAdvanced     = 130; //  enables the modification of advanced settings
  JET_paramMaxColtyp      = 131; // maximum coltyp supported by this version of ESE
  {$IFEND}

  //  cache performance parameters
  //
  JET_paramBatchIOBufferMax    = 22; // maximum batch I/O buffers in pages
  JET_paramCacheSize      = 41; // current cache size in pages
  JET_paramCacheSizeMin     = 60; // minimum cache size in pages
  JET_paramCacheSizeMax     = 23; // maximum cache size in pages
  JET_paramCheckpointDepthMax    = 24; // maximum checkpoint depth in bytes
  JET_paramLRUKCorrInterval    = 25;  // time (usec) under which page accesses are correlated
  JET_paramLRUKHistoryMax     = 26;  // maximum LRUK history records
  JET_paramLRUKPolicy      = 27;  // K-ness of LRUK page eviction algorithm (1...2)
  JET_paramLRUKTimeout     = 28;  // time (sec) after which cached pages are always evictable
  JET_paramLRUKTrxCorrInterval   = 29;  // Not Used: time (usec) under which page accesses by the same transaction are correlated
  JET_paramOutstandingIOMax    = 30; // maximum outstanding I/Os
  JET_paramStartFlushThreshold   = 31; // evictable pages at which to start a flush (proportional to CacheSizeMax)
  JET_paramStopFlushThreshold    = 32; // evictable pages at which to stop a flush (proportional to CacheSizeMax)
  {$IF JET_VERSION >= $0600}
  JET_paramEnableViewCache    = 127; //  enable the use of memory mapped file I/O for database files
  JET_paramCheckpointIOMax    = 135; //  maxiumum number of pending flush writes
  {$IFEND}

  {$IF JET_VERSION >= $0600}
  // TableClass names
  JET_paramTableClass1Name    = 137;  // name of tableclass1
  JET_paramTableClass2Name    = 138;  // name of tableclass2
  JET_paramTableClass3Name    = 139;  // name of tableclass3
  JET_paramTableClass4Name    = 140;  // name of tableclass4
  JET_paramTableClass5Name    = 141;  // name of tableclass5
  JET_paramTableClass6Name    = 142;  // name of tableclass6
  JET_paramTableClass7Name    = 143;  // name of tableclass7
  JET_paramTableClass8Name    = 144;  // name of tableclass8
  JET_paramTableClass9Name    = 145;  // name of tableclass9
  JET_paramTableClass10Name    = 146;  // name of tableclass10
  JET_paramTableClass11Name    = 147;  // name of tableclass11
  JET_paramTableClass12Name    = 148;  // name of tableclass12
  JET_paramTableClass13Name    = 149;  // name of tableclass13
  JET_paramTableClass14Name    = 150;  // name of tableclass14
  JET_paramTableClass15Name    = 151;  // name of tableclass15
  {$IFEND}
  {$IF JET_VERSION >= $0600}
  // Values for JET_paramIOPriority
  JET_IOPriorityNormal                    = 0;       // default
  JET_IOPriorityLow                       = 1;
  JET_paramIOPriority               = 152;     // adjust IO priority per instance, anytime. Mainly for background recovery
                                               //  Doesn't affect pending IOs, just subsequent ones
  {$IFEND}

  // debug only parameters
  //
  JET_paramRecovery      = 34; // enable recovery
  JET_paramEnableOnlineDefrag    = 35; // enable online defrag

  // Application specific parameter
  //
  JET_paramCheckFormatWhenOpenFail  = 44; // JetInit may return JET_errDatabaseXXXformat instead of database corrupt when it is set
  JET_paramEnableTempTableVersioning  = 46; // Enable versioning of temp tables
  JET_paramIgnoreLogVersion    = 47; // Do not check the log version
  JET_paramDeleteOldLogs     = 48; // Delete the log files if the version is old, after deleting may make database non-recoverable
  JET_paramEventSourceKey     = 49; // Event source registration key value
  JET_paramNoInformationEvent    = 50; // Disable logging information event
  {$IF JET_VERSION >= $0501}
  JET_paramEventLoggingLevel    = 51; // Set the type of information that goes to event log
  JET_paramDeleteOutOfRangeLogs   = 52; // Delete the log files that are not matching (generation wise) during soft recovery
  JET_paramAccessDeniedRetryPeriod  = 53; // Number of milliseconds to retry when about to fail with AccessDenied
  {$IFEND}

  // Index-checking parameters
  //
  // Different versions of windows normalize unicode text in different ways. That means indexes built under one version of Windows may
  // not work on other versions. Windows Server 2003 Beta 3 introduced GetNLSVersion() which can be used to determine the version of unicode normalization
  // that the OS currently provides. Indexes built in server 2003 are flagged with the version of unicode normalization that they were
  // built with (older indexes have no version information). Most unicode normalization changes consist of adding new characters -- codepoints
  // which were previously undefined are defined and normalize differently. Thus, if binary data is stored in a unicode column it will normalize
  // differently as new codepoints are defined.
  //
  // As of Windows Server 2003 RC1 ESENT tracks unicode index entries that contain undefined codepoints. These can be used to fixup an index when the
  // set of defined unicode characters changes.
  //
  // These parameters control what happens when ESENT attaches to a database that was last used under a different build of the OS (the OS version
  // is stamped in the database header).
  //
  // If JET_paramEnableIndexChecking is TRUE JetAttachDatabase() will delete indexes if JET_bitDbDeleteCorruptIndexes or return an error if
  // the grbit was not specified and there are indexes which need deletion. If it is set to FALSE then JetAttachDatabase() will succeed, even
  // if there are potentially corrupt indexes.
  //
  // If JET_paramEnableIndexCleanup is set, the internal fixup table will be used to fixup index entries. This may not fixup all index corruptions
  // but will be transparent to the application.
  //

  JET_paramEnableIndexChecking   = 45;  // Enable checking OS version for indexes
  {$IF JET_VERSION >= $0502}
  JET_paramEnableIndexCleanup    = 54;  // Enable cleanup of out-of-date index entries
  {$IFEND}

  //            60 // JET_paramCacheSizeMin defined above
  //            63 // JET_paramPreferredVerPages defined above
  JET_paramDatabasePageSize    = 64; // set database page size
  {$IF JET_VERSION >= $0501}
  JET_paramDisableCallbacks    = 65; // turn off callback resolution (for defrag/repair)
  {$IFEND}
  {$IF JET_VERSION >= $0600}
  JET_paramDisablePerfmon     = 107; //  disable perfmon support for this process
  {$IFEND}
  {$IF JET_VERSION >= $0501}
  {$IF JET_VERSION >= $0600}
  JET_paramEnablePersistedCallbacks  = 156;  //  allow the database engine to resolve and use callbacks persisted in a database
  {$IFEND}

  //  Backup performance parameters
  //
  JET_paramBackupChunkSize    = 66;  // backup read size in pages
  JET_paramBackupOutstandingReads   = 67; // backup maximum reads outstanding

  JET_paramSLVProviderEnable    = 68;  // Enable SLV Provider
  // begin_PubEsent
  JET_paramLogFileCreateAsynch   = 69; // prepares next log file while logging to the current one to smooth response time
  {$IFEND}
  JET_paramErrorToString     = 70;  // turns a JET_err into a string (taken from the comment in jet.h)
  {$IF JET_VERSION >= $0501}
  JET_paramZeroDatabaseDuringBackup  = 71; // Overwrite deleted records/LVs during backup
  {$IFEND}
  JET_paramUnicodeIndexDefault   = 72; // default LCMapString() lcid and flags to use for CreateIndex() and unique multi-values check
             //  (pass JET_UNICODEINDEX structure for lParam)
  {$IF JET_VERSION >= $0501}
  JET_paramRuntimeCallback    = 73; // pointer to runtime-only callback function
  {$IFEND}
  JET_paramCleanupMismatchedLogFiles  = 77; // instead of erroring out after a successful recovery with JET_errLogFileSizeMismatchDatabasesConsistent, ESE will silently delete the old log files and checkpoint file and continue operations
  {$IF JET_VERSION >= $0501}
  JET_paramRecordUpgradeDirtyLevel  = 78; // how aggresively should pages with their record format converted be flushed (0-3)
  //            81 // JET_paramGlobalMinVerPages defined above
  JET_paramOSSnapshotTimeout    = 82; // timeout for the freeze period in msec
  {$IFEND}

  JET_paramExceptionAction    = 98; // what to do with exceptions generated within JET
  JET_paramEventLogCache     = 99;  // number of bytes of eventlog records to cache if service is not available
  {$IF JET_VERSION >= $0501}
  JET_paramCreatePathIfNotExist   = 100; // create system/temp/log/log-failover paths if they do not exist
  JET_paramPageHintCacheSize    = 101; // maximum size of the fast page latch hint cache in bytes
  JET_paramOneDatabasePerSession   = 102; // allow just one open user database per session
  JET_paramMaxInstances     = 104; // maximum number of instances per process

  JET_paramIndexTuplesLengthMin   = 110; // for tuple indexes, minimum length of a tuple
  JET_paramIndexTuplesLengthMax   = 111; // for tuple indexes, maximum length of a tuple
  JET_paramIndexTuplesToIndexMax   = 112; // for tuple indexes, maximum number of characters in a given string to index
  {$IFEND}
  {$IF JET_VERSION >= $0502}
  JET_paramAlternateDatabaseRecoveryPath = 113; // recovery-only - search for dirty-shutdown databases in specified location only
  {$IFEND}
  {$IF JET_VERSION >= $0600}
  JET_paramIndexTupleIncrement   = 132; // for tuple indexes, offset increment for each succesive tuple
  JET_paramIndexTupleStart    = 133; // for tuple indexes, offset to start tuple indexing
  JET_paramKeyMost      = 134; // read only maximum settable key length before key trunctation occurs
  JET_paramLegacyFileNames    = 136;  // Legacy  file name characteristics to preserve ( JET_bitESE98FileNames | JET_bitEightDotThreeSoftCompat )
  {$IFEND}
  {$IF JET_VERSION >= $0601}
  JET_paramWaypointLatency    = 153;  // The latency (in logs) behind the tip / highest committed log to defer database page flushes.
  JET_paramDefragmentSequentialBTrees  = 160;  // Turn on/off automatic sequential B-tree defragmentation
  JET_paramDefragmentSequentialBTreesDensityCheckFrequency = 161;  // Determine how frequently B-tree density is checked
  JET_paramIOThrottlingTimeQuanta   = 162;  // Max time (in MS) that the I/O throttling mechanism gives a task to run for it to be considered 'completed'.
  JET_paramLVChunkSizeMost    = 163;  // Max LV chuck size supported wrt the chosen page size (R/O)
  JET_paramMaxCoalesceReadSize   = 164;  // Max number of bytes that can be grouped for a coalesced read operation.
  JET_paramMaxCoalesceWriteSize   = 165;  // Max number of bytes that can be grouped for a coalesced write operation.
  JET_paramMaxCoalesceReadGapSize  = 166;  // Max number of bytes that can be gapped for a coalesced read IO operation.
  JET_paramMaxCoalesceWriteGapSize  = 167; // Max number of bytes that can be gapped for a coalesced write IO operation.
  {$IFEND}


  {$IF JET_VERSION >= $0600}

  { Flags for JET_paramLegacyFileNames }

  JET_bitESE98FileNames  = $00000001; // Preserve the .log and .chk extension for compatibility reasons (i.e. Exchange)
  JET_bitEightDotThreeSoftCompat = $00000002; // Preserve the 8.3 naming syntax for as long as possible. (this should not be changed, w/o ensuring there are no log files)
  {$IFEND}

  { Flags for JetInit2, JetInit3 }

  {$IF JET_VERSION >= $0501}
  // IGNORE_MISSING_ATTACH, ignoring hanging asserts for missing databases during recovery
  JET_bitReplayIgnoreMissingDB = $00000004; // ignore missing databases
  {$IFEND}
  {$IF JET_VERSION >= $0600}
  JET_bitRecoveryWithoutUndo  = $00000008; // perform recovery, but halt at the Undo phase
  JET_bitTruncateLogsAfterRecovery = $00000010; // on successful soft recovery, truncate log files
  JET_bitReplayMissingMapEntryDB = $00000020; { missing database map entry default to same location }
  JET_bitLogStreamMustExist  = $00000040; // transaction logs must exist in the logfile directory (ie. cannot auto-start a new log stream)
  {$IFEND}
  {$IF JET_VERSION >= $0601}
  JET_bitReplayIgnoreLostLogs  = $00000080; // ignore logs lost from the end of the log stream
  {$IFEND}

  {$IF JET_VERSION >= $0501}
  JET_flagsSoftRecovery =  JET_bitReplayIgnoreMissingDB
                           {$IF JET_VERSION >= $0600}
                           or JET_bitReplayMissingMapEntryDB
                           {$IFEND}
                           {$IF JET_VERSION >= JET_VERSION_WIN7}
                           or JET_bitReplayIgnoreLostLogs
                           {$IFEND};
  {$IFEND}

  { Flags for JetTerm2 }

  JET_bitTermComplete   = $00000001;
  JET_bitTermAbrupt   = $00000002;
  JET_bitTermStopBackup  = $00000004;
  {$IF JET_VERSION >= $0601}
  JET_bitTermDirty   = $00000008;
  {$IFEND}

  { Flags for JetIdle }

  JET_bitIdleFlushBuffers  = $00000001;
  JET_bitIdleCompact   = $00000002;
  JET_bitIdleStatus   = $00000004;

  { Flags for JetEndSession }


  { Flags for JetAttach/OpenDatabase }

  JET_bitDbReadOnly   = $00000001;
  JET_bitDbExclusive   = $00000002; { multiple opens allowed }
  JET_bitDbDeleteCorruptIndexes = $00000010; { delete indexes possibly corrupted by NT version upgrade }
  {$IF JET_VERSION >= $0502}
  JET_bitDbDeleteUnicodeIndexes = $00000400; { delete all indexes with unicode columns }
  {$IFEND}
  {$IF JET_VERSION >= $0501}
  JET_bitDbUpgrade   = $00000200; { }
  {$IFEND}

  { Flags for JetDetachDatabase2 }

  {$IF JET_VERSION >= $0501}
  JET_bitForceDetach      = $00000001;
  JET_bitForceCloseAndDetach =  ($00000002 or JET_bitForceDetach);
  {$IFEND}

  { Flags for JetCreateDatabase }

  JET_bitDbRecoveryOff   = $00000008; { disable logging/recovery for this database }
  JET_bitDbShadowingOff  = $00000080; { disable catalog shadowing }
  {$IF JET_VERSION >= $0501}
  JET_bitDbOverwriteExisting = $00000200; { overwrite existing database with same name }
  {$IFEND}

  { Flags for JetBackup, JetBeginExternalBackup, JetBeginExternalBackupInstance, JetBeginSurrogateBackup }

  JET_bitBackupIncremental = $00000001;
  JET_bitBackupAtomic   = $00000004;
  {$IF JET_VERSION >= $0501}
  JET_bitBackupSnapshot  = $00000010;
  {$IFEND}

  { Flags for JetEndExternalBackupInstance2, JetEndSurrogateBackup }

  {$IF JET_VERSION >= $0501}
  JET_bitBackupEndNormal   = $0001;
  JET_bitBackupEndAbort   = $0002;
  {$IFEND}
  {$IF JET_VERSION >= $0600}
  JET_bitBackupTruncateDone  = $0100;
  {$IFEND}

  { Database types }

  JET_dbidNil   = JET_DBID( $FFFFFFFF);


  { Flags for JetCreateTableColumnIndex }
  JET_bitTableCreateFixedDDL  = $00000001; { DDL is fixed }
  JET_bitTableCreateTemplateTable = $00000002; { DDL is inheritable (implies FixedDDL) }
  {$IF JET_VERSION >= $0501}
  JET_bitTableCreateNoFixedVarColumnsInDerivedTables = $00000004;
              // used in conjunction with JET_bitTableCreateTemplateTable
              // to disallow fixed/var columns in derived tables (so that
              // fixed/var columns may be added to the template in the future)
  {$IFEND}


  { Flags for JetAddColumn, JetGetColumnInfo, JetOpenTempTable }

  JET_bitColumnFixed   = $00000001;
  JET_bitColumnTagged   = $00000002;
  JET_bitColumnNotNULL  = $00000004;
  JET_bitColumnVersion   = $00000008;
  JET_bitColumnAutoincrement = $00000010;
  JET_bitColumnUpdatable  = $00000020; { JetGetColumnInfo only }
  JET_bitColumnTTKey   = $00000040; { JetOpenTempTable only }
  JET_bitColumnTTDescending = $00000080; { JetOpenTempTable only }
  JET_bitColumnMultiValued  = $00000400;
  JET_bitColumnEscrowUpdate = $00000800; { escrow updated }
  JET_bitColumnUnversioned = $00001000; { for add column only - add column unversioned }
  {$IF JET_VERSION >= $0501}
  JET_bitColumnMaybeNull  = $00002000; { for retrieve column info of outer join where no match from the inner table }
  JET_bitColumnFinalize   = $00004000; { this is a finalizable column (issue callback if escrow value equals 0) }
  JET_bitColumnUserDefinedDefault = $00008000; { default value from a user-provided callback }
  {$IFEND}
  {$IF JET_VERSION >= $0502}
  JET_bitColumnDeleteOnZero = $00020000; { this is a finalizable column (delete record if escrow value equals 0) }
  {$IFEND}
  {$IF JET_VERSION >= $0601}
  JET_bitColumnCompressed  = $00080000; { data in the column can be compressed }
  {$IFEND}

  {$IF JET_VERSION >= $0501}
  // flags for JetDeleteColumn
  JET_bitDeleteColumnIgnoreTemplateColumns = $00000001; // for derived tables, don't bother looking in template columns
  {$IFEND}


  { Flags for JetSetCurrentIndex }

  JET_bitMoveFirst   = $00000000;
  JET_bitNoMove    = $00000002;

  { Flags for JetMakeKey }

  JET_bitNewKey    = $00000001;
  JET_bitStrLimit    = $00000002;
  JET_bitSubStrLimit   = $00000004;
  JET_bitNormalizedKey   = $00000008;
  JET_bitKeyDataZeroLength = $00000010;
  {$IF JET_VERSION >= $0501} 
  JET_bitFullColumnStartLimit = $00000100;
  JET_bitFullColumnEndLimit = $00000200;
  JET_bitPartialColumnStartLimit = $00000400;
  JET_bitPartialColumnEndLimit = $00000800;
  {$IFEND}

  { Flags for JetSetIndexRange }

  JET_bitRangeInclusive  = $00000001;
  JET_bitRangeUpperLimit  = $00000002;
  JET_bitRangeInstantDuration = $00000004;
  JET_bitRangeRemove   = $00000008;

  { Flags for JetGetLock }

  JET_bitReadLock    = $00000001;
  JET_bitWriteLock   = $00000002;

  { Constants for JetMove }

  JET_MoveFirst    = ($80000000);
  JET_MovePrevious =   (-1);
  JET_MoveNext     = (+1);
  JET_MoveLast     = ($7fffffff);

  { Flags for JetMove }

  JET_bitMoveKeyNE   = $00000001;

  { Flags for JetSeek }

  JET_bitSeekEQ    = $00000001;
  JET_bitSeekLT    = $00000002;
  JET_bitSeekLE    = $00000004;
  JET_bitSeekGE    = $00000008;
  JET_bitSeekGT     = $00000010;
  JET_bitSetIndexRange  = $00000020;
  {$IF JET_VERSION >= $0502}
  JET_bitCheckUniqueness  = $00000040; // to be used with JET_bitSeekEQ only, returns JET_wrnUniqueKey if seek lands on a key which has no dupes
  {$IFEND}

  {$IF JET_VERSION >= $0501}
  // Flags for JetGotoSecondaryIndexBookmark
  JET_bitBookmarkPermitVirtualCurrency = $00000001; // place cursor on relative position in index if specified bookmark no longer exists
  {$IFEND}

  { Flags for JET_CONDITIONALCOLUMN }
  JET_bitIndexColumnMustBeNull = $00000001;
  JET_bitIndexColumnMustBeNonNull = $00000002;

  { Flags for JET_INDEXRANGE }
  JET_bitRecordInIndex  = $00000001;
  JET_bitRecordNotInIndex  = $00000002;

  { Flags for JetCreateIndex }

  JET_bitIndexUnique   = $00000001;
  JET_bitIndexPrimary   = $00000002;
  JET_bitIndexDisallowNull = $00000004;
  JET_bitIndexIgnoreNull  = $00000008;
  JET_bitIndexIgnoreAnyNull = $00000020;
  JET_bitIndexIgnoreFirstNull = $00000040;
  JET_bitIndexLazyFlush  = $00000080;
  JET_bitIndexEmpty   = $00000100; // don't attempt to build index, because all entries would evaluate to NULL (MUST also specify JET_bitIgnoreAnyNull)
  JET_bitIndexUnversioned  = $00000200;
  JET_bitIndexSortNullsHigh = $00000400; // NULL sorts after data for all columns in the index
  JET_bitIndexUnicode   = $00000800; // LCID field of JET_INDEXCREATE actually points to a JET_UNICODEINDEX struct to allow user-defined LCMapString() flags
  {$IF JET_VERSION >= $0501}
  JET_bitIndexTuples   = $00001000; // index on substring tuples (text columns only)
  {$IFEND}
  {$IF JET_VERSION >= $0502}
  JET_bitIndexTupleLimits  = $00002000; // cbVarSegMac field of JET_INDEXCREATE actually points to a JET_TUPLELIMITS struct to allow custom tuple index limits (implies JET_bitIndexTuples)
  {$IFEND}
  {$IF JET_VERSION >= $0600}
  JET_bitIndexCrossProduct = $00004000; // index over multiple multi-valued columns has full cross product
  JET_bitIndexKeyMost   = $00008000; // custom index key size set instead of default of 255 bytes
  JET_bitIndexDisallowTruncation = $00010000; // fail update rather than truncate index keys
  JET_bitIndexNestedTable  = $00020000; // index over multiple multi-valued columns but only with values of same itagSequence
  {$IFEND}

  { Flags for index key definition }

  JET_bitKeyAscending   = $00000000;
  JET_bitKeyDescending  = $00000001;

  { Flags for JetOpenTable }

  JET_bitTableDenyWrite  = $00000001;
  JET_bitTableDenyRead  = $00000002;
  JET_bitTableReadOnly  = $00000004;
  JET_bitTableUpdatable  = $00000008;
  JET_bitTablePermitDDL  = $00000010; {  override table flagged as FixedDDL (must be used with DenyRead) }
  JET_bitTableNoCache  = $00000020; { don't cache the pages for this table }
  JET_bitTablePreread  = $00000040; { assume the table is probably not in the buffer cache }
  JET_bitTableOpportuneRead = $00000080; { attempt to opportunely read physically adjacent leaf pages using larger physical IOs }
  JET_bitTableSequential  = $00008000;  {  assume the table will be scanned sequentially }


  JET_bitTableClassMask = $000F0000; {  table stats class mask  }
  JET_bitTableClassNone = $00000000;  {  table belongs to no stats class (default)  }
  JET_bitTableClass1  = $00010000;  {  table belongs to stats class 1  }
  JET_bitTableClass2  = $00020000;  {  table belongs to stats class 2  }
  JET_bitTableClass3  = $00030000;  {  table belongs to stats class 3  }
  JET_bitTableClass4  = $00040000;  {  table belongs to stats class 4  }
  JET_bitTableClass5  = $00050000;  {  table belongs to stats class 5  }
  JET_bitTableClass6  = $00060000;  {  table belongs to stats class 6  }
  JET_bitTableClass7  = $00070000;  {  table belongs to stats class 7  }
  JET_bitTableClass8  = $00080000;  {  table belongs to stats class 8  }
  JET_bitTableClass9  = $00090000;  {  table belongs to stats class 9  }
  JET_bitTableClass10  = $000A0000;  {  table belongs to stats class 10  }
  JET_bitTableClass11  = $000B0000;  {  table belongs to stats class 11  }
  JET_bitTableClass12  = $000C0000;  {  table belongs to stats class 12  }
  JET_bitTableClass13  = $000D0000;  {  table belongs to stats class 13  }
  JET_bitTableClass14  = $000E0000;  {  table belongs to stats class 14  }
  JET_bitTableClass15  = $000F0000;  {  table belongs to stats class 15  }

  {$IF JET_VERSION >= $0501}
  JET_bitLSReset   = $00000001; { reset LS value }
  JET_bitLSCursor   = $00000002; { set/retrieve LS of table cursor }
  JET_bitLSTable   = $00000004; { set/retrieve LS of table }

  JET_LSNil    = JET_LS( not 0);
  {$IFEND}

  {$IF JET_VERSION >= $0601}
  { Flags for JetSetTableSequential }

  JET_bitPrereadForward = $00000001; { Hint that the sequential traversal will be in the forward direction }
  JET_bitPrereadBackward = $00000002; { Hint that the sequential traversal will be in the backward direction }
  {$IFEND}

  { Flags for JetOpenTempTable }

  JET_bitTTIndexed  = $00000001; { Allow seek }
  JET_bitTTUnique   = $00000002; { Remove duplicates }
  JET_bitTTUpdatable  = $00000004; { Allow updates }
  JET_bitTTScrollable  = $00000008; { Allow backwards scrolling }
  JET_bitTTSortNullsHigh = $00000010; { NULL sorts after data for all columns in the index }
  JET_bitTTForceMaterialization = $00000020;      { Forces temp. table to be materialized into a btree (allows for duplicate detection) }
  {$IF JET_VERSION >= $0501}
  JET_bitTTErrorOnDuplicateInsertion = JET_bitTTForceMaterialization; { Error always returned when duplicate is inserted (instead of dupe being silently removed) }
  {$IFEND}
  {$IF JET_VERSION >= $0502}
  JET_bitTTForwardOnly = $00000040; { Prevents temp. table from being materialized into a btree (and enables duplicate keys) }
  {$IFEND}
  {$IF JET_VERSION >= $0601}
  JET_bitTTIntrinsicLVsOnly = $00000080; // permit only intrinsic LV's (so materialisation is not required simply because a TT has an LV column)
  {$IFEND}

  { Flags for JetSetColumn }

  JET_bitSetAppendLV    = $00000001;
  JET_bitSetOverwriteLV   = $00000004; { overwrite JET_coltypLong* byte range }
  JET_bitSetSizeLV    = $00000008; { set JET_coltypLong* size }
  JET_bitSetZeroLength   = $00000020;
  JET_bitSetSeparateLV    = $00000040; { force LV separation }
  JET_bitSetUniqueMultiValues  = $00000080; { prevent duplicate multi-values }
  JET_bitSetUniqueNormalizedMultiValues = $00000100; { prevent duplicate multi-values, normalizing all data before performing comparisons }
  {$IF JET_VERSION >= $0501}
  JET_bitSetRevertToDefaultValue = $00000200; { if setting last tagged instance to NULL, revert to default value instead if one exists }
  JET_bitSetIntrinsicLV   = $00000400; { store whole LV in record without bursting or return an error }
  {$IFEND}
  {$IF JET_VERSION >= $0601}
  JET_bitSetCompressed   = $00020000; { attempt compression when storing the data }
  JET_bitSetUncompressed   = $00010000; { don't attempt compression when storing the data }
  {$IFEND}


  {$IF JET_VERSION >= $0601}
  { Space Hint Flags / JET_SPACEHINTS }

  // Generic
  JET_bitSpaceHintsUtilizeParentSpace  = $00000001; // This changes the internal allocation policy to get space heirarchically from a B-Tree's immediate parent.
  // Create
  JET_bitCreateHintAppendSequential  = $00000002; // This bit will enable Append split behavior to grow according to the growth dynamics of the table (set by cbMinExtent, ulGrowth, cbMaxExtent).
  JET_bitCreateHintHotpointSequential  = $00000004; // This bit will enable Hotpoint split behavior to grow according to the growth dynamics of the table (set by cbMinExtent, ulGrowth, cbMaxExtent).
  // Retrieve
  JET_bitRetrieveHintReserve1    = $00000008; // Reserved and ignored
  JET_bitRetrieveHintTableScanForward  = $00000010; // By setting this the client indicates that forward sequential scan is the predominant usage pattern of this table.
  JET_bitRetrieveHintTableScanBackward = $00000020; // By setting this the client indicates that backwards sequential scan is the predominant usage pattern of this table.
  JET_bitRetrieveHintReserve2    = $00000040; // Reserved and ignored
  JET_bitRetrieveHintReserve3    = $00000080; // Reserved and ignored
  // Update
  //const JET_bitUpdateReserved     = $00000000; // TBD.
  // Delete / .grbitDelete
  JET_bitDeleteHintTableSequential  = $00000100; // This means that the application expects this table to be cleaned up in-order sequentially (from lowest key to highest key)
  {$IFEND}


type
  { Set column parameter structure for JetSetColumns }
  JET_SETCOLUMN = record
    columnid: JET_COLUMNID;
    pvData: Pointer;
    cbData: Cardinal;
    grbit: JET_GRBIT;
    ibLongValue : Cardinal;
    itagSequence: Cardinal;
    err: JET_ERR;
  end;
  PJET_SETCOLUMN = ^JET_SETCOLUMN;

  {$IF JET_VERSION >= $0501}
  JET_SETSYSPARAM_A = record
    paramid: Cardinal;
    lParam: Pointer;
    sz: PAnsiChar;
    err: JET_ERR;
  end;
  PJET_SETSYSPARAM_A = ^JET_SETSYSPARAM_A;

  JET_SETSYSPARAM_W = record
    paramid: Cardinal;
    lParam: Pointer;
    sz: PWideChar;
    err: JET_ERR;
  end;
  PJET_SETSYSPARAM_W = ^JET_SETSYSPARAM_W;

  {$IFDEF JET_UNICODE}
  JET_SETSYSPARAM = JET_SETSYSPARAM_W;
  {$ELSE}
  JET_SETSYSPARAM = JET_SETSYSPARAM_A;
  {$ENDIF}

  {$IFEND}

const
  { Options for JetPrepareUpdate }
  JET_prepInsert      = 0;
  JET_prepReplace      = 2;
  JET_prepCancel      = 3;
  JET_prepReplaceNoLock    = 4;
  JET_prepInsertCopy     = 5;
  {$IF JET_VERSION >= $0501}
  JET_prepInsertCopyDeleteOriginal = 7; // used for updating a record in the primary key; avoids the delete/insert process }
  {$IFEND}

  // Flags for JetUpdate
  {$IF JET_VERSION >= $0502}
  JET_bitUpdateCheckESE97Compatibility = $00000001; // check whether record fits if represented in ESE97 database format
  {$IFEND}

  { Flags for JetEscrowUpdate }
  JET_bitEscrowNoRollback   = $0001;

  { Flags for JetRetrieveColumn }

  JET_bitRetrieveCopy    = $00000001;
  JET_bitRetrieveFromIndex  = $00000002;
  JET_bitRetrieveFromPrimaryBookmark = $00000004;
  JET_bitRetrieveTag    = $00000008;
  JET_bitRetrieveNull    = $00000010; { for columnid 0 only }
  JET_bitRetrieveIgnoreDefault = $00000020; { for columnid 0 only }
  JET_bitRetrieveLongId   = $00000040;
  JET_bitRetrieveLongValueRefCount = $00000080; {  for testing use only }
  {$IF JET_VERSION >= $0600}
  JET_bitRetrieveTuple   = $00000800; { retrieve tuple fragment from index }
  {$IFEND}

type
  { Retrieve column parameter structure for JetRetrieveColumns }
  JET_RETRIEVECOLUMN = record
    columnid: JET_COLUMNID;
    pvData: Pointer;
    cbData: Cardinal;
    cbActual: Cardinal;
    grbit: JET_GRBIT;
    ibLongValue : Cardinal;
    itagSequence: Cardinal;
    columnidNextTagged: JET_COLUMNID;
    err: JET_ERR;
  end;
  PJET_RETRIEVECOLUMN = ^JET_RETRIEVECOLUMN;

{$IF JET_VERSION >= $0501}
const
  { Flags for JetEnumerateColumns }

  JET_bitEnumerateCopy      = JET_bitRetrieveCopy;
  JET_bitEnumerateIgnoreDefault    = JET_bitRetrieveIgnoreDefault;
  JET_bitEnumeratePresenceOnly   = $00020000;
  JET_bitEnumerateTaggedOnly    = $00040000;
  JET_bitEnumerateCompressOutput   = $00080000;
  {$IF JET_VERSION >= $0502}
  // Available on Server 2003 SP1
  JET_bitEnumerateIgnoreUserDefinedDefault = $00100000;
  {$IFEND}
  {$IF JET_VERSION >= $0601}
  JET_bitEnumerateInRecordOnly   = $00200000;
  {$IFEND}

type
  { Parameter structures for JetEnumerateColumns }
  JET_ENUMCOLUMNID = record
    columnid: JET_COLUMNID;
    ctagSequence: Cardinal;
    rgtagSequence: PCardinal;
  end;
  PJET_ENUMCOLUMNID = ^JET_ENUMCOLUMNID;

  JET_ENUMCOLUMNVALUE = record
    itagSequence: Cardinal;
    err: JET_ERR;
    cbData: Cardinal;
    pvData: Pointer;
  end;
  PJET_ENUMCOLUMNVALUE = ^JET_ENUMCOLUMNVALUE;

  JET_ENUMCOLUMN = record
    columnid: JET_COLUMNID;
    err: JET_ERR;
    case Byte of
      0: (enum: record   { err != JET_wrnColumnSingleValue }
             cEnumColumnValue: Cardinal;
             rgEnumColumnValue: PJET_ENUMCOLUMNVALUE;
           end);
      1: (data: record  { err == JET_wrnColumnSingleValue }
             cbData: Cardinal;
             pvData: Pointer;
           end);
  end;
  PJET_ENUMCOLUMN = ^JET_ENUMCOLUMN;

  { Realloc callback for JetEnumerateColumns }
  JET_PFNREALLOC = function(pvContext: Pointer;
                            pv: Pointer;
                            cb: Cardinal
                           ): Pointer;  stdcall;



{$IFEND}


{$IF JET_VERSION >= $0600}
const
  { Flags for JetGetRecordSize }

  JET_bitRecordSizeInCopyBuffer  = $00000001; // use record in copy buffer
  JET_bitRecordSizeRunningTotal  = $00000002; // increment totals in output buffer instead of setting them
  JET_bitRecordSizeLocal    = $00000004; // ignore Long Values (and other data otherwise not in the same page as the record)

type
  { parameter structures for JetGetRecordSize }
  JET_RECSIZE = record
    cbData: UInt64;     // user data in record
    cbLongValueData: UInt64;  // user data associated with the record but stored in the long-value tree (NOTE: does NOT count intrinsic long-values)
    cbOverhead: UInt64;    // record overhead
    cbLongValueOverhead: UInt64; // overhead of long-value data (NOTE: does not count intrinsic long-values)
    cNonTaggedColumns: UInt64;  // total number of fixed/variable columns
    cTaggedColumns: UInt64;   // total number of tagged columns
    cLongValues: UInt64;   // total number of values stored in the long-value tree for this record (NOTE: does NOT count intrinsic long-values)
    cMultiValues: UInt64;   // total number of values beyond the first for each column in the record
  end;
  {$IFEND}


  {$IF JET_VERSION >= $0601}
  JET_RECSIZE2 = record
    cbData: UInt64;     // user data in record
    cbLongValueData: UInt64;  // user data associated with the record but stored in the long-value tree (NOTE: does NOT count intrinsic long-values)
    cbOverhead: UInt64;    // record overhead
    cbLongValueOverhead: UInt64; // overhead of long-value data (NOTE: does not count intrinsic long-values)
    cNonTaggedColumns: UInt64;  // total number of fixed/variable columns
    cTaggedColumns: UInt64;   // total number of tagged columns
    cLongValues: UInt64;   // total number of values stored in the long-value tree for this record (NOTE: does NOT count intrinsic long-values)
    cMultiValues: UInt64;   // total number of values beyond the first for each column in the record
    cCompressedColumns: UInt64;  // total number of columns which are compressed
    cbDataCompressed: UInt64;  // compressed size of user data in record (same as cbData if no intrinsic long-values are compressed)
    cbLongValueDataCompressed: UInt64; // compressed size of user data in the long-value tree (same as cbLongValue data if no separated long values are compressed)
  end;
  {$IFEND}

const
  { Flags for JetBeginTransaction2 }
  {$IF JET_VERSION >= $0501}
  JET_bitTransactionReadOnly = $00000001; { transaction will not modify the database }
  {$IFEND}

  { Flags for JetCommitTransaction }

  JET_bitCommitLazyFlush  = $00000001; { lazy flush log buffers. }
  JET_bitWaitLastLevel0Commit = $00000002; { wait for last level 0 commit record flushed }
  {$IF JET_VERSION >= $0502}
  JET_bitWaitAllLevel0Commit = $00000008; { wait for all level 0 commits to be flushed }
  {$IFEND}
  {$IF JET_VERSION >= $0601}
  JET_bitForceNewLog   = $00000010;
  {$IFEND}

  { Flags for JetRollback }

  JET_bitRollbackAll   = $00000001;


  {$IF JET_VERSION >= $0600}
  { Flags for JetOSSnapshot APIs }

  { Flags for JetOSSnapshotPrepare }
  JET_bitIncrementalSnapshot = $00000001; { bit 0: full (0) or incremental (1) snapshot }
  JET_bitCopySnapshot   = $00000002; { bit 1: normal (0) or copy (1) snapshot }
  JET_bitContinueAfterThaw = $00000004; { bit 2: end on thaw (0) or wait for [truncate +] end snapshot }
  {$IF JET_VERSION >= $0601}
  JET_bitExplicitPrepare  = $00000008; { bit 3: all instaces prepared by default (0) or no instance prepared by default (1)  }
  {$IFEND}

  { Flags for JetOSSnapshotTruncateLog & JetOSSnapshotTruncateLogInstance }
  JET_bitAllDatabasesSnapshot = $00000001; { bit 0: there are detached dbs in the instance (i.e. can't truncate logs) }

  { Flags for JetOSSnapshotEnd }
  JET_bitAbortSnapshot  = $00000001;  { snapshot process failed }
  {$IFEND}


  { Info parameter for JetGetDatabaseInfo and JetGetDatabaseFileInfo }

  JET_DbInfoFilename   = 0;
  JET_DbInfoConnect   = 1;
  JET_DbInfoCountry   = 2;
  {$IF JET_VERSION >= $0501}
  JET_DbInfoLCID    = 3;
  {$IFEND}
  JET_DbInfoLangid   = 3;  // OBSOLETE: use JET_DbInfoLCID instead
  JET_DbInfoCp    = 4;
  JET_DbInfoCollate   = 5;
  JET_DbInfoOptions   = 6;
  JET_DbInfoTransactions  = 7;
  JET_DbInfoVersion   = 8;
  JET_DbInfoIsam    = 9;
  JET_DbInfoFilesize   = 10;
  JET_DbInfoSpaceOwned  = 11;
  JET_DbInfoSpaceAvailable = 12;
  JET_DbInfoUpgrade_   = 13;  // renamed from JET_DbInfoUpgrade
  JET_DbInfoMisc_    = 14;    // renamed from JET_DbInfoMisc
  {$IF JET_VERSION >= $0501}
  JET_DbInfoDBInUse   = 15;
  JET_DbInfoPageSize   = 17;
  {$IFEND}
  {$IF JET_VERSION >= $0600}
  JET_DbInfoFileType   = 19;

  {$IFEND}

  { Dbstates from JetGetDatabaseFileInfo }

  JET_dbstateJustCreated     = 1;
  JET_dbstateDirtyShutdown    = 2;
  JET_dbstateCleanShutdown    = 3;
  JET_dbstateBeingConverted    = 4;
  {$IF JET_VERSION >= $0501}
  JET_dbstateForceDetach     = 5;
  {$IFEND}

  {$IF JET_VERSION >= $0600}

  // supported file types (returned from JetGetDatabaseFileInfo with JET_DbInfoFileType)

  JET_filetypeUnknown   = 0;
  JET_filetypeDatabase  = 1;
  JET_filetypeLog    = 3;
  JET_filetypeCheckpoint  = 4;
  JET_filetypeTempDatabase = 5;

  {$IFEND}

  { Column data types }

  JET_coltypNil    = 0;
  JET_coltypBit    = 1; { True, False, or NULL }
  JET_coltypUnsignedByte  = 2; { 1-byte integer, unsigned }
  JET_coltypShort    = 3; { 2-byte integer, signed }
  JET_coltypLong    = 4; { 4-byte integer, signed }
  JET_coltypCurrency   = 5; { 8 byte integer, signed }
  JET_coltypIEEESingle  = 6; { 4-byte IEEE single precision }
  JET_coltypIEEEDouble  = 7; { 8-byte IEEE double precision }
  JET_coltypDateTime   = 8; { Integral date, fractional time }
  JET_coltypBinary   = 9; { Binary data, < 255 bytes }
  JET_coltypText    = 10; { ANSI text, case insensitive, < 255 bytes }
  JET_coltypLongBinary  = 11; { Binary data, long value }
  JET_coltypLongText   = 12; { ANSI text, long value }
  {$IF JET_VERSION >= $0600}
  JET_coltypSLV    = 13; { SLV's }
  JET_coltypUnsignedLong  = 14; { 4-byte unsigned integer }
  JET_coltypLongLong   = 15; { 8-byte signed integer }
  JET_coltypGUID    = 16; { 16-byte globally unique identifier }
  JET_coltypUnsignedShort  = 17; { 2-byte unsigned integer }
  JET_coltypMax    = 18; { the number of column types  }
                         { used for validity tests and }
                         { array declarations.     }
  {$ELSEIF JET_VERSION >= $0501}
  JET_coltypSLV    = 13; { SLV's }
  JET_coltypMax    = 14; { the number of column types  }
                         { used for validity tests and }
                         { array declarations.     }
  {$ELSE}
  JET_coltypMax    = 13; { the number of column types  }
                         { used for validity tests and }
                         { array declarations.     }
  {$IFEND}

  { Info levels for JetGetObjectInfo }

  JET_ObjInfo     = Cardinal(0);
  JET_ObjInfoListNoStats  = Cardinal(1);
  JET_ObjInfoList    = Cardinal(2);
  JET_ObjInfoSysTabCursor  = Cardinal(3);
  JET_ObjInfoListACM   = Cardinal(4); { Blocked by JetGetObjectInfo }
  JET_ObjInfoNoStats   = Cardinal(5);
  JET_ObjInfoSysTabReadOnly = Cardinal(6);
  JET_ObjInfoRulesLoaded  = Cardinal(7);
  JET_ObjInfoMax    = Cardinal(8);

  { Info levels for JetGetTableInfo }

  JET_TblInfo    = Cardinal(0);
  JET_TblInfoName   = Cardinal(1);
  JET_TblInfoDbid   = Cardinal(2);
  JET_TblInfoMostMany   = Cardinal(3);
  JET_TblInfoRvt   = Cardinal(4);
  JET_TblInfoOLC   = Cardinal(5);
  JET_TblInfoResetOLC  = Cardinal(6);
  JET_TblInfoSpaceUsage = Cardinal(7);
  JET_TblInfoDumpTable = Cardinal(8);
  JET_TblInfoSpaceAlloc = Cardinal(9);
  JET_TblInfoSpaceOwned = Cardinal(10);     // OwnExt
  JET_TblInfoSpaceAvailable  = Cardinal(11);   // AvailExt
  JET_TblInfoTemplateTableName = Cardinal(12);

  { Info levels for JetGetIndexInfo and JetGetTableIndexInfo }

  JET_IdxInfo     = Cardinal(0);
  JET_IdxInfoList    = Cardinal(1);
  JET_IdxInfoSysTabCursor  = Cardinal(2);
  JET_IdxInfoOLC    = Cardinal(3);
  JET_IdxInfoResetOLC   = Cardinal(4);
  JET_IdxInfoSpaceAlloc  = Cardinal(5);
  {$IF JET_VERSION >= $0501}
  JET_IdxInfoLCID    = Cardinal(6);
  {$IFEND}
  JET_IdxInfoLangid   = Cardinal(6);  // OBSOLETE: use JET_IdxInfoLCID instead
  JET_IdxInfoCount   = Cardinal(7);
  JET_IdxInfoVarSegMac  = Cardinal(8);
  JET_IdxInfoIndexId   = Cardinal(9);
  {$IF JET_VERSION >= $0600}
  JET_IdxInfoKeyMost   = Cardinal(10);
  {$IFEND}
  {$IF JET_VERSION >= $0601}
  JET_IdxInfoCreateIndex  = Cardinal(11);  // return a JET_INDEXCREATE structure suitable for use by JetCreateIndex2()
  JET_IdxInfoCreateIndex2  = Cardinal(12);  // return a JET_INDEXCREATE2 structure suitable for use by JetCreateIndex2()
  {$IFEND}

  { Info levels for JetGetColumnInfo and JetGetTableColumnInfo }

  JET_ColInfo     = Cardinal(0);
  JET_ColInfoList    = Cardinal(1);
  JET_ColInfoSysTabCursor  = Cardinal(3);
  JET_ColInfoBase    = Cardinal(4);
  JET_ColInfoListCompact   = Cardinal(5);  // INTERNAL USE ONLY
  {$IF JET_VERSION >= $0501}
  JET_ColInfoByColid   = Cardinal(6);
  JET_ColInfoListSortColumnid = Cardinal(7);  // OBSOLETE: use grbit instead
  {$IFEND}
  {$IF JET_VERSION >= $0600}
  JET_ColInfoBaseByColid  = Cardinal(8);
  {$IFEND}

  {$IF JET_VERSION >= $0600}

  // Grbits for JET_GetColumnInfo and JetGetTableColumnInfo (OR together with the info level)
  JET_ColInfoGrbitNonDerivedColumnsOnly = $80000000; // for lists, only return non-derived columns (if the table is derived from a template)
  JET_ColInfoGrbitMinimalInfo   = $40000000; // for lists, only return the column name and columnid of each column
  JET_ColInfoGrbitSortByColumnid  = $20000000; // for lists, sort returned column list by columnid (default is to sort list by column name)

  {$IFEND}

  {$IF JET_VERSION >= $0600}

  { Info levels for JetGetInstanceMiscInfo, which is very different than JetGetInstanceInfo, as that retrieves a list of all instances }

  JET_InstanceMiscInfoLogSignature = Cardinal(0);

  {$IFEND}

  { Engine Object Types }

  JET_objtypNil    = 0;
  JET_objtypTable    = 1;

  { Compact Options }

  JET_bitCompactStats   = $00000020; { Dump off-line compaction stats (only when progress meter also specified) }
  JET_bitCompactRepair  = $00000040; { Don't preread and ignore duplicate keys }

  { Status Notification Processes }

  JET_snpRepair     = 2;
  JET_snpCompact     = 4;
  JET_snpRestore     = 8;
  JET_snpBackup     = 9;
  JET_snpUpgrade     = 10;
  {$IF JET_VERSION >= $0501}
  JET_snpScrub     = 11;
  JET_snpUpgradeRecordFormat  = 12;
  {$IFEND}


  { Status Notification Types }

  JET_sntBegin   = 5; { callback for beginning of operation }
  JET_sntRequirements  = 7; { callback for returning operation requirements }
  JET_sntProgress   = 0; { callback for progress }
  JET_sntComplete   = 6; { callback for completion of operation }
  JET_sntFail    = 3; { callback for failure during progress }

  { Exception action }

  JET_ExceptionMsgBox = $0001;  { Display message box on exception }
  JET_ExceptionNone = $0002;  { Do nothing on exceptions }


  {$IF JET_VERSION >= $0501}
  // Online defragmentation options
  JET_OnlineDefragDisable  = $0000;  // disable online defrag
  JET_OnlineDefragAllOBSOLETE = $0001;  // enable online defrag for everything (must be 1 for backward compatibility)
  JET_OnlineDefragDatabases = $0002;  // enable online defrag of databases
  JET_OnlineDefragSpaceTrees = $0004;  // enable online defrag of space trees
  JET_OnlineDefragAll   = $ffff;  // enable online defrag for everything

  {$IFEND}


  {********************************************************************}
  {************************** ERROR CODES *****************************}
  {********************************************************************}

  { The Error codes are not versioned with WINVER. }

  { SUCCESS }

  JET_errSuccess       = 0;    { Successful Operation }

  { ERRORS }

  JET_wrnNyi       = -1;    { Function Not Yet Implemented }

  { SYSTEM errors
  {}
  JET_errRfsFailure       = -100;  { Resource Failure Simulator failure }
  JET_errRfsNotArmed      = -101;  { Resource Failure Simulator not initialized }
  JET_errFileClose        = -102;  { Could not close file }
  JET_errOutOfThreads     = -103;  { Could not start thread }
  JET_errTooManyIO        = -105;  { System busy due to too many IOs }
  JET_errTaskDropped      = -106;  { A requested async task could not be executed }
  JET_errInternalError    = -107;  { Fatal internal error }

  // BUFFER MANAGER errors
  //
  JET_errDatabaseBufferDependenciesCorrupted  = -255; { Buffer dependencies improperly set. Recovery failure }

  { DIRECTORY MANAGER errors
  {}
  JET_wrnRemainingVersions    =  321;  { The version store is still active }
  JET_errPreviousVersion      = -322;  { Version already existed. Recovery failure }
  JET_errPageBoundary         = -323;  { Reached Page Boundary }
  JET_errKeyBoundary          = -324;  { Reached Key Boundary }
  JET_errBadPageLink          = -327;  { Database corrupted }
  JET_errBadBookmark          = -328;  { Bookmark has no corresponding address in database }
  JET_errNTSystemCallFailed    = -334;  // A call to the operating system failed
  JET_errBadParentPageLink    = -338;  // Database corrupted
  JET_errSPAvailExtCacheOutOfSync  = -340;  // AvailExt cache doesn't match btree
  JET_errSPAvailExtCorrupted       = -341;  // AvailExt space tree is corrupt
  JET_errSPAvailExtCacheOutOfMemory = -342;  // Out of memory allocating an AvailExt cache node
  JET_errSPOwnExtCorrupted          = -343;  // OwnExt space tree is corrupt
  JET_errDbTimeCorrupted            = -344;  // Dbtime on current page is greater than global database dbtime
  JET_wrnUniqueKey                  = 345;  // seek on non-unique index yielded a unique key
  JET_errKeyTruncated               = -346;  // key truncated on index that disallows key truncation

  { RECORD MANAGER errors
  {}
  JET_wrnSeparateLongValue    = 406;  { Column is a separated long-value }
  // see below: JET_wrnRecordFoundGreater   = JET_wrnSeekNotEqual;
  // see below: JET_wrnRecordFoundLess      = JET_wrnSeekNotEqual;
  // see below: JET_errColumnIllegalNull    = JET_errNullInvalid;
  JET_errKeyTooBig     = -408;  { Key is too large }

  { LOGGING/RECOVERY errors
  {}
  JET_errInvalidLoggedOperation  = -500;  { Logged operation cannot be redone }
  JET_errLogFileCorrupt      = -501;  { Log file is corrupt }
  JET_errNoBackupDirectory    = -503;  { No backup directory given }
  JET_errBackupDirectoryNotEmpty   = -504;  { The backup directory is not emtpy }
  JET_errBackupInProgress    = -505;  { Backup is active already }
  JET_errRestoreInProgress   = -506;  { Restore in progress }
  JET_errMissingPreviousLogFile  = -509;  { Missing the log file for check point }
  JET_errLogWriteFail     = -510;  { Failure writing to log file }
  JET_errLogDisabledDueToRecoveryFailure = -511; { Try to log something after recovery faild }
  JET_errCannotLogDuringRecoveryRedo = -512; { Try to log something during recovery redo }
  JET_errLogGenerationMismatch  = -513;  { Name of logfile does not match internal generation number }
  JET_errBadLogVersion        = -514;  { Version of log file is not compatible with Jet version }
  JET_errInvalidLogSequence     = -515;  { Timestamp in next log does not match expected }
  JET_errLoggingDisabled     = -516;  { Log is not active }
  JET_errLogBufferTooSmall   = -517;  { Log buffer is too small for recovery }
  JET_errLogSequenceEnd    = -519;  { Maximum log file number exceeded }
  JET_errNoBackup      = -520;  { No backup in progress }
  JET_errInvalidBackupSequence  = -521;  { Backup call out of sequence }
  JET_errBackupNotAllowedYet   = -523;  { Cannot do backup now }
  JET_errDeleteBackupFileFail      = -524;  { Could not delete backup file }
  JET_errMakeBackupDirectoryFail   = -525;  { Could not make backup temp directory }
  JET_errInvalidBackup     = -526;  { Cannot perform incremental backup when circular logging enabled }
  JET_errRecoveredWithErrors   = -527;  { Restored with errors }
  JET_errMissingLogFile    = -528;  { Current log file missing }
  JET_errLogDiskFull     = -529;  { Log disk full }
  JET_errBadLogSignature    = -530;  { Bad signature for a log file }
  JET_errBadDbSignature    = -531;  { Bad signature for a db file }
  JET_errBadCheckpointSignature  = -532;  { Bad signature for a checkpoint file }
  JET_errCheckpointCorrupt   = -533;  { Checkpoint file not found or corrupt }
  JET_errMissingPatchPage    = -534;  { Patch file page not found during recovery }
  JET_errBadPatchPage     = -535;  { Patch file page is not valid }
  JET_errRedoAbruptEnded    = -536;  { Redo abruptly ended due to sudden failure in reading logs from log file }
  JET_errBadSLVSignature    = -537;  { Signature in SLV file does not agree with database }
  JET_errPatchFileMissing    = -538;  { Hard restore detected that patch file is missing from backup set }
  JET_errDatabaseLogSetMismatch  = -539;  { Database does not belong with the current set of log files }
  JET_errDatabaseStreamingFileMismatch = -540; { Database and streaming file do not match each other }
  JET_errLogFileSizeMismatch   = -541;  { actual log file size does not match JET_paramLogFileSize }
  JET_errCheckpointFileNotFound  = -542;  { Could not locate checkpoint file }
  JET_errRequiredLogFilesMissing  = -543;  { The required log files for recovery is missing. }
  JET_errSoftRecoveryOnBackupDatabase = -544;  { Soft recovery is intended on a backup database. Restore should be used instead }
  JET_errLogFileSizeMismatchDatabasesConsistent = -545;  { databases have been recovered, but the log file size used during recovery does not match JET_paramLogFileSize }
  JET_errLogSectorSizeMismatch  = -546;  { the log file sector size does not match the current volume's sector size }
  JET_errLogSectorSizeMismatchDatabasesConsistent = -547;  { databases have been recovered, but the log file sector size (used during recovery) does not match the current volume's sector size }
  JET_errLogSequenceEndDatabasesConsistent  = -548; { databases have been recovered, but all possible log generations in the current sequence are used; delete all log files and the checkpoint file and backup the databases before continuing }

  JET_errStreamingDataNotLogged  = -549;  { Illegal attempt to replay a streaming file operation where the data wasn't logged. Probably caused by an attempt to roll-forward with circular logging enabled }

  JET_errDatabaseDirtyShutdown  = -550;  { Database was not shutdown cleanly. Recovery must first be run to properly complete database operations for the previous shutdown. }
  JET_errDatabaseInconsistent  =  JET_errDatabaseDirtyShutdown; { OBSOLETE }
  JET_errConsistentTimeMismatch  = -551;  { Database last consistent time unmatched }
  JET_errDatabasePatchFileMismatch = -552;  { Patch file is not generated from this backup }
  JET_errEndingRestoreLogTooLow  = -553;  { The starting log number too low for the restore }
  JET_errStartingRestoreLogTooHigh = -554;  { The starting log number too high for the restore }
  JET_errGivenLogFileHasBadSignature = -555;  { Restore log file has bad signature }
  JET_errGivenLogFileIsNotContiguous = -556;  { Restore log file is not contiguous }
  JET_errMissingRestoreLogFiles  = -557;  { Some restore log files are missing }
  JET_wrnExistingLogFileHasBadSignature=  558;  { Existing log file has bad signature }
  JET_wrnExistingLogFileIsNotContiguous=  559;  { Existing log file is not contiguous }
  JET_errMissingFullBackup   = -560;  { The database miss a previous full backup befor incremental backup }
  JET_errBadBackupDatabaseSize  = -561;  { The backup database size is not in 4k }
  JET_errDatabaseAlreadyUpgraded  = -562;  { Attempted to upgrade a database that is already current }
  JET_errDatabaseIncompleteUpgrade = -563;  { Attempted to use a database which was only partially converted to the current format -- must restore from backup }
  JET_wrnSkipThisRecord    =  564;  { INTERNAL ERROR }
  JET_errMissingCurrentLogFiles  = -565;  { Some current log files are missing for continuous restore }

  JET_errDbTimeTooOld      = -566;  { dbtime on page smaller than dbtimeBefore in record }
  JET_errDbTimeTooNew      = -567;  { dbtime on page in advance of the dbtimeBefore in record }
  JET_errMissingFileToBackup    = -569;  { Some log or patch files are missing during backup }

  JET_errLogTornWriteDuringHardRestore = -570; { torn-write was detected in a backup set during hard restore }
  JET_errLogTornWriteDuringHardRecovery = -571; { torn-write was detected during hard recovery (log was not part of a backup set) }
  JET_errLogCorruptDuringHardRestore  = -573; { corruption was detected in a backup set during hard restore }
  JET_errLogCorruptDuringHardRecovery   = -574; { corruption was detected during hard recovery (log was not part of a backup set) }

  JET_errMustDisableLoggingForDbUpgrade = -575; { Cannot have logging enabled while attempting to upgrade db }

  JET_errBadRestoreTargetInstance   = -577; { TargetInstance specified for restore is not found or log files don't match }
  JET_wrnTargetInstanceRunning   =  578; { TargetInstance specified for restore is running }

  JET_errRecoveredWithoutUndo    = -579; { Soft recovery successfully replayed all operations, but the Undo phase of recovery was skipped }

  JET_errDatabasesNotFromSameSnapshot  = -580; { Databases to be restored are not from the same shadow copy backup }
  JET_errSoftRecoveryOnSnapshot   = -581; { Soft recovery on a database from a shadow copy backup set }
  JET_errCommittedLogFilesMissing   = -582; { One or more logs that were committed to this database, are missing.  These log files are required to maintain durable ACID semantics, but not required to maintain consistency if the JET_bitReplayIgnoreLostLogs bit is specified during recovery. }
  JET_errSectorSizeNotSupported   = -583; { The physical sector size reported by the disk subsystem, is unsupported by ESE for a specific file type. }
  JET_errRecoveredWithoutUndoDatabasesConsistent = -584; { Soft recovery successfully replayed all operations and intended to skip the Undo phase of recovery, but the Undo phase was not required }
  JET_wrnCommittedLogFilesLost  =  585;  { One or more logs that were committed to this database, were not recovered.  The database is still clean/consistent, as though the lost log's transactions were committed lazily (and lost). }
  JET_errCommittedLogFileCorrupt   = -586; { One or more logs were found to be corrupt during recovery.  These log files are required to maintain durable ACID semantics, but not required to maintain consistency if the JET_bitIgnoreLostLogs bit and JET_paramDeleteOutOfRangeLogs is specified during recovery. }
  JET_wrnCommittedLogFilesRemoved  =  587;  { One or more logs that were committed to this database, were no recovered.  The database is still clean/consistent, as though the corrupted log's transactions were committed lazily (and lost). }

  JET_wrnDatabaseRepaired     =  595; { Database corruption has been repaired }

  JET_errUnicodeTranslationBufferTooSmall = -601; { Unicode translation buffer too small }
  JET_errUnicodeTranslationFail   = -602; { Unicode normalization failed }
  JET_errUnicodeNormalizationNotSupported = -603; { OS does not provide support for Unicode normalisation (and no normalisation callback was specified) }

  JET_errExistingLogFileHasBadSignature = -610; { Existing log file has bad signature }
  JET_errExistingLogFileIsNotContiguous = -611; { Existing log file is not contiguous }

  JET_errLogReadVerifyFailure   = -612;  { Checksum error in log file during backup }
  JET_errSLVReadVerifyFailure   = -613;  { Checksum error in SLV file during backup }

  JET_errCheckpointDepthTooDeep  = -614; // too many outstanding generations between checkpoint and current generation

  JET_errRestoreOfNonBackupDatabase = -615; // hard recovery attempted on a database that wasn't a backup database
  JET_errLogFileNotCopied    = -616; // log truncation attempted but not all required logs were copied

  JET_errInvalidGrbit     = -900;  { Invalid parameter }

  JET_errTermInProgress      = -1000; { Termination in progress }
  JET_errFeatureNotAvailable   = -1001; { API not supported }
  JET_errInvalidName     = -1002; { Invalid name }
  JET_errInvalidParameter    = -1003; { Invalid API parameter }
  JET_wrnColumnNull     =  1004; { Column is NULL-valued }
  JET_wrnBufferTruncated    =  1006; { Buffer too small for data }
  JET_wrnDatabaseAttached    =  1007; { Database is already attached }
  JET_errDatabaseFileReadOnly   = -1008; { Tried to attach a read-only database file for read/write operations }
  JET_wrnSortOverflow     =  1009; { Sort does not fit in memory }
  JET_errInvalidDatabaseId   = -1010; { Invalid database id }
  JET_errOutOfMemory     = -1011; { Out of Memory }
  JET_errOutOfDatabaseSpace    = -1012; { Maximum database size reached }
  JET_errOutOfCursors     = -1013; { Out of table cursors }
  JET_errOutOfBuffers     = -1014; { Out of database page buffers }
  JET_errTooManyIndexes    = -1015; { Too many indexes }
  JET_errTooManyKeys     = -1016; { Too many columns in an index }
  JET_errRecordDeleted    = -1017; { Record has been deleted }
  JET_errReadVerifyFailure   = -1018; { Checksum error on a database page }
  JET_errPageNotInitialized   = -1019; { Blank database page }
  JET_errOutOfFileHandles     = -1020; { Out of file handles }
  JET_errDiskReadVerificationFailure = -1021; { The OS returned ERROR_CRC from file IO }
  JET_errDiskIO      = -1022; { Disk IO error }
  JET_errInvalidPath     = -1023; { Invalid file path }
  JET_errInvalidSystemPath   = -1024; { Invalid system path }
  JET_errInvalidLogDirectory   = -1025; { Invalid log directory }
  JET_errRecordTooBig     = -1026; { Record larger than maximum size }
  JET_errTooManyOpenDatabases   = -1027; { Too many open databases }
  JET_errInvalidDatabase    = -1028; { Not a database file }
  JET_errNotInitialized    = -1029; { Database engine not initialized }
  JET_errAlreadyInitialized   = -1030; { Database engine already initialized }
  JET_errInitInProgress    = -1031; { Database engine is being initialized }
  JET_errFileAccessDenied    = -1032; { Cannot access file, the file is locked or in use }
  JET_errBufferTooSmall    = -1038; { Buffer is too small }
  JET_wrnSeekNotEqual     =  1039; { Exact match not found during seek }
  JET_errTooManyColumns    = -1040; { Too many columns defined }
  JET_errContainerNotEmpty   = -1043; { Container is not empty }
  JET_errInvalidFilename    = -1044; { Filename is invalid }
  JET_errInvalidBookmark    = -1045; { Invalid bookmark }
  JET_errColumnInUse     = -1046; { Column used in an index }
  JET_errInvalidBufferSize   = -1047; { Data buffer doesn't match column size }
  JET_errColumnNotUpdatable   = -1048; { Cannot set column value }
  JET_errIndexInUse     = -1051; { Index is in use }
  JET_errLinkNotSupported    = -1052; { Link support unavailable }
  JET_errNullKeyDisallowed   = -1053; { Null keys are disallowed on index }
  JET_errNotInTransaction    = -1054; { Operation must be within a transaction }
  JET_wrnNoErrorInfo     =  1055; { No extended error information }
  JET_wrnNoIdleActivity     =  1058; { No idle activity occured }
  JET_errTooManyActiveUsers   = -1059; { Too many active database users }
  JET_errInvalidCountry    = -1061; { Invalid or unknown country code }
  JET_errInvalidLanguageId   = -1062; { Invalid or unknown language id }
  JET_errInvalidCodePage    = -1063; { Invalid or unknown code page }
  JET_errInvalidLCMapStringFlags  = -1064; { Invalid flags for LCMapString() }
  JET_errVersionStoreEntryTooBig  = -1065; { Attempted to create a version store entry (RCE) larger than a version bucket }
  JET_errVersionStoreOutOfMemoryAndCleanupTimedOut = -1066; { Version store out of memory (and cleanup attempt failed to complete) }
  JET_wrnNoWriteLock     =  1067; { No write lock at transaction level 0 }
  JET_wrnColumnSetNull       =  1068; { Column set to NULL-value }
  JET_errVersionStoreOutOfMemory  = -1069; { Version store out of memory (cleanup already attempted) }
  JET_errCannotIndex        = -1071; { Cannot index escrow column or SLV column }
  JET_errRecordNotDeleted    = -1072; { Record has not been deleted }
  JET_errTooManyMempoolEntries  = -1073; { Too many mempool entries requested }
  JET_errOutOfObjectIDs    = -1074; { Out of btree ObjectIDs (perform offline defrag to reclaim freed/unused ObjectIds) }
  JET_errOutOfLongValueIDs   = -1075; { Long-value ID counter has reached maximum value. (perform offline defrag to reclaim free/unused LongValueIDs) }
  JET_errOutOfAutoincrementValues  = -1076; { Auto-increment counter has reached maximum value (offline defrag WILL NOT be able to reclaim free/unused Auto-increment values). }
  JET_errOutOfDbtimeValues   = -1077; { Dbtime counter has reached maximum value (perform offline defrag to reclaim free/unused Dbtime values) }
  JET_errOutOfSequentialIndexValues = -1078; { Sequential index counter has reached maximum value (perform offline defrag to reclaim free/unused SequentialIndex values) }

  JET_errRunningInOneInstanceMode  = -1080; { Multi-instance call with single-instance mode enabled }
  JET_errRunningInMultiInstanceMode = -1081; { Single-instance call with multi-instance mode enabled }
  JET_errSystemParamsAlreadySet  = -1082; { Global system parameters have already been set }

  JET_errSystemPathInUse    = -1083; { System path already used by another database instance }
  JET_errLogFilePathInUse    = -1084; { Logfile path already used by another database instance }
  JET_errTempPathInUse    = -1085; { Temp path already used by another database instance }
  JET_errInstanceNameInUse   = -1086; { Instance Name already in use }

  JET_errInstanceUnavailable   = -1090; { This instance cannot be used because it encountered a fatal error }
  JET_errDatabaseUnavailable   = -1091; { This database cannot be used because it encountered a fatal error }
  JET_errInstanceUnavailableDueToFatalLogDiskFull = -1092; { This instance cannot be used because it encountered a log-disk-full error performing an operation (likely transaction rollback) that could not tolerate failure }

  JET_errOutOfSessions      = -1101; { Out of sessions }
  JET_errWriteConflict    = -1102; { Write lock failed due to outstanding write lock }
  JET_errTransTooDeep     = -1103; { Transactions nested too deeply }
  JET_errInvalidSesid     = -1104; { Invalid session handle }
  JET_errWriteConflictPrimaryIndex = -1105; { Update attempted on uncommitted primary index }
  JET_errInTransaction    = -1108; { Operation not allowed within a transaction }
  JET_errRollbackRequired    = -1109; { Must rollback current transaction -- cannot commit or begin a new one }
  JET_errTransReadOnly    = -1110; { Read-only transaction tried to modify the database }
  JET_errSessionWriteConflict   = -1111; { Attempt to replace the same record by two diffrerent cursors in the same session }

  JET_errRecordTooBigForBackwardCompatibility    = -1112; { record would be too big if represented in a database format from a previous version of Jet }
  JET_errCannotMaterializeForwardOnlySort     = -1113; { The temp table could not be created due to parameters that conflict with JET_bitTTForwardOnly }

  JET_errSesidTableIdMismatch   = -1114; { This session handle can't be used with this table id }
  JET_errInvalidInstance    = -1115; { Invalid instance handle }
  JET_errDirtyShutdown    = -1116; { The instance was shutdown successfully but all the attached databases were left in a dirty state by request via JET_bitTermDirty }


  JET_errDatabaseDuplicate   = -1201; { Database already exists }
  JET_errDatabaseInUse    = -1202; { Database in use }
  JET_errDatabaseNotFound    = -1203; { No such database }
  JET_errDatabaseInvalidName   = -1204; { Invalid database name }
  JET_errDatabaseInvalidPages   = -1205; { Invalid number of pages }
  JET_errDatabaseCorrupted   = -1206; { Non database file or corrupted db }
  JET_errDatabaseLocked    = -1207; { Database exclusively locked }
  JET_errCannotDisableVersioning  = -1208; { Cannot disable versioning for this database }
  JET_errInvalidDatabaseVersion  = -1209; { Database engine is incompatible with database }

  { The following error code are for NT clients only. It will return such error during
 * JetInit if JET_paramCheckFormatWhenOpenFail is set.
  }
  JET_errDatabase200Format   = -1210; { The database is in an older (200) format }
  JET_errDatabase400Format   = -1211; { The database is in an older (400) format }
  JET_errDatabase500Format   = -1212; { The database is in an older (500) format }

  JET_errPageSizeMismatch    = -1213; { The database page size does not match the engine }
  JET_errTooManyInstances    = -1214; { Cannot start any more database instances }
  JET_errDatabaseSharingViolation  = -1215; { A different database instance is using this database }
  JET_errAttachedDatabaseMismatch  = -1216; { An outstanding database attachment has been detected at the start or end of recovery, but database is missing or does not match attachment info }
  JET_errDatabaseInvalidPath   = -1217; { Specified path to database file is illegal }
  JET_errDatabaseIdInUse    = -1218; { A database is being assigned an id already in use }
  JET_errForceDetachNotAllowed   = -1219; { Force Detach allowed only after normal detach errored out }
  JET_errCatalogCorrupted    = -1220; { Corruption detected in catalog }
  JET_errPartiallyAttachedDB   = -1221; { Database is partially attached. Cannot complete attach operation }
  JET_errDatabaseSignInUse   = -1222; { Database with same signature in use }

  JET_errDatabaseCorruptedNoRepair = -1224; { Corrupted db but repair not allowed }
  JET_errInvalidCreateDbVersion  = -1225; { recovery tried to replay a database creation, but the database was originally created with an incompatible (likely older) version of the database engine }


  JET_wrnTableEmpty      =  1301; { Opened an empty table }
  JET_errTableLocked     = -1302; { Table is exclusively locked }
  JET_errTableDuplicate    = -1303; { Table already exists }
  JET_errTableInUse     = -1304; { Table is in use, cannot lock }
  JET_errObjectNotFound    = -1305; { No such table or object }
  JET_errDensityInvalid    = -1307; { Bad file/index density }
  JET_errTableNotEmpty    = -1308; { Table is not empty }
  JET_errInvalidTableId    = -1310; { Invalid table id }
  JET_errTooManyOpenTables   = -1311; { Cannot open any more tables (cleanup already attempted) }
  JET_errIllegalOperation    = -1312; { Oper. not supported on table }
  JET_errTooManyOpenTablesAndCleanupTimedOut = -1313; { Cannot open any more tables (cleanup attempt failed to complete) }
  JET_errObjectDuplicate    = -1314; { Table or object name in use }
  JET_errInvalidObject    = -1316; { Object is invalid for operation }
  JET_errCannotDeleteTempTable  = -1317; { Use CloseTable instead of DeleteTable to delete temp table }
  JET_errCannotDeleteSystemTable  = -1318; { Illegal attempt to delete a system table }
  JET_errCannotDeleteTemplateTable = -1319; { Illegal attempt to delete a template table }
  JET_errExclusiveTableLockRequired = -1322; { Must have exclusive lock on table. }
  JET_errFixedDDL      = -1323; { DDL operations prohibited on this table }
  JET_errFixedInheritedDDL   = -1324; { On a derived table, DDL operations are prohibited on inherited portion of DDL }
  JET_errCannotNestDDL    = -1325; { Nesting of hierarchical DDL is not currently supported. }
  JET_errDDLNotInheritable   = -1326; { Tried to inherit DDL from a table not marked as a template table. }
  JET_wrnTableInUseBySystem   =  1327; { System cleanup has a cursor open on the table }
  JET_errInvalidSettings    = -1328; { System parameters were set improperly }
  JET_errClientRequestToStopJetService   = -1329; { Client has requested stop service }
  JET_errCannotAddFixedVarColumnToDerivedTable = -1330; { Template table was created with NoFixedVarColumnsInDerivedTables }

  { DDL=  errors
  {}
  //=  Note: Some DDL errors have snuck into other categories.
  JET_errIndexCantBuild    = -1401; { Index build failed }
  JET_errIndexHasPrimary    = -1402; { Primary index already defined }
  JET_errIndexDuplicate    = -1403; { Index is already defined }
  JET_errIndexNotFound    = -1404; { No such index }
  JET_errIndexMustStay    = -1405; { Cannot delete clustered index }
  JET_errIndexInvalidDef    = -1406; { Illegal index definition }
  JET_errInvalidCreateIndex    = -1409; { Invalid create index description }
  JET_errTooManyOpenIndexes   = -1410; { Out of index description blocks }
  JET_errMultiValuedIndexViolation = -1411; { Non-unique inter-record index keys generated for a multivalued index }
  JET_errIndexBuildCorrupted   = -1412; { Failed to build a secondary index that properly reflects primary index }
  JET_errPrimaryIndexCorrupted  = -1413; { Primary index is corrupt. The database must be defragmented }
  JET_errSecondaryIndexCorrupted  = -1414; { Secondary index is corrupt. The database must be defragmented }
  JET_wrnCorruptIndexDeleted   =  1415; { Out of date index removed }
  JET_errInvalidIndexId    = -1416; { Illegal index id }

  JET_errIndexTuplesSecondaryIndexOnly  = -1430; // tuple index can only be on a secondary index
  JET_errIndexTuplesTooManyColumns   = -1431; // tuple index may only have eleven columns in the index
  JET_errIndexTuplesOneColumnOnly   =  JET_errIndexTuplesTooManyColumns; { OBSOLETE }
  JET_errIndexTuplesNonUniqueOnly    = -1432; // tuple index must be a non-unique index
  JET_errIndexTuplesTextBinaryColumnsOnly  = -1433; // tuple index must be on a text/binary column
  JET_errIndexTuplesTextColumnsOnly  =  JET_errIndexTuplesTextBinaryColumnsOnly;  { OBSOLETE }
  JET_errIndexTuplesVarSegMacNotAllowed  = -1434; // tuple index does not allow setting cbVarSegMac
  JET_errIndexTuplesInvalidLimits    = -1435; // invalid min/max tuple length or max characters to index specified
  JET_errIndexTuplesCannotRetrieveFromIndex = -1436; // cannot call RetrieveColumn() with RetrieveFromIndex on a tuple index
  JET_errIndexTuplesKeyTooSmall    = -1437; // specified key does not meet minimum tuple length

  { DML=  errors
  {}
  //=  Note: Some DML errors have snuck into other categories.
  // Note:=  Some DDL errors have inappropriately snuck in here.
  JET_errColumnLong     = -1501; { Column value is long }
  JET_errColumnNoChunk    = -1502; { No such chunk in long value }
  JET_errColumnDoesNotFit    = -1503; { Field will not fit in record }
  JET_errNullInvalid     = -1504; { Null not valid }
  JET_errColumnIndexed    = -1505; { Column indexed, cannot delete }
  JET_errColumnTooBig     = -1506; { Field length is greater than maximum }
  JET_errColumnNotFound    = -1507; { No such column }
  JET_errColumnDuplicate    = -1508; { Field is already defined }
  JET_errMultiValuedColumnMustBeTagged = -1509; { Attempted to create a multi-valued column, but column was not Tagged }
  JET_errColumnRedundant    = -1510; { Second autoincrement or version column }
  JET_errInvalidColumnType   = -1511; { Invalid column data type }
  JET_wrnColumnMaxTruncated    =  1512; { Max length too big, truncated }
  JET_errTaggedNotNULL    = -1514; { No non-NULL tagged columns }
  JET_errNoCurrentIndex    = -1515; { Invalid w/o a current index }
  JET_errKeyIsMade     = -1516; { The key is completely made }
  JET_errBadColumnId     = -1517; { Column Id Incorrect }
  JET_errBadItagSequence    = -1518; { Bad itagSequence for tagged column }
  JET_errColumnInRelationship   = -1519; { Cannot delete, column participates in relationship }
  JET_wrnCopyLongValue    =  1520; { Single instance column bursted }
  JET_errCannotBeTagged    = -1521; { AutoIncrement and Version cannot be tagged }
  JET_errDefaultValueTooBig   = -1524; { Default value exceeds maximum size }
  JET_errMultiValuedDuplicate   = -1525; { Duplicate detected on a unique multi-valued column }
  JET_errLVCorrupted     = -1526; { Corruption encountered in long-value tree }
  JET_errMultiValuedDuplicateAfterTruncation = -1528; { Duplicate detected on a unique multi-valued column after data was normalized, and normalizing truncated the data before comparison }
  JET_errDerivedColumnCorruption  = -1529; { Invalid column in derived table }
  JET_errInvalidPlaceholderColumn  = -1530; { Tried to convert column to a primary index placeholder, but column doesn't meet necessary criteria }
  JET_wrnColumnSkipped    =  1531; { Column value(s) not returned because the corresponding column id or itagSequence requested for enumeration was null }
  JET_wrnColumnNotLocal    =  1532; { Column value(s) not returned because they could not be reconstructed from the data at hand }
  JET_wrnColumnMoreTags    =  1533; { Column values exist that were not requested for enumeration }
  JET_wrnColumnTruncated    =  1534; { Column value truncated at the requested size limit during enumeration }
  JET_wrnColumnPresent    =  1535; { Column values exist but were not returned by request }
  JET_wrnColumnSingleValue   =  1536; { Column value returned in JET_COLUMNENUM as a result of JET_bitEnumerateCompressOutput }
  JET_wrnColumnDefault    =  1537; { Column value(s) not returned because they were set to their default value(s) and JET_bitEnumerateIgnoreDefault was specified }
  JET_errColumnCannotBeCompressed  = -1538; { Only JET_coltypLongText and JET_coltypLongBinary columns can be compressed }
  JET_wrnColumnNotInRecord   =  1539; { Column value(s) not returned because they could not be reconstructed from the data in the record }

  JET_errRecordNotFound    = -1601; { The key was not found }
  JET_errRecordNoCopy     = -1602; { No working buffer }
  JET_errNoCurrentRecord    = -1603; { Currency not on a record }
  JET_errRecordPrimaryChanged   = -1604; { Primary key may not change }
  JET_errKeyDuplicate     = -1605; { Illegal duplicate key }
  JET_errAlreadyPrepared    = -1607; { Attempted to update record when record update was already in progress }
  JET_errKeyNotMade     = -1608; { No call to JetMakeKey }
  JET_errUpdateNotPrepared   = -1609; { No call to JetPrepareUpdate }
  JET_wrnDataHasChanged     =  1610; { Data has changed }
  JET_errDataHasChanged    = -1611; { Data has changed, operation aborted }
  JET_wrnKeyChanged      =  1618; { Moved to new key }
  JET_errLanguageNotSupported   = -1619; { Windows installation does not support language }
  JET_errDecompressionFailed   = -1620; { Internal error: data could not be decompressed }

  { Sort=  Table errors
  {}
  JET_errTooManySorts     = -1701; { Too many sort processes }
  JET_errInvalidOnSort    = -1702; { Invalid operation on Sort }

  { Other errors
  {}
  JET_errTempFileOpenError   = -1803; { Temp file could not be opened }
  JET_errTooManyAttachedDatabases  = -1805; { Too many open databases }
  JET_errDiskFull      = -1808; { No space left on disk }
  JET_errPermissionDenied    = -1809; { Permission denied }
  JET_errFileNotFound     = -1811; { File not found }
  JET_errFileInvalidType    = -1812; { Invalid file type }
  JET_wrnFileOpenReadOnly    =  1813; { Database file is read only }

  JET_errAfterInitialization   = -1850; { Cannot Restore after init. }
  JET_errLogCorrupted     = -1852; { Logs could not be interpreted }

  JET_errInvalidOperation    = -1906; { Invalid operation }
  JET_errAccessDenied     = -1907; { Access denied }
  JET_wrnIdleFull      =  1908; { Idle registry full }
  JET_errTooManySplits    = -1909; { Infinite split }
  JET_errSessionSharingViolation  = -1910; { Multiple threads are using the same session }
  JET_errEntryPointNotFound   = -1911; { An entry point in a DLL we require could not be found }
  JET_errSessionContextAlreadySet  = -1912; { Specified session already has a session context set }
  JET_errSessionContextNotSetByThisThread = -1913; { Tried to reset session context, but current thread did not orignally set the session context }
  JET_errSessionInUse     = -1914; { Tried to terminate session in use }

  JET_errRecordFormatConversionFailed = -1915; { Internal error during dynamic record format conversion }
  JET_errOneDatabasePerSession  = -1916; { Just one open user database per session is allowed (JET_paramOneDatabasePerSession) }
  JET_errRollbackError    = -1917; { error during rollback }

  JET_wrnDefragAlreadyRunning   =  2000; { Online defrag already running on specified database }
  JET_wrnDefragNotRunning    =  2001; { Online defrag not running on specified database }


  JET_wrnCallbackNotRegistered        =  2100; { Unregistered a non-existant callback function }
  JET_errCallbackFailed    = -2101; { A callback failed }
  JET_errCallbackNotResolved   = -2102; { A callback function could not be found }

  JET_errSpaceHintsInvalid   = -2103; { An element of the JET space hints structure was not correct or actionable. }


  JET_errOSSnapshotInvalidSequence = -2401; { OS Shadow copy API used in an invalid sequence }
  JET_errOSSnapshotTimeOut   = -2402; { OS Shadow copy ended with time-out }
  JET_errOSSnapshotNotAllowed   = -2403; { OS Shadow copy not allowed (backup or recovery in progress) }
  JET_errOSSnapshotInvalidSnapId  = -2404; { invalid JET_OSSNAPID }

  JET_errLSCallbackNotSpecified  = -3000; { Attempted to use Local Storage without a callback function being specified }
  JET_errLSAlreadySet     = -3001; { Attempted to set Local Storage for an object which already had it set }
  JET_errLSNotSet      = -3002; { Attempted to retrieve Local Storage from an object which didn't have it set }

  {* FILE and DISK ERRORS
 *}
  //JET_errFileAccessDenied     -1032
  //JET_errFileNotFound      -1811
  //JET_errInvalidFilename     -1044
  JET_errFileIOSparse     = -4000; { an I/O was issued to a location that was sparse }
  JET_errFileIOBeyondEOF    = -4001; { a read was issued to a location beyond EOF (writes will expand the file) }
  JET_errFileIOAbort     = -4002; { instructs the JET_ABORTRETRYFAILCALLBACK caller to abort the specified I/O }
  JET_errFileIORetry     = -4003; { instructs the JET_ABORTRETRYFAILCALLBACK caller to retry the specified I/O }
  JET_errFileIOFail     = -4004; { instructs the JET_ABORTRETRYFAILCALLBACK caller to fail the specified I/O }
  JET_errFileCompressed    = -4005; { read/write access is not supported on compressed files }

  // further RECORD MANAGER errors
  JET_wrnRecordFoundGreater   = JET_wrnSeekNotEqual;
  JET_wrnRecordFoundLess      = JET_wrnSeekNotEqual;
  JET_errColumnIllegalNull    = JET_errNullInvalid;


{********************************************************************}
{***************************** PROTOTYPES ***************************}
{********************************************************************}

{$IFDEF DYNAMIC_LINK}

function JetGetDatabaseFileInfo(szDatabaseName: JET_PCSTR;
                                pvResult: Pointer;       // _bcount( cbMax )
                                cbMax: Cardinal;
                                InfoLevel: Cardinal
                                  ): JET_ERR;  stdcall;

function JetInit(pinstance: PJET_INSTANCE): JET_ERR;  stdcall;

function JetCreateInstance(out instance: JET_INSTANCE;
                           szInstanceName: JET_PCSTR
                           ): JET_ERR;  stdcall;

function JetTerm2(instance: JET_INSTANCE;
                  grbit: JET_GRBIT
                ): JET_ERR;  stdcall;

function JetSetSystemParameter(pinstance: PJET_INSTANCE;
                               sesid    : JET_SESID;
                               paramid  : Cardinal;
                               lParam   : Pointer;
                               szParam  : JET_PCSTR
                               ): JET_ERR;  stdcall;

function JetBeginSession(instance  : JET_INSTANCE;
                         out psesid: JET_SESID;
                         szUserName: JET_PCSTR;
                         szPassword: JET_PCSTR
                         ): JET_ERR;  stdcall;

function JetGetVersion(sesid: JET_SESID;
                       out wVersion: Cardinal
                      ): JET_ERR;  stdcall;

function JetAttachDatabase(sesid: JET_SESID;
                           szFilename: JET_PCSTR;
                           grbit: JET_GRBIT
                             ): JET_ERR;  stdcall;

function JetOpenDatabase(sesid: JET_SESID;
                         szFilename: JET_PCSTR;
                         szConnect: JET_PCSTR;
                         out dbid: JET_DBID;
                         grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;

function JetOpenTable(sesid: JET_SESID;
                      dbid: JET_DBID;
                      szTableName: JET_PCSTR;
                      pvParameters: Pointer;       // _bcount( cbParameters )
                      cbParameters: Cardinal;
                      grbit: JET_GRBIT;
                      out tableid: JET_TABLEID
                        ): JET_ERR;  stdcall;

function JetMove(sesid: JET_SESID;
                 tableid: JET_TABLEID;
                 cRow: long;
                 grbit: JET_GRBIT
                  ): JET_ERR;  stdcall;

function JetGetTableColumnInfo(sesid: JET_SESID;
                               tableid: JET_TABLEID;
                               szColumnName: JET_PCSTR;
                               pvResult: Pointer;       // _bcount( cbMax )
                               cbMax: Cardinal;
                               InfoLevel: Cardinal
                               ): JET_ERR;  stdcall;

function JetRetrieveColumns(sesid: JET_SESID;
                            tableid: JET_TABLEID;
                            pretrievecolumn: PJET_RETRIEVECOLUMN;  // _ecount( cretrievecolumn )
                            cretrievecolumn: Cardinal
                             ): JET_ERR;  stdcall;

function JetCloseTable(sesid: JET_SESID;
                       tableid: JET_TABLEID
                        ): JET_ERR;  stdcall;

function JetCloseDatabase(sesid: JET_SESID;
                          dbid: JET_DBID;
                          grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;

function JetDetachDatabase(sesid: JET_SESID;
                           szFilename: JET_PCSTR
                           ): JET_ERR;  stdcall;

function JetEndSession(sesid: JET_SESID;
                       grbit: JET_GRBIT
                      ): JET_ERR;  stdcall;

{$ELSE}

function JetInit(pinstance: PJET_INSTANCE): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0501}
function JetInit2(pinstance: PJET_INSTANCE;
                  grbit: JET_GRBIT
                ): JET_ERR;  stdcall;

{$IFEND}


{$IF JET_VERSION >= $0600}
{$IF JET_VERSION < $0600}
JetInit3A = JetInit3;
{$IFEND}

function JetInit3A(pinstance: PJET_INSTANCE;
                   prstInfo: PJET_RSTINFO_A;
                   grbit: JET_GRBIT
                   ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetInit3W(pinstance: PJET_INSTANCE;
                   prstInfo: PJET_RSTINFO_W;
                   grbit: JET_GRBIT
                  ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetInit3(pinstance: PJET_INSTANCE;
                  prstInfo: PJET_RSTINFO_W;
                  grbit: JET_GRBIT
                  ): JET_ERR;  stdcall;
{$ELSE}
function JetInit3(pinstance: PJET_INSTANCE;
                  prstInfo: PJET_RSTINFO_A;
                  grbit: JET_GRBIT
                   ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IFEND}

{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetCreateInstance(out instance: JET_INSTANCE;
                           szInstanceName: JET_PCSTR
                           ): JET_ERR;  stdcall;
{$IFEND}

function JetCreateInstanceA(out instance: JET_INSTANCE;
                            szInstanceName: JET_PCSTR
                           ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetCreateInstanceW(out instance: JET_INSTANCE;
                            szInstanceName: JET_PCWSTR
                           ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetCreateInstance(out instance: JET_INSTANCE;
                           szInstanceName: JET_PCWSTR
                           ): JET_ERR;  stdcall;
{$ELSE}
function JetCreateInstance(out instance: JET_INSTANCE;
                           szInstanceName: JET_PCSTR
                           ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600}
function JetCreateInstance2(out instance  : JET_INSTANCE;
                            szInstanceName: JET_PCSTR;
                            szDisplayName : JET_PCSTR;
                            grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;
{$IFEND}

function JetCreateInstance2A(out instance  : JET_INSTANCE;
                             szInstanceName: JET_PCSTR;
                             szDisplayName : JET_PCSTR;
                             grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetCreateInstance2W(out instance  : JET_INSTANCE;
                             szInstanceName: JET_PCWSTR;
                             szDisplayName : JET_PCWSTR;
                             grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetCreateInstance2(out instance  : JET_INSTANCE;
                            szInstanceName: JET_PCWSTR;
                            szDisplayName : JET_PCWSTR;
                            grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;
{$ELSE}
function JetCreateInstance2(out instance  : JET_INSTANCE;
                            szInstanceName: JET_PCSTR;
                            szDisplayName : JET_PCSTR;
                            grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IFEND}

{$IF JET_VERSION >= $0600}

function JetGetInstanceMiscInfo(instance: JET_INSTANCE;
                                pvResult: Pointer;
                                cbMax: Cardinal;  // bytes
                                InfoLevel: Cardinal
                               ): JET_ERR;  stdcall;

{$IFEND}

function JetTerm(instance: JET_INSTANCE): JET_ERR;  stdcall;

function JetTerm2(instance: JET_INSTANCE;
                  grbit: JET_GRBIT
                ): JET_ERR;  stdcall;

function JetStopService(): JET_ERR;  stdcall;
{$IF JET_VERSION >= $0501}
function JetStopServiceInstance(instance: JET_INSTANCE): JET_ERR;  stdcall;
{$IFEND}

function JetStopBackup(): JET_ERR;  stdcall;
{$IF JET_VERSION >= $0501}
function JetStopBackupInstance(instance: JET_INSTANCE): JET_ERR;  stdcall;
{$IFEND}

{$IF JET_VERSION < $0600}
function JetSetSystemParameter(pinstance: PJET_INSTANCE;
                               sesid    : JET_SESID;
                               paramid  : Cardinal;
                               lParam   : Pointer;
                               szParam  : JET_PCSTR
                               ): JET_ERR;  stdcall;
{$IFEND}

function JetSetSystemParameterA(pinstance: PJET_INSTANCE;
                                sesid    : JET_SESID;
                                paramid  : Cardinal;
                                lParam   : Pointer;
                                szParam  : JET_PCSTR
                               ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetSetSystemParameterW(pinstance: PJET_INSTANCE;
                                sesid    : JET_SESID;
                                paramid  : Cardinal;
                                lParam   : Pointer;
                                szParam  : JET_PCWSTR
                               ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetSetSystemParameter(pinstance: PJET_INSTANCE;
                               sesid    : JET_SESID;
                               paramid  : Cardinal;
                               lParam   : Pointer;
                               szParam  : JET_PCWSTR
                               ): JET_ERR;  stdcall;
{$ELSE}
function JetSetSystemParameter(pinstance: PJET_INSTANCE;
                               sesid    : JET_SESID;
                               paramid  : Cardinal;
                               lParam   : Pointer;
                               szParam  : JET_PCSTR
                               ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600}
function JetGetSystemParameter(instance: JET_INSTANCE;
                               sesid   : JET_SESID;
                               paramid : Cardinal;
                               plParam: PtrPointer;
                               szParam: JET_PSTR;       // _bcount( cbMax )
                               cbMax: Cardinal
                                ): JET_ERR;  stdcall;
{$IFEND}

function JetGetSystemParameterA(instance: JET_INSTANCE;
                                sesid   : JET_SESID;
                                paramid : Cardinal;
                                plParam: PtrPointer;
                                szParam: JET_PSTR;       // _bcount( cbMax )
                                cbMax: Cardinal
                                ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetGetSystemParameterW(instance: JET_INSTANCE;
                                sesid   : JET_SESID;
                                paramid : Cardinal;
                                plParam : PtrPointer;
                                szParam : JET_PWSTR;       // _bcount( cbMax )
                                cbMax: Cardinal
                                ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetGetSystemParameter(instance: JET_INSTANCE;
                               sesid   : JET_SESID;
                               paramid : Cardinal;
                               plParam : PtrPointer;
                               szParam : JET_PWSTR;       // _bcount( cbMax )
                               cbMax: Cardinal
                                ): JET_ERR;  stdcall;
{$ELSE}
function JetGetSystemParameter(instance: JET_INSTANCE;
                               sesid   : JET_SESID;
                               paramid : Cardinal;
                               plParam: PtrPointer;
                               szParam: JET_PSTR;       // _bcount( cbMax )
                               cbMax: Cardinal
                                ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}

{$IF JET_VERSION < $0600}
function JetEnableMultiInstance(psetsysparam: PJET_SETSYSPARAM_A;       // __ecount( csetsysparam )
                                csetsysparam: Cardinal;
                                pcsetsucceed: PCardinal
                                ): JET_ERR;  stdcall;
{$IFEND}

function JetEnableMultiInstanceA(psetsysparam: PJET_SETSYSPARAM_A;       // __ecount( csetsysparam )
                                 csetsysparam: Cardinal;
                                 pcsetsucceed: PCardinal
                                ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetEnableMultiInstanceW(psetsysparam: PJET_SETSYSPARAM_W;      // __ecount( csetsysparam )
                                 csetsysparam: Cardinal;
                                 pcsetsucceed: PCardinal
                                ): JET_ERR;  stdcall;


{$IFDEF JET_UNICODE}
function JetEnableMultiInstance(psetsysparam: PJET_SETSYSPARAM_W;      // __ecount( csetsysparam )
                                csetsysparam: Cardinal;
                                pcsetsucceed: PCardinal
                                ): JET_ERR;  stdcall;
{$ELSE}
function JetEnableMultiInstance(psetsysparam: PJET_SETSYSPARAM_A;       // __ecount( csetsysparam )
                                csetsysparam: Cardinal;
                                pcsetsucceed: PCardinal
                                ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IFEND}


{$IF JET_VERSION >= $0600}
function JetGetThreadStats(pvResult: Pointer;       // _bcount( cbMax )
                           cbMax: Cardinal
                          ): JET_ERR;  stdcall;
{$IFEND}

{$IF JET_VERSION < $0600}
function JetBeginSession(instance  : JET_INSTANCE;
                         out psesid: JET_SESID;
                         szUserName: JET_PCSTR;
                         szPassword: JET_PCSTR
                         ): JET_ERR;  stdcall;
{$IFEND}

function JetBeginSessionA(instance  : JET_INSTANCE;
                          out psesid: JET_SESID;
                          szUserName: JET_PCSTR;
                          szPassword: JET_PCSTR
                         ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetBeginSessionW(instance  : JET_INSTANCE;
                          out psesid: JET_SESID;
                          szUserName: JET_PCWSTR;
                          szPassword: JET_PCWSTR
                         ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetBeginSession(instance  : JET_INSTANCE;
                         out psesid: JET_SESID;
                         szUserName: JET_PCWSTR;
                         szPassword: JET_PCWSTR
                         ): JET_ERR;  stdcall;
{$ELSE}
function JetBeginSession(instance  : JET_INSTANCE;
                         out psesid: JET_SESID;
                         szUserName: JET_PCSTR;
                         szPassword: JET_PCSTR
                         ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

function JetDupSession(sesid: JET_SESID;
                       out psesid: JET_SESID
                      ): JET_ERR;  stdcall;

function JetEndSession(sesid: JET_SESID;
                       grbit: JET_GRBIT
                      ): JET_ERR;  stdcall;

function JetGetVersion(sesid: JET_SESID;
                       out wVersion: Cardinal
                      ): JET_ERR;  stdcall;

function JetIdle(sesid: JET_SESID;
                 grbit: JET_GRBIT
                ): JET_ERR;  stdcall;

{$IF JET_VERSION < $0600}
function JetCreateDatabase(sesid     : JET_SESID;
                           szFilename: JET_PCSTR;
                           szConnect : JET_PCSTR;
                           out dbid  : JET_DBID;
                           grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;
{$IFEND}

function JetCreateDatabaseA(sesid     : JET_SESID;
                            szFilename: JET_PCSTR;
                            szConnect : JET_PCSTR;
                            out dbid  : JET_DBID;
                            grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetCreateDatabaseW(sesid     : JET_SESID;
                            szFilename: JET_PCWSTR;
                            szConnect : JET_PCWSTR;
                            out dbid  : JET_DBID;
                            grbit     : JET_GRBIT
                           ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetCreateDatabase(sesid     : JET_SESID;
                           szFilename: JET_PCWSTR;
                           szConnect : JET_PCWSTR;
                           out dbid  : JET_DBID;
                           grbit     : JET_GRBIT
                           ): JET_ERR;  stdcall;
{$ELSE}
function JetCreateDatabase(sesid     : JET_SESID;
                           szFilename: JET_PCSTR;
                           szConnect : JET_PCSTR;
                           out dbid  : JET_DBID;
                           grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetCreateDatabase2(sesid: JET_SESID;
                            szFilename: JET_PCSTR;
                            cpgDatabaseSizeMax: Cardinal;
                            out dbid: JET_DBID;
                            grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;
{$IFEND}

function JetCreateDatabase2A(sesid: JET_SESID;
                             szFilename: JET_PCSTR;
                             cpgDatabaseSizeMax: Cardinal;
                             out dbid: JET_DBID;
                             grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetCreateDatabase2W(sesid: JET_SESID;
                             szFilename: JET_PCWSTR    ;
                             cpgDatabaseSizeMax: Cardinal;
                             out dbid: JET_DBID;
                             grbit: JET_GRBIT
                             ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetCreateDatabase2(sesid: JET_SESID;
                            szFilename: JET_PCWSTR    ;
                            cpgDatabaseSizeMax: Cardinal;
                            out dbid: JET_DBID;
                            grbit: JET_GRBIT
                             ): JET_ERR;  stdcall;
{$ELSE}
function JetCreateDatabase2(sesid: JET_SESID;
                            szFilename: JET_PCSTR;
                            cpgDatabaseSizeMax: Cardinal;
                            out dbid: JET_DBID;
                            grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600}
function JetAttachDatabase(sesid: JET_SESID;
                           szFilename: JET_PCSTR;
                           grbit: JET_GRBIT
                             ): JET_ERR;  stdcall;
{$IFEND}

function JetAttachDatabaseA(sesid: JET_SESID;
                            szFilename: JET_PCSTR;
                            grbit: JET_GRBIT
                             ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetAttachDatabaseW(sesid: JET_SESID;
                            szFilename: JET_PCWSTR;
                            grbit: JET_GRBIT
                             ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetAttachDatabase(sesid: JET_SESID;
                           szFilename: JET_PCWSTR;
                           grbit: JET_GRBIT
                             ): JET_ERR;  stdcall;
{$ELSE}
function JetAttachDatabase(sesid: JET_SESID;
                           szFilename: JET_PCSTR;
                           grbit: JET_GRBIT
                             ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600}
function JetAttachDatabase2(sesid: JET_SESID;
                            szFilename: JET_PCSTR;
                            cpgDatabaseSizeMax: Cardinal;
                            grbit: JET_GRBIT
                              ): JET_ERR;  stdcall;
{$IFEND}

function JetAttachDatabase2A(sesid: JET_SESID;
                             szFilename: JET_PCSTR;
                             cpgDatabaseSizeMax: Cardinal;
                             grbit: JET_GRBIT
                              ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetAttachDatabase2W(sesid: JET_SESID;
                             szFilename: JET_PCWSTR;
                             cpgDatabaseSizeMax: Cardinal;
                             grbit: JET_GRBIT
                              ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetAttachDatabase2(sesid: JET_SESID;
                            szFilename: JET_PCWSTR;
                            cpgDatabaseSizeMax: Cardinal;
                            grbit: JET_GRBIT
                              ): JET_ERR;  stdcall;
{$ELSE}
function JetAttachDatabase2(sesid: JET_SESID;
                            szFilename: JET_PCSTR;
                            cpgDatabaseSizeMax: Cardinal;
                            grbit: JET_GRBIT
                              ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600}
function JetDetachDatabase(sesid: JET_SESID;
                           szFilename: JET_PCSTR
                           ): JET_ERR;  stdcall;
{$IFEND}

function JetDetachDatabaseA(sesid: JET_SESID;
                            szFilename: JET_PCSTR
                           ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetDetachDatabaseW(sesid: JET_SESID;
                            szFilename: JET_PCWSTR
                           ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetDetachDatabase(sesid: JET_SESID;
                           szFilename: JET_PCWSTR
                           ): JET_ERR;  stdcall;
{$ELSE}
function JetDetachDatabase(sesid: JET_SESID;
                           szFilename: JET_PCSTR
                           ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetDetachDatabase2(sesid: JET_SESID;
                            szFilename: JET_PCSTR;
                            grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;
{$IFEND}

function JetDetachDatabase2A(sesid: JET_SESID;
                             szFilename: JET_PCSTR;
                             grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetDetachDatabase2W(sesid: JET_SESID;
                             szFilename: JET_PCWSTR;
                             grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetDetachDatabase2(sesid: JET_SESID;
                            szFilename: JET_PCWSTR;
                            grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;
{$ELSE}
function JetDetachDatabase2(sesid: JET_SESID;
                            szFilename: JET_PCSTR;
                            grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IFEND}


{$IF JET_VERSION < $0600}
function JetGetObjectInfo(sesid          : JET_SESID;
                          dbid           : JET_DBID;
                          objtyp         : JET_OBJTYP;
                          szContainerName: JET_PCSTR;
                          szObjectName   : JET_PCSTR;
                          pvResult       : Pointer;       // _bcount( cbMax )
                          cbMax          : Cardinal;
                          InfoLevel      : Cardinal
                          ): JET_ERR;  stdcall;
{$IFEND}

function JetGetObjectInfoA(sesid          : JET_SESID;
                           dbid           : JET_DBID;
                           objtyp         : JET_OBJTYP;
                           szContainerName: JET_PCSTR;
                           szObjectName   : JET_PCSTR;
                           pvResult       : Pointer;       // _bcount( cbMax )
                           cbMax          : Cardinal;
                           InfoLevel      : Cardinal
                          ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetGetObjectInfoW(sesid          : JET_SESID;
                           dbid           : JET_DBID;
                           objtyp         : JET_OBJTYP;
                           szContainerName: JET_PCWSTR;
                           szObjectName   : JET_PCWSTR;
                           pvResult       : Pointer;       // _bcount( cbMax )
                           cbMax          : Cardinal;
                           InfoLevel      : Cardinal
                          ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetGetObjectInfo(sesid          : JET_SESID;
                          dbid           : JET_DBID;
                          objtyp         : JET_OBJTYP;
                          szContainerName: JET_PCWSTR;
                          szObjectName   : JET_PCWSTR;
                          pvResult       : Pointer;       // _bcount( cbMax )
                          cbMax          : Cardinal;
                          InfoLevel      : Cardinal
                          ): JET_ERR;  stdcall;
{$ELSE}
function JetGetObjectInfo(sesid          : JET_SESID;
                          dbid           : JET_DBID;
                          objtyp         : JET_OBJTYP;
                          szContainerName: JET_PCSTR;
                          szObjectName   : JET_PCSTR;
                          pvResult       : Pointer;       // _bcount( cbMax )
                          cbMax          : Cardinal;
                          InfoLevel      : Cardinal
                          ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

function JetGetTableInfo(sesid    : JET_SESID;
                         tableid  : JET_TABLEID;
                         pvResult : Pointer;       // _bcount( cbMax )
                         cbMax    : Cardinal;
                         InfoLevel: Cardinal
                         ): JET_ERR;  stdcall;



{$IF JET_VERSION < $0600}
function JetCreateTable(sesid: JET_SESID;
                        dbid: JET_DBID;
                        szTableName: JET_PCSTR;
                        lPages  : Cardinal;
                        lDensity: Cardinal;
                        out tableid: JET_TABLEID
                        ): JET_ERR;  stdcall;
{$IFEND}

function JetCreateTableA(sesid: JET_SESID;
                         dbid: JET_DBID;
                         szTableName: JET_PCSTR;
                         lPages  : Cardinal;
                         lDensity: Cardinal;
                         out tableid: JET_TABLEID
                        ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetCreateTableW(sesid: JET_SESID;
                         dbid: JET_DBID;
                         szTableName: JET_PCWSTR;
                         lPages  : Cardinal;
                         lDensity: Cardinal;
                         out tableid: JET_TABLEID
                        ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetCreateTable(sesid: JET_SESID;
                        dbid: JET_DBID;
                        szTableName: JET_PCWSTR;
                        lPages  : Cardinal;
                        lDensity: Cardinal;
                        out tableid: JET_TABLEID
                        ): JET_ERR;  stdcall;
{$ELSE}
function JetCreateTable(sesid: JET_SESID;
                        dbid: JET_DBID;
                        szTableName: JET_PCSTR;
                        lPages  : Cardinal;
                        lDensity: Cardinal;
                        out tableid: JET_TABLEID
                        ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600}
function JetCreateTableColumnIndex(sesid: JET_SESID;
                                   dbid : JET_DBID;
                                   var tablecreate: JET_TABLECREATE_A
                                   ): JET_ERR;  stdcall;
{$IFEND}

function JetCreateTableColumnIndexA(sesid: JET_SESID;
                                    dbid : JET_DBID;
                                    var tablecreate: JET_TABLECREATE_A
                                   ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetCreateTableColumnIndexW(sesid: JET_SESID;
                                    dbid : JET_DBID;
                                    var tablecreate: JET_TABLECREATE_W
                                   ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetCreateTableColumnIndex(sesid: JET_SESID;
                                   dbid : JET_DBID;
                                   var tablecreate: JET_TABLECREATE_W
                                   ): JET_ERR;  stdcall;
{$ELSE}
function JetCreateTableColumnIndex(sesid: JET_SESID;
                                   dbid : JET_DBID;
                                   var tablecreate: JET_TABLECREATE_A
                                   ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetCreateTableColumnIndex2(sesid: JET_SESID;
                                    dbid: JET_DBID;
                                    var tablecreate: JET_TABLECREATE2_A
                                    ): JET_ERR;  stdcall;
{$IFEND}

function JetCreateTableColumnIndex2A(sesid: JET_SESID;
                                     dbid: JET_DBID;
                                     var tablecreate: JET_TABLECREATE2_A
                                    ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetCreateTableColumnIndex2W(sesid: JET_SESID;
                                     dbid: JET_DBID;
                                     var tablecreate: JET_TABLECREATE2_W
                                    ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetCreateTableColumnIndex2(sesid: JET_SESID;
                                    dbid: JET_DBID;
                                    var tablecreate: JET_TABLECREATE2_W
                                    ): JET_ERR;  stdcall;
{$ELSE}
function JetCreateTableColumnIndex2(sesid: JET_SESID;
                                    dbid: JET_DBID;
                                    var tablecreate: JET_TABLECREATE2_A
                                    ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}
{$IFEND}

{$IF JET_VERSION >= $0601}

function JetCreateTableColumnIndex3A(sesid: JET_SESID;
                                     dbid: JET_DBID;
                                     tablecreate: JET_TABLECREATE3_A
                                    ): JET_ERR;  stdcall;

function JetCreateTableColumnIndex3W(sesid: JET_SESID;
                                     dbid: JET_DBID;
                                     tablecreate: JET_TABLECREATE3_W
                                    ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetCreateTableColumnIndex3(sesid: JET_SESID;
                                    dbid: JET_DBID;
                                    tablecreate: JET_TABLECREATE3_W
                                    ): JET_ERR;  stdcall;
{$ELSE}
function JetCreateTableColumnIndex3(sesid: JET_SESID;
                                    dbid: JET_DBID;
                                    tablecreate: JET_TABLECREATE3_A
                                    ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetDeleteTable(sesid: JET_SESID;
                        dbid: JET_DBID;
                        szTableName: JET_PCSTR
                        ): JET_ERR;  stdcall;
{$IFEND}

function JetDeleteTableA(sesid: JET_SESID;
                         dbid: JET_DBID;
                         szTableName: JET_PCSTR
                        ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetDeleteTableW(sesid: JET_SESID;
                         dbid: JET_DBID;
                         szTableName: JET_PCWSTR
                        ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetDeleteTable(sesid: JET_SESID;
                        dbid: JET_DBID;
                        szTableName: JET_PCWSTR
                        ): JET_ERR;  stdcall;
{$ELSE}
function JetDeleteTable(sesid: JET_SESID;
                        dbid: JET_DBID;
                        szTableName: JET_PCSTR
                        ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetRenameTable(sesid: JET_SESID;
                        dbid: JET_DBID;
                        szName: JET_PCSTR;
                        szNameNew: JET_PCSTR
                        ): JET_ERR;  stdcall;
{$IFEND}

function JetRenameTableA(sesid: JET_SESID;
                         dbid: JET_DBID;
                         szName: JET_PCSTR;
                         szNameNew: JET_PCSTR
                        ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetRenameTableW(sesid: JET_SESID;
                         dbid: JET_DBID;
                         szName: JET_PCWSTR ;
                         szNameNew: JET_PCWSTR
                        ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetRenameTable(sesid: JET_SESID;
                        dbid: JET_DBID;
                        szName: JET_PCWSTR ;
                        szNameNew: JET_PCWSTR
                        ): JET_ERR;  stdcall;
{$ELSE}
function JetRenameTable(sesid: JET_SESID;
                        dbid: JET_DBID;
                        szName: JET_PCSTR;
                        szNameNew: JET_PCSTR
                        ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetGetTableColumnInfo(sesid: JET_SESID;
                               tableid: JET_TABLEID;
                               szColumnName: JET_PCSTR;
                               pvResult: Pointer;       // _bcount( cbMax )
                               cbMax: Cardinal;
                               InfoLevel: Cardinal
                               ): JET_ERR;  stdcall;
{$IFEND}

function JetGetTableColumnInfoA(sesid: JET_SESID;
                                tableid: JET_TABLEID;
                                szColumnName: JET_PCSTR;
                                pvResult: Pointer;       // _bcount( cbMax )
                                cbMax: Cardinal;
                                InfoLevel: Cardinal
                               ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetGetTableColumnInfoW(sesid: JET_SESID;
                                tableid: JET_TABLEID;
                                szColumnName: JET_PCWSTR;
                                pvResult: Pointer;       // _bcount( cbMax )
                                cbMax: Cardinal;
                                InfoLevel: Cardinal
                               ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetGetTableColumnInfo(sesid: JET_SESID;
                               tableid: JET_TABLEID;
                               szColumnName: JET_PCWSTR;
                               pvResult: Pointer;       // _bcount( cbMax )
                               cbMax: Cardinal;
                               InfoLevel: Cardinal
                               ): JET_ERR;  stdcall;
{$ELSE}
function JetGetTableColumnInfo(sesid: JET_SESID;
                               tableid: JET_TABLEID;
                               szColumnName: JET_PCSTR;
                               pvResult: Pointer;       // _bcount( cbMax )
                               cbMax: Cardinal;
                               InfoLevel: Cardinal
                               ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetGetColumnInfo(sesid: JET_SESID;
                          dbid: JET_DBID;
                          szTableName: JET_PCSTR;
                          szColumnName: JET_PCSTR;
                          pvResult: Pointer;       // _bcount( cbMax )
                          cbMax: Cardinal;
                          InfoLevel: Cardinal
                          ): JET_ERR;  stdcall;
{$IFEND}

function JetGetColumnInfoA(sesid: JET_SESID;
                           dbid: JET_DBID;
                           szTableName: JET_PCSTR;
                           szColumnName: JET_PCSTR;
                           pvResult: Pointer;       // _bcount( cbMax )
                           cbMax: Cardinal;
                           InfoLevel: Cardinal
                          ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetGetColumnInfoW(sesid: JET_SESID;
                           dbid: JET_DBID;
                           szTableName: JET_PCWSTR;
                           szColumnName: JET_PCWSTR;
                           pvResult: Pointer;       // _bcount( cbMax )
                           cbMax: Cardinal;
                           InfoLevel: Cardinal
                          ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetGetColumnInfo(sesid: JET_SESID;
                          dbid: JET_DBID;
                          szTableName: JET_PCWSTR;
                          szColumnName: JET_PCWSTR;
                          pvResult: Pointer;       // _bcount( cbMax )
                          cbMax: Cardinal;
                          InfoLevel: Cardinal
                          ): JET_ERR;  stdcall;
{$ELSE}
function JetGetColumnInfo(sesid: JET_SESID;
                          dbid: JET_DBID;
                          szTableName: JET_PCSTR;
                          szColumnName: JET_PCSTR;
                          pvResult: Pointer;       // _bcount( cbMax )
                          cbMax: Cardinal;
                          InfoLevel: Cardinal
                          ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetAddColumn(sesid: JET_SESID;
                      tableid: JET_TABLEID;
                      szColumnName: JET_PCSTR;
                      const columndef: JET_COLUMNDEF;
                      pvDefault: Pointer;       // _bcount( cbDefault )
                      cbDefault: Cardinal;
                      pcolumnid: PJET_COLUMNID
                        ): JET_ERR;  stdcall;
{$IFEND}

function JetAddColumnA(sesid: JET_SESID;
                       tableid: JET_TABLEID;
                       szColumnName: JET_PCSTR;
                       const columndef: JET_COLUMNDEF;
                       pvDefault: Pointer;       // _bcount( cbDefault )
                       cbDefault: Cardinal;
                       pcolumnid: PJET_COLUMNID
                        ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetAddColumnW(sesid: JET_SESID;
                       tableid: JET_TABLEID;
                       szColumnName: JET_PCWSTR;
                       const columndef: JET_COLUMNDEF;
                       pvDefault: Pointer;       // _bcount( cbDefault )
                       cbDefault: Cardinal;
                       pcolumnid: PJET_COLUMNID
                        ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetAddColumn(sesid: JET_SESID;
                      tableid: JET_TABLEID;
                      szColumnName: JET_PCWSTR;
                      const columndef: JET_COLUMNDEF;
                      pvDefault: Pointer;       // _bcount( cbDefault )
                      cbDefault: Cardinal;
                      pcolumnid: PJET_COLUMNID
                        ): JET_ERR;  stdcall;
{$ELSE}
function JetAddColumn(sesid: JET_SESID;
                      tableid: JET_TABLEID;
                      szColumnName: JET_PCSTR;
                      const columndef: JET_COLUMNDEF;
                      pvDefault: Pointer;       // _bcount( cbDefault )
                      cbDefault: Cardinal;
                      pcolumnid: PJET_COLUMNID
                        ): JET_ERR;  stdcall;

{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600}
function JetDeleteColumn(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         szColumnName: JET_PCSTR
                           ): JET_ERR;  stdcall;
{$IFEND}

function JetDeleteColumnA(sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          szColumnName: JET_PCSTR
                           ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetDeleteColumnW(sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          szColumnName: JET_PCWSTR
                           ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetDeleteColumn(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         szColumnName: JET_PCWSTR
                           ): JET_ERR;  stdcall;
{$ELSE}
function JetDeleteColumn(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         szColumnName: JET_PCSTR
                           ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetDeleteColumn2(sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          szColumnName: JET_PCSTR;
                          grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;
{$IFEND}

function JetDeleteColumn2A(sesid: JET_SESID;
                           tableid: JET_TABLEID;
                           szColumnName: JET_PCSTR;
                           grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetDeleteColumn2W(sesid: JET_SESID;
                           tableid: JET_TABLEID;
                           szColumnName: JET_PCWSTR;
                           grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetDeleteColumn2(sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          szColumnName: JET_PCWSTR;
                          grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;
{$ELSE}
function JetDeleteColumn2(sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          szColumnName: JET_PCSTR;
                          grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetRenameColumn(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         szName: JET_PCSTR;
                         szNameNew: JET_PCSTR;
                         grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;
{$IFEND}

function JetRenameColumnA(sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          szName: JET_PCSTR;
                          szNameNew: JET_PCSTR;
                          grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetRenameColumnW(sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          szName: JET_PCWSTR;
                          szNameNew: JET_PCWSTR;
                          grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetRenameColumn(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         szName: JET_PCWSTR;
                         szNameNew: JET_PCWSTR;
                         grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;
{$ELSE}
function JetRenameColumn(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         szName: JET_PCSTR;
                         szNameNew: JET_PCSTR;
                         grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IFEND}


{$IF JET_VERSION < $0600}
function JetSetColumnDefaultValue(sesid: JET_SESID;
                                  dbid: JET_DBID;
                                  szTableName: JET_PCSTR;
                                  szColumnName: JET_PCSTR;
                                  pvData: Pointer;       // _bcount( cbData )
                                  cbData: Cardinal;
                                  grbit: JET_GRBIT
                                    ): JET_ERR;  stdcall;
{$IFEND}

function JetSetColumnDefaultValueA(sesid: JET_SESID;
                                   dbid: JET_DBID;
                                   szTableName: JET_PCSTR;
                                   szColumnName: JET_PCSTR;
                                   pvData: Pointer;       // _bcount( cbData )
                                   cbData: Cardinal;
                                   grbit: JET_GRBIT
                                    ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetSetColumnDefaultValueW(sesid: JET_SESID;
                                   dbid: JET_DBID;
                                   szTableName: JET_PCWSTR;
                                   szColumnName: JET_PCWSTR;
                                   pvData: Pointer;       // _bcount( cbData )
                                   cbData: Cardinal;
                                   grbit: JET_GRBIT
                                    ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetSetColumnDefaultValue(sesid: JET_SESID;
                                  dbid: JET_DBID;
                                  szTableName: JET_PCWSTR;
                                  szColumnName: JET_PCWSTR;
                                  pvData: Pointer;       // _bcount( cbData )
                                  cbData: Cardinal;
                                  grbit: JET_GRBIT
                                    ): JET_ERR;  stdcall;
{$ELSE}
function JetSetColumnDefaultValue(sesid: JET_SESID;
                                  dbid: JET_DBID;
                                  szTableName: JET_PCSTR;
                                  szColumnName: JET_PCSTR;
                                  pvData: Pointer;       // _bcount( cbData )
                                  cbData: Cardinal;
                                  grbit: JET_GRBIT
                                    ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetGetTableIndexInfo(sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCSTR;
                              pvResult: Pointer;       // _bcount( cbResult )
                              cbResult: Cardinal;
                              InfoLevel: Cardinal
                                ): JET_ERR;  stdcall;
{$IFEND}

function JetGetTableIndexInfoA(sesid: JET_SESID;
                               tableid: JET_TABLEID;
                               szIndexName: JET_PCSTR;
                               pvResult: Pointer;       // _bcount( cbResult )
                               cbResult: Cardinal;
                               InfoLevel: Cardinal
                                ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetGetTableIndexInfoW(sesid: JET_SESID;
                               tableid: JET_TABLEID;
                               szIndexName: JET_PCWSTR;
                               pvResult: Pointer;       // _bcount( cbResult )
                               cbResult: Cardinal;
                               InfoLevel: Cardinal
                                ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetGetTableIndexInfo(sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCWSTR;
                              pvResult: Pointer;       // _bcount( cbResult )
                              cbResult: Cardinal;
                              InfoLevel: Cardinal
                                ): JET_ERR;  stdcall;
{$ELSE}
function JetGetTableIndexInfo(sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCSTR;
                              pvResult: Pointer;       // _bcount( cbResult )
                              cbResult: Cardinal;
                              InfoLevel: Cardinal
                                ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetGetIndexInfo(sesid: JET_SESID;
                         dbid: JET_DBID;
                         szTableName: JET_PCSTR;
                         szIndexName: JET_PCSTR;
                         pvResult: Pointer;       // _bcount( cbResult )
                         cbResult: Cardinal;
                         InfoLevel: Cardinal
                           ): JET_ERR;  stdcall;
{$IFEND}

function JetGetIndexInfoA(sesid: JET_SESID;
                          dbid: JET_DBID;
                          szTableName: JET_PCSTR;
                          szIndexName: JET_PCSTR;
                          pvResult: Pointer;       // _bcount( cbResult )
                          cbResult: Cardinal;
                          InfoLevel: Cardinal
                           ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetGetIndexInfoW(sesid: JET_SESID;
                          dbid: JET_DBID;
                          szTableName: JET_PCWSTR;
                          szIndexName: JET_PCWSTR;
                          pvResult: Pointer;       // _bcount( cbResult )
                          cbResult: Cardinal;
                          InfoLevel: Cardinal
                           ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetGetIndexInfo(sesid: JET_SESID;
                         dbid: JET_DBID;
                         szTableName: JET_PCWSTR;
                         szIndexName: JET_PCWSTR;
                         pvResult: Pointer;       // _bcount( cbResult )
                         cbResult: Cardinal;
                         InfoLevel: Cardinal
                           ): JET_ERR;  stdcall;
{$ELSE}
function JetGetIndexInfo(sesid: JET_SESID;
                         dbid: JET_DBID;
                         szTableName: JET_PCSTR;
                         szIndexName: JET_PCSTR;
                         pvResult: Pointer;       // _bcount( cbResult )
                         cbResult: Cardinal;
                         InfoLevel: Cardinal
                           ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetCreateIndex(sesid: JET_SESID;
                        tableid: JET_TABLEID;
                        szIndexName: JET_PCSTR;
                        grbit: JET_GRBIT;
                        szKey: PAnsiChar;       // _bcount( cbKey )
                        cbKey: Cardinal;
                        lDensity: Cardinal
                          ): JET_ERR;  stdcall;
{$IFEND}

function JetCreateIndexA(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         szIndexName: JET_PCSTR;
                         grbit: JET_GRBIT;
                         szKey: PAnsiChar;       // _bcount( cbKey )
                         cbKey: Cardinal;
                         lDensity: Cardinal
                          ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetCreateIndexW(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         szIndexName: JET_PCWSTR;
                         grbit: JET_GRBIT;
                         szKey: PWideChar;       // _bcount( cbKey )
                         cbKey: Cardinal;
                         lDensity: Cardinal
                          ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetCreateIndex(sesid: JET_SESID;
                        tableid: JET_TABLEID;
                        szIndexName: JET_PCWSTR;
                        grbit: JET_GRBIT;
                        szKey: PWideChar;       // _bcount( cbKey )
                        cbKey: Cardinal;
                        lDensity: Cardinal
                          ): JET_ERR;  stdcall;
{$ELSE}
function JetCreateIndex(sesid: JET_SESID;
                        tableid: JET_TABLEID;
                        szIndexName: JET_PCSTR;
                        grbit: JET_GRBIT;
                        szKey: PAnsiChar;       // _bcount( cbKey )
                        cbKey: Cardinal;
                        lDensity: Cardinal
                          ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetCreateIndex2(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         pindexcreate: PJET_INDEXCREATE_A;     // __ecount( cIndexCreate )
                         cIndexCreate: Cardinal
                           ): JET_ERR;  stdcall;
{$IFEND}

function JetCreateIndex2A(sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          pindexcreate: PJET_INDEXCREATE_A;     // __ecount( cIndexCreate )
                          cIndexCreate: Cardinal
                           ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetCreateIndex2W(sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          pindexcreate: PJET_INDEXCREATE_W;     // __ecount( cIndexCreate )
                          cIndexCreate: Cardinal
                           ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetCreateIndex2(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         pindexcreate: PJET_INDEXCREATE_W;     // __ecount( cIndexCreate )
                         cIndexCreate: Cardinal
                           ): JET_ERR;  stdcall;
{$ELSE}
function JetCreateIndex2(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         pindexcreate: PJET_INDEXCREATE_A;     // __ecount( cIndexCreate )
                         cIndexCreate: Cardinal
                           ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IF JET_VERSION >= $0601}

function JetCreateIndex3A(sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          pindexcreate: PJET_INDEXCREATE2_A;     // __ecount( cIndexCreate )
                          cIndexCreate: Cardinal
                           ): JET_ERR;  stdcall;

function JetCreateIndex3W(sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          pindexcreate :PJET_INDEXCREATE2_W;     // __ecount( cIndexCreate )
                          cIndexCreate: Cardinal
                           ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetCreateIndex3(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         pindexcreate :PJET_INDEXCREATE2_W;     // __ecount( cIndexCreate )
                         cIndexCreate: Cardinal
                           ): JET_ERR;  stdcall;
{$ELSE}
function JetCreateIndex3(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         pindexcreate: PJET_INDEXCREATE2_A;     // __ecount( cIndexCreate )
                         cIndexCreate: Cardinal
                           ): JET_ERR;  stdcall;
{$ENDIF}

{$IFEND}

{$IF JET_VERSION < $0600}
function JetDeleteIndex(sesid: JET_SESID;
                        tableid: JET_TABLEID;
                        szIndexName: JET_PCSTR
                          ): JET_ERR;  stdcall;
{$IFEND}

function JetDeleteIndexA(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         szIndexName: JET_PCSTR
                          ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetDeleteIndexW(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         szIndexName: JET_PCWSTR
                          ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetDeleteIndex(sesid: JET_SESID;
                        tableid: JET_TABLEID;
                        szIndexName: JET_PCWSTR
                          ): JET_ERR;  stdcall;
{$ELSE}
function JetDeleteIndex(sesid: JET_SESID;
                        tableid: JET_TABLEID;
                        szIndexName: JET_PCSTR
                          ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


function JetBeginTransaction(sesid: JET_SESID): JET_ERR;  stdcall;

function JetBeginTransaction2(sesid: JET_SESID;
                              grbit: JET_GRBIT
                               ): JET_ERR;  stdcall;


function JetCommitTransaction(sesid: JET_SESID;
                              grbit: JET_GRBIT
                               ): JET_ERR;  stdcall;

function JetRollback(sesid: JET_SESID;
                     grbit: JET_GRBIT
                      ): JET_ERR;  stdcall;



function JetGetDatabaseInfo(sesid: JET_SESID;
                            dbid: JET_DBID;
                            pvResult: Pointer;       // _bcount( cbMax )
                            cbMax: Cardinal;
                            InfoLevel: Cardinal
                              ): JET_ERR;  stdcall;

{$IF JET_VERSION < $0600}
function JetGetDatabaseFileInfo(szDatabaseName: JET_PCSTR;
                                pvResult: Pointer;       // _bcount( cbMax )
                                cbMax: Cardinal;
                                InfoLevel: Cardinal
                                  ): JET_ERR;  stdcall;
{$IFEND}

function JetGetDatabaseFileInfoA(szDatabaseName: JET_PCSTR;
                                 pvResult: Pointer;       // _bcount( cbMax )
                                 cbMax: Cardinal;
                                 InfoLevel: Cardinal
                                  ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetGetDatabaseFileInfoW(szDatabaseName: JET_PCWSTR;
                                 pvResult: Pointer;       // _bcount( cbMax )
                                 cbMax: Cardinal;
                                 InfoLevel: Cardinal
                                  ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetGetDatabaseFileInfo(szDatabaseName: JET_PCWSTR;
                                pvResult: Pointer;       // _bcount( cbMax )
                                cbMax: Cardinal;
                                InfoLevel: Cardinal
                                  ): JET_ERR;  stdcall;
{$ELSE}
function JetGetDatabaseFileInfo(szDatabaseName: JET_PCSTR;
                                pvResult: Pointer;       // _bcount( cbMax )
                                cbMax: Cardinal;
                                InfoLevel: Cardinal
                                  ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetOpenDatabase(sesid: JET_SESID;
                         szFilename: JET_PCSTR;
                         szConnect: JET_PCSTR;
                         out dbid: JET_DBID;
                         grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;
{$IFEND}

function JetOpenDatabaseA(sesid: JET_SESID;
                          szFilename: JET_PCSTR;
                          szConnect: JET_PCSTR;
                          out dbid: JET_DBID;
                          grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetOpenDatabaseW(sesid: JET_SESID;
                          szFilename: JET_PCWSTR;
                          szConnect: JET_PCWSTR;
                          out dbid: JET_DBID;
                          grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetOpenDatabase(sesid: JET_SESID;
                         szFilename: JET_PCWSTR;
                         szConnect: JET_PCWSTR;
                         out dbid: JET_DBID;
                         grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;
{$ELSE}
function JetOpenDatabase(sesid: JET_SESID;
                         szFilename: JET_PCSTR;
                         szConnect: JET_PCSTR;
                         out dbid: JET_DBID;
                         grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

function JetCloseDatabase(sesid: JET_SESID;
                          dbid: JET_DBID;
                          grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;

{$IF JET_VERSION < $0600}
function JetOpenTable(sesid: JET_SESID;
                      dbid: JET_DBID;
                      szTableName: JET_PCSTR;
                      pvParameters: Pointer;       // _bcount( cbParameters )
                      cbParameters: Cardinal;
                      grbit: JET_GRBIT;
                      out tableid: JET_TABLEID
                        ): JET_ERR;  stdcall;
{$IFEND}

function JetOpenTableA(sesid: JET_SESID;
                       dbid: JET_DBID;
                       szTableName: JET_PCSTR;
                       pvParameters: Pointer;       // _bcount( cbParameters )
                       cbParameters: Cardinal;
                       grbit: JET_GRBIT;
                       out tableid: JET_TABLEID
                        ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetOpenTableW(sesid: JET_SESID;
                       dbid: JET_DBID;
                       szTableName: JET_PCWSTR;
                       pvParameters: Pointer;       // _bcount( cbParameters )
                       cbParameters: Cardinal;
                       grbit: JET_GRBIT;
                       out tableid: JET_TABLEID
                        ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetOpenTable(sesid: JET_SESID;
                      dbid: JET_DBID;
                      szTableName: JET_PCWSTR;
                      pvParameters: Pointer;       // _bcount( cbParameters )
                      cbParameters: Cardinal;
                      grbit: JET_GRBIT;
                      out tableid: JET_TABLEID
                        ): JET_ERR;  stdcall;
{$ELSE}
function JetOpenTable(sesid: JET_SESID;
                      dbid: JET_DBID;
                      szTableName: JET_PCSTR;
                      pvParameters: Pointer;       // _bcount( cbParameters )
                      cbParameters: Cardinal;
                      grbit: JET_GRBIT;
                      out tableid: JET_TABLEID
                        ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
function JetSetTableSequential(sesid: JET_SESID;
                               tableid: JET_TABLEID;
                               grbit: JET_GRBIT
                                ): JET_ERR;  stdcall;

function JetResetTableSequential(sesid: JET_SESID;
                                 tableid: JET_TABLEID;
                                 grbit: JET_GRBIT
                                  ): JET_ERR;  stdcall;
{$IFEND}

function JetCloseTable(sesid: JET_SESID;
                       tableid: JET_TABLEID
                        ): JET_ERR;  stdcall;

function JetDelete(sesid: JET_SESID;
                   tableid: JET_TABLEID
                    ): JET_ERR;  stdcall;

function JetUpdate(sesid: JET_SESID;
                   tableid: JET_TABLEID;
                   pvBookmark: Pointer;   // _bcount_part( cbBookmark, *pcbActual )
                   cbBookmark: Cardinal;
                   pcbActual: PCardinal
                    ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0502}
function JetUpdate2(sesid: JET_SESID;
                    tableid: JET_TABLEID;
                    pvBookmark: Pointer;   // _bcount_part( cbBookmark, *pcbActual )
                    cbBookmark: Cardinal;
                    pcbActual: PCardinal;
                    grbit: JET_GRBIT
                     ): JET_ERR;  stdcall;
{$IFEND}

function JetEscrowUpdate(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         columnid: JET_COLUMNID;
                         pv: Pointer;               // _bcount( cbMax )
                         cbMax: Cardinal;
                         pvOld: Pointer;   // _bcount_part( cbOldMax, *pcbOldActual )
                         cbOldMax: Cardinal;
                         pcbActual: PCardinal;
                         grbit: JET_GRBIT
                          ): JET_ERR;  stdcall;

function JetRetrieveColumn(sesid: JET_SESID;
                           tableid: JET_TABLEID;
                           columnid: JET_COLUMNID;
                           pvData: Pointer;   // _bcount_part( cbData, *pcbActual )
                           cbData: Cardinal;
                           pcbActual: PCardinal;
                           grbit: JET_GRBIT;
                           pretinfo: PJET_RETINFO
                            ): JET_ERR;  stdcall;

function JetRetrieveColumns(sesid: JET_SESID;
                            tableid: JET_TABLEID;
                            pretrievecolumn: PJET_RETRIEVECOLUMN;  // _ecount( cretrievecolumn )
                            cretrievecolumn: Cardinal
                             ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0501}
function JetEnumerateColumns(sesid: JET_SESID;
                             tableid: JET_TABLEID;
                             cEnumColumnId: Cardinal;
                             rgEnumColumnId: PJET_ENUMCOLUMNID;     // __ecount( cEnumColumnId )
                             out cEnumColumn: Cardinal;
                             out prgEnumColumn: PJET_ENUMCOLUMN;  // _ecount( *pcEnumColumn )
                             pfnRealloc: JET_PFNREALLOC;
                             pvReallocContext: Pointer;
                             cbDataMost: Cardinal;
                             grbit: JET_GRBIT
                              ): JET_ERR;  stdcall;
{$IFEND}


{$IF JET_VERSION >= $0600}
function JetGetRecordSize(sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          out recsize: JET_RECSIZE;
                          grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;
{$IFEND}

{$IF JET_VERSION >= $0601}
function JetGetRecordSize2(sesid: JET_SESID;
                           tableid: JET_TABLEID;
                           out recsize: JET_RECSIZE2;
                           grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;
{$IFEND}

function JetSetColumn(sesid: JET_SESID;
                      tableid: JET_TABLEID;
                      columnid: JET_COLUMNID;
                      pvData: Pointer;       // _bcount( cbData )
                      cbData: Cardinal;
                      grbit: JET_GRBIT;
                      psetinfo: PJET_SETINFO
                       ): JET_ERR;  stdcall;

function JetSetColumns(sesid: JET_SESID;
                       tableid: JET_TABLEID;
                        psetcolumn: PJET_SETCOLUMN;     // __ecount( csetcolumn )
                       csetcolumn: Cardinal
                        ): JET_ERR;  stdcall;

function JetPrepareUpdate(sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          prep: Cardinal
                           ): JET_ERR;  stdcall;

function JetGetRecordPosition(sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              out recpos: JET_RECPOS;   // _bcount( cbRecpos )
                              cbRecpos: Cardinal
                               ): JET_ERR;  stdcall;

function JetGotoPosition(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         const recpos: JET_RECPOS
                          ): JET_ERR;  stdcall;

function JetGetCursorInfo(sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          pvResult: Pointer;       // _bcount( cbMax )
                          cbMax: Cardinal;
                          InfoLevel: Cardinal
                           ): JET_ERR;  stdcall;

function JetDupCursor(sesid: JET_SESID;
                      tableid: JET_TABLEID;
                      out duptableid: JET_TABLEID;
                      grbit: JET_GRBIT
                       ): JET_ERR;  stdcall;


{$IF JET_VERSION < $0600}
function JetGetCurrentIndex( sesid: JET_SESID;
                             tableid: JET_TABLEID;
                             szIndexName: JET_PSTR;       // _bcount( ccbIndexName )
                             ccbIndexName: Cardinal
                              ): JET_ERR;  stdcall;
{$IFEND}

function JetGetCurrentIndexA(sesid: JET_SESID;
                             tableid: JET_TABLEID;
                             szIndexName: JET_PSTR;       // _bcount( ccbIndexName )
                             ccbIndexName: Cardinal
                              ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetGetCurrentIndexW(sesid: JET_SESID;
                             tableid: JET_TABLEID;
                             szIndexName: JET_PWSTR;       // _bcount( ccbIndexName )
                             ccbIndexName: Cardinal
                              ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetGetCurrentIndex( sesid: JET_SESID;
                             tableid: JET_TABLEID;
                             szIndexName: JET_PWSTR;       // _bcount( ccbIndexName )
                             ccbIndexName: Cardinal
                              ): JET_ERR;  stdcall;
{$ELSE}
function JetGetCurrentIndex( sesid: JET_SESID;
                             tableid: JET_TABLEID;
                             szIndexName: JET_PSTR;       // _bcount( ccbIndexName )
                             ccbIndexName: Cardinal
                              ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetSetCurrentIndex( sesid: JET_SESID;
                             tableid: JET_TABLEID;
                             szIndexName: JET_PCSTR
                              ): JET_ERR;  stdcall;
{$IFEND}

function JetSetCurrentIndexA(sesid: JET_SESID;
                             tableid: JET_TABLEID;
                             szIndexName: JET_PCSTR
                              ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetSetCurrentIndexW(sesid: JET_SESID;
                             tableid: JET_TABLEID;
                             szIndexName: JET_PCWSTR
                              ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetSetCurrentIndex( sesid: JET_SESID;
                             tableid: JET_TABLEID;
                             szIndexName: JET_PCWSTR
                              ): JET_ERR;  stdcall;
{$ELSE}
function JetSetCurrentIndex( sesid: JET_SESID;
                             tableid: JET_TABLEID;
                             szIndexName: JET_PCSTR
                              ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600}
function JetSetCurrentIndex2( sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCSTR;
                              grbit: JET_GRBIT
                               ): JET_ERR;  stdcall;
{$IFEND}

function JetSetCurrentIndex2A(sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCSTR;
                              grbit: JET_GRBIT
                               ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetSetCurrentIndex2W(sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCWSTR;
                              grbit: JET_GRBIT
                               ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetSetCurrentIndex2( sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCWSTR;
                              grbit: JET_GRBIT
                               ): JET_ERR;  stdcall;
{$ELSE}
function JetSetCurrentIndex2( sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCSTR;
                              grbit: JET_GRBIT
                               ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetSetCurrentIndex3( sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCSTR;
                              grbit: JET_GRBIT;
                              itagSequence: Cardinal
                               ): JET_ERR;  stdcall;
{$IFEND}

function JetSetCurrentIndex3A(sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCSTR;
                              grbit: JET_GRBIT;
                              itagSequence: Cardinal
                               ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetSetCurrentIndex3W(sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCWSTR;
                              grbit: JET_GRBIT;
                              itagSequence: Cardinal
                               ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetSetCurrentIndex3( sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCWSTR;
                              grbit: JET_GRBIT;
                              itagSequence: Cardinal
                               ): JET_ERR;  stdcall;
{$ELSE}
function JetSetCurrentIndex3( sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCSTR;
                              grbit: JET_GRBIT;
                              itagSequence: Cardinal
                               ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetSetCurrentIndex4( sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCSTR;
                              pindexid: PJET_INDEXID;
                              grbit: JET_GRBIT;
                              itagSequence: Cardinal
                               ): JET_ERR;  stdcall;
{$IFEND}

function JetSetCurrentIndex4A(sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCSTR;
                              pindexid: PJET_INDEXID;
                              grbit: JET_GRBIT;
                              itagSequence: Cardinal
                               ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetSetCurrentIndex4W(sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCWSTR;
                              pindexid: PJET_INDEXID;
                              grbit: JET_GRBIT;
                              itagSequence: Cardinal
                               ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetSetCurrentIndex4( sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCWSTR;
                              pindexid: PJET_INDEXID;
                              grbit: JET_GRBIT;
                              itagSequence: Cardinal
                               ): JET_ERR;  stdcall;
{$ELSE}
function JetSetCurrentIndex4( sesid: JET_SESID;
                              tableid: JET_TABLEID;
                              szIndexName: JET_PCSTR;
                              pindexid: PJET_INDEXID;
                              grbit: JET_GRBIT;
                              itagSequence: Cardinal
                               ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


function JetMove(sesid: JET_SESID;
                 tableid: JET_TABLEID;
                 cRow: long;
                 grbit: JET_GRBIT
                  ): JET_ERR;  stdcall;

function JetGetLock(sesid: JET_SESID;
                    tableid: JET_TABLEID;
                    grbit: JET_GRBIT
                     ): JET_ERR;  stdcall;

function JetMakeKey(sesid: JET_SESID;
                    tableid: JET_TABLEID;
                    pvData: Pointer;       // _bcount( cbData )
                    cbData: Cardinal;
                    grbit: JET_GRBIT
                     ): JET_ERR;  stdcall;

function JetSeek(sesid: JET_SESID;
                 tableid: JET_TABLEID;
                 grbit: JET_GRBIT
                  ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0601}
function JetPrereadKeys( sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         rgpvKeys: PtrPointer;     // __ecount(ckeys)
                         rgcbKeys: PCardinal;     // __ecount(ckeys)
                         ckeys: long;
                         out ckeysPreread: long;
                         grbit: JET_GRBIT
                         ): JET_ERR;  stdcall;
{$IFEND}

function JetGetBookmark(   sesid: JET_SESID;
                           tableid: JET_TABLEID;
                           pvBookmark: Pointer;   // _bcount_part( cbMax, *pcbActual )
                           cbMax: Cardinal;
                           pcbActual: PCardinal
                         ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0501}
function JetGetSecondaryIndexBookmark(  sesid: JET_SESID;
                                        tableid: JET_TABLEID;
                                        pvSecondaryKey: Pointer;   // _bcount_part( cbSecondaryKeyMax, *pcbSecondaryKeyActual )
                                        cbSecondaryKeyMax: Cardinal;
                                        pcbSecondaryKeyActual: PCardinal;
                                        pvPrimaryBookmark: Pointer;   // _bcount_part( cbPrimaryBookmarkMax, *pcbPrimaryKeyActual )
                                        cbPrimaryBookmarkMax: Cardinal;
                                        pcbPrimaryKeyActual: PCardinal;
                                        grbit: JET_GRBIT
                                       ): JET_ERR;  stdcall;
{$IFEND}


{$IF JET_VERSION < $0600}
function JetCompact( sesid: JET_SESID;
                     szDatabaseSrc: JET_PCSTR;
                     szDatabaseDest: JET_PCSTR;
                     pfnStatus: JET_PFNSTATUS;
                     pconvert: PJET_CONVERT_A;
                     grbit: JET_GRBIT
                      ): JET_ERR;  stdcall;
{$IFEND}

function JetCompactA(sesid: JET_SESID;
                     szDatabaseSrc: JET_PCSTR;
                     szDatabaseDest: JET_PCSTR;
                     pfnStatus: JET_PFNSTATUS;
                     pconvert: PJET_CONVERT_A;
                     grbit: JET_GRBIT
                      ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetCompactW(sesid: JET_SESID;
                     szDatabaseSrc: JET_PCWSTR;
                     szDatabaseDest: JET_PCWSTR;
                     pfnStatus: JET_PFNSTATUS;
                     pconvert: PJET_CONVERT_W;
                     grbit: JET_GRBIT
                      ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetCompact( sesid: JET_SESID;
                     szDatabaseSrc: JET_PCWSTR;
                     szDatabaseDest: JET_PCWSTR;
                     pfnStatus: JET_PFNSTATUS;
                     pconvert: PJET_CONVERT_W;
                     grbit: JET_GRBIT
                      ): JET_ERR;  stdcall;
{$ELSE}
function JetCompact( sesid: JET_SESID;
                     szDatabaseSrc: JET_PCSTR;
                     szDatabaseDest: JET_PCSTR;
                     pfnStatus: JET_PFNSTATUS;
                     pconvert: PJET_CONVERT_A;
                     grbit: JET_GRBIT
                      ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetDefragment( sesid: JET_SESID;
                        dbid: JET_DBID;
                        szTableName: JET_PCSTR;
                            pcPasses: PCardinal;
                            pcSeconds: PCardinal;
                        grbit: JET_GRBIT
                         ): JET_ERR;  stdcall;
{$IFEND}

function JetDefragmentA(sesid: JET_SESID;
                        dbid: JET_DBID;
                        szTableName: JET_PCSTR;
                        pcPasses: PCardinal;
                        pcSeconds: PCardinal;
                        grbit: JET_GRBIT
                         ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetDefragmentW(sesid: JET_SESID;
                        dbid: JET_DBID;
                        szTableName: JET_PCWSTR;
                        pcPasses: PCardinal;
                        pcSeconds: PCardinal;
                        grbit: JET_GRBIT
                         ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetDefragment( sesid: JET_SESID;
                        dbid: JET_DBID;
                        szTableName: JET_PCWSTR;
                        pcPasses: PCardinal;
                        pcSeconds: PCardinal;
                        grbit: JET_GRBIT
                         ): JET_ERR;  stdcall;
{$ELSE}
function JetDefragment( sesid: JET_SESID;
                        dbid: JET_DBID;
                        szTableName: JET_PCSTR;
                        pcPasses: PCardinal;
                        pcSeconds: PCardinal;
                        grbit: JET_GRBIT
                         ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetDefragment2( sesid: JET_SESID;
                         dbid: JET_DBID;
                         szTableName: JET_PCSTR;
                         pcPasses: PCardinal;
                         pcSeconds: PCardinal;
                         callback: JET_CALLBACK;
                         grbit: JET_GRBIT
                          ): JET_ERR;  stdcall;
{$IFEND}

function JetDefragment2A(sesid: JET_SESID;
                         dbid: JET_DBID;
                         szTableName: JET_PCSTR;
                         pcPasses: PCardinal;
                         pcSeconds: PCardinal;
                         callback: JET_CALLBACK;
                         grbit: JET_GRBIT
                          ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetDefragment2W(sesid: JET_SESID;
                         dbid: JET_DBID;
                         szTableName: JET_PCWSTR;
                         pcPasses: PCardinal;
                         pcSeconds: PCardinal;
                         callback: JET_CALLBACK;
                         grbit: JET_GRBIT
                          ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetDefragment2( sesid: JET_SESID;
                         dbid: JET_DBID;
                         szTableName: JET_PCWSTR;
                         pcPasses: PCardinal;
                         pcSeconds: PCardinal;
                         callback: JET_CALLBACK;
                         grbit: JET_GRBIT
                          ): JET_ERR;  stdcall;
{$ELSE}
function JetDefragment2( sesid: JET_SESID;
                         dbid: JET_DBID;
                         szTableName: JET_PCSTR;
                         pcPasses: PCardinal;
                         pcSeconds: PCardinal;
                         callback: JET_CALLBACK;
                         grbit: JET_GRBIT
                          ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetDefragment3( sesid: JET_SESID;
                         szDatabaseName: JET_PCSTR;
                         szTableName: JET_PCSTR;
                         pcPasses: PCardinal;
                         pcSeconds: PCardinal;
                         callback: JET_CALLBACK;
                         pvContext: Pointer;
                         grbit: JET_GRBIT
                          ): JET_ERR;  stdcall;
{$IFEND}

function JetDefragment3A(sesid: JET_SESID;
                         szDatabaseName: JET_PCSTR;
                         szTableName: JET_PCSTR;
                         pcPasses: PCardinal;
                         pcSeconds: PCardinal;
                         callback: JET_CALLBACK;
                         pvContext: Pointer;
                         grbit: JET_GRBIT
                          ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetDefragment3W(sesid: JET_SESID;
                         szDatabaseName: JET_PCWSTR;
                         szTableName: JET_PCWSTR;
                         pcPasses: PCardinal;
                         pcSeconds: PCardinal;
                         callback: JET_CALLBACK;
                         pvContext: Pointer;
                         grbit: JET_GRBIT
                          ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetDefragment3( sesid: JET_SESID;
                         szDatabaseName: JET_PCWSTR;
                         szTableName: JET_PCWSTR;
                         pcPasses: PCardinal;
                         pcSeconds: PCardinal;
                         callback: JET_CALLBACK;
                         pvContext: Pointer;
                         grbit: JET_GRBIT
                          ): JET_ERR;  stdcall;
{$ELSE}
function JetDefragment3( sesid: JET_SESID;
                         szDatabaseName: JET_PCSTR;
                         szTableName: JET_PCSTR;
                         pcPasses: PCardinal;
                         pcSeconds: PCardinal;
                         callback: JET_CALLBACK;
                         pvContext: Pointer;
                         grbit: JET_GRBIT
                          ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IFEND}

{$IF JET_VERSION < $0600}
function JetSetDatabaseSize( sesid: JET_SESID;
                             szDatabaseName: JET_PCSTR;
                             cpg: Cardinal;
                             out cpgReal: Cardinal
                              ): JET_ERR;  stdcall;
{$IFEND}

function JetSetDatabaseSizeA(sesid: JET_SESID;
                             szDatabaseName: JET_PCSTR;
                             cpg: Cardinal;
                             out cpgReal: Cardinal
                              ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetSetDatabaseSizeW(sesid: JET_SESID;
                             szDatabaseName: JET_PCWSTR;
                             cpg: Cardinal;
                             out cpgReal: Cardinal
                              ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetSetDatabaseSize( sesid: JET_SESID;
                             szDatabaseName: JET_PCWSTR;
                             cpg: Cardinal;
                             out cpgReal: Cardinal
                              ): JET_ERR;  stdcall;
{$ELSE}
function JetSetDatabaseSize( sesid: JET_SESID;
                             szDatabaseName: JET_PCSTR;
                             cpg: Cardinal;
                             out cpgReal: Cardinal
                              ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


function JetGrowDatabase(sesid: JET_SESID;
                         dbid: JET_DBID;
                         cpg: Cardinal;
                         pcpgReal: PCardinal
                          ): JET_ERR;  stdcall;

function JetSetSessionContext(sesid: JET_SESID;
                              ulContext: Pointer
                               ): JET_ERR;  stdcall;

function JetResetSessionContext(sesid: JET_SESID): JET_ERR;  stdcall;

function JetGotoBookmark(sesid: JET_SESID;
                         tableid: JET_TABLEID;
                         pvBookmark: Pointer;       // _bcount( cbBookmark )
                         cbBookmark: Cardinal
                          ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0501}
function JetGotoSecondaryIndexBookmark(sesid: JET_SESID;
                                       tableid: JET_TABLEID;
                                       pvSecondaryKey: Pointer;       // _bcount( cbSecondaryKey )
                                       cbSecondaryKey: Cardinal;
                                       pvPrimaryBookmark: Pointer;       // _bcount( cbPrimaryBookmark )
                                       cbPrimaryBookmark: Cardinal;
                                       grbit: JET_GRBIT
                                        ): JET_ERR;  stdcall;
{$IFEND}

function JetIntersectIndexes(sesid: JET_SESID;
                             prgindexrange: PJET_INDEXRANGE;     // __ecount( cindexrange )
                             cindexrange: Cardinal;
                             var recordlist: JET_RECORDLIST;
                             grbit: JET_GRBIT
                              ): JET_ERR;  stdcall;

function JetComputeStats(sesid: JET_SESID;
                         tableid: JET_TABLEID
                          ): JET_ERR;  stdcall;

function JetOpenTempTable(sesid: JET_SESID;
                          const rgcolumndef: JET_COLUMNDEF;     // __ecount( ccolumn )
                          ccolumn: Cardinal;
                          grbit: JET_GRBIT;
                          out tableid: JET_TABLEID;
                          out rgcolumnid: JET_COLUMNID   // _ecount( ccolumn )
                           ): JET_ERR;  stdcall;

function JetOpenTempTable2(sesid: JET_SESID;
                           const rgcolumndef: JET_COLUMNDEF;     // __ecount( ccolumn )
                           lcid: Cardinal;
                           ccolumn: Cardinal;
                           grbit: JET_GRBIT;
                           out tableid: JET_TABLEID;
                           out rgcolumnid: JET_COLUMNID   // _ecount( ccolumn )
                            ): JET_ERR;  stdcall;

function JetOpenTempTable3(sesid: JET_SESID;
                           const rgcolumndef: JET_COLUMNDEF;     // __ecount( ccolumn )
                           ccolumn: Cardinal;
                           pidxunicode: PJET_UNICODEINDEX;
                           grbit: JET_GRBIT;
                           out tableid: JET_TABLEID;
                           out rgcolumnid: JET_COLUMNID   // _ecount( ccolumn )
                            ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}
function JetOpenTemporaryTable(sesid: JET_SESID;
                               const opentemporarytable: JET_OPENTEMPORARYTABLE
                                ): JET_ERR;  stdcall;
{$IFEND}


{$IF JET_VERSION < $0600}
function JetBackup( szBackupPath: JET_PCSTR;
                    grbit: JET_GRBIT;
                    pfnStatus: JET_PFNSTATUS
                     ): JET_ERR;  stdcall;
{$IFEND}

function JetBackupA(szBackupPath: JET_PCSTR;
                    grbit: JET_GRBIT;
                    pfnStatus: JET_PFNSTATUS
                     ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetBackupW(szBackupPath: JET_PCWSTR;
                    grbit: JET_GRBIT;
                    pfnStatus: JET_PFNSTATUS
                     ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetBackup( szBackupPath: JET_PCWSTR;
                    grbit: JET_GRBIT;
                    pfnStatus: JET_PFNSTATUS
                     ): JET_ERR;  stdcall;
{$ELSE}
function JetBackup( szBackupPath: JET_PCSTR;
                    grbit: JET_GRBIT;
                    pfnStatus: JET_PFNSTATUS
                     ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetBackupInstance( instance: JET_INSTANCE;
                            szBackupPath: JET_PCSTR;
                            grbit: JET_GRBIT;
                            pfnStatus: JET_PFNSTATUS
                             ): JET_ERR;  stdcall;
{$IFEND}

function JetBackupInstanceA(instance: JET_INSTANCE;
                            szBackupPath: JET_PCSTR;
                            grbit: JET_GRBIT;
                            pfnStatus: JET_PFNSTATUS
                             ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetBackupInstanceW(instance: JET_INSTANCE;
                            szBackupPath: JET_PCWSTR;
                            grbit: JET_GRBIT;
                            pfnStatus: JET_PFNSTATUS
                             ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetBackupInstance( instance: JET_INSTANCE;
                            szBackupPath: JET_PCWSTR;
                            grbit: JET_GRBIT;
                            pfnStatus: JET_PFNSTATUS
                             ): JET_ERR;  stdcall;
{$ELSE}
function JetBackupInstance( instance: JET_INSTANCE;
                            szBackupPath: JET_PCSTR;
                            grbit: JET_GRBIT;
                            pfnStatus: JET_PFNSTATUS
                             ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IFEND}


{$IF JET_VERSION < $0600}
function JetRestore( sz : JET_PCSTR;
                     pfn: JET_PFNSTATUS
                      ): JET_ERR;  stdcall;
{$IFEND}

function JetRestoreA(sz : JET_PCSTR;
                     pfn: JET_PFNSTATUS
                      ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetRestoreW(sz : JET_PCWSTR;
                     pfn: JET_PFNSTATUS
                      ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetRestore( sz : JET_PCWSTR;
                     pfn: JET_PFNSTATUS
                      ): JET_ERR;  stdcall;
{$ELSE}
function JetRestore( sz : JET_PCSTR;
                     pfn: JET_PFNSTATUS
                      ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetRestore2( sz: JET_PCSTR;
                      szDest :JET_PCSTR;
                      pfn: JET_PFNSTATUS
                       ): JET_ERR;  stdcall;
{$IFEND}

function JetRestore2A(sz: JET_PCSTR;
                      szDest :JET_PCSTR;
                      pfn: JET_PFNSTATUS
                       ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetRestore2W(sz: JET_PCWSTR;
                      szDest: JET_PCWSTR;
                      pfn: JET_PFNSTATUS
                       ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetRestore2( sz: JET_PCWSTR;
                      szDest: JET_PCWSTR;
                      pfn: JET_PFNSTATUS
                       ): JET_ERR;  stdcall;
{$ELSE}
function JetRestore2( sz: JET_PCSTR;
                      szDest :JET_PCSTR;
                      pfn: JET_PFNSTATUS
                       ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetRestoreInstance( instance: JET_INSTANCE;
                             sz: JET_PCSTR;
                             szDest: JET_PCSTR;
                             pfn: JET_PFNSTATUS
                              ): JET_ERR;  stdcall;
{$IFEND}

function JetRestoreInstanceA(instance: JET_INSTANCE;
                             sz: JET_PCSTR;
                             szDest: JET_PCSTR;
                             pfn: JET_PFNSTATUS
                              ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetRestoreInstanceW(instance: JET_INSTANCE;
                             sz: JET_PCWSTR;
                             szDest: JET_PCWSTR;
                             pfn: JET_PFNSTATUS
                              ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetRestoreInstance( instance: JET_INSTANCE;
                             sz: JET_PCWSTR;
                             szDest: JET_PCWSTR;
                             pfn: JET_PFNSTATUS
                              ): JET_ERR;  stdcall;
{$ELSE}
function JetRestoreInstance( instance: JET_INSTANCE;
                             sz: JET_PCSTR;
                             szDest: JET_PCSTR;
                             pfn: JET_PFNSTATUS
                              ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IFEND}

function JetSetIndexRange(sesid: JET_SESID;
                          tableidSrc: JET_TABLEID;
                          grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;

function JetIndexRecordCount(sesid: JET_SESID;
                             tableid: JET_TABLEID;
                             out crec: Cardinal;
                             crecMax: Cardinal
                              ): JET_ERR;  stdcall;

function JetRetrieveKey(  sesid: JET_SESID;
                          tableid: JET_TABLEID;
                          pvData: Pointer;   // _bcount_part( cbMax, *pcbActual )
                          cbMax: Cardinal;
                          pcbActual: PCardinal;
                          grbit: JET_GRBIT
                         ): JET_ERR;  stdcall;

function JetBeginExternalBackup( grbit: JET_GRBIT
                                                 ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0501}
function JetBeginExternalBackupInstance( instance: JET_INSTANCE;
                                         grbit: JET_GRBIT
                ): JET_ERR;  stdcall;
{$IFEND}

{$IF JET_VERSION < $0600}
function JetGetAttachInfo(     pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                               cbMax: Cardinal;
                               pcbActual: PCardinal
                            ): JET_ERR;  stdcall;
{$IFEND}

function JetGetAttachInfoA( {$IF JET_VERSION < $0600}
                               pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                            {$ELSE}
                               szz: JET_PSTR;   // _bcount_part( cbMax, *pcbActual )
                            {$IFEND}
                               cbMax: Cardinal;
                               pcbActual: PCardinal
                            ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetGetAttachInfoW(    szz: JET_PWSTR;   // _bcount_part( cbMax, *pcbActual )
                               cbMax: Cardinal;
                               pcbActual: PCardinal
                            ): JET_ERR;  stdcall;


{$IFDEF JET_UNICODE}
function JetGetAttachInfo(     szz: JET_PWSTR;   // _bcount_part( cbMax, *pcbActual )
                               cbMax: Cardinal;
                               pcbActual: PCardinal
                            ): JET_ERR;  stdcall;
{$ELSE}
function JetGetAttachInfo(  {$IF JET_VERSION < $0600}
                               pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                            {$ELSE}
                               szz: JET_PSTR;   // _bcount_part( cbMax, *pcbActual )
                            {$IFEND}
                               cbMax: Cardinal;
                               pcbActual: PCardinal
                            ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetGetAttachInfoInstance(instance: JET_INSTANCE;
                                  pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                                  cbMax: Cardinal;
                                  pcbActual: PCardinal
                                    ): JET_ERR;  stdcall;
{$IFEND}

function JetGetAttachInfoInstanceA(    instance: JET_INSTANCE;
                                    {$IF JET_VERSION < $0600}
                                       pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                                    {$ELSE}
                                       szz: JET_PSTR;   // _bcount_part( cbMax, *pcbActual )
                                    {$IFEND}
                                       cbMax: Cardinal;
                                       pcbActual: PCardinal
                                    ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetGetAttachInfoInstanceW(instance: JET_INSTANCE;
                                   szz: JET_PWSTR;   // _bcount_part( cbMax, *pcbActual )
                                   cbMax: Cardinal;
                                   pcbActual: PCardinal
                                    ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetGetAttachInfoInstance(instance: JET_INSTANCE;
                                  szz: JET_PWSTR;   // _bcount_part( cbMax, *pcbActual )
                                  cbMax: Cardinal;
                                  pcbActual: PCardinal
                                    ): JET_ERR;  stdcall;
{$ELSE}
function JetGetAttachInfoInstance(     instance: JET_INSTANCE;
                                    {$IF JET_VERSION < $0600}
                                       pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                                    {$ELSE}
                                       szz: JET_PSTR;   // _bcount_part( cbMax, *pcbActual )
                                    {$IFEND}
                                       cbMax: Cardinal;
                                       pcbActual: PCardinal
                                    ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IFEND}


{$IF JET_VERSION < $0600}
function JetOpenFile( szFileName: JET_PCSTR;
                      out hfFile: JET_HANDLE;
                      pulFileSizeLow: PCardinal;
                      pulFileSizeHigh: PCardinal
                       ): JET_ERR;  stdcall;
{$IFEND}

function JetOpenFileA(szFileName: JET_PCSTR;
                      out hfFile: JET_HANDLE;
                      pulFileSizeLow: PCardinal;
                      pulFileSizeHigh: PCardinal
                       ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetOpenFileW(szFileName: JET_PCWSTR;
                      out hfFile: JET_HANDLE;
                      pulFileSizeLow: PCardinal;
                      pulFileSizeHigh: PCardinal
                       ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetOpenFile( szFileName: JET_PCWSTR;
                      out hfFile: JET_HANDLE;
                      pulFileSizeLow: PCardinal;
                      pulFileSizeHigh: PCardinal
                       ): JET_ERR;  stdcall;
{$ELSE}
function JetOpenFile( szFileName: JET_PCSTR;
                      out hfFile: JET_HANDLE;
                      pulFileSizeLow: PCardinal;
                      pulFileSizeHigh: PCardinal
                       ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetOpenFileInstance( instance: JET_INSTANCE;
                              szFileName: JET_PCSTR;
                              out hfFile: JET_HANDLE;
                              pulFileSizeLow: PCardinal;
                              pulFileSizeHigh: PCardinal
                               ): JET_ERR;  stdcall;
{$IFEND}

function JetOpenFileInstanceA(instance: JET_INSTANCE;
                              szFileName: JET_PCSTR;
                              out hfFile: JET_HANDLE;
                              pulFileSizeLow: PCardinal;
                              pulFileSizeHigh: PCardinal
                               ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetOpenFileInstanceW(instance: JET_INSTANCE;
                              szFileName: JET_PCWSTR;
                              out hfFile: JET_HANDLE;
                              pulFileSizeLow: PCardinal;
                              pulFileSizeHigh: PCardinal
                               ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetOpenFileInstance( instance: JET_INSTANCE;
                              szFileName: JET_PCWSTR;
                              out hfFile: JET_HANDLE;
                              pulFileSizeLow: PCardinal;
                              pulFileSizeHigh: PCardinal
                               ): JET_ERR;  stdcall;
{$ELSE}
function JetOpenFileInstance( instance: JET_INSTANCE;
                              szFileName: JET_PCSTR;
                              out hfFile: JET_HANDLE;
                              pulFileSizeLow: PCardinal;
                              pulFileSizeHigh: PCardinal
                               ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

{$IFEND}

function JetReadFile(  hfFile: JET_HANDLE;
                       pv: Pointer;    // _bcount_part( cb, *pcbActual )
                       cb: Cardinal;
                       pcbActual: PCardinal
                      ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0501}
function JetReadFileInstance(  instance: JET_INSTANCE;
                               hfFile: JET_HANDLE;
                               pv: Pointer;    // _bcount_part( cb, *pcbActual )
                               cb: Cardinal;
                               pcbActual: PCardinal
                              ): JET_ERR;  stdcall;
{$IFEND}


function JetCloseFile(hfFile: JET_HANDLE): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0501}
function JetCloseFileInstance(instance: JET_INSTANCE;
                              hfFile: JET_HANDLE
                               ): JET_ERR;  stdcall;
{$IFEND}

{$IF JET_VERSION < $0600}
function JetGetLogInfo(pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                       cbMax: Cardinal;
                       pcbActual: PCardinal
                         ): JET_ERR;  stdcall;
{$IFEND}

function JetGetLogInfoA( {$IF JET_VERSION < $0600}
                            pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                         {$ELSE}
                            szz: JET_PSTR;   // _bcount_part( cbMax, *pcbActual )
                         {$IFEND}
                            cbMax: Cardinal;
                            pcbActual: PCardinal
                         ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetGetLogInfoW(szz: JET_PWSTR;   // _bcount_part( cbMax, *pcbActual )
                        cbMax: Cardinal;
                        pcbActual: PCardinal
                         ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetGetLogInfo(szz: JET_PWSTR;   // _bcount_part( cbMax, *pcbActual )
                       cbMax: Cardinal;
                       pcbActual: PCardinal
                         ): JET_ERR;  stdcall;
{$ELSE}
function JetGetLogInfo(  {$IF JET_VERSION < $0600}
                            pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                         {$ELSE}
                            szz: JET_PSTR;   // _bcount_part( cbMax, *pcbActual )
                         {$IFEND}
                        cbMax: Cardinal;
                            pcbActual: PCardinal
                         ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetGetLogInfoInstance( instance: JET_INSTANCE;
                                pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                                cbMax: Cardinal;
                                pcbActual: PCardinal
                                 ): JET_ERR;  stdcall;
{$IFEND}

function JetGetLogInfoInstanceA(instance: JET_INSTANCE;
                                 {$IF JET_VERSION < $0600}
                                    pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                                 {$ELSE}
                                    szz: JET_PSTR;    // _bcount_part( cbMax, *pcbActual )
                                 {$IFEND}
                                cbMax: Cardinal;
                                    pcbActual: PCardinal
                                 ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetGetLogInfoInstanceW(instance: JET_INSTANCE;
                                szz: JET_PWSTR;    // _bcount_part( cbMax, *pcbActual )
                                cbMax: Cardinal;
                                pcbActual: PCardinal
                                 ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetGetLogInfoInstance( instance: JET_INSTANCE;
                                szz: JET_PWSTR;    // _bcount_part( cbMax, *pcbActual )
                                cbMax: Cardinal;
                                pcbActual: PCardinal
                                 ): JET_ERR;  stdcall;
{$ELSE}
function JetGetLogInfoInstance( instance: JET_INSTANCE;
                                 {$IF JET_VERSION < $0600}
                                    pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                                 {$ELSE}
                                    szz: JET_PSTR;    // _bcount_part( cbMax, *pcbActual )
                                 {$IFEND}
                                cbMax: Cardinal;
                                    pcbActual: PCardinal
                                 ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


const
  JET_BASE_NAME_LENGTH  = 3;

type
  JET_LOGINFO_A = record
    cbSize: Cardinal;
    ulGenLow: Cardinal;
    ulGenHigh: Cardinal;
    szBaseName: array[0..JET_BASE_NAME_LENGTH] of AnsiChar;
  end;
  PJET_LOGINFO_A = ^JET_LOGINFO_A;

  JET_LOGINFO_W = record
    cbSize: Cardinal;
    ulGenLow: Cardinal;
    ulGenHigh: Cardinal;
    szBaseName: array[0..JET_BASE_NAME_LENGTH] of WideChar;
  end;
  PJET_LOGINFO_W = ^JET_LOGINFO_W;

  {$IFDEF JET_UNICODE}
  JET_LOGINFO = JET_LOGINFO_W;
  {$ELSE}
  JET_LOGINFO = JET_LOGINFO_A;
  {$ENDIF}

{$IF JET_VERSION < $0600}
function JetGetLogInfoInstance2( instance: JET_INSTANCE;
                                 pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                                 cbMax: Cardinal;
                                 pcbActual: PCardinal;
                                 pLogInfo: PJET_LOGINFO_A
                                  ): JET_ERR;  stdcall;
{$IFEND}

function JetGetLogInfoInstance2A(instance: JET_INSTANCE;
                                  {$IF JET_VERSION < $0600}
                                     pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                                  {$ELSE}
                                    szz: JET_PSTR;   // _bcount_part( cbMax, *pcbActual )
                                  {$IFEND}
                                 cbMax: Cardinal;
                                 pcbActual: PCardinal;
                                 pLogInfo: PJET_LOGINFO_A
                                  ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetGetLogInfoInstance2W(instance: JET_INSTANCE;
                                 szz: JET_PWSTR;    // _bcount_part( cbMax, *pcbActual )
                                 cbMax: Cardinal;
                                 pcbActual: PCardinal;
                                 pLogInfo: PJET_LOGINFO_W
                                  ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetGetLogInfoInstance2( instance: JET_INSTANCE;
                                 szz: JET_PWSTR;    // _bcount_part( cbMax, *pcbActual )
                                 cbMax: Cardinal;
                                 pcbActual: PCardinal;
                                 pLogInfo: PJET_LOGINFO_W
                                  ): JET_ERR;  stdcall;
{$ELSE}
function JetGetLogInfoInstance2( instance: JET_INSTANCE;
                                  {$IF JET_VERSION < $0600}
                                     pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                                  {$ELSE}
                                    szz: JET_PSTR;   // _bcount_part( cbMax, *pcbActual )
                                  {$IFEND}
                                 cbMax: Cardinal;
                                 pcbActual: PCardinal;
                                 pLogInfo: PJET_LOGINFO_A
                                  ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetGetTruncateLogInfoInstance( instance: JET_INSTANCE;
                                        pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                                        cbMax: Cardinal;
                                        pcbActual: PCardinal
                                         ): JET_ERR;  stdcall;
{$IFEND}

function JetGetTruncateLogInfoInstanceA(instance: JET_INSTANCE;
                                         {$IF JET_VERSION < $0600}
                                            pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                                         {$ELSE}
                                            szz: JET_PSTR;    // _bcount_part( cbMax, *pcbActual )
                                         {$IFEND}
                                        cbMax: Cardinal;
                                        pcbActual: PCardinal
                                         ): JET_ERR;  stdcall;


{$IF JET_VERSION >= $0600}

function JetGetTruncateLogInfoInstanceW(instance: JET_INSTANCE;
                                        szz: JET_PWSTR;    // _bcount_part( cbMax, *pcbActual )
                                        cbMax: Cardinal;
                                        pcbActual: PCardinal
                                         ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetGetTruncateLogInfoInstance( instance: JET_INSTANCE;
                                        szz: JET_PWSTR;    // _bcount_part( cbMax, *pcbActual )
                                        cbMax: Cardinal;
                                        pcbActual: PCardinal
                                         ): JET_ERR;  stdcall;
{$ELSE}
function JetGetTruncateLogInfoInstance( instance: JET_INSTANCE;
                                         {$IF JET_VERSION < $0600}
                                            pv: Pointer;    // _bcount_part( cbMax, *pcbActual )
                                         {$ELSE}
                                            szz: JET_PSTR;    // _bcount_part( cbMax, *pcbActual )
                                         {$IFEND}
                                        cbMax: Cardinal;
                                        pcbActual: PCardinal
                                         ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IFEND}

function JetTruncateLog: JET_ERR;  stdcall;

{$IF JET_VERSION >= $0501}
function JetTruncateLogInstance(instance: JET_INSTANCE): JET_ERR;  stdcall;
{$IFEND}

function JetEndExternalBackup: JET_ERR;  stdcall;

{$IF JET_VERSION >= $0501}
function JetEndExternalBackupInstance(instance: JET_INSTANCE): JET_ERR;  stdcall;

function JetEndExternalBackupInstance2(instance: JET_INSTANCE;
                                       grbit: JET_GRBIT
                                      ): JET_ERR;  stdcall;
{$IFEND}


{$IF JET_VERSION < $0600}
function JetExternalRestore(  szCheckpointFilePath: JET_PSTR;
                              szLogPath: JET_PSTR;
                              rgrstmap: PJET_RSTMAP_A;       // __ecount( crstfilemap )
                              crstfilemap: long;
                              szBackupLogPath: JET_PSTR;
                              genLow: long;
                              genHigh: long;
                              pfn: JET_PFNSTATUS
                              ): JET_ERR;  stdcall;
{$IFEND}

function JetExternalRestoreA( szCheckpointFilePath: JET_PSTR;
                              szLogPath: JET_PSTR;
                              rgrstmap: PJET_RSTMAP_A;       // __ecount( crstfilemap )
                              crstfilemap: long;
                              szBackupLogPath: JET_PSTR;
                              genLow: long;
                              genHigh: long;
                              pfn: JET_PFNSTATUS
                              ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetExternalRestoreW( szCheckpointFilePath: JET_PWSTR;
                              szLogPath: JET_PWSTR;
                              rgrstmap: PJET_RSTMAP_W;     // __ecount( crstfilemap )
                              crstfilemap: long;
                              szBackupLogPath: JET_PWSTR;
                              genLow: long;
                              genHigh: long;
                              pfn: JET_PFNSTATUS
                              ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetExternalRestore(  szCheckpointFilePath: JET_PWSTR;
                              szLogPath: JET_PWSTR;
                              rgrstmap: PJET_RSTMAP_W;     // __ecount( crstfilemap )
                              crstfilemap: long;
                              szBackupLogPath: JET_PWSTR;
                              genLow: long;
                              genHigh: long;
                              pfn: JET_PFNSTATUS
                              ): JET_ERR;  stdcall;
{$ELSE}
function JetExternalRestore(  szCheckpointFilePath: JET_PSTR;
                              szLogPath: JET_PSTR;
                              rgrstmap: PJET_RSTMAP_A;       // __ecount( crstfilemap )
                              crstfilemap: long;
                              szBackupLogPath: JET_PSTR;
                              genLow: long;
                              genHigh: long;
                              pfn: JET_PFNSTATUS
                              ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetExternalRestore2(  szCheckpointFilePath: JET_PSTR;
                               szLogPath: JET_PSTR;
                               rgrstmap: PJET_RSTMAP_A;       // __ecount( crstfilemap )
                               crstfilemap: long;
                               szBackupLogPath: JET_PSTR;
                               var LogInfo: JET_LOGINFO_A;
                               szTargetInstanceName: JET_PSTR;
                               szTargetInstanceLogPath: JET_PSTR;
                               szTargetInstanceCheckpointPath: JET_PSTR;
                               pfn: JET_PFNSTATUS
                               ): JET_ERR;  stdcall;
{$IFEND}

function JetExternalRestore2A( szCheckpointFilePath: JET_PSTR;
                               szLogPath: JET_PSTR;
                               rgrstmap: PJET_RSTMAP_A;       // __ecount( crstfilemap )
                               crstfilemap: long;
                               szBackupLogPath: JET_PSTR;
                               var LogInfo: JET_LOGINFO_A;
                               szTargetInstanceName: JET_PSTR;
                               szTargetInstanceLogPath: JET_PSTR;
                               szTargetInstanceCheckpointPath: JET_PSTR;
                               pfn: JET_PFNSTATUS
                               ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetExternalRestore2W( szCheckpointFilePath: JET_PWSTR;
                               szLogPath: JET_PWSTR;
                               rgrstmap: PJET_RSTMAP_W;     // __ecount( crstfilemap )
                               crstfilemap: long;
                               szBackupLogPath: JET_PWSTR;
                               var LogInfo: JET_LOGINFO_W;
                               szTargetInstanceName: JET_PWSTR;
                               szTargetInstanceLogPath: JET_PWSTR;
                               szTargetInstanceCheckpointPath: JET_PWSTR;
                               pfn: JET_PFNSTATUS
                               ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetExternalRestore2(  szCheckpointFilePath: JET_PWSTR;
                               szLogPath: JET_PWSTR;
                               rgrstmap: PJET_RSTMAP_W;     // __ecount( crstfilemap )
                               crstfilemap: long;
                               szBackupLogPath: JET_PWSTR;
                               var LogInfo: JET_LOGINFO_W;
                               szTargetInstanceName: JET_PWSTR;
                               szTargetInstanceLogPath: JET_PWSTR;
                               szTargetInstanceCheckpointPath: JET_PWSTR;
                               pfn: JET_PFNSTATUS
                               ): JET_ERR;  stdcall;
{$ELSE}
function JetExternalRestore2(  szCheckpointFilePath: JET_PSTR;
                               szLogPath: JET_PSTR;
                               rgrstmap: PJET_RSTMAP_A;       // __ecount( crstfilemap )
                               crstfilemap: long;
                               szBackupLogPath: JET_PSTR;
                               var LogInfo: JET_LOGINFO_A;
                               szTargetInstanceName: JET_PSTR;
                               szTargetInstanceLogPath: JET_PSTR;
                               szTargetInstanceCheckpointPath: JET_PSTR;
                               pfn: JET_PFNSTATUS
                               ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

function JetRegisterCallback(sesid: JET_SESID;
                             tableid: JET_TABLEID;
                             cbtyp: JET_CBTYP;
                             pCallback: JET_CALLBACK;
                             pvContext: Pointer;
                             out hCallbackId: JET_HANDLE
                              ): JET_ERR;  stdcall;


function JetUnregisterCallback(sesid: JET_SESID;
                               tableid: JET_TABLEID;
                               cbtyp: JET_CBTYP;
                               hCallbackId: JET_HANDLE
                                ): JET_ERR;  stdcall;

type
  _JET_INSTANCE_INFO_A = record
    hInstanceId          : JET_INSTANCE;
    szInstanceName       : PAnsiChar;
    cDatabases           : Pointer;
    szDatabaseFileName   : PPAnsiChar;
    szDatabaseDisplayName: PPAnsiChar;
    szDatabaseSLVFileName: PPAnsiChar;
  end;
  JET_INSTANCE_INFO_A = _JET_INSTANCE_INFO_A;
  PJET_INSTANCE_INFO_A = ^JET_INSTANCE_INFO_A;

  _JET_INSTANCE_INFO_W = record
    hInstanceId          : JET_INSTANCE;
    szInstanceName       : PWideChar;
    cDatabases           : Pointer;
    szDatabaseFileName   : PPWideChar;
    szDatabaseDisplayName: PPWideChar;
    szDatabaseSLVFileName: PPWideChar;
  end;
  JET_INSTANCE_INFO_W = _JET_INSTANCE_INFO_W;
  PJET_INSTANCE_INFO_W = ^JET_INSTANCE_INFO_W;

  {$IFDEF JET_UNICODE}
  JET_INSTANCE_INFO = JET_INSTANCE_INFO_W;
  {$ELSE}
  JET_INSTANCE_INFO = JET_INSTANCE_INFO_A;
  {$ENDIF}

{$IF JET_VERSION < $0600}
function JetGetInstanceInfo(out cInstanceInfo: Cardinal;
                            out paInstanceInfo: PJET_INSTANCE_INFO_A    //__deref_ecount( *pcInstanceInfo )
                              ): JET_ERR;  stdcall;
{$IFEND}

function JetGetInstanceInfoA(out cInstanceInfo: Cardinal;
                             out paInstanceInfo: PJET_INSTANCE_INFO_A    //__deref_ecount( *pcInstanceInfo )
                              ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}

function JetGetInstanceInfoW(out cInstanceInfo: Cardinal;
                             out paInstanceInfo: PJET_INSTANCE_INFO_W    //__deref_ecount( *pcInstanceInfo )
                              ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetGetInstanceInfo(out cInstanceInfo: Cardinal;
                            out paInstanceInfo: PJET_INSTANCE_INFO_W    //__deref_ecount( *pcInstanceInfo )
                              ): JET_ERR;  stdcall;
{$ELSE}
function JetGetInstanceInfo(out cInstanceInfo: Cardinal;
                            out paInstanceInfo: PJET_INSTANCE_INFO_A    //__deref_ecount( *pcInstanceInfo )
                              ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


function JetFreeBuffer(pbBuf: PAnsiChar): JET_ERR;  stdcall;

function JetSetLS(sesid  : JET_SESID;
                  tableid: JET_TABLEID;
                  ls     : JET_LS;
                  grbit  : JET_GRBIT
                ): JET_ERR;  stdcall;

function JetGetLS(sesid  : JET_SESID;
                  tableid: JET_TABLEID;
                  out ls : JET_LS;
                  grbit  : JET_GRBIT
                ): JET_ERR;  stdcall;

type
  JET_OSSNAPID = Pointer;   { Snapshot Session Identifier }

function JetOSSnapshotPrepare(out snapId: JET_OSSNAPID;
                              grbit: JET_GRBIT
                             ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}
function JetOSSnapshotPrepareInstance(snapId  : JET_OSSNAPID;
                                      instance: JET_INSTANCE;
                                      grbit   : JET_GRBIT
                                       ): JET_ERR;  stdcall;
{$IFEND}

{$IF JET_VERSION < $0600}
function JetOSSnapshotFreeze(snapId: JET_OSSNAPID;
                             out cInstanceInfo : Cardinal;
                             out paInstanceInfo: PJET_INSTANCE_INFO_A;  // __deref_ecount( *pcInstanceInfo )
                             grbit: JET_GRBIT
                               ): JET_ERR;  stdcall;
{$IFEND}

function JetOSSnapshotFreezeA(snapId: JET_OSSNAPID;
                              out cInstanceInfo : Cardinal;
                              out paInstanceInfo: PJET_INSTANCE_INFO_A;  // __deref_ecount( *pcInstanceInfo )
                              grbit: JET_GRBIT
                               ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}
function JetOSSnapshotFreezeW(snapId: JET_OSSNAPID;
                              out cInstanceInfo : Cardinal;
                              out paInstanceInfo: PJET_INSTANCE_INFO_W;  // __deref_ecount( *pcInstanceInfo )
                              grbit: JET_GRBIT
                               ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetOSSnapshotFreeze(snapId: JET_OSSNAPID;
                             out cInstanceInfo : Cardinal;
                             out paInstanceInfo: PJET_INSTANCE_INFO_W;  // __deref_ecount( *pcInstanceInfo )
                             grbit: JET_GRBIT
                               ): JET_ERR;  stdcall;
{$ELSE}
function JetOSSnapshotFreeze(snapId: JET_OSSNAPID;
                             out cInstanceInfo : Cardinal;
                             out paInstanceInfo: PJET_INSTANCE_INFO_A;  // __deref_ecount( *pcInstanceInfo )
                             grbit: JET_GRBIT
                               ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}


function JetOSSnapshotThaw(snapId: JET_OSSNAPID;
                           grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;

{$IFEND}

{$IF JET_VERSION >= $0502}
function JetOSSnapshotAbort(snapId: JET_OSSNAPID;
                            grbit: JET_GRBIT
                             ): JET_ERR;  stdcall;
{$IFEND}

{$IF JET_VERSION >= $0600}

function JetOSSnapshotTruncateLog(snapId: JET_OSSNAPID;
                                  grbit: JET_GRBIT
                                   ): JET_ERR;  stdcall;

function JetOSSnapshotTruncateLogInstance(snapId: JET_OSSNAPID;
                                          instance: JET_INSTANCE;
                                          grbit: JET_GRBIT
                                           ): JET_ERR;  stdcall;

{$IF JET_VERSION < $0600}
JetOSSnapshotGetFreezeInfoA = JetOSSnapshotGetFreezeInfo;
{$IFEND}

function JetOSSnapshotGetFreezeInfoA(snapId: JET_OSSNAPID;
                                     out cInstanceInfo: Cardinal;
                                     out paInstanceInfo: PJET_INSTANCE_INFO_A;  // __deref_ecount( *pcInstanceInfo )
                                     grbit: JET_GRBIT
                                      ): JET_ERR;  stdcall;

{$IF JET_VERSION >= $0600}
function JetOSSnapshotGetFreezeInfoW(snapId: JET_OSSNAPID;
                                     out cInstanceInfo: Cardinal;
                                     out paInstanceInfo: PJET_INSTANCE_INFO_W;  // __deref_ecount( *pcInstanceInfo )
                                     grbit: JET_GRBIT
                                      ): JET_ERR;  stdcall;

{$IFDEF JET_UNICODE}
function JetOSSnapshotGetFreezeInfo(snapId: JET_OSSNAPID;
                                    out cInstanceInfo: Cardinal;
                                    out paInstanceInfo: PJET_INSTANCE_INFO_W;  // __deref_ecount( *pcInstanceInfo )
                                    grbit: JET_GRBIT
                                      ): JET_ERR;  stdcall;
{$ELSE}
function JetOSSnapshotGetFreezeInfo(snapId: JET_OSSNAPID;
                                    out cInstanceInfo: Cardinal;
                                    out paInstanceInfo: PJET_INSTANCE_INFO_A;  // __deref_ecount( *pcInstanceInfo )
                                    grbit: JET_GRBIT
                                      ): JET_ERR;  stdcall;
{$ENDIF}
{$IFEND}

function JetOSSnapshotEnd(snapId: JET_OSSNAPID;
                          grbit: JET_GRBIT
                           ): JET_ERR;  stdcall;

{$IFEND}



{$IF JET_VERSION >= $0601}

const
  //  Options for JetConfigureProcessForCrashDump
  JET_bitDumpMinimum     = $00000001;
  //  dump minimum includes cache minimum
  JET_bitDumpMaximum     = $00000002;
  //  dump maximum includes dump minimum
  //  dump maximum includes cache maximum
  JET_bitDumpCacheMinimum    = $00000004;
  //  cache minimum includes pages that are latched
  //  cache minimum includes pages that are used for memory
  //  cache minimum includes pages that are flagged with errors
  JET_bitDumpCacheMaximum    = $00000008;
  //  cache maximum includes cache minim
  //  cache maximum includes the entire cache image
  JET_bitDumpCacheIncludeDirtyPages = $00000010;
  //  dump includes pages that are modified
  JET_bitDumpCacheIncludeCachedPages = $00000020;
  //  dump includes pages that contain valid data
  JET_bitDumpCacheIncludeCorruptedPages = $00000040;
  //  dump includes pages that are corrupted (expensive to compute)


function JetConfigureProcessForCrashDump(grbit: JET_GRBIT): JET_ERR;  stdcall;
{$IFEND}

{$ENDIF}

implementation

const
  ESENT_DLL = 'esent.dll';

{$IFDEF DYNAMIC_LINK}

procedure GetProcedureAddress(var P: Pointer; const ModuleName, ProcName: string);
var
  ModuleHandle: HMODULE;
begin
  if not Assigned(P) then begin
    ModuleHandle:=GetModuleHandle(PChar(ModuleName));
    if ModuleHandle=0 then begin
      ModuleHandle:=LoadLibrary(PChar(ModuleName));
      if ModuleHandle=0 then
        Exit;
    end;
    P:=GetProcAddress(ModuleHandle,PChar(ProcName));
    if not Assigned(P) then
      Exit;
  end;
end;

type
  TJetGetDatabaseFileInfo = function (szDatabaseName: JET_PCSTR;
                                      pvResult: Pointer;       // _bcount( cbMax )
                                      cbMax: Cardinal;
                                      InfoLevel: Cardinal
                                     ): JET_ERR;  stdcall;
var
  _JetGetDatabaseFileInfo: TJetGetDatabaseFileInfo;

function JetGetDatabaseFileInfo;
begin
  GetProcedureAddress(Pointer(@_JetGetDatabaseFileInfo),ESENT_DLL,'JetGetDatabaseFileInfo');
  if Assigned(_JetGetDatabaseFileInfo) then
    Result:=_JetGetDatabaseFileInfo(szDatabaseName,pvResult,cbMax,InfoLevel)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetInit = function (pinstance: PJET_INSTANCE): JET_ERR;  stdcall;
var
  _JetInit: TJetInit;

function JetInit;
begin
  GetProcedureAddress(Pointer(@_JetInit),ESENT_DLL,'JetInit');
  if Assigned(_JetInit) then
    Result:=_JetInit(pinstance)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetCreateInstance = function (out instance: JET_INSTANCE;
                                 szInstanceName: JET_PCSTR
                                ): JET_ERR;  stdcall;
var
  _JetCreateInstance: TJetCreateInstance;

function JetCreateInstance;
var
  em: {$IFDEF RAD9PLUS}TArithmeticExceptionMask{$ELSE}TFPUExceptionMask{$ENDIF};
begin
  GetProcedureAddress(Pointer(@_JetCreateInstance),ESENT_DLL,'JetCreateInstance');
  // if you get "floating point invalid operation" on _JetCreateInstance call then you must call Set8087CW($027F); on the start of your app
  em:=GetExceptionMask;
  SetExceptionMask({$IFDEF RAD9PLUS}exAllArithmeticExceptions{$ELSE}[exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]{$ENDIF});
  if Assigned(_JetCreateInstance) then
    Result:=_JetCreateInstance(instance,szInstanceName)
  else
    Result:=JET_errFeatureNotAvailable;
  SetExceptionMask(em);
end;

type
  TJetTerm2 = function (instance: JET_INSTANCE;
                        grbit: JET_GRBIT
                       ): JET_ERR;  stdcall;
var
  _JetTerm2: TJetTerm2;

function JetTerm2;
begin
  GetProcedureAddress(Pointer(@_JetTerm2),ESENT_DLL,'JetTerm2');
  if Assigned(_JetTerm2) then
    Result:=_JetTerm2(instance,grbit)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetSetSystemParameter = function (pinstance: PJET_INSTANCE;
                                     sesid    : JET_SESID;
                                     paramid  : Cardinal;
                                     lParam   : Pointer;
                                     szParam  : JET_PCSTR
                                    ): JET_ERR;  stdcall;
var
  _JetSetSystemParameter: TJetSetSystemParameter;

function JetSetSystemParameter;
begin
  GetProcedureAddress(Pointer(@_JetSetSystemParameter),ESENT_DLL,'JetSetSystemParameter');
  if Assigned(_JetSetSystemParameter) then
    Result:=_JetSetSystemParameter(pinstance,sesid,paramid,lParam,szparam)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetBeginSession = function (instance  : JET_INSTANCE;
                               out psesid: JET_SESID;
                               szUserName: JET_PCSTR;
                               szPassword: JET_PCSTR
                               ): JET_ERR;  stdcall;
var
  _JetBeginSession: TJetBeginSession;

function JetBeginSession;
begin
  GetProcedureAddress(Pointer(@_JetBeginSession),ESENT_DLL,'JetBeginSession');
  if Assigned(_JetBeginSession) then
    Result:=_JetBeginSession(instance,psesid,szUserName,szPassword)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetGetVersion = function (sesid: JET_SESID;
                             out wVersion: Cardinal
                            ): JET_ERR;  stdcall;
var
  _JetGetVersion: TJetGetVersion;

function JetGetVersion;
begin
  GetProcedureAddress(Pointer(@_JetGetVersion),ESENT_DLL,'JetGetVersion');
  if Assigned(_JetGetVersion) then
    Result:=_JetGetVersion(sesid,wVersion)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetAttachDatabase = function (sesid: JET_SESID;
                                 szFilename: JET_PCSTR;
                                 grbit: JET_GRBIT
                                ): JET_ERR;  stdcall;
var
  _JetAttachDatabase: TJetAttachDatabase;

function JetAttachDatabase;
begin
  GetProcedureAddress(Pointer(@_JetAttachDatabase),ESENT_DLL,'JetAttachDatabase');
  if Assigned(_JetAttachDatabase) then
    Result:=_JetAttachDatabase(sesid,szFilename,grbit)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetOpenDatabase = function (sesid: JET_SESID;
                               szFilename: JET_PCSTR;
                               szConnect: JET_PCSTR;
                               out dbid: JET_DBID;
                               grbit: JET_GRBIT
                                 ): JET_ERR;  stdcall;
var
  _JetOpenDatabase: TJetOpenDatabase;

function JetOpenDatabase;
begin
  GetProcedureAddress(Pointer(@_JetOpenDatabase),ESENT_DLL,'JetOpenDatabase');
  if Assigned(_JetOpenDatabase) then
    Result:=_JetOpenDatabase(sesid,szFilename,szConnect,dbid,grbit)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetOpenTable = function (sesid: JET_SESID;
                            dbid: JET_DBID;
                            szTableName: JET_PCSTR;
                            pvParameters: Pointer;       // _bcount( cbParameters )
                            cbParameters: Cardinal;
                            grbit: JET_GRBIT;
                            out tableid: JET_TABLEID
                              ): JET_ERR;  stdcall;
var
  _JetOpenTable: TJetOpenTable;

function JetOpenTable;
begin
  GetProcedureAddress(Pointer(@_JetOpenTable),ESENT_DLL,'JetOpenTable');
  if Assigned(_JetOpenTable) then
    Result:=_JetOpenTable(sesid,dbid,szTableName,pvParameters,cbParameters,grbit,tableid)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetMove = function (sesid: JET_SESID;
                       tableid: JET_TABLEID;
                       cRow: long;
                       grbit: JET_GRBIT
                        ): JET_ERR;  stdcall;

var
  _JetMove: TJetMove;

function JetMove;
begin
  GetProcedureAddress(Pointer(@_JetMove),ESENT_DLL,'JetMove');
  if Assigned(_JetMove) then
    Result:=_JetMove(sesid,tableid,cRow,grbit)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetGetTableColumnInfo = function (sesid: JET_SESID;
                                     tableid: JET_TABLEID;
                                     szColumnName: JET_PCSTR;
                                     pvResult: Pointer;       // _bcount( cbMax )
                                     cbMax: Cardinal;
                                     InfoLevel: Cardinal
                                     ): JET_ERR;  stdcall;
var
  _JetGetTableColumnInfo: TJetGetTableColumnInfo;

function JetGetTableColumnInfo;
begin
  GetProcedureAddress(Pointer(@_JetGetTableColumnInfo),ESENT_DLL,'JetGetTableColumnInfo');
  if Assigned(_JetGetTableColumnInfo) then
    Result:=_JetGetTableColumnInfo(sesid,tableid,szColumnName,pvResult,cbMax,InfoLevel)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetRetrieveColumns = function (sesid: JET_SESID;
                                  tableid: JET_TABLEID;
                                  pretrievecolumn: PJET_RETRIEVECOLUMN;  // _ecount( cretrievecolumn )
                                  cretrievecolumn: Cardinal
                                   ): JET_ERR;  stdcall;
var
  _JetRetrieveColumns: TJetRetrieveColumns;

function JetRetrieveColumns;
begin
  GetProcedureAddress(Pointer(@_JetRetrieveColumns),ESENT_DLL,'JetRetrieveColumns');
  if Assigned(_JetRetrieveColumns) then
    Result:=_JetRetrieveColumns(sesid,tableid,pretrievecolumn,cretrievecolumn)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetCloseTable = function (sesid: JET_SESID;
                       tableid: JET_TABLEID
                        ): JET_ERR;  stdcall;
var
  _JetCloseTable: TJetCloseTable;

function JetCloseTable;
begin
  GetProcedureAddress(Pointer(@_JetCloseTable),ESENT_DLL,'JetCloseTable');
  if Assigned(_JetCloseTable) then
    Result:=_JetCloseTable(sesid,tableid)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetCloseDatabase = function (sesid: JET_SESID;
                                dbid: JET_DBID;
                                grbit: JET_GRBIT
                                 ): JET_ERR;  stdcall;
var
  _JetCloseDatabase: TJetCloseDatabase;

function JetCloseDatabase;
begin
  GetProcedureAddress(Pointer(@_JetCloseDatabase),ESENT_DLL,'JetCloseDatabase');
  if Assigned(_JetCloseDatabase) then
    Result:=_JetCloseDatabase(sesid,dbid,grbit)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetDetachDatabase = function (sesid: JET_SESID;
                                 szFilename: JET_PCSTR
                                 ): JET_ERR;  stdcall;
var
  _JetDetachDatabase: TJetDetachDatabase;

function JetDetachDatabase;
begin
  GetProcedureAddress(Pointer(@_JetDetachDatabase),ESENT_DLL,'JetDetachDatabase');
  if Assigned(_JetDetachDatabase) then
    Result:=_JetDetachDatabase(sesid,szFilename)
  else
    Result:=JET_errFeatureNotAvailable;
end;

type
  TJetEndSession = function (sesid: JET_SESID;
                             grbit: JET_GRBIT
                            ): JET_ERR;  stdcall;
var
  _JetEndSession: TJetEndSession;

function JetEndSession;
begin
  GetProcedureAddress(Pointer(@_JetEndSession),ESENT_DLL,'JetEndSession');
  if Assigned(_JetEndSession) then
    Result:=_JetEndSession(sesid,grbit)
  else
    Result:=JET_errFeatureNotAvailable;
end;

{$ELSE}

function JetInit;  external ESENT_DLL;

{$IF JET_VERSION >= $0501}
function JetInit2;  external ESENT_DLL;
{$IFEND}


{$IF JET_VERSION >= $0600}
{$IF JET_VERSION < $0600}
JetInit3A = JetInit3;
{$IFEND}

function JetInit3A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}
function JetInit3W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetInit3;  external ESENT_DLL name 'JetInit3W';
{$ELSE}
function JetInit3;  external ESENT_DLL name 'JetInit3A';
{$ENDIF}
{$IFEND}


{$IFEND}

{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetCreateInstance;  external ESENT_DLL;
{$IFEND}

function JetCreateInstanceA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}
function JetCreateInstanceW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetCreateInstance;  external ESENT_DLL name 'JetCreateInstanceW';
{$ELSE}
function JetCreateInstance;  external ESENT_DLL name 'JetCreateInstanceA';
{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600}
function JetCreateInstance2;  external ESENT_DLL;
{$IFEND}

function JetCreateInstance2A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetCreateInstance2W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetCreateInstance2;  external ESENT_DLL name 'JetCreateInstance2W';
{$ELSE}
function JetCreateInstance2;  external ESENT_DLL name 'JetCreateInstance2A';  
{$ENDIF}
{$IFEND}

{$IFEND}

{$IF JET_VERSION >= $0600}

function JetGetInstanceMiscInfo;  external ESENT_DLL;

{$IFEND}

function JetTerm;  external ESENT_DLL;
function JetTerm2;  external ESENT_DLL;
function JetStopService;  external ESENT_DLL;

{$IF JET_VERSION >= $0501}
function JetStopServiceInstance;  external ESENT_DLL;
{$IFEND}

function JetStopBackup;  external ESENT_DLL;

{$IF JET_VERSION >= $0501}
function JetStopBackupInstance;  external ESENT_DLL;
{$IFEND}

{$IF JET_VERSION < $0600}
function JetSetSystemParameter;  external ESENT_DLL;
{$IFEND}

function JetSetSystemParameterA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetSetSystemParameterW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetSetSystemParameter;  external ESENT_DLL name 'JetSetSystemParameterW';
{$ELSE}
function JetSetSystemParameter;  external ESENT_DLL name 'JetSetSystemParameterA';  
{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600}
function JetGetSystemParameter;  external ESENT_DLL;
{$IFEND}

function JetGetSystemParameterA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetGetSystemParameterW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetSystemParameter;  external ESENT_DLL name 'JetGetSystemParameterW';
{$ELSE}
function JetGetSystemParameter;  external ESENT_DLL name 'JetGetSystemParameterA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}

{$IF JET_VERSION < $0600}
function JetEnableMultiInstance;  external ESENT_DLL;
{$IFEND}

function JetEnableMultiInstanceA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetEnableMultiInstanceW;  external ESENT_DLL;


{$IFDEF JET_UNICODE}
function JetEnableMultiInstance;  external ESENT_DLL name 'JetEnableMultiInstanceW';
{$ELSE}
function JetEnableMultiInstance;  external ESENT_DLL name 'JetEnableMultiInstanceA';  
{$ENDIF}
{$IFEND}

{$IFEND}


{$IF JET_VERSION >= $0600}
function JetGetThreadStats;  external ESENT_DLL;
{$IFEND}

{$IF JET_VERSION < $0600}
function JetBeginSession;  external ESENT_DLL;
{$IFEND}

function JetBeginSessionA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetBeginSessionW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetBeginSession;  external ESENT_DLL name 'JetBeginSessionW';
{$ELSE}
function JetBeginSession;  external ESENT_DLL name 'JetBeginSessionA';  
{$ENDIF}
{$IFEND}

function JetDupSession;  external ESENT_DLL;
function JetEndSession;  external ESENT_DLL;
function JetGetVersion;  external ESENT_DLL;
function JetIdle;  external ESENT_DLL;

{$IF JET_VERSION < $0600}
function JetCreateDatabase;  external ESENT_DLL;
{$IFEND}

function JetCreateDatabaseA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetCreateDatabaseW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetCreateDatabase;  external ESENT_DLL name 'JetCreateDatabaseW';
{$ELSE}
function JetCreateDatabase;  external ESENT_DLL name 'JetCreateDatabaseA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetCreateDatabase2;  external ESENT_DLL;
{$IFEND}

function JetCreateDatabase2A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetCreateDatabase2W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetCreateDatabase2;  external ESENT_DLL name 'JetCreateDatabase2W';
{$ELSE}
function JetCreateDatabase2;  external ESENT_DLL name 'JetCreateDatabase2A';  
{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600}
function JetAttachDatabase;  external ESENT_DLL;
{$IFEND}

function JetAttachDatabaseA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetAttachDatabaseW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetAttachDatabase;  external ESENT_DLL name 'JetAttachDatabaseW';
{$ELSE}
function JetAttachDatabase;  external ESENT_DLL name 'JetAttachDatabaseA';  
{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600}
function JetAttachDatabase2;  external ESENT_DLL;
{$IFEND}

function JetAttachDatabase2A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetAttachDatabase2W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetAttachDatabase2;  external ESENT_DLL name 'JetAttachDatabase2W';
{$ELSE}
function JetAttachDatabase2;  external ESENT_DLL name 'JetAttachDatabase2A';  
{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600} 
function JetDetachDatabase;  external ESENT_DLL;
{$IFEND}

function JetDetachDatabaseA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetDetachDatabaseW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetDetachDatabase;  external ESENT_DLL name 'JetDetachDatabaseW';
{$ELSE}
function JetDetachDatabase;  external ESENT_DLL name 'JetDetachDatabaseA';  
{$ENDIF}
{$IFEND}

{$IF JET_VERSION >= $0501} 
{$IF JET_VERSION < $0600} 
function JetDetachDatabase2;  external ESENT_DLL;
{$IFEND}

function JetDetachDatabase2A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetDetachDatabase2W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetDetachDatabase2;  external ESENT_DLL name 'JetDetachDatabase2W';
{$ELSE}
function JetDetachDatabase2;  external ESENT_DLL name 'JetDetachDatabase2A';  
{$ENDIF}
{$IFEND}

{$IFEND}


{$IF JET_VERSION < $0600}
function JetGetObjectInfo;  external ESENT_DLL;
{$IFEND}

function JetGetObjectInfoA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetGetObjectInfoW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetObjectInfo;  external ESENT_DLL name 'JetGetObjectInfoW';
{$ELSE}
function JetGetObjectInfo;  external ESENT_DLL name 'JetGetObjectInfoA';  
{$ENDIF}
{$IFEND}

{$IFDEF JET_UNICODE}
function JetGetTableInfo;  external ESENT_DLL name 'JetGetTableInfoW';
{$ELSE}
function JetGetTableInfo;  external ESENT_DLL name 'JetGetTableInfoA';
{$ENDIF}

{$IF JET_VERSION < $0600} 
function JetCreateTable;  external ESENT_DLL;
{$IFEND}

function JetCreateTableA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetCreateTableW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetCreateTable;  external ESENT_DLL name 'JetCreateTableW';
{$ELSE}
function JetCreateTable;  external ESENT_DLL name 'JetCreateTableA';  
{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600}
function JetCreateTableColumnIndex;  external ESENT_DLL;
{$IFEND}

function JetCreateTableColumnIndexA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetCreateTableColumnIndexW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetCreateTableColumnIndex;  external ESENT_DLL name 'JetCreateTableColumnIndexW';
{$ELSE}
function JetCreateTableColumnIndex;  external ESENT_DLL name 'JetCreateTableColumnIndexA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetCreateTableColumnIndex2;  external ESENT_DLL;
{$IFEND}

function JetCreateTableColumnIndex2A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetCreateTableColumnIndex2W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetCreateTableColumnIndex2;  external ESENT_DLL name 'JetCreateTableColumnIndex2W';
{$ELSE}
function JetCreateTableColumnIndex2;  external ESENT_DLL name 'JetCreateTableColumnIndex2A';  
{$ENDIF}
{$IFEND}
{$IFEND}

{$IF JET_VERSION >= $0601}

function JetCreateTableColumnIndex3A;  external ESENT_DLL;
function JetCreateTableColumnIndex3W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetCreateTableColumnIndex3;  external ESENT_DLL name 'JetCreateTableColumnIndex3W';
{$ELSE}
function JetCreateTableColumnIndex3;  external ESENT_DLL name 'JetCreateTableColumnIndex3A';
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetDeleteTable;  external ESENT_DLL;
{$IFEND}

function JetDeleteTableA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetDeleteTableW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetDeleteTable;  external ESENT_DLL name 'JetDeleteTableW';
{$ELSE}
function JetDeleteTable;  external ESENT_DLL name 'JetDeleteTableA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetRenameTable;  external ESENT_DLL;
{$IFEND}

function JetRenameTableA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetRenameTableW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetRenameTable;  external ESENT_DLL name 'JetRenameTableW';
{$ELSE}
function JetRenameTable;  external ESENT_DLL name 'JetRenameTableA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetGetTableColumnInfo;  external ESENT_DLL;
{$IFEND}

function JetGetTableColumnInfoA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetGetTableColumnInfoW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetTableColumnInfo;  external ESENT_DLL name 'JetGetTableColumnInfoW';
{$ELSE}
function JetGetTableColumnInfo;  external ESENT_DLL name 'JetGetTableColumnInfoA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetGetColumnInfo;  external ESENT_DLL;
{$IFEND}

function JetGetColumnInfoA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetGetColumnInfoW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetColumnInfo;  external ESENT_DLL name 'JetGetColumnInfoW';
{$ELSE}
function JetGetColumnInfo;  external ESENT_DLL name 'JetGetColumnInfoA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetAddColumn;  external ESENT_DLL;
{$IFEND}

function JetAddColumnA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetAddColumnW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetAddColumn;  external ESENT_DLL name 'JetAddColumnW';
{$ELSE}
function JetAddColumn;  external ESENT_DLL name 'JetAddColumnA';  

{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetDeleteColumn;  external ESENT_DLL;
{$IFEND}

function JetDeleteColumnA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetDeleteColumnW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetDeleteColumn;  external ESENT_DLL name 'JetDeleteColumnW';
{$ELSE}
function JetDeleteColumn;  external ESENT_DLL name 'JetDeleteColumnA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetDeleteColumn2;  external ESENT_DLL;
{$IFEND}

function JetDeleteColumn2A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetDeleteColumn2W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetDeleteColumn2;  external ESENT_DLL name 'JetDeleteColumn2W';
{$ELSE}
function JetDeleteColumn2;  external ESENT_DLL name 'JetDeleteColumn2A';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetRenameColumn;  external ESENT_DLL;
{$IFEND}

function JetRenameColumnA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetRenameColumnW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetRenameColumn;  external ESENT_DLL name 'JetRenameColumnW';
{$ELSE}
function JetRenameColumn;  external ESENT_DLL name 'JetRenameColumnA';  
{$ENDIF}
{$IFEND}


{$IFEND}


{$IF JET_VERSION < $0600}
function JetSetColumnDefaultValue;  external ESENT_DLL;
{$IFEND}

function JetSetColumnDefaultValueA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetSetColumnDefaultValueW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetSetColumnDefaultValue;  external ESENT_DLL name 'JetSetColumnDefaultValueW';
{$ELSE}
function JetSetColumnDefaultValue;  external ESENT_DLL name 'JetSetColumnDefaultValueA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetGetTableIndexInfo;  external ESENT_DLL;
{$IFEND}

function JetGetTableIndexInfoA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetGetTableIndexInfoW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetTableIndexInfo;  external ESENT_DLL name 'JetGetTableIndexInfoW';
{$ELSE}
function JetGetTableIndexInfo;  external ESENT_DLL name 'JetGetTableIndexInfoA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetGetIndexInfo;  external ESENT_DLL;
{$IFEND}

function JetGetIndexInfoA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetGetIndexInfoW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetIndexInfo;  external ESENT_DLL name 'JetGetIndexInfoW';
{$ELSE}
function JetGetIndexInfo;  external ESENT_DLL name 'JetGetIndexInfoA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetCreateIndex;  external ESENT_DLL;
{$IFEND}

function JetCreateIndexA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetCreateIndexW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetCreateIndex;  external ESENT_DLL name 'JetCreateIndexW';
{$ELSE}
function JetCreateIndex;  external ESENT_DLL name 'JetCreateIndexA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetCreateIndex2;  external ESENT_DLL;
{$IFEND}

function JetCreateIndex2A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetCreateIndex2W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetCreateIndex2;  external ESENT_DLL name 'JetCreateIndex2W';
{$ELSE}
function JetCreateIndex2;  external ESENT_DLL name 'JetCreateIndex2A';  
{$ENDIF}
{$IFEND}

{$IF JET_VERSION >= $0601}

function JetCreateIndex3A;  external ESENT_DLL;

function JetCreateIndex3W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetCreateIndex3;  external ESENT_DLL name 'JetCreateIndex3W';
{$ELSE}
function JetCreateIndex3;  external ESENT_DLL name 'JetCreateIndex3A';  
{$ENDIF}

{$IFEND}

{$IF JET_VERSION < $0600}
function JetDeleteIndex;  external ESENT_DLL;
{$IFEND}

function JetDeleteIndexA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetDeleteIndexW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetDeleteIndex;  external ESENT_DLL name 'JetDeleteIndexW';
{$ELSE}
function JetDeleteIndex;  external ESENT_DLL name 'JetDeleteIndexA';  
{$ENDIF}
{$IFEND}


function JetBeginTransaction;  external ESENT_DLL;
function JetBeginTransaction2;  external ESENT_DLL;
function JetCommitTransaction;  external ESENT_DLL;
function JetRollback;  external ESENT_DLL;


{$IFDEF JET_UNICODE}
function JetGetDatabaseInfo;  external ESENT_DLL name 'JetGetDatabaseInfoW';
{$ELSE}
function JetGetDatabaseInfo;  external ESENT_DLL name 'JetGetDatabaseInfoA';
{$ENDIF}

{$IF JET_VERSION < $0600}
function JetGetDatabaseFileInfo;  external ESENT_DLL;
{$IFEND}

function JetGetDatabaseFileInfoA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetGetDatabaseFileInfoW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetDatabaseFileInfo;  external ESENT_DLL name 'JetGetDatabaseFileInfoW';
{$ELSE}
function JetGetDatabaseFileInfo;  external ESENT_DLL name 'JetGetDatabaseFileInfoA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetOpenDatabase;  external ESENT_DLL;
{$IFEND}

function JetOpenDatabaseA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetOpenDatabaseW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetOpenDatabase;  external ESENT_DLL name 'JetOpenDatabaseW';
{$ELSE}
function JetOpenDatabase;  external ESENT_DLL name 'JetOpenDatabaseA';  
{$ENDIF}
{$IFEND}

function JetCloseDatabase;  external ESENT_DLL;

{$IF JET_VERSION < $0600}
function JetOpenTable;  external ESENT_DLL;
{$IFEND}

function JetOpenTableA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetOpenTableW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetOpenTable;  external ESENT_DLL name 'JetOpenTableW';
{$ELSE}
function JetOpenTable;  external ESENT_DLL name 'JetOpenTableA';
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
function JetSetTableSequential;  external ESENT_DLL;
function JetResetTableSequential;  external ESENT_DLL;
{$IFEND}

function JetCloseTable;  external ESENT_DLL;
function JetDelete;  external ESENT_DLL;
function JetUpdate;  external ESENT_DLL;

{$IF JET_VERSION >= $0502}
function JetUpdate2;  external ESENT_DLL;
{$IFEND}

function JetEscrowUpdate;  external ESENT_DLL;
function JetRetrieveColumn;  external ESENT_DLL;

function JetRetrieveColumns;  external ESENT_DLL;

{$IF JET_VERSION >= $0501}
function JetEnumerateColumns;  external ESENT_DLL;
{$IFEND}


{$IF JET_VERSION >= $0600}
function JetGetRecordSize;  external ESENT_DLL;
{$IFEND}

{$IF JET_VERSION >= $0601}
function JetGetRecordSize2;  external ESENT_DLL;
{$IFEND}

function JetSetColumn;  external ESENT_DLL;
function JetSetColumns;  external ESENT_DLL;
function JetPrepareUpdate;  external ESENT_DLL;
function JetGetRecordPosition;  external ESENT_DLL;
function JetGotoPosition;  external ESENT_DLL;
function JetGetCursorInfo;  external ESENT_DLL;
function JetDupCursor;  external ESENT_DLL;


{$IF JET_VERSION < $0600}
function JetGetCurrentIndex;  external ESENT_DLL;
{$IFEND}

function JetGetCurrentIndexA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetGetCurrentIndexW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetCurrentIndex;  external ESENT_DLL name 'JetGetCurrentIndexW';
{$ELSE}
function JetGetCurrentIndex;  external ESENT_DLL name 'JetGetCurrentIndexA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetSetCurrentIndex;  external ESENT_DLL;
{$IFEND}

function JetSetCurrentIndexA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetSetCurrentIndexW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetSetCurrentIndex;  external ESENT_DLL name 'JetSetCurrentIndexW';
{$ELSE}
function JetSetCurrentIndex;  external ESENT_DLL name 'JetSetCurrentIndexA';  
{$ENDIF}
{$IFEND}

{$IF JET_VERSION < $0600}
function JetSetCurrentIndex2;  external ESENT_DLL;
{$IFEND}

function JetSetCurrentIndex2A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetSetCurrentIndex2W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetSetCurrentIndex2;  external ESENT_DLL name 'JetSetCurrentIndex2W';
{$ELSE}
function JetSetCurrentIndex2;  external ESENT_DLL name 'JetSetCurrentIndex2A';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetSetCurrentIndex3;  external ESENT_DLL;
{$IFEND}

function JetSetCurrentIndex3A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetSetCurrentIndex3W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetSetCurrentIndex3;  external ESENT_DLL name 'JetSetCurrentIndex3W';
{$ELSE}
function JetSetCurrentIndex3;  external ESENT_DLL name 'JetSetCurrentIndex3A';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetSetCurrentIndex4;  external ESENT_DLL;
{$IFEND}

function JetSetCurrentIndex4A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetSetCurrentIndex4W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetSetCurrentIndex4;  external ESENT_DLL name 'JetSetCurrentIndex4W';
{$ELSE}
function JetSetCurrentIndex4;  external ESENT_DLL name 'JetSetCurrentIndex4A';  
{$ENDIF}
{$IFEND}


function JetMove;  external ESENT_DLL;
function JetGetLock;  external ESENT_DLL;
function JetMakeKey;  external ESENT_DLL;
function JetSeek;  external ESENT_DLL;

{$IF JET_VERSION >= $0601}
function JetPrereadKeys;  external ESENT_DLL;
{$IFEND}

function JetGetBookmark;  external ESENT_DLL;

{$IF JET_VERSION >= $0501}
function JetGetSecondaryIndexBookmark;  external ESENT_DLL;
{$IFEND}


{$IF JET_VERSION < $0600}
function JetCompact;  external ESENT_DLL;
{$IFEND}

function JetCompactA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetCompactW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetCompact;  external ESENT_DLL name 'JetCompactW';
{$ELSE}
function JetCompact;  external ESENT_DLL name 'JetCompactA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetDefragment;  external ESENT_DLL;
{$IFEND}

function JetDefragmentA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetDefragmentW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetDefragment;  external ESENT_DLL name 'JetDefragmentW';
{$ELSE}
function JetDefragment;  external ESENT_DLL name 'JetDefragmentA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetDefragment2;  external ESENT_DLL;
{$IFEND}

function JetDefragment2A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetDefragment2W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetDefragment2;  external ESENT_DLL name 'JetDefragment2W';
{$ELSE}
function JetDefragment2;  external ESENT_DLL name 'JetDefragment2A';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetDefragment3;  external ESENT_DLL;
{$IFEND}

function JetDefragment3A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetDefragment3W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetDefragment3;  external ESENT_DLL name 'JetDefragment3W';
{$ELSE}
function JetDefragment3;  external ESENT_DLL name 'JetDefragment3A';  
{$ENDIF}
{$IFEND}

{$IFEND}

{$IF JET_VERSION < $0600}
function JetSetDatabaseSize;  external ESENT_DLL;
{$IFEND}

function JetSetDatabaseSizeA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetSetDatabaseSizeW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetSetDatabaseSize;  external ESENT_DLL name 'JetSetDatabaseSizeW';
{$ELSE}
function JetSetDatabaseSize;  external ESENT_DLL name 'JetSetDatabaseSizeA';  
{$ENDIF}
{$IFEND}


function JetGrowDatabase;  external ESENT_DLL;
function JetSetSessionContext;  external ESENT_DLL;
function JetResetSessionContext;  external ESENT_DLL;
function JetGotoBookmark;  external ESENT_DLL;

{$IF JET_VERSION >= $0501}
function JetGotoSecondaryIndexBookmark;  external ESENT_DLL;
{$IFEND}

function JetIntersectIndexes;  external ESENT_DLL;
function JetComputeStats;  external ESENT_DLL;
function JetOpenTempTable;  external ESENT_DLL;
function JetOpenTempTable2;  external ESENT_DLL;
function JetOpenTempTable3;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}
function JetOpenTemporaryTable;  external ESENT_DLL;
{$IFEND}


{$IF JET_VERSION < $0600}
function JetBackup;  external ESENT_DLL;
{$IFEND}

function JetBackupA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetBackupW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetBackup;  external ESENT_DLL name 'JetBackupW';
{$ELSE}
function JetBackup;  external ESENT_DLL name 'JetBackupA';  
{$ENDIF}
{$IFEND}

{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetBackupInstance;  external ESENT_DLL;
{$IFEND}

function JetBackupInstanceA;  external ESENT_DLL;
{$IF JET_VERSION >= $0600}

function JetBackupInstanceW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetBackupInstance;  external ESENT_DLL name 'JetBackupInstanceW';
{$ELSE}
function JetBackupInstance;  external ESENT_DLL name 'JetBackupInstanceA';  
{$ENDIF}
{$IFEND}

{$IFEND}


{$IF JET_VERSION < $0600}
function JetRestore;  external ESENT_DLL;
{$IFEND}

function JetRestoreA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetRestoreW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetRestore;  external ESENT_DLL name 'JetRestoreW';
{$ELSE}
function JetRestore;  external ESENT_DLL name 'JetRestoreA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetRestore2;  external ESENT_DLL;
{$IFEND}

function JetRestore2A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetRestore2W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetRestore2;  external ESENT_DLL name 'JetRestore2W';
{$ELSE}
function JetRestore2;  external ESENT_DLL name 'JetRestore2A';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetRestoreInstance;  external ESENT_DLL;
{$IFEND}

function JetRestoreInstanceA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetRestoreInstanceW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetRestoreInstance;  external ESENT_DLL name 'JetRestoreInstanceW';
{$ELSE}
function JetRestoreInstance;  external ESENT_DLL name 'JetRestoreInstanceA';  
{$ENDIF}
{$IFEND}

{$IFEND}

function JetSetIndexRange;  external ESENT_DLL;
function JetIndexRecordCount;  external ESENT_DLL;
function JetRetrieveKey;  external ESENT_DLL;
function JetBeginExternalBackup;  external ESENT_DLL;

{$IF JET_VERSION >= $0501}
function JetBeginExternalBackupInstance;  external ESENT_DLL;
{$IFEND}

{$IF JET_VERSION < $0600}
function JetGetAttachInfo;  external ESENT_DLL;
{$IFEND}

function JetGetAttachInfoA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetGetAttachInfoW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetAttachInfo;  external ESENT_DLL name 'JetGetAttachInfoW';
{$ELSE}
function JetGetAttachInfo;  external ESENT_DLL name 'JetGetAttachInfoA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetGetAttachInfoInstance;  external ESENT_DLL;
{$IFEND}

function JetGetAttachInfoInstanceA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetGetAttachInfoInstanceW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetAttachInfoInstance;  external ESENT_DLL name 'JetGetAttachInfoInstanceW';
{$ELSE}
function JetGetAttachInfoInstance;  external ESENT_DLL name 'JetGetAttachInfoInstanceA';  
{$ENDIF}
{$IFEND}

{$IFEND}


{$IF JET_VERSION < $0600}
function JetOpenFile;  external ESENT_DLL;
{$IFEND}

function JetOpenFileA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetOpenFileW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetOpenFile;  external ESENT_DLL name 'JetOpenFileW';
{$ELSE}
function JetOpenFile;  external ESENT_DLL name 'JetOpenFileA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetOpenFileInstance;  external ESENT_DLL;
{$IFEND}

function JetOpenFileInstanceA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetOpenFileInstanceW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetOpenFileInstance;  external ESENT_DLL name 'JetOpenFileInstanceW';
{$ELSE}
function JetOpenFileInstance;  external ESENT_DLL name 'JetOpenFileInstanceA';  
{$ENDIF}
{$IFEND}

{$IFEND}

function JetReadFile;  external ESENT_DLL;

{$IF JET_VERSION >= $0501}
function JetReadFileInstance;  external ESENT_DLL;
{$IFEND}


function JetCloseFile;  external ESENT_DLL;

{$IF JET_VERSION >= $0501}
function JetCloseFileInstance;  external ESENT_DLL;
{$IFEND}

{$IF JET_VERSION < $0600}
function JetGetLogInfo;  external ESENT_DLL;
{$IFEND}

function JetGetLogInfoA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetGetLogInfoW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetLogInfo;  external ESENT_DLL name 'JetGetLogInfoW';
{$ELSE}
function JetGetLogInfo;  external ESENT_DLL name 'JetGetLogInfoA';
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetGetLogInfoInstance;  external ESENT_DLL;
{$IFEND}

function JetGetLogInfoInstanceA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetGetLogInfoInstanceW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetLogInfoInstance;  external ESENT_DLL name 'JetGetLogInfoInstanceW';
{$ELSE}
function JetGetLogInfoInstance;  external ESENT_DLL name 'JetGetLogInfoInstanceA';  
{$ENDIF}
{$IFEND}



{$IF JET_VERSION < $0600}
function JetGetLogInfoInstance2;  external ESENT_DLL;
{$IFEND}

function JetGetLogInfoInstance2A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetGetLogInfoInstance2W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetLogInfoInstance2;  external ESENT_DLL name 'JetGetLogInfoInstance2W';
{$ELSE}
function JetGetLogInfoInstance2;  external ESENT_DLL name 'JetGetLogInfoInstance2A';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION < $0600}
function JetGetTruncateLogInfoInstance;  external ESENT_DLL;
{$IFEND}

function JetGetTruncateLogInfoInstanceA;  external ESENT_DLL;


{$IF JET_VERSION >= $0600}

function JetGetTruncateLogInfoInstanceW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetTruncateLogInfoInstance;  external ESENT_DLL name 'JetGetTruncateLogInfoInstanceW';
{$ELSE}
function JetGetTruncateLogInfoInstance;  external ESENT_DLL name 'JetGetTruncateLogInfoInstanceA';  
{$ENDIF}
{$IFEND}


{$IFEND}

function JetTruncateLog;  external ESENT_DLL;

{$IF JET_VERSION >= $0501}
function JetTruncateLogInstance;  external ESENT_DLL;
{$IFEND}

function JetEndExternalBackup;  external ESENT_DLL;

{$IF JET_VERSION >= $0501}
function JetEndExternalBackupInstance;  external ESENT_DLL;
function JetEndExternalBackupInstance2;  external ESENT_DLL;
{$IFEND}


{$IF JET_VERSION < $0600}
function JetExternalRestore;  external ESENT_DLL;
{$IFEND}

function JetExternalRestoreA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetExternalRestoreW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetExternalRestore;  external ESENT_DLL name 'JetExternalRestoreW';
{$ELSE}
function JetExternalRestore;  external ESENT_DLL name 'JetExternalRestoreA';  
{$ENDIF}
{$IFEND}


{$IF JET_VERSION >= $0501}
{$IF JET_VERSION < $0600}
function JetExternalRestore2;  external ESENT_DLL;
{$IFEND}

function JetExternalRestore2A;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetExternalRestore2W;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetExternalRestore2;  external ESENT_DLL name 'JetExternalRestore2W';
{$ELSE}
function JetExternalRestore2;  external ESENT_DLL name 'JetExternalRestore2A';  
{$ENDIF}
{$IFEND}

function JetRegisterCallback;  external ESENT_DLL;


function JetUnregisterCallback;  external ESENT_DLL;


{$IF JET_VERSION < $0600}
function JetGetInstanceInfo;  external ESENT_DLL;
{$IFEND}

function JetGetInstanceInfoA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}

function JetGetInstanceInfoW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetGetInstanceInfo;  external ESENT_DLL name 'JetGetInstanceInfoW';
{$ELSE}
function JetGetInstanceInfo;  external ESENT_DLL name 'JetGetInstanceInfoA';  
{$ENDIF}
{$IFEND}


function JetFreeBuffer;  external ESENT_DLL;
function JetSetLS;  external ESENT_DLL;
function JetGetLS;  external ESENT_DLL;
function JetOSSnapshotPrepare;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}
function JetOSSnapshotPrepareInstance;  external ESENT_DLL;
{$IFEND}

{$IF JET_VERSION < $0600}
function JetOSSnapshotFreeze;  external ESENT_DLL;
{$IFEND}

function JetOSSnapshotFreezeA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}
function JetOSSnapshotFreezeW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetOSSnapshotFreeze;  external ESENT_DLL name 'JetOSSnapshotFreezeW';
{$ELSE}
function JetOSSnapshotFreeze;  external ESENT_DLL name 'JetOSSnapshotFreezeA';  
{$ENDIF}
{$IFEND}


function JetOSSnapshotThaw;  external ESENT_DLL;

{$IFEND}

{$IF JET_VERSION >= $0502}
function JetOSSnapshotAbort;  external ESENT_DLL;
{$IFEND}

{$IF JET_VERSION >= $0600}

function JetOSSnapshotTruncateLog;  external ESENT_DLL;
function JetOSSnapshotTruncateLogInstance;  external ESENT_DLL;

{$IF JET_VERSION < $0600}
JetOSSnapshotGetFreezeInfoA = JetOSSnapshotGetFreezeInfo;
{$IFEND}

function JetOSSnapshotGetFreezeInfoA;  external ESENT_DLL;

{$IF JET_VERSION >= $0600}
function JetOSSnapshotGetFreezeInfoW;  external ESENT_DLL;

{$IFDEF JET_UNICODE}
function JetOSSnapshotGetFreezeInfo;  external ESENT_DLL name 'JetOSSnapshotGetFreezeInfoW';
{$ELSE}
function JetOSSnapshotGetFreezeInfo;  external ESENT_DLL name 'JetOSSnapshotGetFreezeInfoA';
{$ENDIF}
{$IFEND}

function JetOSSnapshotEnd;  external ESENT_DLL;

{$IFEND}

{$IF JET_VERSION >= $0601}
function JetConfigureProcessForCrashDump;  external ESENT_DLL;
{$IFEND}

{$ENDIF}

end.


