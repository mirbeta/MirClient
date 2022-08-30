{*******************************************************}
{               MiTeC Common Routines                   }
{                   SQLite3 API                         }
{                                                       }
{          Copyright (c) 1997-2019 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_SqLite3Api;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, System.Variants;
     {$ELSE}
     Variants, Windows, SysUtils, Classes;
     {$ENDIF}

const
  SQLITEDLL = 'sqlite3.dll';
  SQLITEDLL64 = 'sqlite3_64.dll';

  SQLITE_OK         =  0;   // Successful result
  SQLITE_ERROR      =  1;   // SQL error or missing database
  SQLITE_INTERNAL   =  2;   // An internal logic error in SQLite
  SQLITE_PERM       =  3;   // Access permission denied
  SQLITE_ABORT      =  4;   // Callback routine requested an abort
  SQLITE_BUSY       =  5;   // The database file is locked
  SQLITE_LOCKED     =  6;   // A table in the database is locked
  SQLITE_NOMEM      =  7;   // A malloc() failed
  SQLITE_READONLY   =  8;   // Attempt to write a readonly database
  SQLITE_INTERRUPT  =  9;   // Operation terminated by sqlite_interrupt()
  SQLITE_IOERR      = 10;   // Some kind of disk I/O error occurred
  SQLITE_CORRUPT    = 11;   // The database disk image is malformed
  SQLITE_NOTFOUND   = 12;   // (Internal Only) Table or record not found
  SQLITE_FULL       = 13;   // Insertion failed because database is full
  SQLITE_CANTOPEN   = 14;   // Unable to open the database file
  SQLITE_PROTOCOL   = 15;   // Database lock protocol error
  SQLITE_EMPTY      = 16;   // (Internal Only) Database table is empty
  SQLITE_SCHEMA     = 17;   // The database schema changed
  SQLITE_TOOBIG     = 18;   // Too much data for one row of a table
  SQLITE_CONSTRAINT = 19;   // Abort due to contraint violation
  SQLITE_MISMATCH   = 20;   // Data type mismatch
  SQLITE_MISUSE     = 21;   // Library used incorrectly
  SQLITE_NOLFS      = 22;   // Uses OS features not supported on host
  SQLITE_AUTH       = 23;   // Authorization denied
  SQLITE_FORMAT     = 24;   // Auxiliary database format error
  SQLITE_RANGE      = 25;   // 2nd parameter to sqlite_bind out of range
  SQLITE_NOTADB     = 26;   // File opened that is not a database file
  SQLITE_ROW        = 100;  // sqlite_step() has another row ready
  SQLITE_DONE       = 101;  //  sqlite_step() has finished executing

  SQLITE_INTEGER = 1;
  SQLITE_FLOAT = 2;
  SQLITE_TEXT = 3;
  SQLITE_BLOB = 4;
  SQLITE_NULL = 5;


// These are the allowed values for the eTextRep argument to
// sqlite3_create_collation and sqlite3_create_function.

  SQLITE_UTF8     = 1;
  SQLITE_UTF16LE  = 2;
  SQLITE_UTF16BE  = 3;
  SQLITE_UTF16    = 4;    // Use native byte order
  SQLITE_ANY      = 5;    // sqlite3_create_function only

  SQLITE_STATIC = 0;
  SQLITE_TRANSIENT = -1;

type
  PPAnsiChar  =  ^PAnsiChar;
  PPUnicodeChar  =  ^PWideChar;
  PPValue = ^PValue;
  PValue = Pointer;

  TPAnsiCharArray = packed array[0..(MaxLongint div SizeOf(PAnsiChar))-1] of PAnsiChar;
  PPAnsiCharArray = ^TPAnsiCharArray;

  TDestructor = procedure(data:pointer); cdecl;
  TExecCallBack = function(user:pointer;
                         ncols:integer;
                         values:pPAnsiChar;
                         names:pPAnsiChar):integer; cdecl;

  TBusyHandler = function(user:pointer; count:integer):integer; cdecl;
  TFuncHandler = procedure(context:pointer; nArgs:integer; args:ppvalue); cdecl;
  TFuncFinalizer = procedure(context:pointer); cdecl;
  TUserCollation = function(user:pointer;
                          lenA:integer;
                          a:PAnsiChar;
                          lenB:integer;
                          b:PAnsiChar):integer; cdecl;

  TUserCollationNeeded = procedure(user:pointer;
                                 db:pointer;
                                 eTextRep:integer;
                                 zName:PAnsiChar); cdecl;

var
  sqlite3_libVersion: function(): PAnsiChar; cdecl;

  sqlite3_close: function(db: Pointer):integer; cdecl;
  sqlite3_exec: function(db: Pointer; SQLStatement: PAnsiChar; CallbackPtr: TExecCallBack;
                         CbParam: Pointer; ErrMsg: PPAnsiChar): integer; cdecl;

  sqlite3_last_insert_rowid: function(db: Pointer): int64; cdecl;
  sqlite3_changes: function(db: Pointer): integer; cdecl;
  sqlite3_total_changes: function(db: Pointer): integer; cdecl;
  sqlite3_interrupt: procedure(db: Pointer); cdecl;
  sqlite3_complete: function(P: PAnsiChar): integer; cdecl;
  sqlite3_busy_handler: function(db: Pointer; CallbackPtr:TBusyHandler;
                                 user:pointer):integer; cdecl;
  
  sqlite3_busy_timeout: function(db: Pointer; TimeOut: integer):integer; cdecl;
  sqlite3_free: procedure(P: PAnsiChar); cdecl;
  sqlite3_open: function(dbname: PAnsiChar; var db:pointer):integer; cdecl;
  sqlite3_errcode: function(db:pointer):integer; cdecl;
  sqlite3_errmsg: function(db:pointer):PAnsiChar; cdecl;
  sqlite3_extended_errcode: function(db: Pointer): Integer; cdecl;

  sqlite3_prepare: function(db:Pointer; Sql:PAnsiChar; nBytes:Integer; var stmt:pointer;
                            pzTail:pPAnsiChar):integer; cdecl;
  sqlite3_prepare_v2: function (db: Pointer; ASQL: PAnsiChar; ALen: Integer; var AStmt: Pointer;var ATail:PAnsiChar): Integer; cdecl;

  sqlite3_bind_double: function(stmt:pointer; idx:integer; value:double):integer; cdecl;
  sqlite3_bind_int: function(stmt:pointer; idx:integer; value:integer):integer; cdecl;
  sqlite3_bind_int64: function(stmt:pointer; idx:integer; value:int64):integer; cdecl;
  sqlite3_bind_null: function(stmt:pointer; idx:integer):integer; cdecl;
  //sqlite3_bind_value: function(stmt:pointer; idx:integer; value:pointer):integer; cdecl;
  sqlite3_bind_text: function(stmt:Pointer; idx:Integer; value:PAnsiChar;
                              size:Integer; xDel:Integer):integer; cdecl;
  sqlite3_bind_blob: function(stmt:Pointer; idx:Integer; value:pointer;
                              size:Integer; xDel:integer):integer; cdecl;
  
  sqlite3_bind_parameter_count: function(stmt:pointer):integer; cdecl;
  sqlite3_bind_parameter_name: function(stmt:pointer; idx:integer):PAnsiChar; cdecl;

  

  sqlite3_bind_parameter_index: function(stmt:pointer; zName:PAnsiChar):integer; cdecl;


  sqlite3_column_count: function(pStmt:pointer):integer; cdecl;
  sqlite3_column_name: function(pStmt:pointer; idx:integer):PAnsiChar; cdecl;
  sqlite3_column_decltype: function(pStmt:pointer; idx:integer):PAnsiChar; cdecl;
  sqlite3_step: function(pStmt:pointer):integer; cdecl;

  sqlite3_data_count: function(pStmt:pointer):integer; cdecl;

  sqlite3_column_blob: function(pStmt:pointer; col:integer):pointer; cdecl;
  sqlite3_column_bytes: function(pStmt:pointer; col:integer):integer; cdecl;
  sqlite3_column_double: function(pStmt:pointer; col:integer):double; cdecl;
  sqlite3_column_int: function(pStmt:pointer; col:integer):integer; cdecl;
  sqlite3_column_int64: function(pStmt:pointer; col:integer):int64; cdecl;
  sqlite3_column_text: function(pStmt:pointer; col:integer):PAnsiChar; cdecl;
  sqlite3_column_type: function(pStmt:pointer; col:integer):integer; cdecl;
  
  sqlite3_finalize: function(pStmt:pointer):integer; cdecl;
  sqlite3_reset: function(pStmt:pointer):integer; cdecl;
  
  sqlite3_create_function: function(db:Pointer; zFunctionName:PAnsiChar; nArg:integer;
                                    eTextRep:Integer; userData:Pointer; xFunc, xStep:TFuncHandler;
                                    xFinal:TFuncFinalizer):integer; cdecl;
  
  sqlite3_aggregate_count: function(sqlite3_context:pointer):integer;  cdecl;
  
  sqlite3_value_blob: function(v:pvalue):pointer; cdecl;
  sqlite3_value_bytes: function(v:pvalue):integer; cdecl;
  sqlite3_value_double: function(v:pvalue):double; cdecl;
  sqlite3_value_int: function(v:pvalue):integer; cdecl;
  sqlite3_value_int64: function(v:pvalue):int64; cdecl;
  sqlite3_value_text: function(v:pvalue):PAnsiChar; cdecl;
  sqlite3_value_type: function(v:pvalue):integer; cdecl;
  
  sqlite3_aggregate_context: function(context:pointer; nBytes:integer):pointer; cdecl;
  
  sqlite3_user_data: function(context:pointer):pointer; cdecl;
  
  sqlite3_get_auxdata: function(context:pointer; idx:integer):pointer; cdecl;
  sqlite3_set_auxdata: procedure(context:pointer; idx:integer;
                                data:pointer;
                                xDel:integer); cdecl;
  
  sqlite3_result_blob: procedure(context:pointer; value:pointer; size:integer;
                                xDel:integer); cdecl;
  sqlite3_result_double: procedure(context:pointer; value:double); cdecl;
  sqlite3_result_error: procedure(context:pointer; msg:PAnsiChar; len:integer); cdecl;
  sqlite3_result_int: procedure(context:pointer; value:integer); cdecl;
  sqlite3_result_int64: procedure(context:pointer; value:int64); cdecl;
  sqlite3_result_null: procedure(context:pointer); cdecl;
  sqlite3_result_text: procedure(context:pointer; value:PAnsiChar; len:integer;
                                xDel:integer); cdecl;
  sqlite3_result_value: procedure(context:pointer; value:pvalue); cdecl;

  sqlite3_create_collation: function(db:Pointer; zName:PAnsiChar; eTextRep:integer;
                                     userData:Pointer; func:TUserCollation):integer; cdecl;

  sqlite3_collation_needed: function(db:Pointer; userData:Pointer; func:TUserCollationNeeded):integer; cdecl;

  sqlite3_table_column_metadata: function(db: Pointer; const zDbName: PAnsiChar; const zTableName: PAnsiChar; const zColumnName: PAnsiChar; const pzDataType: PPAnsiChar; const pzCollSeq: PPAnsiChar; pNotNull: PInteger; pPrimaryKey: PInteger; pAutoinc: PInteger): Integer; cdecl;

  sqlite3_get_table: function(db: Pointer; const zSql: PAnsiChar; var pazResult: PPAnsiCharArray; pnRow: PInteger; pnColumn: PInteger; pzErrmsg: PPAnsiChar): Integer; cdecl;
  sqlite3_free_table: procedure(result: PPAnsiCharArray); cdecl;

  sqlite3_blob_open: function(db: Pointer; const zDb: PAnsiChar; const zTable: PAnsiChar; const zColumn: PAnsiChar; iRow: Int64; flags: Integer; var ppBlob: Pointer): Integer; cdecl;
  sqlite3_blob_close: function(pBlob: Pointer): Integer; cdecl;
  sqlite3_blob_bytes: function(pBlob: Pointer): Integer; cdecl;
  sqlite3_blob_read: function(pBlob: Pointer; Z: Pointer; N: Integer; iOffset: Integer): Integer; cdecl;
  sqlite3_blob_write: function(pBlob: Pointer; const z: Pointer; n: Integer; iOffset: Integer): Integer; cdecl;

  sqlite3_next_stmt: function(pDb: Pointer; pStmt: Pointer): Pointer; cdecl;
  sqlite3_sql: function(pStmt: Pointer): PAnsiChar; cdecl;

  sqlite3_column_database_name: function(pStmt: Pointer; N: Integer): PAnsiChar; cdecl;
  sqlite3_column_database_name16: function(pStmt: Pointer; N: Integer): PWideChar; cdecl;
  sqlite3_column_table_name: function(pStmt: Pointer; N: Integer): PAnsiChar; cdecl;
  sqlite3_column_table_name16: function(pStmt: Pointer; N: Integer): PWideChar; cdecl;
  sqlite3_column_origin_name: function(pStmt: Pointer; N: Integer): PAnsiChar; cdecl;
  sqlite3_column_origin_name16: function(pStmt: Pointer; N: Integer): PWideChar; cdecl;

  SQLiteLoaded: Boolean = false;

procedure InitSQLite(ADLLNAME: string = SQLITEDLL);

type
  TSQLiteField = record
    tableName: string;
    fieldName: string;
    fieldType: Integer;
    dataType: string;
    collationSequence: string;
    notNull: Boolean;
    primaryKey: Boolean;
    foreignKey,
    uniqueKey: Boolean;
    autoIncrement: Boolean;
    defaultValue: string;
    Value: string;
    Size: Integer;
    isNull: Boolean;
    isBLOB: Boolean;
    xmlType: string;
  end;

  TSQLiteFieldInfo = array of TSQLiteField;

  TSQLiteIndex = record
    Name: string;
    Unique,
    Primary,
    IsUnique: Boolean;
    Fields: string;
  end;

  TSQLiteIndexInfo = array of TSQLiteIndex;

  TSQLiteForeignKey = record
    refTable: string;
    fromField: string;
    toField: string;
    onUpdate: string;
    onDelete: string;
    Match: string;
  end;

  TSQLiteFKInfo = array of TSQLiteForeignKey;

function WideToUTF8Ansi(const ws: WideString): AnsiString;
function ISO8601ToDateTime(Value: string): TDateTime;
function ConnectDB(AFilename: string; var ADBHandle: Pointer): Integer;
procedure DisconnectDB(var ADBHandle: Pointer);
function GetFieldValue(AStmt: Pointer; AField: Integer): Variant;
function GetFieldByNameValue(AStmt: Pointer; AField: string): Variant;
function SQLLookupList(ADB, ASQL: string; AList: TStrings; ADelimiter: string = '='): Boolean; overload;
function SQLLookupList(ADB: Pointer; ASQL: String; AList: TStrings; ADelimiter: string = '='): Boolean; overload;
function SQLLookup(ADB, ASQL: string): Variant; overload;
function SQLLookup(ADB: Pointer; ASQL: string): Variant; overload;
function OpenSQL(ADB: Pointer; ASQL: string; var AStmt: Pointer): boolean;
procedure CloseSQL(var AStmt: Pointer);
function ExecSQL(ADB: Pointer; ASQL: string): Integer;
procedure GetFieldInfo(ADB: Pointer; ADBName,ATablename: string; var FieldInfo: TSQLiteFieldInfo);
procedure GetIndexInfo(ADB: Pointer; ATablename: string; var IndexInfo: TSQLiteIndexInfo);
procedure GetFKInfo(ADB: Pointer; ATablename: string; var FKInfo: TSQLiteFKInfo);
function CreateDeleteStmt(ADB: Pointer; ADBname,ATablename: string): string;
function CreateUpdateStmt(ADB: Pointer; ADBname,ATablename: string; var AFieldInfo: TSQLiteFieldInfo; AReadFI: Boolean = True): string;
function CreateInsertStmt(ADB: Pointer; ADBName,ATablename: string; var AFieldInfo: TSQLiteFieldInfo; AReadFI: Boolean = True; QuoteStrings: boolean = false): string;
function CreateInsertStmt1(ADB: Pointer; ADBName,ATablename: string; var AFieldInfo: TSQLiteFieldInfo; AReadFI: Boolean = True): string;
function ErrorMsg(ACode: Integer): string;
function LoadBLOB(ADB: Pointer; ADBName,ATablename,AFieldName: string; ARecNo: Int64; AStream: TStream): Boolean;
function GetXMLType(AType: string): string;

var
  sqlite_LastExecResult: string;

implementation

var
  DLLHandle: THandle = 0;

function ISO8601ToDateTime(Value: String): TDateTime;
{$IFDEF D7PLUS}
var
  FormatSettings: TFormatSettings;
begin
  if Value='' then begin
    Result:=0;
    Exit;
  end;
  {$IFDEF RAD8PLUS}
  FormatSettings:=TFormatSettings.Create(GetThreadLocale);
  {$ELSE}
  GetLocaleFormatSettings(GetThreadLocale,FormatSettings);
  {$ENDIF}
  FormatSettings.DateSeparator:='-';
  FormatSettings.ShortDateFormat:='yyyy-MM-dd';
  Result:=StrToDateTime(Value,FormatSettings);
end;
{$ELSE}
var
  ds: Char;
  sdf: string;
begin
  if Value='' then begin
    Result:=0;
    Exit;
  end;
  DateSeparator:='-';
  ShortDateFormat:='yyyy-MM-dd';
  try
    Result:=StrToDateTime(Value);
  finally
    DateSeparator:=ds;
    ShortDateFormat:=sdf;
  end;
end;
{$ENDIF}

function WideToUTF8Ansi(const ws: WideString): AnsiString;
var
  l: integer;
  f: Cardinal;
begin
  f:=0;
  if ws = '' then
    Result := ''
  else begin
    l := WideCharToMultiByte(CP_UTF8,f,@ws[1],-1,nil,0,nil,nil);
    SetLength(Result,l-1);
    if l>1 then
      WideCharToMultiByte(CP_UTF8,f,@ws[1],-1,@Result[1],l-1,nil,nil);
  end;
end;


function ConnectDB;
begin
  if Assigned(ADBHandle) then
    sqlite3_close(ADBHandle);
  Result:=sqlite3_open(PAnsiChar({$IFDEF UNICODE}WideToUTF8Ansi{$ENDIF}(AFilename)),ADBHandle);
  if Result=SQLITE_OK then
    Result:=sqlite3_busy_timeout(ADBHandle,10);
end;

procedure DisconnectDB;
begin
  if Assigned(ADBHandle) then
    sqlite3_close(ADBHandle);
  ADBHandle:=nil;  
end;

function GetFieldValue(AStmt: Pointer; AField: Integer): Variant;
begin
  Result:=null;
  case sqlite3_column_type(AStmt,AField) of
    SQLITE_TEXT: Result:=Trim(UTF8ToAnsi(sqlite3_column_text(AStmt,AField)));
    SQLITE_INTEGER: Result:=sqlite3_column_int64(AStmt,AField);
    SQLITE_FLOAT: Result:=sqlite3_column_double(AStmt,AField);
    SQLITE_NULL: Result:='';
  end;
end;

function GetFieldByNameValue(AStmt: Pointer; AField: string): Variant;
var
  i,c,idx,t: Integer;
begin
  Result:=null;
  c:=sqlite3_column_count(AStmt);
  idx:=-1;
  for i:=0 to c-1 do
    if SameText(Trim(UTF8ToAnsi(sqlite3_column_name(AStmt,i))),AField) then begin
      idx:=i;
      Break;
    end;
  if idx>-1 then begin
    t:=sqlite3_column_type(AStmt,idx);
    case t of
      SQLITE_TEXT: Result:=Trim(UTF8ToAnsi(sqlite3_column_text(AStmt,idx)));
      SQLITE_INTEGER: Result:=sqlite3_column_int64(AStmt,idx);
      SQLITE_FLOAT: Result:=sqlite3_column_double(AStmt,idx);
      SQLITE_NULL: Result:='';
    end;
  end;
end;

function OpenSQL(ADB: Pointer; ASQL: string; var AStmt: Pointer): Boolean;
var
  n: PAnsiChar;
begin
  try
    sqlite3_prepare_v2(ADB,PAnsiChar('PRAGMA synchronous=NORMAL'),-1,AStmt,n);
    if Assigned(AStmt) then begin
      sqlite3_step(Astmt);
      sqlite3_finalize(Astmt);
    end;
    //repeat
      sqlite3_prepare_v2(ADB,PAnsiChar({$IFDEF UNICODE}WideToUTF8Ansi{$ENDIF}(ASQL)),-1,Astmt,n);
      //ASQL:=n;
    //until n='';
    Result:=True;
  finally

  end;
end;

function ExecSQL;
var
  n: PAnsiChar;
begin
  Result:=-1;
  try
    sqlite3_exec(ADB,PAnsiChar({$IFDEF UNICODE}WideToUTF8Ansi{$ENDIF}(ASQL)),nil,nil,@n);
    if n='' then
      Result:=sqlite3_changes(ADB);
    sqlite_LastExecResult:=string(n);
  finally
  end;
end;

procedure CloseSQL;
begin
  if Assigned(AStmt) then
    sqlite3_finalize(AStmt);
  AStmt:=nil;
end;

function SQLLookupList(ADB: Pointer; ASQL: string; AList: TStrings; ADelimiter: string = '='): Boolean;
var
  stmt: Pointer;
  c,i: Integer;
  p: Pointer;
  s: string;
begin
  p:=nil;
  Result:=False;
  for i:=0 to AList.Count-1 do
    if Assigned(AList.Objects[i]) then
      Freemem(Pointer(AList.Objects[i]));
  AList.Clear;

  OpenSQL(ADB,ASQL,stmt);
  try
    if Assigned(Stmt) then begin
      c:=sqlite3_column_count(stmt);
      while (sqlite3_step(stmt)=SQLITE_ROW) do begin
        if c>1 then begin
          if ADelimiter='' then begin
            case sqlite3_column_type(stmt,0) of
              SQLITE_TEXT: begin
                s:=Trim(UTF8ToAnsi(sqlite3_column_text(stmt,0)));
                p:=Allocmem(Length(s)+1);
                StrPCopy(p,s);
              end;
              SQLITE_INTEGER: begin
                p:=AllocMem(SizeOf(Integer));
                PInteger(p)^:=sqlite3_column_int(stmt,0);
              end;
            end;
            if Assigned(p) then
              AList.AddObject(Trim(UTF8ToAnsi(sqlite3_column_text(stmt,1))),p);
          end else
            AList.Add(Format('%s%s%s',[Trim(UTF8ToAnsi(sqlite3_column_text(stmt,0))),
                                       ADelimiter,
                                       Trim(UTF8ToAnsi(sqlite3_column_text(stmt,1)))]));
        end else
          AList.Add(Trim(UTF8ToAnsi(sqlite3_column_text(stmt,0))));
      end;
      Result:=True;
    end;
  finally
    CloseSQL(stmt);
  end;
end;

function SQLLookupList(ADB, ASQL: string; AList: TStrings; ADelimiter: string = '='): Boolean;
var
  db: Pointer;
begin
  try
    sqlite3_open(PAnsiChar({$IFDEF UNICODE}WideToUTF8Ansi{$ENDIF}(ADB)),db);
    sqlite3_busy_timeout(db,10);
    Result:=SQLLookupList(db,ASQL,AList,ADelimiter);
  finally
    sqlite3_close(db);
  end;
end;

function SQLLookup(ADB: Pointer; ASQL: string): Variant;
var
  stmt: Pointer;
  c,i: Integer;
begin
  Result:=null;
  OpenSQL(ADB,ASQL,stmt);
  try
    if Assigned(Stmt) then begin
      c:=sqlite3_column_count(stmt);
      if (sqlite3_step(stmt)=SQLITE_ROW) then begin
        if c=1 then
          Result:=GetFieldValue(stmt,0)
        else begin
          Result:=VarArrayCreate([0,c],varVariant);
          for i:=0 to c-1 do
            Result[i]:=GetFieldValue(stmt,i);
        end;
      end;
    end;
  finally
    CloseSQL(stmt);
  end;
end;

function SQLLookup(ADB, ASQL: string): Variant;
var
  db: Pointer;
begin
  Result:=null;
  try
    sqlite3_open(PAnsiChar({$IFDEF UNICODE}WideToUTF8Ansi{$ENDIF}(ADB)),db);
    sqlite3_busy_timeout(db,10);
    Result:=SQLLookup(db,ASQL);
  finally
    sqlite3_close(db);
  end;
end;

procedure GetFieldInfo;
var
  stmt: Pointer;
  c,i,r: Integer;
  f: TSQLiteField;
  p1,p2: PAnsiChar;
begin
  Finalize(FieldInfo);
  if not Assigned(sqlite3_table_column_metadata) then
    Exit;
  OpenSQL(ADB,Format('PRAGMA table_info("%s")',[ATablename]),stmt);
  try
    i:=0;
    while (sqlite3_step(stmt)=SQLITE_ROW) do begin
      Finalize(f);
      f.FieldName:=Trim(UTF8ToAnsi(sqlite3_column_text(stmt,1)));
      f.dataType:=Trim(UTF8ToAnsi(sqlite3_column_text(stmt,2)));
      f.notNull:=sqlite3_column_int(stmt,3)=1;
      f.defaultValue:=Trim(UTF8ToAnsi(sqlite3_column_text(stmt,4)));
      f.primaryKey:=sqlite3_column_int(stmt,5)=1;
      f.fieldType:=sqlite3_column_type(stmt,i);
      if f.dataType='' then
        case f.fieldType of
          SQLITE_INTEGER: f.dataType:='integer';
          SQLITE_FLOAT: f.dataType:='float';
          SQLITE_TEXT: f.dataType:='text';
          SQLITE_BLOB: f.dataType:='BLOB';
        end;
      f.isBLOB:=(f.fieldType=SQLITE_BLOB) or (Pos('BLOB',UpperCase(f.dataType))>0);
      f.xmlType:=GetXMLType(f.dataType);
      SetLength(FieldInfo,Length(FieldInfo)+1);
      FieldInfo[High(FieldInfo)]:=f;
      Inc(i);
    end;
  finally
    CloseSQL(stmt);
  end;

  if not Assigned(sqlite3_table_column_metadata) then
    Exit;
  OpenSQL(ADB,Format('select * from "%s"."%s"',[ADBName,ATablename]),stmt);
  try
    c:=sqlite3_column_count(stmt);
    for i:=0 to c-1 do begin
      f:=FieldInfo[i];
      r:=sqlite3_table_column_metadata(ADB,PAnsiChar({$IFDEF UNICODE}WideToUTF8Ansi{$ENDIF}(ADBName)),
                                           PAnsiChar({$IFDEF UNICODE}WideToUTF8Ansi{$ENDIF}(ATableName)),
                                           PAnsiChar({$IFDEF UNICODE}WideToUTF8Ansi{$ENDIF}(f.FieldName)),
                                           @p1,@p2,@f.notNull,@f.primaryKey,@f.autoIncrement);
      if r=SQLITE_OK then
        f.collationSequence:=string(p2);
      FieldInfo[i].collationSequence:=f.collationSequence;
      FieldInfo[i].autoIncrement:=f.autoIncrement;
      FieldInfo[i].primaryKey:=f.primaryKey;
    end;
  finally
    CloseSQL(stmt);
  end;
end;

procedure GetIndexInfo;
var
  stmt,stmt1: Pointer;
  f: TSQLiteIndex;
begin
  Finalize(IndexInfo);
  if not Assigned(sqlite3_table_column_metadata) then
    Exit;
  OpenSQL(ADB,Format('PRAGMA index_list("%s")',[ATablename]),stmt);
  try
    while (sqlite3_step(stmt)=SQLITE_ROW) do begin
      Finalize(f);
      f.Name:=Trim(UTF8ToAnsi(sqlite3_column_text(stmt,1)));
      f.Primary:=SameText(Trim(UTF8ToAnsi(sqlite3_column_text(stmt,3))),'pk');
      f.Unique:=SameText(Trim(UTF8ToAnsi(sqlite3_column_text(stmt,3))),'u');
      f.IsUnique:=sqlite3_column_int(stmt,2)=1;
      OpenSQL(ADB,Format('PRAGMA index_info("%s")',[f.Name]),stmt1);
      try
        while (sqlite3_step(stmt1)=SQLITE_ROW) do begin
          f.Fields:=f.Fields+Trim(UTF8ToAnsi(sqlite3_column_text(stmt1,2)))+',';
        end;
        SetLength(f.Fields,Length(f.Fields)-1);
      finally
        CloseSQL(stmt1);
      end;
      SetLength(IndexInfo,Length(IndexInfo)+1);
      IndexInfo[High(IndexInfo)]:=f;
    end;
  finally
    CloseSQL(stmt);
  end;
end;

procedure GetFKInfo;
var
  stmt: Pointer;
  f: TSQLiteForeignKey;
begin
  Finalize(FKInfo);
  if not Assigned(sqlite3_table_column_metadata) then
    Exit;
  OpenSQL(ADB,Format('PRAGMA foreign_key_list("%s")',[ATablename]),stmt);
  try
    while (sqlite3_step(stmt)=SQLITE_ROW) do begin
      Finalize(f);
      f.refTable:=Trim(UTF8ToAnsi(sqlite3_column_text(stmt,2)));
      f.fromField:=Trim(UTF8ToAnsi(sqlite3_column_text(stmt,3)));
      f.toField:=Trim(UTF8ToAnsi(sqlite3_column_text(stmt,4)));
      f.onUpdate:=Trim(UTF8ToAnsi(sqlite3_column_text(stmt,5)));
      f.onDelete:=Trim(UTF8ToAnsi(sqlite3_column_text(stmt,6)));
      f.Match:=Trim(UTF8ToAnsi(sqlite3_column_text(stmt,7)));
      SetLength(FKInfo,Length(FKInfo)+1);
      FKInfo[High(FKInfo)]:=f;
    end;
  finally
    CloseSQL(stmt);
  end;
end;

function CreateDeleteStmt;
var
  i: Integer;
  s: string;
  fi: TSQLiteFieldInfo;
begin
  if ADBname='' then
    ADBName:='main';
  GetFieldInfo(ADB,ADBName,Atablename,fi);
  Result:=Format('delete from "%s"."%s" where ',[ADBname,ATablename]);
  for i:=0 to High(fi) do begin
    if (Pos('CHAR',Uppercase(fi[i].dataType))>0) or (Pos('TEXT',Uppercase(fi[i].dataType))>0) then
      s:='''%s'''
    else
      s:='%s';
    Result:=Result+Format('%s=%s and ',[fi[i].FieldName,s]);
  end;
  SetLength(Result,Length(Result)-5);
  Result:=Result+';';
end;

function CreateUpdateStmt;
var
  i: Integer;
  s,r: string;
begin
  if ADBname='' then
    ADBName:='main';
  if AReadFI then
    GetFieldInfo(ADB,ADBName,Atablename,AFieldInfo);
  r:='';
  for i:=0 to High(AFieldInfo) do begin
    if (Pos('CHAR',Uppercase(AFieldInfo[i].dataType))>0) or (Pos('TEXT',Uppercase(AFieldInfo[i].dataType))>0) then
      s:='''%s'''
    else
      s:='%s';
    r:=r+Format('%s=%s and ',[AFieldInfo[i].FieldName,s]);
  end;
  SetLength(r,Length(r)-5);
  Result:=Format('update "%s"."%s" set %s where %s',[ADBName,ATablename,'%s',r]);
  Result:=Result+';';
end;

function CreateInsertStmt;
var
  i: Integer;
  s,r: string;
begin
  if AReadFI then
    GetFieldInfo(ADB,ADBName,ATablename,AFieldInfo);
  r:='';
  s:='';
  for i:=0 to High(AFieldInfo) do begin
    if QuoteStrings and ((Pos('CHAR',Uppercase(AFieldInfo[i].dataType))>0) or (Pos('TEXT',Uppercase(AFieldInfo[i].dataType))>0)) then
      s:=s+'''%s'','
    else
      s:=s+'%s,';
    r:=r+AFieldInfo[i].FieldName+',';
  end;
  SetLength(r,Length(r)-1);
  SetLength(s,Length(s)-1);
  Result:=Format('insert into "%s"."%s" (%s) values (%s)',[ADBname,ATablename,r,s]);
  Result:=Result+';';
end;

function CreateInsertStmt1;
var
  i: Integer;
  r: string;
begin
  if ADBname='' then
    ADBName:='main';
  if AReadFI then
    GetFieldInfo(ADB,ADBName,ATablename,AFieldInfo);
  r:='';
  for i:=0 to High(AFieldInfo) do
    r:=r+AFieldInfo[i].FieldName+',';
  SetLength(r,Length(r)-1);
  Result:=Format('insert into "%s"."%s" (%s) values (%s)',[ADBname,ATablename,r,'%s']);
  Result:=Result+';';
end;

function LoadBLOB(ADB: Pointer; ADBName,ATablename,AFieldName: string; ARecNo: Int64; AStream: TStream): Boolean;
var
  buf: PAnsiChar;
  n: Int64;
  blob: Pointer;
  r: Integer;
begin
  Result:=False;
  r:=sqlite3_blob_open(ADB,PAnsiChar({$IFDEF UNICODE}WideToUTF8Ansi{$ENDIF}(ADBName)),
                           PAnsiChar({$IFDEF UNICODE}WideToUTF8Ansi{$ENDIF}(ATablename)),
                           PAnsiChar({$IFDEF UNICODE}WideToUTF8Ansi{$ENDIF}(AFieldName)),ARecNo,0,blob);
  if r<>SQLITE_OK then
    Exit;
  try
    n:=sqlite3_blob_bytes(blob);
    buf:=Allocmem(n);
    try
      sqlite3_blob_read(blob,buf,n,0);
      AStream.Write(buf^,n);
      AStream.Position:=0;
    finally
      Freemem(buf);
    end;
  finally
    if Assigned(blob) then
      sqlite3_blob_close(blob);
    blob:=nil;
  end;
end;

function GetXMLType(AType: string): string;
begin
  if SameText(AType,'NUMERIC') or
     SameText(AType,'INTEGER') or
     SameText(AType,'BIGINT') or
     SameText(AType,'SMALLINT') then
    Result:='i8'
  else if SameText(AType,'BLOB') then
    Result:='bin.hex'
  else if SameText(AType,'DATE') then
    Result:='date'
  else if SameText(AType,'REAL') or
          SameText(AType,'DOUBLE PRECISION') then
    Result:='r8'
  else if SameText(AType,'TIME') then
    Result:='time'
  else if SameText(AType,'DATETIME') then
    Result:='datetime'
  else
    Result:='string';
end;

function ErrorMsg;
begin
  case ACode of
    SQLITE_OK         : Result:='Successful result';
    SQLITE_ERROR      : Result:='SQL error or missing database';
    SQLITE_INTERNAL   : Result:='An internal logic error in SQLite';
    SQLITE_PERM       : Result:='Access permission denied';
    SQLITE_ABORT      : Result:='Callback routine requested an abort';
    SQLITE_BUSY       : Result:='The database file is locked';
    SQLITE_LOCKED     : Result:='A table in the database is locked';
    SQLITE_NOMEM      : Result:='A malloc() failed';
    SQLITE_READONLY   : Result:='Attempt to write a readonly database';
    SQLITE_INTERRUPT  : Result:='Operation terminated by sqlite_interrupt()';
    SQLITE_IOERR      : Result:='Some kind of disk I/O error occurred';
    SQLITE_CORRUPT    : Result:='The database disk image is malformed';
    SQLITE_NOTFOUND   : Result:='(Internal Only) Table or record not found';
    SQLITE_FULL       : Result:='Insertion failed because database is full';
    SQLITE_CANTOPEN   : Result:='Unable to open the database file';
    SQLITE_PROTOCOL   : Result:='Database lock protocol error';
    SQLITE_EMPTY      : Result:='(Internal Only) Database table is empty';
    SQLITE_SCHEMA     : Result:='The database schema changed';
    SQLITE_TOOBIG     : Result:='Too much data for one row of a table';
    SQLITE_CONSTRAINT : Result:='Abort due to contraint violation';
    SQLITE_MISMATCH   : Result:='Data type mismatch';
    SQLITE_MISUSE     : Result:='Library used incorrectly';
    SQLITE_NOLFS      : Result:='Uses OS features not supported on host';
    SQLITE_AUTH       : Result:='Authorization denied';
    SQLITE_FORMAT     : Result:='Auxiliary database format error';
    SQLITE_RANGE      : Result:='2nd parameter to sqlite_bind out of range';
    SQLITE_NOTADB     : Result:='File opened that is not a database file';
    SQLITE_ROW        : Result:='sqlite_step() has another row ready';
    SQLITE_DONE       : Result:='sqlite_step() has finished executing';
  end;
end;

procedure InitSQLite(ADLLNAME: string = SQLITEDLL);
begin
   if SQLiteLoaded then
     Exit;
   if not FileExists(ADLLNAME) then
     if FileExists(ExtractFilePath(ParamStr(0))+ExtractFilename(SQLITEDLL)) then
       ADLLNAME:=ExtractFilePath(ParamStr(0))+ExtractFilename(SQLITEDLL);
   DLLHandle:=GetModuleHandle(PChar(ExtractFilename(ADLLNAME)));
   if DLLHandle=0 then
     DLLHandle:=LoadLibrary(PChar(ADLLNAME));
   if DLLHandle<>0 then begin
     @sqlite3_libversion:=GetProcAddress(DLLHandle,'sqlite3_libversion');
     @sqlite3_close:=GetProcAddress(DLLHandle,'sqlite3_close');
     @sqlite3_exec:=GetProcAddress(DLLHandle,'sqlite3_exec');
     @sqlite3_last_insert_rowid:=GetProcAddress(DLLHandle,'sqlite3_last_insert_rowid');
     @sqlite3_changes:=GetProcAddress(DLLHandle,'sqlite3_changes');
     @sqlite3_total_changes:=GetProcAddress(DLLHandle,'sqlite3_total_changes');
     @sqlite3_interrupt:=GetProcAddress(DLLHandle,'sqlite3_interrupt');
     @sqlite3_complete:=GetProcAddress(DLLHandle,'sqlite3_complete');
     @sqlite3_busy_handler:=GetProcAddress(DLLHandle,'sqlite3_busy_handler');
     @sqlite3_busy_timeout:=GetProcAddress(DLLHandle,'sqlite3_busy_timeout');
     @sqlite3_free:=GetProcAddress(DLLHandle,'sqlite3_free');
     @sqlite3_open:=GetProcAddress(DLLHandle,'sqlite3_open');
     @sqlite3_errcode:=GetProcAddress(DLLHandle,'sqlite3_errcode');
     @sqlite3_errmsg:=GetProcAddress(DLLHandle,'sqlite3_errmsg');
     @sqlite3_prepare:=GetProcAddress(DLLHandle,'sqlite3_prepare');
     @sqlite3_prepare_v2:=GetProcAddress(DLLHandle,'sqlite3_prepare_v2');
     @sqlite3_bind_double:=GetProcAddress(DLLHandle,'sqlite3_bind_double');
     @sqlite3_bind_int:=GetProcAddress(DLLHandle,'sqlite3_bind_int');
     @sqlite3_bind_int64:=GetProcAddress(DLLHandle,'sqlite3_bind_int64');
     @sqlite3_bind_null:=GetProcAddress(DLLHandle,'sqlite3_bind_null');
     //@sqlite3_bind_value:=GetProcAddress(DLLHandle,'sqlite3_bind_value');
     @sqlite3_bind_text:=GetProcAddress(DLLHandle,'sqlite3_bind_text');
     @sqlite3_bind_blob:=GetProcAddress(DLLHandle,'sqlite3_bind_blob');
     @sqlite3_column_count:=GetProcAddress(DLLHandle,'sqlite3_column_count');
     @sqlite3_column_name:=GetProcAddress(DLLHandle,'sqlite3_column_name');
     @sqlite3_column_decltype:=GetProcAddress(DLLHandle,'sqlite3_column_decltype');
     @sqlite3_step:=GetProcAddress(DLLHandle,'sqlite3_step');
     @sqlite3_data_count:=GetProcAddress(DLLHandle,'sqlite3_data_count');
     @sqlite3_column_blob:=GetProcAddress(DLLHandle,'sqlite3_column_blob');
     @sqlite3_column_bytes:=GetProcAddress(DLLHandle,'sqlite3_column_bytes');
     @sqlite3_column_double:=GetProcAddress(DLLHandle,'sqlite3_column_double');
     @sqlite3_column_int:=GetProcAddress(DLLHandle,'sqlite3_column_int');
     @sqlite3_column_int64:=GetProcAddress(DLLHandle,'sqlite3_column_int64');
     @sqlite3_column_text:=GetProcAddress(DLLHandle,'sqlite3_column_text');
     @sqlite3_column_type:=GetProcAddress(DLLHandle,'sqlite3_column_type');
     @sqlite3_finalize:=GetProcAddress(DLLHandle,'sqlite3_finalize');
     @sqlite3_reset:=GetProcAddress(DLLHandle,'sqlite3_reset');
     @sqlite3_create_function:=GetProcAddress(DLLHandle,'sqlite3_create_function');
     @sqlite3_aggregate_count:=GetProcAddress(DLLHandle,'sqlite3_aggregate_count');
     @sqlite3_value_blob:=GetProcAddress(DLLHandle,'sqlite3_value_blob');
     @sqlite3_value_bytes:=GetProcAddress(DLLHandle,'sqlite3_value_bytes');
     @sqlite3_value_double:=GetProcAddress(DLLHandle,'sqlite3_value_double');
     @sqlite3_value_int:=GetProcAddress(DLLHandle,'sqlite3_value_int');
     @sqlite3_value_int64:=GetProcAddress(DLLHandle,'sqlite3_value_int64');
     @sqlite3_value_text:=GetProcAddress(DLLHandle,'sqlite3_value_text');
     @sqlite3_value_type:=GetProcAddress(DLLHandle,'sqlite3_value_type');
     @sqlite3_aggregate_context:=GetProcAddress(DLLHandle,'sqlite3_aggregate_context');
     @sqlite3_user_data:=GetProcAddress(DLLHandle,'sqlite3_user_data');
     @sqlite3_get_auxdata:=GetProcAddress(DLLHandle,'sqlite3_get_auxdata');
     @sqlite3_set_auxdata:=GetProcAddress(DLLHandle,'sqlite3_set_auxdata');
     @sqlite3_result_blob:=GetProcAddress(DLLHandle,'sqlite3_result_blob');
     @sqlite3_result_double:=GetProcAddress(DLLHandle,'sqlite3_result_double');
     @sqlite3_result_error:=GetProcAddress(DLLHandle,'sqlite3_result_error');
     @sqlite3_result_int:=GetProcAddress(DLLHandle,'sqlite3_result_int');
     @sqlite3_result_int64:=GetProcAddress(DLLHandle,'sqlite3_result_int64');
     @sqlite3_result_null:=GetProcAddress(DLLHandle,'sqlite3_result_null');
     @sqlite3_result_text:=GetProcAddress(DLLHandle,'sqlite3_result_text');
     @sqlite3_result_value:=GetProcAddress(DLLHandle,'sqlite3_result_value');
     @sqlite3_create_collation:=GetProcAddress(DLLHandle,'sqlite3_create_collation');
     @sqlite3_collation_needed:=GetProcAddress(DLLHandle,'sqlite3_collation_needed');
     @sqlite3_bind_parameter_count:=GetProcAddress(DLLHandle,'sqlite3_bind_parameter_count');
     @sqlite3_bind_parameter_name:=GetProcAddress(DLLHandle,'sqlite3_bind_parameter_name');
     @sqlite3_bind_parameter_index:=GetProcAddress(DLLHandle,'sqlite3_bind_parameter_index');
     @sqlite3_table_column_metadata:=GetProcAddress(DLLHandle,'sqlite3_table_column_metadata');
     @sqlite3_get_table:=GetProcAddress(DLLHandle,'sqlite3_get_table');
     @sqlite3_free_table:=GetProcAddress(DLLHandle,'sqlite3_free_table');
     @sqlite3_blob_open:=GetProcAddress(DLLHandle,'sqlite3_blob_open');
     @sqlite3_blob_close:=GetProcAddress(DLLHandle,'sqlite3_blob_close');
     @sqlite3_blob_read:=GetProcAddress(DLLHandle,'sqlite3_blob_read');
     @sqlite3_blob_write:=GetProcAddress(DLLHandle,'sqlite3_blob_write');
     @sqlite3_blob_bytes:=GetProcAddress(DLLHandle,'sqlite3_blob_bytes');
     @sqlite3_next_stmt:=GetProcAddress(DLLHandle,'sqlite3_next_stmt');
     @sqlite3_sql:=GetProcAddress(DLLHandle,'sqlite3_sql');
     @sqlite3_column_database_name:=GetProcAddress(DLLHandle,'sqlite3_column_database_name');
     @sqlite3_column_database_name16:=GetProcAddress(DLLHandle,'sqlite3_column_database_name16');
     @sqlite3_column_origin_name:=GetProcAddress(DLLHandle,'sqlite3_column_origin_name');
     @sqlite3_column_origin_name16:=GetProcAddress(DLLHandle,'sqlite3_column_origin_name16');
     @sqlite3_column_table_name:=GetProcAddress(DLLHandle,'sqlite3_column_table_name');
     @sqlite3_column_table_name16:=GetProcAddress(DLLHandle,'sqlite3_column_table_name16');
     @sqlite3_extended_errcode:=GetProcAddress(DLLHandle,'sqlite3_extended_errcode');
     SQLiteLoaded:=Assigned(sqlite3_bind_parameter_index);
   end;
end;

initialization
finalization
  if DLLHandle<>0 then
    FreeLibrary(DLLHandle);
end.




