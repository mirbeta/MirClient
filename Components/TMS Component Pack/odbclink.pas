{************************************************************************}
{ TODBCLINK component                                                    }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by                                                             }
{   TMS Software                                                         }
{   copyright © 1996-2011                                                }
{   Email : info@tmssoftware.com                                         }
{   Web : http://www.tmssoftware.com                                     }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit odbclink;

interface

uses
 winprocs,wintypes,classes,sysutils,dialogs,grids,seldlg,controls,
 comctrls,odbccst;

const
  ODBCSUCCESS = [SQL_SUCCESS,SQL_SUCCESS_WITH_INFO];
  MAJ_VER = 0; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v0.4.0.0 : Deprecated

type
 phandle = ^thandle;

 PSQLstringbuf = ^TSQLstringbuf;
 TSQLstringbuf = array[0..SQL_DATA_SIZE] of ansichar;

 TODBCStatus = (odbcOpening,odbcInserting,odbcUpdating,odbcCreating,odbcDeleting,odbcNormal);

 TODBCKeyType = (odbcNormalKey,odbcRequiredKey);

 TODBCLink = class;

 EODBCError = class(Exception);

 TODBCErrorEvent = procedure(Sender:TObject;odbcstate,odbcmsg:string;errcode:integer) of object;

 TODBCField = class(TCollectionItem)
              private
               fFieldName:string;
               fDisplayName:string;
               fSize:integer;
               fDataTypeIdx:integer;
               fKey:boolean;
               procedure SetFieldName(value:string);
               procedure SetDisplName(value:string);
              public
               ColIndex:integer;
              published
               property DisplayName:string read fDisplayName write SetDisplName;
               property FieldName:string read fFieldName write SetFieldName;
               property Size:integer read fSize write fSize;
               property DatatypeIdx:integer read fDataTypeIdx write fDataTypeIdx;
               property Key:boolean read fKey write fKey;
              end;

 TODBCFieldCollection = class(TCollection)
                        private
                         fodbclink:todbclink;
                        public
                         constructor Create(odbclink:todbclink);
                         function HasKey:boolean;
                         function GetOwner: tPersistent; override;
                        protected
                         procedure Update(Item: TCollectionItem); override;
                        end;

 TODBCTypeInfo = record
                  datatypename:ansistring;
                  prefix:ansistring;
                  suffix:ansistring;
                  creparams:ansistring;
                 end;

 TODBCConnection = class(TObject)
                   private
                    henv,hdbc,stmt,querystmt:pointer;
                    strlist:tstrings;
                    info,sql:array[0..1024] of ansichar;
                    odbctablepath,odbctablename:ansistring;
                    afieldcollection:tODBCFieldCollection;
                    fConnectOptions:ansistring;
                    fAuthentication:ansistring;
                    fUserID:ansistring;
                   public 
                    constructor Create(afc:tODBCFieldCollection);
                    destructor Destroy; override;
                    function DriverConnect(wnd:hwnd;cstr:ansistring):ansistring;
                    function BrowseConnect(wnd:hwnd;cstr:ansistring):string;
                    function DriverConnectFile(wnd:hwnd;drv,fname:ansistring):tstrings;
                    function DSNConnectFile(wnd:hwnd;dsn,fname:ansistring):tstrings;
                    function FullConnect(dsn:ansistring):integer;
                    function DataSources:tstrings;
                    function Drivers:tstrings;
                    function Tables:tstrings;
                    function HasTables:boolean;
                    function NumFields(table:ansistring):integer;
                    function Fields(table:ansistring;update:boolean):tstrings;
                    function Statistics(table:ansistring):tstrings;
                    function ExecDirect(s:ansistring):integer;
                    function ExecSQL(s:ansistring):integer;
                    function PrepSQL(s:ansistring):integer;
                    function Error:integer;
                    procedure BindCol(i,coltype:integer;ptr:pointer;sz:smallint;var avail:longint);
                    function Fetch:boolean;
                    function GetInfoInt(i:integer):integer;
                    function GetInfoStr(i:integer):string;
                    function DateField(s:string): TDatetime;
                    function DataSourceFileExt(ext:ansistring):tstrings;
                    function DataSourceFileUse(drv:ansistring):integer;
                    function DataSourceFilter:string;
                    function GetTypeInfo(DatatypeIdx:integer):TODBCTypeInfo;
                    function BindParam(parnum:smallint;par:pointer;parintype,pardbtype,parlen:smallint):integer;
                    function Exec:integer;
                    function FreeSTMT:integer;
                    procedure SetConnectOptions(const s:ansistring);
                   end;

 TODBCStatement = class(TObject)
                  private
                    sqlstmt:pointer;
                    odbcConnection:TODBCConnection;
                   public 
                    constructor Create(aODBCConnection:TODBCConnection);
                    destructor Destroy; override;
                    function BindCol(i,coltype:integer;ptr:pointer;sz:smallint;var avail:longint):boolean;
                    function BindParam(parnum:smallint;par:pointer;parintype,pardbtype,parlen:smallint):integer;
                    function Fetch:boolean;
                    function ExtendedFetch(fetchtype:word;row:longint;var numrows:longint;rowstatus:pointer):boolean;
                    function Prep(s:ansistring):boolean;
                    function ExecDirect(s:ansistring):boolean;
                    function Execute:boolean;
                    function Cancel:boolean;
                    function Columns:integer;
                    function Rows:integer;
                    function Error:integer;
                    function SetCursorName(cursname:ansistring):boolean;
                    function GetCursorName:string;
                    function SetPos(row,option,lock:word):boolean;
                    function SetOption(option:word;param:integer):boolean;
                    function Attribute(i,desc:word):integer;
                   end;

 TODBCLink = class(TComponent)
             private
              fAuthentication:ansistring;
              fUserID:ansistring;
              fActive:boolean;
              fSQL:tstringlist;
              fDataSource:ansistring;
              fDriver:ansistring;
              fTable:ansistring;
              fDBFile:ansistring;
              fResultList:tstrings;
              fGrid:tStringGrid;
              fListView:tListView;
              fFields:tODBCFieldCollection;
              fShowFields:boolean;
              fShowErrors:boolean;
              fOnODBCError:TODBCErrorEvent;
              fODBCStatus:tODBCStatus;
              fKeyType:TODBCKeyType;
              procedure SetActive(avalue:boolean);
              procedure SetTable(avalue:ansistring);
              procedure SetDriver(avalue:ansistring);
              procedure SetDataSource(avalue:ansistring);
              procedure SetSQL(avalue:tstringlist);
              procedure SetFieldCollection(value:tODBCFieldCollection);
              function GetVersion: string;
              procedure SetVersion(const Value: string);
             protected
              function GetVersionNr: Integer; virtual;
              procedure Loaded; override;
             public
              constructor Create(aOwner:tComponent); override;
              destructor Destroy; override;
              procedure OpenDatabase;
              procedure SaveDatabase;
              function BrowseDatabase(doOpen:boolean):boolean;
              function DataSources:tstrings;
              function Drivers:tstrings;
              function Tables:tstrings;
              function DriverForExtension(const S:string):string;
              function HasDataType(sqltype:integer):boolean;
              function CreateDataSource(szDriver,szAttr:ansistring):boolean;
              function InsertRows(RowIndex,RowCount:integer):boolean;
              function UpdateRows(RowIndex,RowCount:integer):boolean;
              function DeleteRows(RowIndex,RowCount:integer):boolean;
              property Status:tODBCStatus read fODBCStatus;
              property KeyType:tODBCKeyType read fKeyType write fKeyType;
             published
              property Active:boolean read fActive write SetActive;
              property SQL:TStringlist read fSQL write SetSQL;
              property DataSource:ansistring read FDataSource write SetDataSource;
              property Driver:ansistring read FDriver write SetDriver;
              property Table:ansistring read FTable write SetTable;
              property Databasefile:ansistring read fDBFile write fDBFile;
              property Grid:TStringGrid read fGrid write fGrid;
              property ListView:tListView read fListView write fListview;
              property Fields:TODBCFieldCollection read fFields write SetFieldCollection;
              property ShowFields:boolean read fShowFields write fShowFields;
              property ShowErrors:boolean read fShowErrors write fShowErrors;
              property UserID:ansistring read fUserID write fUserID;
              property Authentication:ansistring read fAuthentication write fAuthentication;
              property OnODBCError:TODBCErrorEvent read fOnODBCError write fOnODBCError;
              property Version: string read GetVersion write SetVersion;
             end;
{
 TODBCDataSourceProperty = class(TStringProperty)
                         public
                          function GetAttributes:TPropertyAttributes; override;
                          procedure GetValues(Proc:TGetStrProc); override;
                         end;

 TODBCDriverProperty = class(TStringProperty)
                         public
                          function GetAttributes:TPropertyAttributes; override;
                          procedure GetValues(Proc:TGetStrProc); override;
                         end;

 TODBCTableProperty = class(TStringProperty)
                         public
                          function GetAttributes:TPropertyAttributes; override;
                          procedure GetValues(Proc:TGetStrProc); override;
                         end;

 TODBCFieldProperty = class(TIntegerProperty)
                         public
                          function GetAttributes:TPropertyAttributes; override;
                          procedure GetValues(Proc:TGetStrProc); override;
                          procedure SetValue(const value:string); override;
                          function GetValue:string; override;
                         end;

 TODBCFieldNameProperty = class(TStringProperty)
                         public
                          function GetAttributes:TPropertyAttributes; override;
                          procedure GetValues(Proc:TGetStrProc); override;
                         end;
}
function StrListToPas(p: pchar;len:word): string;
function GetListParam(par,s:string): string;
function ReplaceStr(const S, Srch, Replace: string): string;
function UpcaseStr(const S:string): string;
function NCPos(const Srch,S:string): integer;

function SQLAllocEnv(var phenv:pointer):smallint; stdcall;
function SQLAllocConnect(henv:pointer;var phdbc:pointer):smallint; stdcall;
function SQLAllocStmt(hdbc:pointer;var pstmt:pointer):smallint; stdcall;
function SQLConnect(hdbc:pointer;szDSN:pansichar;cbDSN:smallint;szUID:pansichar;cbUID:smallint;szAuth:pansichar;cbAuth:smallint):smallint; stdcall;
function SQLDriverConnect(hdbc:pointer;hwindow:hwnd;ConnStr:pansichar;
                          cbStr:smallint;szCompl:pansichar;cbMax:smallint;cbCompl:pointer;compl:word):smallint; stdcall;
function SQLExecDirect(stmt:pointer;cmd:pansichar;cbCMD:longint):smallint; stdcall;
function SQLExecute(stmt:pointer):smallint; stdcall;
function SQLDisconnect(hdbc:pointer):smallint; stdcall;
function SQLFreeConnect(hdbc:pointer):smallint; stdcall;
function SQLFreeEnv(henv:pointer):smallint; stdcall;
function SQLFreeStmt(stmt:pointer;foption:word):smallint; stdcall;
function SQLError(henv,hdbc:pointer;hstmt:pointer;state:pansichar;var errcode:integer;errmsg:pansichar;count:word;var pberr:smallint):smallint; stdcall;
function SQLPrepare(stmt:pointer;sqlstr:pansichar;cbMax:longint):smallint; stdcall;
function SQLBindParameter(stmt:pointer;paramnum:word;fparamtype,ctype,sqltype:smallint;coldef:longint;
                             scale:smallint;buf:pointer;bufsize:longint;pbuflen:pointer):smallint; stdcall;
function SQLSetParam(stmt:pointer;paramnum:word;ctype,sqltype:smallint;coldef:longint;
                             scale:integer;buf:pointer;pbuflen:pointer):smallint; stdcall;
function SQLNumResultCols(stmt:pointer;var numcols:smallint):smallint; stdcall;
function SQLDescribeCol(stmt:pointer;icol:integer;colname:pansichar;cbmax:smallint;var pcollen,psqltype,plcoldef,pscale,pnull:smallint):smallint; stdcall;
function SQLBindCol(stmt:pointer;icol,fctype:smallint;data:pointer;cbmax:longint;var datalen:longint):smallint; stdcall;
function SQLFetch(stmt:pointer):smallint; stdcall;
function SQLExtendedFetch(stmt:pointer;fetchtype:word;row:longint;var numrows:longint;rowstatus:pointer):smallint; stdcall;
function SQLTables(stmt:pointer;szTableQualifier:pansichar;cbTableQualifier:smallint;szTableOwner:pansichar;cbTableOwner:smallint;
                                szTableName:pansichar;cbTableName:smallint;szTableType:pansichar;cbTableType:smallint):smallint; stdcall;
function SQLColumns(stmt:pointer;szTableQualifier:pansichar;cbTableQualifier:smallint;szTableOwner:pansichar;cbTableOwner:smallint;
                                szTableName:pansichar;cbTableName:smallint;szColumnName:pansichar;cbColumnName:smallint):smallint; stdcall;

function SQLGetInfo(hdbc:pointer;finfotype:word;retbuf:pointer;buflen:smallint;var buflenret:smallint):smallint; stdcall;
function SQLGetTypeInfo(stmt:pointer;fsqltype:word):smallint; stdcall;
function SQLGetFunctions(hdbc:pointer;functype:word;retfunc:pointer):smallint; stdcall;
function SQLDataSources(henv:pointer;direction:word;szDSN:pansichar;cbDSN:word;var pbDSN:word;
                                                    szDescr:pansichar;cbDescr:word;var pbDescr:word):smallint; stdcall;
function SQLDrivers(henv:pointer;direction:word;szDescr:pansichar;cbDescr:word;var pbDescr:word;
                                                szAttr:pansichar;cbAttr:word;var pbAttr:word):smallint; stdcall;
function SQLConfigDataSource(HWND:hwnd;fRequest:WORD;lpszDriver,lpszAttributes:pansichar):boolean; stdcall;
function SQLSetConnectOption(hdbc:pointer;fOption:WORD;vParam:integer):smallint; stdcall;

function SQLBrowseConnect(hdbc:pointer;szConnStrIn:pansichar;cbConnStrIn:smallint;
                                       szConnStrOut:pansichar;cbConnStrOutMax:smallint;var pcbConnStrOut:smallint):smallint; stdcall;
function SQLCancel(stmt:pointer):smallint; stdcall;
function SQLSetPos(hstmt:pointer; irow,fOption,fLock:word):smallint; stdcall;
function SQLGetStmtOption(hstmt:pointer; fOption:word ;pvParam:pointer):smallint; stdcall;

function SQLSetCursorName(hstmt:pointer;szCursor:pansichar;cbCursor:smallint):smallint; stdcall;
function SQLGetCursorName(hstmt:pointer;szCursor:pansichar;cbCursorMax:smallint;var pcbCursor:smallint):smallint; stdcall;
function SQLRowCount(hstmt:pointer;var pcrow:integer):smallint; stdcall;
function SQLGetData(hstmt:pointer;icol,fCType:smallint;rgbValue:pointer;cbValueMax:integer;var pcbValue:integer):smallint; stdcall;
function SQLPutData(hstmt:pointer;rgbValue:pointer;cbValue:integer):smallint; stdcall;
function SQLSetScrollOptions(hstmt:pointer; fConcurrency:word;crowKeyset:integer;crowRowset:smallint):smallint; stdcall;
function SQLSetStmtOption(hstmt:pointer;fOption:word;vParam:integer):smallint; stdcall;
function SQLPrimaryKeys(hstmt:pointer;szTableQualifier:pansichar;cbTableQualifier:smallint;
                                      szTableOwner:pansichar;cbTableOwner:smallint;
                                      szTableName:pansichar;cbTableName:smallint):smallint; stdcall;
function SQLColAttributes(hstmt:pointer;icol,fDesc:smallint;pDesc:pointer;cbDesc:word ;var pcbDesc:word;var pfDesc:longint):smallint; stdcall;
function SQLStatistics(hstmt:pointer;szTableQualifier:pansichar;cbTableQualifier:smallint;
                                     szTableOwner:pansichar;cbTableOwner:smallint;
                                     szTableName:pansichar;cbTableName:smallint;fUnique,fAccuracy:word):smallint; stdcall;


implementation

//helper function

function StrListToPas(p:pchar;len:word): string;
var
 i:integer;
begin
 result:='';
 for i:=0 to len do
  begin
   if (char(p^)<>#0) then result:=result+char(p^) else result:=result+';';
   inc(p);
  end;
end;

function GetListParam(par,s:string):string;
begin
 result:='';
 if (pos(par,s)>0) then
  begin
   delete(s,1,pos(par,s)); //delete until identifier
   delete(s,1,pos('=',s)); //delete until equal
   if (pos(';',s)>0) then result:=copy(s,1,pos(';',s)-1) else result:=s;
  end;
end;

function AStrPas(pa: PAnsiChar): string;
begin
  Result := string(StrPas(pa));
end;

function ReplaceStr(const S, Srch, Replace: string): string;
var
  I: Integer;
  Source: string;
begin
  Source := S;
  Result := '';
  repeat
    I := Pos(Srch, Source);
    if I > 0 then begin
      Result := Result + Copy(Source, 1, I - 1) + Replace;
      Source := Copy(Source, I + Length(Srch), MaxInt);
    end
    else Result := Result + Source;
  until I <= 0;
end;

function UpcaseStr(const S:string):string;
var
 i:integer;
begin
 result:='';
 for i:=1 to length(s) do result:=result+upcase(s[i]);
end;

function NCPos(const Srch,S: string):integer;
begin
  result:=pos(UpcaseStr(Srch),UpcaseStr(S));
end;

procedure msg(s:ansistring);
begin
{$IFDEF TMSDEBUG}
 messagedlg(string(s),mtinformation,[mbok],0);
{$ENDIF}
end;

constructor TODBCConnection.Create(afc:TODBCFieldCollection);
begin
 inherited Create;

 if (SQLAllocEnv(henv)<>SQL_SUCCESS) then
   raise EODBCError.Create('Cannot allocate ODBC handle');

 if (SQLAllocConnect(henv,hdbc)<>SQL_SUCCESS) then
   raise EODBCError.Create('Cannot allocate ODBC connection');

 strlist:=tstringlist.create;
 querystmt:=nil;

 afieldcollection:=afc;
end;

destructor TODBCConnection.Destroy;
begin
  if (querystmt<>nil) then SQLFreeStmt(querystmt,SQL_CLOSE);
  strlist.free;

  SQLFreeConnect(hdbc);
  SQLFreeEnv(henv);
  inherited Destroy;
end;

function TODBCConnection.FullConnect(dsn: ansistring):integer;
var
  buf,uid,auth:array[0..255] of ansichar;
begin
 strpcopy(buf,dsn);
 strpcopy(uid,'userid');
 strpcopy(auth,'password');

 result:=SQLConnect(hdbc,buf,strlen(buf),nil,0,nil,0);
 if result=SQL_SUCCESS then msg('full connect ok')
 else
  error;
end;

function TODBCConnection.BrowseConnect(wnd:hwnd;cstr:ansistring):string;
var
 buf,res:array[0..255] of ansichar;
 i:smallint;
begin
 strpcopy(buf,cstr);
 strpcopy(res,'');
 if (SQLBrowseConnect(hdbc,buf,255,res,255,i) in [SQL_NEED_DATA,SQL_SUCCESS]) then
  msg(res)
 else
  error;
end;

function TODBCConnection.DriverConnect(wnd:hwnd;cstr:ansistring): ansistring;
var
  buf,res:array[0..255] of ansichar;
  i:smallint;
begin
  strpcopy(buf,cstr);
  strcopy(res,'');
  if SQLDriverConnect(hdbc,wnd,buf,sizeof(buf),res,sizeof(res),@i,SQL_DRIVER_NOPROMPT)=SQL_ERROR then
    Error;
  result := Strpas(res);
end;

function TODBCConnection.Drivers: TStrings;
var
 descr,attr:array[0..255] of ansichar;
 pbdescr,pbAttr:word;

begin
 strlist.clear;
 result:=strlist;

 if SQLDrivers(henv,SQL_FETCH_FIRST,descr,sizeof(descr),pbdescr,attr,sizeof(attr),pbAttr)=SQL_SUCCESS then
  begin
   strlist.add(AStrpas(descr));
   while SQLDrivers(henv,SQL_FETCH_NEXT,descr,sizeof(descr),pbdescr,attr,sizeof(attr),pbAttr)=SQL_SUCCESS do
    begin
     strlist.add(AStrpas(descr));
    end;
  end;
end;




function TODBCConnection.DataSources:tstrings;
var
 dsn,descr:array[0..255] of ansichar;
 cbdsn,cbdescr:word;
begin
 strlist.clear;

 result:=strlist;

 if SQLDataSources(henv,SQL_FETCH_FIRST,dsn,sizeof(dsn),cbDSN,descr,sizeof(descr),cbdescr)=SQL_SUCCESS then
    strlist.add(AStrpas(dsn))
 else exit;

 while SQLDataSources(henv,SQL_FETCH_NEXT,dsn,sizeof(dsn),cbDSN,descr,sizeof(descr),cbdescr)=SQL_SUCCESS do
   strlist.add(AStrpas(dsn));

end;

function TODBCConnection.HasTables:boolean;
var
 cbInfo:smallint;
begin
 result:=false;
 if (SQLGetInfo(hdbc,SQL_ACCESSIBLE_TABLES,@info,sizeof(info),cbInfo)=SQL_SUCCESS) then
    result:=(strcomp(info,'Y')=0);
end;

function TODBCConnection.Tables: TStrings;
var
 tablequalifier,qualifier:array[0..128] of ansichar;
 tabletype:array[0..128] of ansichar;
 tableowner:array[0..128] of ansichar;
 tablename:array[0..128] of ansichar;
 remarks:array[0..254] of ansichar;

 tablequalifierlen:longint;
 tabletypelen:longint;
 tableownerlen:longint;
 tablenamelen:longint;
 remarklen:longint;

begin
  Result := strlist;
  strlist.clear;

  if not hastables then
    Strlist.add(string(odbctablename));

  if (SQLAllocSTMT(hdbc,stmt)<>SQL_SUCCESS) then exit;

  strpcopy(qualifier,odbctablepath);

 if (SQLTables(stmt,qualifier,SQL_NTS,nil,SQL_NTS,nil,SQL_NTS,'TABLE',SQL_NTS)=SQL_SUCCESS) then
  begin
     tablequalifierlen:=128;
     tableownerlen:=128;
     tablenamelen:=128;
     tabletypelen:=128;
     remarklen:=254;

     SQLBindCol(stmt,1,SQL_C_CHAR,@tablequalifier,sizeof(tablequalifier),tablequalifierlen);
     SQLBindCol(stmt,2,SQL_C_CHAR,@tableowner,sizeof(tableowner),tableownerlen);
     SQLBindCol(stmt,3,SQL_C_CHAR,@tablename,sizeof(tablename),tablenamelen);
     SQLBindCol(stmt,4,SQL_C_CHAR,@tabletype,sizeof(tabletype),tabletypelen);
     SQLBindCol(stmt,5,SQL_C_CHAR,@remarks,sizeof(remarks),remarklen);

     while SQLFetch(stmt) in ODBCSUCCESS do
       begin
        {$IFDEF TMSDEBUG}
        outputdebugstring('result set');
        outputdebugstring(tableowner);
        outputdebugstring(tablename);
        outputdebugstring(tabletype);
        outputdebugstring(tablequalifier);
        {$ENDIF}
        strlist.add(AStrpas(tablename));
       end;

     SQLFreeSTMT(stmt,SQL_CLOSE);
     stmt:=nil;
  end;

end;

function TODBCConnection.NumFields(table:ansistring):integer;
var
  numcols:smallint;

begin
  Result:=-1;

  strpcopy(sql,'SELECT * FROM ['+table+']');

  if (SQLAllocStmt(hdbc,stmt)=SQL_SUCCESS) then
  begin
   if (SQLExecDirect(stmt,sql,SQL_NTS)=SQL_SUCCESS) then
     begin
      SQLNumResultCols(stmt,numcols);
      result:=numcols;
     end;
   SQLFreeSTMT(stmt,SQL_CLOSE);
   stmt:=nil;
  end;
end;

function TODBCConnection.Statistics(table:ansistring):tstrings;
var
 tbl:array[0..128] of ansichar;
 tablequalifier:array[0..128] of ansichar;
 tableowner:array[0..128] of ansichar;
 tablename:array[0..128] of ansichar;
 indexname:array[0..128] of ansichar;
 indexqual:array[0..128] of ansichar;
 unique,infotype:array[0..128] of ansichar;

 tablequalifierlen:longint;
 tableownerlen:longint;
 tablenamelen:longint;
 indexnamelen:longint;
 indexquallen:longint;
 uniquelen,infotypelen:longint;


begin
 result:=strlist;
 strlist.clear;

 if (SQLAllocSTMT(hdbc,querystmt)=SQL_SUCCESS) then
  begin
   strpcopy(tbl,table);

   SQLStatistics(querystmt,nil,0,nil,0,tbl,SQL_NTS,SQL_INDEX_ALL,SQL_ENSURE);

   tablequalifierlen:=128;
   tableownerlen:=128;
   tablenamelen:=128;
   indexnamelen:=128;
   indexquallen:=128;
   uniquelen:=128;
   infotypelen:=128;

   SQLBindCol(querystmt,1,SQL_C_CHAR,@tablequalifier,sizeof(tablequalifier),tablequalifierlen);
   SQLBindCol(querystmt,2,SQL_C_CHAR,@tableowner,sizeof(tableowner),tableownerlen);
   SQLBindCol(querystmt,3,SQL_C_CHAR,@tablename,sizeof(tablename),tablenamelen);
   SQLBindCol(querystmt,4,SQL_C_CHAR,@unique,sizeof(unique),uniquelen);
   SQLBindCol(querystmt,5,SQL_C_CHAR,@indexqual,sizeof(indexqual),indexquallen);
   SQLBindCol(querystmt,6,SQL_C_CHAR,@indexname,sizeof(indexname),indexnamelen);
   SQLBindCol(querystmt,7,SQL_C_CHAR,@infotype,sizeof(infotype),infotypelen);

   while SQLFetch(querystmt) in ODBCSUCCESS do
    begin
     {
     outputdebugstring('result set');
     outputdebugstring(tableowner);
     outputdebugstring(tablename);
     outputdebugstring(columnname);
     outputdebugstring(indexqual);
     outputdebugstring(indexname);
     outputdebugstring(infotype);
     }
    end;

   SQLFreeSTMT(querystmt,SQL_CLOSE);
   querystmt:=nil;
  end;


end;

function TODBCConnection.Fields(table:ansistring;update:boolean):TStrings;
var
 numcols,i:smallint;
 cbColLen,colType,Collen,scale,nullable:smallint;
 cbDesc:word;
 pfDesc:longint;

begin
 result:=strlist;
 strlist.clear;

 if update then
  if assigned(aFieldCollection) then aFieldCollection.Clear;

 strpcopy(sql,'SELECT * FROM ['+table+']');

 if (SQLAllocSTMT(hdbc,stmt)=SQL_SUCCESS) then
  begin
   if (SQLExecDirect(stmt,sql,SQL_NTS)=SQL_SUCCESS) then
     begin
      SQLNumResultCols(stmt,numcols);
      for i:=1 to numcols do
       begin
        SQLDescribeCol(stmt,i,@info,sizeof(info),cbcollen,coltype,collen,scale,nullable);

        SQLColAttributes(stmt,i,SQL_COLUMN_TYPE,nil,0,cbDesc,pfDesc);

        strlist.add(AStrpas(info));
        {$IFDEF TMSDEBUG}
        outputdebugstring(pchar(strpas(info)+' type ='+inttostr(pfDesc)));
        {$ENDIF}
        if update then
         if assigned(aFieldCollection) then
          with aFieldCollection.Add as TODBCField do
           begin
            FieldName:=AStrpas(info);
            DatatypeIdx:=pfDesc;
            Size:=collen;
           end;

       end;
     end;
   SQLFreeSTMT(stmt,SQL_CLOSE);
   stmt:=nil;
  end;
end;

procedure TODBCConnection.SetConnectOptions(const s:ansistring);
begin
  FConnectOptions:=s;
end;

function TODBCConnection.ExecDirect(s:ansistring):integer;
var
 numcols:smallint;

begin
 if (querystmt<>nil) then SQLFreeStmt(querystmt,SQL_CLOSE);
 SQLAllocStmt(hdbc,querystmt);
 strpcopy(info,s);
 if SQLExecDirect(querystmt,info,SQL_NTS)<>SQL_SUCCESS then
   raise EODBCError.Create('SQLExecDirect failed');
 SQLNumResultCols(querystmt,numcols);
 result:=numcols;
end;

function TODBCConnection.Error:integer;
var
 state,errmsg:array[0..SQL_MAX_MESSAGE_LENGTH-1] of ansichar;
 errcode:integer;
 pberr:smallint;
 odbclink:todbclink;

begin
 SQLError(henv,hdbc,querystmt,state,errcode,errmsg,sizeof(errmsg),pberr);

 if assigned(aFieldCollection) then
  begin
   odbclink:=(aFieldCollection as TODBCFieldCollection).GetOwner as TODBCLink;

   if assigned(odbclink) then
    begin
     if odbclink.ShowErrors then
      messagedlg('ODBC error in state : '+AStrpas(state)+#13+
                 'Error code : '+inttohex(errcode,4)+#13+
                 'Error message : '+AStrpas(errmsg),mtError,[mbok],0);

     if assigned(odbclink.OnODBCError) then
     odbclink.OnODBCError(odbclink,AStrpas(state),AStrpas(errmsg),errcode);
    end;
  end;

 result:=errcode;
end;

function TODBCConnection.PrepSQL(s:ansistring):integer;
begin
 result:=-1;
 if (querystmt<>nil) then SQLFreeStmt(querystmt,SQL_CLOSE);
 SQLAllocStmt(hdbc,querystmt);
 strpcopy(info,s);
 if SQLPrepare(querystmt,info,SQL_NTS)<>SQL_SUCCESS then
   Error
 else
   result:=0;    
end;

function TODBCConnection.BindParam(parnum:smallint;par:pointer;parintype,pardbtype,parlen:smallint):integer;
begin
 result:=SQLBindParameter(querystmt,parnum,SQL_PARAM_INPUT,parintype,pardbtype,parlen,0,par,0,nil);
end;

function TODBCConnection.FreeSTMT:integer;
begin
 result:=SQLFreeSTMT(querystmt,SQL_CLOSE);
 stmt:=nil;
end;

function TODBCConnection.Exec:integer;
begin
 result:=SQLExecute(querystmt);
end;

function TODBCConnection.ExecSQL(s:ansistring):integer;
begin
 result:=-1;
 if (querystmt<>nil) then SQLFreeStmt(querystmt,SQL_CLOSE);
 SQLAllocStmt(hdbc,querystmt);
 if SQLSetConnectOption(hdbc,SQL_ACCESS_MODE,SQL_MODE_READ_WRITE)<>SQL_SUCCESS then
   Error;
 strpcopy(info,s);
 if SQLExecDirect(querystmt,info,SQL_NTS)<>SQL_SUCCESS then
   begin
    Error;
    {
    raise EODBCError.Create('SQLExecDirect failed');
    }
   end;
 SQLFreeSTMT(stmt,SQL_CLOSE);
 stmt:=nil;
end;


procedure TODBCConnection.BindCol(i,coltype:integer;ptr:pointer;sz:smallint;var avail:longint);
begin
 if querystmt=nil then exit;
 SQLBindCol(querystmt,i,coltype,ptr,sz,avail);
end;

function TODBCConnection.Fetch:boolean;
begin
 result:=false;
 if querystmt=nil then exit;
 result:=SQLFetch(querystmt) in ODBCSUCCESS;
end;

function TODBCConnection.GetInfoInt(i:integer):integer;
var
 ret:integer;
 cbret:smallint;

begin
 ret:=0;
 if SQLGetInfo(hdbc,i,@ret,4,cbret)<>SQL_SUCCESS then
    raise EODBCError.Create('GetInfo failed');
 result:=ret;
end;

function TODBCConnection.GetInfoStr(i:integer):string;
var
  cbret: smallint;
begin
  if SQLGetInfo(hdbc,i,@info,sizeof(info),cbret)<>SQL_SUCCESS then
    raise EODBCError.Create('GetInfo failed');
  Result := AStrpas(info);
end;

function TODBCConnection.DateField(s:string):tdatetime;
var
 res1,res2,res3:integer;
 da,mo,ye:word;

begin
 {try yyyy-mm-dd}
 val(copy(s,1,4),ye,res1);
 val(copy(s,6,2),mo,res2);
 val(copy(s,9,2),da,res3);

 {try dd-mm-yyyy}
 if (res1<>0) or (res2<>0) or (res3<>0) then {try it in another way}
  begin
   val(copy(s,1,2),da,res1);
   val(copy(s,4,2),mo,res2);
   val(copy(s,7,4),ye,res3);
  end;

 {try dd-mm-yy}
 if (res1<>0) or (res2<>0) or (res3<>0) then {try it in another way}
  begin
   val(copy(s,1,2),da,res1);
   val(copy(s,4,2),mo,res2);
   val(copy(s,7,2),ye,res3);
  end;

  if (ye>1900) then ye:=ye-1900;

  result:=encodedate(ye,mo,da);
end;

function TODBCConnection.DataSourceFilter:string;
var
 descr,attr:array[0..255] of ansichar;
 pbdescr,pbAttr:word;
 attrstr: string;
begin
 result:='';
 if SQLDrivers(henv,SQL_FETCH_FIRST,descr,sizeof(descr),pbdescr,attr,sizeof(attr),pbAttr)=SQL_SUCCESS then
  begin
   attrstr := AStrpas(attr);
   result:=result+AStrpas(descr)+'|'+replacestr(GetListParam('FileExtn',StrListToPas(PChar(attrstr),pbAttr)),',',';');
   while SQLDrivers(henv,SQL_FETCH_NEXT,descr,sizeof(descr),pbdescr,attr,sizeof(attr),pbAttr)=SQL_SUCCESS do
    begin
     result:=result+'|'+AStrpas(descr)+'|'+replacestr(GetListParam('FileExtn',StrListToPas(PChar(attrstr),pbAttr)),',',';');
    end;
  end;
end;

function TODBCConnection.DataSourceFileExt(ext:ansistring): TStrings;
var
  descr,attr:array[0..255] of ansichar;
  pbdescr,pbAttr:word;
  res:ansistring;
  attrstr: string;
begin
  strlist.clear;
  result:=strlist;

  if SQLDrivers(henv,SQL_FETCH_FIRST,descr,sizeof(descr),pbdescr,attr,sizeof(attr),pbAttr)=SQL_SUCCESS then
  begin
    attrstr := AStrpas(attr);
    res:=ansistring(ReplaceStr(GetListParam('FileExtn',StrListToPas(PChar(attrstr),pbAttr)),',',';')+';');

    if (ncpos(string(ext)+';',string(res))>0) then
      strlist.add(AStrpas(descr));

    while SQLDrivers(henv,SQL_FETCH_NEXT,descr,sizeof(descr),pbdescr,attr,sizeof(attr),pbAttr)=SQL_SUCCESS do
    begin
      res := ansistring(ReplaceStr(GetListParam('FileExtn',StrListToPas(PChar(attrstr),pbAttr)),',',';')+';');

      if (ncpos(string(ext)+';',string(res))>0) then
        strlist.add(AStrpas(descr));
    end;
  end;
end;

function TODBCConnection.DataSourceFileUse(drv:ansistring):integer;
var
 descr,attr:array[0..255] of ansichar;
 pbdescr,pbAttr:word;
 res:string;
 i,code:integer;
 attrstr: string;
begin
 result:=-1;
 if SQLDrivers(henv,SQL_FETCH_FIRST,descr,sizeof(descr),pbdescr,attr,sizeof(attr),pbAttr)=SQL_SUCCESS then
  begin
   attrstr := AStrpas(attr);
   if comparetext(string(drv),AStrPas(descr))=0 then
    begin
     res:=GetListParam('FileUsage',StrListToPas(PChar(attrstr),pbAttr));
     val(res,i,code);
     if (code<>0) then result:=-1 else result:=i;
     exit;
    end;

   while SQLDrivers(henv,SQL_FETCH_NEXT,descr,sizeof(descr),pbdescr,attr,sizeof(attr),pbAttr)=SQL_SUCCESS do
    begin
     if comparetext(string(drv),AStrPas(descr))=0 then
       begin
        res:=GetListParam('FileUsage',StrListToPas(PChar(attrstr),pbAttr));
        val(res,i,code);
        if (code<>0) then result:=-1 else result:=i;
        exit;
       end;
    end;
  end;

end;

function TODBCConnection.DriverConnectFile(wnd:hwnd;drv,fname:ansistring):TStrings;
var
 connstr:ansistring;
 fileusage:smallint;
begin
 strlist.clear;
 result:=strlist;

 fileusage:=DataSourceFileUse(drv);

 case fileusage of
 1:begin          {dbase type}
    {$IFDEF TMSDEBUG}
    outputdebugstring('directory with tables');
    {$ENDIF}
    ODBCtablename:=ansistring(extractfilename(string(fname)));
    ODBCtablepath:=ansistring(extractfilepath(string(fname)));
   end;
 2:begin          {access type}
    {$IFDEF TMSDEBUG}
    outputdebugstring('file with tables');
    {$ENDIF}
    ODBCtablepath:=fname;
    ODBCtablename:='';
   end;
 else
   exit;
 end;

 connstr:='DRIVER='+drv+';DBQ='+ODBCtablepath+fconnectoptions;

 if (fUserID<>'') then
   connstr:=connstr+';UID='+fUserID+';PWD='+fAuthentication;

 msg(connstr);

 connstr:=DriverConnect(wnd,connstr);

 msg(connstr);

 if (connstr='') and (fileusage=1) then //retry with other DBQ
  begin
   connstr:='DRIVER='+drv+';DBQ='+fname+fconnectoptions;

   msg(connstr);

   connstr:=DriverConnect(wnd,connstr);

   msg(connstr);
  end;

 if (connstr<>'') then
 case fileusage of
 1:begin
    if (ODBCTableName='') then
     begin
      odbctablepath:='%';
      Tables;
     end
    else
      Strlist.Add(string(ODBCtablename));

    {$IFDEF TMSDEBUG}
    outputdebugstring(pchar('return table : '+odbctablename+' - '+inttostr(strlist.count)) );
    {$ENDIF}
   end;
 2:begin
    {$IFDEF TMSDEBUG}
    outputdebugstring('return table list');
    {$ENDIF}
    Tables;
   end;
 end;
end;

function TODBCConnection.DSNConnectFile(wnd:hwnd;dsn,fname:ansistring):TStrings;
var
 connstr:ansistring;

begin
 strlist.clear;
 result:=strlist;

 connstr:='DSN='+dsn+';DBQ='+fname;

 if (fUserID<>'') then
   connstr:=connstr+';UID='+fUserID+';PWD='+fAuthentication;

 msg(connstr);

 connstr:=DriverConnect(wnd,connstr);

 msg(connstr);

 if (connstr<>'') then Tables;
end;

function TODBCConnection.GetTypeInfo(DatatypeIdx:integer):TODBCTypeInfo;
var
 datatypename,prefix,suffix,precision,creparams,sqltype:array[0..255] of ansichar;
 datatypenamelen,prefixlen,suffixlen,precisionlen,creparamlen,sqltypelen:longint;
 res:smallint;

begin
 result.datatypename:='';
 if (SQLAllocSTMT(hdbc,stmt)<>SQL_SUCCESS) then exit;

 SQLGetTypeInfo(stmt,datatypeIdx);

 SQLBindCol(stmt,1,SQL_C_CHAR,@datatypename,sizeof(datatypename),datatypenamelen);
 SQLBindCol(stmt,2,SQL_C_CHAR,@sqltype,sizeof(sqltype),sqltypelen);
 SQLBindCol(stmt,3,SQL_C_CHAR,@precision,sizeof(precision),precisionlen);
 SQLBindCol(stmt,4,SQL_C_CHAR,@prefix,sizeof(prefix),prefixlen);
 SQLBindCol(stmt,5,SQL_C_CHAR,@suffix,sizeof(suffix),suffixlen);
 SQLBindCol(stmt,6,SQL_C_CHAR,@creparams,sizeof(creparams),creparamlen);

 res:=SQLFetch(stmt);
 if (res in ODBCSUCCESS) then
  begin
   result.datatypename:=strpas(datatypename);
   result.prefix :=strpas(prefix);
   result.suffix:=strpas(suffix);
   result.creparams:=strpas(creparams);
  end;

 SQLFreeSTMT(stmt,SQL_CLOSE);
 stmt:=nil;
end;

constructor TODBCStatement.Create(aODBCConnection:TODBCConnection);
begin
 inherited Create;
 sqlstmt:=nil;

 if not assigned(aODBCConnection) then
    raise EODBCError.Create('ODBC connection is not assigned');

 ODBCConnection:=aODBCConnection;

 with ODBCConnection do
  begin
   if (SQLAllocSTMT(hdbc,sqlstmt)<>SQL_SUCCESS) then
     raise EODBCError.Create('Cannot allocate statement handle');
  end;
end;

destructor TODBCStatement.Destroy;
begin
 if sqlstmt<>nil then SQLFreeSTMT(sqlstmt,SQL_CLOSE);
 inherited Destroy;
end;

function TODBCStatement.BindCol(i,coltype:integer;ptr:pointer;sz:smallint;var avail:longint):boolean;
begin
 result:=false;
 if (sqlstmt=nil) then exit;
 result:=SQLBindCol(sqlstmt,i,coltype,ptr,sz,avail) in ODBCSUCCESS;
end;

function TODBCStatement.BindParam(parnum:smallint;par:pointer;parintype,pardbtype,parlen:smallint):integer;
begin
 result:=SQLBindParameter(sqlstmt,parnum,SQL_PARAM_INPUT,parintype,pardbtype,parlen,0,par,0,nil);
end;

function TODBCStatement.Fetch:boolean;
begin
 result:=false;
 if sqlstmt=nil then exit;
 result:=SQLFetch(sqlstmt) in ODBCSUCCESS;
end;

function TODBCStatement.ExtendedFetch(fetchtype:word;row:longint;var numrows:longint;rowstatus:pointer):boolean;
begin
 result:=SQLExtendedFetch(sqlstmt,fetchtype,row,numrows,rowstatus) in ODBCSUCCESS;
end;

function TODBCStatement.Cancel:boolean;
begin
 result:=false;
 if sqlstmt=nil then exit;
 result:=SQLCancel(sqlstmt) in ODBCSUCCESS;
end;

function TODBCStatement.Prep(s:ansistring):boolean;
var
 sqlcmd:array[0..SQL_COMMAND_SIZE] of ansichar;
begin
 strpcopy(sqlcmd,s);
 if not (SQLPrepare(sqlstmt,sqlcmd,SQL_NTS) in ODBCSUCCESS) then
   begin
     error;
     raise EODBCError.Create('SQLPrepare failed');
   end;
 result:=true;
end;

function TODBCStatement.Execute:boolean;
begin
 result:=false;
 if not (SQLExecute(sqlstmt) in ODBCSUCCESS) then
  error
 else result:=true;
end;

function TODBCStatement.ExecDirect(s:ansistring):boolean;
var
 sqlcmd:array[0..SQL_COMMAND_SIZE] of ansichar;
begin
 {$IFDEF TMSDEBUG}
 outputdebugstring(pchar('sqlexecdirect :'+s));
 {$ENDIF}
 strpcopy(sqlcmd,s);
 if not (SQLExecDirect(sqlstmt,sqlcmd,SQL_NTS) in ODBCSUCCESS) then
   begin
     error;
     raise EODBCError.Create('SQLExecDirect failed');
   end
 else result:=true;
end;

function TODBCStatement.GetCursorName:string;
var
  sqlcurs:array[0..SQL_COMMAND_SIZE] of ansichar;
  cbCurs,pbCurs:smallint;
begin
  cbCurs:=sizeof(sqlcurs);
  SQLGetCursorName(sqlstmt,sqlcurs,cbcurs,pbcurs);
  Result := astrpas(sqlcurs);
end;


function TODBCStatement.SetCursorName(cursname:ansistring):boolean;
var
 sqlcurs:array[0..SQL_COMMAND_SIZE] of ansichar;
begin
 strpcopy(sqlcurs,cursname);
 result:=SQLSetCursorName(sqlstmt,sqlcurs,SQL_NTS) in ODBCSUCCESS;
 if not result then Error;
end;

function TODBCStatement.SetOption(option:word;param:integer):boolean;
begin
 result:=SQLSetStmtOption(sqlstmt,Option,Param) in ODBCSUCCESS;
 if not result then Error;
end;

function TODBCStatement.SetPos(row,option,lock:word):boolean;
begin
 result:=SQLSetPos(sqlstmt,row,option,lock) in ODBCSUCCESS;
 if not result then Error;
end;

function TODBCStatement.Attribute(i,desc:word):integer;
var
 pfDesc:longint;
 cbDesc:word;
begin
 result:=-1;
 if (SQLColAttributes(sqlstmt,i,desc,nil,0,cbDesc,pfDesc) in ODBCSUCCESS) then
  result:=pfDesc
 else error;
end;

function TODBCStatement.Columns:integer;
var
 numcols:smallint;
begin
 result:=-1;
 if (SQLNumResultCols(sqlstmt,numcols) in SQL_OK) then result:=numcols;
end;

function TODBCStatement.Rows:integer;
var
 numrow:integer;
begin
 result:=-1;
 if (SQLRowCount(sqlstmt,numrow) in SQL_OK) then result:=numrow else error;
end;


function TODBCStatement.Error:integer;
var
 state,errmsg:array[0..SQL_MAX_MESSAGE_LENGTH-1] of ansichar;
 errcode:integer;
 pberr:smallint;
 odbclink:todbclink;
begin
 with ODBCConnection do
  begin
   SQLError(henv,hdbc,sqlstmt,state,errcode,errmsg,sizeof(errmsg),pberr);

   if assigned(aFieldCollection) then
    begin
     odbclink:=(aFieldCollection as TODBCFieldCollection).GetOwner as TODBCLink;
     if assigned(odbclink) then
      begin
       if (odbclink.ShowErrors) then
       messagedlg('ODBC error in state : '+string(strpas(state))+#13+
                  'Error code : '+inttohex(errcode,4)+#13+
                  'Error message : '+string(strpas(errmsg)),mtError,[mbok],0);

       if assigned(odbclink.OnODBCError) then
       odbclink.OnODBCError(odbclink,string(strpas(state)),string(strpas(errmsg)),errcode);
      end;
    end;
  end;
 result:=errcode;
end;

//
//SQLConfigDataSource(NULL, ODBC_ADD_DSN,    "Microsoft Access Driver (*.mdb)",
//     "CREATE_DB=\\test.mdb General\0");

function SQLAllocConnect; external 'ODBC32.DLL';
function SQLAllocEnv; external 'ODBC32.DLL';
function SQLAllocSTMT; external 'ODBC32.DLL';
function SQLBindCol; external 'ODBC32.DLL';
function SQLBindParameter; external 'ODBC32.DLL';
function SQLBrowseConnect; external 'ODBC32.DLL';
function SQLCancel; external 'ODBC32.DLL';

function SQLConnect; external 'ODBC32.DLL';
function SQLDriverConnect; external 'ODBC32.DLL';
function SQLExecDirect; external 'ODBC32.DLL';
function SQLExecute; external 'ODBC32.DLL';
function SQLDisConnect; external 'ODBC32.DLL';
function SQLFreeConnect; external 'ODBC32.DLL';
function SQLFreeEnv; external 'ODBC32.DLL';
function SQLFreeSTMT; external 'ODBC32.DLL';
function SQLError; external 'ODBC32.DLL';
function SQLPrepare; external 'ODBC32.DLL';

function SQLSetParam; external 'ODBC32.DLL';
function SQLNumResultCols; external 'ODBC32.DLL';
function SQLDescribeCol; external 'ODBC32.DLL';

function SQLFetch; external 'ODBC32.DLL';
function SQLExtendedFetch; external 'ODBC32.DLL';
function SQLTables; external 'ODBC32.DLL';
function SQLColumns; external 'ODBC32.DLL';
function SQLGetInfo; external 'ODBC32.DLL';
function SQLGetTypeInfo; external 'ODBC32.DLL';
function SQLGetFunctions; external 'ODBC32.DLL';
function SQLDataSources; external 'ODBC32.DLL';
function SQLDrivers; external 'ODBC32.DLL';
function SQLSetConnectOption; external 'ODBC32.DLL';
function SQLSetPos; external 'ODBC32.DLL';
function SQLGetStmtOption; external 'ODBC32.DLL';

function SQLSetCursorName; external 'ODBC32.DLL';
function SQLGetCursorName; external 'ODBC32.DLL';
function SQLRowCount; external 'ODBC32.DLL';

function SQLGetData; external 'ODBC32.DLL';
function SQLPutData; external 'ODBC32.DLL';

function SQLSetScrollOptions; external 'ODBC32.DLL';
function SQLSetStmtOption; external 'ODBC32.DLL';
function SQLPrimaryKeys; external 'ODBC32.DLL';
function SQLColAttributes; external 'ODBC32.DLL';
function SQLStatistics; external 'ODBC32.DLL';

//32bit ODBC config
function SQLConfigDataSource; external 'ODBCCP32.DLL';


{ TODBCLink }

constructor TODBCLink.Create(aOwner: tComponent);
begin
 inherited Create(aOwner);
 fSQL:=tstringlist.create;
 fFields:=tODBCFieldCollection.Create(self);
 fUserID:='';
 fAuthentication:='';
 fResultlist:=tStringList.Create;
 fodbcStatus:=odbcNormal;
end;

procedure TODBCLink.Loaded;
begin
 inherited Loaded;
 Active:=self.fActive;
end;

destructor TODBCLink.Destroy;
begin
 fSQL.Free;
 fResultList.free;
 inherited Destroy;
end;

procedure TODBCLink.SetActive(avalue: boolean);
begin
 fActive:=avalue;
 if fActive then self.OpenDatabase;
end;

function TODBCLink.Drivers:tstrings;
var
 odbc:tODBCConnection;
begin
 odbc:=tODBCConnection.Create(fFields);
 fResultList.Clear;
 fResultList.AddStrings(odbc.drivers);
 odbc.free;
 result:=fResultList;
end;

function TODBCLink.Tables:tstrings;
var
 odbc:tODBCConnection;
begin
 fResultList.Clear;
 result:=fResultList;

 if ((Driver <>'') or (Datasource<>'')) and (Databasefile <>'') then
  begin
   ODBC:=tODBCConnection.Create(fFields);

   if (Driver<>'') then
    fResultList.AddStrings(ODBC.DriverConnectFile(0,Driver,Databasefile))
   else
    fResultList.AddStrings(ODBC.DSNConnectFile(0,DataSource,Databasefile));

   result:=fResultList;
   ODBC.Free;
  end;
end;

function TODBCLink.DriverForExtension(const S:String):string;
var
 odbc:tODBCConnection;
 i:integer;
begin
 odbc:=tODBCConnection.Create(fFields);
 fResultList.Clear;
 fResultList.AddStrings(odbc.drivers);
 odbc.free;
 for i:=1 to fResultList.Count do
  begin
   if (pos(S,fResultList.Strings[i-1])>0) then
    result:=fResultList.Strings[i-1];
  end;
end;


function TODBCLink.CreateDataSource(szDriver,szAttr:ansistring):boolean;
begin
  Result := SQLConfigDataSource(0,ODBC_ADD_DSN,PAnsiChar(szDriver),PAnsiChar(szAttr));
end;

function TODBCLink.Datasources:tstrings;
var
 odbc:tODBCConnection;
begin
 odbc:=tODBCConnection.Create(fFields);
 fResultList.Clear;
 fResultList.AddStrings(odbc.datasources);
 odbc.free;
 result:=fResultList;
end;

procedure TODBCLink.OpenDatabase;
var
 odbc:tODBCConnection;
 stmt:tODBCStatement;
 cols,i,j,k,stmtcols:integer;
 cac:integer;
 ca:array[0..SQL_MAX_COLUMNS] of PSQLstringbuf;
 sqlcmd:ansistring;
 fldname:string;
 info:array[0..128] of char;
 cbColLen,colType,Collen,scale,nullable:smallint;
 lis:tlistitem;

begin
 if ((Grid<>nil) or (ListView<>nil)) and
    ((Driver<>'') or (DataSource<>'')) and (Databasefile<>'') and (Table<>'') then
  begin
   fODBCStatus:=odbcOpening;

   odbc:=tODBCConnection.Create(fFields);

   odbc.fAuthentication:=fAuthentication;
   odbc.fUserID:=fUserID;

   if (Driver<>'') then
    odbc.DriverConnectFile(0,driver,databasefile)
   else
    odbc.DSNConnectFile(0,datasource,databasefile);

   {??? if fields exist ... then reopen different}

   if (fFields.Count=0) then odbc.Fields(table,true);

//   odbc.Statistics(table);

   Stmt:=TODBCStatement.Create(ODBC);

   try
    if (SQL.count>0) then
     begin
      sqlcmd:='';
      for i:=0 to SQL.count-1 do
       sqlcmd:=sqlcmd+' '+ansistring(SQL[i]);
     end
    else
      sqlcmd:='SELECT * FROM ['+table+']';

    Stmt.ExecDirect(sqlcmd);

    stmtcols:=Stmt.Columns;

    cols:=fFields.Count;

    if (cols>0) and (grid<>nil) then
     begin
      {clear the grid}

      for i:=grid.fixedcols to grid.colcount-1 do
       for j:=grid.fixedrows to grid.rowcount-1 do grid.cells[i,j]:='';

      if grid.colcount<cols then grid.colcount:=cols+grid.fixedcols;

      if (fShowFields) and (grid.fixedrows>0) then
        begin
         for i:=grid.fixedcols to grid.colcount-1 do grid.cells[i,0]:='';
        end;

       for i:=1 to cols do
         (fFields.Items[i-1] as TODBCField).colIndex:=0;

       for i:=1 to stmtcols do
          begin
           SQLDescribeCol(stmt.sqlstmt,i,@info,sizeof(info),cbcollen,coltype,collen,scale,nullable);

           fldname:=strpas(info);

           if (SQL.Count=0) then
             for j:=0 to fFields.Count-1 do
              begin
               if (fFields.Items[j] as TODBCField).FieldName=fldname then
                 begin
                  (fFields.Items[j] as TODBCField).colIndex:=i;
                  if (fShowfields) and (grid.fixedrows>0) then
                  begin
                   if (fFields.Items[j] as TODBCField).DisplayName='' then
                    grid.cells[j+grid.fixedcols,0]:=strpas(info)
                   else
                    grid.cells[j+grid.fixedcols,0]:=(fFields.Items[j] as TODBCField).DisplayName;
                  end;
                 end;
              end
             else
              if (fShowfields) and (grid.fixedrows>0) then
              grid.cells[i+grid.fixedcols-1,0]:=fldname;
        end;

      grid.rowcount:=Grid.fixedrows+1;

      for i:=0 to stmtcols-1 do
       begin
        new(ca[i]);
        ca[i]^ := #0;
        Stmt.BindCol(i+1,SQL_C_CHAR,ca[i],sizeof(tSQLstringbuf),cac);
       end;

       j:=1;

       if (sql.Count=0) then
       while Stmt.Fetch do
        begin
         for i:=0 to cols-1 do
          begin
           k:=(fFields.Items[i] as TODBCField).ColIndex-1;
           if (k>=0) then
            grid.cells[i+grid.fixedcols,j] := string(strpas(ca[k]^));
          end;
          inc(j);
          if (j>grid.rowcount) then grid.rowcount:=j;
        end
       else
        while Stmt.Fetch do
        begin
         for i:=0 to stmtcols-1 do
          begin
           grid.cells[i+grid.fixedcols,j] := string(strpas(ca[i]^));
          end;
          inc(j);
          if (j>grid.rowcount) then grid.rowcount:=j;
        end;

       for i:=0 to stmtcols-1 do
         begin
          dispose(ca[i]);
         end;
     end
    else
    if (cols>0) and (listview<>nil) then
     begin
      {clear the grid}
      listview.items.clear;
      listview.columns.clear;

      for i:=1 to cols do listview.columns.add;

      for i:=1 to cols do
          (fFields.Items[i-1] as TODBCField).colIndex:=0;

      for i:=1 to stmtcols do
         begin
          SQLDescribeCol(stmt.sqlstmt,i,@info,sizeof(info),cbcollen,coltype,collen,scale,nullable);

          for j:=0 to fFields.Count-1 do
           begin
            if (fFields.Items[j] as TODBCField).FieldName=strpas(info) then
             begin
              (fFields.Items[j] as TODBCField).colIndex:=i;

                if (fShowFields) then
                begin
                 if (fFields.Items[j] as TODBCField).DisplayName='' then
                  listview.columns.items[j].caption:=strpas(info)
                 else
                  listview.columns.items[j].caption:=(fFields.Items[j] as TODBCField).DisplayName;
                end;
             end;
         end;
       end;

      for i:=0 to stmtcols-1 do
       begin
        new(ca[i]);
        Stmt.BindCol(i+1,SQL_C_CHAR,ca[i],sizeof(tSQLstringbuf),cac);
       end;

       while Stmt.Fetch do
        begin
         lis:=listview.items.add;

         for i:=0 to cols-1 do
          begin
           k:=(fFields.Items[i] as TODBCField).ColIndex-1;

           if (k>=0) then
            begin
             if (i=0) then lis.caption:=string(strpas(ca[k]^))
              else
               lis.subitems.add(string(strpas(ca[k]^)));
            end;
          end;

        end;

       for i:=0 to stmtcols-1 do
         begin
          dispose(ca[i]);
         end;
     end;

     finally
       Stmt.Free;
     end;

   fODBCStatus:=odbcNormal;
  end;
end;

procedure TODBCLink.SaveDatabase;
var
 odbc:tODBCConnection;
 stmt:tODBCStatement;
 odbctypeinfo:tODBCTypeInfo;
 i,j:integer;
 SQLcmd,SQLspec:ansistring;
 ca:array[0..SQL_MAX_COLUMNS] of pSQLStringBuf;

begin
 if not assigned(grid) and not assigned(listview) then
  raise EODBCError.Create('No grid or listview control assigned');

 if fFields.Count=0 then
  raise EODBCError.Create('No fields specified for CREATE TABLE statement');

 if table='' then
  raise EODBCError.Create('No table specified for CREATE TABLE statement');

 if (fFields.Count>grid.colcount-grid.fixedcols) then
  raise EODBCError.Create('Number of fields exceeds number of columns in grid');

 if (grid.rowcount-grid.fixedrows<=0) then
  raise EODBCError.Create('No datarows in grid');

 odbc:=tODBCConnection.Create(nil);
 odbc.fAuthentication:=fAuthentication;
 odbc.fUserID:=fUserID;

 if pos('EXCEL',uppercase(string(Driver)))>0 then
  odbc.SetConnectOptions(';FIRSTROWHASNAMES=1;READONLY=FALSE;CREATE_DB="'+databasefile+'"');

 if (self.Driver<>'') then
  odbc.DriverConnectFile(0,self.Driver,databasefile)
 else
  odbc.DSNConnectFile(0,self.Datasource,databasefile);


 sqlcmd:='CREATE TABLE '+table+' (';
 for i:=0 to fFields.Count-1 do
  begin
   odbctypeinfo:=odbc.GetTypeInfo((fFields.Items[i] as TODBCField).DatatypeIdx);
   SQLspec:=odbctypeinfo.datatypename;
   if odbctypeinfo.creparams<>'' then
     SQLspec:=SQLspec+'('+ansistring(inttostr((fFields.Items[i] as TODBCField).Size))+')';

   sqlcmd:=sqlcmd+ansistring((fFields.Items[i] as TODBCField).FieldName)+' '+SQLspec+',';


  end;
 delete(sqlcmd,length(sqlcmd),1);

 if fFields.HasKey then
  begin
   sqlcmd:=sqlcmd+', CONSTRAINT PK PRIMARY KEY(';
   for i:=0 to fFields.Count-1 do
    begin
     if (fFields.Items[i] as TODBCField).Key then
      sqlcmd:=sqlcmd+ansistring((fFields.Items[i] as TODBCField).FieldName)+',';
    end;
   delete(sqlcmd,length(sqlcmd),1);
   sqlcmd:=sqlcmd+')';
  end;

 sqlcmd:=sqlcmd+')';

 msg(sqlcmd);

 Stmt:=TODBCStatement.Create(ODBC);
 if not Stmt.ExecDirect(sqlcmd) then
   begin
    odbc.free;
    raise EODBCError.Create('CREATE TABLE statement failed');
   end;
 stmt.free;

 if (KeyType=odbcRequiredKey) then
 begin
  sqlcmd:='CREATE UNIQUE INDEX '+table+' ON '+table+'('+ansistring((fFields.Items[0] as TODBCField).FieldName)+')';
  msg(sqlcmd);
  stmt:=TODBCStatement.Create(odbc);
  if not Stmt.ExecDirect(sqlcmd) then
    begin
     odbc.free;
     raise EODBCError.Create('Index creation failed');
    end;
  stmt.free;
 end;

 stmt:=TODBCStatement.Create(odbc);

 sqlcmd:='INSERT INTO '+table+' VALUES (';
 for i:=0 to fFields.Count-1 do sqlcmd:=sqlcmd+'?,';

 delete(sqlcmd,length(sqlcmd),1);
 sqlcmd:=sqlcmd+')';

 stmt.Prep(sqlcmd);

 for i:=0 to fFields.Count-1 do
  begin
   new(ca[i]);
   with (fFields.Items[i] as TODBCField) do
   stmt.BindParam(i+1,ca[i],SQL_C_CHAR,SQL_CHAR,Size);
  end;

 if assigned(grid) then
 begin
  for j:=grid.fixedrows to grid.rowcount-1 do
   begin
    for i:=0 to fFields.Count-1 do
     begin
      strpcopy(pchar(ca[i]),grid.cells[i+grid.FixedCols,j]);
     end;

    if not stmt.Execute then
     begin
      stmt.free;
      odbc.free;
      raise EODBCError.Create('INSERT INTO TABLE statement failed');
     end;

  end;
 end;

 if not assigned(grid) and assigned(listview) then
  begin
  for j:=0 to listview.items.Count-1 do
   begin
    for i:=0 to fFields.Count-1 do
     begin
      if (i=0) then
       strpcopy(pchar(ca[i]),listview.items[j].caption)
      else
       strpcopy(pchar(ca[i]),listview.items[j].subitems[i-1]);
     end;
    if not stmt.Execute then
     begin
      stmt.free;
      odbc.free;
      raise EODBCError.Create('INSERT INTO TABLE statement failed');
     end;
  end;
 end;

 for i:=0 to fFields.Count-1 do
  begin
   dispose(ca[i]);
  end;

 stmt.free;
 odbc.free;
end;

function TODBCLink.InsertRows(RowIndex,RowCount:integer):boolean;
var
 odbc:tODBCConnection;
 stmt:tODBCStatement;
 i,j:integer;
 SQLcmd:ansistring;
 ca:array[0..SQL_MAX_COLUMNS] of pSQLStringBuf;

begin
 if (RowCount<=0) or (RowIndex<0) then
  raise EODBCError.Create('Invalid row index or rowcount');

 if (grid.Rowcount<RowIndex+RowCount) then
  raise EODBCError.Create('Insert rows exceeds nr. of available rows');

 if fFields.Count=0 then
  raise EODBCError.Create('No fields specified for table');

 if table='' then
  raise EODBCError.Create('No table specified for insert rows');

 if (fFields.Count>grid.colcount-grid.fixedcols) then
  raise EODBCError.Create('Number of fields exceeds number of columns in grid');

 odbc:=tODBCConnection.Create(nil);

 odbc.fAuthentication:=fAuthentication;
 odbc.fUserID:=fUserID;

 if (self.Driver<>'') then
   odbc.DriverConnectFile(0,self.Driver,databasefile)
 else
   odbc.DSNConnectFile(0,self.Datasource,databasefile);

 stmt:=TODBCStatement.Create(odbc);

 sqlcmd:='INSERT INTO '+table+' (';

 for i:=0 to fFields.Count-1 do
   sqlcmd:=sqlcmd+ansistring((fFields.Items[i] as TODBCField).FieldName)+',';

 delete(sqlcmd,length(sqlcmd),1);

 sqlcmd:=sqlcmd+') VALUES (';
 for i:=0 to fFields.Count-1 do sqlcmd:=sqlcmd+'?,';

 delete(sqlcmd,length(sqlcmd),1);
 sqlcmd:=sqlcmd+')';

 stmt.Prep(sqlcmd);

 for i:=0 to fFields.Count-1 do
  begin
   new(ca[i]);
   with (fFields.Items[i] as TODBCField) do
    begin
     stmt.BindParam(i+1,ca[i],SQL_C_CHAR,SQL_CHAR,Size);
    end;
  end;

 for j:=RowIndex to (RowIndex+RowCount-1) do
  begin
   for i:=0 to fFields.Count-1 do
    begin
     strplcopy(pchar(ca[i]),grid.cells[i+grid.FixedCols,j],(fFields.Items[i] as TODBCField).Size);
    end;

   if not stmt.Execute then
     begin
      stmt.free;
      odbc.free;
      raise EODBCError.Create('INSERT statement failed');
     end;
  end;

 for i:=0 to fFields.Count-1 do
  begin
   dispose(ca[i]);
  end;

 stmt.free;
 odbc.free;
 result:=true;
end;

function TODBCLink.DeleteRows(RowIndex,RowCount:integer):boolean;
const
 ROWSET = 100;

var
 odbc:TODBCConnection;
 stmt:TODBCStatement;
 sqlcmd:ansistring;
 i,j:integer;
 rowstatus:array[1..100] of word;
 rows:longint;
 setidx,cntidx:longint;

begin
 if ((Grid<>nil) or (ListView<>nil)) and
    ((Driver<>'') or (DataSource<>'')) and (Databasefile<>'') and (Table<>'') then
  begin
   odbc:=tODBCConnection.Create(fFields);

   odbc.fAuthentication:=fAuthentication;
   odbc.fUserID:=fUserID;
   
   if (driver<>'') then
    odbc.DriverConnectFile(0,driver,databasefile)
   else
    odbc.DSNConnectFile(0,datasource,databasefile);

   Stmt:=TODBCStatement.Create(ODBC);

   stmt.SetOption(SQL_CONCURRENCY,SQL_CONCUR_ROWVER);

   stmt.SetOption(SQL_CURSOR_TYPE,SQL_CURSOR_KEYSET_DRIVEN);

   stmt.SetOption(SQL_ROWSET_SIZE,ROWSET);

   stmt.SetCursorName('C1');

   try
    if (SQL.count>0) then
     begin
      sqlcmd:='';
      for i:=0 to SQL.count-1 do
       sqlcmd:=sqlcmd+' '+ansistring(SQL[i]);
     end
    else
      sqlcmd:='SELECT * FROM ['+table+']';

    Stmt.ExecDirect(sqlcmd);

    j:=0;
    cntidx:=RowCount;

    while Stmt.ExtendedFetch(SQL_FETCH_NEXT,1,rows,@rowstatus) do
     begin
      setidx:=RowIndex-j;

      while (setidx<=ROWS)and (setidx>0) and (cntidx>0) do
       begin
        Stmt.SetPos(setidx,SQL_DELETE,SQL_LOCK_NO_CHANGE);
        dec(cntidx);
        inc(setidx);
       end;

      j:=j+ROWS;
     end;

   finally
    stmt.Free;
    odbc.Free;
   end;

  end;
 result:=true;
end;

function TODBCLink.UpdateRows(RowIndex,RowCount:integer):boolean;
const
  ROWSET = 1;
var
  odbc:TODBCConnection;
  stmt:TODBCStatement;
  stmtcols:integer;
  sqlcmd:ansistring;
  i,j:integer;
  cac:longint;
  rowstatus:array[1..100] of word;
  rows:longint;
  setidx,cntidx:longint;
  ca:array[0..SQL_MAX_COLUMNS] of pSQLstringbuf;

begin
 if ((Grid<>nil) or (ListView<>nil)) and
    ((Driver<>'') or (DataSource<>'')) and (Databasefile<>'') and (Table<>'') then
  begin
   odbc:=tODBCConnection.Create(fFields);

   odbc.fAuthentication:=fAuthentication;
   odbc.fUserID:=fUserID;
   if (Driver<>'') then
    odbc.DriverConnectFile(0,driver,databasefile)
   else
    odbc.DSNConnectFile(0,datasource,databasefile);


   Stmt:=TODBCStatement.Create(ODBC);

   stmt.SetOption(SQL_CONCURRENCY,SQL_CONCUR_ROWVER);

   stmt.SetOption(SQL_CURSOR_TYPE,SQL_CURSOR_KEYSET_DRIVEN);

   stmt.SetOption(SQL_ROWSET_SIZE,ROWSET);

   stmt.SetCursorName('C1');

   try
    if (SQL.count>0) then
     begin
      sqlcmd:='';
      for i:=0 to SQL.count-1 do
       sqlcmd := sqlcmd+' '+ ansistring(SQL[i]);
     end
    else
      sqlcmd:='SELECT * FROM ['+table+']';

    Stmt.ExecDirect(sqlcmd);

    stmtcols:=stmt.Columns;

    for i:=0 to stmtcols-1 do
     begin
      new(ca[i]);
      Stmt.BindCol(i+1,SQL_C_CHAR,ca[i],sizeof(tSQLstringbuf),cac);
     end;

    j:=0;
    cntidx:=RowCount;

    while Stmt.ExtendedFetch(SQL_FETCH_NEXT,1,rows,@rowstatus) do
     begin
      setidx:=RowIndex-j;

      while (setidx<=ROWS)and (setidx>0) and (cntidx>0) do
       begin

        for i:=0 to fFields.Count-1 do
         begin
          strpcopy(pchar(ca[i]),grid.cells[i+grid.FixedCols,RowIndex+RowCount-cntidx]);
         end;

        Stmt.SetPos(setidx,SQL_UPDATE,SQL_LOCK_NO_CHANGE);
        dec(cntidx);
        inc(setidx);
       end;

      j:=j+ROWS;
     end;

    for i:=0 to stmtcols-1 do
     begin
      dispose(ca[i]);
     end;

   finally
    stmt.Free;
    odbc.Free;
   end;

  end;
 result:=true;

end;

function TODBCLink.HasDataType(sqltype:integer):boolean;
var
 odbc:todbcconnection;
 ODBCTypeInfo:tODBCTypeInfo;

begin
 result:=false;
 if ((Driver<>'') or (DataSource<>'')) and (Databasefile<>'') and (Table<>'') then
  begin
   odbc:=tODBCConnection.Create(fFields);
   odbc.fAuthentication:=fAuthentication;
   odbc.fUserID:=fUserID;

   if (driver<>'') then
    odbc.DriverConnectFile(0,driver,databasefile)
   else
    odbc.DSNConnectFile(0,datasource,databasefile);
    
   ODBCTypeInfo:=ODBC.GetTypeInfo(sqltype);
   result:=ODBCTypeInfo.datatypename<>'';
   odbc.Free;
  end;
end;


function TODBCLink.BrowseDatabase(doOpen:boolean):boolean;
var
 od:topendialog;
 odbc:tODBCConnection;
 reslist: TStrings;
 selectdialog: TSelectdialog;

begin
  Result := false;

  od := TOpenDialog.create(self.GetParentComponent);

  odbc := TODBCConnection.Create(fFields);
  od.filter := ODBC.DataSourceFilter;

  if od.execute then
  begin
   Driver := ansistring(odbc.Drivers.strings[od.FilterIndex-1]);

   odbc.fuserID:=fUserID;
   odbc.fAuthentication:=fAuthentication;

   reslist:=odbc.DriverConnectFile(0,ansistring(odbc.Drivers.strings[od.FilterIndex-1]),ansistring(od.Filename));

   {$IFDEF TMSDEBUG}
   outputdebugstring(pchar(reslist.strings[0]));
   {$ENDIF}

   case reslist.count of
   0:messagedlg('No tables in database',mtinformation,[mbok],0);
   1:begin
      self.Table:=ansistring(reslist.Strings[0]);

      self.DataSource:=ansistring(odbc.Drivers.strings[od.FilterIndex-1]);
      self.Databasefile:= ansistring(od.filename);

      if (pos('.xls',od.filename)>0) or (pos('.XLS',od.filename)>0) then
          self.table:='database';

      {$IFDEF TMSDEBUG}
      messagedlg('databasefile = '+self.databasefile,mtinformation,[mbok],0);
      messagedlg('table name = '+self.table,mtinformation,[mbok],0);
      {$ENDIF}

      if doOpen then self.Active:=true else odbc.Fields(table,true);

      result:=true;
     end;
    else
     begin
      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('nr of tables = '+inttostr(reslist.count)));
      for i:=0 to reslist.count-1 do
       outputdebugstring(pchar(reslist.strings[i]));
      messagedlg('Multiple tables in database',mtinformation,[mbok],0);
      {$ENDIF}

      selectdialog:=tselectdialog.Create(self);
      selectdialog.tablelist.items.addstrings(reslist);
      selectdialog.tablelist.itemindex:=0;
      try
       if selectdialog.showmodal=mrok then
        begin
         self.databasefile := ansistring(od.filename);
         self.table := ansistring(selectdialog.tablelist.Items[selectdialog.tablelist.itemindex]);
         self.datasource:= ansistring(odbc.Drivers.strings[od.FilterIndex-1]);
         result:=true;
         if doOpen then self.Active:=true else odbc.Fields(table,true);
        end;
      finally
       selectdialog.free;
      end;

     end;
    end;
  end;
 odbc.Free;
end;  

procedure TODBCLink.SetDataSource(avalue: ansistring);
begin
 if not (csLoading in ComponentState) then
   begin
    Active:=false;
    fFields.clear;
   end;
 fDataSource:=avalue;
end;

procedure TODBCLink.SetDriver(avalue: ansistring);
begin
 if not (csLoading in ComponentState) then
   begin
    Active:=false;
    fFields.clear;
   end;
 fDriver:=avalue;
end;

procedure TODBCLink.SetTable(avalue: ansistring);
begin
 if not (csLoading in ComponentState) then
   begin
    Active:=false;
    fFields.clear;
   end;
 fTable:=avalue;
end;

procedure TODBCLink.SetSQL(avalue:tstringlist);
begin
 if assigned(avalue) then fSQL.assign(avalue);
end;

procedure TODBCLink.SetFieldCollection(value:todbcfieldcollection);
begin
 fFields.Assign(value);
 {
 if self.Active then
   begin
    active:=false;
    active:=true;
   end;
 }
end;

function TODBCLink.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TODBCLink.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TODBCLink.SetVersion(const Value: string);
begin

end;

{ TODBCDataSourceEditor }
{
function TODBCDataSourceProperty.GetAttributes: TPropertyAttributes;
begin
 result:=[paValueList,paSortList];
end;

procedure TODBCDataSourceProperty.GetValues(Proc: TGetStrProc);
var
 odbc:todbcconnection;
 i:integer;
 strlist:tstrings;
begin
 odbc:=todbcconnection.create(nil);
 strlist:=tstringlist.Create;
 strlist.assign(odbc.datasources);
 odbc.free;
 if strlist.count>0 then
 for i:=0 to strlist.count-1 do
  begin
   proc(strlist.strings[i]);
  end;
 strlist.free;
end;

function TODBCDriverProperty.GetAttributes: TPropertyAttributes;
begin
 result:=[paValueList,paSortList];
end;

procedure TODBCDriverProperty.GetValues(Proc: TGetStrProc);
var
 odbc:todbcconnection;
 i:integer;
 strlist:tstrings;
begin
 odbc:=todbcconnection.create(nil);
 strlist:=tstringlist.Create;
 strlist.assign(odbc.drivers);
 odbc.free;
 if strlist.count>0 then
 for i:=0 to strlist.count-1 do
  begin
   proc(strlist.strings[i]);
  end;
 strlist.free;
end;
}
{ TODBCTableProperty }
{
function TODBCTableProperty.GetAttributes: TPropertyAttributes;
begin
 result:=[paValueList,paSortList];
end;

procedure TODBCTableProperty.GetValues(Proc: TGetStrProc);
var
 ODBClink:todbclink;
 ODBC:tODBCConnection;
 strlist:tstrings;
 i:integer;
begin
 ODBClink:=(GetComponent(0) as TODBCLink);

 if ((ODBCLink.Driver <>'') or (ODBCLink.Datasource<>'')) and (ODBCLink.Databasefile <>'') then
  begin
   ODBC:=tODBCConnection.Create(nil);

   if (ODBCLink.Driver<>'') then
    strlist:=ODBC.DriverConnectFile(0,ODBCLink.Driver,ODBCLink.Databasefile)
   else
    strlist:=ODBC.DSNConnectFile(0,ODBCLink.DataSource,ODBCLink.Databasefile);

   //get the tables here

   if strlist.count>0 then
    for i:=0 to strlist.count-1 do
     begin
      proc(strlist.strings[i]);
     end;

   ODBC.Free;
  end;
end;

procedure TODBCFieldProperty.SetValue(const value:string);
begin
 SetOrdValue(0);
 if value='SQL_CHAR' then SetOrdValue(1);
 if value='SQL_NUMERIC' then SetOrdValue(2);
 if value='SQL_DECIMAL' then SetOrdValue(3);
 if value='SQL_INTEGER' then SetOrdValue(4);
 if value='SQL_SMALLINT' then SetOrdValue(5);
 if value='SQL_FLOAT' then SetOrdValue(6);
 if value='SQL_REAL' then SetOrdValue(7);
 if value='SQL_DOUBLE' then SetOrdValue(8);
 if value='SQL_VARCHAR' then SetOrdValue(12);

 if value='SQL_DATE' then SetOrdValue(9);
 if value='SQL_TIME' then SetOrdValue(10);
 if value='SQL_TIMESTAMP' then SetOrdValue(11);
 if value='SQL_LONGVARCHAR' then SetOrdValue(-1);
 if value='SQL_BINARY' then SetOrdValue(-2);
 if value='SQL_VARBINARY' then SetOrdValue(-3);
 if value='SQL_LONGVARBINARY' then SetOrdValue(-4);
 if value='SQL_BIGINT' then SetOrdValue(-5);
 if value='SQL_TINYINT' then SetOrdValue(-6);
 if value='SQL_BIT' then SetOrdValue(-7);
end;


function TODBCFieldProperty.GetValue:string;
begin
 case GetOrdValue of
 1:result:='SQL_CHAR';
 2:result:='SQL_NUMERIC';
 3:result:='SQL_DECIMAL';
 4:result:='SQL_INTEGER';
 5:result:='SQL_SMALLINT';
 6:result:='SQL_FLOAT';
 7:result:='SQL_REAL';
 8:result:='SQL_DOUBLE';
 9:result:='SQL_DATE';
 10:result:='SQL_TIME';
 11:result:='SQL_TIMESTAMP';
 12:result:='SQL_VARCHAR';
 -1:result:='SQL_LONGVARCHAR';
 -2:result:='SQL_BINARY';
 -3:result:='SQL_VARBINARY';
 -4:result:='SQL_LONGVARBINARY';
 -5:result:='SQL_BIGINT';
 -6:result:='SQL_TINYINT';
 -7:result:='SQL_BIT';
 else result:='SQL_UNKNOWN';
 end;
end;


function TODBCFieldProperty.GetAttributes: TPropertyAttributes;
begin
 result:=[paValueList,paSortList];
end;

procedure TODBCFieldProperty.GetValues(Proc: TGetStrProc);
var
 ODBCField:tODBCField;
 ODBCLink:TODBCLink;
 ODBC:tODBCConnection;
 ODBCTypeInfo:TODBCTypeInfo;
 i:integer;
begin
 ODBCField:=(GetComponent(0) as TODBCField);

 ODBCLink:=(ODBCField.Collection as TODBCFieldCollection).GetOwner as TODBCLink;

 if ((ODBCLink.Driver<>'') or (ODBCLink.Datasource<>'')) and (ODBCLink.Databasefile <>'') then
  begin
   ODBC:=tODBCConnection.Create(nil);
   if (ODBCLink.Driver<>'') then
    ODBC.DriverConnectFile(0,ODBCLink.Driver,ODBCLink.Databasefile)
   else
    ODBC.DSNConnectFile(0,ODBCLink.Datasource,ODBCLink.Databasefile);


   //get the tables here

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_CHAR);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_CHAR');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_NUMERIC);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_NUMERIC');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_DECIMAL);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_DECIMAL');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_INTEGER);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_INTEGER');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_SMALLINT);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_SMALLINT');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_FLOAT);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_FLOAT');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_REAL);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_REAL');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_DOUBLE);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_DOUBLE');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_VARCHAR);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_VARCHAR');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_DATE);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_DATE');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_TIME);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_TIME');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_TIMESTAMP);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_TIMESTAMP');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_LONGVARCHAR);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_LONGVARCHAR');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_BINARY);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_BINARY');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_VARBINARY);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_VARBINARY');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_BIGINT);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_BIGINT');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_TINYINT);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_TINYINT');

   ODBCTypeInfo:=ODBC.GetTypeInfo(SQL_BIT);
   if ODBCTypeInfo.datatypename <>'' then proc('SQL_BIT');

   proc('SQL_UNKNOWN');

   ODBC.Free;

  end;

end;

}

{ TODBCFieldCollection }

constructor TODBCFieldCollection.Create(odbclink: todbclink);
begin
 inherited Create(TODBCField);
 fodbclink:=odbclink;
end;

function TODBCFieldCollection.HasKey:boolean;
var
 i:integer;
begin
 result:=false;
 if self.Count<=0 then exit;
 for i:=0 to self.Count-1 do
  begin
   if (self.Items[i] as TODBCField).Key then result:=true;
  end;
end;

function TODBCFieldCollection.GetOwner: tPersistent;
begin
 result:=fOdbclink;
end;

procedure TODBCFieldCollection.Update(Item: TCollectionItem);
var
 odbclink:tODBCLink;
begin
 inherited Update(item);

 odbclink:= GetOwner as TODBCLink;

 if (csDesigning in odbclink.ComponentState) and
    (odbclink.Status=odbcNormal) then
  begin
   if odbclink.active then
    begin
     odbclink.active:=false;
     odbclink.active:=true;
    end;
  end;

end;


{ TODBCFieldNameProperty }
{
function TODBCFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
 result:=[paValueList,paSortList];
end;

procedure TODBCFieldNameProperty.GetValues(Proc:TGetStrProc);
var
 i:integer;
 strlist:tstrings;
 ODBCField:tODBCField;
 ODBCLink:TODBCLink;
 ODBC:tODBCConnection;

begin
 ODBCField:=(GetComponent(0) as TODBCField);

 ODBCLink:=(ODBCField.Collection as TODBCFieldCollection).GetOwner as TODBCLink;

 if ((ODBCLink.Driver<>'') or (ODBCLink.Datasource<>'')) and (ODBCLink.Databasefile <>'') and (ODBCLink.Table<>'') then
  begin
   ODBC:=tODBCConnection.Create(nil);
   if (ODBCLink.Driver<>'') then
    ODBC.DriverConnectFile(0,ODBCLink.Driver,ODBCLink.Databasefile)
   else
    ODBC.DSNConnectFile(0,ODBCLink.Datasource,ODBCLink.Databasefile);

   strlist:=tstringlist.Create;
   strlist.assign(ODBC.Fields(ODBCLink.Table,false));
   ODBC.Free;

   if strlist.count>0 then
   for i:=0 to strlist.count-1 do
    begin
     proc(strlist.strings[i]);
    end;
   strlist.free;
  end;
end;
}
procedure TODBCField.SetDisplName(value: string);
begin
 fDisplayName:=value;
 changed(false);
end;

procedure TODBCField.SetFieldName(value:string);
begin
 fFieldName:=value;
 changed(false);
end;


end.
