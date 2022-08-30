{*******************************************************}
{                                                       }
{            MiTeC MS Access Objects                    }
{                                                       }
{        Copyright (c) 1999-2013 Michal Mutl            }
{                                                       }
{*******************************************************}

{$I Compilers.inc}

unit MiTeC_MSAccess;

interface

uses {$IFDEF JOURNAL} MiTeC_Journal, {$ENDIF}
     {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, VCL.Dialogs, System.Win.ComObj,
     System.Variants;
     {$ELSE}
     Windows, SysUtils, Classes, Variants, COMObj, Dialogs;
     {$ENDIF}


const
{ Workspace type }
  dbUseODBC = $00000001;
  dbUseJet = $00000002;

{ Permissions }
  dbSecNoAccess = $00000000;
  dbSecFullAccess = $000FFFFF;
  dbSecDelete = $00010000;
  dbSecReadSec = $00020000;
  dbSecWriteSec = $00040000;
  dbSecWriteOwner = $00080000;
  dbSecDBCreate = $00000001;
  dbSecDBOpen = $00000002;
  dbSecDBExclusive = $00000004;
  dbSecDBAdmin = $00000008;
  dbSecCreate = $00000001;
  dbSecReadDef = $00000004;
  dbSecWriteDef = $0001000C;
  dbSecRetrieveData = $00000014;
  dbSecInsertData = $00000020;
  dbSecReplaceData = $00000040;
  dbSecDeleteData = $00000080;

{ Recordset open type }
  dbOpenTable = $00000001;
  dbOpenDynaset = $00000002;
  dbOpenSnapshot = $00000004;
  dbOpenForwardOnly = $00000008;
  dbOpenDynamic = $00000010;

{ Lock strategy }
  dbPessimistic = $00000002;
  dbOptimistic = $00000003;
  dbOptimisticValue = $00000001;
  dbOptimisticBatch = $00000005;

{ Field Types }
  dbBoolean = $00000001;
  dbByte = $00000002;
  dbInteger = $00000003;
  dbLong = $00000004;
  dbCurrency = $00000005;
  dbSingle = $00000006;
  dbDouble = $00000007;
  dbDate = $00000008;
  dbBinary = $00000009;
  dbText = $0000000A;
  dbLongBinary = $0000000B;
  dbMemo = $0000000C;
  dbGUID = $0000000F;
  dbBigInt = $00000010;
  dbVarBinary = $00000011;
  dbChar = $00000012;
  dbNumeric = $00000013;
  dbDecimal = $00000014;
  dbFloat = $00000015;
  dbTime = $00000016;
  dbTimeStamp = $00000017;

{ Query types }
  dbQSelect = $00000000;
  dbQProcedure = $000000E0;
  dbQAction = $000000F0;
  dbQCrosstab = $00000010;
  dbQDelete = $00000020;
  dbQUpdate = $00000030;
  dbQAppend = $00000040;
  dbQMakeTable = $00000050;
  dbQDDL = $00000060;
  dbQSQLPassThrough = $00000070;
  dbQSetOperation = $00000080;
  dbQSPTBulk = $00000090;
  dbQCompound = $000000A0;

{ Characteristics }
  dbDenyWrite = $00000001;
  dbDenyRead = $00000002;
  dbReadOnly = $00000004;
  dbAppendOnly = $00000008;
  dbInconsistent = $00000010;
  dbConsistent = $00000020;
  dbSQLPassThrough = $00000040;
  dbFailOnError = $00000080;
  dbForwardOnly = $00000100;
  dbSeeChanges = $00000200;
  dbRunAsync = $00000400;
  dbExecDirect = $00000800;

{ Relations }
  dbRelationUnique = $00000001;
  dbRelationDontEnforce = $00000002;
  dbRelationInherited = $00000004;
  dbRelationUpdateCascade = $00000100;
  dbRelationDeleteCascade = $00001000;
  dbRelationLeft = $01000000;
  dbRelationRight = $02000000;

{ Table attributes }
  dbAttachExclusive = $00010000;
  dbAttachSavePWD = $00020000;
  dbSystemObject = $80000002;
  dbAttachedTable = $40000000;
  dbAttachedODBC = $20000000;
  dbHiddenObject = $00000001;

{ Database Types }
  dbVersion10 = $00000001;
  dbEncrypt = $00000002;
  dbDecrypt = $00000004;
  dbVersion11 = $00000008;
  dbVersion20 = $00000010;
  dbVersion30 = $00000020;

{ Parameter Direction constants }
const
  dbParamInput = $00000001;
  dbParamOutput = $00000002;
  dbParamInputOutput = $00000003;
  dbParamReturnValue = $00000004;

{ OLE object table class string }
  daoEngine120 = 'DAO.DBEngine.120';
  daoEngine36  = 'DAO.DBEngine.36';
  daoEngine35  = 'DAO.DBEngine.35';
  daoEngine30  = 'DAO.DBEngine';

{ System MS Access database filename }
  daoSystemDB = 'system.mdw';

{ Defaults for MS Access security }
  daoDefaultUser = 'Admin';
  daoDefaultPassword = #0;
  daoDefaultWorkspace = '#Default Workspace#';

{ Reserved keywords for MS Access }
  csysusrEngine = 'ENGINE';
  csysusrCreator = 'CREATOR';
  csysusrAdmin = 'ADMIN';
  csysgrpAdmins = 'ADMINS';
  csysgrpUsers = 'USERS';

type
  TDAOType = (dao30,dao35,dao36,dao120);

  { Exception for TMSAccess class }
  EMSAccessException = class(Exception);

  TMSAccess = class(TPersistent)
  private
    FEngine,        //DAO Engine OLE interface object
    FWorkspace,     //DAO Engine workspace OLE interface object
    FConnection     //DAO Engine database connection OLE interface object
    : OLEVariant;
    FDB,
    FSysDB,
    FUsername,
    FPassword: string;
    FActive, FConnected, FOpen: boolean;
    FDAOCS: string;
    FDAOType: TDAOType;
    FLAstError: string;
    FReadOnly: Boolean;
    FExclusive: Boolean;
    function GetDAOVersion: string;
    procedure SetActive(const Value: boolean);
    function GetQuery(index: word): OLEVariant;
    procedure SetQuery(index: word; const Value: OLEVariant);
    function GetTable(index: word): OLEVariant;
    function GetQueryDefCount: word;
    function GetTableDefCount: word;
    function GetQueryByName(Name: string): OLEVariant;
    function GetTableByName(name: string): OLEVariant;
    procedure SetQueryByName(Name: string; const Value: OLEVariant);
    function GetGroup(index: dword): string;
    function GetGroupCount: integer;
    function GetUser(index: dword): string;
    function GetUserCount: integer;
    function GetJetVersion: string;
    function GetWorkspace(Index: DWORD): OLEVariant;
    function GetDB: string;
    function GetUsername: string;
    function GetContainer(index: word): OLEVariant;
    function GetContainerByName(Name: string): OLEVariant;
    function GetContainerCount: word;
    function GetRecordset(index: word): OLEVariant;
    function GetRecordsetByName(Name: string): OLEVariant;
    function GetRecordsetCount: word;
    function GetRelation(index: word): OLEVariant;
    function GetRelationByName(Name: string): OLEVariant;
    function GetRelationCount: word;
    procedure SetDAOType(Value: TDAOType);
    procedure SetConnected(const Value: boolean);
    procedure SetOpen(const Value: boolean);
    function GetSystemDatabase: string;
    function GetWorkspaceCount: integer;
    function GetCurrentWorkspace: string;
    function GetOpenDatabase: OLEVariant;
  protected
    { Get DAO OLE interface object }
    function GetDAOEngine(CLSID :string) :OLEVariant;

    { Create workspace in DAO Engine }
    function CreateWorkspace(AName, AUser, APwd: string): OLEVariant;

    { Initialize DAO Engine thru OLE }
    function OpenEngine(ClassString: string): boolean;

    { Close DAO engine and release OLE object }
    procedure CloseEngine;

    { Open/Create specified workspace in DAO Engine }
    function OpenWorkspace(AName, AUser, APwd :string) :boolean;

    { Close workspace }
    procedure CloseWorkspace;

    { Open specified database from open workspace in DAO Engine }
    function OpenDatabase(AFilename :string; Exclusive, ReadOnly: boolean) :OLEVariant;

    { Close open database }
    procedure CloseDatabase;


  public
    constructor Create;
    destructor Destroy; override;

    { Create query definition in QueryDefs collection }
    function CreateQuery(AName, SQL :string) :OLEVariant;

    { Get list of existing table definitions in current connection }
    procedure GetTableList(var AList: TStringList);

    { Get list of existing query definitions in current connection }
    procedure GetQueryList(var AList: TStringList);

    { Get list of existing recordset definitions in current connection }
    procedure GetRecordsetList(var AList: TStringList);

    { Get list of existing realtion definitions in current connection }
    procedure GetRelationList(var AList: TStringList);

    { Get list of existing container definitions in current connection }
    procedure GetContainerList(var AList: TStringList);

    { Get field list of specified table definition }
    procedure GetTableFieldList(ATable :string; var AList :TStringlist);

    { Get field list of specified query definition }
    procedure GetQueryFieldList(AQuery :string; var AList :TStringlist);

    { Get field list of specified recordset definition }
    procedure GetRecordsetFieldList(ARecordset :string; var AList :TStringlist);

    { Get field list of specified relation definition }
    procedure GetRelationFieldList(ARelation :string; var AList :TStringlist);

    { Get document list of specified container definition }
    procedure GetContainerDocumentList(ACont :string; var AList :TStringlist);

    { Open recordset as query defined in ASQL }
    function OpenDynaset(ASQL :string) :OLEVariant;

    function OpenTable(ATable: string): OLEVariant;

    { Returns list of group members }
    procedure GetGroupUsers(AName :string; Userlist :TStrings);

    { Returns list of user memberships }
    procedure GetUserGroups(AName :string; Grouplist :TStrings);

    { Returns user index by name }
    function GetUserIndex(AName :string) :integer;

    { Returns group index by name }
    function GetGroupIndex(AName :string) :integer;

    { Add new user to MS Access }
    function AddUser(AName, APID, APwd :string) :boolean;

    { Delete user from MS Access }
    procedure DeleteUser(AName :string);

    { Add new group to MS Access }
    function AddGroup(AName, APID :string) :boolean;

    { Delete group from MS Access }
    procedure DeleteGroup(AName :string);

    { Create user membership in specified group }
    function AddUserToGroup(AUserName, AGroupName :string) :boolean;
    function AddGroupToUser(AUserName, AGroupName :string) :boolean;

    { Dismiss user membership }
    procedure DeleteUserFromGroup(AUserName, AGroupName :string);
    procedure DeleteGroupFromUser(AUserName, AGroupName :string);

    { Change user password }
    function ChangeUserPwd(AName, AOldPwd, ANewPwd :string) :boolean;

    { Repair specified database }
    procedure RepairDatabase(const Filename :string);

    { Defragment specified database to new database }
    procedure CompactDatabase(const OldFilename, NewFilename: string);

    { Encrypt specified database to new database }
    procedure EncryptDatabase(const OldFilename, NewFilename, Password: string);

    { Decrypt specified database to new database }
    procedure DecryptDatabase(const OldFilename, NewFilename, Password: string);

    { Find document for object }
    function FindDocument(AName: string): OLEVariant;

    property LastError: string read FLAstError;

    property EngineActive :boolean read FActive write SetActive;
    property EngineConnected: boolean read FConnected write SetConnected;
    property DatabaseOpen: boolean read FOpen write SetOpen;

    property DAOType: TDAOType read FDAOType write SetDAOType;
    property Username :string read GetUsername write FUsername;
    property Password: string write FPassword;
    property SystemDatabase :string read GetSystemDatabase;
    property SysDB :string read FSysDB write FSysDB;
    property DatabaseName :string read GetDB write FDB;
    property DAOVersion :string read GetDAOVersion;
    property JetVersion :string read GetJetVersion;
    property Workspace: string read GetCurrentWorkspace;

    property Database: OLEVariant read GetOpenDatabase;

    property Exclusive: Boolean read FExclusive write FExclusive;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;

    property TableDefs[index :word] :OLEVariant read GetTable;
    property TableDefByName[name :string] :OLEVariant read GetTableByName;
    property TableDefCount :word read GetTableDefCount;

    property QueryDefs[index :word] :OLEVariant read GetQuery write SetQuery;
    property QueryDefByName[Name :string] :OLEVariant read GetQueryByName write SetQueryByName;
    property QueryDefCount :word read GetQueryDefCount;

    property Recordsets[index :word] :OLEVariant read GetRecordset;
    property RecordsetByName[Name :string] :OLEVariant read GetRecordsetByName;
    property RecordsetCount :word read GetRecordsetCount;

    property Relations[index :word] :OLEVariant read GetRelation;
    property RelationByName[Name :string] :OLEVariant read GetRelationByName;
    property RelationCount :word read GetRelationCount;

    property Containers[index :word] :OLEVariant read GetContainer;
    property ContainerByName[Name :string] :OLEVariant read GetContainerByName;
    property ContainerCount :word read GetContainerCount;

    property Users[index :dword] :string read GetUser;
    property UserCount :integer read GetUserCount;
    property Groups[index :dword] :string read GetGroup;
    property GroupCount :integer read GetGroupCount;

    property Workspaces[Index: DWORD]: OLEVariant read GetWorkspace;
    property WorkspaceCount: integer read GetWorkspaceCount;
  end;

{$IFDEF JOURNAL}
var
  Journal: TJournal;
{$ENDIF}


const
{ Reserved SQL keywords }
  sqlSELECT = 'SELECT';
  sqlDELETE = 'DELETE';
  sqlINSERT = 'INSERT';
  sqlUPDATE = 'UPDATE';
  sqlCREATE = 'CREATE';

  function GetFieldTypeStr(AType: SmallInt): string;

  function GetTableTypeStr(AType: longword): string;

  function GetQueryTypeStr(AType: Smallint): string;

  function GetStmtType(AStmt: string): SmallInt;

  function GetRelationTypeStr(AType: dword): string;
  function GetParameterTypeStr(AType: dword): string;

  function GetFieldValue(ARecordset: OLEVariant; AField, ANullValue: string): Variant; overload;
  function GetFieldValue(ARecordset: OLEVariant; AField: integer; ANullValue: string): Variant; overload;


implementation

uses {$IFDEF RAD9PLUS}
     WinAPI.ActiveX,
     {$ELSE}
     ActiveX,
     {$ENDIF}
     MiTeC_Routines;

resourcestring
  SCannotChngPwd = 'Cannot change Password when session is active.';
  SCannotChngUser = 'Cannot change Username when session is active.';
  SCannotChngDAO = 'Cannot change DAOType when session is active.';

function GetFieldTypeStr(AType: SmallInt): string;
begin
  case AType of
   dbBoolean:  Result := 'Boolean';
   dbByte:     Result := 'Byte';
   dbInteger:  Result := 'Integer';
   dbLong:     Result := 'Long';
   dbCurrency: Result := 'Currency';
   dbSingle:   Result := 'Single';
   dbDouble:   Result := 'Double';
   dbDate:     Result := 'Date';
   dbBinary:   Result := 'Binary';
   dbText:     Result := 'Text';
   dbLongBinary: Result := 'LongBinary';
   dbMemo:       Result := 'Memo';
   dbGUID:       Result := 'GUID';
   dbBigInt:     Result := 'BigInt';
   dbVarBinary:  Result := 'VarBinary';
   dbChar:       Result := 'Char';
   dbNumeric:    Result := 'Numeric';
   dbDecimal:    Result := 'Decimal';
   dbFloat:      Result := 'Float';
   dbTime:       Result := 'Time';
   dbTimeStamp:  Result := 'TimeStamp';
   else          Result := 'Unkown DAO Type';
  end;
end;

function GetTableTypeStr(AType: longword): string;
begin
  result:='';
  if AType and dbAttachExclusive=dbAttachExclusive then
    result:=result+'AttachExclusive, ';
  if Atype and dbAttachSavePWD=dbAttachSavePWD then
    result:=result+'AttachSavePWD, ';
  if (Atype and dbSystemObject=dbSystemObject) or
     (Atype and dbSystemObject=DBSystemObject and $80000000) or
     (Atype and dbSystemObject=2)
  then
    result:=result+'System, ';
  if AType and dbAttachedTable=dbAttachedTable then
    result:=result+'Attached, ';
  if AType and dbAttachedODBC=dbAttachedTable then
    result:=result+'AttachedODBC, ';
  if AType and dbHiddenObject=dbHiddenObject then
    result:=result+'Hidden, ';
  result:=copy(result,1,length(result)-2);
  if Result='' then
    Result:='Normal';
  Result:=Result+Format(' (0x%x)',[AType]);  
end;

function GetQueryTypeStr(AType: Smallint): string;
var
  s: string;
begin
  if AType and 8 = 8 then
    s:=', (Hidden)'
  else
    s:='';
  result:='';
  case AType of
    dbQSelect, dbQSelect or 8: result:='Select';
    dbQProcedure, dbQProcedure or 8: result:='Procedure';
    dbQAction, dbQAction or 8: result:='Action';
    dbQCrosstab, dbQCrosstab or 8: result:='Crosstab';
    dbQDelete, dbQDelete or 8: result:='Delete';
    dbQUpdate, dbQUpdate or 8: result:='Update';
    dbQAppend, dbQAppend or 8: result:='Append';
    dbQMakeTable, dbQMakeTable or 8: result:='Make-table';
    dbQDDL, dbQDDL or 8: result:='Data-definition';
    dbQSQLPassThrough, dbQSQLPassThrough or 8: result:='Pass-through';
    dbQSetOperation, dbQSetOperation or 8: result:='Union';
    dbQSPTBulk, dbQSPTBulk or 8: result:='SPTBulk';
    dbQCompound, dbQCompound or 8: result:='Compound';
  end;
  Result:=Result+s;
end;

function GetRelationTypeStr(AType: dword): string;
begin
  case AType of
    dbRelationUnique: result:='1:1';
    dbRelationDontEnforce: result:='None';
    dbRelationInherited: result:='Inherited';
    dbRelationUpdateCascade: result:='Cascade update';
    dbRelationDeleteCascade: result:='Cascade delete';
    dbRelationLeft: result:='Left';
    dbRelationRight: result:='Right';
    else result:='1:N';
  end;
end;

function GetParameterTypeStr(AType: dword): string;
begin
  case AType of
    dbParamInput: Result:='Input';
    dbParamOutput: Result:='Output';
    dbParamInputOutput: Result:='InputOutput';
    dbParamReturnValue: Result:='Return';
  end;
end;

function GetStmtType(AStmt: string): SmallInt;
begin
  Astmt:=uppercase(AStmt);
  if pos(sqlSelect,AStmt)=1 then
    result:=dbQSelect
  else
    if pos(sqlDelete,AStmt)=1 then
      result:=dbQDelete
    else
      if pos(sqlUpdate,AStmt)=1 then
        result:=dbQUpdate
      else
        if pos(sqlInsert,AStmt)=1 then
          result:=dbQAppend
        else
          if pos(sqlCreate,AStmt)=1 then
            result:=dbQDDL
          else
            result:=dbQAction;
end;

function GetFieldValue(ARecordset: OLEVariant; AField, ANullValue: string): Variant;
begin
  if VarIsNull(ARecordset.Fields[AField].Value) then
    Result:=ANullValue
  else
    Result:=ARecordset.Fields[AField].Value;
end;

function GetFieldValue(ARecordset: OLEVariant; AField: integer; ANullValue: string): Variant;
begin
  if VarIsNull(ARecordset.Fields[AField].Value) then
    Result:=ANullValue
  else
    Result:=ARecordset.Fields[AField].Value;
end;

{ TMSAccess }

constructor TMSAccess.Create;
begin
  {$IFDEF JOURNAL}
  if Journal=nil then
    MessageDlg('MSAccess: Journal is not assigned.',mtError,[mbOK],0);
  {$ENDIF}
  FActive:=false;
  FOpen:=false;
  FConnected:=false;
  SetDAOType(dao35);
end;

function TMSAccess.CreateQuery;
begin
  try
    result:=FConnection.CreateQueryDef(aname,sql);
  except
    on e:Exception do begin
      raise;
      result:=null;
    end;
  end;
end;

function TMSAccess.CreateWorkspace;
begin
  FLastError:='';
  try
    result:=FEngine.CreateWorkspace(AName,AUser,APwd,dbUseJet);
  except
    on e:exception do begin
      FLastError:=e.message;
      result:=null;
    end;
  end;
end;

destructor TMSAccess.Destroy;
begin
  if TVarData(FEngine).VType=varDispatch then
//    IDispatch(FEngine)._Release;
    FEngine:=null;
  inherited;
end;

function GetActiveOleObject(const ClassName: string): IDispatch;
var
  ClassID: TCLSID;
  Unknown: IUnknown;
  HR: HRESULT;
begin
  ClassID:=ProgIDToClassID(ClassName);
  HR:=GetActiveObject(ClassID, nil, Unknown);
  if Succeeded(HR) then 
    HR:=Unknown.QueryInterface(IDispatch, Result);
  if not Succeeded(HR) then
    Result:=nil;
end;

function CreateOleObject(const ClassName: string): IDispatch;
var
  ClassID: TCLSID;
  HR: HRESULT;
begin
  ClassID := ProgIDToClassID(ClassName);
  HR:=CoCreateInstance(ClassID, nil, CLSCTX_INPROC_SERVER or
    CLSCTX_LOCAL_SERVER, IDispatch, Result);
  if not Succeeded(HR) then
    Result:=nil;
end;

function TMSAccess.GetDAOEngine(CLSID: string): OLEVariant;
var
  idisp: IDispatch;
begin
  try
    idisp:=GetActiveOLEObject(CLSID);
    if not Assigned(idisp) then
      idisp:=CreateOLEObject(CLSID);
    if not Assigned(idisp) then
      Result:=null
    else
      Result:=idisp;
  except
    on e:exception do begin
      raise;
      result:=null;
    end;
  end;
end;

function TMSAccess.GetQuery(index: word): OLEVariant;
begin
  FLastError:='';
  try
    result:=FConnection.QueryDefs[index];
  except
    on e:Exception do begin
      FLastError:=e.message;
      result:=null;
    end;
  end;
end;

procedure TMSAccess.GetTableList;
var
  i,n: integer;
begin
  AList.Clear;
  try
    n:=FConnection.Tabledefs.Count;
    for i:=0 to n-1 do
      AList.Add(FConnection.Tabledefs[i].Name);
  except on e:Exception do
    raise;
  end;
end;

procedure TMSAccess.GetQueryList;
var
  i,n: integer;
begin
  AList.Clear;
  try
    n:=FConnection.Querydefs.Count;
    for i:=0 to n-1 do
      AList.Add(FConnection.Querydefs[i].Name);
  except on e:Exception do
    raise;
  end;
end;

function TMSAccess.GetTable(index: word): OLEVariant;
begin
  try
    result:=FConnection.TableDefs[index];
  except
    on e:Exception do begin
      {$IFDEF JOURNAL}
      Journal.WriteSimpleEvent(Format('MSAccess.GetTable: %s',[e.message]),jeError);
      {$ENDIF}
      raise;
      result:=null;
    end;
  end;
end;

function TMSAccess.GetDAOVersion: string;
begin
  if FActive then
    result:=FEngine.Version
  else
    result:='';
end;

function TMSAccess.OpenWorkspace;
begin
  FLastError:='';
  if FActive then begin
    try
      FWorkspace:=FEngine.Workspaces[AName];
    except
      on e:Exception do begin
        FLAstError:=e.message;
        FWorkspace:=null;
      end;
    end;
    if TVarData(FWorkspace).VType<>varDispatch then
      try
        FLastError:='';
        FWorkspace:=FEngine.CreateWorkspace(AName,AUser,APwd,dbUseJet);
      except
        on e:Exception do begin
          FLAstError:=e.message;
          FWorkspace:=null;
        end;
      end;
  end;
  result:=TVarData(FWorkspace).VType=varDispatch;
end;

procedure TMSAccess.SetActive(const Value: boolean);
begin
  if FActive<>Value then begin
    if not Value then begin
      if DatabaseOpen then
        DatabaseOpen:=false;
      if EngineConnected then
        EngineConnected:=false;
      CloseEngine;
      FActive:=false;
    end else
      FActive:=OpenEngine(FDAOCS);
  end;
end;

procedure TMSAccess.SetQuery(index: word; const Value: OLEVariant);
begin
  try
    if index<FConnection.QueryDefs.Count then
      FConnection.QueryDefs[index].sql:=value.sql;
  except
    on e:Exception do
      raise;
  end;
end;

procedure TMSAccess.GetQueryFieldList(AQuery: string;
  var AList: TStringlist);
var
  i,n: integer;
  QD :OLEVariant;
begin
  AList.Clear;
  try
    QD:=GetQueryByName(AQuery);
    if TVarData(QD).VType=varDispatch then begin
      n:=QD.Fields.Count;
      for i:=0 to n-1 do
        AList.Add(QD.Fields[i].Name);
    end;
  except on e:Exception do
    raise;
  end;
end;

procedure TMSAccess.GetTableFieldList(ATable: string;
  var AList: TStringlist);
var
  i,n: integer;
  TD :OLEVariant;
begin
  AList.Clear;
  try
    TD:=GetTableByName(ATable);
    if TVarData(TD).VType=varDispatch then begin
      n:=TD.Fields.Count;
      for i:=0 to n-1 do
        AList.Add(TD.Fields[i].Name);
    end;
  except on e:Exception do
    raise;
  end;
end;

function TMSAccess.GetQueryDefCount: word;
begin
  result:=FConnection.QueryDefs.Count;
end;

function TMSAccess.GetTableDefCount: word;
begin
  result:=FConnection.TableDefs.Count;
end;

function TMSAccess.GetQueryByName(Name: string): OLEVariant;
begin
  Result:=FConnection.QueryDefs[Name];
end;

function TMSAccess.GetTableByName(name: string): OLEVariant;
begin
  result:=FConnection.TableDefs[Name];
end;

procedure TMSAccess.SetQueryByName(Name: string; const Value: OLEVariant);
begin
  FConnection.QueryDefs[Name].SQL:=Value.SQL;
end;

function TMSAccess.OpenDynaset(ASQL: string): OLEVariant;
begin
  Result:=null;
  if FOpen then
    result:=FConnection.OpenRecordset(asql,dbOpenDynaset,dbPessimistic);
end;

function TMSAccess.OpenTable(ATable: string): OLEVariant;
begin
  if FOpen then
    try
      result:=FConnection.OpenRecordset(atable,dbOpenTable,dbPessimistic)
    except
      on e:Exception do begin
        raise;
        result:=null;
      end;
    end;
end;

function TMSAccess.GetGroup;
begin
  if FActive and (index<FWorkspace.Groups.Count) then
    result:=FWorkspace.Groups[index].Name
  else
    result:='';
end;

function TMSAccess.GetGroupCount;
begin
  if FActive then
    result:=FWorkspace.Groups.Count
  else
    result:=0;
end;

function TMSAccess.GetGroupIndex(AName: string): integer;
var
  i,c :integer;
begin
  result:=-1;
  AName:=uppercase(AName);
  if FActive then begin
    c:=FWorkspace.Groups.Count;
    for i:=0 to c-1 do
      if uppercase(FWorkspace.Groups[i].Name)=AName then begin
        result:=i;
        break;
      end;
  end;
end;

procedure TMSAccess.GetGroupUsers;
var
  i,c,idx :integer;
begin
  idx:=-1;
  AName:=uppercase(AName);
  Userlist.clear;
  if FActive then begin
    c:=FWorkspace.Groups.Count;
    for i:=0 to c-1 do
      if uppercase(FWorkspace.Groups[i].Name)=AName then begin
        idx:=i;
        break;
      end;
    if idx>-1 then begin
      FWorkspace.Groups[idx].Users.refresh;
      c:=FWorkspace.Groups[idx].Users.Count;
      for i:=0 to c-1 do
        Userlist.add(FWorkspace.Groups[idx].Users[i].Name);
    end;
  end;
end;


function TMSAccess.GetUser;
begin
  if FActive and (index<FWorkspace.Users.Count) then
    result:=FWorkspace.Users[index].Name
  else
    result:='';
end;

function TMSAccess.GetUserCount;
begin
  if FActive then
    result:=FWorkspace.Users.Count
  else
    result:=0;
end;

procedure TMSAccess.GetUserGroups;
var
  i,c,idx :integer;
begin
  idx:=-1;
  AName:=uppercase(AName);
  Grouplist.clear;
  if FActive then begin
    c:=FWorkspace.Users.Count;
    for i:=0 to c-1 do
      if uppercase(FWorkspace.Users[i].Name)=AName then begin
        idx:=i;
        break;
      end;
    if idx>-1 then begin
      FWorkspace.Users[idx].Groups.refresh;
      c:=FWorkspace.Users[idx].Groups.Count;
      for i:=0 to c-1 do
        Grouplist.add(FWorkspace.Users[idx].Groups[i].Name);
    end;
  end;
end;

function TMSAccess.GetUserIndex(AName: string): integer;
var
  i,c :integer;
begin
  result:=-1;
  AName:=uppercase(AName);
  if FActive then begin
    c:=FWorkspace.Users.Count;
    for i:=0 to c-1 do
      if uppercase(FWorkspace.Users[i].Name)=AName then begin
        result:=i;
        break;
      end;
  end;
end;

function TMSAccess.AddUser(AName, APID, APwd: string): boolean;
var
  Account :OLEVariant;
begin
  result:=false;
  if FActive then
    try
      Account:=FWorkspace.CreateUser(aname,apid,apwd);
      FWorkspace.Users.Append(Account);
      result:=true;
      //account.close;
      account:=null;
    except on e:Exception do
      raise;
    end;
end;

procedure TMSAccess.DeleteUser(AName: string);
begin
  if FActive then
    try
      FWorkspace.Users.Delete(AName);
    except on e:Exception do
      raise;
    end;
end;

function TMSAccess.AddGroup(AName, APID: string): boolean;
var
  Account :OLEVariant;
begin
  result:=false;
  if FActive then
    try
      Account:=FWorkspace.CreateGroup(aname,apid);
      FWorkspace.Groups.Append(Account);
//      account.close;
      account:=null;
      result:=true;
    except on e:Exception do
      raise;
    end;
end;

procedure TMSAccess.DeleteGroup(AName: string);
begin
  if FActive then
    try
      FWorkspace.Groups.delete(AName);
    except on e:Exception do
      raise;
    end;
end;

function TMSAccess.AddUserToGroup;
var
  VGroup,VUser :OLEVariant;
  i :integer;
begin
  result:=false;
  AGroupname:=uppercase(AGroupname);
  if FActive then
    try
      for i:=0 to FWorkspace.Groups.count-1 do
        if uppercase(FWorkspace.Groups[i].Name)=AGroupname then begin
          vgroup:=FWorkspace.Groups[i];
          break;
        end;
      if TVarData(VGroup).VType=varDispatch then begin
        VUser:=VGroup.CreateUser(ausername);
        VGroup.Users.Append(VUser);
        //VUser.close;
        VUser:=null;
        //VGroup.close;
        VGroup:=null;
        result:=true;
      end;
    except on e:Exception do
      raise;
    end;
end;

procedure TMSAccess.DeleteUserFromGroup;
var
  VAccount :OLEVariant;
  i :integer;
begin
  if FActive then
    try
      for i:=0 to FWorkspace.Groups.count-1 do
        if FWorkspace.Groups[i].Name=AGroupname then begin
          vaccount:=FWorkspace.Groups[i];
          break;
        end;
      if TVarData(vaccount).VType=varDispatch then begin
        vaccount.Users.Delete(ausername);
        //vaccount.close;
        vaccount:=null;
      end;
    except on e:Exception do
      raise;
    end;
end;

function TMSAccess.ChangeUserPwd;
var
  i :integer;
  VUser :OLEVariant;
begin
  result:=false;
  aname:=uppercase(aname);
  if FActive then
   try
      for i:=0 to FWorkspace.Users.count-1 do
        if uppercase(FWorkspace.Users[i].Name)=Aname then begin
          vuser:=FWorkspace.Users[i];
          break;
        end;
      if TVarData(VUser).VType=varDispatch then begin
        VUser.NewPassword(aoldpwd,anewpwd);
        result:=true;
        //VUser.close;
        Vuser:=null;
      end;
   except
     on e:Exception do
      raise;
   end;
end;

function TMSAccess.AddGroupToUser(AUserName,
  AGroupName: string): boolean;
var
  VGroup,VUser :OLEVariant;
  i :integer;
begin
  result:=false;
  AUsername:=uppercase(ausername);
  if FActive then
    try
      for i:=0 to FWorkspace.Users.count-1 do
        if uppercase(FWorkspace.Users[i].Name)=AUsername then begin
          vuser:=FWorkspace.Users[i];
          break;
        end;
      if TVarData(VUser).VType=varDispatch then begin
        VGroup:=VUser.CreateGroup(agroupname);
        VUser.Groups.Append(VGroup);
        //VUser.close;
        VUser:=null;
        //VGroup.close;
        VGroup:=null;
        result:=true;
      end;
    except on e:Exception do
      raise;
    end;
end;

procedure TMSAccess.DeleteGroupFromUser(AUserName, AGroupName: string);
var
  VAccount :OLEVariant;
  i :integer;
begin
  if FActive then
    try
      for i:=0 to FWorkspace.Users.count-1 do
        if FWorkspace.Users[i].Name=AUsername then begin
          vaccount:=FWorkspace.Users[i];
          break;
        end;
      if TVarData(vaccount).VType=varDispatch then begin
        vaccount.Groups.Delete(aGroupname);
        //vaccount.close;
        vaccount:=null;
      end;
    except on e:Exception do
      raise;
    end;
end;

function TMSAccess.GetJetVersion: string;
begin
  if FConnected then
    result:=FConnection.Version
  else
    result:='';
end;

procedure TMSAccess.CompactDatabase(const OldFilename, NewFilename: string);
begin
  if FActive then
    try
      FEngine.CompactDatabase(OldFilename,NewFilename);
    except
      raise;
    end;
end;

procedure TMSAccess.RepairDatabase(const Filename: string);
begin
  if FActive then
    try
      FEngine.RepairDatabase(Filename);
    except
      raise;
    end;
end;

procedure TMSAccess.DecryptDatabase(const OldFilename, NewFilename,
  Password: string);
begin
  if FActive then
    try
      FEngine.CompactDatabase(OldFilename,NewFilename,dbDecrypt,';pwd='+Password);
    except
      raise;
    end;
end;

procedure TMSAccess.EncryptDatabase(const OldFilename, NewFilename,
  Password: string);
begin
  if FActive then
    try
      FEngine.CompactDatabase(OldFilename,NewFilename,dbEncrypt,';pwd='+Password);
    except
      raise;
    end;
end;

function TMSAccess.OpenDatabase;
begin
  if FConnected then
    try
      FLastError:='';
      result:=FWorkspace.OpenDatabase(FDB,Exclusive,ReadOnly);
      {$IFDEF JOURNAL}
      Journal.WriteSimpleEvent(Format('MSAccess: Database %s connected and opened',[FDB]),jeAction);
      {$ENDIF}
    except
      on e:exception do begin
        {$IFDEF JOURNAL}
        Journal.WriteSimpleEvent(Format('MSAccess: Database %s: %s',[FDB,e.Message]),jeError);
        {$ENDIF}
        FLAstError:=e.message;
        result:=null;
      end;
    end
  else
    result:=null;
end;

function TMSAccess.GetCurrentWorkspace: string;
begin
  if FConnected then
    result:=FWorkspace.Name
  else
    result:=''
end;

function TMSAccess.GetDB: string;
begin
  if FOpen then
    result:=FConnection.Name
  else
    result:='';
end;

function TMSAccess.GetUsername: string;
begin
  if FConnected then
    result:=FWorkspace.Username
  else
    if FActive then
      result:=FEngine.DefaultUser
    else
      result:='';
end;

function TMSAccess.GetOpenDatabase: OLEVariant;
begin
  result:=FConnection;
end;

function TMSAccess.GetContainer(index: word): OLEVariant;
begin
  try
    result:=FConnection.Containers[index];
  except
    on e:Exception do begin
      raise;
      result:=null;
    end;
  end;
end;

function TMSAccess.GetContainerByName(Name: string): OLEVariant;
begin
  try
    result:=FConnection.Containers[Name];
  except
    on e:Exception do begin
      raise;
      result:=null;
    end;
  end;
end;

function TMSAccess.GetContainerCount: word;
begin
  result:=FConnection.Containers.Count;
end;

procedure TMSAccess.GetContainerDocumentList(ACont: string;
  var AList: TStringlist);
var
  i,n: integer;
  OD :OLEVariant;
begin
  AList.Clear;
  try
    OD:=GetContainerByName(ACont);
    if TVarData(OD).VType=varDispatch then begin
      n:=OD.Documents.Count;
      for i:=0 to n-1 do
        AList.Add(OD.Documents[i].Name);
    end;
  except on e:Exception do
    raise;
  end;
end;

procedure TMSAccess.GetContainerList(var AList: TStringList);
var
  i,n: integer;
begin
  AList.Clear;
  try
    n:=FConnection.Containers.Count;
    for i:=0 to n-1 do
      AList.Add(FConnection.Containers[i].Name);
  except on e:Exception do
    raise;
  end;
end;

function TMSAccess.GetRecordset(index: word): OLEVariant;
begin
  try
    result:=FConnection.Recordsets[index];
  except
    on e:Exception do begin
      raise;
      result:=null;
    end;
  end;
end;

function TMSAccess.GetRecordsetCount: word;
begin
  result:=FConnection.Recordsets.Count;
end;

procedure TMSAccess.GetRecordsetFieldList(ARecordset: string;
  var AList: TStringlist);
var
  i,n: integer;
  OD :OLEVariant;
begin
  AList.Clear;
  try
    OD:=GetRecordsetByName(ARecordset);
    if TVarData(OD).VType=varDispatch then begin
      n:=OD.Fields.Count;
      for i:=0 to n-1 do
        AList.Add(OD.Fields[i].Name);
    end;
  except on e:Exception do
    raise;
  end;
end;

procedure TMSAccess.GetRecordsetList(var AList: TStringList);
var
  i,n: integer;
begin
  AList.Clear;
  try
    n:=FConnection.Recordsets.Count;
    for i:=0 to n-1 do
      AList.Add(FConnection.RecordSets[i].Name);
  except on e:Exception do
    raise;
  end;
end;

function TMSAccess.GetRelation(index: word): OLEVariant;
begin
  try
    result:=FConnection.Relations[index];
  except
    on e:Exception do begin
      raise;
      result:=null;
    end;
  end;
end;

function TMSAccess.GetRelationByName(Name: string): OLEVariant;
begin
  try
    result:=FConnection.Relations[Name];
  except
    on e:Exception do begin
      raise;
      result:=null;
    end;
  end;
end;

function TMSAccess.GetRelationCount: word;
begin
  result:=FConnection.Relations.Count;
end;

procedure TMSAccess.GetRelationFieldList(ARelation: string;
  var AList: TStringlist);
var
  i,n: integer;
  OD :OLEVariant;
begin
  AList.Clear;
  try
    OD:=GetRelationByName(ARelation);
    if TVarData(OD).VType=varDispatch then begin
      n:=OD.Fields.Count;
      for i:=0 to n-1 do
        AList.Add(OD.Fields[i].Name);
    end;
  except on e:Exception do
    raise;
  end;
end;

procedure TMSAccess.GetRelationList(var AList: TStringList);
var
  i,n: integer;
begin
  AList.Clear;
  try
    n:=FConnection.Relations.Count;
    for i:=0 to n-1 do
      AList.Add(FConnection.Relations[i].Name);
  except on e:Exception do
    raise;
  end;
end;

function TMSAccess.GetRecordsetByName(Name: string): OLEVariant;
begin
  try
    result:=FConnection.Resordsets[Name];
  except
    on e:Exception do begin
      raise;
      result:=null;
    end;
  end;
end;

function TMSAccess.FindDocument(AName: string): OLEVariant;
var
  i,j,n,nd: integer;
  found: boolean;
begin
  found:=false;
  n:=ContainerCount;
  for i:=0 to n-1 do begin
    nd:=Containers[i].Documents.Count;
    for j:=0 to nd-1 do begin
      if AName=Containers[i].Documents[j].Name then begin
        found:=true;
        result:=Containers[i].Documents[j];
        break;
      end;
    end;
    if found then
      break;
  end;
end;

procedure TMSAccess.SetDAOType(Value: TDAOType);
begin
  if not FActive then begin
    FDAOType:=Value;

    case FDAOType of
      dao30: FDAOCS:=daoEngine30;
      dao35: FDAOCS:=daoEngine35;
      dao36: FDAOCS:=daoEngine36;
      dao120: FDAOCS:=daoEngine120;
    end;
  end else
    raise EMSAccessException(SCannotChngDAO);
end;

procedure TMSAccess.SetConnected(const Value: boolean);
begin
  if (Value<>FConnected) then begin
    if not Value then begin
      if FOpen then
        DatabaseOpen:=false;
      CloseWorkspace;
      FConnected:=false;
    end else
      FConnected:=OpenWorkspace(daoDefaultWorkspace,FUsername,FPassword);
  end;
end;

procedure TMSAccess.CloseDatabase;
begin
  FConnection.Close;
  FConnection:=null;
  {$IFDEF JOURNAL}
  Journal.WriteSimpleEvent('MSAccess: Database closed',jeAction);
  {$ENDIF}
end;

procedure TMSAccess.SetOpen(const Value: boolean);
begin
  if (Value<>FOpen) then begin
    if not Value then
      CloseDatabase
    else
      FConnection:=Opendatabase(FDB,FExclusive,FReadOnly);
  end;
  FOpen:=TVarData(FConnection).VType=varDispatch;
end;

procedure TMSAccess.CloseWorkspace;
begin
  FWorkspace.Close;
  FWorkspace:=null;
end;

procedure TMSAccess.CloseEngine;
begin
  //IDispatch(FEngine)._Release;
  FEngine:=null;
end;

function TMSAccess.OpenEngine(ClassString: string): boolean;
var
  s: string;
begin
  try
    FLastError:='';
    FEngine:=GetDAOEngine(ClassString);
    if TVarData(FEngine).VType=varDispatch then begin
      FEngine.DefaultType:=dbUseJet;
      FEngine.DefaultUser:=daoDefaultUser;
      FEngine.DefaultPassword:=daoDefaultPassword;
      if not FileExists(fsysdb) then
        fsysdb:=ExpandFilename(FileSearch(daoSystemDB,GetSysDir+';'+GetWinDir+';'));
      if FileExists(fsysdb) then
        FEngine.SystemDB:=s;
      result:=true;
    end else
      result:=false;
  except
    on e: exception do begin
      FLastError:=e.message;
      result:=false;
    end;
  end;
end;

function TMSAccess.GetSystemDatabase: string;
var
  s: string;
begin
  s:=FEngine.SystemDB;
  if FActive then
    result:=s
  else
    result:='';
end;

function TMSAccess.GetWorkspaceCount: integer;
begin
  Result:=0;
  if FActive then
    try
      Result:=FEngine.Workspaces.Count;
    except
      raise;
    end;
end;

function TMSAccess.GetWorkspace;
begin
  if FActive and (index<FEngine.Workspaces.Count) then
    result:=FEngine.Workspaces[Index]
  else
    result:=null;
end;

initialization
  {$IFDEF JOURNAL}
  Journal:=nil;
  {$ENDIF}
end.
