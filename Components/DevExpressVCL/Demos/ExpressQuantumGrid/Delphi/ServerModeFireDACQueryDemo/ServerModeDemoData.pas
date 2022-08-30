unit ServerModeDemoData;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  SysUtils, Classes, DB, dxServerModeData, dxServerModeADODataSource, ServerModeDemoConnection,
  dxServerModeFireDACDataSource, Controls, Dialogs,
{$IFDEF DELPHI19}
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Dapt,
  FireDAC.Comp.Client, FireDAC.VCLUI.Wait, FireDAC.Comp.UI, FireDAC.Phys.MSSQL;
{$ELSE}
  uADStanIntf, uADStanOption, uADStanError,
  uADGUIxIntf, uADPhysIntf, uADStanDef, uADStanPool, uADStanAsync,
  uADPhysManager, uADGUIxFormsWait, uADStanParam, uADDatSManager, uADDAptIntf,
  uADDAptManager, uADCompDataSet, uADCompClient,
  uADPhysODBCBase, uADPhysMSSQL, uADCompGUIx, cxClasses;
{$ENDIF}

type
  TServerModeDemoDataDM = class(TDataModule)
    ServerModeQueryDataSource: TdxServerModeFireDACQueryDataSource;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure ServerModeQueryDataSourceFatalError(Sender: TdxServerModeCustomDataSource; const AMessage: string);
    procedure ServerModeQueryDataSourceInconsistentCache(Sender: TdxServerModeCustomDataSource; const AMessage: string;
      var ARecoverCache: Boolean);
  private
  {$IFDEF DELPHI19}
    ADConnection: TFDConnection;
    ADQuery: TFDQuery;
  {$ELSE}
    ADConnection: TADConnection;
    ADQuery: TADQuery;
  {$ENDIF}
    function DoGetRecordsCount(const ATableName: string): Integer;
    function GetCustomerInsertSQL: string;
    function GetOrderInsertSQL: string;
  public
    procedure AddRecords(ACount: Integer; AProgress: TdxProgressEvent);
    procedure Connect(const AHostName, ADatabaseName, AUserName, APassword: string;
      OSAuthentication: Boolean);
    procedure CreateDatabase;
    procedure CreateTable;
    procedure ExecSQL(const ASQL: string);
    function GetRecordsCount: Integer;
  end;

function GetCaption: string;
function GetDatabaseName: string;
function GetOrdersTableName: string;
function GetCustomersTableName: string;
function GetDescription: string;

const
  dxCustomersTableName = 'ServerModeGridCustomersDemo';
  dxOrdersTableName = 'ServerModeGridOrdersDemo';
  MaxCustomerCount = 100;
var
  ServerModeDemoDataDM: TServerModeDemoDataDM;

implementation

uses
  DateUtils;

{$R *.dfm}

function GetCaption: string;
begin
  Result := 'ExpressQuantumGrid FireDAC Server Mode Query Demo';
end;

function GetDatabaseName: string;
begin
  Result := 'ServerModeGridDemo';
end;

function GetOrdersTableName: string;
begin
  Result := dxOrdersTableName;
end;

function GetCustomersTableName: string;
begin
  Result := dxCustomersTableName;
end;

function GetDescription: string;
begin
  Result := 'This demo shows a grid control''s capabilities when bound to a large ' +
    'amount of data in Server Mode via a FireDAC connection. The data is retrieved ' +
    'using a FireDAC query.';
end;

{ TServerModeDemoDataDM }


procedure TServerModeDemoDataDM.DataModuleCreate(Sender: TObject);
begin
{$IFDEF DELPHI19}
  ADConnection := TFDConnection.Create(nil);
  ADQuery := TFDQuery.Create(nil);
{$ELSE}
  ADConnection := TADConnection.Create(nil);
  ADQuery := TADQuery.Create(nil);
{$ENDIF}
  ADConnection.LoginPrompt := False;
  ADQuery.Connection := ADConnection;
  ADQuery.ResourceOptions.AssignedValues := [rvDirectExecute];
  ADQuery.ResourceOptions.DirectExecute := True;
  ServerModeQueryDataSource.SQLAdapterClassName := 'TdxServerModeMSSQLAdapter';
  ServerModeQueryDataSource.Connection := ADConnection;
end;

procedure TServerModeDemoDataDM.DataModuleDestroy(Sender: TObject);
begin
  ADQuery.Free;
  ADConnection.Free;
end;

procedure TServerModeDemoDataDM.AddRecords(ACount: Integer; AProgress: TdxProgressEvent);
var
  ASQL: string;
  ASubCount: Integer;

  procedure AddCustomers(ACustomersCount: Integer);
  var
    I, J: Integer;
  begin
    ADConnection.StartTransaction;
    try
      for I := 0 to (ACustomersCount div ASubCount) - 1 do
      begin
        ASQL := '';
        for J := 1 to ASubCount do
          ASQL := ASQL + GetCustomerInsertSQL;
        ADConnection.ExecSQL(ASQL);
      end;
      ADConnection.Commit;
    except
      ADConnection.Rollback;
    end;
  end;

var
  I, J, ACustomerCount: Integer;
begin
  ASubCount := 10;
  ACustomerCount := DoGetRecordsCount(dxCustomersTableName);
  if ACustomerCount < MaxCustomerCount then
    AddCustomers(MaxCustomerCount - ACustomerCount);
  ADConnection.StartTransaction;
  try
    for I := 0 to (ACount div ASubCount) - 1 do
    begin
      if I mod 100 = 0 then
        AProgress(Self, 100 / (ACount div ASubCount) * I);
      ASQL := '';
      for J := 1 to ASubCount do
        ASQL := ASQL + GetOrderInsertSQL;
      ADConnection.ExecSQL(ASQL);
    end;
    ADConnection.Commit;
  except
    ADConnection.Rollback;
  end;
end;

procedure TServerModeDemoDataDM.Connect(const AHostName, ADatabaseName, AUserName, APassword: string;
  OSAuthentication: Boolean);
begin
  ADConnection.Connected := False;
  ADConnection.LoginPrompt := False;
  ADConnection.Params.Values['DriverID'] :=  'MSSQL';
  if OSAuthentication then
    ADConnection.Params.Values['OSAuthent'] := 'Yes'
  else
  begin
    ADConnection.Params.Values['OSAuthent'] := 'No';
    ADConnection.Params.Values['User_Name'] := AUserName;
    ADConnection.Params.Values['Password'] := APassword;
  end;
  ADConnection.Params.Values['Server'] :=  AHostName;
  ADConnection.Params.Values['Database'] := ADatabaseName;
end;

procedure TServerModeDemoDataDM.CreateDatabase;
begin
  ExecSQL(Format('IF DB_ID(N''%s'') IS NULL' + sLineBreak +
    'CREATE DATABASE "%0:s";',
    [GetDatabaseName]));
end;

procedure TServerModeDemoDataDM.CreateTable;
var
  ASQL: string;
begin
  ASQL := Format('IF OBJECT_ID(N''' + GetDatabaseName + '.dbo.%s'') IS NULL' + sLineBreak +
    'CREATE TABLE "dbo"."%0:s"(' +
    ' "OID" int IDENTITY(1,1) NOT NULL,' +
    ' "FirstName" nvarchar(10) NOT NULL,' +
    ' "LastName" nvarchar(10) NOT NULL,' +
    ' "Company" nvarchar(30) NOT NULL,' +
    ' "Prefix" nvarchar(5) NOT NULL,' +
    ' "Title" nvarchar(5) NOT NULL,' +
    ' "Address" nvarchar(50) NOT NULL,' +
    ' "City" nvarchar(15) NOT NULL, ' +
    ' "State" nvarchar(2) NOT NULL,' +
    ' "ZipCode" nvarchar(10) NOT NULL,' +
    ' "Source" nvarchar(10) NOT NULL,' +
    ' "Customer" bit NULL,' +
    ' "HomePhone" nvarchar(15) NOT NULL,' +
    ' "FaxPhone" nvarchar(15) NOT NULL,' +
    ' "Description" text NOT NULL,' +
    ' "Email" nvarchar(30) NOT NULL' +
    ' CONSTRAINT "PK_%0:s" PRIMARY KEY CLUSTERED' +
    '(' +
    ' "OID" ASC' +
    ')WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON "PRIMARY"' +
    ') ON "PRIMARY";', [dxCustomersTableName]);
  ExecSQL(ASQL);

  ASQL := Format('IF OBJECT_ID(N''' + GetDatabaseName + '.dbo.%s'') IS NULL' + sLineBreak +
    'CREATE TABLE "dbo"."%0:s"(' +
    ' "OID" int IDENTITY(1,1) NOT NULL,' +
    ' "CustomerID" bigint NOT NULL,' +
    ' "OrderDate" datetime NULL,' +
    ' "Trademark" nvarchar(20) NOT NULL,' +
    ' "Model" nvarchar(30) NOT NULL,' +
    ' "HP" bigint NOT NULL,' +
    ' "Cyl" bigint NOT NULL,' +
    ' "TransmissSpeedCount" bigint NOT NULL,' +
    ' "TransmissAutomatic" bit NULL,' +
    ' "Category" nvarchar(10) NOT NULL,' +
    ' "Price" bigint NOT NULL,' +
    ' CONSTRAINT "PK_%0:s" PRIMARY KEY CLUSTERED' +
    '(' +
    ' "OID" ASC' +
    ')WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON "PRIMARY"' +
    ') ON "PRIMARY";', [dxOrdersTableName]);
  ExecSQL(ASQL);
end;

function TServerModeDemoDataDM.DoGetRecordsCount(const ATableName: string): Integer;
var
  ASQL: string;
begin
  Result := 0;
  ASQL := Format('IF DB_ID(N''%s'') IS NOT NULL AND OBJECT_ID(N''%0:s.dbo.%1:s'') IS NOT NULL' + sLineBreak +
    '  SELECT COUNT(*) FROM %0:s.dbo.%1:s;' + sLineBreak +
    'ELSE' + sLineBreak +
    '  SELECT 0;', [GetDatabaseName, ATableName]);
  ADQuery.Active := False;
  ADQuery.SQL.Text := ASQL;
  ADQuery.Open;
  if not ADQuery.Eof then
    Result := ADQuery.Fields[0].AsInteger;
end;

procedure TServerModeDemoDataDM.ExecSQL(const ASQL: string);
begin
  ADQuery.Active := False;
  ADQuery.SQL.Text := ASQL;
  ADQuery.ExecSQL;
end;

function TServerModeDemoDataDM.GetCustomerInsertSQL: string;
const
  AFirstName: array[0..18] of string = ('Jane', 'Sam', 'Karen', 'Bobbie', 'Ricardo', 'Frank', 'Christa',
    'Jimmie', 'Alfred', 'James', 'Robert', 'June', 'Mildred', 'Henry', 'Michael', 'Scott', 'Mickey',
    'Roger', 'Leticia');
  ALastName: array[0..18] of string = ('Doe', 'Hill', 'Holmes', 'Valentine', 'Menendez', 'Frankson',
    'Christie', 'Jones', 'Newman', 'Johnson', 'James', 'Alessandro', 'Johansson', 'McAllister', 'Jeffers',
    'Mathewson', 'Alcorn', 'Michelson', 'Ford');
  ACompany: array[0..18] of string = ('Doe Enterprises', 'Hill Corporation', 'Holmes World',
    'Valentine Hearts', 'Menedez Development', 'Frankson Media', 'Christies House of Design', 'Jones & Assoc',
    'Newman Systems', 'Development House', 'James Systems', 'Alessandro & Associates', 'Mildreds World',
    'McAllister Systems', 'Jeffers Clinic', 'Mathewson Design', 'Mickeys World of Fun','Michelson Systems',
    'Ford Consulting');
  APrefix: array[0..18] of string = ('Ms.', 'Mr.', 'Ms.', 'Mr.', 'Mr.', 'Mr.', 'Ms.', 'Mrs.', 'Mr.', 'Mr.',
    'Mr.', 'Mrs.', 'Ms.', 'Mr.', 'Mr.', 'Mr.', 'Mr.', 'Mr.', 'Mrs.');
  ATitle: array[0..18] of string = ('', '', '', 'Dr.', '', '', 'PhD', '', 'PhD', 'PhD', 'MS', 'PhD', '', 'MS',
    'PhD', 'MA', '', 'PhD', 'PhD');

  AAddress: array[0..18] of string = ('123 Home Lane', '45 Hill St.', '9333 Holmes Dr.', '933 Heart St. Suite 1',
    '939 Center Street', '121 Media Center Drive', '349 Graphic Design Lane', '990 King Lane', '900 Newman Center',
    '93900 Carter Lane', '390-1 Fourth St.', '90283 Los Angeles Ave.', '390290 Mildred Lane', '029-938 Excelsior Way',
    '233 First St.', '111 McHenry St.', '436 1st Ave.', '3920 Michelson Dr.', '2900 Ford Drive');
  ACity: array[0..18] of string = ('Atlanta', 'Hillsville', 'Johnsonville', 'Chicago', 'Atlanta', 'New York', 'New York',
    'Kingsville', 'Newman', 'Cartersville', 'New York', 'Los Angeles', 'Cleveland', 'San Francisco', 'Los Angeles',
    'New York', 'Cleveland', 'Bridgeford', 'Lansing');
  AState: array[0..18] of string = ('CA', 'VA', 'NY', 'IL', 'GA', 'NY', 'CA', 'CA', 'OK', 'GA', 'NY', 'CA', 'OH', 'CA',
    'CA', 'NY', 'OH', 'CT', 'MI');
  AZipCode: array[0..18] of string = ('55555', '44444', '12121', '89123', '45012', '12121', '12211', '93939',
    '9900-', '909309', '12121', '93090', '37288-2323', '98454', '92939', '11111-2222', '37288', '93200', '23920');
  ASource: array[0..18] of string = ('', 'NY Times', 'Bob', 'Jennie', '', 'NY Times', 'NY Times', '', 'LA Times', '',
    'NY Times', 'LA Times', 'Referral', 'Referral', 'LA Times', 'NY Times', 'Referral', '', 'Referral');
  AHomePhone: array[0..18] of string = ('(233)323-33-23', '(222)222-22-22', '(333)333-33-33', '(898)745-15-11',
    '(151)615-16-11', '(339)339-39-39', '(939)399-99-99', '(993)939-93-99', '(930)930-30-93', '(309)209-30-20',
    '(029)020-90-90', '(902)092-09-30', '(090)983-02-48', '(923)022-08-34', '(093)008-30-23', '(209)093-84-98',
    '(000)002-32-32', '(203)290-89-02', '(228)320-83-20');
  AFaxPhone: array[0..18] of string = ('(444)444-44-44', '(222)222-22-22', '(212)556-56-55', '(321)516-51-51',
    '(656)161-65-16', '(393)090-20-99', '(909)098-00-98', '(930)930-98-09', '(920)983-02-02', '(094)302-89-08',
    '(090)909-00-90', '(940)104-80-93', '(190)890-02-83', '(084)029-80-28', '(080)098-90-08', '(098)900-98-90',
    '(098)902-98-34', '(098)900-83-04', '(098)908-00-80');
  ADescription: array[0..18] of string = ('This is a description for Jane Doe.'#$D#$A'Notice the Auto Preview Feature.',
    'This is a description for Sam Hill.'#$D#$A'Notice the Auto Preview Feature.',
    'This is a description for Karen Holmes.'#$D#$A'Notice the Auto Preview Feature.', '',
    'This is a description for Ricardo Menendez.'#$D#$A'Notice the Auto Preview Feature.',
    'This is a description for Frank Frankson.'#$D#$A'Notice the Auto Preview Feature.',
    'This is a description for Christa Christie.'#$D#$A'Notice the Auto Preview Feature.',
    'This is a description for Jimmie Jones.'#$D#$A'Notice the Auto Preview Feature.',
    'This is a description for Alfred Newman.'#$D#$A'Notice the Auto Preview Feature.',
    'This is a description for James Johnson.'#$D#$A'Notice the Auto Preview Feature.',
    'This is a description for Robert James.'#$D#$A'Notice the Auto Preview Feature.', '',
    'This is a description for Mildred Johansson.'#$D#$A'Notice the Auto Preview Feature.', '',
    'This is a description for Michael Jeffers.'#$D#$A'Notice the Auto Preview Feature.',
    'This is a description for Scott Mathewson.'#$D#$A'Notice the Auto Preview Feature.',
    'This is a description for Mickey Alcorn.'#$D#$A'Notice the Auto Preview Feature.',
    'This is a description for Mickey Alcorn.'#$D#$A'Notice the Auto Preview Feature.',
    '');
  AEmail: array[0..18] of string = ('doej@doeent.com', 'hills@hillcorp.com', 'holmesk@holmesw.com',
    'valentineb@valetntineh.com', 'menendezr@menedezdev.com', 'franksonf@frankfmedia.com',
    'christiec@christiesdesign.com', 'jonesj@jonesjim.com', 'newmanalf@newmansyst.com',
    'johnsonj@jdevhouse.com', 'jamesr@jrsengin.com', 'alessandroj@alessandroassoc',
    'johanssonm@mildrworld.com', 'mcallisterh@mcallistersyst.com', 'jeffersm@jeffersclinic.com',
    'mathewsons@mathewstondsgn.com', 'alcornm@mikeysworld.com', 'michelsonr@michelsonsyst.com',
    'fordl@fordconsult.com');

  function GetCustomer: string;
  begin
    case Random(3) of
      0:
        Result := '0';
      1:
        Result := '1';
      else
        Result := 'NULL';
    end;
  end;

var
  ACustomerIndex, AAddressIndex: Integer;
begin
  ACustomerIndex := Random(19);
  AAddressIndex := Random(19);
  Result := Format('INSERT INTO "%s" ("FirstName", "LastName", "Company", "Prefix", "Title", "Address", "City", ' +
    '"State", "ZipCode", "Source", "Customer", "HomePhone", "FaxPhone", "Description", "Email") VALUES ' +
    '(''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', %s, ''%s'', '+
    '''%s'', ''%s'', ''%s'')', [dxCustomersTableName, AFirstName[ACustomerIndex], ALastName[ACustomerIndex], ACompany[ACustomerIndex],
    APrefix[ACustomerIndex], ATitle[ACustomerIndex], AAddress[AAddressIndex], ACity[AAddressIndex],
    AState[AAddressIndex], AZipCode[AAddressIndex], ASource[AAddressIndex], GetCustomer, AHomePhone[Random(19)],
    AFaxPhone[Random(19)], ADescription[AAddressIndex], AEmail[AAddressIndex]]);
end;

function TServerModeDemoDataDM.GetOrderInsertSQL: string;

  function GetDateTime: string;
  var
    ADate: TDateTime;
  begin
    ADate := IncSecond(Now, -Random(315360000));
    Result := FormatDateTime('yyyymmdd hh:mm:ss', ADate);
  end;

const
  Trademark: array[0..23] of string = ('Mercedes-Benz', 'Mercedes-Benz', 'Mercedes-Benz', 'BMW',
    'Rolls-Royce', 'Jaguar', 'Cadillac', 'Cadillac', 'Lexus', 'Lexus', 'Ford', 'Dodge', 'GMC',
    'Nissan','Toyota', 'Infiniti', 'Infiniti', 'Jaguar', 'Audi', 'Audi', 'BMW', 'BMW', 'Acura',
    'Acura');
  Model: array[0..23] of string = ('SL500 Roadster', 'CLK55 AMG Cabriolet', 'C230 Kompressor Sport Coupe',
    '530i', 'Corniche', 'S-Type 3.0', 'Seville', 'DeVille', 'LS430', 'GS 430', 'Ranger FX-4', 'Ram 1500',
    'Siera Quadrasteer', 'Crew Cab SE', 'Tacoma S-Runner', 'Q45', 'G35 Sport Coupe Leather 6MT', 'XK8 Coupe',
    'A6 3.0', 'TT Roadster', '760i Sedan', 'Z4 3.0 Roadster', 'TSX', 'NSX 3.2');
  HP: array[0..23] of string = ('302', '342', '189', '225', '325', '235', '275', '275', '290', '300',
    '135', '215', '200', '143', '190', '340', '280', '294', '220', '180', '438', '225', '200', '290');
  Cyl: array[0..23] of string = ('8', '8', '4', '6', '8', '6', '8', '8', '8', '8', '4', '6', '6', '4',
    '6', '8', '6', '8', '6', '4', '12', '6', '4', '6');
  TransmissSpeedCount: array[0..23] of string = ('5', '5', '5', '6', '4', '5', '4', '4', '5', '5', '5',
    '4', '4', '4', '5', '5', '6', '6', '5', '6', '6', '6', '6', '6');
  TransmissAutomatic: array[0..23] of string = ('1', '1', '1', '0', '1', '0', '1', '1', '1', '1', '1',
    '1', '1', '1', '0', '1', 'NULL', '1', '1', '1', '1', 'NULL', '0', 'NULL');
  Category: array[0..23] of string = ('SPORTS', 'SPORTS', 'SPORTS', 'SALOON', 'SALOON', 'SALOON', 'SALOON',
    'SALOON', 'SALOON', 'SALOON', 'TRUCK', 'TRUCK', 'TRUCK', 'TRUCK', 'TRUCK', 'SALOON', 'SPORTS', 'SPORTS',
    'SALOON', 'SPORTS', 'SALOON', 'SPORTS', 'SPORTS', 'SPORTS');
  Price: array[0..23] of string = ('83800', '79645', '25600', '39450', '370485', '44320', '49600', '47780',
    '54900', '41242', '12565', '17315', '17748', '12800', '20000', '62300', '34000', '73000', '38000', '45000',
    '120000', '45000', '28500', '95000');

var
  I: Integer;
begin
  I := Random(24);
  Result := Format('INSERT INTO "%s" ("CustomerID", "OrderDate", "Trademark", "Model", "HP", "Cyl", "TransmissSpeedCount", ' +
    '"TransmissAutomatic", "Category", "Price") VALUES (''%d'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', %s, ''%s'', ''%s'')',
    [dxOrdersTableName, Random(MaxCustomerCount) + 1, GetDateTime, Trademark[I], Model[I], HP[I], Cyl[I],
      TransmissSpeedCount[I], TransmissAutomatic[I], Category[I], Price[I]]);
end;

function TServerModeDemoDataDM.GetRecordsCount: Integer;
begin
  Result := 0;
  if DoGetRecordsCount(dxCustomersTableName) > 0 then
    Result := DoGetRecordsCount(dxOrdersTableName);
end;

procedure TServerModeDemoDataDM.ServerModeQueryDataSourceFatalError(Sender: TdxServerModeCustomDataSource;
  const AMessage: string);
var
  S: string;
begin
  S := 'The following error occurred when obtaining data: "%s".' + sLineBreak +
    'Do you want to reload the data?';
  if MessageDlg(Format(S, [AMessage]), mtError, [mbYes, mbNo], 0) = mrYes then
    Sender.Open;
end;

procedure TServerModeDemoDataDM.ServerModeQueryDataSourceInconsistentCache(Sender: TdxServerModeCustomDataSource;
  const AMessage: string; var ARecoverCache: Boolean);
begin
  ARecoverCache := MessageDlg('The cache state is inconsistent. Do you want to recover it?', mtError, [mbYes, mbNo], 0) = mrYes;
  if not ARecoverCache then
    Sender.Close;
end;

end.
