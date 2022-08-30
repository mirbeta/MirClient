unit UnboundListDemoClasses;

{$I cxVer.inc}

interface

uses
  Variants, Classes, cxCustomData, cxGridCustomTableView;

const
  IndexOfID = 0;
  IndexOfName = 1;
  IndexOfDescription = 2;

type
  TCustomer = class
  private
    FID: Integer;
    FName: string;
    FDescription: string;
    procedure SetID(Value: Integer);
  protected
    function GetDescription: string;
    procedure SetDescription(Value: string);
    function GetName: string;
    procedure SetName(Value: string);
  public
    constructor Create(AID: Integer);
    destructor Destroy; override;
    property Description: string read GetDescription write SetDescription;
    property ID: Integer read FID write SetID;
    property Name: string read GetName write SetName;
  end;

  TCustomerList = class
  private
    FList: TList;
    FNextID: Integer;
    procedure ReleaseAllCustomers;
    procedure ReleaseCustomer(AIndex: Integer);
    function GetCustomer(AIndex: Integer): TCustomer;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(Customer: TCustomer): Integer;
    procedure Delete(AIndex: Integer);
    procedure Insert(AIndex: integer; Customer: TCustomer);
    property Customers[Index: Integer]: TCustomer read GetCustomer; default;
    property Count: Integer read GetCount;
    property NextID: Integer read FNextID;
  end;

  TCustomerDataSource = class(TcxCustomDataSource)
  private
    FCustomers: TCustomerList;
    FModified: boolean;
  protected
    function AppendRecord: TcxDataRecordHandle; override;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;
    function InsertRecord(ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
  public
    constructor Create(ACustomerList: TCustomerList);
    property Modified: boolean read FModified;
  end;


implementation

{ TCustomer }

constructor TCustomer.Create(AID: Integer);
begin
  inherited Create;
  FID := AID;
  FName := '';
end;

destructor TCustomer.Destroy;
begin
  inherited Destroy;
end;

function TCustomer.GetDescription: string;
begin
  Result := FDescription;
end;

function TCustomer.GetName: string;
begin
  Result := FName;
end;

procedure TCustomer.SetDescription(Value: string);
begin
  if FDescription <> Value then
    FDescription := Value;
end;

procedure TCustomer.SetID(Value: Integer);
begin
  if FID <> Value then
    FID := Value;
end;

procedure TCustomer.SetName(Value: string);
begin
  if FName <> Value then
    FName := Value;
end;

{ TCustomerList }

function TCustomerList.Add(Customer: TCustomer): Integer;
begin
  Result := FList.Add(Customer);
  Inc(FNextID);
end;

procedure TCustomerList.Clear;
begin
  ReleaseAllCustomers;
end;

constructor TCustomerList.Create;
begin
  inherited Create;
  FList := TList.Create;
  FNextID := 1;
end;

procedure TCustomerList.Delete(AIndex: Integer);
begin
  ReleaseCustomer(AIndex);
  FList.Delete(AIndex);
end;

destructor TCustomerList.Destroy;
begin
  ReleaseAllCustomers;
  FList.Free;
  inherited Destroy;
end;

function TCustomerList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCustomerList.GetCustomer(AIndex: Integer): TCustomer;
begin
  Result := TCustomer(FList[AIndex]);
end;

procedure TCustomerList.Insert(AIndex: integer; Customer: TCustomer);
begin
  FList.Insert(AIndex, Customer);
  Inc(FNextID);
end;

procedure TCustomerList.ReleaseAllCustomers;
var
  I : Integer;
begin
  for I := 0 to Count -1 do
    ReleaseCustomer(I);
  FList.Clear;
end;

procedure TCustomerList.ReleaseCustomer(AIndex: Integer);
begin
  TCustomer(FList[AIndex]).Free;
end;

{ TCustomDataSource }

function TCustomerDataSource.AppendRecord: TcxDataRecordHandle;
var
  ACustomer: TCustomer;
begin
  ACustomer := TCustomer.Create(FCustomers.NextID);
  Result := TcxDataRecordHandle(FCustomers.Add(ACustomer));
  DataChanged;
  if not Modified then
    FModified := True;
end;

constructor TCustomerDataSource.Create(ACustomerList: TCustomerList);
begin
  inherited Create;
  FCustomers := ACustomerList;
end;

procedure TCustomerDataSource.DeleteRecord(
  ARecordHandle: TcxDataRecordHandle);
begin
  FCustomers.Delete(Integer(ARecordHandle));
  DataChanged;
  if not Modified then
    FModified := True;
end;

function TCustomerDataSource.GetRecordCount: Integer;
begin
  Result := FCustomers.Count;
end;

function TCustomerDataSource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
var
  AColumnId: Integer;
  ACustomer: TCustomer;
begin
  ACustomer := FCustomers[Integer(ARecordHandle)];
  AColumnId := GetDefaultItemID(Integer(AItemHandle));
  case AColumnId of
    IndexOfID:
      Result := ACustomer.ID;
    IndexOfName:
      Result := ACustomer.Name;
    IndexOfDescription:
      Result := ACustomer.Description;
  end;
end;

function TCustomerDataSource.InsertRecord(
  ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle;
var
  ACustomer: TCustomer;
begin
  ACustomer := TCustomer.Create(FCustomers.NextID);
  FCustomers.Insert(Integer(ARecordHandle), ACustomer);
  Result := TcxDataRecordHandle(ARecordHandle);
  DataChanged;
  if not Modified then
    FModified := True;
end;

procedure TCustomerDataSource.SetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle; const AValue: Variant);
var
 ACustomer: TCustomer;
 AColumnId: Integer;
begin
  AColumnId := GetDefaultItemID(Integer(AItemHandle));
  ACustomer := FCustomers[Integer(ARecordHandle)];
  case AColumnId of
    IndexOfID:
      if VarIsNull(AValue) then
        ACustomer.ID := 0
      else
        ACustomer.ID := AValue;
    IndexOfName:
      ACustomer.Name := VarToStr(AValue) ;
    IndexOfDescription:
      ACustomer.Description := VarToStr(AValue);
  end;
  if not Modified then
    FModified := True;
end;

end.



