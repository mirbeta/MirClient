unit UnboundExternalMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DemoBasicMain, cxLookAndFeels, Menus, StdCtrls, cxControls,
  cxCustomPivotGrid, cxPivotGrid, cxCustomData, cxClasses, cxGraphics,
  cxStyles, cxLookAndFeelPainters, cxEdit;

type
  TcxExternalDataSource = class;
  TfrmUnboundExternal = class(TfrmDemoBasicMain)
    UnboundPivotGrid: TcxPivotGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected
    ExternalData: TcxExternalDataSource;
    function GetPivotGrid: TcxCustomPivotGrid; override;
    procedure SetFieldPos(const AFieldName: string; AArea: TcxPivotGridFieldArea);
  public
    { Public declarations }
  end;

  TcxExternalDataSource = class(TcxCustomDataSource)
  private
    FFieldCount: Integer;
    function GetFieldName(AIndex: Integer): string;
    function GetFieldType(AIndex: Integer): string;
    function GetItemFromStr(const AString: string): string;
  protected
    Records: TStringList;
    procedure AfterLoad;
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant; override;
    function GetValueFromString(ARecord: string; AIndex: Integer): Variant;
    function GetVarTypeByName(const AName: string): Integer; 
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    property FieldCount: Integer read FFieldCount;
    property FieldNames[AIndex: Integer]: string read GetFieldName;
    property FieldTypes[AIndex: Integer]: string read GetFieldType;
    property RecordCount: Integer read GetRecordCount;
  end;

var
  frmUnboundExternal: TfrmUnboundExternal;

implementation

{$R *.dfm}

uses
  dxCore;

function TfrmUnboundExternal.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := UnboundPivotGrid;
end;

procedure TfrmUnboundExternal.SetFieldPos(
  const AFieldName: string; AArea: TcxPivotGridFieldArea);
var
  AField: TcxPivotGridField; 
begin
  AField := PivotGrid.GetFieldByName(AFieldName);
  AField.Area := AArea;
end;

procedure TfrmUnboundExternal.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  inherited;
  ExternalData := TcxExternalDataSource.Create('ExternalData.dat');
  PivotGrid.BeginUpdate;
  try
    for I := 0 to ExternalData.FieldCount - 1 do
    begin
      with PivotGrid.CreateField do
      begin
        Caption := ExternalData.FieldNames[I];
        DataBinding.ValueType := ExternalData.FieldTypes[I];
        Visible := True;
      end;
    end;
    SetFieldPos('PaymentType', faColumn);
    SetFieldPos('Payment Amount', faData);
    SetFieldPos('Quantity', faData);
    SetFieldPos('Company Name', faRow);
    SetFieldPos('Car Name', faRow);
    PivotGrid.DataController.CustomDataSource := ExternalData;
  finally
    PivotGrid.EndUpdate;
    PivotGrid.ApplyBestFit;
  end;
end;

procedure TfrmUnboundExternal.FormDestroy(Sender: TObject);
begin
  ExternalData.Free; 
end;

{  TcxExternalDataSource }

constructor TcxExternalDataSource.Create(const AFileName: string);
begin
  Records := TStringList.Create;
  Records.LoadFromFile(AFileName);
  AfterLoad;
end;

destructor TcxExternalDataSource.Destroy;
begin
  Records.Free;
  inherited Destroy;
end;

procedure TcxExternalDataSource.AfterLoad;
var
  S, AItem: string;
begin
  S := Records[0];
  FFieldCount := 0;
  repeat
    Inc(FFieldCount);
    AItem := GetItemFromStr(S);
    S := Copy(S, Length(AItem) + 2, MaxInt);
  until (Length(AItem) = 0) or (Length(S) = 0);
end;

function TcxExternalDataSource.GetRecordCount: Integer;
begin
  Result := Records.Count - 2;
end;

function TcxExternalDataSource.GetValue(
  ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant;
begin
  Result := GetValueFromString(Records[Integer(ARecordHandle) + 2], Integer(AItemHandle));
  VarCast(Result, Result, GetVarTypeByName(FieldTypes[Integer(AItemHandle)]));
end;

function TcxExternalDataSource.GetValueFromString(
  ARecord: string; AIndex: Integer): Variant;
var
  S: string;
  I: Integer;
begin
  S := '';
  for I := 0 to AIndex do
  begin
    S := GetItemFromStr(ARecord);
    ARecord := Copy(ARecord, Length(S) + 2, MaxInt);
  end;
  Result := S;
end;

function TcxExternalDataSource.GetVarTypeByName(const AName: string): Integer;
begin
  if SameText(AName, 'Currency') then
    Result := varCurrency
  else
    if SameText(AName, 'DateTime') then
      Result := varDate
    else
      if SameText(AName, 'Integer') then
        Result := varInteger
      else
        Result := varString;
end;

function TcxExternalDataSource.GetFieldName(AIndex: Integer): string;
begin
  Result := GetValueFromString(Records[0], AIndex);
end;

function TcxExternalDataSource.GetFieldType(AIndex: Integer): string;
begin
  Result := GetValueFromString(Records[1], AIndex);
end;

function TcxExternalDataSource.GetItemFromStr(const AString: string): string;
var
  I, ACount: Integer;
begin
  ACount := 0;
  for I := 1 to Length(AString) do
  begin
    if dxCharInSet(AString[I], [#9, #10, #13]) then
      Break
    else
      Inc(ACount);
  end;
  Result := Copy(AString, 1, ACount);
end;

end.
