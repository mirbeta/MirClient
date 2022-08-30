{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

{*******************************************************}
{        Dataset Designer Define Field Dialog           }
{*******************************************************}

unit RDDSDefine;

interface

{$IFDEF MSWINDOWS}
uses Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, Buttons, DB, DesignIntf,
  WideStrings;
{$ENDIF}

{$IFDEF LINUX}
uses Windows, SysUtils, Messages, Classes, QGraphics, QControls, QForms,
  QStdCtrls, QExtCtrls, QButtons, DB, DesignIntf;
{$ENDIF}

type

  TDefineField = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    FieldGroup: TGroupBox;
    ComponentNameLabel: TLabel;
    FieldNameLabel: TLabel;
    ComponentNameEdit: TEdit;
    FieldNameEdit: TEdit;
    FieldTypeList: TComboBox;
    SizeEditLabel: TLabel;
    SizeEdit: TEdit;
    FieldKind: TRadioGroup;
    LookupGroup: TGroupBox;
    DatasetList: TComboBox;
    DatasetLabel: TLabel;
    KeyFieldsList: TComboBox;
    LookupKeysList: TComboBox;
    ResultFieldList: TComboBox;
    KeyFieldsLabel: TLabel;
    LookupKeysLabel: TLabel;
    ResultFieldLabel: TLabel;
    FieldTypeLabel: TLabel;
    procedure FieldNameEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure DatasetListDropDown(Sender: TObject);
    procedure LookupKeysListDropDown(Sender: TObject);
    procedure KeyFieldsListDropDown(Sender: TObject);
    procedure ResultFieldListDropDown(Sender: TObject);
    procedure FieldKindClick(Sender: TObject);
    procedure DatasetListChange(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FieldTypeListChange(Sender: TObject);
  private
    FDataset: TDataset;
    FDesigner: IDesigner;
    FDSDesigner: TDatasetDesigner;
    FField: TField;
    function GetCalculated: Boolean;
    function GetComponentName: string;
    function GetFieldClass: TFieldClass;
    function GetFieldName: string;
    function GetLookup: Boolean;
    function GetLookupDataset: TDataset;
    function GetKeyFields: string;
    function GetLookupKeyFields: string;
    function GetLookupResultField: string;
    procedure GetLookupFields(Items: TWideStrings); overload;
    procedure GetLookupFields(Items: TStrings); overload;
    function GetSize: Integer;
    procedure SetCalculated(Value: Boolean);
    procedure SetComponentName(const Value: string);
    procedure SetDataset(Value: TDataset);
    procedure SetFieldClass(Value: TFieldClass);
    procedure SetFieldName(const Value: string);
    procedure SetLookup(Value: Boolean);
    procedure SetSize(Value: Integer);
    procedure UpdateLookupControls;
  public
    procedure ConfigureForLookupOnly(const ADataSet, AKey, ALookup,
      AResult, AType: string; ASize: Word);
    property Calculated: Boolean read GetCalculated write SetCalculated;
    property Lookup: Boolean read GetLookup write SetLookup;
    property ComponentName: string read GetComponentName
      write SetComponentName;
    property FieldClass: TFieldClass read GetFieldClass write SetFieldClass;
    property FieldName: string read GetFieldName write SetFieldName;
    property Field: TField read FField;
    property Size: Integer read GetSize write SetSize;
    property LookupDataset: TDataset read GetLookupDataset;
    property KeyFields: string read GetKeyFields;
    property LookupKeyFields: string read GetLookupKeyFields;
    property LookupResultField: string read GetLookupResultField;
    property Dataset: TDataset read FDataset write SetDataset;
    property Designer: IDesigner read FDesigner write FDesigner;
    property DSDesigner: TDatasetDesigner read FDSDesigner write FDSDesigner;
  end;

function ClassNameNoT(FieldClass: TFieldClass): string;

var
  DefineField: TDefineField;

implementation

uses Menus, DBConsts, Dialogs, RDDSDesign, LibHelp, TypInfo;


{$IFDEF MSWINDOWS}
{$R *.dfm}
{$ENDIF}

var
  FieldClasses: TList;

function ClassNameNoT(FieldClass: TFieldClass): string;
begin
  Result := FieldClass.ClassName;
  if Result[1] = 'T' then Delete(Result, 1, 1);
  if CompareText('Field', Copy(Result, Length(Result) - 4, 5)) = 0 then { do not localize }
    Delete(Result, Length(Result) - 4, 5);
end;

procedure RegFields(const AFieldClasses: array of TFieldClass); far;
var
  I: Integer;
begin
  if FieldClasses = nil then FieldClasses := TList.Create;
  for I := Low(AFieldClasses) to High(AFieldClasses) do
    if FieldClasses.IndexOf(AFieldClasses[I]) = -1 then
    begin
      FieldClasses.Add(AFieldClasses[I]);
      RegisterClass(AFieldClasses[I]);
    end;
end;

function FindFieldClass(const FieldClassName: string): TFieldClass;
var
  I: Integer;
begin
  for I := 0 to FieldClasses.Count - 1 do
  begin
    Result := FieldClasses[I];
    if (CompareText(FieldClassName, Result.ClassName) = 0)
      or (CompareText(FieldClassName, ClassNameNoT(Result)) = 0) then
      Exit;
  end;
  Result := nil;
end;

{ TNewField }

procedure TDefineField.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FieldClasses.Count - 1 do
    FieldTypeList.Items.Add(ClassNameNoT(FieldClasses[I]));
  HelpContext := hcDDefineField;
end;

function TDefineField.GetCalculated: Boolean;
begin
  Result := FieldKind.ItemIndex = 1;
end;

function TDefineField.GetComponentName: string;
begin
  Result := ComponentNameEdit.Text;
end;

function TDefineField.GetFieldClass: TFieldClass;
begin
  Result := FindFieldClass(FieldTypeList.Text);
end;

function TDefineField.GetFieldName: string;
begin
  Result := FieldNameEdit.Text;
end;

function TDefineField.GetLookup: Boolean;
begin
  Result := FieldKind.ItemIndex = 2;
end;

function TDefineField.GetLookupDataset: TDataset;
begin
  Result := Designer.GetComponent(DatasetList.Text) as TDataset;
end;

function TDefineField.GetKeyFields: string;
begin
  Result := KeyFieldsList.Text;
end;

function TDefineField.GetLookupKeyFields: string;
begin
  Result := LookupKeysList.Text;
end;

function TDefineField.GetLookupResultField: string;
begin
  Result := ResultFieldList.Text;
end;

function TDefineField.GetSize: Integer;
begin
  Result := -1;
  if SizeEdit.Text <> '' then Result := StrToInt(SizeEdit.Text);
end;

procedure TDefineField.SetCalculated(Value: Boolean);
begin
  if Value or not Lookup then
    FieldKind.ItemIndex := Ord(Value);
end;

procedure TDefineField.SetComponentName(const Value: string);
begin
  ComponentNameEdit.Text := Value;
end;

procedure TDefineField.SetDataset(Value: TDataset);
begin
  FDataset := Value;
  FieldNameEdit.Text := '';
end;

procedure TDefineField.SetFieldClass(Value: TFieldClass);
begin
  if Value <> nil then
    with FieldTypeList do
      ItemIndex := Items.IndexOf(ClassNameNoT(Value));
end;

procedure TDefineField.SetFieldName(const Value: string);
begin
  FieldNameEdit.Text := Value;
end;

procedure TDefineField.SetLookup(Value: Boolean);
begin
  if Value or not Calculated then
    FieldKind.ItemIndex := Ord(Value) * 2;
end;

procedure TDefineField.SetSize(Value: Integer);
begin
  SizeEdit.Text := IntToStr(Value);
end;

procedure TDefineField.FieldNameEditChange(Sender: TObject);
var
  I: Integer;
begin
  if FieldName <> '' then
    ComponentName := CreateUniqueName(Dataset, FieldName, FieldClass, nil) else
    ComponentName := '';
  I := Dataset.FieldDefs.IndexOf(FieldName);
  if I >= 0 then FieldClass := Dataset.FieldDefs[I].FieldClass;
  if (Dataset.FieldDefs.Count <> 0) and (FieldKind.ItemIndex = 0) then
    Calculated := I < 0;
end;

procedure TDefineField.OkBtnClick(Sender: TObject);
var
  ErrorFound: Boolean;

  procedure ErrorMsg(const Msg: string; L: TLabel);
  begin
    MessageDlg(Msg, mtError, [mbOK], 0);
    if L.FocusControl <> nil then L.FocusControl.SetFocus;
    ErrorFound := True;
  end;

  procedure Error(L: TLabel);
  var
    C: string;
    I: Integer;
  begin
    C := StripHotKey(L.Caption);
    I := Length(C);
    if IsDelimiter(':', C, I) then Delete(C, I, 1);
    ErrorMsg(Format(SDSMustBeSpecified, [C]), L);
  end;

begin
  ModalResult := mrNone;
  ErrorFound := False;
  if Length(FieldName) > 31 then
    raise Exception.Create(Format(SFieldNameTooLarge, [FieldName, 31]));
  if FieldName = '' then Error(FieldNameLabel)
  else if FieldClass = nil then Error(FieldTypeLabel)
  else if ComponentName = '' then Error(ComponentNameLabel)
  else if Lookup then
    if LookupDataset = nil then Error(DatasetLabel)
    else if LookupDataset = Dataset then
      ErrorMsg(SCircularDataLink, DatasetLabel)
    else if LookupKeyFields = '' then Error(LookupKeysLabel)
    else if KeyFields = '' then Error(KeyFieldsLabel)
    else if LookupResultField = '' then Error(ResultFieldLabel);
  if ErrorFound then Exit;
  FField := FieldClass.Create(Dataset.Owner);
  try
    Field.Name := ComponentName;
    Field.FieldName := FieldName;
    if Calculated then
      Field.FieldKind := fkCalculated
    else if Lookup then
    begin
      Field.FieldKind := fkLookup;
      Field.LookupDataset := LookupDataset;
      Field.KeyFields := KeyFields;
      Field.LookupKeyFields := LookupKeyFields;
      Field.LookupResultField := LookupResultField;
    end
    else if FieldKind.ItemIndex = 3 then
      Field.FieldKind := fkInternalCalc
    else if FieldKind.ItemIndex = 4 then
    begin
      Field.FieldKind := fkAggregate;
      Field.Visible := False;
    end;
    if (Field.FieldKind = fkData) and (DataSet.Active) then
    begin
      ErrorMsg(SDSDataFieldOnOpenTable, FieldTypeLabel);
      Field.Free;
      Exit;
    end;
    if Size <> -1 then Field.Size := Size;
    DSDesigner.BeginDesign;
    try
      Field.Dataset := Dataset;
    finally
      DSDesigner.EndDesign;
    end;
  except
    Field.Free;
    raise;
  end;
  ModalResult := mrOK;
end;

procedure TDefineField.UpdateLookupControls;
var
  LookupDatasetValid: Boolean;
begin
  LookupDatasetValid := Lookup and (Designer.GetComponent(DatasetList.Text) <> nil);
  DatasetList.Enabled := Lookup;
  DatasetLabel.Enabled := Lookup;
  KeyFieldsList.Enabled := Lookup;
  KeyFieldsLabel.Enabled := Lookup;
  LookupKeysList.Enabled := LookupDatasetValid;
  LookupKeysLabel.Enabled := LookupDatasetValid;
  ResultFieldList.Enabled := LookupDatasetValid;
  ResultFieldLabel.Enabled := LookupDatasetValid;
end;

procedure TDefineField.DatasetListDropDown(Sender: TObject);
var
  OldValue: string;
begin
  OldValue := DatasetList.Text;
  DatasetList.Clear;
  Designer.GetComponentNames(GetTypeData(TDataset.ClassInfo),
    DatasetList.Items.Append);
  DatasetList.Text := OldValue;
end;

procedure TDefineField.KeyFieldsListDropDown(Sender: TObject);
var
  OldValue: string;
begin
  OldValue := KeyFieldsList.Text;
  KeyFieldsList.Clear;
  Dataset.GetFieldNames(KeyFieldsList.Items);
  KeyFieldsList.Text := OldValue;
end;

procedure TDefineField.GetLookupFields(Items: TWideStrings);
var
  LookupDataset: TDataset;
begin
  LookupDataset := Designer.GetComponent(DatasetList.Text) as TDataset;
  if LookupDataset <> nil then LookupDataset.GetFieldNames(Items);
end;

procedure TDefineField.GetLookupFields(Items: TStrings);
var
  wItem: TWIdeStringList;
begin
  wItem := TWIdeStringList.Create;
  try
    GetLookupFields(wItem);
    Items.Assign(wItem);
  except
    wItem.Free;
  end;
end;

procedure TDefineField.LookupKeysListDropDown(Sender: TObject);
var
  OldValue: string;
begin
  OldValue := LookupKeysList.Text;
  LookupKeysList.Clear;
  GetLookupFields(LookupKeysList.Items);
  LookupKeysList.Text := OldValue;
end;

procedure TDefineField.ResultFieldListDropDown(Sender: TObject);
var
  OldValue: string;
begin
  OldValue := ResultFieldList.Text;
  ResultFieldList.Clear;
  GetLookupFields(ResultFieldList.Items);
  ResultFieldList.Text := OldValue;
end;

procedure TDefineField.FieldKindClick(Sender: TObject);
begin
  if FieldKind.ItemIndex = 4 then
    FieldTypeList.Text := 'Aggregate'; { do not localize }
  UpdateLookupControls;
end;

procedure TDefineField.DatasetListChange(Sender: TObject);
begin
  UpdateLookupControls;
end;

procedure TDefineField.HelpBtnClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
   Application.HelpContext(HelpContext);
{$ENDIF}
end;

type
  TFieldAccess = class(TField);
  TFieldAccessClass = class of TFieldAccess;

procedure TDefineField.FieldTypeListChange(Sender: TObject);
var
  FieldClass: TFieldClass;
begin
  if (FieldTypeList.Text <> '') then
  try
    FieldClass := Self.FieldClass;
    if Assigned(FieldClass) then
      TFieldAccessClass(FieldClass).CheckTypeSize(1);
    SizeEdit.Enabled := True;
  except
    SizeEdit.Text := '0'; { do not localize }
    SizeEdit.Enabled := False;
  end;
end;

procedure TDefineField.ConfigureForLookupOnly(const ADataSet, AKey, ALookup,
  AResult, AType: string; ASize: Word);
var
  vDelta: Integer;
begin
  Lookup := True;
  FieldKind.Hide;
  vDelta := LookupGroup.Top - FieldKind.Top;
  LookupGroup.Top := FieldKind.Top;
  OkBtn.Top := OkBtn.Top - vDelta;
  CancelBtn.Top := CancelBtn.Top - vDelta;
  HelpBtn.Top := HelpBtn.Top - vDelta;
  Height := Height - vDelta;
  Caption := SNewLookupFieldCaption;
  DataSetList.Text := ADataSet;
  KeyFieldsList.Text := AKey;
  LookupKeysList.Text := ALookup;
  ResultFieldList.Text := AResult;
  SizeEdit.Text := IntToStr(ASize);
  FieldTypeList.Text := AType;
  UpdateLookupControls;
end;

initialization
  RegisterFieldsProc := RegFields;

end.
