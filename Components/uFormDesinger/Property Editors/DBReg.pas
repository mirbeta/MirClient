{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit DBReg;

interface

uses SysUtils, Classes, DesignIntf, DesignEditors,
{$IFDEF MSWINDOWS}DSDesign{$ENDIF}{$IFDEF LINUX}DSDesignLin{$ENDIF},
  WideStrings;

type
  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDBWideStringProperty = class(TWideStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TWideStrings); virtual;
    procedure GetValues(Proc: TGetWideStrProc); override;
  end;

  TDataFieldProperty = class(TDBWideStringProperty)
  public
    function GetDataSourcePropName: string; virtual;
    procedure GetValueList(List: TWideStrings); override;
  end;

  TDataFieldAggProperty = class(TDBWideStringProperty)
  public
    function GetDataSourcePropName: string; virtual;
    procedure GetValueList(List: TWideStrings); override;
  end;

  TDataSetEditor = class(TComponentEditor{$IFDEF LINUX}, IDesignerThreadAffinity{$ENDIF})
  protected
    function GetDSDesignerClass: TDSDesignerClass; virtual;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
{$IFDEF LINUX}
    procedure Edit; override;
    {IDesignerThreadAffinity}
    function GetThreadAffinity: TThreadAffinity;
{$ENDIF}
  end;

  TIndexFieldNamesProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  TIndexNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{ TListFieldProperty }

type
  TListFieldProperty = class(TDataFieldProperty)
  public
    function GetDataSourcePropName: string; override;
  end;

procedure Register;

implementation

uses
  Windows, Controls, Forms, Mask, TypInfo, DsnDBCst, DB, 
  ColnEdit, ActnList, DBActRes, {$IFDEF MSWINDOWS}DBColnEd;{$ENDIF}
  {$IFDEF LINUX}ClxDBColnEd, TConnect;{$ENDIF}

{ TDataSetEditor }

function TDataSetEditor.GetDSDesignerClass: TDSDesignerClass;
begin
  Result := TDSDesigner;
end;

procedure TDataSetEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    ShowFieldsEditor(Designer, TDataSet(Component), GetDSDesignerClass);
end;

function TDataSetEditor.GetVerb(Index: Integer): string;
begin
  Result := SDatasetDesigner;
end;

function TDataSetEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF LINUX}
function TDataSetEditor.GetThreadAffinity: TThreadAffinity;
begin
  Result  := taQT;
end;

procedure TDataSetEditor.Edit; 
begin
  ShowFieldsEditor(Designer, TDataSet(Component), GetDSDesignerClass);
end;

{$ENDIF}

{ TDataSetProperty }

type
  TDataSetProperty = class(TComponentProperty)
  private
    FCheckProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure TDataSetProperty.CheckComponent(const Value: string);
var
  J: Integer;
  Dataset: TDataset;
begin
  Dataset := TDataset(Designer.GetComponent(Value));
  for J := 0 to PropCount - 1 do
    if TDataSource(GetComponent(J)).IsLinkedTo(Dataset) then
      Exit;
  FCheckProc(Value);
end;

procedure TDataSetProperty.GetValues(Proc: TGetStrProc);
begin
  FCheckProc := Proc;
  inherited GetValues(CheckComponent);
end;

{ TDataSourceProperty }

type
  TDataSourceProperty = class(TComponentProperty)
  private
    FCheckProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure TDataSourceProperty.CheckComponent(const Value: string);
var
  J: Integer;
  DataSource: TDataSource;
begin
  DataSource := TDataSource(Designer.GetComponent(Value));
  for J := 0 to PropCount - 1 do
    if TDataSet(GetComponent(J)).IsLinkedTo(DataSource) then
      Exit;
  FCheckProc(Value);
end;

procedure TDataSourceProperty.GetValues(Proc: TGetStrProc);
begin
  FCheckProc := Proc;
  inherited GetValues(CheckComponent);
end;

{ TNestedDataSetProperty }

type
  TNestedDataSetProperty = class(TComponentProperty)
  private
    FCheckProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure TNestedDataSetProperty.CheckComponent(const Value: string);
var
  DataSet: TDataset;
begin
  DataSet := (GetComponent(0) as TDataSetField).DataSet;
  if TDataset(Designer.GetComponent(Value)) <> DataSet then
    FCheckProc(Value);
end;

procedure TNestedDataSetProperty.GetValues(Proc: TGetStrProc);
begin
  FCheckProc := Proc;
  inherited GetValues(CheckComponent);
end;

{ TDBStringProperty }

function TDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBStringProperty.GetValueList(List: TStrings);
begin
end;

procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

function GetIndexDefs(Component: TPersistent): TIndexDefs;
var
  DataSet: TDataSet;
begin
  DataSet := Component as TDataSet;
  Result := GetObjectProp(DataSet, 'IndexDefs') as TIndexDefs;
  if Assigned(Result) then
  begin
    Result.Updated := False;
    Result.Update;
  end;
end;

{ TDBWideStringProperty }

function TDBWideStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBWideStringProperty.GetValueList(List: TWideStrings);
begin
end;

procedure TDBWideStringProperty.GetValues(Proc: TGetWideStrProc);
var
  I: Integer;
  Values: TWideStringList;
begin
  Values := TWideStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{ TIndexNameProperty }

procedure TIndexNameProperty.GetValueList(List: TStrings);
begin
  GetIndexDefs(GetComponent(0)).GetItemNames(List);
end;

{ TIndexFieldNamesProperty }

procedure TIndexFieldNamesProperty.GetValueList(List: TStrings);
var
  I: Integer;
  IndexDefs: TIndexDefs;
begin
  IndexDefs := GetIndexDefs(GetComponent(0));
  for I := 0 to IndexDefs.Count - 1 do
    with IndexDefs[I] do
      if (Options * [ixExpression, ixDescending] = []) and (Fields <> '') then
        List.Add(Fields);
end;

{ TDataFieldProperty }

function TDataFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'DataSource';
end;

procedure TDataFieldProperty.GetValueList(List: TWideStrings);
var
  DataSource: TDataSource;
begin
  DataSource := GetObjectProp(GetComponent(0), GetDataSourcePropName) as TDataSource;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.GetFieldNames(List);
end;

{ TDataFieldAggProperty }

function TDataFieldAggProperty.GetDataSourcePropName: string;
begin
  Result := 'DataSource';
end;

procedure TDataFieldAggProperty.GetValueList(List: TWideStrings);
var
  DataSource: TDataSource;
  AggList: TWideStringList;
begin
  DataSource := GetObjectProp(GetComponent(0), GetDataSourcePropName) as TDataSource;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
  begin
    DataSource.DataSet.GetFieldNames(List);
    if DataSource.DataSet.AggFields.Count > 0 then
    begin
      AggList := TWideStringList.Create;
      try
        DataSource.DataSet.AggFields.GetFieldNames(AggList);
        List.AddStrings(AggList);
      finally
        AggList.Free;
      end;
    end;
  end;
end;

{ TLookupSourceProperty }

type
  TLookupSourceProperty = class(TDBWideStringProperty)
  public
    procedure GetValueList(List: TWideStrings); override;
  end;

procedure TLookupSourceProperty.GetValueList(List: TWideStrings);
begin
  with GetComponent(0) as TField do
    if DataSet <> nil then DataSet.GetFieldNames(List);
end;

{ TLookupDestProperty }

type
  TLookupDestProperty = class(TDBWideStringProperty)
  public
    procedure GetValueList(List: TWideStrings); override;
  end;

procedure TLookupDestProperty.GetValueList(List: TWideStrings);
begin
  with GetComponent(0) as TField do
    if LookupDataSet <> nil then LookupDataSet.GetFieldNames(List);
end;

function TListFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'ListSource';
end;

{ TLookupFieldProperty }

type
  TLookupFieldProperty = class(TDataFieldProperty)
  public
    function GetDataSourcePropName: string; override;
  end;

function TLookupFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'LookupSource';
end;

{ TLookupIndexProperty }

type
  TLookupIndexProperty = class(TLookupFieldProperty)
  public
    procedure GetValueList(List: TWideStrings); override;
  end;

procedure TLookupIndexProperty.GetValueList(List: TWideStrings);
var
  DataSource: TDataSource;
begin
  DataSource := GetObjectProp(GetComponent(0), GetDataSourcePropName) as TDataSource;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.GetFieldNames(List);
end;

{ Registration }

procedure Register;
begin
  { Database Components are excluded from the STD SKU }
  if GDAL <> LongWord(-16) then
  begin
    RegisterComponents(srDAccess, [TDataSource]);

{$IFDEF LINUX}
    RegisterComponents(srDAccess, [TLocalConnection]);
{$ENDIF}

    RegisterNoIcon([TField]);

    RegisterFields([TStringField, TIntegerField, TSmallintField, TWordField,
      TFloatField, TCurrencyField, TBCDField, TFMTBcdField, TBooleanField, TDateField,
      TVarBytesField, TBytesField, TTimeField, TDateTimeField, TSQLTimeStampField,
      TBlobField, TMemoField, TGraphicField, TAutoIncField, TLargeIntField,
      TADTField, TArrayField, TDataSetField, TReferenceField, TAggregateField,
      TWideStringField, TVariantField, TGuidField, TInterfaceField, TIDispatchField,
      TWideMemoField, TLongWordField, TShortintField, TByteField, TExtendedField,
      TSQLTimeStampOffsetField, TSingleField]);

    RegisterPropertyEditor(TypeInfo(TDataSet), TDataSource, 'DataSet', TDataSetProperty);
    RegisterPropertyEditor(TypeInfo(TDataSet), TDataSetField, 'NestedDataSet', TNestedDataSetProperty);
    RegisterPropertyEditor(TypeInfo(TDataSource), TDataSet, 'MasterSource', TDataSourceProperty);
    RegisterPropertyEditor(TypeInfo(TDataSource), TDataSet, 'DataSource', TDataSourceProperty);
    RegisterPropertyEditor(TypeInfo(WideString), TField, 'KeyFields', TLookupSourceProperty);
    RegisterPropertyEditor(TypeInfo(WideString), TField, 'LookupKeyFields', TLookupDestProperty);
    RegisterPropertyEditor(TypeInfo(WideString), TField, 'LookupResultField', TLookupDestProperty);
    RegisterPropertyEditor(TypeInfo(string),     TComponent, 'DataField', TDataFieldProperty);
    RegisterPropertyEditor(TypeInfo(WideString), TComponent, 'DataField', TDataFieldProperty);
    RegisterPropertyEditor(TypeInfo(string),     TWinControl, 'LookupField', TLookupIndexProperty);
    RegisterPropertyEditor(TypeInfo(WideString), TWinControl, 'LookupField', TLookupIndexProperty);
    RegisterPropertyEditor(TypeInfo(string),     TWinControl, 'LookupDisplay', TLookupFieldProperty);
    RegisterPropertyEditor(TypeInfo(WideString), TWinControl, 'LookupDisplay', TLookupFieldProperty);

    RegisterComponentEditor(TDataset, TDataSetEditor);


    { Property Category registration }
    RegisterPropertiesInCategory(sDatabaseCategoryName, TDataSet,
      ['*Field', '*Fields', 'Index*', 'Lookup*', '*Defs', 'ObjectView', 'Table*',
       'Param*', 'Cache*', 'Lock*', 'Cursor*']);
    RegisterPropertiesInCategory(sDatabaseCategoryName, TField,
      ['*Field', '*Fields']);
    RegisterPropertyInCategory(sDatabaseCategoryName, TComponent, 'DataField');

  end;
end;

end.
