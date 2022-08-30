{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressVerticalGrid                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSVERTICALGRID AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}
unit cxVGridDXInspConverter;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, DB, Graphics,
  cxClasses, cxConverterFactory, cxVGridConverter, cxVGrid, cxDBVGrid,
  cxConverterUtils, cxLookAndFeels, cxInplaceContainer, cxStyles,
  cxCustomData, cxCustomConverter, cxBlobEdit, cxPropertiesConverters,
  cxButtonEdit, cxCalc, cxCheckBox, cxDropDownEdit, cxCurrencyEdit, cxCalendar,
  cxDBLookupComboBox, cxHyperLinkEdit, cxImage, cxImageComboBox, cxMaskEdit, cxMemo,
  cxMRUEdit, cxSpinEdit, cxTextEdit, cxTimeEdit, cxVariants, cxOI;

type
  TcxCustomVerticalGridAccess = class(TcxCustomVerticalGrid);
  TcxUnboundVerticalGridAccess = class(TcxUnboundVerticalGrid);

  { TcxCustomDXInspConverter }

  TcxCustomDXInspConverter = class(TcxCustomVerticalGridConverter)
  private
    FcxRowCache: TStringList;
    FdxRowCache: TStringList;
    FDefaultRowHeight: Integer;
    FSeparatorString: string;
    function GetAccess: TcxCustomVerticalGridAccess;
    function GetComponent: TComponent;
  protected
    function AddRow(AcxRowParent: TcxCustomRow; const AdxRowName: string): TcxCustomRow;
    procedure AssignCaptionProperties(AdxRow: TObject; AProperties: TcxCaptionRowProperties);
    procedure AssignEditorDataBinding(AdxRow: TObject; AProperties: TcxCustomEditorRowProperties); virtual;
    procedure AssignEditorProperties(AdxRow: TObject; AProperties: TcxCustomEditorRowProperties);
    procedure AssignVerticalGrid; virtual;
    procedure AssignVerticalGridOptions; virtual;
    procedure ConvertPaintStyle;
    function CreateMultiEditorRowItem(
      AcxRow: TcxCustomRow): TcxCollectionItemEditorRowProperties; virtual;
    procedure DoRealImport; override;
    function GetConverterIndex(AdxRow: TObject): Integer; virtual;
    function GetRowClassType(AdxRow: TObject): TcxCustomRowClass; virtual;
    procedure ImportCategoryRow(AdxRow: TObject; AcxRow: TcxCustomRow);
    procedure ImportEditorRow(AdxRow: TObject; AcxRow: TcxCustomRow); virtual;
    procedure ImportLayout;
    procedure ImportMultiEditorRow(AdxRow: TObject; AcxRow: TcxCustomRow); virtual;
    procedure ImportRow(AcxRow: TcxCustomRow; AdxRow: TObject);
    procedure ImportRows; virtual;
    procedure SetRowName(AcxRow: TcxCustomRow); virtual;
    property Access: TcxCustomVerticalGridAccess read GetAccess;
    property Component: TComponent read GetComponent;
  public
    constructor Create(ADestination: TObject); override;
    destructor Destroy; override;
  end;

  { TcxDXInspConverter }

  TcxDXInspConverter = class(TcxCustomDXInspConverter)
  private
    function GetDestination: TcxUnboundVerticalGridAccess;
  protected
    procedure AssignEditorDataBinding(AdxRow: TObject; AProperties: TcxCustomEditorRowProperties); override;
    procedure AssignVerticalGridOptions; override;
    function CreateMultiEditorRowItem(
      AcxRow: TcxCustomRow): TcxCollectionItemEditorRowProperties; override;
    function GetRowClassType(AdxRow: TObject): TcxCustomRowClass; override;
  public
    class function GetSourceClassName: string; override;
    property Destination: TcxUnboundVerticalGridAccess read GetDestination;
  end;

  { TcxDXDBInspConverter }

  TcxDXDBInspConverter = class(TcxCustomDXInspConverter)
  private
    function GetDestination: TcxDBVerticalGrid;
  protected
    procedure AssignEditorDataBinding(AdxRow: TObject; AProperties: TcxCustomEditorRowProperties); override;
    procedure AssignVerticalGrid; override;
    procedure AssignVerticalGridOptions; override;
    function CreateMultiEditorRowItem(
      AcxRow: TcxCustomRow): TcxCollectionItemEditorRowProperties; override;
    function GetConverterIndex(AdxRow: TObject): Integer; override;
    function GetRowClassType(AdxRow: TObject): TcxCustomRowClass; override;
    procedure SetRowName(AcxRow: TcxCustomRow); override;
  public
    class function GetSourceClassName: string; override;
    property Destination: TcxDBVerticalGrid read GetDestination;
  end;

  { TcxDXRTTIConverter }

  TcxDXRTTIConverter = class(TcxDXInspConverter)
  protected
    procedure DoRealImport; override;
    procedure ImportRows; override;
  public
    class function GetSourceClassName: string; override;
  end;

implementation

uses
  Controls, cxDesignWindows, cxControls, cxEdit, cxDataStorage;

type
  TcxCustomRowAccess = class(TcxCustomRow);
  TcxCustomEditorRowAccess = class(TcxCustomEditorRow);
  TcxCustomEditorRowPropertiesAccess = class(TcxCustomEditorRowProperties);
  TcxCustomMultiEditorRowAccess = class(TcxCustomMultiEditorRow);
  TcxCustomPropertiesConverterClass = class of TcxCustomPropertiesConverter;

const
  ConverterTable: array[0..17] of record
    RowClassName: string;
    DBRowClassName: string;
    PropertiesClass: TcxCustomEditPropertiesClass;
    ConverterClass: TcxCustomPropertiesConverterClass;
    ValueType: TcxValueTypeClass;
  end = (
  (RowClassName: 'TdxInspectorTextRow';
    DBRowClassName: 'TdxInspectorDBRow';
    PropertiesClass: TcxTextEditProperties;
    ConverterClass: TcxTextEditPropertiesConverter;
    ValueType: nil),
  (RowClassName: 'TdxInspectorTextMaskRow';
    DBRowClassName: 'TdxInspectorDBMaskRow';
    PropertiesClass: TcxMaskEditProperties;
    ConverterClass: TcxMaskEditPropertiesConverter;
    ValueType: nil),
  (RowClassName: 'TdxInspectorTextDateRow';
    DBRowClassName: 'TdxInspectorDBDateRow';
    PropertiesClass: TcxDateEditProperties;
    ConverterClass: TcxDateEditPropertiesConverter;
    ValueType: TcxDateTimeValueType),
  (RowClassName: 'TdxInspectorTextCheckRow';
    DBRowClassName: 'TdxInspectorDBCheckRow';
    PropertiesClass: TcxCheckBoxProperties;
    ConverterClass: TcxCheckBoxPropertiesConverter;
    ValueType: TcxVariantValueType),
  (RowClassName: 'TdxInspectorTextCalcRow';
    DBRowClassName: 'TdxInspectorDBCalcRow';
    PropertiesClass: TcxCalcEditProperties;
    ConverterClass: TcxCalcEditPropertiesConverter;
    ValueType: TcxFloatValueType),
  (RowClassName: 'TdxInspectorTextButtonRow';
    DBRowClassName: 'TdxInspectorDBButtonRow';
    PropertiesClass: TcxButtonEditProperties;
    ConverterClass: TcxButtonEditPropertiesConverter;
    ValueType: nil),
  (RowClassName: 'TdxInspectorTextSpinRow';
    DBRowClassName: 'TdxInspectorDBSpinRow';
    PropertiesClass: TcxSpinEditProperties;
    ConverterClass: TcxSpinEditPropertiesConverter;
    ValueType: TcxFloatValueType),
  (RowClassName: 'TdxInspectorTextPickRow';
    DBRowClassName: 'TdxInspectorDBPickRow';
    PropertiesClass: TcxComboBoxProperties;
    ConverterClass: TcxComboBoxPropertiesConverter;
    ValueType: nil),
  (RowClassName: 'TdxInspectorTextImageRow';
    DBRowClassName: 'TdxInspectorDBImageRow';
    PropertiesClass: TcxImageComboBoxProperties;
    ConverterClass: TcxImageComboBoxPropertiesConverter;
    ValueType: TcxVariantValueType),
  (RowClassName: 'TdxInspectorTextTimeRow';
    DBRowClassName: 'TdxInspectorDBTimeRow';
    PropertiesClass: TcxTimeEditProperties;
    ConverterClass: TcxTimeEditPropertiesConverter;
    ValueType: TcxDateTimeValueType),
  (RowClassName: 'TdxInspectorTextCurrencyRow';
    DBRowClassName: 'TdxInspectorDBCurrencyRow';
    PropertiesClass: TcxCurrencyEditProperties;
    ConverterClass: TcxCurrencyEditPropertiesConverter;
    ValueType: TcxCurrencyValueType),
  (RowClassName: 'TdxInspectorTextHyperLinkRow';
    DBRowClassName: 'TdxInspectorDBHyperLinkRow';
    PropertiesClass: TcxHyperLinkEditProperties;
    ConverterClass: TcxHyperLinkEditPropertiesConverter;
    ValueType: nil),
  (RowClassName: 'TdxInspectorTextBlobRow';
    DBRowClassName: 'TdxInspectorBlobRow';
    PropertiesClass: TcxBlobEditProperties;
    ConverterClass: TcxBlobEditPropertiesConverter;
    ValueType: TcxVariantValueType),
  (RowClassName: 'TdxInspectorTextMRURow';
    DBRowClassName: 'TdxInspectorDBMRURow';
    PropertiesClass: TcxMRUEditProperties;
    ConverterClass: TcxMRUEditPropertiesConverter;
    ValueType: nil),
  (RowClassName: 'TdxInspectorTextPopupRow';
    DBRowClassName: 'TdxInspectorDBPopupRow';
    PropertiesClass: TcxPopupEditProperties;
    ConverterClass: TcxPopupEditPropertiesConverter;
    ValueType: nil),
  (RowClassName: 'TdxInspectorTextMemoRow';
    DBRowClassName: 'TdxInspectorDBMemoRow';
    PropertiesClass: TcxMemoProperties;
    ConverterClass: TcxMemoPropertiesConverter;
    ValueType: nil),
  (RowClassName: 'TdxInspectorTextGraphicRow';
    DBRowClassName: 'TdxInspectorDBGraphicRow';
    PropertiesClass: TcxImageProperties;
    ConverterClass: TcxImagePropertiesConverter;
    ValueType: TcxVariantValueType),
  (RowClassName: '';
    DBRowClassName: 'TdxInspectorLookupRow';
    PropertiesClass: TcxLookupComboBoxProperties;
    ConverterClass: TcxLookupComboBoxPropertiesConverter;
    ValueType: nil)
  );

function InheritsFromEx(AObject: TObject; const AParentClassName: string): Boolean;
var
  AClass: TClass;
begin
  Result := False;
  AClass := AObject.ClassType;
  while AClass <> nil do
    if SameText(AClass.ClassName, AParentClassName) then
    begin
      Result := True;
      break;
    end
    else
      AClass := AClass.ClassParent;
end;

{ TcxCustomDXInspConverter }

constructor TcxCustomDXInspConverter.Create(ADestination: TObject);
begin
  inherited Create(ADestination);
  FcxRowCache := TStringList.Create;
  FcxRowCache.Sorted := True;
  FdxRowCache := TStringList.Create;
  FdxRowCache.Sorted := True;
end;

destructor TcxCustomDXInspConverter.Destroy;
begin
  FcxRowCache.Free;
  FdxRowCache.Free;
  inherited Destroy;
end;

function TcxCustomDXInspConverter.AddRow(AcxRowParent: TcxCustomRow;
  const AdxRowName: string): TcxCustomRow;
var
  ARowClass: TcxCustomRowClass;
  AdxRow: TObject;
begin
  AdxRow := Component.Owner.FindComponent(AdxRowName);
  ARowClass := GetRowClassType(AdxRow);
  if ARowClass <> nil then
  begin
    Result := Destination.AddChild(AcxRowParent, ARowClass);
    ImportRow(Result, AdxRow);
    SetRowName(Result);
  end
  else
    Result := nil;
end;

procedure TcxCustomDXInspConverter.AssignCaptionProperties(AdxRow: TObject;
  AProperties: TcxCaptionRowProperties);
begin
  AProperties.Caption := GetStringProperty(AdxRow, 'Caption');
  AProperties.Hint := GetStringProperty(AdxRow, 'Hint');
  AProperties.ImageIndex := GetIntegerProperty(AdxRow, 'ImageIndex', -1);
end;

procedure TcxCustomDXInspConverter.AssignEditorDataBinding(
  AdxRow: TObject; AProperties: TcxCustomEditorRowProperties);
begin
end;

procedure TcxCustomDXInspConverter.AssignEditorProperties(
  AdxRow: TObject; AProperties: TcxCustomEditorRowProperties);
var
  AConverter: TcxCustomPropertiesConverter;
  Index: Integer;
begin
  Index := GetConverterIndex(AdxRow);
  if Index < 0 then Exit;
  AProperties.EditPropertiesClass := ConverterTable[Index].PropertiesClass;
  AConverter := ConverterTable[Index].ConverterClass.Create(AProperties.EditProperties);
  try
    AConverter.ImportFrom(AdxRow);
  finally
    AConverter.Free;
  end;
  AssignEditorDataBinding(AdxRow, AProperties);
end;

procedure TcxCustomDXInspConverter.AssignVerticalGrid;
var
  AFont: TFont;
begin
  ConvertPaintStyle;
  AFont := GetClassProperty(Source, 'Font') as TFont;
  Access.Font.Assign(AFont);
  with Destination do
  begin
    if SameText(GetEnumProperty(Source, 'BorderStyle'), 'bsNone') then
      BorderStyle := cxcbsNone
    else
      BorderStyle := cxcbsDefault;
    FDefaultRowHeight := GetIntegerProperty(Source, 'RowHeight', 17);
    FSeparatorString := GetStringProperty(Source, 'ComplexRowSeparator');
    StoringName := GetStringProperty(Source, 'RegistryPath');
    Images := TImageList(GetClassProperty(Source, 'Images'));
  end;
end;

procedure TcxCustomDXInspConverter.AssignVerticalGridOptions;
begin
  with Destination.OptionsView do
  begin
    if FDefaultRowHeight <> 17 then RowHeight := FDefaultRowHeight;
    CellTextMaxLineCount := GetIntegerProperty(Source, 'MaxRowTextLineCount', 0);
    RowHeaderMinWidth := GetIntegerProperty(Source, 'MinColumnWidth', 24);
    RowHeaderWidth := GetIntegerProperty(Source, 'DividerPos', 100);
    ValueWidth := GetIntegerProperty(Source, 'BandWidth', 150) - RowHeaderWidth;
    GridLineColor := GetIntegerProperty(Source, 'GridColor', clBtnFace);
  end;
end;

procedure TcxCustomDXInspConverter.ConvertPaintStyle;
var
  S: string;
begin
  S := GetEnumProperty(Source, 'PaintStyle');
  if SameText(S, 'ipsNet') or SameText(S, 'ipsCategorized') then
    Destination.OptionsView.PaintStyle := psdotNet
  else
    Destination.OptionsView.PaintStyle := psDelphi;
  if GetBooleanProperty(Source, 'Flat') then
    Destination.LookAndFeel.Kind := lfFlat
  else
    Destination.LookAndFeel.Kind := lfStandard;
end;

function TcxCustomDXInspConverter.CreateMultiEditorRowItem(
  AcxRow: TcxCustomRow): TcxCollectionItemEditorRowProperties;
begin
  Result := nil;
end;

procedure TcxCustomDXInspConverter.DoRealImport;
begin
  AssignVerticalGrid;
  AssignVerticalGridOptions;
  ImportRows;
end;

function TcxCustomDXInspConverter.GetConverterIndex(AdxRow: TObject): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to 16 do
    if SameText(AdxRow.ClassName, ConverterTable[I].RowClassName) then
    begin
      Result := I;
      break
    end;
end;

function TcxCustomDXInspConverter.GetRowClassType(
  AdxRow: TObject): TcxCustomRowClass;
begin
  Result := nil;
end;

type
  TComponentAccess = class(TComponent);

  PdxRowNodeInfo = ^TdxRowNodeInfo;
  TdxRowNodeInfo = packed record
    Count: Integer;
    StrLen: Integer;
    Str: record end;
  end;

  TcxConverterFiler = class(TFiler)
  private
    FHasData: Boolean;
    FReadDataProc: TStreamProc;
    FWriteDataProc: TStreamProc;
  public
    constructor CreateFor(AComponent: TObject);
    procedure CreateVisibleRows(AStream: TStream; AConverter: TcxCustomDXInspConverter);
    procedure DefineProperty(const Name: string; ReadData: TReaderProc;
      WriteData: TWriterProc; HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string; AReadData, AWriteData: TStreamProc; HasData: Boolean); override;
    procedure FlushBuffer; override;
    property HasData: Boolean read FHasData;
    property ReadDataProc: TStreamProc read FReadDataProc;
    property WriteDataProc: TStreamProc read FWriteDataProc;
  end;

constructor TcxConverterFiler.CreateFor(AComponent: TObject);
begin
  TComponentAccess(AComponent).DefineProperties(Self);
end;

procedure TcxConverterFiler.CreateVisibleRows(AStream: TStream;
  AConverter: TcxCustomDXInspConverter);
var
  AReader: TcxReader;
  I, AItemCount: Integer;

  procedure ReadInfo(AcxParentRow: TcxCustomRow);
  var
    PInfo: PdxRowNodeInfo;
    I, ACount, ASize: Integer;
    S: string;
  begin
    ASize := AReader.ReadInteger;
    GetMem(PInfo, ASize);
    try
      AStream.ReadBuffer(PInfo^, ASize);
      SetLength(S, PInfo^.StrLen);
      AStream.ReadBuffer(S[1], PInfo^.StrLen);
      ACount := PInfo^.Count;
      AcxParentRow := AConverter.AddRow(AcxParentRow, S);
      if AcxParentRow <> nil then
        AConverter.FdxRowCache.AddObject(S, AcxParentRow);
      for I := 0 to ACount - 1 do
        ReadInfo(AcxParentRow);
    finally
      FreeMem(PInfo, ASize);
    end;
  end;

  procedure ReadExpandedData;
  var
    PInfo: PdxRowNodeInfo;
    I, Index, ACount, ASize: Integer;
    S: string;
  begin
    if AStream.Position = AStream.Size then Exit;
    AConverter.Destination.FullCollapse;
    ACount := AReader.ReadInteger;
    for I := 0 to ACount - 1 do
    begin
      ASize := AReader.ReadInteger;
      GetMem(PInfo, ASize);
      try
        AStream.ReadBuffer(PInfo^, ASize);
        SetLength(S, PInfo^.StrLen);
        AStream.ReadBuffer(S[1], PInfo^.StrLen);
        Index := AConverter.FdxRowCache.IndexOf(S);
        if Index >= 0 then
          TcxCustomRow(AConverter.FdxRowCache.Objects[Index]).Expanded := True;
      finally
        FreeMem(PInfo, ASize);
      end;
    end;
  end;

begin
  AReader := TcxReader.Create(AStream);
  try
    AConverter.Destination.ClearRows;
    try
      if (AStream.Size > 0) and (AReader.ReadInteger > 0) then
      begin
        AItemCount := AReader.ReadInteger;
        for I := 0 to AItemCount - 1 do
          ReadInfo(nil);
        ReadExpandedData;
      end;
    except
      AConverter.Destination.ClearRows;
      raise;
    end;
  finally
    AReader.Free;
  end;
end;

procedure TcxConverterFiler.DefineProperty(const Name: string; ReadData: TReaderProc;
  WriteData: TWriterProc; HasData: Boolean);
begin
end;

procedure TcxConverterFiler.DefineBinaryProperty(const Name: string;
  AReadData, AWriteData: TStreamProc; HasData: Boolean);
begin
  if Name <> 'Data' then Exit;
  FHasData := HasData;
  FReadDataProc := AReadData;
  FWriteDataProc := AWriteData;
end;

procedure TcxConverterFiler.FlushBuffer;
begin
end;

procedure TcxCustomDXInspConverter.ImportCategoryRow(AdxRow: TObject;
  AcxRow: TcxCustomRow);
begin
  AssignCaptionProperties(AdxRow, TcxCategoryRow(AcxRow).Properties);
end;

procedure TcxCustomDXInspConverter.ImportEditorRow(AdxRow: TObject;
  AcxRow: TcxCustomRow);
var
  AProperties: TcxCustomEditorRowProperties;
begin
  AProperties := TcxCustomEditorRowAccess(AcxRow).Properties;
  AssignCaptionProperties(AdxRow, AProperties);
  AssignEditorProperties(AdxRow, AProperties);
end;

procedure TcxCustomDXInspConverter.ImportLayout;
var
  AFiler: TcxConverterFiler;
  AMemStream: TMemoryStream;
begin
  AFiler := TcxConverterFiler.CreateFor(Source);
  try
    if AFiler.HasData then
    begin
      AMemStream := TMemoryStream.Create;
      try
        begin
          AFiler.WriteDataProc(AMemStream);
          AMemStream.Position := 0;
          AFiler.CreateVisibleRows(AMemStream, Self);
        end;
      finally
        AMemStream.Free;
      end;
    end;
  finally
    AFiler.Free;
  end;
end;

procedure TcxCustomDXInspConverter.ImportMultiEditorRow(AdxRow: TObject;
  AcxRow: TcxCustomRow);
var
  I, AIndex: Integer;
  AItems, AItem, AItemRow: TObject;
  AMultiEditorRowItem: TcxCollectionItemEditorRowProperties;
begin
  if FSeparatorString <> '' then
    with TcxCustomMultiEditorRowAccess(AcxRow).Properties do
    begin
      SeparatorKind := skString;
      SeparatorString := FSeparatorString;
    end;
  AItems := GetClassProperty(AdxRow, 'Items');
  if AItems is TCollection then
    with TCollection(AItems) do
    begin
      for I := 0 to Count - 1 do
      begin
        AItem := Items[I];
        AItemRow := GetClassProperty(AItem, 'Row');
        if AItemRow <> nil then
        begin
          AMultiEditorRowItem := CreateMultiEditorRowItem(AcxRow);
          AssignCaptionProperties(AItemRow, AMultiEditorRowItem);
          AssignEditorProperties(AItemRow, AMultiEditorRowItem);
          if (I = 0) and (AMultiEditorRowItem.ImageIndex = -1) then
            AMultiEditorRowItem.ImageIndex := GetIntegerProperty(AdxRow, 'ImageIndex');
          AMultiEditorRowItem.Width := GetIntegerProperty(AItem, 'Width', 50);
          AIndex := FdxRowCache.IndexOf(GetStringProperty(AItemRow, 'Name'));
          if AIndex >= 0 then FdxRowCache.Delete(AIndex);
        end;
      end;
    end;
end;

procedure TcxCustomDXInspConverter.ImportRow(AcxRow: TcxCustomRow;
  AdxRow: TObject);
var
  H: Integer;
begin
  H := GetIntegerProperty(AdxRow, 'Height', 17);
  if H <> FDefaultRowHeight then
    AcxRow.Height := H;
  AcxRow.Visible := GetBooleanProperty(AdxRow, 'Visible');
  if AcxRow is TcxCategoryRow then
    ImportCategoryRow(AdxRow, AcxRow)
  else if AcxRow is TcxCustomMultiEditorRow then
    ImportMultiEditorRow(AdxRow, AcxRow)
  else
    ImportEditorRow(AdxRow, AcxRow);
end;

procedure TcxCustomDXInspConverter.ImportRows;
var
  I: Integer;
  AComponent: TComponent;
begin
  // get invisible rows
  with Component.Owner do
    for I := 0 to ComponentCount - 1 do
    begin
      AComponent := Components[I];
      if InheritsFromEx(AComponent, 'TdxInspectorRow') and
        (AComponent.GetParentComponent = Source) then
          if not GetBooleanProperty(AComponent, 'Visible', True) then
            FdxRowCache.AddObject(AComponent.Name, nil);
    end;
  // get visible rows
  ImportLayout;
  for I := 0 to FdxRowCache.Count -1 do
    if FdxRowCache.Objects[I] = nil then
       FdxRowCache.Objects[I] := AddRow(nil, FdxRowCache[I]);
end;

procedure TcxCustomDXInspConverter.SetRowName(AcxRow: TcxCustomRow);
begin
  AcxRow.Name := CreateUniqueName(Destination.Owner, Destination, AcxRow, 'Tcx', '');
end;

function TcxCustomDXInspConverter.GetAccess: TcxCustomVerticalGridAccess;
begin
  Result := TcxCustomVerticalGridAccess(inherited Destination);
end;

function TcxCustomDXInspConverter.GetComponent: TComponent;
begin
  Result := Source as TComponent;
end;

{ TcxDXInspConverter }

class function TcxDXInspConverter.GetSourceClassName: string;
begin
  Result := 'TdxInspector';
end;

procedure TcxDXInspConverter.AssignEditorDataBinding(
  AdxRow: TObject; AProperties: TcxCustomEditorRowProperties);
var
  Index: Integer;
  S: string;
begin
  Index := GetConverterIndex(AdxRow);
  if Index < 0 then Exit;
  if ConverterTable[Index].ValueType <> nil then
    TcxEditorRowProperties(AProperties).DataBinding.ValueTypeClass :=
      ConverterTable[Index].ValueType;
  S := GetStringProperty(AdxRow, 'Text');
  if S <> '' then
  try
    if ConverterTable[Index].ValueType = TcxDateTimeValueType then
      TcxEditorRowProperties(AProperties).Value := StrToDateTime(S)
    else
      TcxEditorRowProperties(AProperties).Value := S;
  except
  end;
end;

procedure TcxDXInspConverter.AssignVerticalGridOptions;
var
  S: TStringList;
begin
  inherited AssignVerticalGridOptions;
  S := TStringList.Create;
  with Destination do
  try
    GetSetProperty(Source, 'Options', S);
    if S.IndexOf('ioAutoBandCount') >= 0 then
      LayoutStyle := ulsBandsView
    else
      LayoutStyle := ulsSingleRecordView;
    OptionsView.AutoScaleBands := False;
    OptionsView.CellAutoHeight := S.IndexOf('ioRowAutoHeight') >= 0;
    OptionsView.CellEndEllipsis := S.IndexOf('ioDrawEndEllipsis') >= 0;
    OptionsBehavior.BandSizing := S.IndexOf('ioBandSizing') >= 0;
    OptionsBehavior.HeaderSizing := S.IndexOf('ioColumnSizing') >= 0;
    OptionsBehavior.GoToNextCellOnTab := S.IndexOf('ioTabThrough') >= 0;
    OptionsBehavior.GoToNextCellOnEnter := S.IndexOf('ioEnterThrough') >= 0;
    OptionsBehavior.RowSizing := S.IndexOf('ioRowSizing') >= 0;
    OptionsData.Editing := S.IndexOf('ioEditing') >= 0;
  finally
    S.Free;
  end;
end;

function TcxDXInspConverter.CreateMultiEditorRowItem(
  AcxRow: TcxCustomRow): TcxCollectionItemEditorRowProperties;
begin
  Result := TcxMultiEditorRow(AcxRow).Properties.Editors.Add;
end;

function TcxDXInspConverter.GetRowClassType(
  AdxRow: TObject): TcxCustomRowClass;
begin
  Result := nil;
  if AdxRow = nil then Exit;
  if GetBooleanProperty(AdxRow, 'IsCategory') then
    Result := TcxCategoryRow
  else if SameText(AdxRow.ClassName, 'TdxInspectorComplexRow') then
    Result := TcxMultiEditorRow
  else
    Result := TcxEditorRow;
end;

function TcxDXInspConverter.GetDestination: TcxUnboundVerticalGridAccess;
begin
  Result := TcxUnboundVerticalGridAccess(inherited Destination);
end;

{ TcxDXDBInspConverter }

class function TcxDXDBInspConverter.GetSourceClassName: string;
begin
  Result := 'TdxDBInspector';
end;

procedure TcxDXDBInspConverter.AssignEditorDataBinding(
  AdxRow: TObject; AProperties: TcxCustomEditorRowProperties);
begin
  TcxDBVerticalGridItemDataBinding(TcxCustomEditorRowPropertiesAccess(
    AProperties).DataBinding).FieldName := GetStringProperty(AdxRow, 'FieldName');
end;

procedure TcxDXDBInspConverter.AssignVerticalGrid;
begin
  inherited AssignVerticalGrid;
  Destination.DataController.DataSource :=
    TDataSource(GetClassProperty(Source, 'DataSource'));
end;

procedure TcxDXDBInspConverter.AssignVerticalGridOptions;
var
  S: TStringList;
begin
  inherited AssignVerticalGridOptions;
  S := TStringList.Create;
  with Destination do
  try
    GetSetProperty(Source, 'Options', S);
    if S.IndexOf('dioAutoBandCount') >= 0 then
      LayoutStyle := lsBandsView
    else
      LayoutStyle := lsSingleRecordView;
    OptionsView.AutoScaleBands := False;
    OptionsView.CellAutoHeight := S.IndexOf('dioRowAutoHeight') >= 0;
    OptionsView.CellEndEllipsis := S.IndexOf('dioDrawEndEllipsis') >= 0;
    OptionsBehavior.BandSizing := S.IndexOf('dioBandSizing') >= 0;
    OptionsBehavior.HeaderSizing := S.IndexOf('dioColumnSizing') >= 0;
    OptionsBehavior.GoToNextCellOnTab := S.IndexOf('dioTabThrough') >= 0;
    OptionsBehavior.GoToNextCellOnEnter := S.IndexOf('dioEnterThrough') >= 0;
    OptionsBehavior.RowSizing := S.IndexOf('dioRowSizing') >= 0;
    OptionsData.CancelOnExit := S.IndexOf('dioCancelOnExit') >= 0;
    OptionsData.Editing := S.IndexOf('dioEditing') >= 0;
  finally
    S.Free;
  end;
end;

function TcxDXDBInspConverter.CreateMultiEditorRowItem(
  AcxRow: TcxCustomRow): TcxCollectionItemEditorRowProperties;
begin
  Result := TcxDBMultiEditorRow(AcxRow).Properties.Editors.Add;
end;

function TcxDXDBInspConverter.GetConverterIndex(AdxRow: TObject): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to 17 do
    if SameText(AdxRow.ClassName, ConverterTable[I].DBRowClassName) then
    begin
      Result := I;
      break
    end;
end;

function TcxDXDBInspConverter.GetRowClassType(
  AdxRow: TObject): TcxCustomRowClass;
begin
  Result := nil;
  if AdxRow = nil then Exit;
  if GetBooleanProperty(AdxRow, 'IsCategory') then
    Result := TcxCategoryRow
  else if SameText(AdxRow.ClassName, 'TdxInspectorComplexRow') then
    Result := TcxDBMultiEditorRow
  else
    Result := TcxDBEditorRow;
end;

procedure TcxDXDBInspConverter.SetRowName(AcxRow: TcxCustomRow);
begin
  if AcxRow is TcxDBEditorRow then
    AcxRow.Name := CreateUniqueName(Destination.Owner, Destination, AcxRow,
      'Tcx', TcxDBEditorRow(AcxRow).Properties.DataBinding.FieldName)
  else
    inherited SetRowName(AcxRow);
end;

function TcxDXDBInspConverter.GetDestination: TcxDBVerticalGrid;
begin
  Result := TcxDBVerticalGrid(inherited Destination);
end;

{ TcxDXRTTIConverter }

class function TcxDXRTTIConverter.GetSourceClassName: string;
begin
  Result := 'TdxRTTIInspector';
end;

procedure TcxDXRTTIConverter.DoRealImport;
begin
  inherited DoRealImport;
  TcxCustomRTTIInspector(Destination).InspectedObject :=
    TPersistent(GetClassProperty(Source, 'InspectedObject'));
end;

procedure TcxDXRTTIConverter.ImportRows;
begin
//do nothing
end;

initialization
  ConverterFactory(cxVGGroupConverterName).RegisterConverter('DX Inspector Converter', TcxDXInspConverter);
  ConverterFactory(cxDBVGGroupConverterName).RegisterConverter('DX DBInspector Converter', TcxDXDBInspConverter);
  ConverterFactory(cxRTTIVGGroupConverterName).RegisterConverter('DX RTTIInspector Converter', TcxDXRTTIConverter);

end.
