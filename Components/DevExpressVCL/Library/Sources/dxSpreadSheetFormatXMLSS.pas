{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetFormatXMLSS;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxXMLDoc, dxHashUtils, dxCoreGraphics, cxGraphics, cxGeometry,
  dxSpreadSheetCore, dxSpreadSheetClasses, dxSpreadSheetUtils, dxSpreadSheetContainers, dxSpreadSheetTypes,
  dxSpreadSheetGraphics, dxTypeHelpers, dxSpreadSheetCoreStyles;

type

  { TdxSpreadSheetXMLSSReaderStylesMap }

  TdxSpreadSheetXMLSSReaderStylesMap = class(TDictionary<string, TdxSpreadSheetCellStyleHandle>)
  protected
    procedure ValueNotify(const Value: TdxSpreadSheetCellStyleHandle; Action: TCollectionNotification); override;
  end;

  { TdxSpreadSheetXMLSSReader }

  TdxSpreadSheetXMLSSReader = class(TdxSpreadSheetCustomReader)
  strict private
    FFormulasRefs: TdxSpreadSheetFormulaAsTextInfoList;
    FNumberFormatMap: TDictionary<string, Variant>;
    FStyles: TdxSpreadSheetXMLSSReaderStylesMap;
    FXML: TdxXMLDocument;
    FXMLIsOwn: Boolean;
  protected
    function ConvertBorderStyle(const ALineStyle: string; AWeight: Integer): TdxSpreadSheetCellBorderStyle;
    function ConvertColor(S: string): TColor;

    function CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper; override;
    procedure Initialize(ADoc: TdxXMLDocument; AIsOwnDoc: Boolean); virtual;
    procedure ReadNames(ANode: TdxXMLNode); virtual;
    procedure ReadStyles(ANode: TdxXMLNode); virtual;
    procedure ReadWorksheet(ANode: TdxXMLNode); virtual;

    function ReadStyleBorders(ANode: TdxXMLNode; AParentStyle: TdxSpreadSheetBordersHandle): TdxSpreadSheetBordersHandle; virtual;
    function ReadStyleBrush(ANode: TdxXMLNode; AParentStyle: TdxSpreadSheetBrushHandle): TdxSpreadSheetBrushHandle; virtual;
    function ReadStyleFont(ANode: TdxXMLNode; AParentStyle: TdxSpreadSheetFontHandle): TdxSpreadSheetFontHandle; virtual;
    function ReadStyleFormat(ANode: TdxXMLNode; AParentStyle: TdxSpreadSheetFormatHandle): TdxSpreadSheetFormatHandle; virtual;
    procedure ReadStyle(ANode: TdxXMLNode); virtual;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; ADocument: TdxXMLDocument); reintroduce; overload;
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); overload; override;
    destructor Destroy; override;
    procedure ReadData; override;
    //
    property FormulasRefs: TdxSpreadSheetFormulaAsTextInfoList read FFormulasRefs;
    property NumberFormatMap: TDictionary<string, Variant> read FNumberFormatMap;
    property Styles: TdxSpreadSheetXMLSSReaderStylesMap read FStyles;
    property XML: TdxXMLDocument read FXML;
  end;

  { TdxSpreadSheetXMLSSDefinedNamesReader }

  TdxSpreadSheetXMLSSDefinedNamesReader = class(TdxSpreadSheetCustomFilerSubTask)
  strict private
    FNode: TdxXMLNode;
    FView: TdxSpreadSheetTableView;
  protected
    procedure ReadNamedRange(ANode: TdxXMLNode); virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetXMLSSReader; AView: TdxSpreadSheetTableView; ANode: TdxXMLNode); overload;
    procedure Execute; override;
    //
    property Node: TdxXMLNode read FNode;
    property View: TdxSpreadSheetTableView read FView;
  end;

  { TdxSpreadSheetXMLSSTableViewReader }

  TdxSpreadSheetXMLSSTableViewReader = class(TdxSpreadSheetCustomFilerSubTask)
  strict private
    FAnchor: TPoint;
    FNode: TdxXMLNode;
    FOptions: TdxSpreadSheetClipboardPasteOptions;
    FView: TdxSpreadSheetTableView;

    function ConvertSize(const AValue: Double): Integer;
    function GetFonts: TdxSpreadSheetFonts;
    function GetOwner: TdxSpreadSheetXMLSSReader;
  protected
    procedure ReadCell(ANode: TdxXMLNode; ARowIndex, AColumnIndex: Integer); virtual;
    procedure ReadCellComment(ANode: TdxXMLNode; ACell: TdxSpreadSheetCell); virtual;
    procedure ReadCellData(ANode: TdxXMLNode; ACell: TdxSpreadSheetCell); virtual;
    procedure ReadColumn(ANode: TdxXMLNode; AColumnIndex: Integer); virtual;
    function ReadFormattedSharedString(ANode: TdxXMLNode; ADefaultFontHandle: TdxSpreadSheetFontHandle = nil): TdxSpreadSheetFormattedSharedString; virtual;
    procedure ReadProperties(ANode: TdxXMLNode); virtual;
    procedure ReadRow(ANode: TdxXMLNode; ARowIndex: Integer); virtual;
    procedure ReadRowCells(ANode: TdxXMLNode; ARowIndex: Integer); virtual;
    function ReadSharedString(ANode: TdxXMLNode; ADefaultFontHandle: TdxSpreadSheetFontHandle = nil): TdxSpreadSheetSharedString; virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetXMLSSReader; AView: TdxSpreadSheetTableView; ANode: TdxXMLNode); overload;
    constructor Create(AOwner: TdxSpreadSheetXMLSSReader; AView: TdxSpreadSheetTableView; ANode: TdxXMLNode;
      const AAnchor: TPoint; AOptions: TdxSpreadSheetClipboardPasteOptions); overload;
    procedure Execute; override;

    property Anchor: TPoint read FAnchor;
    property Fonts: TdxSpreadSheetFonts read GetFonts;
    property Node: TdxXMLNode read FNode;
    property Options: TdxSpreadSheetClipboardPasteOptions read FOptions;
    property Owner: TdxSpreadSheetXMLSSReader read GetOwner;
    property View: TdxSpreadSheetTableView read FView;
  end;

  { TdxSpreadSheetXMLSSWriter }

  TdxSpreadSheetXMLSSWriter = class(TdxSpreadSheetCustomWriter)
  strict private
    FStyles: TList<TdxSpreadSheetCellStyleHandle>;
    FXML: TdxXMLDocument;
    FXMLIsOwn: Boolean;

    function ConvertColor(AValue: TColor): string;
    function ConvertSize(AValue: Integer): Double;
  protected
    function CanWriteFormula(AFormula: TdxSpreadSheetFormula): Boolean; virtual;
    function CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper; override;
    procedure Initialize(ADoc: TdxXMLDocument; AIsOwnDoc: Boolean); virtual;
    procedure WriteCell(ANode: TdxXMLNode; ACell: TdxSpreadSheetCell; AStyleID: Integer; const AArea, AMergedArea: TRect);
    procedure WriteCellData(ACellNode: TdxXMLNode; ACell: TdxSpreadSheetCell);
    procedure WriteColumns(AParentNode: TdxXMLNode; AView: TdxSpreadSheetTableView; AArea: TRect);
    procedure WriteComment(ANode: TdxXMLNode; AComment: TdxSpreadSheetCommentContainer);
    procedure WriteNames(AParentNode: TdxXMLNode); virtual;
    procedure WriteRows(AParentNode: TdxXMLNode; AView: TdxSpreadSheetTableView; AArea: TRect);
    procedure WriteString(ACellNode: TdxXMLNode; AString: TdxSpreadSheetSharedString);
    procedure WriteStyle(ANode: TdxXMLNode; AStyle: TdxSpreadSheetCellStyleHandle; AStyleID: Integer);
    procedure WriteStyleAlignment(ANode: TdxXMLNode; AStyle: TdxSpreadSheetCellStyleHandle);
    procedure WriteStyleBorders(ANode: TdxXMLNode; AStyle: TdxSpreadSheetBordersHandle);
    procedure WriteStyleBrush(ANode: TdxXMLNode; AStyle: TdxSpreadSheetBrushHandle);
    procedure WriteStyleFont(ANode: TdxXMLNode; AStyle: TdxSpreadSheetFontHandle);
    procedure WriteStyleNumberFormat(ANode: TdxXMLNode; AStyle: TdxSpreadSheetFormatHandle);
    procedure WriteView(AParentNode: TdxXMLNode; AView: TdxSpreadSheetTableView; const AArea: TRect);
    procedure WriteViews(AParentNode: TdxXMLNode); virtual;
    //
    property Styles: TList<TdxSpreadSheetCellStyleHandle> read FStyles;
    property XML: TdxXMLDocument read FXML;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; ADocument: TdxXMLDocument); reintroduce; overload;
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); overload; override;
    destructor Destroy; override;
    procedure WriteData; override;
  end;

const
  sHTMLAttrColor = 'html:Color';
  sHTMLAttrFace = 'html:Face';
  sHTMLAttrSize = 'html:Size';

  sXMLSSAttrArrayRange = 'ss:ArrayRange';
  sXMLSSAttrAuthor = 'ss:Author';
  sXMLSSAttrAutoFitHeight = 'ss:AutoFitHeight';
  sXMLSSAttrAutoFitWidth = 'ss:AutoFitWidth';
  sXMLSSAttrBold = 'ss:Bold';
  sXMLSSAttrCharset = 'x:CharSet';
  sXMLSSAttrColor = 'ss:Color';
  sXMLSSAttrDefaultColumnWidth = 'ss:DefaultColumnWidth';
  sXMLSSAttrDefaultRowHeight = 'ss:DefaultRowHeight';
  sXMLSSAttrExpandedColumnCount = 'ss:ExpandedColumnCount';
  sXMLSSAttrExpandedRowCount = 'ss:ExpandedRowCount';
  sXMLSSAttrFontName = 'ss:FontName';
  sXMLSSAttrFormat = 'ss:Format';
  sXMLSSAttrFormula = 'ss:Formula';
  sXMLSSAttrHeight = 'ss:Height';
  sXMLSSAttrHorizontal = 'ss:Horizontal';
  sXMLSSAttrHRef = 'ss:HRef';
  sXMLSSAttrHRefScreenTip = 'x:HRefScreenTip';
  sXMLSSAttrID = 'ss:ID';
  sXMLSSAttrIndent = 'ss:Indent';
  sXMLSSAttrIndex = 'ss:Index';
  sXMLSSAttrItalic = 'ss:Italic';
  sXMLSSAttrLineStyle = 'ss:LineStyle';
  sXMLSSAttrMergedAcross = 'ss:MergeAcross';
  sXMLSSAttrMergedDown = 'ss:MergeDown';
  sXMLSSAttrName = 'ss:Name';
  sXMLSSAttrParent = 'ss:Parent';
  sXMLSSAttrPattern = 'ss:Pattern';
  sXMLSSAttrPatternColor = 'ss:PatternColor';
  sXMLSSAttrPosition = 'ss:Position';
  sXMLSSAttrProtected = 'ss:Protected';
  sXMLSSAttrReferTo = 'ss:ReferTo';
  sXMLSSAttrShowAlways = 'ss:ShowAlways';
  sXMLSSAttrShrinkToFit = 'ss:ShrinkToFit';
  sXMLSSAttrSize = 'ss:Size';
  sXMLSSAttrSpan = 'ss:Span';
  sXMLSSAttrStrikeThrough = 'ss:StrikeThrough';
  sXMLSSAttrStyleID = 'ss:StyleID';
  sXMLSSAttrType = 'ss:Type';
  sXMLSSAttrUnderline = 'ss:Underline';
  sXMLSSAttrVertical = 'ss:Vertical';
  sXMLSSAttrWeight = 'ss:Weight';
  sXMLSSAttrWidth = 'ss:Width';
  sXMLSSAttrWrapText = 'ss:WrapText';

  sXMLSSNodeAlignment = 'Alignment';
  sXMLSSNodeBorder = 'Border';
  sXMLSSNodeBorders = 'Borders';
  sXMLSSNodeCell = 'Cell';
  sXMLSSNodeColumn = 'Column';
  sXMLSSNodeComment = 'Comment';
  sXMLSSNodeCommentData = 'ss:Data';
  sXMLSSNodeData = 'Data';
  sXMLSSNodeData2 = 'ss:Data';
  sXMLSSNodeFont = 'Font';
  sXMLSSNodeInterior = 'Interior';
  sXMLSSNodeNamedRange = 'NamedRange';
  sXMLSSNodeNames = 'Names';
  sXMLSSNodeNumberFormat = 'NumberFormat';
  sXMLSSNodeProtection = 'Protection';
  sXMLSSNodeRow = 'Row';
  sXMLSSNodeStyle = 'Style';
  sXMLSSNodeStyles = 'Styles';
  sXMLSSNodeTable = 'Table';
  sXMLSSNodeWorkbook = 'Workbook';
  sXMLSSNodeWorksheet = 'Worksheet';

  sXMLSSStyleNameTemplate = 's%d';
  sXMLSSValueSingle = 'Single';
  sXMLSSDataTypeBoolean = 'Boolean';
  sXMLSSDataTypeDateTime = 'DateTime';
  sXMLSSDataTypeError = 'Error';
  sXMLSSDataTypeNumber = 'Number';
  sXMLSSDataTypeString = 'String';

implementation

uses
  dxSpreadSheetHyperlinks, dxSpreadSheetNumberFormat, Variants, dxSpreadSheetStyles, dxStringHelper;

type
  TdxSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetCellAccess = class(TdxSpreadSheetCell);
  TdxSpreadSheetCellDataFormatAccess = class(TdxSpreadSheetCellDataFormat);
  TdxSpreadSheetCommentContainerAccess = class(TdxSpreadSheetCommentContainer);
  TdxSpreadSheetTableColumnsAccess = class(TdxSpreadSheetTableColumns);
  TdxSpreadSheetTableRowAccess = class(TdxSpreadSheetTableRow);
  TdxSpreadSheetTableRowCellsAccess = class(TdxSpreadSheetTableRowCells);
  TdxSpreadSheetTableRowsAccess = class(TdxSpreadSheetTableRows);
  TdxSpreadSheetFormulaAccess = class(TdxSpreadSheetFormula);

  TdxXMLSSLineStyleInfo = record
    Name: string;
    Weight: Integer;
  end;

const
  HTMLNamespace = 'http://www.w3.org/TR/REC-html40';
  FontStyleMap: array[TFontStyle] of AnsiString = ('B', 'I', 'U', 'S');
  BordersNames: array[TcxBorder] of string = (
    'Left', 'Top', 'Right', 'Bottom'
  );
  CellDataTypeMap: array[TdxSpreadSheetCellDataType] of string = ('', sXMLSSDataTypeBoolean, sXMLSSDataTypeError,
    sXMLSSDataTypeNumber, sXMLSSDataTypeNumber, sXMLSSDataTypeDateTime, sXMLSSDataTypeNumber, sXMLSSDataTypeString,
    sXMLSSDataTypeString
  );
  CellFillStyleNames: array[TdxSpreadSheetCellFillStyle] of string = (
    'Solid', 'Gray75', 'Gray50', 'Gray25', 'Gray125', 'Gray0625', 'HorzStripe', 'VertStripe', 'ReverseDiagStripe',
    'DiagStripe', 'DiagCross', 'ThickDiagCross', 'ThinHorzStripe', 'ThinVertStripe', 'ThinReverseDiagStripe',
    'ThinDiagStripe', 'ThinHorzCross', 'ThinDiagCross'
  );
  HorzAttrValuesMap: array[TdxSpreadSheetDataAlignHorz] of string = (
    'Automatic', 'Left', 'Center', 'Right', 'Fill', 'Justify', 'Distributed'
  );
  VertAttrValuesMap: array[TdxSpreadSheetDataAlignVert] of string = (
    'Top', 'Center', 'Bottom', 'Justify', 'Distributed'
  );
  LineStyleMap: array[TdxSpreadSheetCellBorderStyle] of TdxXMLSSLineStyleInfo = (
    (Name: ''; Weight: 1),
    (Name: 'Continuous'; Weight: 0),
    (Name: 'Dot'; Weight: 1),
    (Name: 'DashDotDot'; Weight: 1),
    (Name: 'DashDot'; Weight: 1),
    (Name: 'Dash'; Weight: 1),
    (Name: 'Continuous'; Weight: 1),
    (Name: 'DashDotDot'; Weight: 2),
    (Name: 'SlantDashDot'; Weight: 2),
    (Name: 'DashDot'; Weight: 2),
    (Name: 'Dash'; Weight: 2),
    (Name: 'Continuous'; Weight: 2),
    (Name: 'Continuous'; Weight: 3),
    (Name: 'Double'; Weight: 3),
    (Name: 'None'; Weight: 0)
  );

function StringToAlignHorz(const S: string): TdxSpreadSheetDataAlignHorz;
var
  I: TdxSpreadSheetDataAlignHorz;
begin
  for I := Low(TdxSpreadSheetDataAlignHorz) to High(TdxSpreadSheetDataAlignHorz) do
  begin
    if S = HorzAttrValuesMap[I] then
      Exit(I);
  end;
  if S = 'JustifyDistributed' then
    Result := ssahJustify
  else
    Result := ssahGeneral;
end;

function StringToAlignVert(const S: string): TdxSpreadSheetDataAlignVert;
var
  I: TdxSpreadSheetDataAlignVert;
begin
  for I := Low(TdxSpreadSheetDataAlignVert) to High(TdxSpreadSheetDataAlignVert) do
  begin
    if S = VertAttrValuesMap[I] then
      Exit(I);
  end;
  if S = 'JustifyDistributed' then
    Result := ssavJustify
  else
    Result := ssavBottom;
end;

function StringToCellDataType(const S: string): TdxSpreadSheetCellDataType;
var
  I: TdxSpreadSheetCellDataType;
begin
  for I := Low(TdxSpreadSheetCellDataType) to High(TdxSpreadSheetCellDataType) do
  begin
    if CellDataTypeMap[I] = S then
      Exit(I);
  end;
  Result := cdtBlank;
end;

function StringToCellFillStyle(const S: string): TdxSpreadSheetCellFillStyle;
var
  I: TdxSpreadSheetCellFillStyle;
begin
  for I := Low(TdxSpreadSheetCellFillStyle) to High(TdxSpreadSheetCellFillStyle) do
    if CellFillStyleNames[I] = S then
      Exit(I);
  Result := sscfsSolid;
end;

{ TdxSpreadSheetXMLSSReaderStylesMap }

procedure TdxSpreadSheetXMLSSReaderStylesMap.ValueNotify(
  const Value: TdxSpreadSheetCellStyleHandle; Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      Value.AddRef;
    cnRemoved:
      Value.Release;
  end;
end;

{ TdxSpreadSheetXMLSSReader }

constructor TdxSpreadSheetXMLSSReader.Create(AOwner: TdxCustomSpreadSheet; ADocument: TdxXMLDocument);
begin
  inherited Create(AOwner, nil);
  Initialize(ADocument, False);
end;

constructor TdxSpreadSheetXMLSSReader.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
begin
  inherited Create(AOwner, AStream);
  Initialize(TdxXMLDocument.Create(nil), True);
  FXML.LoadFromStream(AStream);
end;

destructor TdxSpreadSheetXMLSSReader.Destroy;
begin
  if FXMLIsOwn then
    FreeAndNil(FXML);
  FreeAndNil(FNumberFormatMap);
  FreeAndNil(FFormulasRefs);
  FreeAndNil(FStyles);
  inherited Destroy;
end;

function TdxSpreadSheetXMLSSReader.ConvertBorderStyle(
  const ALineStyle: string; AWeight: Integer): TdxSpreadSheetCellBorderStyle;
var
  AStyle: TdxSpreadSheetCellBorderStyle;
begin
  for AStyle := Low(TdxSpreadSheetCellBorderStyle) to High(TdxSpreadSheetCellBorderStyle) do
  begin
    if (LineStyleMap[AStyle].Name = ALineStyle) and (LineStyleMap[AStyle].Weight = AWeight) then
      Exit(AStyle);
  end;
  Result := sscbsDefault;
end;

function TdxSpreadSheetXMLSSReader.ConvertColor(S: string): TColor;
begin
  if (S <> '') and (S[1] = '#') then
    Delete(S, 1, 1);
  Result := dxAlphaColorToColor(TdxColorHelper.HexCodeToAlphaColor(S, False));
end;

function TdxSpreadSheetXMLSSReader.CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
begin
  Result := TdxSpreadSheetCustomFilerProgressHelper.Create(Self, 1);
end;

procedure TdxSpreadSheetXMLSSReader.Initialize(ADoc: TdxXMLDocument; AIsOwnDoc: Boolean);
begin
  FXML := ADoc;
  FXMLIsOwn := AIsOwnDoc;
  FStyles := TdxSpreadSheetXMLSSReaderStylesMap.Create;
  FFormulasRefs := TdxSpreadSheetFormulaAsTextInfoList.Create(SpreadSheet);
  FNumberFormatMap := TDictionary<string, Variant>.Create;

  FNumberFormatMap.Add('General Date', $16);
  FNumberFormatMap.Add('Long Date', $0F);
  FNumberFormatMap.Add('Medium Date', $0F);
  FNumberFormatMap.Add('Short Date', $0E);
  FNumberFormatMap.Add('Long Time', $13);
  FNumberFormatMap.Add('Medium Time', $12);
  FNumberFormatMap.Add('Short Time', $14);
  FNumberFormatMap.Add('Currency', $08);
  FNumberFormatMap.Add('Euro Currency', '[$€-2]\ ###,000_);[Red]\([$€-2]\ ###,000\)');
  FNumberFormatMap.Add('Fixed', $02);
  FNumberFormatMap.Add('Standard', $04);
  FNumberFormatMap.Add('Percent', $0A);
  FNumberFormatMap.Add('Scientific', $0B);
  FNumberFormatMap.Add('Yes/No', '"Yes";"Yes";"No"');
  FNumberFormatMap.Add('True/False', '"True";"True";"False"');
  FNumberFormatMap.Add('On/Off', '"On";"On";"Off"');
end;

procedure TdxSpreadSheetXMLSSReader.ReadData;
var
  ANode: TdxXMLNode;
  ASavedFormatSettings: TFormatSettings;
  ASavedR1C1Reference: Boolean;
begin
  ASavedR1C1Reference := SpreadSheet.OptionsView.R1C1Reference;
  ASavedFormatSettings := SpreadSheet.FormulaController.FormatSettings.Data;
  SpreadSheet.BeginUpdate;
  try
    SpreadSheet.OptionsView.R1C1Reference := True;
    dxGetLocaleFormatSettings(dxGetInvariantLocaleID, SpreadSheet.FormulaController.FormatSettings.Data);

    if XML.FindChild([sXMLSSNodeWorkbook], ANode) then
    begin
      ProgressHelper.BeginStage(ANode.Count);
      try
        ANode.ForEach(
          procedure (ANode: TdxXMLNode; AData: Pointer)
          begin
            if ANode.Name = sXMLSSNodeStyles then
              ReadStyles(ANode);
            if ANode.Name = sXMLSSNodeWorksheet then
              ReadWorksheet(ANode);
            if ANode.Name = sXMLSSNodeNames then
              ReadNames(ANode);
            ProgressHelper.NextTask;
          end);
        FormulasRefs.ResolveReferences;
      finally
        ProgressHelper.EndStage;
      end;
    end;
  finally
    SpreadSheet.FormulaController.FormatSettings.Data := ASavedFormatSettings;
    SpreadSheet.OptionsView.R1C1Reference := ASavedR1C1Reference;
    SpreadSheet.EndUpdate;
  end;
end;

procedure TdxSpreadSheetXMLSSReader.ReadNames(ANode: TdxXMLNode);
begin
  ExecuteSubTask(TdxSpreadSheetXMLSSDefinedNamesReader.Create(Self, nil, ANode));
end;

procedure TdxSpreadSheetXMLSSReader.ReadStyles(ANode: TdxXMLNode);
begin
  ANode.ForEach(
    procedure (ANode: TdxXMLNode; AData: Pointer)
    begin
      ReadStyle(ANode);
    end);
end;

procedure TdxSpreadSheetXMLSSReader.ReadWorksheet(ANode: TdxXMLNode);
var
  ATableNode: TdxXMLNode;
begin
  if ANode.FindChild(sXMLSSNodeTable, ATableNode) then
  begin
    ExecuteSubTask(TdxSpreadSheetXMLSSTableViewReader.Create(Self,
      AddTableView(ANode.Attributes.GetValueAsString(sXMLSSAttrName)), ATableNode));
  end;
end;

function TdxSpreadSheetXMLSSReader.ReadStyleBorders(
  ANode: TdxXMLNode; AParentStyle: TdxSpreadSheetBordersHandle): TdxSpreadSheetBordersHandle;
var
  AStyle: TdxSpreadSheetBordersHandle;
begin
  if (ANode = nil) or (ANode.Count = 0) then
    Exit(AParentStyle);

  AStyle := CreateTempBordersHandle;
  AStyle.Assign(AParentStyle);

  ANode.ForEach(
    procedure (ANode: TdxXMLNode; AData: Pointer)
    var
      ASide: TcxBorder;
    begin
      for ASide := Low(TcxBorder) to High(TcxBorder) do
        if ANode.Attributes.GetValueAsString(sXMLSSAttrPosition) = BordersNames[ASide] then
        begin
          AStyle.BorderStyle[ASide] := ConvertBorderStyle(
            ANode.Attributes.GetValueAsString(sXMLSSAttrLineStyle),
            ANode.Attributes.GetValueAsInteger(sXMLSSAttrWeight));
          if ANode.Attributes.Exists(sXMLSSAttrColor) then
            AStyle.BorderColor[ASide] := ConvertColor(ANode.Attributes.GetValueAsString(sXMLSSAttrColor));
          Break;
        end;
    end);

  Result := AddBorders(AStyle);
end;

function TdxSpreadSheetXMLSSReader.ReadStyleBrush(
  ANode: TdxXMLNode; AParentStyle: TdxSpreadSheetBrushHandle): TdxSpreadSheetBrushHandle;
begin
  if ANode = nil then
    Exit(AParentStyle);

  Result := CreateTempBrushHandle;
  Result.Assign(AParentStyle);
  if ANode.Attributes.Exists(sXMLSSAttrColor) then
    Result.BackgroundColor := ConvertColor(ANode.Attributes.GetValueAsString(sXMLSSAttrColor));
  if ANode.Attributes.Exists(sXMLSSAttrPatternColor) then
    Result.ForegroundColor := ConvertColor(ANode.Attributes.GetValueAsString(sXMLSSAttrPatternColor));
  Result.Style := StringToCellFillStyle(ANode.Attributes.GetValueAsString(sXMLSSAttrPattern));
  Result := AddBrush(Result);
end;

function TdxSpreadSheetXMLSSReader.ReadStyleFont(
  ANode: TdxXMLNode; AParentStyle: TdxSpreadSheetFontHandle): TdxSpreadSheetFontHandle;
begin
  if ANode = nil then
    Exit(AParentStyle);

  Result := CreateTempFontHandle;
  Result.Assign(AParentStyle);
  Result.Name := ANode.Attributes.GetValueAsString(sXMLSSAttrFontName);
  Result.Size := ANode.Attributes.GetValueAsInteger(sXMLSSAttrSize, 10);
  Result.Color := ConvertColor(ANode.Attributes.GetValueAsString(sXMLSSAttrColor));
  Result.Charset := ANode.Attributes.GetValueAsInteger(sXMLSSAttrCharset);

  if ANode.Attributes.GetValueAsBoolean(sXMLSSAttrBold) then
    Include(Result.Style, fsBold);
  if ANode.Attributes.GetValueAsBoolean(sXMLSSAttrItalic) then
    Include(Result.Style, fsItalic);
  if ANode.Attributes.GetValueAsBoolean(sXMLSSAttrStrikeThrough) then
    Include(Result.Style, fsStrikeOut);
  if ANode.HasAttribute(sXMLSSAttrUnderline) and (ANode.Attributes.GetValueAsString(sXMLSSAttrUnderline) <> 'None') then
    Include(Result.Style, fsUnderline);

  Result := AddFont(Result);
end;

function TdxSpreadSheetXMLSSReader.ReadStyleFormat(
  ANode: TdxXMLNode; AParentStyle: TdxSpreadSheetFormatHandle): TdxSpreadSheetFormatHandle;
var
  AFormat: string;
  AValue: Variant;
begin
  Result := AParentStyle;
  if (ANode <> nil) and ANode.Attributes.Exists(sXMLSSAttrFormat) then
  begin
    AFormat := ANode.Attributes.GetValueAsString(sXMLSSAttrFormat);
    if FNumberFormatMap.TryGetValue(AFormat, AValue) then
    begin
      if VarIsNumeric(AValue) then
        Exit(CellStyles.Formats.PredefinedFormats.GetFormatHandleByID(AValue));
      AFormat := AValue;
    end;
    Result := AddNumberFormat(AFormat);
  end;
end;

procedure TdxSpreadSheetXMLSSReader.ReadStyle(ANode: TdxXMLNode);

  procedure ModifyState(AStyleHandle: TdxSpreadSheetCellStyleHandle; AStateIndex: TdxSpreadSheetCellState; AValue: Boolean);
  begin
    if AValue then
      AStyleHandle.States := AStyleHandle.States + [AStateIndex]
    else
      AStyleHandle.States := AStyleHandle.States - [AStateIndex];
  end;

var
  AParentStyleHandle: TdxSpreadSheetCellStyleHandle;
  AStyleHandle: TdxSpreadSheetCellStyleHandle;
  ASubNode: TdxXMLNode;
begin
  if not Styles.TryGetValue(ANode.Attributes.GetValueAsString(sXMLSSAttrParent), AParentStyleHandle) then
    AParentStyleHandle := SpreadSheet.DefaultCellStyle.Handle;

  AStyleHandle := CellStyles.CreateStyle;
  AStyleHandle.Borders := ReadStyleBorders(ANode.FindChild(sXMLSSNodeBorders), AParentStyleHandle.Borders);
  AStyleHandle.Font := ReadStyleFont(ANode.FindChild(sXMLSSNodeFont), AParentStyleHandle.Font);
  AStyleHandle.DataFormat := ReadStyleFormat(ANode.FindChild(sXMLSSNodeNumberFormat), AParentStyleHandle.DataFormat);
  AStyleHandle.Brush := ReadStyleBrush(ANode.FindChild(sXMLSSNodeInterior), AParentStyleHandle.Brush);

  if ANode.FindChild(sXMLSSNodeAlignment, ASubNode) then
  begin
    AStyleHandle.AlignHorz := StringToAlignHorz(ASubNode.Attributes.GetValueAsString(sXMLSSAttrHorizontal));
    AStyleHandle.AlignVert := StringToAlignVert(ASubNode.Attributes.GetValueAsString(sXMLSSAttrVertical));
    AStyleHandle.AlignHorzIndent := ASubNode.Attributes.GetValueAsInteger(sXMLSSAttrIndent);

    ModifyState(AStyleHandle, csShrinkToFit, ASubNode.Attributes.GetValueAsBoolean(sXMLSSAttrShrinkToFit));
    ModifyState(AStyleHandle, csWordWrap, ASubNode.Attributes.GetValueAsBoolean(sXMLSSAttrWrapText));
  end;

  if ANode.FindChild(sXMLSSNodeProtection, ASubNode) then
  begin
    if ASubNode.Attributes.Exists(sXMLSSAttrProtected) then
      ModifyState(AStyleHandle, csLocked, ASubNode.Attributes.GetValueAsBoolean(sXMLSSAttrProtected, True));
  end;

  Styles.AddOrSetValue(ANode.Attributes.GetValueAsString(sXMLSSAttrID), AddCellStyle(AStyleHandle));
end;

{ TdxSpreadSheetXMLSSDefinedNamesReader }

constructor TdxSpreadSheetXMLSSDefinedNamesReader.Create(
  AOwner: TdxSpreadSheetXMLSSReader; AView: TdxSpreadSheetTableView; ANode: TdxXMLNode);
begin
  inherited Create(AOwner);
  FNode := ANode;
  FView := AView;
end;

procedure TdxSpreadSheetXMLSSDefinedNamesReader.Execute;
begin
  Node.ForEach(
    procedure (ANode: TdxXMLNode; AUserData: Pointer)
    begin
      if ANode.Name = sXMLSSNodeNamedRange then
        ReadNamedRange(ANode);
    end);
end;

procedure TdxSpreadSheetXMLSSDefinedNamesReader.ReadNamedRange(ANode: TdxXMLNode);
begin
  try
    SpreadSheet.DefinedNames.Add(
      ANode.Attributes.GetValueAsString(sXMLSSAttrName),
      ANode.Attributes.GetValueAsString(sXMLSSAttrReferTo), View);
  except
    // do nothing
  end;
end;

{ TdxSpreadSheetXMLSSTableViewReader }

constructor TdxSpreadSheetXMLSSTableViewReader.Create(
  AOwner: TdxSpreadSheetXMLSSReader; AView: TdxSpreadSheetTableView; ANode: TdxXMLNode);
begin
  Create(AOwner, AView, ANode, cxNullPoint, [cpoValues..cpoColumnWidths]);
end;

constructor TdxSpreadSheetXMLSSTableViewReader.Create(
  AOwner: TdxSpreadSheetXMLSSReader; AView: TdxSpreadSheetTableView; ANode: TdxXMLNode;
  const AAnchor: TPoint; AOptions: TdxSpreadSheetClipboardPasteOptions);
begin
  inherited Create(AOwner);
  FOptions := AOptions;
  FAnchor := AAnchor;
  FNode := ANode;
  FView := AView;
end;

procedure TdxSpreadSheetXMLSSTableViewReader.Execute;
type
  TProcessProc = procedure (ANode: TdxXMLNode; AItem: Integer) of object;

  procedure ProcessTableItems(var ANode: TdxXMLNode; AAnchor: Integer;
    const AItemName: TdxXMLString; AProcessProc: TProcessProc);
  var
    AIndex: Integer;
    ASpan: Integer;
  begin
    AIndex := 0;
    while (ANode <> nil) and (ANode.Name = AItemName) do
    begin
      if ANode.HasAttribute(sXMLSSAttrIndex) then
        AIndex := ANode.Attributes.GetValueAsInteger(sXMLSSAttrIndex) - 1;
      ASpan := ANode.Attributes.GetValueAsInteger(sXMLSSAttrSpan) + 1;
      while ASpan > 0 do
      begin
        AProcessProc(ANode, AAnchor + AIndex);
        Inc(AIndex);
        Dec(ASpan);
      end;
      ANode := ANode.Next;
    end;
  end;

var
  ANode: TdxXMLNode;
begin
  ReadProperties(Node);
  if Node.Count > 0 then
  begin
    ANode := Node.First;
    ProcessTableItems(ANode, Anchor.X, sXMLSSNodeColumn, ReadColumn);
    ProcessTableItems(ANode, Anchor.Y, sXMLSSNodeRow, ReadRow);
  end;
end;

procedure TdxSpreadSheetXMLSSTableViewReader.ReadCell(ANode: TdxXMLNode; ARowIndex, AColumnIndex: Integer);
var
  ACell: TdxSpreadSheetCell;
  AHandle: TdxSpreadSheetCellStyleHandle;
  ASubNode: TdxXMLNode;
begin
  if (cpoSkipBlanks in Options) and (ANode.Count = 0) then
    Exit;

  ACell := nil;
  if [cpoStyles, cpoNumberFormatting] * Options <> [] then
  begin
    if Owner.Styles.TryGetValue(ANode.Attributes.GetValueAsString(sXMLSSAttrStyleID), AHandle) then
    begin
      if ACell = nil then
        ACell := View.CreateCell(ARowIndex, AColumnIndex);
      if cpoStyles in Options then
      begin
        if not ACell.Style.Locked then
        begin
          ACell.Style.Handle := AHandle;
          ACell.Style.Locked := False;
        end
        else
          ACell.Style.Handle := AHandle;
      end
      else
        TdxSpreadSheetCellDataFormatAccess(ACell.Style.DataFormat).Handle := AHandle.DataFormat;
    end;
  end;

  if cpoValues in Options then
  begin
    if ACell = nil then
      ACell := View.CreateCell(ARowIndex, AColumnIndex);
    ReadCellData(ANode, ACell);
  end;

  if cpoComments in Options then
  begin
    if ANode.FindChild(sXMLSSNodeComment, ASubNode) then
    begin
      if ACell = nil then
        ACell := View.CreateCell(ARowIndex, AColumnIndex);
      ReadCellComment(ASubNode, ACell);
    end;
  end;
end;

procedure TdxSpreadSheetXMLSSTableViewReader.ReadCellComment(ANode: TdxXMLNode; ACell: TdxSpreadSheetCell);
var
  AComment: TdxSpreadSheetCommentContainerAccess;
  ASharedString: TdxSpreadSheetFormattedSharedString;
begin
  ASharedString := ReadFormattedSharedString(ANode.FindChild(sXMLSSNodeCommentData));
  try
    if not View.Containers.FindCommentContainer(ACell, AComment) then
      View.Containers.Add(TdxSpreadSheetCommentContainer, AComment);

    AComment.BeginChanging;
    try
      AComment.Cell := ACell;
      AComment.Author := ANode.Attributes.GetValueAsString(sXMLSSAttrAuthor);
      AComment.Calculator.UpdateAnchors(TdxSpreadSheetCommentContainerHelper.GetDefaultPosition(AComment.Cell));
      AComment.Visible := ANode.Attributes.GetValueAsBoolean(sXMLSSAttrShowAlways);
      AComment.TextBox.TextAsString := ASharedString.Value;
      if ASharedString.Runs.Count > 0 then
        AComment.TextBox.Font.Handle := ASharedString.Runs[0].FontHandle;
    finally
      AComment.EndChanging;
    end;
  finally
    ASharedString.Free;
  end;
end;

procedure TdxSpreadSheetXMLSSTableViewReader.ReadCellData(ANode: TdxXMLNode; ACell: TdxSpreadSheetCell);
var
  ADataNode: TdxXMLNode;
  AHyperlink: TdxSpreadSheetHyperlink;
begin
  ACell.Clear;

  if ANode.HasAttribute(sXMLSSAttrHRef) then
  begin
    AHyperlink := View.Hyperlinks.FindItem(ACell.RowIndex, ACell.ColumnIndex);
    if AHyperlink = nil then
      AHyperlink := View.Hyperlinks.Add(ACell.RowIndex, ACell.ColumnIndex);
    AHyperlink.ScreenTip := ANode.Attributes.GetValueAsString(sXMLSSAttrHRefScreenTip);
    AHyperlink.Value := ANode.Attributes.GetValueAsString(sXMLSSAttrHRef);
  end;

  if ANode.HasAttribute(sXMLSSAttrFormula) and (cpoFormulas in Options) then
  begin
    Owner.FormulasRefs.Add(ACell, ANode.Attributes.GetValueAsString(sXMLSSAttrFormula),
      ANode.HasAttribute(sXMLSSAttrArrayRange), False,
      dxStringToReferenceArea(ANode.Attributes.GetValueAsString(sXMLSSAttrArrayRange)));
  end
  else
    if ANode.FindChild(sXMLSSNodeData, ADataNode) or ANode.FindChild(sXMLSSNodeData2, ADataNode) then
      case StringToCellDataType(ADataNode.Attributes.GetValueAsString(sXMLSSAttrType)) of
        cdtBoolean:
          ACell.AsBoolean := ADataNode.TextAsBoolean;
        cdtCurrency, cdtFloat, cdtInteger:
          ACell.SetText(ADataNode.TextAsString);
        cdtDateTime:
          ACell.AsDateTime := ADataNode.TextAsDateTime;
        cdtError:
          ACell.AsString := ADataNode.TextAsString;
        cdtString:
          ACell.AsSharedString := Owner.StringTable.Add(ReadSharedString(ADataNode, ACell.StyleHandle.Font));
      end;
end;

procedure TdxSpreadSheetXMLSSTableViewReader.ReadColumn(ANode: TdxXMLNode; AColumnIndex: Integer);
begin
  if cpoColumnWidths in Options then
  begin
    if ANode.HasAttribute(sXMLSSAttrWidth) then
      View.Columns.CreateItem(AColumnIndex).Size := ConvertSize(ANode.Attributes.GetValueAsFloat(sXMLSSAttrWidth));
  end;
end;

function TdxSpreadSheetXMLSSTableViewReader.ReadFormattedSharedString(
  ANode: TdxXMLNode; ADefaultFontHandle: TdxSpreadSheetFontHandle = nil): TdxSpreadSheetFormattedSharedString;

  function ReadFont(ANode, ARootNode: TdxXMLNode): TdxSpreadSheetFontHandle;
  var
    AStyle: TFontStyle;
  begin
    Result := Fonts.CreateFont;
    if ADefaultFontHandle = nil then
      ADefaultFontHandle := Result;

    while ANode <> ARootNode do
    begin
      for AStyle := Low(AStyle) to High(AStyle) do
        if ANode.Name = FontStyleMap[AStyle] then
        begin
          Result.Style := Result.Style + [AStyle];
          Break;
        end;

      if ANode.Name = sXMLSSNodeFont then
      begin
        Result.Charset := ANode.Attributes.GetValueAsInteger(sXMLSSAttrCharset, ADefaultFontHandle.Charset);
        Result.Color := Owner.ConvertColor(ANode.Attributes.GetValueAsString(sHTMLAttrColor));
        Result.Name := ANode.Attributes.GetValueAsString(sHTMLAttrFace, ADefaultFontHandle.Name);
        Result.Size := ANode.Attributes.GetValueAsInteger(sHTMLAttrSize, ADefaultFontHandle.Size);
      end;

      ANode := ANode.Parent;
    end;
    Result := Fonts.AddFont(Result);
  end;

  procedure Process(ARuns: TdxSpreadSheetFormattedSharedStringRuns; ABuffer: TStringBuilder; ANode, AParentNode: TdxXMLNode);
  begin
    if ANode.Text <> '' then
    begin
      ARuns.Add(ABuffer.Length + 1, ReadFont(ANode, AParentNode));
      ABuffer.Append(ANode.TextAsString);
    end;

    ANode := ANode.First;
    while ANode <> nil do
    begin
      Process(ARuns, ABuffer, ANode, AParentNode);
      ANode := ANode.Next;
    end;
  end;

var
  ABuffer: TStringBuilder;
  ARuns: TdxSpreadSheetFormattedSharedStringRuns;
begin
  ABuffer := TdxStringBuilderManager.Get(128);
  try
    ARuns := TdxSpreadSheetFormattedSharedStringRuns.Create;
    try
      Process(ARuns, ABuffer, ANode, ANode);
      Result := TdxSpreadSheetFormattedSharedString.CreateObject(ABuffer.ToString);
      Result.Runs.Assign(ARuns);
    finally
      ARuns.Free;
    end;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

procedure TdxSpreadSheetXMLSSTableViewReader.ReadRow(ANode: TdxXMLNode; ARowIndex: Integer);
var
  ARow: TdxSpreadSheetTableRowAccess;
begin
  ARow := TdxSpreadSheetTableRowAccess(View.Rows.CreateItem(ARowIndex));
  if ANode.HasAttribute(sXMLSSAttrHeight) then
    ARow.Size := ConvertSize(ANode.Attributes.GetValueAsFloat(sXMLSSAttrHeight));
  ARow.IsCustomSize := not ANode.Attributes.GetValueAsBoolean(sXMLSSAttrAutoFitHeight, True);
  ReadRowCells(ANode.First, ARowIndex);
end;

procedure TdxSpreadSheetXMLSSTableViewReader.ReadRowCells(ANode: TdxXMLNode; ARowIndex: Integer);
var
  AAreaSize: TSize;
  AColumnIndex: Integer;
begin
  AColumnIndex := 0;
  while (ANode <> nil) and (ANode.Name = sXMLSSNodeCell) do
  begin
    if ANode.HasAttribute(sXMLSSAttrIndex) then
      AColumnIndex := ANode.Attributes.GetValueAsInteger(sXMLSSAttrIndex) - 1;
    AAreaSize := cxSize(
      ANode.Attributes.GetValueAsInteger(sXMLSSAttrMergedAcross) + 1,
      ANode.Attributes.GetValueAsInteger(sXMLSSAttrMergedDown) + 1);
    ReadCell(ANode, ARowIndex, Anchor.X + AColumnIndex);
    if (AAreaSize.cx > 1) or (AAreaSize.cy > 1) then
    begin
      if cpoStyles in Options then
        View.MergedCells.Add(cxRectBounds(Anchor.X + AColumnIndex, ARowIndex, AAreaSize.cx - 1, AAreaSize.cy - 1));
    end;
    Inc(AColumnIndex, AAreaSize.cx);
    ANode := ANode.Next;
  end;
end;

function TdxSpreadSheetXMLSSTableViewReader.ReadSharedString(
  ANode: TdxXMLNode; ADefaultFontHandle: TdxSpreadSheetFontHandle = nil): TdxSpreadSheetSharedString;
begin
  if ANode.Count > 0 then
    Result := ReadFormattedSharedString(ANode, ADefaultFontHandle)
  else
    Result := TdxSpreadSheetSharedString.CreateObject(ANode.TextAsString);
end;

procedure TdxSpreadSheetXMLSSTableViewReader.ReadProperties(ANode: TdxXMLNode);
begin
  if ANode.HasAttribute(sXMLSSAttrDefaultColumnWidth) then
    View.Columns.DefaultSize := ConvertSize(ANode.Attributes.GetValueAsFloat(sXMLSSAttrDefaultColumnWidth));
  if ANode.HasAttribute(sXMLSSAttrDefaultRowHeight) then
    View.Rows.DefaultSize := ConvertSize(ANode.Attributes.GetValueAsFloat(sXMLSSAttrDefaultRowHeight));
end;

function TdxSpreadSheetXMLSSTableViewReader.ConvertSize(const AValue: Double): Integer;
begin
  Result := Round(AValue / 0.75);
end;

function TdxSpreadSheetXMLSSTableViewReader.GetFonts: TdxSpreadSheetFonts;
begin
  Result := Owner.CellStyles.Fonts;
end;

function TdxSpreadSheetXMLSSTableViewReader.GetOwner: TdxSpreadSheetXMLSSReader;
begin
  Result := TdxSpreadSheetXMLSSReader(inherited Owner);
end;

{ TdxSpreadSheetXMLSSWriter }

constructor TdxSpreadSheetXMLSSWriter.Create(AOwner: TdxCustomSpreadSheet; ADocument: TdxXMLDocument);
begin
  inherited Create(AOwner, nil);
  Initialize(ADocument, False);
end;

constructor TdxSpreadSheetXMLSSWriter.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
begin
  inherited Create(AOwner, AStream);
  Initialize(TdxXMLDocument.Create(nil), True);
end;

destructor TdxSpreadSheetXMLSSWriter.Destroy;
begin
  if FXMLIsOwn then
    FreeAndNil(FXML);
  FreeAndNil(FStyles);
  inherited Destroy;
end;

procedure TdxSpreadSheetXMLSSWriter.WriteData;
var
  ANode: TdxXMLNode;
  ASavedFormatSettings: TFormatSettings;
  ASavedR1C1Reference: Boolean;
  AStylesNode: TdxXMLNode;
  I: Integer;
begin
  ASavedR1C1Reference := SpreadSheet.OptionsView.R1C1Reference;
  ASavedFormatSettings := SpreadSheet.FormulaController.FormatSettings.Data;
  SpreadSheet.BeginUpdate;
  try
    SpreadSheet.OptionsView.R1C1Reference := True;
    dxGetLocaleFormatSettings(dxGetInvariantLocaleID, SpreadSheet.FormulaController.FormatSettings.Data);

    ANode := XML.AddChild(sXMLSSNodeWorkbook);
    ANode.Attributes.Add('xmlns', 'urn:schemas-microsoft-com:office:spreadsheet');
    ANode.Attributes.Add('xmlns:o', 'urn:schemas-microsoft-com:office:office');
    ANode.Attributes.Add('xmlns:x', 'urn:schemas-microsoft-com:office:excel');
    ANode.Attributes.Add('xmlns:ss', 'urn:schemas-microsoft-com:office:spreadsheet');
    ANode.Attributes.Add('xmlns:html', HTMLNamespace);

    AStylesNode := ANode.AddChild(sXMLSSNodeStyles);
    WriteNames(ANode);
    WriteViews(ANode);
    for I := 0 to Styles.Count - 1 do
      WriteStyle(AStylesNode.AddChild(sXMLSSNodeStyle), Styles[I], I);
  finally
    SpreadSheet.FormulaController.FormatSettings.Data := ASavedFormatSettings;
    SpreadSheet.OptionsView.R1C1Reference := ASavedR1C1Reference;
    TdxSpreadSheetAccess(SpreadSheet).Changes := TdxSpreadSheetAccess(SpreadSheet).Changes - [sscData];
    SpreadSheet.EndUpdate;
  end;

  if Stream <> nil then
    XML.SaveToStream(Stream);
end;

function TdxSpreadSheetXMLSSWriter.CanWriteFormula(AFormula: TdxSpreadSheetFormula): Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetXMLSSWriter.CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
begin
  Result := TdxSpreadSheetCustomFilerProgressHelper.Create(Self, 1);
end;

procedure TdxSpreadSheetXMLSSWriter.Initialize(ADoc: TdxXMLDocument; AIsOwnDoc: Boolean);
begin
  FStyles := TList<TdxSpreadSheetCellStyleHandle>.Create;
  FXML := ADoc;
  FXML.AutoIndent := False;
  FXMLIsOwn := AIsOwnDoc;
end;

procedure TdxSpreadSheetXMLSSWriter.WriteCell(ANode: TdxXMLNode;
  ACell: TdxSpreadSheetCell; AStyleID: Integer; const AArea, AMergedArea: TRect);
var
  AComment: TdxSpreadSheetCommentContainer;
  AFormula: TdxSpreadSheetFormula;
  AHyperlink: TdxSpreadSheetHyperlink;
begin
  ANode.Attributes.Add(sXMLSSAttrStyleID, Format(sXMLSSStyleNameTemplate, [AStyleID]));
  ANode.Attributes.Add(sXMLSSAttrIndex, ACell.ColumnIndex - AArea.Left + 1);
  if AMergedArea.Width > 0 then
    ANode.Attributes.Add(sXMLSSAttrMergedAcross, AMergedArea.Width);
  if AMergedArea.Height > 0 then
    ANode.Attributes.Add(sXMLSSAttrMergedDown, AMergedArea.Height);

  if ACell.DataType = cdtFormula then
  begin
    AFormula := ACell.AsFormula;
    if CanWriteFormula(AFormula) then
    begin
      if AFormula.IsArrayFormula then
      begin
        ANode.Attributes.Add(sXMLSSAttrArrayRange,
          dxReferenceToString(cxRect(0, 0, AFormula.ArrayFormulaSize.cx - 1, AFormula.ArrayFormulaSize.cy - 1), True));
      end;
      ANode.Attributes.Add(sXMLSSAttrFormula, AFormula.AsText);
    end;
  end;

  AHyperlink := ACell.View.Hyperlinks.FindItem(ACell.RowIndex, ACell.ColumnIndex);
  if (AHyperlink <> nil) and (AHyperlink.ValueType <> hvtReference) then
  begin
    ANode.Attributes.Add(sXMLSSAttrHRef, AHyperlink.Value);
    ANode.Attributes.Add(sXMLSSAttrHRefScreenTip, AHyperlink.ScreenTip);
  end;

  if ACell.DataType <> cdtBlank then
    WriteCellData(ANode, ACell);

  if ACell.View.Containers.FindCommentContainer(ACell, AComment) then
    WriteComment(ANode.AddChild(sXMLSSNodeComment), AComment);
end;

procedure TdxSpreadSheetXMLSSWriter.WriteCellData(ACellNode: TdxXMLNode; ACell: TdxSpreadSheetCell);
var
  AActualDataType: TdxSpreadSheetCellDataType;
  ANode: TdxXMLNode;
begin
  AActualDataType := ACell.DataType;
  if AActualDataType = cdtString then
  begin
    WriteString(ACellNode, ACell.AsSharedString);
    Exit;
  end;

  if AActualDataType = cdtFormula then
  begin
    if TdxSpreadSheetFormulaAccess(ACell.AsFormula).ActualErrorCode <> ecNone then
      AActualDataType := cdtError
    else
      AActualDataType := dxGetDataTypeByVariantValue(ACell.AsVariant);
  end;

  ANode := ACellNode.AddChild(sXMLSSNodeData);
  ANode.Attributes.Add(sXMLSSAttrType, CellDataTypeMap[AActualDataType]);
  case AActualDataType of
    cdtDateTime:
      ANode.TextAsDateTime := ACell.AsDateTime;
    cdtError:
      ANode.TextAsString := dxSpreadSheetErrorCodeToString(ACell.AsError);
    cdtFloat, cdtCurrency:
      ANode.TextAsString := dxFloatToStr(ACell.AsVariant, dxInvariantFormatSettings);
    cdtBoolean:
      ANode.TextAsString := IntToStr(Ord(ACell.AsBoolean));
  else
    ANode.TextAsString := ACell.AsVariant;
  end;
end;

procedure TdxSpreadSheetXMLSSWriter.WriteColumns(AParentNode: TdxXMLNode; AView: TdxSpreadSheetTableView; AArea: TRect);
begin
  TdxSpreadSheetTableColumnsAccess(AView.Columns).ForEach(
    procedure (AItem: TdxDynamicListItem)
    var
      ANode: TdxXMLNode;
    begin
      ANode := AParentNode.AddChild(sXMLSSNodeColumn);
      ANode.Attributes.Add(sXMLSSAttrIndex, AItem.Index - AArea.Left + 1);
      ANode.Attributes.Add(sXMLSSAttrAutoFitWidth, 0);
      if not TdxSpreadSheetTableColumn(AItem).DefaultSize then
        ANode.Attributes.Add(sXMLSSAttrWidth, ConvertSize(TdxSpreadSheetTableColumn(AItem).Size));
    end,
    AArea.Left, AArea.Right);
end;

procedure TdxSpreadSheetXMLSSWriter.WriteComment(ANode: TdxXMLNode; AComment: TdxSpreadSheetCommentContainer);
var
  AStyle: TFontStyle;
begin
  ANode.Attributes.Add(sXMLSSAttrAuthor, AComment.Author);
  ANode.Attributes.Add(sXMLSSAttrShowAlways, AComment.Visible);

  ANode := ANode.AddChild(sXMLSSNodeCommentData);
  ANode.Attributes.Add('xmlns', HTMLNamespace);
  for AStyle := Low(TFontStyle) to High(TFontStyle) do
  begin
    if AStyle in AComment.TextBox.Font.Style then
      ANode := ANode.AddChild(FontStyleMap[AStyle]);
  end;
  ANode := ANode.AddChild(sXMLSSNodeFont);
  ANode.Attributes.Add(sHTMLAttrFace, AComment.TextBox.Font.Name);
  ANode.Attributes.Add(sHTMLAttrSize, AComment.TextBox.Font.Size);
  ANode.Attributes.Add(sHTMLAttrColor, ConvertColor(AComment.TextBox.Font.Color));
  ANode.Attributes.Add(sXMLSSAttrCharset, AComment.TextBox.Font.Charset);
  ANode.TextAsString := AComment.TextBox.TextAsString;
end;

procedure TdxSpreadSheetXMLSSWriter.WriteNames(AParentNode: TdxXMLNode);
var
  ANode: TdxXMLNode;
  I: Integer;
begin
  if SpreadSheet.DefinedNames.Count > 0 then
  begin
    AParentNode := AParentNode.AddChild(sXMLSSNodeNames);
    for I := 0 to SpreadSheet.DefinedNames.Count - 1 do
    begin
      ANode := AParentNode.AddChild(sXMLSSNodeNamedRange);
      ANode.Attributes.Add(sXMLSSAttrName, SpreadSheet.DefinedNames[I].Caption);
      ANode.Attributes.Add(sXMLSSAttrReferTo, SpreadSheet.DefinedNames[I].Reference);
    end;
  end;
end;

procedure TdxSpreadSheetXMLSSWriter.WriteRows(AParentNode: TdxXMLNode; AView: TdxSpreadSheetTableView; AArea: TRect);
begin
  ProgressHelper.BeginStage(AArea.Height);
  try
    TdxSpreadSheetTableRowsAccess(AView.Rows).ForEach(
      procedure(AItem: TdxDynamicListItem)
      var
        AMergedArea: TRect;
        ANode: TdxXMLNode;
        ARow: TdxSpreadSheetTableRowAccess;
      begin
        ARow := TdxSpreadSheetTableRowAccess(AItem);
        ANode := AParentNode.AddChild(sXMLSSNodeRow);
        ANode.Attributes.Add(sXMLSSAttrIndex, ARow.Index - AArea.Top + 1);
        if not ARow.DefaultSize then
          ANode.Attributes.Add(sXMLSSAttrHeight, ConvertSize(ARow.Size));
        ANode.Attributes.Add(sXMLSSAttrAutoFitHeight, not ARow.IsCustomSize);
        ProgressHelper.SetTaskNumber(ARow.Index - AArea.Top);

        TdxSpreadSheetTableRowCellsAccess(ARow.RowCells).ForEach(
          procedure(AItem: TdxDynamicListItem)
          var
            ACell: TdxSpreadSheetCell;
            AStyleIndex: Integer;
          begin
            ACell := TdxSpreadSheetCell(AItem);
            AMergedArea := AView.MergedCells.ExpandArea(ACell.ColumnIndex, ACell.RowIndex);
            if (AMergedArea.Left = ACell.ColumnIndex) and (AMergedArea.Top = ACell.RowIndex) then
            begin
              AStyleIndex := Styles.IndexOf(ACell.Style.Handle);
              if AStyleIndex < 0 then
                AStyleIndex := Styles.Add(ACell.Style.Handle);
              WriteCell(ANode.AddChild(sXMLSSNodeCell), ACell, AStyleIndex, AArea, AMergedArea);
            end;
          end,
          AArea.Left, AArea.Right);
      end,
      AArea.Top, AArea.Bottom);
  finally
    ProgressHelper.EndStage;
  end;
end;

procedure TdxSpreadSheetXMLSSWriter.WriteString(ACellNode: TdxXMLNode; AString: TdxSpreadSheetSharedString);

  function IsFormattedString(AString: TdxSpreadSheetSharedString): Boolean;
  begin
    Result := (AString is TdxSpreadSheetFormattedSharedString) and (TdxSpreadSheetFormattedSharedString(AString).Runs.Count > 0);
  end;

  procedure WriteStringRun(ANode: TdxXMLNode; AStartIndex, ANextRunIndex: Integer; AFontHandle: TdxSpreadSheetFontHandle = nil);
  var
    AStyle: TFontStyle;
  begin
    if AStartIndex >= ANextRunIndex then
      Exit;
    if AFontHandle <> nil then
    begin
      for AStyle := Low(TFontStyle) to High(TFontStyle) do
      begin
        if AStyle in AFontHandle.Style then
          ANode := ANode.AddChild(FontStyleMap[AStyle]);
      end;
      ANode := ANode.AddChild(sXMLSSNodeFont);
      ANode.Attributes.Add(sHTMLAttrFace, AFontHandle.Name);
      ANode.Attributes.Add(sHTMLAttrSize, AFontHandle.Size);
      ANode.Attributes.Add(sHTMLAttrColor, ConvertColor(AFontHandle.Color));
    end;
    ANode.TextAsString := Copy(AString.Value, AStartIndex, ANextRunIndex - AStartIndex);
  end;

var
  ANode: TdxXMLNode;
  ARun: TdxSpreadSheetFormattedSharedStringRun;
  ARuns: TdxSpreadSheetFormattedSharedStringRuns;
  I: Integer;
begin
  if IsFormattedString(AString) then
  begin
    ANode := ACellNode.AddChild(sXMLSSNodeData2);
    ANode.Attributes.Add(sXMLSSAttrType, CellDataTypeMap[cdtString]);
    ANode.Attributes.Add('xmlns', HTMLNamespace);

    ARuns := TdxSpreadSheetFormattedSharedString(AString).Runs;
    WriteStringRun(ANode, 1, ARuns[0].StartIndex);

    for I := 1 to ARuns.Count - 1 do
    begin
      ARun := ARuns[I - 1];
      WriteStringRun(ANode, ARun.StartIndex, ARuns[I].StartIndex, ARun.FontHandle);
    end;

    ARun := ARuns[ARuns.Count - 1];
    WriteStringRun(ANode, ARun.StartIndex, Length(AString.Value) + 1, ARun.FontHandle);
  end
  else
  begin
    ANode := ACellNode.AddChild(sXMLSSNodeData);
    ANode.Attributes.Add(sXMLSSAttrType, CellDataTypeMap[cdtString]);
    ANode.TextAsString := AString.Value;
  end;
end;

procedure TdxSpreadSheetXMLSSWriter.WriteStyle(ANode: TdxXMLNode; AStyle: TdxSpreadSheetCellStyleHandle; AStyleID: Integer);
var
  ASubNode: TdxXMLNode;
begin
  ANode.Attributes.Add(sXMLSSAttrID, Format(sXMLSSStyleNameTemplate, [AStyleID]));
  WriteStyleAlignment(ANode.AddChild(sXMLSSNodeAlignment), AStyle);
  WriteStyleBorders(ANode.AddChild(sXMLSSNodeBorders), AStyle.Borders);
  WriteStyleFont(ANode.AddChild(sXMLSSNodeFont), AStyle.Font);
  WriteStyleBrush(ANode.AddChild(sXMLSSNodeInterior), AStyle.Brush);
  WriteStyleNumberFormat(ANode.AddChild(sXMLSSNodeNumberFormat), AStyle.DataFormat);

  ASubNode := ANode.AddChild(sXMLSSNodeProtection);
  ASubNode.Attributes.Add(sXMLSSAttrProtected, csLocked in AStyle.States);
end;

procedure TdxSpreadSheetXMLSSWriter.WriteStyleAlignment(ANode: TdxXMLNode; AStyle: TdxSpreadSheetCellStyleHandle);
begin
  ANode.Attributes.Add(sXMLSSAttrHorizontal, HorzAttrValuesMap[AStyle.AlignHorz]);
  if AStyle.AlignHorzIndent <> 0 then
    ANode.Attributes.Add(sXMLSSAttrIndent, AStyle.AlignHorzIndent);
  ANode.Attributes.Add(sXMLSSAttrVertical, VertAttrValuesMap[AStyle.AlignVert]);
  ANode.Attributes.Add(sXMLSSAttrShrinkToFit, csShrinkToFit in AStyle.States);
  ANode.Attributes.Add(sXMLSSAttrWrapText, csWordWrap in AStyle.States);
end;

procedure TdxSpreadSheetXMLSSWriter.WriteStyleBorders(ANode: TdxXMLNode; AStyle: TdxSpreadSheetBordersHandle);
var
  ASide: TcxBorder;
  AStyleInfo: TdxXMLSSLineStyleInfo;
  ASubNode: TdxXMLNode;
begin
  for ASide := Low(TcxBorder) to High(TcxBorder) do
  begin
    ASubNode := ANode.AddChild(sXMLSSNodeBorder);
    if cxColorIsValid(AStyle.BorderColor[ASide]) then
      ASubNode.Attributes.Add(sXMLSSAttrColor, ConvertColor(AStyle.BorderColor[ASide]));
    AStyleInfo := LineStyleMap[AStyle.BorderStyle[ASide]];
    if AStyleInfo.Name <> '' then
      ASubNode.Attributes.Add(sXMLSSAttrLineStyle, AStyleInfo.Name);
    if AStyleInfo.Weight > 0 then
      ASubNode.Attributes.Add(sXMLSSAttrWeight, AStyleInfo.Weight);
    if ASubNode.Attributes.Count > 0 then
      ASubNode.Attributes.Add(sXMLSSAttrPosition, BordersNames[ASide]);
  end;
end;

procedure TdxSpreadSheetXMLSSWriter.WriteStyleBrush(ANode: TdxXMLNode; AStyle: TdxSpreadSheetBrushHandle);
begin
  if cxColorIsValid(AStyle.BackgroundColor) then
  begin
    ANode.Attributes.Add(sXMLSSAttrColor, ConvertColor(AStyle.BackgroundColor));
    ANode.Attributes.Add(sXMLSSAttrPattern, CellFillStyleNames[AStyle.Style]);
    if AStyle.Style <> sscfsSolid then
      ANode.Attributes.Add(sXMLSSAttrPatternColor, ConvertColor(AStyle.ForegroundColor));
  end;
end;

procedure TdxSpreadSheetXMLSSWriter.WriteStyleFont(ANode: TdxXMLNode; AStyle: TdxSpreadSheetFontHandle);
begin
  ANode.Attributes.Add(sXMLSSAttrFontName, AStyle.Name);
  ANode.Attributes.Add(sXMLSSAttrSize, AStyle.Size);
  ANode.Attributes.Add(sXMLSSAttrColor, ConvertColor(AStyle.Color));
  ANode.Attributes.Add(sXMLSSAttrCharset, AStyle.Charset);

  if fsBold in AStyle.Style then
    ANode.Attributes.Add(sXMLSSAttrBold, 1);
  if fsItalic in AStyle.Style then
    ANode.Attributes.Add(sXMLSSAttrItalic, 1);
  if fsStrikeOut in AStyle.Style then
    ANode.Attributes.Add(sXMLSSAttrStrikeThrough, 1);
  if fsUnderline in AStyle.Style then
    ANode.Attributes.Add(sXMLSSAttrUnderline, sXMLSSValueSingle);
end;

procedure TdxSpreadSheetXMLSSWriter.WriteStyleNumberFormat(ANode: TdxXMLNode; AStyle: TdxSpreadSheetFormatHandle);
var
  APredefinedFormat: TdxSpreadSheetFormatHandle;
begin
  APredefinedFormat := SpreadSheet.CellStyles.Formats.PredefinedFormats.GetFormatHandleByID(AStyle.FormatCodeID);
  if APredefinedFormat <> nil then
    AStyle := APredefinedFormat;
  if AStyle.FormatCode <> '' then
    ANode.Attributes.Add(sXMLSSAttrFormat, AStyle.FormatCode);
end;

procedure TdxSpreadSheetXMLSSWriter.WriteView(AParentNode: TdxXMLNode; AView: TdxSpreadSheetTableView; const AArea: TRect);
var
  ANode: TdxXMLNode;
begin
  // Worksheet
  ANode := AParentNode.AddChild(sXMLSSNodeWorksheet);
  ANode.Attributes.Add(sXMLSSAttrName, AView.Caption);

  // Table
  ANode := ANode.AddChild(sXMLSSNodeTable);
  ANode.Attributes.Add(sXMLSSAttrExpandedColumnCount, AArea.Width + 1);
  ANode.Attributes.Add(sXMLSSAttrExpandedRowCount, AArea.Height + 1);
  ANode.Attributes.Add(sXMLSSAttrDefaultColumnWidth, ConvertSize(AView.Columns.DefaultSize));
  ANode.Attributes.Add(sXMLSSAttrDefaultRowHeight, ConvertSize(AView.Rows.DefaultSize));
  WriteColumns(ANode, AView, AArea);
  WriteRows(ANode, AView, AArea);
end;

procedure TdxSpreadSheetXMLSSWriter.WriteViews(AParentNode: TdxXMLNode);
var
  AView: TdxSpreadSheetCustomView;
  I: Integer;
begin
  for I := 0 to SpreadSheet.SheetCount - 1 do
  begin
    AView := SpreadSheet.Sheets[I];
    if AView is TdxSpreadSheetTableView then
      WriteView(AParentNode, TdxSpreadSheetTableView(AView), TdxSpreadSheetTableView(AView).Dimensions);
  end;
end;

function TdxSpreadSheetXMLSSWriter.ConvertColor(AValue: TColor): string;
begin
  Result := '#' + TdxColorHelper.AlphaColorToHexCode(dxColorToAlphaColor(AValue), False, False);
end;

function TdxSpreadSheetXMLSSWriter.ConvertSize(AValue: Integer): Double;
begin
  Result := AValue * 0.75;
end;

end.
