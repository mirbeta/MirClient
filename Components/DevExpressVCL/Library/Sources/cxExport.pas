{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressExport                                            }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEXPORT AND ALL                 }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE       }
{   PROGRAM ONLY.                                                    }
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

unit cxExport;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
{$IFNDEF NONDB}
  DB, FMTBcd, SqlTimSt,
{$ENDIF}
  Windows, Classes, SysUtils, Math, Graphics, Variants, Generics.Defaults, Generics.Collections, ImgList,
  dxCore, dxGdiPlusClasses, cxGraphics, cxClasses, cxGeometry, cxExportStrs, dxSpreadSheet, cxEdit,
  dxSpreadSheetConditionalFormatting, dxCoreClasses;

resourcestring
  cxExportListIndexError = 'List index out of bounds (%d)';

type
  EcxExportData = class(EdxException);

  { Define  CX  style items }

  TcxAlignText = (catLeft, catCenter, catRight);
  TcxAlignTextVert = (atvDefault, atvTop, atvCenter, atvBottom);

  TcxBrushStyle = (cbsClear, cbsSolid);
  TcxValueDisplayFormatType = (vdftAuto, vdftNumeric, vdftDateTime, vdftRTF);

  { TcxCellBorders }

  TcxCellBorders = packed record
    IsDefault: Boolean;
    Width: Integer;
    Color: Integer;
  end;

  { TcxCacheCellStyle }

  PcxCacheCellStyle = ^TcxCacheCellStyle;
  TcxCacheCellStyle = packed record
    AlignText: TcxAlignText;
    AlignTextVert: TcxAlignTextVert;
    Borders: array[0..3] of TcxCellBorders;
    BrushBkColor: Integer;
    BrushFgColor: Integer;
    BrushStyle: TcxBrushStyle;
    FontCharset: Integer;
    FontColor: Integer;
    FontName: TFontName;
    FontSize: Integer;
    FontStyle: TFontStyles;
    Reference: TObject;
    SingleLine: Boolean;
  end;

  TcxExportCustomItem = class
  public
    Bounds: TRect;
    Borders: TcxBorders;
    Style: Integer;
  end;

  TcxExportDataItem = class(TcxExportCustomItem)
  public
    DataType: Byte;
    DataFormat: TObject;
    Value: Double;
  end;

  TcxExportDataItemClass = class of TcxExportCustomItem;

  TcxEnumExportTypes = procedure (const AExportType: Integer; const AExportName: string);
  TcxEnumTypes = procedure (const AExportType: Integer);

  { IcxExportBeforeSave }

  IcxExportBeforeSave = interface
  ['{607B31EF-A10A-4CCC-BEAE-1A708327A61A}']
    procedure OnBeforeSave(Sender: TdxSpreadSheet);
  end;

  { IcxExportProgress }

  IcxExportProgress = interface(IcxProgress)
  ['{E1416589-62FF-4D57-B198-3510CC9DBF5A}']
  end;

  { IcxExportProvider }

  IcxExportProvider = interface
  ['{442A08A8-CDDA-4FD6-8E15-9D8BD34554F6}']
    procedure AddConditionalFormattingRule(ARule: TdxSpreadSheetCustomConditionalFormattingRule; AAreas: TdxRectList);
    procedure AddOutlineGroup(AStart, AFinish: Integer);
    procedure AddSharedImageHandle(AImageList: TObject; AImageIndex: Integer; AHandle: TObject);
    procedure ApplyBestFit;
    procedure Commit(AProgressHelper: TcxCustomProgressCalculationHelper; AHandler: TObject);
    procedure FreezePanes(ACol, ARow: Integer);
    function GetFormatCode(ACol, ARow: Integer): string;
    function GetStyle(AStyleIndex: Integer): TcxCacheCellStyle;
    function RegisterStyle(const AStyle: TcxCacheCellStyle): Integer;
    function SetCellGraphic(const AArea: TRect; AStyleIndex: Integer; AGraphic: TGraphic; AFitMode: TcxImageFitMode): TObject;
    procedure SetCellGraphicAsSharedHandle(const AArea: TRect; AStyleIndex: Integer; AHandle: TObject; AFitMode: TcxImageFitMode);
    procedure SetCellPNGImage(const ACol, ARow: Integer; APNGImage: TGraphic; AFitMode: TcxImageFitMode);
    procedure SetCellStyle(const ACol, ARow, AStyleIndex: Integer); overload;
    procedure SetCellStyle(const AArea: TRect; AStyleIndex: Integer); overload;
    procedure SetCellUnion(const ACol, ARow: Integer; H, W: Integer);
    procedure SetCellValue(const ACol, ARow: Integer; const AValue: Variant;
      const ADisplayFormat: string = ''; ADisplayFormatType: TcxValueDisplayFormatType = vdftAuto);
    procedure SetCellValueAsFormula(const ACol, ARow: Integer; const AValue: string; ADisplayText: string = '';
      AFormatCode: string = ''; AListSeparator: Char = ',');
    procedure SetCellValueAsString(const ACol, ARow: Integer; const AText: string);
    procedure SetColumnWidth(const ACol, AWidth: Integer);
    procedure SetDefaultStyle(const AStyle: TcxCacheCellStyle);
    procedure SetRange(const AColCount, ARowCount: Integer; IsVisible: Boolean = True);
    procedure SetRowHeight(const ARow, AHeight: Integer);
    function TryGetSharedImageHandle(AImageList: TObject; AImageIndex: Integer; var AHandle: TObject): Boolean;
    function SupportGraphic: Boolean;
    function SupportRTF: Boolean;
  end;

  IcxExportProvider2 = interface(IcxExportProvider)
  ['{B2DBA469-70C9-46D1-B015-567F545385FF}']

    procedure AddGraphic(AItem: TcxExportDataItem; AGraphic: TGraphic; AFitMode: TcxImageFitMode);
    procedure AddImageListItem(AItem: TcxExportDataItem; AImages: TCustomImageList; AImageIndex: Integer; ABkColor: TColor);
    procedure AddText(AItem: TcxExportDataItem; const AText: string);
    procedure AddValue(AItem: TcxExportDataItem; const AValue: Variant; AProperties: TcxCustomEditProperties;
      const AValueDisplayFormat: string = ''; AValueDisplayFormatType: TcxValueDisplayFormatType = vdftAuto);
    procedure SetData(AItem: TcxExportDataItem);
  end;

  { IcxExportWithSeparators }

  IcxExportWithSeparators = interface
  ['{0E2919A6-8B49-49D7-B55B-B44B6DECF2E5}']
    procedure AddSeparator(const ASeparator: string);
  end;

  { IcxNameExportProvider }

  IcxNameExportProvider = interface
  ['{FC69194E-E3C7-41F4-98AE-5948813210AE}']
    procedure SetName(const AName: string);
    procedure SetRangeName(const AName: string; const ARange: TRect);
  end;

  { IcxExportProviderEncoding }

  IcxExportProviderEncoding = interface
  ['{B18118E5-37B7-4EF2-B246-162169C900A8}']
    procedure SetEncoding(AEncoding: TEncoding);
  end;

  { TcxCustomExportProvider }

  TcxExportProviderClass = class of TcxCustomExportProvider;

  TcxCustomExportProvider = class(TInterfacedObject)
  strict private
    FFileName: string;
  protected
    procedure Clear; dynamic;
    //
    property FileName: string read FFileName;
  public
    constructor Create(const AFileName: string); virtual;
    procedure BeforeDestruction; override;
    class function ExportType: Integer; virtual;
    class function ExportName: string; virtual;
  end;

  { TcxExport }

  TcxExport = class
  protected
    class function GetExportClassByType(AExportType: Integer): TcxExportProviderClass; virtual;
  public
    class function Provider(AExportType: Integer; const AFileName: string): TcxCustomExportProvider;
    class procedure SupportExportTypes(EnumSupportTypes: TcxEnumExportTypes);
    class procedure SupportTypes(EnumFunc: TcxEnumTypes);
    class function RegisterProviderClass(AProviderClass: TcxExportProviderClass): Boolean;
  end;

  { TcxExportIntList }

  TcxExportIntList = class(TList)
  strict private
    function GetItem(AIndex: Integer): TdxNativeInt; inline;
    procedure SetItem(AIndex: Integer; AValue: TdxNativeInt); inline;
  public
    procedure Add(AValue: TdxNativeInt);
    procedure AddPairs(AValue1, AValue2: TdxNativeInt);
    function Last: TdxNativeInt;
    function First: TdxNativeInt;

    property Items[Index: Integer]: TdxNativeInt read GetItem write SetItem; default;
  end;

  { TcxExportScale }

  TcxExportScale = class(TcxExportIntList)
  strict private
    class function CompareValues(AItem1, AItem2: Pointer): Integer; static;
    function GetDelta(AIndex: Integer): Integer;
    function GetVisibleCount: Integer;
  public
    procedure Arrange;
    function IndexOf(AItem: Integer): Integer;
    function IndexOfEx(AValue, AFirstIndex, ALastIndex: Integer): Integer;
    procedure GetPosition(AValue1, AValue2: Integer; out AIndex1, AIndex2: Integer);
    procedure GetPositionEx(AValue1, AValue2, AFirstIndex, ALastIndex: Integer; out AIndex1, AIndex2: Integer);

    property Delta[Index: Integer]: Integer read GetDelta;
    property VisibleCount: Integer read GetVisibleCount;
  end;

  { TdxExportAxis }

  TdxExportAxis = class
  private
    FDictionary: TDictionary<Integer, Integer>;
    FIsDirty: Boolean;
    FValues: TList;
    function GetCount: Integer; inline;
    function GetItem(AIndex: Integer): Integer; inline;
    function GetSize(AIndex: Integer): Integer;
  protected
    procedure Calculate;

    property Dictionary: TDictionary<Integer, Integer> read FDictionary write FDictionary;
    property IsDirty: Boolean read FIsDirty write FIsDirty;
    property Values: TList read FValues;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AValue: Integer); inline;

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: Integer read GetItem; default;
    property Size[AIndex: Integer]: Integer read GetSize;
  end;

  { TdxExportArea }

  TdxExportArea = class
  private
    FAxisX: TdxExportAxis;
    FAxisY: TdxExportAxis;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const P: TPoint); overload;
    procedure Add(const R: TRect); overload;

    function BoundsToPosition(const ABounds: TRect): TRect;
    function PointToPosition(const P: TPoint): TPoint;

    property AxisX: TdxExportAxis read FAxisX;
    property AxisY: TdxExportAxis read FAxisY;
  end;

const
  cxExportToBinary = Integer($FFFFFFFF);
  cxExportToExcel  = Integer($00000001);
  cxExportToHtml   = Integer($00000002);
  cxExportToXml    = Integer($00000003);
  cxExportToText   = Integer($00000004);
  cxExportToXlsx   = Integer($00000008);
  cxExportToCSV    = Integer($00000009);

  cxExportCurrencyFormat: string = '';
  cxExportGraphicClass: TGraphicClass = TBitmap;

var
  DefaultCellStyle: TcxCacheCellStyle;
  cxBlackColor: Integer;
  cxWindowColor: Integer;
  cxBtnTextColor: Integer;
  cxBtnShadowColor: Integer;
  cxBtnFaceColor: Integer;

function cxGetDisplayFormat(AEditProperties: TcxCustomEditProperties; AField: TField = nil): string;
function cxGetDisplayFormatType(AEditProperties: TcxCustomEditProperties; AField: TField = nil): TcxValueDisplayFormatType;
function cxGetFieldDisplayFormat(AField: TField): string;
function cxStrUnicodeNeeded(const AText: string; ACheckNormal: Boolean = False): Boolean;
function cxValidateFileName(const AFileName: string): string;

function IsRTFSupported: Boolean; overload;
function IsRTFSupported(AProperties: TcxCustomEditProperties): Boolean; overload;

function SupportGraphic(AGraphic: TGraphic): Boolean; overload;
function SupportGraphic(AGraphicClass: TGraphicClass): Boolean; overload;

function GraphicToPNGImage(const AClientRect, ADrawRect: TRect; AGraphic: TGraphic): TdxPNGImage;

implementation

uses
  Types, cxFormats, dxCoreGraphics, cxExportProviders, cxTextEdit, cxTimeEdit, cxCalendar, StrUtils,
  dxSpreadSheetCore, cxRichEdit, dxThreading;

type
  TcxCustomTextEditPropertiesAccess = class(TcxCustomTextEditProperties);

var
  RegisteredClasses: array of TcxExportProviderClass;

function CreateDefaultCellStyle: TcxCacheCellStyle;
var
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.AlignText := catCenter;
  Result.FontName := 'Tahoma';
  Result.FontStyle := [];
  Result.FontColor := cxBtnTextColor;
  Result.FontSize := 8;
  Result.FontCharSet := 0;
  for I := 0 to 3 do
  begin
    Result.Borders[I].IsDefault := False;
    Result.Borders[I].Width := 1;
    Result.Borders[I].Color := cxBtnShadowColor;
  end;
  Result.BrushStyle := cbsSolid;
  Result.BrushBkColor := cxWindowColor;
  Result.BrushFgColor := cxBlackColor;
end;

function GetCurrencyFormat: string;

  function GetPositiveCurrencyFormat(const AFormat, ACurrStr: string): string;
  begin
    if Length(ACurrStr) > 0 then
      case dxFormatSettings.CurrencyFormat of
        0: Result := ACurrStr + AFormat; { '$1' }
        1: Result := AFormat + ACurrStr; { '1$' }
        2: Result := ACurrStr + ' ' + AFormat; { '$ 1' }
        3: Result := AFormat + ' ' + ACurrStr; { '1 $' }
      end;
  end;

  function GetNegativeCurrencyFormat(const AFormat, ACurrStr: string): string;
  begin
    case dxFormatSettings.NegCurrFormat of
      0: Result := '(' + ACurrStr + AFormat + ')';
      1: Result := '-' + ACurrStr + AFormat;
      2: Result := ACurrStr + '-' + AFormat;
      3: Result := ACurrStr + AFormat + '-';
      4: Result := '(' + AFormat + ACurrStr + ')';
      5: Result := '-' + AFormat + ACurrStr;
      6: Result := AFormat + '-' + ACurrStr;
      7: Result := AFormat + ACurrStr + '-';
      8: Result := '-' + AFormat + ' ' + ACurrStr;
      9: Result := '-' + ACurrStr + ' ' + AFormat;
      10: Result := AFormat + ' ' + ACurrStr + '-';
      11: Result := ACurrStr + ' ' + AFormat + '-';
      12: Result := ACurrStr + ' ' + '-' + AFormat;
      13: Result := AFormat + '-' + ' ' + ACurrStr;
      14: Result := '(' + ACurrStr + ' ' + AFormat + ')';
      15: Result := '(' + AFormat + ' ' + ACurrStr + ')';
    end;
  end;

var
  ACurrStr: string;
  I: Integer;
  C: Char;
begin
  if dxFormatSettings.CurrencyDecimals > 0 then
    Result := DupeString('0', dxFormatSettings.CurrencyDecimals)
  else
    Result := '';

  Result := ',0.' + Result;
  ACurrStr := '';
  for I := 1 to Length(dxFormatSettings.CurrencyString) do
  begin
    C := dxFormatSettings.CurrencyString[I];
    if (C = ',') or (C = '.') then
      ACurrStr := ACurrStr + '''' + C + ''''
    else
      ACurrStr := ACurrStr + C;
  end;
  Result := GetPositiveCurrencyFormat(Result, ACurrStr) + ';' + GetNegativeCurrencyFormat(Result, ACurrStr);
end;

function cxGetDisplayFormat(AEditProperties: TcxCustomEditProperties; AField: TField = nil): string;
begin
  Result := '';
  if AEditProperties is TcxTimeEditProperties then
  begin
    Result := cxTimeEditFormats[
      TcxTimeEditProperties(AEditProperties).TimeFormat,
      TcxTimeEditProperties(AEditProperties).Use24HourFormat, 0];
  end;

  if (Result = '') and (AEditProperties is TcxCustomTextEditProperties) then
    Result := TcxCustomTextEditPropertiesAccess(AEditProperties).DisplayFormat;

  if Result = '' then
    Result := cxGetFieldDisplayFormat(AField);

  if (Result = '') and (AEditProperties is TcxDateEditProperties) then
  begin
    if TcxDateEditProperties(AEditProperties).ShowTime then
      Result := 'C'
    else
      Result := dxFormatSettings.ShortDateFormat;
  end;
end;

function cxGetDisplayFormatType(AEditProperties: TcxCustomEditProperties; AField: TField = nil): TcxValueDisplayFormatType;
begin
  if (AEditProperties is TcxTimeEditProperties) or (AEditProperties is TcxDateEditProperties) or
    (AField is TDateTimeField) or (AField is TSQLTimeStampField)
  then
    Result := vdftDateTime
  else
    if IsRTFSupported(AEditProperties) then
      Result := vdftRTF
    else
      Result := vdftAuto;
end;

function cxGetFieldDisplayFormat(AField: TField): string;
begin
  if AField is TNumericField then
    Result := TNumericField(AField).DisplayFormat
  else

  if AField is TDateTimeField then
    Result := TDateTimeField(AField).DisplayFormat
  else

  if AField is TSQLTimeStampField then
    Result := TSQLTimeStampField(AField).DisplayFormat
  else
    Result := '';
end;

function cxValidateFileName(const AFileName: string): string;
begin
  Result := StringReplace(AFileName, '/', '\', [rfReplaceAll])
end;

function cxValidateStr(const AValue: string): string;
var
   I: Integer;
begin
  Result := AValue;
  I := 1;
  while I <= Length(Result) do
  begin
    if Result[I] = #13 then
      Delete(Result, I, 1)
    else
      Inc(I)
  end;
end;

function cxStrUnicodeNeeded(const AText: string; ACheckNormal: Boolean = False): Boolean;
const
  Normal = ['0'..'9', ':', ';', '*', '+', ',', '-', '.', '/', '!', ' ', 'A'..'Z', 'a'..'z', '_', '(', ')'];
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(AText) do
    if (Ord(AText[I]) > $7F) or (ACheckNormal and not dxCharInSet(AText[I], Normal)) then
    begin
      Result := True;
      Break;
    end
end;

function IsRTFSupported: Boolean;
begin
  Result := dxSpreadSheetTextService.IsRTFSupported;
end;

function IsRTFSupported(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := IsRTFSupported and (AProperties is TcxRichEditProperties);
end;

function SupportGraphic(AGraphic: TGraphic): Boolean;
begin
  Result := SupportGraphic(TGraphicClass(AGraphic.ClassType));
end;

function SupportGraphic(AGraphicClass: TGraphicClass): Boolean;
begin
  Result := (AGraphicClass <> nil) and (AGraphicClass.InheritsFrom(TBitmap) or AGraphicClass.InheritsFrom(TMetaFile));
end;

function GraphicToPNGImage(const AClientRect, ADrawRect: TRect; AGraphic: TGraphic): TdxPNGImage;
const
  Transparency: array[Boolean] of Integer = (0, $FF);
var
  ABits: TRGBColors;
  B: TcxBitmap32;
  I: Integer;
  W: Integer;
begin
  if cxRectIsEqual(AClientRect, ADrawRect) and (AGraphic is TBitmap) then
    GetBitmapBits(TBitmap(AGraphic), ABits, True)
  else
  begin
    B := TcxBitmap32.CreateSize(AClientRect, False);
    try
      B.cxCanvas.FillRect(AClientRect, clWhite);
      B.Canvas.StretchDraw(ADrawRect, AGraphic);
      GetBitmapBits(B, ABits, True);
    finally
      B.Free;
    end;
  end;

  W := cxRectWidth(AClientRect);
  for I := 0 to Length(ABits) - 1 do
    ABits[I].rgbReserved := Transparency[PtInRect(ADrawRect, Point(I mod W, I div W))];

  Result := TdxPNGImage.Create;
  Result.LoadFromBits(cxRectWidth(AClientRect), cxRectHeight(AClientRect), ABits, True);
end;

{ TcxExport }

class function TcxExport.Provider(AExportType: Integer; const AFileName: string): TcxCustomExportProvider;
begin
  Result := GetExportClassByType(AExportType).Create(AFileName);
end;

class procedure TcxExport.SupportExportTypes(EnumSupportTypes: TcxEnumExportTypes);
var
  I: Integer;
begin
  for I := 0 to Length(RegisteredClasses) - 1  do
  begin
    with RegisteredClasses[I] do
      EnumSupportTypes(ExportType, ExportName);
  end;
end;

class procedure TcxExport.SupportTypes(EnumFunc: TcxEnumTypes);
var
  I: Integer;
begin
  for I := 0 to Length(RegisteredClasses) - 1 do
    EnumFunc(RegisteredClasses[I].ExportType);
end;

class function TcxExport.RegisterProviderClass(AProviderClass: TcxExportProviderClass): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AProviderClass = nil then
    Exit;
  for I := 0 to Length(RegisteredClasses) - 1 do
  begin
    if (AProviderClass.ExportType = RegisteredClasses[I].ExportType) or (AProviderClass = RegisteredClasses[I]) then
      Exit;
  end;
  I := Length(RegisteredClasses);
  SetLength(RegisteredClasses, I + 1);
  RegisteredClasses[I] := AProviderClass;
  Result := True;
end;

class function TcxExport.GetExportClassByType(AExportType: Integer): TcxExportProviderClass;
var
  I: Integer;
begin
  for I := 0 to Length(RegisteredClasses) - 1 do
  begin
    if RegisteredClasses[I].ExportType = AExportType then
    begin
      Result := RegisteredClasses[I];
      Exit;
    end;
  end;
  raise EcxExportData.CreateFmt(cxGetResourceString(@scxUnsupportedExport), [AExportType]);
end;

{ TcxCustomExportProvider }

constructor TcxCustomExportProvider.Create(const AFileName: string);
begin
  FFileName := cxValidateFileName(AFileName);
end;

procedure TcxCustomExportProvider.BeforeDestruction;
begin
  Clear;
end;

class function TcxCustomExportProvider.ExportType: Integer;
begin
  Result := -1;
end;

class function TcxCustomExportProvider.ExportName: string;
begin
  Result := '';
end;

procedure TcxCustomExportProvider.Clear;
begin
end;

{ TcxExportIntList }

procedure TcxExportIntList.Add(AValue: TdxNativeInt);
begin
  if Capacity - Count < 2 then
    if Count * 2 < 1024 then
      Capacity := 1024
    else
      Capacity := Count  * 2;

  inherited Add(Pointer(AValue));
end;

procedure TcxExportIntList.AddPairs(AValue1, AValue2: TdxNativeInt);
begin
  if Capacity - Count < 2 then
    if Count * 2 < 1024 then
      Capacity := 1024
    else
      Capacity := Count  * 2;

  inherited Add(Pointer(AValue1));
  inherited Add(Pointer(AValue2));
end;

function TcxExportIntList.Last: TdxNativeInt;
begin
  Result := TdxNativeInt(inherited Last);
end;

function TcxExportIntList.First: TdxNativeInt;
begin
  Result := TdxNativeInt(inherited First);
end;

function TcxExportIntList.GetItem(AIndex: Integer): TdxNativeInt;
begin
  Result := TdxNativeInt(List[AIndex]);
end;

procedure TcxExportIntList.SetItem(AIndex: Integer; AValue: TdxNativeInt);
begin
  List[AIndex] := Pointer(AValue);
end;

{ TcxExportScale }

procedure TcxExportScale.Arrange;
var
  AIndex, I: Integer;
begin
  dxQuickSortList(Self, @CompareValues, Count > 10000);

  AIndex := 0;
  for I := 1 to Count - 1 do
  begin
    if List[AIndex] <> List[I] then
      Inc(AIndex);
    List[AIndex] := List[I];
  end;
  if Count > 0 then
    Count := AIndex + 1;
end;

function TcxExportScale.IndexOf(AItem: Integer): Integer;
begin
  Result := IndexOfEx(AItem, 0, VisibleCount);
end;

function TcxExportScale.IndexOfEx(AValue, AFirstIndex, ALastIndex: Integer): Integer;
var
  L, H, I, C: Integer;
begin
  Result := -1;
  // binary search
  L := AFirstIndex;
  H := ALastIndex;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Items[I] - AValue;
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
  if Result = - 1 then
    Error(@cxExportListIndexError, AValue);
end;

procedure TcxExportScale.GetPosition(AValue1, AValue2: Integer; out AIndex1, AIndex2: Integer);
begin
  AIndex1 := IndexOf(AValue1);
  AIndex2 := IndexOfEx(AValue2, AIndex1, Count - 1);
end;

procedure TcxExportScale.GetPositionEx(AValue1, AValue2, AFirstIndex, ALastIndex: Integer; out AIndex1, AIndex2: Integer);
begin
  AIndex1 := IndexOfEx(AValue1, AFirstIndex, ALastIndex);
  AIndex2 := IndexOfEx(AValue2, AIndex1, ALastIndex);
end;

function TcxExportScale.GetVisibleCount: Integer;
begin
  Result := Count;
  Dec(Result);
end;

function TcxExportScale.GetDelta(AIndex: Integer): Integer;
begin
  Result := Items[AIndex + 1] - Items[AIndex];
end;

class function TcxExportScale.CompareValues(AItem1, AItem2: Pointer): Integer;
begin
  Result := TdxNativeInt(AItem1) - TdxNativeInt(AItem2);
end;

{ TdxExportAxis }

constructor TdxExportAxis.Create;
begin
  FDictionary := TDictionary<Integer, Integer>.Create;
  FValues := TList.Create;
end;

destructor TdxExportAxis.Destroy;
begin
  FValues.Free;
  FDictionary.Free;
  inherited Destroy;
end;

procedure TdxExportAxis.Calculate;
var
  I: Integer;
begin
  if not IsDirty then
    Exit;
  Values.Clear;
  Values.Capacity := FDictionary.Count;
  for I in FDictionary.Keys do
    Values.Add(Pointer(I));
  Values.Sort(dxCompareValues);
  for I := 0 to Values.Count - 1 do
    FDictionary[Integer(Values.List[I])] := I;
  IsDirty := False;
end;

function TdxExportAxis.GetCount: Integer;
begin
  Calculate;
  Result := FValues.Count;
end;

function TdxExportAxis.GetItem(AIndex: Integer): Integer;
begin
  if IsDirty then
    Calculate;
  Result := FDictionary.Items[AIndex];
end;

function TdxExportAxis.GetSize(AIndex: Integer): Integer;
begin
  Result := Integer(FValues.List[AIndex + 1]) - Integer(FValues.List[AIndex]);
end;

procedure TdxExportAxis.Add(AValue: Integer);
begin
  FDictionary.AddOrSetValue(AValue, -1);
  FIsDirty := True;
end;

{ TdxExportArea }

constructor TdxExportArea.Create;
begin
  FAxisX := TdxExportAxis.Create;
  FAxisY := TdxExportAxis.Create;
end;

destructor TdxExportArea.Destroy;
begin
  FAxisX.Free;
  FAxisY.Free;
  inherited Destroy;
end;

procedure TdxExportArea.Add(const P: TPoint);
begin
  FAxisX.Add(P.X);
  FAxisY.Add(P.Y);
end;

procedure TdxExportArea.Add(const R: TRect);
begin
  Add(R.TopLeft);
  Add(R.BottomRight);
end;

function TdxExportArea.BoundsToPosition(const ABounds: TRect): TRect;
begin
  Result.TopLeft := PointToPosition(ABounds.TopLeft);
  Result.BottomRight := PointToPosition(ABounds.BottomRight);
end;

function TdxExportArea.PointToPosition(const P: TPoint): TPoint;
begin
  Result.X := FAxisX[P.X];
  Result.Y := FAxisY[P.Y];
end;

initialization
  cxWindowColor := ColorToRGB(clWindow);
  cxBtnTextColor := ColorToRGB(clBtnText);
  cxBtnFaceColor := ColorToRGB(clBtnFace);
  cxBtnShadowColor := ColorToRGB(clBtnShadow);
  cxExportCurrencyFormat := GetCurrencyFormat;
  DefaultCellStyle := CreateDefaultCellStyle;
end.
