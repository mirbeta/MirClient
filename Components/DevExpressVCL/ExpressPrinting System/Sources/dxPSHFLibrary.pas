{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSHFLibrary;

{$I cxVer.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, TypInfo,
  dxCore, cxGeometry, cxClasses,
  cxGraphics, dxPSGlbl, dxPSUtl, dxPSImgs, dxPSRes, dxPSCore, cxDrawTextUtils,
  dxPrnPg, dxPSReportRenderCanvas;

type
  TdxHFFunctionLibrary = class;
  TdxHFFunctionFormatObjectClass = class of TdxHFFunctionFormatObject;

  { TdxHFFunctionFormatObject }

  TdxHFFunctionFormatObject = class(TObject)
  private
    FCurrentPage: Integer;
    FDateFormat: string;
    FDateTime: TDateTime;
    FMachineName: string;
    FPageNumberFormat: TdxPageNumberFormat;
    FStartPageIndex: Integer;
    FTimeFormat: string;
    FTotalPages: Integer;
    FUserName: string;
  protected
    procedure Initialize; virtual;
  public
    constructor Create; virtual;
    property CurrentPage: Integer read FCurrentPage write FCurrentPage;
    property DateFormat: string read FDateFormat write FDateFormat;
    property DateTime: TDateTime read FDateTime write FDateTime;
    property MachineName: string read FMachineName write FMachineName;
    property PageNumberFormat: TdxPageNumberFormat read FPageNumberFormat write FPageNumberFormat;
    property StartPageIndex: Integer read FStartPageIndex write FStartPageIndex;
    property TimeFormat: string read FTimeFormat write FTimeFormat;
    property TotalPages: Integer read FTotalPages write FTotalPages;
    property UserName: string read FUserName write FUserName;
  end;

  { TdxHFFunctionCustomCategory }

  TdxHFFunctionCustomCategory = class(TObject);
  TdxHFFunctionCustomCategoryClass = class of TdxHFFunctionCustomCategory;
  TdxHFFunctionAuthenticationCategory = class(TdxHFFunctionCustomCategory);
  TdxHFFunctionDateTimeCategory = class(TdxHFFunctionCustomCategory);
  TdxHFFunctionImagesCategory = class(TdxHFFunctionCustomCategory);
  TdxHFFunctionPagesCategory = class(TdxHFFunctionCustomCategory);

  { TdxHFCustomFunction }

  TdxHFConvertFunction = function (const Source: string; const AFormatObject: TdxHFFunctionFormatObject): string;
  TdxHFCustomFunctionClass = class of TdxHFCustomFunction;

  TdxHFCustomFunction = class(TPersistent)
  strict private
    FGlyph: TcxBitmap;
    FHint: string;
    FIsHintAssigned: Boolean;
    FIsTemplateStringAssigned: Boolean;
    FTemplateString: string;

    function GetHint: string;
    function GetTemplateString: string;
    procedure SetGlyph(Value: TcxBitmap);
    procedure SetTemplateString(const Value: string);
    procedure SetHint(const Value: string);
  protected
    function ConvertFunc(const Source: string; const AFormatObject: TdxHFFunctionFormatObject): string; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DoProcess(const Source: string; const AFormatObject: TdxHFFunctionFormatObject): string; virtual;
    class function DefaultHintValue: string; virtual;
    class function DefaultTemplateStringValue: string; virtual;
    class function FunctionClass: TdxHFCustomFunctionClass;
    class function GetCategory: TdxHFFunctionCustomCategoryClass; virtual;
    class function GetName: string; virtual;
    //
    property Glyph: TcxBitmap read FGlyph write SetGlyph;
    property Hint: string read GetHint write SetHint;
    property TemplateString: string read GetTemplateString write SetTemplateString;
  end;

  { TdxHFPagesFunctions }

  TdxHFPagesFunctions = class(TdxHFCustomFunction)
  public
    class function GetCategory: TdxHFFunctionCustomCategoryClass; override;
  end;

  { TdxHFPageNumberFunction }

  TdxHFPageNumberFunction = class(TdxHFPagesFunctions)
  protected
    function ConvertFunc(const Source: string; const AFormatObject: TdxHFFunctionFormatObject): string; override;
  public
    constructor Create; override;
    class function DefaultHintValue: string; override;
    class function DefaultTemplateStringValue: string; override;
    class function GetName: string; override;
  end;

  { TdxHFTotalPagesFunction }

  TdxHFTotalPagesFunction = class(TdxHFPagesFunctions)
  protected
    function ConvertFunc(const Source: string; const AFormatObject: TdxHFFunctionFormatObject): string; override;
  public
    constructor Create; override;
    class function DefaultHintValue: string; override;
    class function DefaultTemplateStringValue: string; override;
    class function GetName: string; override;
  end;

  { TdxHFPageOfPagesFunction }

  TdxHFPageOfPagesFunction = class(TdxHFPagesFunctions)
  protected
    function ConvertFunc(const Source: string; const AFormatObject: TdxHFFunctionFormatObject): string; override;
  public
    constructor Create; override;
    class function DefaultHintValue: string; override;
    class function DefaultTemplateStringValue: string; override;
    class function GetName: string; override;
  end;

  { TdxHFAuthenticationFunctions }

  TdxHFAuthenticationFunctions = class(TdxHFCustomFunction)
  public
    class function GetCategory: TdxHFFunctionCustomCategoryClass; override;
  end;

  { TdxHFMachineNameFunction }

  TdxHFMachineNameFunction = class(TdxHFAuthenticationFunctions)
  protected
    function ConvertFunc(const Source: string; const AFormatObject: TdxHFFunctionFormatObject): string; override;
  public
    constructor Create; override;
    class function DefaultHintValue: string; override;
    class function DefaultTemplateStringValue: string; override;
    class function GetName: string; override;
  end;

  { TdxHFUserNameFunction }

  TdxHFUserNameFunction = class(TdxHFAuthenticationFunctions)
  protected
    function ConvertFunc(const Source: string; const AFormatObject: TdxHFFunctionFormatObject): string; override;
  public
    constructor Create; override;
    class function DefaultHintValue: string; override;
    class function DefaultTemplateStringValue: string; override;
    class function GetName: string; override;
  end;

  { TdxHFDateTimeFunctions }

  TdxHFDateTimeFunctions = class(TdxHFCustomFunction)
  public
    class function GetCategory: TdxHFFunctionCustomCategoryClass; override;
  end;

  { TdxHFDateTimeFunction }

  TdxHFDateTimeFunction = class(TdxHFDateTimeFunctions)
  protected
    function ConvertFunc(const Source: string; const AFormatObject: TdxHFFunctionFormatObject): string; override;
  public
    constructor Create; override;
    class function DefaultHintValue: string; override;
    class function DefaultTemplateStringValue: string; override;
    class function GetName: string; override;
  end;

  { TdxHFDateFunction }

  TdxHFDateFunction = class(TdxHFDateTimeFunctions)
  protected
    function ConvertFunc(const Source: string; const AFormatObject: TdxHFFunctionFormatObject): string; override;
  public
    constructor Create; override;
    class function DefaultHintValue: string; override;
    class function DefaultTemplateStringValue: string; override;
    class function GetName: string; override;
  end;

  { TdxHFTimeFunction }

  TdxHFTimeFunction = class(TdxHFDateTimeFunctions)
  protected
    function ConvertFunc(const Source: string; const AFormatObject: TdxHFFunctionFormatObject): string; override;
  public
    constructor Create; override;
    class function DefaultHintValue: string; override;
    class function DefaultTemplateStringValue: string; override;
    class function GetName: string; override;
  end;

  { TdxHFImageFunction }

  TdxHFImageFunction = class(TdxHFCustomFunction)
  public
    constructor Create; override;
    class function DefaultHintValue: string; override;
    class function DefaultTemplateStringValue: string; override;
    class function GetCategory: TdxHFFunctionCustomCategoryClass; override;
    class function GetName: string; override;
  end;

  { TdxHFFunctionLibrary }

  TdxHFFunctionEnumProc = procedure(Sender: TdxHFFunctionLibrary; const AHFFunction: TdxHFCustomFunction) of object;
  TdxHFFunctionLibraryClass = class of TdxHFFunctionLibrary;

  TdxHFFunctionLibrary = class(TPersistent)
  private
    FItems: TList;
    function GetCount: Integer;
    function GetFunction(Index: Integer): TdxHFCustomFunction;
    function GetFunctionByClass(FunctionClass: TdxHFCustomFunctionClass): TdxHFCustomFunction;
    procedure SetFunction(Index: Integer; Value: TdxHFCustomFunction);
    procedure SetFunctionByClass(FunctionClass: TdxHFCustomFunctionClass; Value: TdxHFCustomFunction);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(AFunctionClass: TdxHFCustomFunctionClass): TdxHFCustomFunction;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Enumerate(AProc: TdxHFFunctionEnumProc); virtual;
    procedure GetFunctions(AStrings: TStrings);
    procedure GetFunctionsByCategory(ACategory: TdxHFFunctionCustomCategoryClass; AStrings: TStrings);
    function IndexOf(const ATemplateString: string): Integer; virtual;
    function IndexOfByName(const AFunctionName: string): Integer;
    function IndexOfByClass(AFunctionClass: TdxHFCustomFunctionClass): Integer;
    function ProcessString(const Source: string; const AFormatObject: TdxHFFunctionFormatObject): string; virtual;
    //
    property Count: Integer read GetCount;
    property Funcs[Index: Integer]: TdxHFCustomFunction read GetFunction write SetFunction; default;
    property FuncsByClass[FunctionClass: TdxHFCustomFunctionClass]: TdxHFCustomFunction read GetFunctionByClass write SetFunctionByClass;
  end;

  { TdxHFReportCellBuilder }

  TdxHFReportCellBuilder = class(TObject)
  protected
    procedure AlignDataItems(ACell: TdxReportCell; AAlignment: TcxTextAlignY);
    procedure CalculateTitlePartSize(ACell: TdxReportCell;
      ACanvas: TdxPSReportRenderCustomCanvas);
  public
    procedure Build(ACanvas: TdxPSReportRenderCustomCanvas;
      AHost: TdxReportCells; const R: TRect; ATitleParts: TdxPageTitleParts;
      APageObject: TCustomdxPageObject; AReverseTitles: Boolean); virtual;
    procedure PlaceTitleParts(ACanvas: TdxPSReportRenderCustomCanvas;
      AHostCell: TdxReportCell; R: TRect; APageObject: TCustomdxPageObject);
    procedure PopulateCells(AParentCell: TdxReportCell; AHost: TdxReportCells;
      ATextAlignX: TcxTextAlignX; ATitlePart: TdxPageTitlePart;
      APageObject: TCustomdxPageObject); overload;
    procedure PopulateCells(AParentCell: TdxReportCell; AHost: TdxReportCells;
      const Source: string; ATextAlignX: TcxTextAlignX; ATextAlignY: TcxTextAlignY;
      APageObject: TCustomdxPageObject; AFormatObject: TdxHFFunctionFormatObject); overload; virtual;
  end;

  { TdxStandardHFFunctionLibrary }

  TdxStandardHFFunctionLibrary = class(TdxHFFunctionLibrary)
  protected
    procedure AddStandardFunctions; virtual;
  public
    constructor Create; override;
  end;

  { TdxHFFunctionPlaceInfo }

  TdxHFFunctionPlaceInfo = packed record
    Index: Integer;
    Left, Right: Integer;
    Name: string;
  end;

  { TdxHFTemplateExpander }

  TdxHFTemplateExpander = class
  strict private
    FCursorIndex: Integer;
    FStr: string;

    function GetStrLength: Integer;
    procedure SetCursorIndex(AValue: Integer);
  public
    constructor Create(const AStr: string);
    function GetFuncInfo(out AInfo: TdxHFFunctionPlaceInfo): Boolean;
    procedure Replace(const AInfo: TdxHFFunctionPlaceInfo; const AValue: string; AWrapBySpaces: Boolean = True);
    //
    property CursorIndex: Integer read FCursorIndex write SetCursorIndex;
    property Str: string read FStr;
    property StrLength: Integer read GetStrLength;
  end;

const
  dxFunctionDelimiters: array[Boolean] of Char = ('[', ']');
  dxFunctionIndexSeparator = '=';
  dxHFFunctionSeparator = ',';

var
  dxHFFunctionLibrary: TdxHFFunctionLibrary = nil;
  dxHFFormatObject: TdxHFFunctionFormatObject = nil;

function dxProcessHFString(const Source: string): string;
procedure dxGetHFFunctionsList(AStrings: TStrings);
procedure dxGetHFFunctionsListByCategory(ACategory: TdxHFFunctionCustomCategoryClass; AStrings: TStrings);
procedure dxPSSplitAutoHFTextEntry(Source: string; var APart1, APart2, APart3: string);

implementation

uses
  Types, Math, StrUtils, cxFormats, dxPgsDlg;

type
  { TdxReportCellTitlePartText }

  TdxReportCellTitlePartText = class(TdxReportCellText)
  strict private
    FLines: TStrings;
  protected
    function GetDTFormat: Cardinal; override;
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  public
    constructor Create(AParent: TdxReportCell); override;
    destructor Destroy; override;
    function MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    //
    property Lines: TStrings read FLines;
  end;

  { TdxReportCellTitlePartImage }

  TdxReportCellTitlePartImage = class(TAbstractdxReportCellData)
  strict private
    FImageIndex: Integer;

    function GetHasImage: Boolean;
    function GetImageCollectionItem: TcxImageCollectionItem;
    function GetImageSize: TSize;
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure Assign(Source: TPersistent); override;
    function MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); override;
    //
    property HasImage: Boolean read GetHasImage;
    property ImageCollectionItem: TcxImageCollectionItem read GetImageCollectionItem;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property ImageSize: TSize read GetImageSize;
  end;

function dxFormatPageNumber(ANumber: Integer; AFormat: TdxPageNumberFormat): string;
begin
  case AFormat of
    pnfChars, pnfUpperChars:
      Result := Int2Chars(ANumber, AFormat = pnfUpperChars);
    pnfRoman, pnfUpperRoman:
      Result := Int2Roman(ANumber, AFormat = pnfUpperRoman);
    else
      Result := IntToStr(ANumber);
  end;
end;

procedure dxPSSplitAutoHFTextEntry(Source: string; var APart1, APart2, APart3: string);

  function DoExtract(var Source: string): string;
  var
    P: Integer;
  begin
    P := Pos(dxHFFunctionSeparator, Source);
    if P = 0 then
    begin
      Result := Source;
      P := Length(Source);
    end
    else
      Result := Copy(Source, 1, P - 1);

    Delete(Source, 1, P);
    Result := Trim(Result);
  end;

begin
  APart1 := '';
  APart2 := '';
  APart3 := '';
  APart1 := DoExtract(Source);
  if Source <> '' then
    APart2 := DoExtract(Source);
  if Source <> '' then
    APart3 := DoExtract(Source);
end;

function dxProcessHFString(const Source: string): string;
begin
  if Assigned(dxHFFunctionLibrary) then
    Result := dxHFFunctionLibrary.ProcessString(Source, dxHFFormatObject)
  else
    Result := Source;
end;

procedure dxGetHFFunctionsList(AStrings: TStrings);
begin
  if Assigned(dxHFFunctionLibrary) then
    dxHFFunctionLibrary.GetFunctions(AStrings);
end;

procedure dxGetHFFunctionsListByCategory(
  ACategory: TdxHFFunctionCustomCategoryClass; AStrings: TStrings);
begin
  if Assigned(dxHFFunctionLibrary) then
    dxHFFunctionLibrary.GetFunctionsByCategory(ACategory, AStrings);
end;

{ TdxHFReportCellBuilder }

procedure TdxHFReportCellBuilder.AlignDataItems(
  ACell: TdxReportCell; AAlignment: TcxTextAlignY);
var
  ADataItem: TAbstractdxReportCellData;
  I: Integer;
begin
  for I := 0 to ACell.DataItemCount - 1 do
  begin
    ADataItem := ACell.DataItems[I];
    case AAlignment of
      taBottom:
        ADataItem.Top := ACell.Height - ADataItem.Height;
      taCenterY:
        ADataItem.Top := (ACell.Height - ADataItem.Height) div 2;
      else
        ADataItem.Top := 0;
    end;
  end;
end;

procedure TdxHFReportCellBuilder.Build(ACanvas: TdxPSReportRenderCustomCanvas;
  AHost: TdxReportCells; const R: TRect; ATitleParts: TdxPageTitleParts;
  APageObject: TCustomdxPageObject; AReverseTitles: Boolean);
const
  ReverseTitlesMap: array[Boolean, TdxPageTitlePart] of TdxPageTitlePart =
    ((tpLeft, tpCenter, tpRight), (tpRight, tpCenter, tpLeft));
  TextAlignMap: array[TdxPageTitlePart] of TcxTextAlignX = (taLeft, taCenterX, taRight);
var
  ACell: TdxReportCell;
  AIndex: TdxPageTitlePart;
begin
  for AIndex := Low(TdxPageTitlePart) to High(TdxPageTitlePart) do
  begin
    ACell := AHost.Cells.AddCell;
    ACell.ClipChildren := True;
    if AIndex in ATitleParts then
    begin
      PopulateCells(ACell, AHost, TextAlignMap[AIndex],
        ReverseTitlesMap[AReverseTitles, AIndex], APageObject);
    end;
    CalculateTitlePartSize(ACell, ACanvas);
  end;
  PlaceTitleParts(ACanvas, AHost.Cells, R, APageObject);
end;

procedure TdxHFReportCellBuilder.CalculateTitlePartSize(
  ACell: TdxReportCell; ACanvas: TdxPSReportRenderCustomCanvas);
var
  ADataItem: TAbstractdxReportCellData;
  I, AWidth, AHeight: Integer;
begin
  AWidth := 0;
  AHeight := 0;
  for I := 0 to ACell.DataItemCount - 1 do
  begin
    ADataItem := ACell.DataItems[I];
    ADataItem.Width := ADataItem.MeasureWidth(ACanvas);
    ADataItem.Height := ADataItem.MeasureHeight(ACanvas);
    ADataItem.Offset(AWidth, 0);
    Inc(AWidth, ADataItem.Width);
    AHeight := Max(AHeight, ADataItem.Height);
  end;
  ACell.Height := AHeight;
  ACell.Width := AWidth;
end;

procedure TdxHFReportCellBuilder.PlaceTitleParts(
  ACanvas: TdxPSReportRenderCustomCanvas; AHostCell: TdxReportCell; R: TRect;
  APageObject: TCustomdxPageObject);

  function CalculateTitlePartPosition(const R: TRect;
    AWidth: Integer; ATitlePart: TdxPageTitlePart): TRect;
  begin
    case ATitlePart of
      tpRight:
        Result := cxRect(Max(R.Right - AWidth, R.Left), R.Top, R.Right, R.Bottom);

      tpCenter:
        begin
          Result := cxRectCenterHorizontally(R, AWidth);
          Result.Right := Min(Result.Right, R.Right);
          Result.Left := Max(Result.Left, R.Left);
        end;

      else
        Result := cxRect(R.Left, R.Top, Min(R.Left + AWidth, R.Right), R.Bottom);
    end;
  end;

  function PlaceTitlePart(const R: TRect; ACell: TdxReportCell; ATitlePart: TdxPageTitlePart): TRect;
  begin
    ACell.BoundsRect := CalculateTitlePartPosition(R, ACell.Width, ATitlePart);
    AlignDataItems(ACell, APageObject.TextAlignY[ATitlePart]);
    Result := ACell.BoundsRect;
  end;

  function CalculateCenterPartPosition(const R: TRect): TRect;
  begin
    if AHostCell[1].Width > 0 then
      Result := PlaceTitlePart(R, AHostCell[1], tpCenter)
    else
    begin
      Result := R;
      Inc(Result.Left, AHostCell[0].Width);
      Dec(Result.Right, AHostCell[2].Width);
      Result.Right := Max(Result.Right, Result.Left);
    end;
  end;

var
  R1: TRect;
begin
  AHostCell.BoundsRect := cxRectOffset(R, R.TopLeft, False);
  R1 := CalculateCenterPartPosition(R);
  PlaceTitlePart(cxRect(R.Left, R.Top, R1.Left, R.Bottom), AHostCell[0], tpLeft);
  PlaceTitlePart(cxRect(R1.Right, R.Top, R.Right, R.Bottom), AHostCell[2], tpRight);
end;

procedure TdxHFReportCellBuilder.PopulateCells(AParentCell: TdxReportCell;
  AHost: TdxReportCells; ATextAlignX: TcxTextAlignX; ATitlePart: TdxPageTitlePart;
  APageObject: TCustomdxPageObject);
begin
  PopulateCells(AParentCell, AHost, APageObject.Titles[ATitlePart].Text,
    ATextAlignX, APageObject.TextAlignY[ATitlePart], APageObject, dxHFFormatObject);
end;

procedure TdxHFReportCellBuilder.PopulateCells(AParentCell: TdxReportCell;
  AHost: TdxReportCells; const Source: string; ATextAlignX: TcxTextAlignX;
  ATextAlignY: TcxTextAlignY; APageObject: TCustomdxPageObject;
  AFormatObject: TdxHFFunctionFormatObject);

  procedure AddTextCell(const AText: string);
  var
    ATextCell: TdxReportCellTitlePartText;
  begin
    if AText <> '' then
    begin
      ATextCell := TdxReportCellTitlePartText(AParentCell.AddDataItem(TdxReportCellTitlePartText));
      ATextCell.FontIndex := AHost.GetIndexByFont(APageObject.Font);
      ATextCell.TextAlignX := ATextAlignX;
      ATextCell.TextAlignY := ATextAlignY;
      ATextCell.Text := AText;
    end;
  end;

  procedure AddImageCell(AImageIndex: Integer);
  var
    AImageCell: TdxReportCellTitlePartImage;
  begin
    if AImageIndex >= 0 then
    begin
      AImageCell := TdxReportCellTitlePartImage(AParentCell.AddDataItem(TdxReportCellTitlePartImage));
      AImageCell.ImageIndex := AImageIndex;
    end;
  end;

var
  AExpander: TdxHFTemplateExpander;
  AFuncInfo: TdxHFFunctionPlaceInfo;
  AFunction: TdxHFCustomFunction;
  AIndex, ATextOffset: Integer;
begin
  ATextOffset := 1;
  AExpander := TdxHFTemplateExpander.Create(Source);
  try
    while AExpander.GetFuncInfo(AFuncInfo) do
    begin
      AIndex := dxHFFunctionLibrary.IndexOf(AFuncInfo.Name);
      if AIndex >= 0 then
      begin
        AFunction := dxHFFunctionLibrary.Funcs[AIndex];
        if AFunction.GetCategory.InheritsFrom(TdxHFFunctionImagesCategory) then
        begin
          AddTextCell(Copy(AExpander.Str, ATextOffset, AFuncInfo.Left - ATextOffset));
          AddImageCell(AFuncInfo.Index);
          ATextOffset := AFuncInfo.Right + 1;
        end
        else
          AExpander.Replace(AFuncInfo, AFunction.DoProcess(AFuncInfo.Name, AFormatObject));
      end;
    end;
    AddTextCell(Copy(AExpander.Str, ATextOffset, AExpander.StrLength - ATextOffset + 1));
  finally
    AExpander.Free;
  end;
end;

{ TdxHFFunctionFormatObject }

constructor TdxHFFunctionFormatObject.Create;
begin
  inherited Create;
  Initialize;
end;

procedure TdxHFFunctionFormatObject.Initialize;
begin
  FDateTime := Now;
  FCurrentPage := 1;
  FDateFormat := dxFormatSettings.LongDateFormat;
  FMachineName := dxPSUtl.GetMachineName;
  FPageNumberFormat := pnfNumeral;
  FTimeFormat := dxFormatSettings.LongTimeFormat;
  FTotalPages := 1;
  FUserName := dxGetUserName;
end;

{ TdxHFCustomFunction }

constructor TdxHFCustomFunction.Create;
begin
  inherited Create;
  FGlyph := TcxBitmap.Create;
end;

destructor TdxHFCustomFunction.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

procedure TdxHFCustomFunction.Assign(Source: TPersistent);
begin
  if Source is TdxHFCustomFunction then
  begin
    TemplateString := TdxHFCustomFunction(Source).TemplateString;
    Glyph := TdxHFCustomFunction(Source).Glyph;
    Hint := TdxHFCustomFunction(Source).Hint;
  end
  else
    inherited Assign(Source);
end;

class function TdxHFCustomFunction.FunctionClass: TdxHFCustomFunctionClass;
begin
  Result := TdxHFCustomFunctionClass(GetTypeData(ClassInfo)^.ClassType);
end;

function TdxHFCustomFunction.DoProcess(const Source: string;
  const AFormatObject: TdxHFFunctionFormatObject): string;
begin
  if AFormatObject <> nil then
    Result := ConvertFunc(Source, AFormatObject)
  else
    Result := Source;
end;

class function TdxHFCustomFunction.GetName: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionNameUnknown);
end;

class function TdxHFCustomFunction.GetCategory: TdxHFFunctionCustomCategoryClass;
begin
  Result := TdxHFFunctionCustomCategory;
end;

function TdxHFCustomFunction.ConvertFunc(const Source: string;
  const AFormatObject: TdxHFFunctionFormatObject): string;
begin
  Result := Source;
end;

class function TdxHFCustomFunction.DefaultHintValue: string;
begin
  Result := '';
end;

class function TdxHFCustomFunction.DefaultTemplateStringValue: string;
begin
  Result := '';
end;

function TdxHFCustomFunction.GetHint: string;
begin
  if FIsHintAssigned then
    Result := FHint
  else
    Result := DefaultHintValue;
end;

function TdxHFCustomFunction.GetTemplateString: string;
begin
  if FIsTemplateStringAssigned then
    Result := FTemplateString
  else
    Result := DefaultTemplateStringValue;
  if Length(Result) > 0 then
  begin
    if Result[1] <> dxFunctionDelimiters[False] then
      Result := dxFunctionDelimiters[False] + Result;
    if Result[Length(Result)] <> dxFunctionDelimiters[True] then
      Result := Result + dxFunctionDelimiters[True];
  end;
end;

procedure TdxHFCustomFunction.SetGlyph(Value: TcxBitmap);
begin
  Glyph.Assign(Value);
end;

procedure TdxHFCustomFunction.SetHint(const Value: string);
begin
  FHint := Value;
  FIsHintAssigned := FHint <> DefaultHintValue;
end;

procedure TdxHFCustomFunction.SetTemplateString(const Value: string);
begin
  FTemplateString := Value;
  FIsTemplateStringAssigned := FTemplateString <> DefaultTemplateStringValue;
end;

{ TdxHFPagesFunctions }

class function TdxHFPagesFunctions.GetCategory: TdxHFFunctionCustomCategoryClass;
begin
  Result := TdxHFFunctionPagesCategory;
end;

{ TdxHFPageNumberFunction }

constructor TdxHFPageNumberFunction.Create;
begin
  inherited Create;
  dxLoadBitmapFromResource(Glyph, IDB_DXPSFUNCTION_PAGENUMBER);
end;

function TdxHFPageNumberFunction.ConvertFunc(const Source: string;
  const AFormatObject: TdxHFFunctionFormatObject): string;
begin
  Result := dxFormatPageNumber(
    AFormatObject.StartPageIndex + AFormatObject.CurrentPage - 1,
    AFormatObject.PageNumberFormat);
end;

class function TdxHFPageNumberFunction.GetName: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionNamePageNumber);
end;

class function TdxHFPageNumberFunction.DefaultHintValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionHintPageNumber);
end;

class function TdxHFPageNumberFunction.DefaultTemplateStringValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionTemplatePageNumber);
end;

{ TdxHFTotalPagesFunction }

constructor TdxHFTotalPagesFunction.Create;
begin
  inherited Create;
  dxLoadBitmapFromResource(Glyph, IDB_DXPSFUNCTION_TOTALPAGES);
end;

function TdxHFTotalPagesFunction.ConvertFunc(const Source: string;
  const AFormatObject: TdxHFFunctionFormatObject): string;
begin
  Result := dxFormatPageNumber(
    AFormatObject.StartPageIndex + AFormatObject.TotalPages - 1,
    AFormatObject.PageNumberFormat);
end;

class function TdxHFTotalPagesFunction.GetName: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionNameTotalPages);
end;

class function TdxHFTotalPagesFunction.DefaultHintValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionHintTotalPages);
end;

class function TdxHFTotalPagesFunction.DefaultTemplateStringValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionTemplateTotalPages);
end;

{ TdxHFPageOfPagesFunction }

constructor TdxHFPageOfPagesFunction.Create;
begin
  inherited Create;
  dxLoadBitmapFromResource(Glyph, IDB_DXPSFUNCTION_PAGENUMBEROFPAGES);
end;

function TdxHFPageOfPagesFunction.ConvertFunc(const Source: string;
  const AFormatObject: TdxHFFunctionFormatObject): string;
begin
  Result :=
    dxFormatPageNumber(
      AFormatObject.CurrentPage + AFormatObject.StartPageIndex - 1,
      AFormatObject.PageNumberFormat) + ' ' + cxGetResourceString(@sdxOf) + ' ' +
    dxFormatPageNumber(
      AFormatObject.TotalPages + AFormatObject.StartPageIndex - 1,
      AFormatObject.PageNumberFormat);
end;

class function TdxHFPageOfPagesFunction.GetName: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionNamePageOfPages);
end;

class function TdxHFPageOfPagesFunction.DefaultHintValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionHintPageOfPages);
end;

class function TdxHFPageOfPagesFunction.DefaultTemplateStringValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionTemplatePageOfPages);
end;

{ TdxHFAuthenticationFunctions }

class function TdxHFAuthenticationFunctions.GetCategory: TdxHFFunctionCustomCategoryClass;
begin
  Result := TdxHFFunctionAuthenticationCategory;
end;

{ TdxHFMachineNameFunction }

constructor TdxHFMachineNameFunction.Create;
begin
  inherited Create;
  dxLoadBitmapFromResource(Glyph, IDB_DXPSFUNCTION_MACHINENAME);
end;

class function TdxHFMachineNameFunction.GetName: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionNameMachineName);
end;

function TdxHFMachineNameFunction.ConvertFunc(const Source: string;
  const AFormatObject: TdxHFFunctionFormatObject): string;
begin
  Result := AFormatObject.MachineName
end;

class function TdxHFMachineNameFunction.DefaultHintValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionHintMachineName);
end;

class function TdxHFMachineNameFunction.DefaultTemplateStringValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionTemplateMachineName);
end;

{ TdxHFMachineNameFunction }

constructor TdxHFUserNameFunction.Create;
begin
  inherited Create;
  dxLoadBitmapFromResource(Glyph, IDB_DXPSFUNCTION_USERNAME);
end;

class function TdxHFUserNameFunction.GetName: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionNameUserName);
end;

function TdxHFUserNameFunction.ConvertFunc(const Source: string;
  const AFormatObject: TdxHFFunctionFormatObject): string;
begin
  Result := AFormatObject.UserName
end;

class function TdxHFUserNameFunction.DefaultHintValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionHintUserName);
end;

class function TdxHFUserNameFunction.DefaultTemplateStringValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionTemplateUserName);
end;

{ TdxHFDateTimeFunctions }

class function TdxHFDateTimeFunctions.GetCategory: TdxHFFunctionCustomCategoryClass;
begin
  Result := TdxHFFunctionDateTimeCategory;
end;

{ TdxHFDateTimeFunction }

constructor TdxHFDateTimeFunction.Create;
begin
  inherited Create;
  dxLoadBitmapFromResource(Glyph, IDB_DXPSFUNCTION_DATETIME);
end;

function TdxHFDateTimeFunction.ConvertFunc(
  const Source: string; const AFormatObject: TdxHFFunctionFormatObject): string;
begin
  Result :=
    GetFormatedDate(AFormatObject.DateTime, AFormatObject.DateFormat) + ' ' +
    GetFormatedTime(AFormatObject.DateTime, AFormatObject.TimeFormat);
end;

class function TdxHFDateTimeFunction.GetName: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionNameDateTime);
end;

class function TdxHFDateTimeFunction.DefaultHintValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionHintDateTime);
end;

class function TdxHFDateTimeFunction.DefaultTemplateStringValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionTemplateDateTime);
end;

{ TdxHFDateFunction }

constructor TdxHFDateFunction.Create;
begin
  inherited Create;
  dxLoadBitmapFromResource(Glyph, IDB_DXPSFUNCTION_DATE);
end;

function TdxHFDateFunction.ConvertFunc(const Source: string;
  const AFormatObject: TdxHFFunctionFormatObject): string;
begin
  Result := GetFormatedDate(AFormatObject.DateTime, AFormatObject.DateFormat);
end;

class function TdxHFDateFunction.GetName: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionNameDate);
end;

class function TdxHFDateFunction.DefaultHintValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionHintDate);
end;

class function TdxHFDateFunction.DefaultTemplateStringValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionTemplateDate);
end;

{ TdxHFTimeFunction }

constructor TdxHFTimeFunction.Create;
begin
  inherited Create;
  dxLoadBitmapFromResource(Glyph, IDB_DXPSFUNCTION_TIME);
end;

function TdxHFTimeFunction.ConvertFunc(const Source: string;
  const AFormatObject: TdxHFFunctionFormatObject): string;
begin
  Result := GetFormatedTime(AFormatObject.DateTime, AFormatObject.TimeFormat);
end;

class function TdxHFTimeFunction.GetName: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionNameTime);
end;

class function TdxHFTimeFunction.DefaultHintValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionHintTime);
end;

class function TdxHFTimeFunction.DefaultTemplateStringValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionTemplateTime);
end;

{ TdxHFImageFunction }

constructor TdxHFImageFunction.Create;
begin
  inherited Create;
  dxLoadBitmapFromResource(Glyph, IDB_DXPSFUNCTION_IMAGE);
end;

class function TdxHFImageFunction.GetCategory: TdxHFFunctionCustomCategoryClass;
begin
  Result := TdxHFFunctionImagesCategory;
end;

class function TdxHFImageFunction.GetName: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionNameImage);
end;

class function TdxHFImageFunction.DefaultHintValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionHintImage);
end;

class function TdxHFImageFunction.DefaultTemplateStringValue: string;
begin
  Result := cxGetResourceString(@sdxHFFunctionTemplateImage);
end;

{ TdxHFFunctionLibrary }

constructor TdxHFFunctionLibrary.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TdxHFFunctionLibrary.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxHFFunctionLibrary.Assign(Source: TPersistent);
begin
  if Source is TdxHFFunctionLibrary then
    FItems.Assign(TdxHFFunctionLibrary(Source).FItems)
  else
    inherited Assign(Source);
end;

function TdxHFFunctionLibrary.Add(AFunctionClass: TdxHFCustomFunctionClass): TdxHFCustomFunction;
begin
  Result := AFunctionClass.Create;
  FItems.Add(Result);
end;

procedure TdxHFFunctionLibrary.Clear;
begin
  while Count > 0 do Delete(Count - 1);
end;

procedure TdxHFFunctionLibrary.Delete(AIndex: Integer);
var
  AFunction: TdxHFCustomFunction;
begin
  AFunction := Funcs[AIndex];
  FItems.Delete(AIndex);
  FreeAndNil(AFunction);
end;

function TdxHFFunctionLibrary.IndexOf(const ATemplateString: string): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if dxSameText(Funcs[Result].TemplateString, ATemplateString) then
      Exit;
  end;
  Result := -1;
end;

function TdxHFFunctionLibrary.IndexOfByName(const AFunctionName: string): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if dxSameText(Funcs[Result].GetName, AFunctionName) then
      Exit;
  end;
  Result := -1;
end;

function TdxHFFunctionLibrary.IndexOfByClass(AFunctionClass: TdxHFCustomFunctionClass): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if Funcs[Result].FunctionClass = AFunctionClass then
      Exit;
  end;
  Result := -1;
end;

procedure TdxHFFunctionLibrary.GetFunctions(AStrings: TStrings);
begin
  GetFunctionsByCategory(nil, AStrings);
end;

procedure TdxHFFunctionLibrary.GetFunctionsByCategory(
  ACategory: TdxHFFunctionCustomCategoryClass; AStrings: TStrings);
var
  AFunction: TdxHFCustomFunction;
  I: Integer;
begin
  AStrings.BeginUpdate;
  try
    for I := 0 to Count - 1 do
    begin
      AFunction := Funcs[I];
      if (AFunction.GetCategory = ACategory) or (ACategory = nil) then
        AStrings.AddObject(AFunction.TemplateString, AFunction);
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

procedure TdxHFFunctionLibrary.Enumerate(AProc: TdxHFFunctionEnumProc);
var
  I: Integer;
begin
  if Assigned(AProc) then
  begin
    for I := 0 to Count - 1 do
      AProc(Self, Funcs[I]);
  end;
end;

function TdxHFFunctionLibrary.ProcessString(const Source: string; const AFormatObject: TdxHFFunctionFormatObject): string;
var
  AExpander: TdxHFTemplateExpander;
  AFuncInfo: TdxHFFunctionPlaceInfo;
  AIndex: Integer;
begin
  AExpander := TdxHFTemplateExpander.Create(Source);
  try
    while AExpander.GetFuncInfo(AFuncInfo) do
    begin
      AIndex := IndexOf(AFuncInfo.Name);
      if AIndex >= 0 then
        AExpander.Replace(AFuncInfo, Funcs[AIndex].DoProcess(AFuncInfo.Name, AFormatObject));
    end;
    Result := AExpander.Str;
  finally
    AExpander.Free;
  end;
end;

function TdxHFFunctionLibrary.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxHFFunctionLibrary.GetFunction(Index: Integer): TdxHFCustomFunction;
begin
  Result := TdxHFCustomFunction(FItems.Items[Index]);
end;

function TdxHFFunctionLibrary.GetFunctionByClass(
  FunctionClass: TdxHFCustomFunctionClass): TdxHFCustomFunction;
var
  AIndex: Integer;
begin
  AIndex := IndexOfByClass(FunctionClass);
  if AIndex >= 0 then
    Result := Funcs[AIndex]
  else
    Result := nil;
end;

procedure TdxHFFunctionLibrary.SetFunction(Index: Integer; Value: TdxHFCustomFunction);
begin
  Funcs[Index].Assign(Value);
end;

procedure TdxHFFunctionLibrary.SetFunctionByClass(
  FunctionClass: TdxHFCustomFunctionClass; Value: TdxHFCustomFunction);
var
  AIndex: Integer;
begin
  AIndex := IndexOfByClass(FunctionClass);
  if AIndex <> -1 then
    Funcs[AIndex] := Value;
end;

{ TdxStdHFFunctionLibrary }

constructor TdxStandardHFFunctionLibrary.Create;
begin
  inherited Create;
  AddStandardFunctions;
end;

procedure TdxStandardHFFunctionLibrary.AddStandardFunctions;
begin
  Add(TdxHFPageNumberFunction);
  Add(TdxHFTotalPagesFunction);
  Add(TdxHFPageOfPagesFunction);
  Add(TdxHFDateTimeFunction);
  Add(TdxHFDateFunction);
  Add(TdxHFTimeFunction);
  Add(TdxHFUserNameFunction);
  Add(TdxHFMachineNameFunction);
  Add(TdxHFImageFunction);
end;

{ TdxHFTemplateExpander }

constructor TdxHFTemplateExpander.Create(const AStr: string);
begin
  inherited Create;
  FCursorIndex := 1;
  FStr := TrimRight(AStr);
end;

procedure TdxHFTemplateExpander.Replace(const AInfo: TdxHFFunctionPlaceInfo; const AValue: string; AWrapBySpaces: Boolean);
begin
  if AWrapBySpaces and (AValue <> '') then
    Replace(AInfo, ' ' + AValue + ' ', False)
  else
  begin
    FStr := Copy(FStr, 1, AInfo.Left - 1) + AValue + Copy(FStr, AInfo.Right + 1, MaxInt);
    CursorIndex := AInfo.Left + Length(AValue);
  end;
end;

function TdxHFTemplateExpander.GetStrLength: Integer;
begin
  Result := Length(FStr);
end;

function TdxHFTemplateExpander.GetFuncInfo(out AInfo: TdxHFFunctionPlaceInfo): Boolean;

  procedure ClearInfo(var AInfo: TdxHFFunctionPlaceInfo);
  begin
    AInfo.Index := 0;
    AInfo.Right := 0;
    AInfo.Name := '';
    AInfo.Left := 0;
  end;

  function FindPart(AFinishPart: Boolean; var APosition: Integer): Boolean;
  begin
    APosition := PosEx(dxFunctionDelimiters[AFinishPart], Str, CursorIndex);
    Result := APosition > 0;
    if Result then
      CursorIndex := APosition;
  end;

var
  AIndexPos, AIndexLength: Integer;
begin
  Result := False;
  ClearInfo(AInfo);
  if CursorIndex <= StrLength then
  begin
    Result := FindPart(False, AInfo.Left) and FindPart(True, AInfo.Right);
    if Result then
    begin
      AInfo.Name := Copy(Str, AInfo.Left, AInfo.Right - AInfo.Left + 1);
      AIndexPos := Pos(dxFunctionIndexSeparator, AInfo.Name);
      if AIndexPos > 0 then
      begin
        AIndexLength := Length(AInfo.Name) - AIndexPos - 1;
        AInfo.Index := StrToIntDef(Copy(AInfo.Name, AIndexPos + 1, AIndexLength), 0);
        Delete(AInfo.Name, AIndexPos, AIndexLength + 1);
      end;
      CursorIndex := AInfo.Right + 1;
    end;
  end;
end;

procedure TdxHFTemplateExpander.SetCursorIndex(AValue: Integer);
begin
  FCursorIndex := Max(Min(AValue, StrLength), 1);
end;

{ TdxReportCellTitlePartText }

constructor TdxReportCellTitlePartText.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  FLines := TStringList.Create;
  EndEllipsis := False;
  Multiline := True;
  CellSides := [];
end;

destructor TdxReportCellTitlePartText.Destroy;
begin
  FreeAndNil(FLines);
  inherited Destroy;
end;

function TdxReportCellTitlePartText.MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Lines.Count - 1 do
    Result := Max(Result, Renderer.CalcTextWidth(ACanvas, Lines[I], Font));
  Inc(Result, 2 * Renderer.LineThickness * dxTextSpace);
end;

function TdxReportCellTitlePartText.GetDTFormat: Cardinal;
begin
  Result := inherited GetDTFormat and not CXTO_WORDBREAK;
  if not Multiline then
    Result := Result or CXTO_SINGLELINE;
end;

function TdxReportCellTitlePartText.GetText: string;
begin
  Result := Lines.Text;
end;

procedure TdxReportCellTitlePartText.SetText(const Value: string);
begin
  Lines.Text := Value;
end;

{ TdxReportCellTitlePartImage }

constructor TdxReportCellTitlePartImage.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  CellSides := [];
end;

procedure TdxReportCellTitlePartImage.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxReportCellTitlePartImage then
    ImageIndex := TdxReportCellTitlePartImage(Source).ImageIndex;
end;

procedure TdxReportCellTitlePartImage.DrawContent(
  ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
begin
  inherited DrawContent(ACanvas, AStage);
  if HasImage then
  begin
    Renderer.DrawGraphicEx(ACanvas, GetInnerBounds(ACanvas),
      GetInnerBounds(ACanvas), nil, -1, ImageCollectionItem.Picture.Graphic,
      False, IsBackgroundBitmapDrawn or Transparent, Color, ContentBkColor,
      ContentPattern);
  end;
end;

function TdxReportCellTitlePartImage.MeasureContentHeight(
  ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := ImageSize.cy;
end;

function TdxReportCellTitlePartImage.MeasureContentWidth(
  ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := ImageSize.cx;
end;

function TdxReportCellTitlePartImage.GetHasImage: Boolean;
begin
  Result := (ImageIndex >= 0) and Assigned(ImageCollectionItem);
end;

function TdxReportCellTitlePartImage.GetImageCollectionItem: TcxImageCollectionItem;
var
  ACollection: TcxImageCollection;
begin
  ACollection := ReportLink.RealPrinterPage.ImageCollection;
  if Assigned(ACollection) and (ImageIndex >= 0) and (ImageIndex < ACollection.Items.Count) then
    Result := ACollection.Items[ImageIndex]
  else
    Result := nil;
end;

function TdxReportCellTitlePartImage.GetImageSize: TSize;

  function ScaleValue(AValue: Integer): Integer;
  var
    AScalePair: TdxWindowScalePair;
  begin
    AScalePair := Renderer.RenderInfo.WindowScalePair;
    Result := MulDiv(AValue, PixelsNumerator, PixelsDenominator);
    Result := MulDiv(Result, AScalePair.Numerator, AScalePair.Denominator);
  end;

begin
  if HasImage then
  begin
    with ImageCollectionItem do
      Result := cxSize(ScaleValue(Width), ScaleValue(Height));
  end
  else
    Result := cxNullSize;
end;

initialization
  dxHFFunctionLibrary := TdxStandardHFFunctionLibrary.Create;
  dxHFFormatObject := TdxHFFunctionFormatObject.Create;

finalization
  FreeAndNil(dxHFFormatObject);
  FreeAndNil(dxHFFunctionLibrary);

end.
