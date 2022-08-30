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

unit dxPrnPg;

interface

{$I cxVer.inc}

uses
  Types, Classes, SysUtils, Windows, Controls, Graphics, Dialogs, CommDlg, Messages,
  dxBase, dxPSGlbl, dxPSUtl, dxWrap, dxPrnDev, dxBkgnd, cxDrawTextUtils, cxGeometry, cxGraphics, dxMeasurementUnits,
  dxPrintUtils;

type
  TdxHFPageType = (ptHeader, ptFooter);
  TdxPageTitlePart = (tpLeft, tpCenter, tpRight);
  TdxPageTitleParts = set of TdxPageTitlePart;
  TdxScaleMode = (smAdjust, smFit);
  TdxMarginType = (mtLeft, mtTop, mtRight, mtBottom, mtHeader, mtFooter);
  TdxMarginTypes = set of TdxMarginType;
  TdxPrinterPageUpdateCode = (ucMarginLeft, ucMarginTop, ucMarginRight, ucMarginBottom, ucMarginHeader, ucMarginFooter, ucScale);
  TdxPrinterPageUpdateCodes = set of TdxPrinterPageUpdateCode;

const
  mtAll: TdxMarginTypes = [mtLeft, mtTop, mtRight, mtBottom, mtHeader, mtFooter];
  ucAll = [ucMarginLeft, ucMarginTop, ucMarginRight, ucMarginBottom, ucMarginHeader, ucMarginFooter, ucScale];
  ucMargins = [ucMarginLeft, ucMarginTop, ucMarginRight, ucMarginBottom, ucMarginHeader, ucMarginFooter];
  uaMarginsVert = [ucMarginLeft, ucMarginRight];
  uaMarginsHorz = [ucMarginTop, ucMarginBottom];

  dxDefaultDMPaper = Windows.DMPAPER_FIRST;
  dxDefaultPaperSource = Windows.DMBIN_AUTO;

// Aliases
type
  TdxMeasurementUnits = dxMeasurementUnits.TdxMeasurementUnits;

const
  muDefault = dxMeasurementUnits.muDefault;
  muInches = dxMeasurementUnits.muInches;
  muMillimeters = dxMeasurementUnits.muMillimeters;

type
  TdxPrinterPage = class;

  TdxPrinterPageObjectClass = class of TdxPrinterPageObject;

  TdxPrinterPageObject = class(TdxBaseObject)
  private
    FBackground: TdxBackground;
    FPage: TdxPrinterPage;

    procedure SetBackground(Value: TdxBackground);
  protected
    procedure DoAssign(Source: TdxBaseObject); override;
    procedure LockUpdate(ALockState: TdxLockState); override;
    procedure Changed; dynamic;
    procedure ChangeScale(M, D: Integer); virtual;

    function CreateBackground: TdxBackground; virtual;
    function GetBackgroundClass: TdxBackgroundClass; virtual;
    procedure InitializeBackground(ABackground: TdxBackground); virtual;

    property Background: TdxBackground read FBackground write SetBackground;
    property Page: TdxPrinterPage read FPage;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TCustomdxPageObject }

  TCustomdxPageObjectClass = class of TCustomdxPageObject;

  TCustomdxPageObject = class(TdxPrinterPageObject)
  strict private
    FAdjustOnScale: Boolean;
    FAlignWithMargins: Boolean;
    FDefaultFont: TFont;
    FFont: TFont;
    FTextAlignY: array[TdxPageTitlePart] of TcxTextAlignY;
    FTitles: array[TdxPageTitlePart] of TStrings;

    function GetPartialTextAlignY(Index: Integer): TcxTextAlignY;
    function GetPartialTitle(Index: Integer): TStrings;
    function GetTextAlignY(Index: TdxPageTitlePart): TcxTextAlignY;
    function GetTitle(Index: TdxPageTitlePart): TStrings;
    function IsFontStored: Boolean;
    procedure SetAdjustOnScale(const Value: Boolean);
    procedure SetAlignWithMargins(const Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetPartialTextAlignY(Index: Integer; Value: TcxTextAlignY);
    procedure SetPartialTitle(Index: Integer; Value: TStrings);
    procedure SetTextAlignY(Index: TdxPageTitlePart; Value: TcxTextAlignY);
    procedure SetTitle(Index: TdxPageTitlePart; Value: TStrings);

    procedure FontChanged(Sender: TObject);
    procedure TitleChanged(Sender: TObject);

    procedure FreeAndNilTitles;
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DoAssign(Source: TdxBaseObject); override;
    procedure InitializeDefaultFont(AFont: TFont); virtual;
    function IsTitleStored(Index: Integer): Boolean; virtual;

    property AdjustOnScale: Boolean read FAdjustOnScale write SetAdjustOnScale;
    property AlignWithMargins: Boolean read FAlignWithMargins write SetAlignWithMargins;
  public
    constructor Create; override;
    destructor Destroy; override;
    function DefaultFont: TFont; virtual;
    function IsEqual(ABaseObject: TdxBaseObject): Boolean; override;

    property CenterTextAlignY: TcxTextAlignY Index 1 read GetPartialTextAlignY write SetPartialTextAlignY default taCenterY;
    property CenterTitle: TStrings Index 1 read GetPartialTitle write SetPartialTitle stored IsTitleStored;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property LeftTextAlignY: TcxTextAlignY Index 0 read GetPartialTextAlignY write SetPartialTextAlignY default taCenterY;
    property LeftTitle: TStrings Index 0 read GetPartialTitle write SetPartialTitle stored IsTitleStored;
    property Page;
    property RightTextAlignY: TcxTextAlignY Index 2 read GetPartialTextAlignY write SetPartialTextAlignY default taCenterY;
    property RightTitle: TStrings Index 2 read GetPartialTitle write SetPartialTitle stored IsTitleStored;
    property TextAlignY[Index: TdxPageTitlePart]: TcxTextAlignY read GetTextAlignY write SetTextAlignY;
    property Titles[Index: TdxPageTitlePart]: TStrings read GetTitle write SetTitle; default;
  published
    property Background;
  end;

  { TdxPageFooter }

  TdxPageFooterClass = class of TdxPageFooter;

  TdxPageFooter = class(TCustomdxPageObject)
  protected
    function IsTitleStored(Index: Integer): Boolean; override;
  published
    property CenterTextAlignY;
    property CenterTitle;
    property Font;
    property LeftTextAlignY;
    property LeftTitle;
    property RightTextAlignY;
    property RightTitle;
  end;

  { TdxPageHeader }

  TdxPageHeaderClass = class of TdxPageHeader;

  TdxPageHeader = class(TCustomdxPageObject)
  protected
    function IsTitleStored(Index: Integer): Boolean; override;
  published
    property CenterTextAlignY;
    property CenterTitle;
    property Font;
    property LeftTextAlignY;
    property LeftTitle;
    property RightTextAlignY;
    property RightTitle;
  end;

  TdxPrinterPageClass = class of TdxPrinterPage;

  TdxPrinterPage = class(TdxPrinterPageObject, IcxImageCollectionListener)
  public const
    MinScaleFactor = 10;
    MaxScaleFactor = 500;
  strict private
    FAssigning: Boolean;
    FAutoSwapMargins: Boolean;
    FCenterOnPageH: Boolean;
    FCenterOnPageV: Boolean;
    FDMPaper: Integer;
    FFitToPagesHorizontally: Integer;
    FFitToPagesVertically: Integer;
    FGrayShading: Boolean;
    FHFG: TdxRectWrapper;
    FImageCollection: TcxImageCollection;
    FLastMU: TdxMeasurementUnits;
    FMargins: TdxRectWrapper;
    FMeasurementUnits: TdxMeasurementUnits;
    FMinMargins: TdxRectWrapper;
    FOrientation: TdxPrinterOrientation;
    FPageFooter: TdxPageFooter;
    FPageHeader: TdxPageHeader;
    FPageOrder: TdxPageOrder;
    FPageSize: TdxPointWrapper;
    FPageSizeChanged: Boolean;
    FPageSizeLocked: Boolean;
    FPaperSource: Integer;
    FReverseTitlesOnEvenPages: Boolean;
    FScaleFactor: Integer;
    FScaleMode: TdxScaleMode;

    FOnChange: TNotifyEvent;
    FOnMarginChange: TNotifyEvent;

    function GetCanSwapMargins: Boolean;
    function GetHFG(index: Integer): Integer;
    function GetRealMeasurementUnits: TdxMeasurementUnits;
    function GetRealPageSize: TPoint;
    function GetRestPageSizeX: Integer;
    function GetRestPageSizeY: Integer;
    function IsMinMarginsStored: Boolean;
    function IsPageSizeStored: Boolean;
    procedure SetDMPaper(Value: Integer);
    procedure SetFitToPagesHorizontally(AValue: Integer);
    procedure SetFitToPagesVertically(AValue: Integer);
    procedure SetHFG(index: Integer; Value: Integer);
    procedure SetImageCollection(AValue: TcxImageCollection);
    procedure SetMargins(Value: TdxRectWrapper);
    procedure SetMeasurementUnits(Value: TdxMeasurementUnits);
    procedure SetMinMargins(Value: TdxRectWrapper);
    procedure SetOrientation(Value: TdxPrinterOrientation);
    procedure SetPageFooter(Value: TdxPageFooter);
    procedure SetPageHeader(Value: TdxPageHeader);
    procedure SetPageSize(Value: TdxPointWrapper);
    procedure SetPaperSource(Value: Integer);
    procedure SetPixelsPerInch(Value: Integer);
    procedure SetRealPageSize(const Value: TPoint);
    procedure SetScaleFactor(Value: Integer);
    procedure SetScaleMode(const Value: TdxScaleMode);

    procedure HFGChanged(Sender: TObject; ASides: TdxRectSides);
    procedure HFGChanging(Sender: TObject; ASides: TdxRectSides; var Values: array of Integer);
    procedure LockUpdated(Sender: TdxBaseObject; ALockState: TdxLockState);
    procedure MarginsChanged(Sender: TObject; ASides: TdxRectSides);
    procedure MarginsChanging(Sender: TObject; ASides: TdxRectSides; var Values: array of Integer);
    procedure MinMarginsChanged(Sender: TObject; ASides: TdxRectSides);
    procedure MinMarginsChanging(Sender: TObject; ASides: TdxRectSides; var Values: array of Integer);
    procedure PageSizeChanged(Sender: TObject; ACoords: TdxPointCoords);
    procedure PageSizeChanging(Sender: TObject; ACoords: TdxPointCoords; var Values: array of Integer);

    procedure ReadInt(Reader: TReader);
    procedure ReadLastMU(Reader: TReader);
    procedure ReadMU(Reader: TReader);
    procedure WriteInt(Writer: TWriter);
    procedure WriteLastMU(Writer: TWriter);
    procedure WriteMU(Writer: TWriter);

    procedure DoSwapMargins;
    procedure FindDMPaperByPageSize;
    procedure FixMargins;
    procedure FixMinMargins;
    function IsLoading: Boolean;
    procedure SetPaperSizeByDMPaper;
  protected
    constructor CreateInstance(Dummy: Integer = 0);
    procedure DefineProperties(Filer: TFiler); override;

    procedure Changed; override;
    procedure ChangeScale(M, D: Integer); override;
    procedure DoAssign(Source: TdxBaseObject); override;
    function GetCurrentDPI: Integer; virtual;
    function GetSupportsScaling: Boolean; virtual;
    function IsDefaultDMPaperSelected: Boolean;
    function IsPageFooterTitleStored(Index: Integer): Boolean; virtual;
    function IsPageHeaderTitleStored(Index: Integer): Boolean; virtual;
    procedure LockUpdate(ALockState: TdxLockState); override;
    procedure MarginChanged; dynamic;
    procedure PageParamsChanged(AUpdateCodes: TdxPrinterPageUpdateCodes); virtual;

    function CreatePageFooter: TdxPageFooter; virtual;
    function GetPageFooterClass: TdxPageFooterClass; virtual;
    procedure InitializePageFooter(APageFooter: TdxPageFooter); virtual;

    function CreatePageHeader: TdxPageHeader; virtual;
    function GetPageHeaderClass: TdxPageHeaderClass; virtual;
    procedure InitializePageHeader(APageHeader: TdxPageHeader); virtual;

    procedure DoUpdateMeasurementUnits;
    procedure ReadPrinterInfos;
    procedure SynchronizeMeasurementUnits;

    // IcxImageCollectionListener
    procedure ImageCollectionChanged;
    procedure ImageCollectionDestroyed;

    property CanSwapMargins: Boolean read GetCanSwapMargins;
    property PixelsPerInch: Integer write SetPixelsPerInch;
    property RestPageSizeX: Integer read GetRestPageSizeX;
    property RestPageSizeY: Integer read GetRestPageSizeY;
    property SupportsScaling: Boolean read GetSupportsScaling;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetNamePath: string; override;

    procedure ApplyToPrintDevice;
    procedure InitFromPrintDevice;
    procedure FixMarginsOutside;
    function GetInnerMeasurementUnits: TdxMeasurementUnits;
    procedure GetRealMinMargins(var AMinLeft, AMinRight, AMinTop, AMinBottom: Integer);
    function IsEqual(ABaseObject: TdxBaseObject): Boolean; override;
    procedure RestoreDefaults;

    procedure MapRect2LoMetric(var R: TRect);
    function FooterLoMetric: Integer;
    function FooterRect: TRect;
    function FooterRectLoMetric: TRect;
    function HeaderLoMetric: Integer;
    function HeaderRect: TRect;
    function HeaderRectLoMetric: TRect;
    function MarginsLoMetric: TRect;
    function MinMarginsLoMetric: TRect;
    function MinPrintableArea: Integer;
    function MinPrintableAreaLoMetric: Integer;
    function PageSizeLoMetric: TPoint;
    function PageSizePixels: TPoint;
    function PaintRectLoMetric: TRect;
    function PaintRectPixels: TRect;
    function RealPageSizeLoMetric: TPoint;
    function RealPageSizePixels: TPoint;
    function ValidateMargins: Boolean;

    property RealMeasurementUnits: TdxMeasurementUnits read GetRealMeasurementUnits;
    property RealPageSize: TPoint read GetRealPageSize write SetRealPageSize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMarginChange: TNotifyEvent read FOnMarginChange write FOnMarginChange;
  published
    property AutoSwapMargins: Boolean read FAutoSwapMargins write FAutoSwapMargins default True;
    property Background;
    property CenterOnPageH: Boolean read FCenterOnPageH write FCenterOnPageH default False;
    property CenterOnPageV: Boolean read FCenterOnPageV write FCenterOnPageV default False;
    property DMPaper: Integer read FDMPaper write SetDMPaper{ default dxDefaultDMPaper}; { DMPAPER_FIRST = DMPAPER_LETTER }
    property FitToPagesHorizontally: Integer read FFitToPagesHorizontally write SetFitToPagesHorizontally default 1;
    property FitToPagesVertically: Integer read FFitToPagesVertically write SetFitToPagesVertically default 0;
    property Footer: Integer index 3 read GetHFG write SetHFG default 0;
    property GrayShading: Boolean read FGrayShading write FGrayShading default False;
    property Header: Integer index 1 read GetHFG write SetHFG default 0;
    property ImageCollection: TcxImageCollection read FImageCollection write SetImageCollection;
    property Margins: TdxRectWrapper read FMargins write SetMargins;
    property MeasurementUnits: TdxMeasurementUnits read FMeasurementUnits write SetMeasurementUnits stored False default muDefault;
    property MinMargins: TdxRectWrapper read FMinMargins write SetMinMargins stored IsMinMarginsStored;
    property Orientation: TdxPrinterOrientation read FOrientation write SetOrientation default poPortrait;
    property PageFooter: TdxPageFooter read FPageFooter write SetPageFooter;
    property PageHeader: TdxPageHeader read FPageHeader write SetPageHeader;
    property PageOrder: TdxPageOrder read FPageOrder write FPageOrder default poOverThenDown;
    property PageSize: TdxPointWrapper read FPageSize write SetPageSize stored IsPageSizeStored;
    property PaperSource: Integer read FPaperSource write SetPaperSource default dxDefaultPaperSource; { DMBIN_AUTO = 7 }
    property ReverseTitlesOnEvenPages: Boolean read FReverseTitlesOnEvenPages write FReverseTitlesOnEvenPages default False;
    property ScaleFactor: Integer read FScaleFactor write SetScaleFactor default 100;
    property ScaleMode: TdxScaleMode read FScaleMode write SetScaleMode default smAdjust;
  end;

var
  dxPSPageTitlePartMap: array[0..2] of TdxPageTitlePart = (tpLeft, tpCenter, tpRight);

const
  dxPSDefaltHFFontColor = clBlack;
  dxPSDefaltHFFontName = 'Tahoma';
  dxPSDefaltHFFontHeight = -11;
  dxPSDefaltHFFontStyle = [];

  dxDefaultHFFont = dxPSDefaltHFFontName; // for backward compatibility

function DefaultPrinterPage: TdxPrinterPage;
function GetDefaultMeasurementUnits: TdxMeasurementUnits;
procedure RereadDefaultPrinterPage;
implementation

uses
  Forms, Math, dxPPAttr, dxDPIAwareUtils;

type

  { TdxPageBackground }

  TdxPageBackground = class(TdxBackground)
  protected
    FPageObject: TdxPrinterPageObject;

    procedure DoChange(AChangeWhats: TdxBackgroundParams); override;
  public
    property PageObject: TdxPrinterPageObject read FPageObject;
  end;

var
  FDefaultPrinterPage: TdxPrinterPage = nil;

function DefaultPrinterPage: TdxPrinterPage;
begin
  if FDefaultPrinterPage = nil then
    FDefaultPrinterPage := TdxPrinterPage.CreateInstance(0);
  Result := FDefaultPrinterPage;
end;

procedure RereadDefaultPrinterPage;
begin
  DefaultPrinterPage.ReadPrinterInfos;
end;

function GetDefaultMeasurementUnits: TdxMeasurementUnits;
begin
  Result := dxGetDefaultMeasurementUnits;
end;

{ TdxPageBackground }

procedure TdxPageBackground.DoChange(AChangeWhats: TdxBackgroundParams);
begin
  inherited;
  if PageObject <> nil then
    PageObject.Changed;
end;

{ TdxPrinterPageObject }

constructor TdxPrinterPageObject.Create;
begin
  inherited;
  FBackground := CreateBackground;
end;

destructor TdxPrinterPageObject.Destroy;
begin
  FreeAndNil(FBackground);
  inherited;
end;

procedure TdxPrinterPageObject.DoAssign(Source: TdxBaseObject);
begin
  inherited;
  Background := TdxPrinterPageObject(Source).Background;
end;

procedure TdxPrinterPageObject.LockUpdate(ALockState: TdxLockState);
begin
  inherited;
  if ALockState = lsUnLock then
    Changed;
end;

procedure TdxPrinterPageObject.Changed;
begin
  if (UpdateCount = 0) and (Page <> nil) then Page.Changed;
end;

procedure TdxPrinterPageObject.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

function TdxPrinterPageObject.CreateBackground: TdxBackground;
begin
  Result := GetBackgroundClass.Create;
  InitializeBackground(Result);
end;

function TdxPrinterPageObject.GetBackgroundClass: TdxBackgroundClass;
begin
  Result := TdxPageBackground;
end;

procedure TdxPrinterPageObject.InitializeBackground(ABackground: TdxBackground);
begin
  TdxPageBackground(ABackground).FPageObject := Self;
end;

procedure TdxPrinterPageObject.SetBackground(Value: TdxBackground);
begin
  Background.Assign(Value);
end;

{ TCustomdxPageObject }

constructor TCustomdxPageObject.Create;
var
  I: TdxPageTitlePart;
begin
  inherited;
  FAlignWithMargins := True;
  for I := Low(TdxPageTitlePart) to High(TdxPageTitlePart) do
  begin
    FTitles[I] := TStringList.Create;
    TStringList(FTitles[I]).OnChange := TitleChanged;
    FTextAlignY[I] := taCenterY;
  end;
  FFont := TFont.Create;
  FFont.Assign(DefaultFont);
  FFont.OnChange := FontChanged;
end;

destructor TCustomdxPageObject.Destroy;
begin
  FreeAndNilTitles;
  FreeAndNil(FFont);
  FreeAndNil(FDefaultFont);
  inherited;
end;

function TCustomdxPageObject.DefaultFont: TFont;
begin
  if FDefaultFont = nil then
  begin
    FDefaultFont := TFont.Create;
    InitializeDefaultFont(FDefaultFont);
  end;
  Result := FDefaultFont;
end;

procedure TCustomdxPageObject.ChangeScale(M, D: Integer);
begin
  inherited;
  Font.Height := MulDiv(Font.Height, M, D);
end;

procedure TCustomdxPageObject.DoAssign(Source: TdxBaseObject);
var
  I: TdxPageTitlePart;
begin
  inherited;
  AlignWithMargins := TCustomdxPageObject(Source).AlignWithMargins;
  AdjustOnScale := TCustomdxPageObject(Source).AdjustOnScale;
  for I := Low(TdxPageTitlePart) to High(TdxPageTitlePart) do
  begin
    Titles[I] := TCustomdxPageObject(Source).Titles[I];
    FTextAlignY[I] := TCustomdxPageObject(Source).FTextAlignY[I];
  end;
  Font := TCustomdxPageObject(Source).Font;
end;

procedure TCustomdxPageObject.InitializeDefaultFont(AFont: TFont);
begin
  AFont.Color := dxPSDefaltHFFontColor;
  AFont.Name := dxPSDefaltHFFontName;
  AFont.Height := dxPSDefaltHFFontHeight;
  AFont.Style := dxPSDefaltHFFontStyle;
end;

function TCustomdxPageObject.IsTitleStored(Index: Integer): Boolean;
begin
  Result := True;
end;

function TCustomdxPageObject.GetPartialTextAlignY(Index: Integer): TcxTextAlignY;
begin
  Result := TextAlignY[TdxPageTitlePart(Index)];
end;

function TCustomdxPageObject.GetPartialTitle(Index: Integer): TStrings;
begin
  Result := Titles[TdxPageTitlePart(Index)];
end;

function TCustomdxPageObject.GetTextAlignY(Index: TdxPageTitlePart): TcxTextAlignY;
begin
  Result := FTextAlignY[Index];
end;

function TCustomdxPageObject.GetTitle(Index: TdxPageTitlePart): TStrings;
begin
  Result := FTitles[Index];
end;

function TCustomdxPageObject.IsEqual(ABaseObject: TdxBaseObject): Boolean;
var
  I: TdxPageTitlePart;
begin
  Result := (ABaseObject is TCustomdxPageObject) and
    dxAreFontsEqual(Font, TCustomdxPageObject(ABaseObject).Font) and
    (TCustomdxPageObject(ABaseObject).AdjustOnScale = AdjustOnScale) and
    (TCustomdxPageObject(ABaseObject).AlignWithMargins = AlignWithMargins);

  if Result then
  begin
    for I := Low(TdxPageTitlePart) to High(TdxPageTitlePart) do
    begin
      Result := Result and Titles[I].Equals(TCustomdxPageObject(ABaseObject).Titles[I]) and
        (FTextAlignY[I] = TCustomdxPageObject(ABaseObject).FTextAlignY[I]);
    end;
  end;
end;

function TCustomdxPageObject.IsFontStored: Boolean;
begin
  Result := not dxPSUtl.dxAreFontsEqual(Font, DefaultFont);
end;

procedure TCustomdxPageObject.SetAdjustOnScale(const Value: Boolean);
begin
  if FAdjustOnScale <> Value then
  begin
    FAdjustOnScale := Value;
    Changed;
  end;
end;

procedure TCustomdxPageObject.SetAlignWithMargins(const Value: Boolean);
begin
  if FAlignWithMargins <> Value then
  begin
    FAlignWithMargins := Value;
    Changed;
  end;
end;

procedure TCustomdxPageObject.SetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

procedure TCustomdxPageObject.SetPartialTextAlignY(Index: Integer; Value: TcxTextAlignY);
begin
  TextAlignY[TdxPageTitlePart(Index)] := Value;
end;

procedure TCustomdxPageObject.SetPartialTitle(Index: Integer; Value: TStrings);
begin
  Titles[TdxPageTitlePart(Index)] := Value;
end;

procedure TCustomdxPageObject.SetTextAlignY(Index: TdxPageTitlePart; Value: TcxTextAlignY);
begin
  if FTextAlignY[Index] <> Value then
  begin
    FTextAlignY[Index] := Value;
    Changed;
  end;
end;

procedure TCustomdxPageObject.SetTitle(Index: TdxPageTitlePart; Value: TStrings);
begin
  Titles[Index].Assign(Value);
end;

procedure TCustomdxPageObject.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCustomdxPageObject.TitleChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCustomdxPageObject.FreeAndNilTitles;
var
  I: TdxPageTitlePart;
begin
  for I := Low(TdxPageTitlePart) to High(TdxPageTitlePart) do
    FreeAndNil(FTitles[I]);
end;

{ TdxPageFooter }

function TdxPageFooter.IsTitleStored(Index: Integer): Boolean;
begin
  if Page <> nil then
    Result := Page.IsPageFooterTitleStored(Index)
  else
    Result := inherited IsTitleStored(Index);
end;

{ TdxPageHooter }

function TdxPageHeader.IsTitleStored(Index: Integer): Boolean;
begin
  if Page <> nil then
    Result := Page.IsPageHeaderTitleStored(Index)
  else
    Result := inherited IsTitleStored(Index);
end;

{ TdxPrinterPage }

constructor TdxPrinterPage.Create;
begin
  CreateInstance(0);
  if FDefaultPrinterPage = nil then
    RereadDefaultPrinterPage;
  RestoreDefaults;

  OnLockUpdate := LockUpdated;
  FMargins.OnChanged := MarginsChanged;
  FMargins.OnChanging := MarginsChanging;
  FHFG.OnChanging := HFGChanging;
  FHFG.OnChanged := HFGChanged;
  FMinMargins.OnChanging := MinMarginsChanging;
  FMinMargins.OnChanged := MinMarginsChanged;
  FPageSize.OnChanging := PageSizeChanging;
  FPageSize.OnChanged := PageSizeChanged;
end;

destructor TdxPrinterPage.Destroy;
begin
  ImageCollection := nil;
  FreeAndNil(FMargins);
  FreeAndNil(FMinMargins);
  FreeAndNil(FHFG);
  FreeAndNil(FPageSize);
  FreeAndNil(FPageHeader);
  FreeAndNil(FPageFooter);
  inherited Destroy;
end;

function TdxPrinterPage.GetNamePath: string;
begin
  Result := ClassName;
end;

procedure TdxPrinterPage.ApplyToPrintDevice;
var
  APaperInfo: TdxPaperInfo;
begin
  if dxPrnDev.dxInitPrintDevice(False) then
  begin
    dxPrnDev.dxPrintDevice.ColorMode := not GrayShading;
    dxPrnDev.dxPrintDevice.Orientation := Orientation;
    dxPrnDev.dxPrintDevice.SelectPaper(DMPaper);
    if DMPaper >= DMPAPER_USER then
    begin
      APaperInfo := TdxPaperInfo(dxPrnDev.dxPrintDevice.Papers.Objects[dxPrnDev.dxPrintDevice.PaperIndex]);
      APaperInfo.Width := PageSizeLoMetric.X;
      APaperInfo.Height := PageSizeLoMetric.Y;
    end;
    dxPrnDev.dxPrintDevice.SelectBin(PaperSource);
  end;
end;

procedure TdxPrinterPage.InitFromPrintDevice;
begin
  GrayShading := not Boolean(dxPrnDev.dxPrintDevice.ColorMode);
  if Orientation <> poAuto then
    Orientation := dxPrnDev.dxPrintDevice.Orientation;
end;

procedure TdxPrinterPage.FixMarginsOutside;
var
  AMinLeft, AMinRight, AMinTop, AMinBottom: Integer;
begin
  GetRealMinMargins(AMinLeft, AMinRight, AMinTop, AMinBottom);
  BeginUpdate;
  try
    if Header < AMinTop then
      Header := AMinTop;
    if Footer < AMinBottom then
      Footer := AMinBottom;
    if Margins.Left < AMinLeft then
      Margins.Left := AMinLeft;
    if Margins.Right < AMinRight then
      Margins.Right := AMinRight;
    FixMargins;
  finally
    EndUpdate;
  end;
end;

function TdxPrinterPage.GetInnerMeasurementUnits: TdxMeasurementUnits;
begin
  Result := dxGetActualMeasurementUnits(MeasurementUnits);
end;

procedure TdxPrinterPage.GetRealMinMargins(var AMinLeft, AMinRight, AMinTop, AMinBottom: Integer);
begin
  AMinLeft := 0;
  AMinRight := 0;
  AMinTop := 0;
  AMinBottom := 0;
  if dxInitPrintDevice(False) then
  begin
    AMinLeft :=
      MulDiv(dxPrintDevice.PhysOffsetX, 1000, GetDeviceCaps(dxPrintDevice.Handle, LOGPIXELSX));
    AMinTop :=
      MulDiv(dxPrintDevice.PhysOffsetY, 1000, GetDeviceCaps(dxPrintDevice.Handle, LOGPIXELSY));
    case Page.GetInnerMeasurementUnits of
      muInches:;
      muMillimeters:
        begin
          AMinLeft := MulDiv(AMinLeft, 254, 10);
          AMinTop := MulDiv(AMinTop, 254, 10);
        end;
    end;
    AMinRight := AMinLeft;
    AMinBottom := AMinTop;
  end;
  if MinMargins.Left > AMinLeft then
    AMinLeft := MinMargins.Left;
  if MinMargins.Right > AMinRight then
    AMinRight := MinMargins.Right;
  if MinMargins.Top > AMinTop then
    AMinTop := MinMargins.Top;
  if MinMargins.Bottom > AMinBottom then
    AMinBottom := MinMargins.Bottom;
end;

function TdxPrinterPage.IsEqual(ABaseObject: TdxBaseObject): Boolean;
begin
  Result := inherited IsEqual(ABaseObject) and
    Background.IsEqual(TdxPrinterPage(ABaseObject).Background) and
    (CenterOnPageH = TdxPrinterPage(ABaseObject).CenterOnPageH) and
    (CenterOnPageV = TdxPrinterPage(ABaseObject).CenterOnPageV) and
    (PageOrder = TdxPrinterPage(ABaseObject).PageOrder) and
    (GrayShading = TdxPrinterPage(ABaseObject).GrayShading) and
    (ScaleFactor = TdxPrinterPage(ABaseObject).ScaleFactor) and
    (ReverseTitlesOnEvenPages = TdxPrinterPage(ABaseObject).ReverseTitlesOnEvenPages) and
    FMargins.IsEqual(TdxPrinterPage(ABaseObject).Margins.Rect) and
    FMinMargins.IsEqual(TdxPrinterPage(ABaseObject).MinMargins.Rect) and
    PageSize.IsEqual(TdxPrinterPage(ABaseObject).PageSize.Point) and
    (MeasurementUnits = TdxPrinterPage(ABaseObject).MeasurementUnits) and
    (Orientation = TdxPrinterPage(ABaseObject).Orientation) and
    (DMPaper = TdxPrinterPage(ABaseObject).DMPaper) and
    (PaperSource = TdxPrinterPage(ABaseObject).PaperSource) and
    PageHeader.IsEqual(TdxPrinterPage(ABaseObject).PageHeader) and
    PageFooter.IsEqual(TdxPrinterPage(ABaseObject).PageFooter) and
    FHFG.IsEqual(TdxPrinterPage(ABaseObject).FHFG.Rect);
end;

procedure TdxPrinterPage.RestoreDefaults;
begin
  if (Self <> FDefaultPrinterPage) and (FDefaultPrinterPage <> nil) then
    Assign(DefaultPrinterPage);
end;

procedure TdxPrinterPage.MapRect2LoMetric(var R: TRect);
begin
  case GetInnerMeasurementUnits of
    muInches:
      R := cxRectScale(TdxInchesUnits.ToMM(R), 1, 100, 1, 100);
    muMillimeters:
      R := cxRectScale(R, 1, 100, 1, 100);
  end;
end;

function TdxPrinterPage.FooterLoMetric: Integer;
begin
  case GetInnerMeasurementUnits of
    muInches:
      Result := MulDiv(TdxInchesUnits.ToMM(Footer), 1, 100);
    else {muMillimeters}
      Result := MulDiv(Footer, 1, 100);
  end;
end;

function TdxPrinterPage.FooterRect: TRect;
begin
  if PageHeader.AlignWithMargins then
    Result := Margins.Rect
  else
    Result := FDefaultPrinterPage.Margins.Rect;

  Result := Rect(Result.Left, RealPageSize.Y - Margins.Bottom, RealPageSize.X - Result.Right, RealPageSize.Y - Footer);
end;

function TdxPrinterPage.FooterRectLoMetric: TRect;
begin
  Result := FooterRect;
  MapRect2LoMetric(Result);
end;

function TdxPrinterPage.HeaderLoMetric: Integer;
begin
  case GetInnerMeasurementUnits of
    muInches:
      Result := MulDiv(TdxInchesUnits.ToMM(Header), 1, 100);
    else {muMillimeters}
      Result := MulDiv(Header, 1, 100);
  end;
end;

function TdxPrinterPage.HeaderRect: TRect;
begin
  if PageHeader.AlignWithMargins then
    Result := Margins.Rect
  else
    Result := FDefaultPrinterPage.Margins.Rect;

  Result := Rect(Result.Left, Header, RealPageSize.X - Result.Right, Margins.Top);
end;

function TdxPrinterPage.HeaderRectLoMetric: TRect;
begin
  Result := HeaderRect;
  MapRect2LoMetric(Result);
end;

function TdxPrinterPage.MarginsLoMetric: TRect;
begin
  case GetInnerMeasurementUnits of
    muInches:
      Result := cxRectScale(TdxInchesUnits.ToMM(Margins.Rect), 1, 100, 1, 100);
    else {muMillimeters}
      Result := cxRectScale(Margins.Rect, 1, 100, 1, 100);
  end;
end;

function TdxPrinterPage.MinMarginsLoMetric: TRect;
begin
  case GetInnerMeasurementUnits of
    muInches:
      Result := cxRectScale(TdxInchesUnits.ToMM(MinMargins.Rect), 1, 100, 1, 100);
    else {muMillimeters}
      Result := cxRectScale(MinMargins.Rect, 1, 100, 1, 100);
  end;
end;

function TdxPrinterPage.MinPrintableArea: Integer;
begin
  Result := dxDefaultMinPrintableArea;
  case GetInnerMeasurementUnits of
    muInches:
      Result := TdxMillimetersUnits.ToInch(Result);
  end;
end;

function TdxPrinterPage.MinPrintableAreaLoMetric: Integer;
begin
  Result := MulDiv(dxDefaultMinPrintableArea, 1, 100);
end;

function TdxPrinterPage.PageSizeLoMetric: TPoint;
begin
  case GetInnerMeasurementUnits of
    muInches:
      Result := ScalePoint(TdxInchesUnits.ToMM(PageSize.Point), 1, 100);
    else {muMillimeters}
      Result := ScalePoint(PageSize.Point, 1, 100);
  end;
end;

function TdxPrinterPage.PageSizePixels: TPoint;
begin
  Result := ScalePoint(RealPageSizeLoMetric, GetCurrentDPI, 254);
end;

function TdxPrinterPage.PaintRectLoMetric: TRect;
var
  Margins: TRect;
  PageSize: TPoint;
begin
  Margins := MarginsLoMetric;
  PageSize := RealPageSizeLoMetric;
  with Result do
  begin
    Left := Margins.Left;
    Top := Margins.Top;
    Right := PageSize.X - Margins.Right;
    Bottom := PageSize.Y - Margins.Bottom;
    if (Right < Left) or (Bottom < Top) then
    begin
      Self.Margins.Empty;
      Result := Rect(0, 0, PageSize.X, PageSize.Y);
    end;
  end;
end;

function TdxPrinterPage.PaintRectPixels: TRect;
begin
  Result := cxRectScale(PaintRectLoMetric, GetCurrentDPI, 254);
end;

function TdxPrinterPage.RealPageSizeLoMetric: TPoint;
var
  V: Integer;
begin
  Result := PageSizeLoMetric;
  if Orientation = poLandscape then
    with Result do
    begin
      V := X;
      X := Y;
      Y := V;
    end;
end;

function TdxPrinterPage.RealPageSizePixels: TPoint;
var
  V: Integer;
begin
  Result := PageSizePixels;
  if Orientation = poLandscape then
    with Result do
    begin
      V := X;
      X := Y;
      Y := V;
    end;
end;

function TdxPrinterPage.ValidateMargins: Boolean;
var
  OffsetX, OffsetY: Integer;
begin
  // PS v3.01: Don't ask a Printer until it's not initialized outside, for instance, first printout from PS

  if dxPrnDev.dxIsPrintDeviceInitialized and (dxPrintDevice.PrinterCount > 0) then
    with dxPrintDevice do
    begin
      OffsetX := MulDiv(PhysOffsetX, 254, GetDeviceCaps(Handle, LOGPIXELSX));
      OffsetY := MulDiv(PhysOffsetY, 254, GetDeviceCaps(Handle, LOGPIXELSY));
    end
  else
    with dxDefaultPhysicalPaperOffsets do
    begin
      OffsetX := X div 100;
      OffsetY := Y div 100;
    end;

  with MarginsLoMetric do
    Result := (Left >= OffsetX) and (Right >= OffsetX) and
    (HeaderLoMetric >= OffsetY) and (FooterLoMetric >= OffsetY);
end;

constructor TdxPrinterPage.CreateInstance(Dummy: Integer = 0);
begin
  inherited Create;
  FPage := Self;

  FAutoSwapMargins := True;
  FCenterOnPageH := False;
  FCenterOnPageV := False;
  FMeasurementUnits := muDefault;
  FLastMU := GetInnerMeasurementUnits;
  FMargins := TdxRectWrapper.CreateEmpty;
  FMinMargins := TdxRectWrapper.CreateEmpty;
  FOrientation := poPortrait;
  FPageSize := TdxPointWrapper.CreateEmpty;
  FHFG := TdxRectWrapper.CreateEmpty;
  FDMPaper := dxPrnDev.dxGetDefaultDMPaper;
  FGrayShading := False;
  FPageFooter := CreatePageFooter;
  FPageHeader := CreatePageHeader;
  FPageOrder := poOverThenDown;
  FPaperSource := dxDefaultPaperSource;{Windows.DMBIN_AUTO}
  FReverseTitlesOnEvenPages := False;
  FFitToPagesHorizontally := 1;
  FFitToPagesVertically := 0;
  FScaleFactor := 100;
  FScaleMode := smAdjust;
end;

procedure TdxPrinterPage.DefineProperties(Filer: TFiler);

  function HasData: Boolean;
  begin
    Result := Filer.Ancestor = nil;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('_dxMeasurementUnits_', ReadMU, WriteMU, HasData);
  Filer.DefineProperty('_dxLastMU_', ReadLastMU, WriteLastMU, HasData);
  Filer.DefineProperty('FitToPagesByTall', ReadInt, WriteInt, False);
  Filer.DefineProperty('FitToPagesByWide', ReadInt, WriteInt, False);
  if Filer is TReader then
    SynchronizeMeasurementUnits;
end;

procedure TdxPrinterPage.Changed;
begin
  if (UpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TdxPrinterPage.ChangeScale(M, D: Integer);
begin
  inherited;
  PageHeader.ChangeScale(M, D);
  PageFooter.ChangeScale(M, D);
end;

procedure TdxPrinterPage.DoAssign(Source: TdxBaseObject);
begin
  FAssigning := True;
  try
    inherited;
    with TdxPrinterPage(Source) do
    begin
      Self.AutoSwapMargins := AutoSwapMargins;
      Self.Background := Background;
      Self.CenterOnPageH := CenterOnPageH;
      Self.CenterOnPageV := CenterOnPageV;
      Self.MeasurementUnits := MeasurementUnits;
      Self.FitToPagesHorizontally := FitToPagesHorizontally;
      Self.FitToPagesVertically := FitToPagesVertically;
      Self.PageSize := PageSize;
      Self.DMPaper := DMPaper;
      Self.GrayShading := GrayShading;
      Self.MinMargins.Rect := MinMargins.Rect;
      Self.Margins := Margins;
      Self.Header := Header;
      Self.Footer := Footer;
      Self.Orientation := Orientation;
      Self.PageHeader := PageHeader;
      Self.PageFooter := PageFooter;
      Self.PageOrder := PageOrder;
      Self.PaperSource :=  PaperSource;
      Self.ReverseTitlesOnEvenPages := ReverseTitlesOnEvenPages;
      Self.ImageCollection := ImageCollection;
      Self.ScaleFactor := ScaleFactor;
      Self.ScaleMode := ScaleMode;
    end;
  finally
    FAssigning := False;
  end;
end;

function TdxPrinterPage.GetCurrentDPI: Integer;
begin
  Result := dxSystemScaleFactor.TargetDPI;
end;

function TdxPrinterPage.GetSupportsScaling: Boolean;
begin
  Result := True;
end;

function TdxPrinterPage.IsDefaultDMPaperSelected: Boolean;
begin
  Result := FDMPaper = dxPrnDev.dxGetDefaultDMPaper;
end;

function TdxPrinterPage.IsPageFooterTitleStored(Index: Integer): Boolean;
begin
  Result := True;
end;

function TdxPrinterPage.IsPageHeaderTitleStored(Index: Integer): Boolean;
begin
  Result := True;
end;

procedure TdxPrinterPage.LockUpdate(ALockState: TdxLockState);
begin
  inherited LockUpdate(ALockState);

  if ALockState = lsUnlock then
  begin
    if FPageSizeChanged then
      PageSizeChanged(Self, [pcX, pcY]);
  end;
end;

procedure TdxPrinterPage.MarginChanged;
begin
  if Assigned(FOnMarginChange) then
    FOnMarginChange(Self);
end;

procedure TdxPrinterPage.PageParamsChanged(AUpdateCodes: TdxPrinterPageUpdateCodes);
begin
  if (UpdateCount = 0) and (ucMargins * AUpdateCodes <> []) then
    MarginChanged;
end;

function TdxPrinterPage.CreatePageFooter: TdxPageFooter;
begin
  Result := GetPageFooterClass.Create;
  InitializePageFooter(Result);
end;

function TdxPrinterPage.GetPageFooterClass: TdxPageFooterClass;
begin
  Result := TdxPageFooter;
end;

procedure TdxPrinterPage.InitializePageFooter(APageFooter: TdxPageFooter);
begin
  APageFooter.FPage := Self;
end;

function TdxPrinterPage.CreatePageHeader: TdxPageHeader;
begin
  Result := GetPageHeaderClass.Create;
  InitializePageHeader(Result);
end;

function TdxPrinterPage.GetPageHeaderClass: TdxPageHeaderClass;
begin
  Result := TdxPageHeader;
end;

procedure TdxPrinterPage.InitializePageHeader(APageHeader: TdxPageHeader);
begin
  APageHeader.FPage := Self;
end;

procedure TdxPrinterPage.ImageCollectionChanged;
begin
  Changed;
end;

procedure TdxPrinterPage.ImageCollectionDestroyed;
begin
  ImageCollection := nil;
end;

procedure TdxPrinterPage.DoUpdateMeasurementUnits;
begin
  BeginUpdate;
  try
    case FLastMU of
      muMillimeters:
        begin
          PageSize.Point := TdxInchesUnits.ToMM(PageSize.Point);
          MinMargins.Rect := TdxInchesUnits.ToMM(MinMargins.Rect);
          Header := TdxInchesUnits.ToMM(Header);
          Footer := TdxInchesUnits.ToMM(Footer);
          Margins.Rect := TdxInchesUnits.ToMM(Margins.Rect);
        end;

      muInches:
        begin
          PageSize.Point := TdxMillimetersUnits.ToInch(PageSize.Point);
          MinMargins.Rect := TdxMillimetersUnits.ToInch(MinMargins.Rect);
          Header := TdxMillimetersUnits.ToInch(Header);
          Footer := TdxMillimetersUnits.ToInch(Footer);
          Margins.Rect := TdxMillimetersUnits.ToInch(Margins.Rect);
        end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxPrinterPage.ReadPrinterInfos;
var
  ADMPaper: Integer;
  AHeader, AFooter: Integer;
  AMargins: TRect;
  AOrientation: TdxPrinterOrientation;
  APageSize: TPoint;
begin
  TdxPrintingDefaults.GetDefaultPageInfo(AMargins, AHeader, AFooter, APageSize, ADMPaper, AOrientation);
  FMinMargins.Rect := cxNullRect;
  FMargins.Rect := AMargins;
  FHFG.Top := AHeader;
  FHFG.Bottom := AFooter;
  FPageSize.Point := APageSize;
  FDMPaper := ADMPaper;
  FOrientation := AOrientation;
  InitFromPrintDevice;
end;

procedure TdxPrinterPage.SynchronizeMeasurementUnits;
var
  MU: TdxMeasurementUnits;
begin
  if MeasurementUnits = muDefault then
  begin
    MU := dxGetDefaultMeasurementUnits;
    if FLastMU <> MU then
    begin
      FLastMU := MU;
      DoUpdateMeasurementUnits;
    end;
  end;
end;

function TdxPrinterPage.GetCanSwapMargins: Boolean;
begin
  Result := not IsLoading and not FAssigning and AutoSwapMargins;
end;

function TdxPrinterPage.GetHFG(Index: Integer): Integer;
begin
  Result := FHFG[TdxRectSide(Index)];
end;

function TdxPrinterPage.GetRealMeasurementUnits: TdxMeasurementUnits;
begin
  Result := dxGetActualMeasurementUnits(MeasurementUnits);
end;

function TdxPrinterPage.GetRealPageSize: TPoint;
var
  V: Integer;
begin
  Result := PageSize.Point;
  if Orientation = poLandscape then
    with Result do
    begin
      V := X;
      X := Y;
      Y := V;
    end;
end;

function TdxPrinterPage.GetRestPageSizeX: Integer;
begin
  Result := RealPageSize.X - MinPrintableArea;
end;

function TdxPrinterPage.GetRestPageSizeY: Integer;
begin
  Result := RealPageSize.Y - MinPrintableArea;
end;

function TdxPrinterPage.IsMinMarginsStored: Boolean;
begin
  Result := not MinMargins.IsEmpty(True);
end;

function TdxPrinterPage.IsPageSizeStored: Boolean;
begin
  Result := True;//dxPPAttr.Papers.FindByDMPaper(DMPaper) = -1;
end;

procedure TdxPrinterPage.SetDMPaper(Value: Integer);
begin
  if FDMPaper <> Value then
  begin
    FDMPaper := Value;
    SetPaperSizeByDMPaper;
  end;
end;

procedure TdxPrinterPage.SetFitToPagesHorizontally(AValue: Integer);
var
  AScaleChanged: Boolean;
begin
  AValue := Max(AValue, 0);
  AScaleChanged := FFitToPagesHorizontally <> AValue;
  FFitToPagesHorizontally := AValue;
  ScaleMode := smFit;
  if AScaleChanged then
    PageParamsChanged([ucScale]);
end;

procedure TdxPrinterPage.SetFitToPagesVertically(AValue: Integer);
var
  AScaleChanged: Boolean;
begin
  AValue := Max(AValue, 0);
  AScaleChanged := FFitToPagesVertically <> AValue;
  FFitToPagesVertically := AValue;
  ScaleMode := smFit;
  if AScaleChanged then
    PageParamsChanged([ucScale]);
end;

procedure TdxPrinterPage.SetHFG(Index: Integer; Value: Integer);
begin
  FHFG[TdxRectSide(Index)] := Max(Value, 0);
end;

procedure TdxPrinterPage.SetImageCollection(AValue: TcxImageCollection);
begin
  if FImageCollection <> AValue then
  begin
    if Assigned(FImageCollection) then
      FImageCollection.RemoveListener(Self);
    FImageCollection := AValue;
    if Assigned(FImageCollection) then
      FImageCollection.AddListener(Self);
    ImageCollectionChanged;
  end;
end;

procedure TdxPrinterPage.SetMargins(Value: TdxRectWrapper);
begin
  FMargins.Assign(Value);
end;

procedure TdxPrinterPage.SetMeasurementUnits(Value: TdxMeasurementUnits);

  function HasMUChanged: Boolean;
  var
    DMU: TdxMeasurementUnits;
  begin
    DMU := GetDefaultMeasurementUnits;
    Result := ((Value = muDefault) and (FMeasurementUnits <> DMU)) or
              ((FMeasurementUnits = muDefault) and (Value <> DMU)) or
              ((Value <> muDefault) and (FMeasurementUnits <> muDefault));
  end;

begin
  if FMeasurementUnits <> Value then
    if HasMUChanged then
    begin
      FMeasurementUnits := Value;
      FLastMU := GetInnerMeasurementUnits;
      DoUpdateMeasurementUnits;
    end
    else
      FMeasurementUnits := Value;
end;

procedure TdxPrinterPage.SetMinMargins(Value: TdxRectWrapper);
begin
  MinMargins.Assign(Value);
end;

procedure TdxPrinterPage.SetOrientation(Value: TdxPrinterOrientation);
var
  APrevOrientation: TdxPrinterOrientation;
begin
  if FOrientation <> Value then
  begin
    APrevOrientation := FOrientation;
    FOrientation := Value;
    if dxPrnDev.dxPrintDevice <> nil then
      dxPrnDev.dxPrintDevice.Orientation := FOrientation;
    if CanSwapMargins and ((APrevOrientation = poLandscape) and (FOrientation in [poAuto,poPortrait]) or
      (APrevOrientation in [poPortrait, poAuto]) and (FOrientation = poLandscape)) then
      DoSwapMargins;
  end;
end;

procedure TdxPrinterPage.SetPageFooter(Value: TdxPageFooter);
begin
  PageFooter.Assign(Value);
end;

procedure TdxPrinterPage.SetPageHeader(Value: TdxPageHeader);
begin
  PageHeader.Assign(Value);
end;

procedure TdxPrinterPage.SetPageSize(Value: TdxPointWrapper);
begin
  PageSize.Assign(Value);
end;

procedure TdxPrinterPage.SetPaperSource(Value: Integer);
begin
  if Value < Windows.DMBIN_FIRST then
    Value := Windows.DMBIN_FIRST;
  if (Value > Windows.DMBIN_LAST) and (Value < Windows.DMBIN_USER) then
    Value := Windows.DMBIN_LAST;
  if FPaperSource <> Value then
    FPaperSource := Value;
end;

procedure TdxPrinterPage.SetPixelsPerInch(Value: Integer);
begin
  PageHeader.Font.PixelsPerInch := Value;
  PageFooter.Font.PixelsPerInch := Value;
end;

procedure TdxPrinterPage.SetRealPageSize(const Value: TPoint);
begin
  if Orientation = poLandscape then
    PageSize.Point := Point(Value.Y, Value.X)
  else
    PageSize.Point := Value;
end;

procedure TdxPrinterPage.SetScaleFactor(Value: Integer);
var
  AScaleChanged: Boolean;
begin
  Value := Min(Max(Value, MinScaleFactor), MaxScaleFactor);
  AScaleChanged := ScaleFactor <> Value;
  FScaleFactor := Value;
  ScaleMode := smAdjust;
  if AScaleChanged then
    PageParamsChanged([ucScale]);
end;

procedure TdxPrinterPage.SetScaleMode(const Value: TdxScaleMode);
begin
  if ScaleMode <> Value then
  begin
    FScaleMode := Value;
    PageParamsChanged([ucScale]);
  end;
end;

procedure TdxPrinterPage.HFGChanged(Sender: TObject; ASides: TdxRectSides);
begin
  if UpdateCount <> 0 then Exit;
  BeginUpdate;
  try
    FixMargins;
  finally
    EndUpdate;
  end;
end;

procedure TdxPrinterPage.HFGChanging(Sender: TObject; ASides: TdxRectSides;
  var Values: array of Integer);
begin
  if UpdateCount <> 0 then Exit;
  if rsTop in ASides then
    Values[1] := MinMax(Values[1], FMinMargins.Top, RestPageSizeY - Margins.Bottom);
  if rsBottom in ASides then
    Values[3] := MinMax(Values[3], FMinMargins.Bottom, RestPageSizeY - Margins.Top);
end;

procedure TdxPrinterPage.LockUpdated(Sender: TdxBaseObject; ALockState: TdxLockState);
begin
  if ALockState = lsUnlock then
    PageParamsChanged(ucAll);
end;

procedure TdxPrinterPage.MarginsChanged(Sender: TObject; ASides: TdxRectSides);

  function GetUpdateCode: TdxPrinterPageUpdateCodes;
  begin
    Result := [];
    if rsLeft in ASides then
      Include(Result, ucMarginLeft);
    if rsTop in ASides then
      Include(Result, ucMarginTop);
    if rsRight in ASides then
      Include(Result, ucMarginRight);
    if rsBottom in ASides then
      Include(Result, ucMarginBottom);
  end;

begin
  if not IsLoading and (UpdateCount = 0) then
    PageParamsChanged(GetUpdateCode);
end;

procedure TdxPrinterPage.MarginsChanging(Sender: TObject; ASides: TdxRectSides; var Values: array of Integer);
begin
  if IsLoading or (UpdateCount <> 0) then
    Exit;
  if rsLeft in ASides then
    Values[0] := MinMax(Values[0], FMinMargins.Left, RestPageSizeX - IfThen(rsRight in ASides, Values[2], Margins.Right));
  if rsTop in ASides then
    Values[1] := MinMax(Values[1], Header, RestPageSizeY - IfThen(rsBottom in ASides, Values[3], Margins.Bottom));
  if rsRight in ASides then
    Values[2] := MinMax(Values[2], FMinMargins.Right, RestPageSizeX - IfThen(rsLeft in ASides, Values[0], Margins.Left));
  if rsBottom in ASides then
    Values[3] := MinMax(Values[3], Footer, RestPageSizeY - IfThen(rsTop in ASides, Values[1], Margins.Top));
end;

procedure TdxPrinterPage.MinMarginsChanged(Sender: TObject; ASides: TdxRectSides);
begin
  if IsLoading or (UpdateCount <> 0) then Exit;

  if rsLeft in ASides then
  begin
    FMargins.Left := MinMax(FMargins.Left, FMinMargins.Left, RestPageSizeX);
    FMargins.Right := MinMax(FMargins.Right, FMinMargins.Right, RestPageSizeX - FMargins.Left);
  end;

  if rsTop in ASides then
  begin
    Header := MinMax(Header, FMinMargins.Top, RestPageSizeY);
    Footer := MinMax(Footer, FMinMargins.Bottom, RestPageSizeY - Header);
    FMargins.Top := MinMax(FMargins.Top, Header, RestPageSizeY - Footer);
    FMargins.Bottom := MinMax(FMargins.Bottom, Footer, RestPageSizeY - Header);
  end;

  if rsRight in ASides then
  begin
    FMargins.Right := MinMax(FMargins.Right, FMinMargins.Right, RestPageSizeX);
    FMargins.Left := MinMax(FMargins.Left, FMinMargins.Left, RestPageSizeX - FMargins.Right);
  end;

  if rsBottom in ASides then
  begin
    Footer := MinMax(Footer, FMinMargins.Bottom, RestPageSizeY);
    Header := MinMax(Header, FMinMargins.Top, RestPageSizeY - Footer);
    FMargins.Bottom := MinMax(FMargins.Bottom, Footer, RestPageSizeY - Header);
    FMargins.Top := MinMax(FMargins.Bottom, Header, RestPageSizeY - Footer);
  end;
end;

procedure TdxPrinterPage.MinMarginsChanging(Sender: TObject; ASides: TdxRectSides; var Values: array of Integer);
begin
  if IsLoading or (UpdateCount <> 0) then
    Exit;
  if rsLeft in ASides then
    Values[0] := MinMax(Values[0], 0, RestPageSizeX - MinMargins.Right);
  if rsTop in ASides then
    Values[1] := MinMax(Values[1], 0, RestPageSizeY - MinMargins.Bottom);
  if rsRight in ASides then
    Values[2] := MinMax(Values[2], 0, RestPageSizeX - MinMargins.Left);
  if rsBottom in ASides then
    Values[3] := MinMax(Values[3], 0, RestPageSizeY - MinMargins.Top);
end;

procedure TdxPrinterPage.PageSizeChanged(Sender: TObject; ACoords: TdxPointCoords);
begin
  FPageSizeChanged := True;
  if UpdateCount = 0 then
  begin
    BeginUpdate;
    try
      if not IsLoading then
      begin
        FixMinMargins;
        FixMargins;
      end;
      if not FPageSizeLocked then
        FindDMPaperByPageSize;
      FPageSizeChanged := False;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxPrinterPage.PageSizeChanging(Sender: TObject; ACoords: TdxPointCoords; var Values: array of Integer);
begin
  if IsLoading then
    Exit;
  if pcX in ACoords then
    Values[0] := Max(Values[0], MinPrintableArea);
  if pcY in ACoords then
    Values[1] := Max(Values[1], MinPrintableArea);
end;

procedure TdxPrinterPage.ReadInt(Reader: TReader);
begin
  Reader.ReadInteger;
end;

procedure TdxPrinterPage.ReadLastMU(Reader: TReader);
begin
  FLastMU := TdxMeasurementUnits(Reader.ReadInteger);
end;

procedure TdxPrinterPage.ReadMU(Reader: TReader);
begin
  FMeasurementUnits := TdxMeasurementUnits(Reader.ReadInteger);
end;

procedure TdxPrinterPage.WriteInt(Writer: TWriter);
begin
end;

procedure TdxPrinterPage.WriteLastMU(Writer: TWriter);
begin
  Writer.WriteInteger(Integer(FLastMU));
end;

procedure TdxPrinterPage.WriteMU(Writer: TWriter);
begin
  Writer.WriteInteger(Integer(FMeasurementUnits));
end;

procedure TdxPrinterPage.DoSwapMargins;
var
  V, V2: Integer;
begin
  BeginUpdate;
  try
    if Orientation = poLandscape then
    begin
      V := MinMargins.Left;
      MinMargins.Left := MinMargins.Bottom;
      V2 := MinMargins.Top;
      MinMargins.Top := V;
      V := MinMargins.Right;
      MinMargins.Right := V2;
      MinMargins.Bottom := V;

      V := Margins.Left;
      Margins.Left := Margins.Bottom;
      V2 := Margins.Top;
      Margins.Top := V;
      V := Margins.Right;
      Margins.Right := V2;
      Margins.Bottom := V;
    end
    else
    begin
      V := MinMargins.Bottom;
      MinMargins.Bottom := MinMargins.Left;
      V2 := MinMargins.Right;
      MinMargins.Right := V;
      V := MinMargins.Top;
      MinMargins.Top := V2;
      MinMargins.Left := V;

      V := Margins.Bottom;
      Margins.Bottom := Margins.Left;
      V2 := Margins.Right;
      Margins.Right := V;
      V := Margins.Top;
      Margins.Top := V2;
      Margins.Left := V;
    end;
    FixMargins;
  finally
    EndUpdate;
  end;
end;

procedure TdxPrinterPage.FindDMPaperByPageSize;
var
  Index: Integer;
begin
  with PageSizeLoMetric do
    Index := dxPPAttr.Papers.FindBySize(X, Y);

  if Index <> -1 then
    FDMPaper := dxPPAttr.Papers[Index].DMPaper
  else
    FDMPaper := DMPAPER_USER;
end;

procedure TdxPrinterPage.FixMargins;
begin
  Header := MinMax(Header, FMinMargins.Top, RestPageSizeY - FMinMargins.Bottom);
  Footer := MinMax(Footer, FMinMargins.Bottom, RestPageSizeY - Header);
  FMargins.Left := MinMax(FMargins.Left, FMinMargins.Left, RestPageSizeX - FMinMargins.Right);
  FMargins.Right := MinMax(FMargins.Right, FMinMargins.Right, RestPageSizeX - FMargins.Left);
  FMargins.Top := MinMax(FMargins.Top, Header, RestPageSizeY - Footer);
  FMargins.Bottom := MinMax(FMargins.Bottom, Footer, RestPageSizeY - Header);
end;

procedure TdxPrinterPage.FixMinMargins;
begin
  FMinMargins.Left := MinMax(FMinMargins.Left, 0, RestPageSizeX);
  if RestPageSizeX = 0 then
    FMinMargins.Right := 0
  else
    FMinMargins.Right := MinMax(FMinMargins.Right, 0, RestPageSizeX - FMinMargins.Left);

  FMinMargins.Top := MinMax(FMinMargins.Top, 0, RestPageSizeY);
  if RestPageSizeY = 0 then
    FMinMargins.Bottom := 0
  else
    FMinMargins.Bottom := MinMax(FMinMargins.Bottom, 0, RestPageSizeY - FMinMargins.Top);
end;

function TdxPrinterPage.IsLoading: Boolean;
var
  AOwner: TPersistent;
begin
  AOwner := GetOwner;
  Result := (AOwner is TComponent) and (csLoading in TComponent(AOwner).ComponentState);
end;

procedure TdxPrinterPage.SetPaperSizeByDMPaper;
var
  PaperSize: TPoint;
  PaperIndex: Integer;
begin
  PaperSize := cxNullPoint;
  PaperIndex := dxPPAttr.Papers.FindByDMPaper(DMPaper);
  if PaperIndex = -1 then
  begin
    PaperIndex := dxPrintDevice.FindPaper(DMPaper);
    if PaperIndex <> -1 then
      PaperSize := ScalePoint(TdxPaperInfo(dxPrintDevice.Papers.Objects[PaperIndex]).Size, 100, 1)
  end
  else
    PaperSize := ScalePoint(TPoint(dxPPAttr.Papers[PaperIndex].Size), 100, 1);

  if (PaperSize.X <> 0) and (PaperSize.Y <> 0) then
  begin
    FPageSizeLocked := True;
    try
      case GetInnerMeasurementUnits of
        muInches:
          PageSize.Point := TdxMillimetersUnits.ToInch(PaperSize);
        muMillimeters:
          PageSize.Point := PaperSize;
      end;
    finally
      FPageSizeLocked := False;
    end;
  end
end;

initialization

finalization
  FreeAndNil(FDefaultPrinterPage);

end.

