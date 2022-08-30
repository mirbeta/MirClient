{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxScreenTip;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, Types, Classes, Graphics, SysUtils, Forms, Controls{must be after Forms for D11},
  dxCore, cxClasses, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxCustomHint, dxGDIPlusClasses,
  cxGeometry, dxCoreGraphics, dxTypeHelpers;

const
  dxScreenTipFontColor = $4C4C4C;

type
  TdxScreenTip = class;
  TdxScreenTipCollection = class;
  TdxScreenTipRepository = class;
  TdxScreenTipWindow = class;
  TdxScreenTipStyle = class;

  IdxScreenTipProvider = interface
    ['{1479340F-AA1B-4A8E-B136-87E3B5CA3D36}']
    function GetAction: TBasicAction;
    function GetScreenTip: TdxScreenTip;
    function GetShortCut: string;
  end;

  TdxOnGetScreenTip = procedure(Sender: TObject; var AScreenTip: TdxScreenTip) of object;

  TdxScreenTipBandTextAlign = (stbtaLeft, stbtaRight);
  TdxScreenTipBandType = (stbHeader, stbDescription, stbFooter);

  { TdxCustomScreenTipBand }

  TdxCustomScreenTipBand = class(TPersistent)
  strict private
    FBandType: TdxScreenTipBandType;
    FGlyph: TdxSmartGlyph;
    FGlyphFixedWidth: Boolean;
    FPlainText: Boolean;
    FText: string;
    FTextAlign: TdxScreenTipBandTextAlign;

    FOnChange: TNotifyEvent;

    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TdxSmartGlyph);
    procedure SetPlainText(Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetTextAlign(Value: TdxScreenTipBandTextAlign);
    procedure SetGlyphFixedWidth(AValue: Boolean);
  protected
    procedure Changed; virtual;
    function GetFont: TFont; virtual; abstract;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(ABandType: TdxScreenTipBandType);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function HasGlyph: Boolean;
    function IsDefaultTextColor: Boolean; virtual; abstract;
    function IsVisible(const AHintText: string): Boolean;

    property BandType: TdxScreenTipBandType read FBandType write FBandType;
    property Font: TFont read GetFont;
  published
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property GlyphFixedWidth: Boolean read FGlyphFixedWidth write SetGlyphFixedWidth default True;
    property PlainText: Boolean read FPlainText write SetPlainText default True;
    property Text: string read FText write SetText;
    property TextAlign: TdxScreenTipBandTextAlign read FTextAlign write SetTextAlign default stbtaRight;
  end;

  { TdxScreenTipBand }

  TdxScreenTipBand = class(TdxCustomScreenTipBand)
  strict private
    FScreenTip: TdxScreenTip;
  protected
    function GetFont: TFont; override;
  public
    constructor Create(AScreenTip: TdxScreenTip; ABandType: TdxScreenTipBandType); virtual;
    function IsDefaultTextColor: Boolean; override;

    property ScreenTip: TdxScreenTip read FScreenTip;
  end;

  { TdxScreenTipFooterBand }

  TdxScreenTipFooterBand = class(TdxCustomScreenTipBand)
  strict private
    FRepository: TdxScreenTipRepository;
  protected
    function GetFont: TFont; override;
  public
    constructor Create(ARepository: TdxScreenTipRepository);
    function IsDefaultTextColor: Boolean; override;

    property Repository: TdxScreenTipRepository read FRepository;
  end;

  { TdxScreenTip }

  TdxScreenTip = class(TcxComponentCollectionItem, IdxScaleFactor)
  strict private
    FBands: array[TdxScreenTipBandType] of TdxScreenTipBand;
    FUseHintAsHeader: Boolean;
    FUseStandardFooter: Boolean;
    FWidth: Integer;

    function GetBand(Index: Integer): TdxScreenTipBand;
    function GetScaleFactor: TdxScaleFactor;
    procedure SetBand(Index: Integer; Value: TdxScreenTipBand);
  protected
    function GetCollection: TdxScreenTipCollection; virtual;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    function GetDisplayName: string; override;
    procedure SetName(const Value: TComponentName); override;
    //
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetBandForCalculation(Index: TdxScreenTipBandType): TdxCustomScreenTipBand;
    function GetWidth: Integer; virtual;

    property Collection: TdxScreenTipCollection read GetCollection;
  published
    property Header: TdxScreenTipBand index stbHeader read GetBand write SetBand;
    property Description: TdxScreenTipBand index stbDescription read GetBand write SetBand;
    property Footer: TdxScreenTipBand index stbFooter read GetBand write SetBand;
    property UseHintAsHeader: Boolean read FUseHintAsHeader write FUseHintAsHeader default False;
    property UseStandardFooter: Boolean read FUseStandardFooter write FUseStandardFooter default False;
    property Width: Integer read FWidth write FWidth default 0;
  end;

  { TdxScreenTipCollection }

  TdxScreenTipCollection = class(TcxComponentCollection)
  strict private
    FRepository: TdxScreenTipRepository;

    function GetItem(Index: Integer): TdxScreenTip;
    procedure SetItem(Index: Integer; Value: TdxScreenTip);
  protected
    procedure SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1); override;
    procedure UpdateFonts;
  public
    constructor Create(AParentComponent: TComponent; AItemClass: TcxComponentCollectionItemClass); override;
    function Add: TdxScreenTip;

    property Repository: TdxScreenTipRepository read FRepository;
    property Items[Index: Integer]: TdxScreenTip read GetItem write SetItem; default;
  end;

  { TdxScreenTipRepository }

  TdxScreenTipRepositoryFonts = set of TdxScreenTipBandType;
  TdxScreenTipRepository = class(TcxScalableComponent)
  strict private
    FAssignedFonts: TdxScreenTipRepositoryFonts;
    FFonts: array[TdxScreenTipBandType] of TFont;
    FItems: TdxScreenTipCollection;
    FShowDescription: Boolean;
    FStandardFooter: TdxScreenTipFooterBand;
    FSystemFont: TFont;

    procedure CreateFonts;
    procedure FontChanged(Sender: TObject);
    function GetFont(Index: Integer): TFont;
    function IsFontStored(Index: Integer): Boolean;
    procedure SetFont(Index: Integer; Value: TFont);
    procedure SetItems(AValue: TdxScreenTipCollection);
    procedure SetStandardFooter(Value: TdxScreenTipFooterBand);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function CreateScreenTips: TdxScreenTipCollection; virtual;
    function GetBandFont(ABandType: TdxScreenTipBandType): TFont; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetSystemFont(ABandType: TdxScreenTipBandType): TFont;
    function IsDefaultBandTextColor(ABandType: TdxScreenTipBandType): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AssignedFonts: TdxScreenTipRepositoryFonts read FAssignedFonts write FAssignedFonts default [];
    property Items: TdxScreenTipCollection read FItems write SetItems;
    property DescriptionFont: TFont index stbDescription read GetFont write SetFont stored IsFontStored;
    property FooterFont: TFont index stbFooter read GetFont write SetFont stored IsFontStored;
    property HeaderFont: TFont index stbHeader read GetFont write SetFont stored IsFontStored;
    property StandardFooter: TdxScreenTipFooterBand read FStandardFooter write SetStandardFooter;
    property ShowDescription: Boolean read FShowDescription write FShowDescription default True;
  end;

  { TdxScreenTipPainter }

  TdxScreenTipPainter = class
  strict private
    FPainter: TcxCustomLookAndFeelPainter;
  public
    constructor Create(APainter: TcxCustomLookAndFeelPainter);
    function GetColorPalette: IdxColorPalette; virtual;
    function GetDescriptionTextColor: TColor; virtual;
    function GetFooterLineSize: Integer; virtual;
    function GetTitleTextColor: TColor; virtual;
    procedure DrawBackground(ACanvas: TcxCanvas; const ARect: TRect); virtual;
    procedure DrawFooterLine(ACanvas: TcxCanvas; const ARect: TRect); virtual;
  end;

  { TdxScreenTipLikeHintViewInfoHelper }

  TdxScreenTipLikeHintViewInfoHelper = class(TdxHintViewInfoHelper)
  strict private
    FPainter: TdxScreenTipPainter;
  protected
    procedure CorrectMinSize(var ASize: TSize); override;
    function GetTextRect: TRect; override;
    procedure SetTextColor(ACanvas: TCanvas); override;
  public
    constructor Create(APainter: TdxScreenTipPainter; const AHint, AShortCut: string; const ACursorPos: TPoint);
    procedure Paint(ACanvas: TCanvas); override;
  end;

  { TdxScreenTipLikeHintViewInfo }

  TdxScreenTipLikeHintViewInfo = class(TdxCustomHintViewInfo)
  strict private
    FCursorPos: TPoint;
    FHint: string;
    FPainter: TdxScreenTipPainter;
    FShortCut: string;
  protected
    procedure CreateHelper; override;
    procedure DestroyHelper; override;
  public
    constructor Create(APainter: TcxCustomLookAndFeelPainter; const AHint, AShortCut: string; const ACursorPos: TPoint);
  end;

  { TdxScreenTipBandViewInfo }

  TdxScreenTipBandViewInfo = class
  strict private
    FBand: TdxCustomScreenTipBand;
    FBounds: TRect;
    FGlyphBounds: TRect;
    FGlyphSize: TSize;
    FHintText: string;
    FScaleFactor: TdxScaleFactor;
    FShortCut: string;
    FTextBounds: TRect;
    FTextSize: TSize;
    FUseRightToLeftAlignment: Boolean;
    FUseRightToLeftReading: Boolean;

    function GetTextFlags(AIsCalculate: Boolean = False): Integer;
    function GetTextSize(ACanvas: TCanvas; AWidth: Integer): TSize;
    function IsPlainText: Boolean;
    function IsTextCenter: Boolean;
  protected
    procedure CorrectMinWidth(ACanvas: TCanvas; var AWidth: Integer);
    function GetAvailGlyphWidth: Integer;
    function GetHorzIndent: Integer;
    function GetVertIndent(APainter: TdxScreenTipPainter): Integer;
    function GetText: string;
    function HasGlyph: Boolean;
    function HasText: Boolean;
    procedure Paint(ACanvas: TcxCanvas; APainter: TdxScreenTipPainter);
    procedure CalculateTextHeight(ACanvas: TCanvas; AText: string; var ARect: TRect);

    property Bounds: TRect read FBounds;
    property GlyphBounds: TRect read FGlyphBounds;
    property ScaleFactor: TdxScaleFactor read FScaleFactor;
    property TextBounds: TRect read FTextBounds;
    property TextSize: TSize read FTextSize;
  public
    constructor Create(ABand: TdxCustomScreenTipBand; const AHintText, AShortCut: string; AScaleFactor: TdxScaleFactor);
    procedure Calculate(ACanvas: TCanvas; APainter: TdxScreenTipPainter; ATop, AWidth: Integer);
    function GetHeight(ACanvas: TCanvas; const AWidth: Integer; APainter: TdxScreenTipPainter): Integer;
    function IsVisible: Boolean;

    property Band: TdxCustomScreenTipBand read FBand;
    property UseRightToLeftAlignment: Boolean read FUseRightToLeftAlignment write FUseRightToLeftAlignment;
    property UseRightToLeftReading: Boolean read FUseRightToLeftReading write FUseRightToLeftReading;
  end;

  { TdxScreenTipViewInfoHelper }

  TdxScreenTipViewInfoHelper = class(TdxCustomHintViewInfoHelper)
  strict private
    FBandViewInfos: array of TdxScreenTipBandViewInfo;
    FHintText: string;
    FPainter: TdxScreenTipPainter;
    FScreenTip: TdxScreenTip;
    FShortCut: string;

    procedure CreateBandViewInfos;
    procedure DestroyBandViewInfos;
    function GetBandViewInfos(Index: Integer): TdxScreenTipBandViewInfo;
    function GetBandViewInfosCount: Integer;
  protected
    function CreateBandViewInfo(ABand: TdxCustomScreenTipBand): TdxScreenTipBandViewInfo; virtual;
    function GetScaleFactor: TdxScaleFactor; override;
  public
    constructor Create(AScreenTip: TdxScreenTip; APainter: TdxScreenTipPainter; const AHintText: string; AShortCut: string = '');
    destructor Destroy; override;
    procedure Calculate(ACanvas: TCanvas); override;
    procedure Paint(ACanvas: TCanvas); override;

    property ScreenTip: TdxScreenTip read FScreenTip;
    property BandViewInfosCount: Integer read GetBandViewInfosCount;
    property BandViewInfos[Index: Integer]: TdxScreenTipBandViewInfo read GetBandViewInfos;
  end;

  { TdxScreenTipViewInfo }

  TdxScreenTipViewInfo = class(TdxCustomHintViewInfo)
  strict private
    FHint: string;
    FPainter: TdxScreenTipPainter;
    FScreenTip: TdxScreenTip;
    FShortCut: string;
  protected
    procedure CreateHelper; override;
    procedure DestroyHelper; override;
  public
    constructor Create(AScreenTip: TdxScreenTip; APainter: TcxCustomLookAndFeelPainter; const AHintText: string; AShortCut: string = '');
  end;

  { TdxCustomScreenTipLink }

  TdxScreenTipLinkClass = class of TdxCustomScreenTipLink;
  TdxCustomScreenTipLink = class(TCollectionItem)
  strict private
    FComponent: TComponent;
    FScreenTip: TdxScreenTip;

    procedure SetComponent(const Value: TComponent);
    procedure SetScreenTip(const Value: TdxScreenTip);
  protected
    function GetOwnerComponent: TComponent;
    property Component: TComponent read FComponent write SetComponent;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ScreenTip: TdxScreenTip read FScreenTip write SetScreenTip;
  end;

  { TdxCustomScreenTipLinks }

  TdxCustomScreenTipLinks = class(TCollection)
  strict private
    FScreenTipStyle: TdxScreenTipStyle;
  protected
    function FindLinkByComponent(AComponent: TComponent): TdxCustomScreenTipLink;
    function GetOwner: TPersistent; override;
    function GetOwnerComponent: TComponent;
    function GetScreenTipLinkClass: TdxScreenTipLinkClass; virtual; abstract;
  public
    constructor Create(AScreenTipStyle: TdxScreenTipStyle); virtual;
    property ScreenTipStyle: TdxScreenTipStyle read FScreenTipStyle;
  end;

  { TdxScreenTipLink }

  TdxScreenTipLink = class(TdxCustomScreenTipLink)
  strict private
    function GetControl: TControl;
    procedure SetControl(AValue: TControl);
  published
    property Control: TControl read GetControl write SetControl;
  end;

  { TdxScreenTipLinks }

  TdxScreenTipLinks = class(TdxCustomScreenTipLinks)
  strict private
    function GetItem(Index: Integer): TdxScreenTipLink;
    procedure SetItem(Index: Integer; Value: TdxScreenTipLink);
  protected
    function GetScreenTipLinkClass: TdxScreenTipLinkClass; override;
  public
    function Add: TdxScreenTipLink;
    function FindScreenTipByControl(AControl: TControl): TdxScreenTip;
    property Items[Index: Integer]: TdxScreenTipLink read GetItem write SetItem; default;
  end;

  { TdxScreenTipActionLink }

  TdxScreenTipActionLink = class(TdxCustomScreenTipLink)
  strict private
    function GetAction: TBasicAction;
    procedure SetAction(AValue: TBasicAction);
  published
    property Action: TBasicAction read GetAction write SetAction;
  end;

  { TdxScreenTipActionLinks }

  TdxScreenTipActionLinks = class(TdxCustomScreenTipLinks)
  strict private
    function GetItem(Index: Integer): TdxScreenTipActionLink;
    procedure SetItem(Index: Integer; Value: TdxScreenTipActionLink);
  protected
    function GetScreenTipLinkClass: TdxScreenTipLinkClass; override;
  public
    function Add: TdxScreenTipActionLink;
    function FindScreenTipByAction(AAction: TBasicAction): TdxScreenTip;
    property Items[Index: Integer]: TdxScreenTipActionLink read GetItem write SetItem; default;
  end;

  { TdxScreenTipStyle }

  TdxScreenTipStyle = class(TcxCustomHintStyle)
  strict private
    FScreenTipActionLinks: TdxScreenTipActionLinks;
    FScreenTipLinks: TdxScreenTipLinks;

    procedure SetScreenTipLinks(const Value: TdxScreenTipLinks);
    procedure SetScreenTipActionLinks(const Value: TdxScreenTipActionLinks);
  protected
    procedure DoShowHint(var AHintStr: string; var ACanShow: Boolean; var AHintInfo: THintInfo); override;
    procedure GetScreenTipInfo(AControl: TControl; out AScreenTip: TdxScreenTip; out AShortCut: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ComponentRemoved(AComponent: TComponent); override;
    function GetHintWindowClass: TcxCustomHintWindowClass; override;
    function GetScreenTipBounds(AScreenTip: TdxScreenTip; const AHint: string = ''): TRect;
    procedure ShowScreenTip(X, Y: Integer; AScreenTip: TdxScreenTip; const AHint: string = '');
  published
    property ScreenTipLinks: TdxScreenTipLinks read FScreenTipLinks write SetScreenTipLinks;
    property ScreenTipActionLinks: TdxScreenTipActionLinks read FScreenTipActionLinks write SetScreenTipActionLinks;
  end;

  { TdxScreenTipWindow }

  TdxScreenTipWindow = class(TcxCustomHintWindow)
  strict private
    FBorderWidth: Integer;
    FViewInfo: TdxCustomHintViewInfo;

    function InternalUseRightToLeftAlignment: Boolean;
    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
  protected
    FScreenTip: TdxScreenTip;

    procedure AdjustActivateRect(var ARect: TRect); override;
    procedure EnableRegion; override;
    function HasWindowRegion: Boolean; override;
    procedure Paint; override;
    procedure ShowHint(X, Y: Integer; ACaption, AHint: string; AMaxWidth: Integer = 0); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
  end;

implementation

uses
  StrUtils, Math, Menus, ActnList, dxDrawRichTextUtils, dxDPIAwareUtils;

var
  FBandTextHelpers: array [TdxScreenTipBandType] of TdxRichTextHelper;

function GetTextHelper(ABandType: TdxScreenTipBandType): TdxRichTextHelper;
begin
  if FBandTextHelpers[ABandType] = nil then
    FBandTextHelpers[ABandType] := TdxRichTextHelper.Create;
  Result := FBandTextHelpers[ABandType];
end;

{ TdxScreenTipRepository }

constructor TdxScreenTipRepository.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateFonts;
  FStandardFooter := TdxScreenTipFooterBand.Create(Self);
  FItems := CreateScreenTips;
  FShowDescription := True;
end;

destructor TdxScreenTipRepository.Destroy;
var
  I: TdxScreenTipBandType;
begin
  FreeAndNil(FItems);
  FreeAndNil(FStandardFooter);
  for I := Low(TdxScreenTipBandType) to High(TdxScreenTipBandType) do
    FreeAndNil(FFonts[I]);
  FreeAndNil(FSystemFont);
  inherited Destroy;
end;

procedure TdxScreenTipRepository.Assign(Source: TPersistent);
begin
  if Source is TdxScreenTipRepository then
  begin
    Items := TdxScreenTipRepository(Source).Items;
    StandardFooter := TdxScreenTipRepository(Source).StandardFooter;
    DescriptionFont := TdxScreenTipRepository(Source).DescriptionFont;
    FooterFont := TdxScreenTipRepository(Source).FooterFont;
    HeaderFont := TdxScreenTipRepository(Source).HeaderFont;
    FAssignedFonts := TdxScreenTipRepository(Source).AssignedFonts;
    ShowDescription := TdxScreenTipRepository(Source).ShowDescription;
  end
  else
    inherited Assign(Source);
end;

procedure TdxScreenTipRepository.ChangeScale(M, D: Integer);
var
  AType: TdxScreenTipBandType;
begin
  inherited ChangeScale(M, D);
  for AType := Low(TdxScreenTipBandType) to High(TdxScreenTipBandType) do
  begin
    if AType in AssignedFonts then
      FFonts[AType].Height := MulDiv(FFonts[AType].Height, M, D);
  end;
end;

function TdxScreenTipRepository.CreateScreenTips: TdxScreenTipCollection;
begin
  Result := TdxScreenTipCollection.Create(Self, TdxScreenTip);
end;

function TdxScreenTipRepository.GetBandFont(ABandType: TdxScreenTipBandType): TFont;
begin
  if ABandType in AssignedFonts then
    Result := FFonts[ABandType]
  else
    Result := GetSystemFont(ABandType);
end;

procedure TdxScreenTipRepository.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    if Items[I].Owner = Root then Proc(Items[I]);
end;

function TdxScreenTipRepository.GetFont(Index: Integer): TFont;
begin
  Result := FFonts[TdxScreenTipBandType(Index)];
end;

function TdxScreenTipRepository.GetSystemFont(ABandType: TdxScreenTipBandType): TFont;
begin
  Result := FSystemFont;
  dxAssignFont(Result, Screen.HintFont, ScaleFactor, dxSystemScaleFactor);
  Result.Color := dxScreenTipFontColor;
  if ABandType in [stbHeader, stbFooter] then
    Result.Style := Result.Style + [fsBold];
end;

function TdxScreenTipRepository.IsDefaultBandTextColor(ABandType: TdxScreenTipBandType): Boolean;
begin
  Result := GetBandFont(ABandType).Color = dxScreenTipFontColor;
end;

function TdxScreenTipRepository.IsFontStored(Index: Integer): Boolean;
begin
  Result := TdxScreenTipBandType(Index) in AssignedFonts;
end;

procedure TdxScreenTipRepository.CreateFonts;

  function CreateFont(ABandType: TdxScreenTipBandType): TFont;
  begin
    Result := TFont.Create;
    dxAssignFont(Result, Screen.HintFont, ScaleFactor, dxSystemScaleFactor);
    Result.Color := dxScreenTipFontColor;
    if ABandType in [stbHeader, stbFooter] then
      Result.Style := Result.Style + [fsBold];
    Result.OnChange := FontChanged;
  end;

var
  I: TdxScreenTipBandType;
begin
  FSystemFont := TFont.Create;
  for I := Low(TdxScreenTipBandType) to High(TdxScreenTipBandType) do
    FFonts[I] := CreateFont(I);
end;

procedure TdxScreenTipRepository.FontChanged(Sender: TObject);
var
  I: TdxScreenTipBandType;
begin
  if csLoading in ComponentState then
    Exit;

  for I := Low(TdxScreenTipBandType) to High(TdxScreenTipBandType) do
    if FFonts[I] = Sender then
      Include(FAssignedFonts, I);
end;

procedure TdxScreenTipRepository.SetFont(Index: Integer; Value: TFont);
begin
  FFonts[TdxScreenTipBandType(Index)].Assign(Value);
end;

procedure TdxScreenTipRepository.SetItems(AValue: TdxScreenTipCollection);
begin
  FItems.Assign(AValue);
end;

procedure TdxScreenTipRepository.SetStandardFooter(Value: TdxScreenTipFooterBand);
begin
  FStandardFooter.Assign(Value);
end;

{ TdxScreenTipCollection }

constructor TdxScreenTipCollection.Create(AParentComponent: TComponent; AItemClass: TcxComponentCollectionItemClass);
begin
  inherited Create(AParentComponent, AItemClass);
  FRepository := TdxScreenTipRepository(AParentComponent);
end;

function TdxScreenTipCollection.Add: TdxScreenTip;
begin
  Result := TdxScreenTip(inherited Add);
end;

procedure TdxScreenTipCollection.SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1);
begin
  AItem.Name := CreateUniqueName(ParentComponent.Owner, ParentComponent, AItem, 'Tdx', '', Count);
end;

procedure TdxScreenTipCollection.UpdateFonts;
begin
  // do nothing
end;

function TdxScreenTipCollection.GetItem(Index: Integer): TdxScreenTip;
begin
  Result := TdxScreenTip(inherited Items[Index]);
end;

procedure TdxScreenTipCollection.SetItem(Index: Integer; Value: TdxScreenTip);
begin
  Items[Index].Assign(Value);
end;

{ TdxCustomScreenTipBand }

constructor TdxCustomScreenTipBand.Create(ABandType: TdxScreenTipBandType);
begin
  inherited Create;
  FBandType := ABandType;
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := GlyphChanged;
  FGlyphFixedWidth := True;
  FTextAlign := stbtaRight;
  FPlainText := True;
end;

destructor TdxCustomScreenTipBand.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

procedure TdxCustomScreenTipBand.Assign(Source: TPersistent);
begin
  if Source is TdxCustomScreenTipBand then
  begin
    Text := TdxCustomScreenTipBand(Source).Text;
    TextAlign := TdxCustomScreenTipBand(Source).TextAlign;
    Glyph := TdxCustomScreenTipBand(Source).Glyph;
    GlyphFixedWidth := TdxCustomScreenTipBand(Source).GlyphFixedWidth;
    PlainText := TdxCustomScreenTipBand(Source).PlainText;
  end
  else
    inherited;
end;

function TdxCustomScreenTipBand.HasGlyph: Boolean;
begin
  Result := not FGlyph.Empty;
end;

function TdxCustomScreenTipBand.IsVisible(const AHintText: string): Boolean;
begin
  Result := (Text <> '') or not Glyph.Empty;
  if not Result and (BandType = stbHeader) then
    Result := AHintText <> '';
end;

procedure TdxCustomScreenTipBand.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TdxCustomScreenTipBand.GlyphChanged(Sender: TObject);
begin
  Changed;
end;

procedure TdxCustomScreenTipBand.SetGlyph(Value: TdxSmartGlyph);
begin
  FGlyph.Assign(Value);
end;

procedure TdxCustomScreenTipBand.SetPlainText(Value: Boolean);
begin
  if FPlainText <> Value then
  begin
    FPlainText := Value;
    Changed;
  end;
end;

procedure TdxCustomScreenTipBand.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TdxCustomScreenTipBand.SetTextAlign(Value: TdxScreenTipBandTextAlign);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    Changed;
  end;
end;

procedure TdxCustomScreenTipBand.SetGlyphFixedWidth(AValue: Boolean);
begin
  if FGlyphFixedWidth <> AValue then
  begin
    FGlyphFixedWidth := AValue;
    Changed;
  end;
end;

{ TdxScreenTipBand }

constructor TdxScreenTipBand.Create(AScreenTip: TdxScreenTip;
  ABandType: TdxScreenTipBandType);
begin
  inherited Create(ABandType);
  FScreenTip := AScreenTip;
end;

function TdxScreenTipBand.IsDefaultTextColor: Boolean;
begin
  Result := ScreenTip.Collection.Repository.IsDefaultBandTextColor(BandType);
end;

function TdxScreenTipBand.GetFont: TFont;
begin
  Result := ScreenTip.Collection.Repository.GetBandFont(BandType);
end;

{ TdxScreenTipFooterBand }

constructor TdxScreenTipFooterBand.Create(
  ARepository: TdxScreenTipRepository);
begin
  inherited Create(stbFooter);
  FRepository := ARepository;
end;

function TdxScreenTipFooterBand.IsDefaultTextColor: Boolean;
begin
  Result := Repository.IsDefaultBandTextColor(BandType);
end;

function TdxScreenTipFooterBand.GetFont: TFont;
begin
  Result := Repository.GetBandFont(BandType);
end;

{ TdxScreenTip }

constructor TdxScreenTip.Create(AOwner: TComponent);
var
  I: TdxScreenTipBandType;
begin
  inherited Create(AOwner);
  for I := Low(TdxScreenTipBandType) to High(TdxScreenTipBandType) do
    FBands[I] := TdxScreenTipBand.Create(Self, I);
end;

destructor TdxScreenTip.Destroy;
var
  I: TdxScreenTipBandType;
begin
  for I := Low(TdxScreenTipBandType) to High(TdxScreenTipBandType) do
    FreeAndNil(FBands[I]);
  inherited Destroy;
end;

procedure TdxScreenTip.Assign(Source: TPersistent);
begin
  if Source is TdxScreenTip then
  begin
    Header := TdxScreenTip(Source).Header;
    Description := TdxScreenTip(Source).Description;
    Footer := TdxScreenTip(Source).Footer;
    UseHintAsHeader := TdxScreenTip(Source).UseHintAsHeader;
    UseStandardFooter := TdxScreenTip(Source).UseStandardFooter;
    Width := TdxScreenTip(Source).Width;
  end
  else
    inherited;
end;

function TdxScreenTip.GetBandForCalculation(
  Index: TdxScreenTipBandType): TdxCustomScreenTipBand;
begin
  if FUseStandardFooter and (Index = stbFooter) then
    Result := Collection.Repository.StandardFooter
  else
    Result := GetBand(Ord(Index));
end;

function TdxScreenTip.GetWidth: Integer;
begin
  Result := Width;
  if Result = 0 then
  begin
    if Description.Glyph.Empty then
      Result := 210
    else
      Result := 318
  end;
  Result := ScaleFactor.Apply(Result);
end;

function TdxScreenTip.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxScreenTipRepository).Items;
end;

function TdxScreenTip.GetDisplayName: string;
begin
  Result := Format('%s - ''%s''', [Name, Header.Text]);
end;

function TdxScreenTip.GetScaleFactor: TdxScaleFactor;
begin
  Result := Collection.Repository.ScaleFactor;
end;

procedure TdxScreenTip.SetName(const Value: TComponentName);
var
  AChangeText: Boolean;
begin
  AChangeText := not (csLoading in ComponentState) and (Name = Header.Text) and
    ((Owner = nil) or not (Owner is TControl) or not (csLoading in TControl(Owner).ComponentState));

  inherited SetName(Value);

  if AChangeText then
    Header.Text := Value;
end;

function TdxScreenTip.GetCollection: TdxScreenTipCollection;
begin
  Result := TdxScreenTipCollection(inherited Collection);
end;

function TdxScreenTip.GetBand(Index: Integer): TdxScreenTipBand;
begin
  Result := FBands[TdxScreenTipBandType(Index)];
end;

procedure TdxScreenTip.SetBand(Index: Integer; Value: TdxScreenTipBand);
begin
  FBands[TdxScreenTipBandType(Index)].Assign(Value);
end;

{ TdxScreenTipLikeHintViewInfo }

constructor TdxScreenTipLikeHintViewInfo.Create(APainter: TcxCustomLookAndFeelPainter;
  const AHint, AShortCut: string; const ACursorPos: TPoint);
begin
  FHint := AHint;
  FShortCut := AShortCut;
  FCursorPos := ACursorPos;
  FPainter := TdxScreenTipPainter.Create(APainter);
  inherited Create;
end;

procedure TdxScreenTipLikeHintViewInfo.CreateHelper;
begin
  FHelper := TdxScreenTipLikeHintViewInfoHelper.Create(FPainter, FHint, FShortCut, FCursorPos);
end;

procedure TdxScreenTipLikeHintViewInfo.DestroyHelper;
begin
  inherited DestroyHelper;
  FreeAndNil(FPainter);
end;

{ TdxScreenTipBandViewInfo }

constructor TdxScreenTipBandViewInfo.Create(ABand: TdxCustomScreenTipBand;
  const AHintText, AShortCut: string; AScaleFactor: TdxScaleFactor);
begin
  inherited Create;
  FBand := ABand;
  FHintText := AHintText;
  FShortCut := AShortCut;
  FScaleFactor := AScaleFactor;
end;

procedure TdxScreenTipBandViewInfo.Calculate(ACanvas: TCanvas; APainter: TdxScreenTipPainter; ATop, AWidth: Integer);
var
  AHeight: Integer;
begin
  AHeight := GetHeight(ACanvas, AWidth, APainter);
  if HasText then
  begin
    if not IsTextCenter then
    begin
      FTextBounds.Top := ATop + GetVertIndent(APainter);
      FTextBounds.Bottom := FTextBounds.Top + FTextSize.cy;
    end
    else
    begin
      FTextBounds.Top := ATop;
      FTextBounds.Bottom := ATop + AHeight;
    end;
    FTextBounds.Left := GetHorzIndent;
    if Band.TextAlign = stbtaRight then
    begin
      if HasGlyph then
        Inc(FTextBounds.Left, GetAvailGlyphWidth + GetHorzIndent);
       FTextBounds.Right := AWidth - GetHorzIndent;
    end
    else
      FTextBounds.Right := FTextBounds.Left + FTextSize.cx;
  end;

  if HasGlyph then
  begin
    if FBand.BandType = stbDescription then
      FGlyphBounds.Top := ATop + GetVertIndent(APainter)
    else
      FGlyphBounds.Top := ATop + (AHeight - FGlyphSize.cy) div 2;

    FGlyphBounds.Bottom := FGlyphBounds.Top + FGlyphSize.cy;
    if Band.TextAlign = stbtaRight then
    begin
      FGlyphBounds.Left := GetHorzIndent + (GetAvailGlyphWidth - FGlyphSize.cx) div 2;
      FGlyphBounds.Right := FGlyphBounds.Left + FGlyphSize.cx;
    end
    else
    begin
      FGlyphBounds.Right := AWidth - (GetHorzIndent + (GetAvailGlyphWidth - FGlyphSize.cx) div 2);
      FGlyphBounds.Left :=  FGlyphBounds.Right - FGlyphSize.cx;
    end;
  end;
  FBounds := cxRect(0, ATop, AWidth, ATop + AHeight);
  if UseRightToLeftAlignment then
  begin
    FTextBounds := TdxRightToLeftLayoutConverter.ConvertRect(FTextBounds, FBounds);
    FGlyphBounds := TdxRightToLeftLayoutConverter.ConvertRect(FGlyphBounds, FBounds);
  end;
end;

function TdxScreenTipBandViewInfo.GetHeight(ACanvas: TCanvas; const AWidth: Integer; APainter: TdxScreenTipPainter): Integer;

  procedure CalculateGlyphSize;
  var
    AAvailWidth: Integer;
  begin
    if HasGlyph then
    begin
      AAvailWidth := GetAvailGlyphWidth;
      FGlyphSize := dxGetImageSize(Band.Glyph, ScaleFactor);
      if FGlyphSize.cx > AAvailWidth then
        FGlyphSize := cxSizeScale(FGlyphSize, AAvailWidth, FGlyphSize.cx);
    end
    else
      FGlyphSize := cxNullSize;
  end;

  procedure CalculateTextSize;
  var
    AAvailWidth: Integer;
  begin
    if HasText then
    begin
      AAvailWidth := AWidth;
      Dec(AAvailWidth, 2 * GetHorzIndent);
      if HasGlyph then
        Dec(AAvailWidth, GetAvailGlyphWidth + GetHorzIndent);
      FTextSize := GetTextSize(ACanvas, AAvailWidth);
    end
    else
      FTextSize  := cxNullSize;
  end;

begin
  Result := 0;
  CalculateGlyphSize;
  CalculateTextSize;
  if HasGlyph or HasText then
  begin
    Result := Max(FGlyphSize.cy, FTextSize.cy);
    Inc(Result, 2 * GetVertIndent(APainter));
  end;
end;

function TdxScreenTipBandViewInfo.IsVisible: Boolean;
begin
  Result := HasText or HasGlyph;
end;

procedure TdxScreenTipBandViewInfo.CorrectMinWidth(ACanvas: TCanvas; var AWidth: Integer);
var
  AMinWidth: Integer;
begin
  AMinWidth := 0;
  if HasGlyph then
    AMinWidth := AMinWidth + GetAvailGlyphWidth + GetHorzIndent;
  if HasText then
    AMinWidth := AMinWidth + GetTextSize(ACanvas, 0).cx + GetHorzIndent * 2;
  AWidth := Max(AWidth, AMinWidth);
end;

function TdxScreenTipBandViewInfo.GetAvailGlyphWidth: Integer;
begin
  if not Band.GlyphFixedWidth then
    Result := dxGetImageSize(Band.Glyph, ScaleFactor).cx
  else
    if Band.BandType = stbDescription then
      Result := ScaleFactor.Apply(108) - GetHorzIndent
    else
      Result := ScaleFactor.Apply(16);
end;

function TdxScreenTipBandViewInfo.GetHorzIndent: Integer;
begin
  if Band.BandType = stbDescription then
    Result := ScaleFactor.Apply(14)
  else
    Result := ScaleFactor.Apply(6);
end;

function TdxScreenTipBandViewInfo.GetVertIndent(APainter: TdxScreenTipPainter): Integer;
begin
  Result := 6;
  if FBand.BandType = stbFooter then
    Inc(Result, APainter.GetFooterLineSize);
  Result := ScaleFactor.Apply(Result);
end;

function TdxScreenTipBandViewInfo.GetText: string;
begin
  Result := Band.Text;
  if Band.BandType = stbHeader then
  begin
    if FHintText <> '' then
      Result := FHintText;
    Result := Result + FShortCut;
  end;
end;

function TdxScreenTipBandViewInfo.HasGlyph: Boolean;
begin
  Result := FBand.HasGlyph;
end;

function TdxScreenTipBandViewInfo.HasText: Boolean;
begin
  Result := GetText <> '';
end;

procedure TdxScreenTipBandViewInfo.Paint(ACanvas: TcxCanvas; APainter: TdxScreenTipPainter);

  procedure DrawTextField(const AText: string; ARect: TRect);
  begin
    if IsPlainText then
      cxDrawText(ACanvas.Handle, AText, ARect, GetTextFlags)
    else
      GetTextHelper(Band.BandType).DrawText(ACanvas.Canvas, ARect);
  end;

  function GetFooterLineBounds: TRect;
  begin
    Result := cxRectInflate(Bounds, -5, 0);
    Result.Bottom := Result.Top + APainter.GetFooterLineSize;
  end;

var
  ATextColor: TColor;
begin
  if HasText then
  begin
    ACanvas.Font.Assign(Band.Font);
    if Band.IsDefaultTextColor then
    begin
      if Band.BandType = stbDescription then
        ATextColor := APainter.GetDescriptionTextColor
      else
        ATextColor := APainter.GetTitleTextColor;

      if ATextColor <> clDefault then
        ACanvas.Font.Color := ATextColor;
    end;
    DrawTextField(GetText, TextBounds);
  end;

  if HasGlyph then
  begin
    cxDrawImage(ACanvas.Handle, GlyphBounds, GlyphBounds, Band.Glyph,
      nil, -1, idmNormal, False, 0, clDefault, True, APainter.GetColorPalette);
  end;

  if Band.BandType = stbFooter then
    APainter.DrawFooterLine(ACanvas, GetFooterLineBounds);
end;

procedure TdxScreenTipBandViewInfo.CalculateTextHeight(ACanvas: TCanvas; AText: string; var ARect: TRect);
begin
  if IsPlainText then
    cxDrawText(ACanvas.Handle, AText, ARect, GetTextFlags(True))
  else
  begin
    GetTextHelper(Band.BandType).Init(ACanvas, AText, ScaleFactor);
    GetTextHelper(Band.BandType).CalculateTextHeight(ACanvas, ARect);
  end;
end;

function TdxScreenTipBandViewInfo.GetTextFlags(AIsCalculate: Boolean): Integer;
begin
  Result := DT_NOPREFIX or IfThen(AIsCalculate, DT_CALCRECT);
  if UseRightToLeftReading then
    Result := Result or DT_RTLREADING;
  if UseRightToLeftAlignment then
    Result := Result or DT_RIGHT;
  if Band.BandType = stbDescription then
    Result := Result or DT_WORDBREAK or DT_EDITCONTROL
  else
    if IsTextCenter then
      Result := Result or DT_SINGLELINE or IfThen(not AIsCalculate, DT_VCENTER);
end;

function TdxScreenTipBandViewInfo.GetTextSize(ACanvas: TCanvas; AWidth: Integer): TSize;
var
  R: TRect;
begin
  ACanvas.Font.Assign(Band.Font);
  R := cxRectBounds(0, 0, AWidth, 0);
  CalculateTextHeight(ACanvas, GetText, R);
  Result := cxSize(R.Right, R.Bottom);
end;

function TdxScreenTipBandViewInfo.IsPlainText: Boolean;
begin
  Result := Band.PlainText;
end;

function TdxScreenTipBandViewInfo.IsTextCenter: Boolean;
begin
  Result := not ((Band.BandType = stbDescription) or Band.GlyphFixedWidth);
end;

{ TdxScreenTipViewInfoHelper }

constructor TdxScreenTipViewInfoHelper.Create(AScreenTip: TdxScreenTip;
  APainter: TdxScreenTipPainter; const AHintText: string; AShortCut: string = '');
begin
  inherited Create;
  FScreenTip := AScreenTip;
  FPainter := APainter;
  if (FScreenTip <> nil) and FScreenTip.UseHintAsHeader then
    FHintText := AHintText;
  FShortCut := AShortCut;
end;

destructor TdxScreenTipViewInfoHelper.Destroy;
begin
  DestroyBandViewInfos;
  inherited Destroy;
end;

procedure TdxScreenTipViewInfoHelper.Calculate(ACanvas: TCanvas);

  function GetScreenTipWidth: Integer;
  var
    I: Integer;
  begin
    Result := FScreenTip.GetWidth;
    for I := 0 to High(FBandViewInfos) do
      if FBandViewInfos[I].Band.BandType in [stbHeader, stbFooter] then
        FBandViewInfos[I].CorrectMinWidth(ACanvas, Result);
  end;

var
  ATop, AWidth: Integer;
  I: Integer;
begin
  DestroyBandViewInfos;
  CreateBandViewInfos;
  AWidth := GetScreenTipWidth;
  ATop := 0;
  for I := 0 to High(FBandViewInfos) do
  begin
    FBandViewInfos[I].UseRightToLeftAlignment := UseRightToLeftAlignment;
    FBandViewInfos[I].UseRightToLeftReading := UseRightToLeftReading;
    FBandViewInfos[I].Calculate(ACanvas, FPainter, ATop, AWidth);
    ATop := FBandViewInfos[I].Bounds.Bottom;
  end;
  BoundsRect := Rect(0, 0, AWidth, ATop);
end;

procedure TdxScreenTipViewInfoHelper.Paint(ACanvas: TCanvas);

  function CreateRoundRgn(const ARect: TRect): HRGN;
  begin
    Result := CreateRoundRectRgn(ARect.Left, ARect.Top, ARect.Right + 1, ARect.Bottom + 1, 2, 2);
  end;

var
  I: Integer;
begin
  cxPaintCanvas.BeginPaint(ACanvas);
  try
    cxPaintCanvas.SetClipRegion(TcxRegion.Create(CreateRoundRgn(BoundsRect)), roIntersect);
    FPainter.DrawBackground(cxPaintCanvas, BoundsRect);
    for I := 0 to BandViewInfosCount - 1 do
      FBandViewInfos[I].Paint(cxPaintCanvas, FPainter);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxScreenTipViewInfoHelper.CreateBandViewInfos;
var
  ABand: TdxCustomScreenTipBand;
  ABandViewInfo: TdxScreenTipBandViewInfo;
  ABandType: TdxScreenTipBandType;
begin
  for ABandType := Low(TdxScreenTipBandType) to High(TdxScreenTipBandType) do
  begin
    ABand := FScreenTip.GetBandForCalculation(ABandType);
    if ABand.IsVisible(FHintText) then
    begin
      ABandViewInfo := CreateBandViewInfo(ABand);
      SetLength(FBandViewInfos, Length(FBandViewInfos) + 1);
      FBandViewInfos[Length(FBandViewInfos) - 1] := ABandViewInfo;
    end;
  end;
end;

procedure TdxScreenTipViewInfoHelper.DestroyBandViewInfos;
var
  I: Integer;
begin
  for I := 0 to BandViewInfosCount - 1 do
    FreeAndNil(FBandViewInfos[I]);
  FBandViewInfos := nil;
end;

function TdxScreenTipViewInfoHelper.CreateBandViewInfo(ABand: TdxCustomScreenTipBand): TdxScreenTipBandViewInfo;
begin
  Result := TdxScreenTipBandViewInfo.Create(ABand, FHintText, FShortCut, ScreenTip.ScaleFactor);
end;

function TdxScreenTipViewInfoHelper.GetScaleFactor: TdxScaleFactor;
begin
  Result := ScreenTip.ScaleFactor;
end;

function TdxScreenTipViewInfoHelper.GetBandViewInfos(Index: Integer): TdxScreenTipBandViewInfo;
begin
  if Index < BandViewInfosCount then
    Result := FBandViewInfos[Index]
  else
    Result := nil;
end;

function TdxScreenTipViewInfoHelper.GetBandViewInfosCount: Integer;
begin
  Result := Length(FBandViewInfos);
end;

{ TdxScreenTipViewInfo }

constructor TdxScreenTipViewInfo.Create(AScreenTip: TdxScreenTip;
  APainter: TcxCustomLookAndFeelPainter; const AHintText: string; AShortCut: string);
begin
  FHint := AHintText;
  FScreenTip := AScreenTip;
  FShortCut := AShortCut;
  FPainter := TdxScreenTipPainter.Create(APainter);
  inherited Create;
end;

procedure TdxScreenTipViewInfo.CreateHelper;
begin
  FHelper := TdxScreenTipViewInfoHelper.Create(FScreenTip, FPainter, FHint,
    FShortCut);
end;

procedure TdxScreenTipViewInfo.DestroyHelper;
begin
  inherited DestroyHelper;
  FreeAndNil(FPainter);
end;

{ TdxScreenTipWindow }

constructor TdxScreenTipWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clInfoBk;
  AnimationStyle := cxhaNone;
  BorderStyle := bsSingle;
  FBorderWidth := 1;// bsSingle
  FScreenTip := nil;
end;

destructor TdxScreenTipWindow.Destroy;
begin
  FreeAndNil(FViewInfo);
  inherited Destroy;
end;

procedure TdxScreenTipWindow.AdjustActivateRect(var ARect: TRect);
begin
  if InternalUseRightToLeftAlignment and cxRectIsEmpty(HintAreaBounds) then
    ARect := cxRectOffsetHorz(ARect, -ARect.Width);
  if HasWindowRegion then
    ARect := cxRectInflate(ARect, FBorderWidth, FBorderWidth);
  inherited AdjustActivateRect(ARect);
end;

procedure TdxScreenTipWindow.EnableRegion;
var
  ARegion: HRGN;
begin
  ARegion := CreateRoundRectRgn(0, 0, ClientWidth + 1, ClientHeight + 1, 2, 2);
  OffsetRgn(ARegion, FBorderWidth, FBorderWidth);
  SetWindowRgn(Handle, ARegion, True);
end;

function TdxScreenTipWindow.HasWindowRegion: Boolean;
begin
  Result := True;
end;

procedure TdxScreenTipWindow.Paint;
begin
  if FStandardHint then
  begin
    DisableRegion;
    inherited Paint;
  end
  else
    FViewInfo.Paint(Canvas);
end;

procedure TdxScreenTipWindow.ShowHint(X, Y: Integer; ACaption, AHint: string; AMaxWidth: Integer = 0);
var
  ARect: TRect;
begin
  if AMaxWidth = 0 then
    AMaxWidth := Screen.Width;
  ARect := CalcHintRect(AMaxWidth, AHint, nil);
  OffsetRect(ARect, X, Y);
  ActivateHint(ARect, AHint);
end;

function TdxScreenTipWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;

  procedure GetScreenTipInfo(AHintStyle: TcxCustomHintStyle; out AScreenTip: TdxScreenTip; out AShortCut: string);
  var
    AControl: Tcontrol;
  begin
    AScreenTip := FScreenTip;
    AShortCut := '';
    if AScreenTip = nil then
    begin
      AControl := cxGetHintedControl;
      if AControl <> nil then
        TdxScreenTipStyle(AHintStyle).GetScreenTipInfo(AControl, AScreenTip, AShortCut);
    end;
  end;

  function CreateViewInfo(AHintStyle: TcxCustomHintStyle): TdxCustomHintViewInfo;
  var
    AScreenTip: TdxScreenTip;
    AHintText: string;
    AShortCut: string;
  begin
    AHintText := AHint;
    GetScreenTipInfo(AHintStyle, AScreenTip, AShortCut);
    if (AScreenTip <> nil) and AScreenTip.Collection.Repository.ShowDescription then
    begin
      if AScreenTip.UseHintAsHeader then
        AShortCut := '';
      Result := TdxScreenTipViewInfo.Create(AScreenTip, GetPainter, AHintText, AShortCut);
    end
    else
    begin
      if (AScreenTip <> nil) and not AScreenTip.UseHintAsHeader and (AScreenTip.Header.Text <> '') then
        AHintText := AScreenTip.Header.Text
      else
        AShortCut := '';

      Result := TdxScreenTipLikeHintViewInfo.Create(GetPainter, AHintText, AShortCut, GetMouseCursorPos);
    end;
  end;

var
  AHintStyle: TcxCustomHintStyle;
begin
  FreeAndNil(FViewInfo);
  AHintStyle := cxGetHintStyle;
  if AHintStyle <> nil then
  begin
    StandardHint := False;
    FViewInfo := CreateViewInfo(AHintStyle);
    FViewInfo.UseRightToLeftAlignment := InternalUseRightToLeftAlignment;
    FViewInfo.UseRightToLeftReading := UseRightToLeftReading;
    FViewInfo.Calculate(Canvas);
    Result := FViewInfo.BoundsRect;
  end
  else
  begin
    StandardHint := True;
    Canvas.Font.Assign(Screen.HintFont);
    if AHint <> '' then
      Result := inherited CalcHintRect(MaxWidth, AHint, AData)
    else
      Result := cxEmptyRect;
  end;
end;

function TdxScreenTipWindow.InternalUseRightToLeftAlignment: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode = bdRightToLeft);
end;

procedure TdxScreenTipWindow.WMShowWindow(var Message: TWMShowWindow);
begin
  if not Message.Show then
  begin
    FreeAndNil(FViewInfo);
    cxRemoveHintedControl;
  end;
end;

{ TdxScreenTipLink }

procedure TdxCustomScreenTipLink.Assign(Source: TPersistent);
begin
  if Source is TdxCustomScreenTipLink then
  begin
    Component := TdxCustomScreenTipLink(Source).Component;
    ScreenTip := TdxCustomScreenTipLink(Source).ScreenTip;
  end
  else
    inherited Assign(Source);
end;

function TdxCustomScreenTipLink.GetOwnerComponent: TComponent;
begin
  Result := TdxCustomScreenTipLinks(Collection).GetOwnerComponent;
end;

procedure TdxCustomScreenTipLink.SetComponent(const Value: TComponent);
begin
  if FComponent <> Value then
  begin
    if FComponent <> nil then
      FComponent.RemoveFreeNotification(GetOwnerComponent);
    FComponent := Value;
    if FComponent <> nil then
      FComponent.FreeNotification(GetOwnerComponent);
  end;
end;

procedure TdxCustomScreenTipLink.SetScreenTip(const Value: TdxScreenTip);
begin
  if FScreenTip <> Value then
  begin
    if FScreenTip <> nil then
      FScreenTip.RemoveFreeNotification(GetOwnerComponent);
    FScreenTip := Value;
    if FScreenTip <> nil then
      FScreenTip.FreeNotification(GetOwnerComponent);
  end;
end;

{ TdxScreenTipLinks }

constructor TdxCustomScreenTipLinks.Create(AScreenTipStyle: TdxScreenTipStyle);
begin
  inherited Create(GetScreenTipLinkClass);
  FScreenTipStyle := AScreenTipStyle;
end;

function TdxCustomScreenTipLinks.FindLinkByComponent(AComponent: TComponent): TdxCustomScreenTipLink;
var
  I: Integer;
  AItem: TdxCustomScreenTipLink;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I] as TdxCustomScreenTipLink;
    if (AItem.Component = AComponent) or (AItem.ScreenTip = AComponent) then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TdxCustomScreenTipLinks.GetOwner: TPersistent;
begin
  Result := FScreenTipStyle;
end;

function TdxCustomScreenTipLinks.GetOwnerComponent: TComponent;
begin
  Result := ScreenTipStyle.Owner as TComponent;
end;

{ TcxScreenTipStyle }

constructor TdxScreenTipStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScreenTipLinks := TdxScreenTipLinks.Create(Self);
  FScreenTipActionLinks := TdxScreenTipActionLinks.Create(Self);
end;

destructor TdxScreenTipStyle.Destroy;
begin
  FreeAndNil(FScreenTipActionLinks);
  FreeAndNil(FScreenTipLinks);
  inherited Destroy;
end;

procedure TdxScreenTipStyle.Assign(Source: TPersistent);
begin
  if Source is TdxScreenTipStyle then
  begin
  //  BeginUpdate;
    try
      ScreenTipLinks := TdxScreenTipStyle(Source).ScreenTipLinks;
      ScreenTipActionLinks := TdxScreenTipStyle(Source).ScreenTipActionLinks;
    finally
    //  EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

procedure TdxScreenTipStyle.ComponentRemoved(AComponent: TComponent);
var
  ALink: TdxCustomScreenTipLink;
begin
  if (FScreenTipLinks <> nil) and (FScreenTipActionLinks <> nil) and
    ((AComponent is TdxScreenTip) or
    (AComponent is TControl) or
    (AComponent is TBasicAction)) then
  begin
    ALink := nil;
    if not (AComponent is TBasicAction) then
      ALink := FScreenTipLinks.FindLinkByComponent(AComponent);
    if (ALink = nil) and not (AComponent is TControl) then
      ALink := FScreenTipActionLinks.FindLinkByComponent(AComponent);
    if ALink <> nil then
      if AComponent is TdxScreenTip then
        ALink.ScreenTip := nil
      else
        ALink.Component := nil;
  end;
end;

function TdxScreenTipStyle.GetHintWindowClass: TcxCustomHintWindowClass;
begin
  Result := TdxScreenTipWindow;
end;

function TdxScreenTipStyle.GetScreenTipBounds(AScreenTip: TdxScreenTip; const AHint: string = ''): TRect;
var
  AHintWindow: TdxScreenTipWindow;
begin
  AHintWindow := (Owner as TcxCustomHintStyleController).HintWindow as TdxScreenTipWindow;
  AHintWindow.FScreenTip := AScreenTip;
  try
    Result := AHintWindow.CalcHintRect(Screen.Width, AHint, nil);
  finally
    AHintWindow.FScreenTip := nil;
  end;
end;

procedure TdxScreenTipStyle.ShowScreenTip(X, Y: Integer;
  AScreenTip: TdxScreenTip; const AHint: string = '');
var
  AHintWindow: TdxScreenTipWindow;
begin
  AHintWindow := (Owner as TcxCustomHintStyleController).HintWindow as TdxScreenTipWindow;
  AHintWindow.FScreenTip := AScreenTip;
  try
    AHintWindow.ShowHint(X, Y, '', AHint);
  finally
    AHintWindow.FScreenTip := nil;
  end;
end;

procedure TdxScreenTipStyle.DoShowHint(var AHintStr: string; var ACanShow: Boolean;
  var AHintInfo: THintInfo);
var
  AScreenTip: TdxScreenTip;
  AShortCut: string;
begin
  if AHintStr = '' then
  begin
    GetScreenTipInfo(AHintInfo.HintControl, AScreenTip, AShortCut);
    if (AScreenTip <> nil) and not AScreenTip.UseHintAsHeader then
      AHintStr := IfThen(AScreenTip.Header.Text <> '', AScreenTip.Header.Text, ' ');
  end;
end;

procedure TdxScreenTipStyle.GetScreenTipInfo(AControl: TControl;
  out AScreenTip: TdxScreenTip; out AShortCut: string);

  function GetScreenTipProvider(AControl: TControl;
    out AScreenTipProvider: IdxScreenTipProvider): Boolean;
  var
    AManagedObject: IdxManagedObject;
  begin
    Result := Supports(AControl, IdxScreenTipProvider, AScreenTipProvider) or
      Supports(AControl, IdxManagedObject, AManagedObject) and
      Supports(AManagedObject.GetManager, IdxScreenTipProvider, AScreenTipProvider);
  end;

var
  AScreenTipProvider: IdxScreenTipProvider;
begin
  AShortCut := '';
  AScreenTip := ScreenTipLinks.FindScreenTipByControl(AControl);
  if (AScreenTip = nil) and (AControl.Action <> nil) then
    AScreenTip := ScreenTipActionLinks.FindScreenTipByAction(AControl.Action);
  if AScreenTip <> nil then
  begin
    if (AControl.Action <> nil) and (AControl.Action is TCustomAction) then
    begin
      AShortCut := ShortCutToText(TCustomAction(AControl.Action).ShortCut);
      AShortCut := IfThen(AShortCut <> '', Format(' (%s)', [AShortCut]));
    end;
  end
  else
    if GetScreenTipProvider(AControl, AScreenTipProvider) then
    begin
      AScreenTip := AScreenTipProvider.GetScreenTip;
      if (AScreenTip = nil) and (AScreenTipProvider.GetAction <> nil) then
        AScreenTip := ScreenTipActionLinks.FindScreenTipByAction(AScreenTipProvider.GetAction);
      AShortCut := AScreenTipProvider.GetShortCut;
    end;
end;

procedure TdxScreenTipStyle.SetScreenTipLinks(const Value: TdxScreenTipLinks);
begin
  FScreenTipLinks.Assign(Value);
end;

procedure TdxScreenTipStyle.SetScreenTipActionLinks(const Value: TdxScreenTipActionLinks);
begin
  FScreenTipActionLinks.Assign(Value);
end;

{ TdxScreenTipLinks }

function TdxScreenTipLinks.Add: TdxScreenTipLink;
begin
  Result := TdxScreenTipLink(inherited Add);
end;

function TdxScreenTipLinks.FindScreenTipByControl(
  AControl: TControl): TdxScreenTip;
var
  ALink: TdxCustomScreenTipLink;
begin
  Result := nil;
  ALink := FindLinkByComponent(AControl);
  if ALink <> nil then
    Result := ALink.ScreenTip;
end;

function TdxScreenTipLinks.GetScreenTipLinkClass: TdxScreenTipLinkClass;
begin
  Result := TdxScreenTipLink;
end;

function TdxScreenTipLinks.GetItem(Index: Integer): TdxScreenTipLink;
begin
  Result := TdxScreenTipLink(inherited GetItem(Index));
end;

procedure TdxScreenTipLinks.SetItem(Index: Integer; Value: TdxScreenTipLink);
begin
  inherited SetItem(Index, Value);
end;

{ TdxScreenTipLink }

function TdxScreenTipLink.GetControl: TControl;
begin
  Result := Component as TControl;
end;

procedure TdxScreenTipLink.SetControl(AValue: TControl);
begin
  Component := AValue;
end;

{ TdxScreenTipActionLink }

function TdxScreenTipActionLink.GetAction: TBasicAction;
begin
  Result := Component as TBasicAction;
end;

procedure TdxScreenTipActionLink.SetAction(AValue: TBasicAction);
begin
  Component := AValue;
end;

{ TdxScreenTipActionLinks }

function TdxScreenTipActionLinks.Add: TdxScreenTipActionLink;
begin
  Result := TdxScreenTipActionLink(inherited Add);
end;

function TdxScreenTipActionLinks.FindScreenTipByAction(
  AAction: TBasicAction): TdxScreenTip;
var
  ALink: TdxCustomScreenTipLink;
begin
  Result := nil;
  ALink := FindLinkByComponent(AAction);
  if ALink <> nil then
    Result := ALink.ScreenTip;
end;

function TdxScreenTipActionLinks.GetItem(
  Index: Integer): TdxScreenTipActionLink;
begin
  Result := TdxScreenTipActionLink(inherited GetItem(Index));
end;

function TdxScreenTipActionLinks.GetScreenTipLinkClass: TdxScreenTipLinkClass;
begin
  Result := TdxScreenTipActionLink;
end;

procedure TdxScreenTipActionLinks.SetItem(Index: Integer;
  Value: TdxScreenTipActionLink);
begin
  inherited SetItem(Index, Value);
end;

{ TdxScreenTipPainter }

constructor TdxScreenTipPainter.Create(APainter: TcxCustomLookAndFeelPainter);
begin
  inherited Create;
  FPainter := APainter;
end;

procedure TdxScreenTipPainter.DrawBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  FPainter.ScreenTipDrawBackground(ACanvas, ARect);
end;

procedure TdxScreenTipPainter.DrawFooterLine(ACanvas: TcxCanvas; const ARect: TRect);
begin
  FPainter.ScreenTipDrawFooterLine(ACanvas, ARect);
end;

function TdxScreenTipPainter.GetColorPalette: IdxColorPalette;
begin
  Result := FPainter.ScreenTipGetColorPalette;
end;

function TdxScreenTipPainter.GetDescriptionTextColor: TColor;
begin
  Result := FPainter.ScreenTipGetDescriptionTextColor;
end;

function TdxScreenTipPainter.GetFooterLineSize: Integer;
begin
  Result := FPainter.ScreenTipGetFooterLineSize;
end;

function TdxScreenTipPainter.GetTitleTextColor: TColor;
begin
  Result := FPainter.ScreenTipGetTitleTextColor;
end;

{ TdxScreenTipLikeHintViewInfoHelper }

constructor TdxScreenTipLikeHintViewInfoHelper.Create(
  APainter: TdxScreenTipPainter; const AHint, AShortCut: string; const ACursorPos: TPoint);
begin
  inherited Create(AHint, AShortCut, ACursorPos);
  FPainter := APainter;
end;

procedure TdxScreenTipLikeHintViewInfoHelper.CorrectMinSize(var ASize: TSize);
begin
  inherited CorrectMinSize(ASize);
  Inc(ASize.cx, 1 + ScaleFactor.Apply(4));
  Inc(ASize.cy, 1 + ScaleFactor.Apply(4));
end;

function TdxScreenTipLikeHintViewInfoHelper.GetTextRect: TRect;
begin
  Result := cxRectInflate(BoundsRect, -1);
  Result := cxRectInflate(Result, -ScaleFactor.Apply(4));
end;

procedure TdxScreenTipLikeHintViewInfoHelper.SetTextColor(ACanvas: TCanvas);
var
  ATextColor: TColor;
begin
  ATextColor := FPainter.GetTitleTextColor;
  if ATextColor <> clDefault then
    ACanvas.Font.Color := ATextColor
  else
    inherited SetTextColor(ACanvas);
end;

procedure TdxScreenTipLikeHintViewInfoHelper.Paint(ACanvas: TCanvas);
begin
  PrepareCanvasFont(ACanvas);
  cxPaintCanvas.BeginPaint(ACanvas);
  try
    FPainter.DrawBackground(cxPaintCanvas, BoundsRect);
    DrawText(cxPaintCanvas.Canvas);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

//

procedure DestroyRichTextHelpers;
var
  ABandType: TdxScreenTipBandType;
begin
  for ABandType := Low(TdxScreenTipBandType) to High(TdxScreenTipBandType) do
    FreeAndNil(FBandTextHelpers[ABandType]);
end;

initialization
  RegisterClasses([TdxScreenTip, TdxScreenTipBand, TdxScreenTipCollection,
    TdxScreenTipStyle, TdxScreenTipLinks, TdxScreenTipLink, TdxScreenTipActionLinks,
    TdxScreenTipActionLink]);
  cxRegisteredHintStyles.Register(TdxScreenTipStyle, 'ScreenTip');

finalization
  DestroyRichTextHelpers;
end.
