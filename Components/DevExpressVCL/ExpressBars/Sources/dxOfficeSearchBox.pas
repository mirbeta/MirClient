{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxOfficeSearchBox;

{$I cxVer.inc}

interface

uses
  Windows, SysUtils, StrUtils, Classes, Controls, Types, Variants, Graphics, Math, Forms,
  cxClasses, dxCore, dxCoreClasses, cxControls, dxGDIPlusClasses, cxGeometry, cxGraphics, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxTextEdit, dxBar, dxBarSkinConsts, dxBarAccessibility, dxRibbon, dxCoreGraphics;

const
  dxOfficeSearchBoxDefaultMaxResultCount = 5;
  dxOfficeSearchBoxDefaultPathDelimiter = #8594;

type
  TdxOfficeSearchBox = class;
  TdxOfficeSearchBoxInnerEdit = class;
  TdxOfficeSearchBoxProperties = class;
  TdxOfficeSearchBoxCustomSearchSourceHelper = class;
  TdxOfficeSearchBoxCustomSearchSourceHelperClass = class of TdxOfficeSearchBoxCustomSearchSourceHelper;

  { TdxOfficeSearchBoxViewInfo }

  TdxOfficeSearchBoxViewInfo = class(TcxCustomTextEditViewInfo)
  strict private const
    ContentOffset = 1;
  strict private
    FGlyphRect: TRect;

    function GetContentDisplayRect: TRect;
    function GetProperties: TdxOfficeSearchBoxProperties;
    function GetScaledContentOffset: Integer;
    function GetScaledInnerEditOffset: Integer;
  protected
    function CalculateEditConstantPartSize: TSize; virtual;
    function GetDefaultEditContentSize: TSize; virtual;
    function GetGlyphColorPalette: IdxColorPalette; virtual;
    //
    procedure InternalPaint(ACanvas: TcxCanvas); override;
  public
    procedure Calculate; virtual;
    //
    property ContentDisplayRect: TRect read GetContentDisplayRect;
    property Properties: TdxOfficeSearchBoxProperties read GetProperties;
  end;

  { TdxOfficeSearchBoxViewData }

  TdxOfficeSearchBoxViewData = class(TcxCustomTextEditViewData)
  strict private
    function GetProperties: TdxOfficeSearchBoxProperties;
  protected
    function InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
      AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize; override;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
      Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    //
    property Properties: TdxOfficeSearchBoxProperties read GetProperties;
  end;

  { TdxOfficeSearchBoxProperties }

  TdxOfficeSearchBoxDropDownMenuItemAddedEvent = procedure (Sender: TdxOfficeSearchBoxProperties; ASourceItem: TObject;
    ADropDownMenuItem: TdxBarItemLink) of object;
  TdxOfficeSearchBoxDropDownMenuItemAddingEvent = procedure (Sender: TdxOfficeSearchBoxProperties; const ASearchText: string;
    ASourceItem: TObject; var AAllow: Boolean) of object;
  TdxOfficeSearchBoxDropDownMenuPopulateEvent = procedure (Sender: TdxOfficeSearchBoxProperties; const ASearchText: string;
    ADropDownMenuItems: TdxBarItemLinks) of object;
  TdxOfficeSearchBoxSourceItemEnumProcRef = reference to procedure (AItem: TObject; out ACanContinueProcessing: Boolean);

  TdxOfficeSearchBoxProperties = class(TcxCustomTextEditProperties)
  strict private
    FBarManagerHolder: TcxComponentHolder;
    FGlyph: TdxSmartGlyph;
    FMaxResultCount: Integer;
    FRecursiveSearch: TdxDefaultBoolean;
    FRibbonHolder: TcxComponentHolder;
    FSearchSourceHelper: TdxOfficeSearchBoxCustomSearchSourceHelper;
    FSearchSourceHolder: TcxComponentHolder;
    FShowResultPaths: Boolean;
    //
    FOnDropDownMenuItemAdded: TdxOfficeSearchBoxDropDownMenuItemAddedEvent;
    FOnDropDownMenuItemAdding: TdxOfficeSearchBoxDropDownMenuItemAddingEvent;
    FOnDropDownMenuPopulate: TdxOfficeSearchBoxDropDownMenuPopulateEvent;
    //
    function GetBarManager: TdxBarManager;
    function GetComponentForOwner(AComponentClass: TComponentClass): TComponent;
    function GetRibbon: TdxCustomRibbon;
    function GetSearchSource: TComponent;
    function GetSearchSourceHelperClass: TdxOfficeSearchBoxCustomSearchSourceHelperClass;
    procedure GlyphChangeHandler(Sender: TObject);
    procedure SetBarManager(AValue: TdxBarManager);
    procedure SetGlyph(AValue: TdxSmartGlyph);
    procedure SetMaxResultCount(AValue: Integer);
    procedure SetRecursiveSearch(AValue: TdxDefaultBoolean);
    procedure SetRibbon(AValue: TdxCustomRibbon);
    procedure SetSearchSource(AValue: TComponent);
    procedure SetShowResultPaths(AValue: Boolean);
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    class function IsValidSearchSource(AValue: TComponent): Boolean; virtual;
    function UseLookupData: Boolean; override;
    //
    procedure DoDropDownMenuItemAdded(ASourceItem: TObject; ADropDownMenuItems: TdxBarItemLinks); virtual;
    function DoDropDownMenuItemAdding(const ASearchText: string; ASourceItem: TObject;
      ADropDownMenuItems: TdxBarItemLinks): Boolean; virtual;
    procedure DoDropDownMenuPopulate(const ASearchText: string; ADropDownMenuItems: TdxBarItemLinks); virtual;
    procedure PopulateDropDownMenu(const ASearchText: string; ADropDownMenuItems: TdxBarItemLinks); virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    class function GetContainerClass: TcxContainerClass; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function GetPaths(ASourceItem: TObject;
      const APathDelimiter: Char = dxOfficeSearchBoxDefaultPathDelimiter): TStringList; virtual;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    function IsResetEditClass: Boolean; override;
  published
    property BarManager: TdxBarManager read GetBarManager write SetBarManager;
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property MaxResultCount: Integer read FMaxResultCount write SetMaxResultCount default dxOfficeSearchBoxDefaultMaxResultCount;
    property Nullstring;
    property RecursiveSearch: TdxDefaultBoolean read FRecursiveSearch write SetRecursiveSearch default bDefault;
    property Ribbon: TdxCustomRibbon read GetRibbon write SetRibbon;
    property SearchSource: TComponent read GetSearchSource write SetSearchSource;
    property ShowResultPaths: Boolean read FShowResultPaths write SetShowResultPaths default False;
    property UseNullString;
    //
    property OnDropDownMenuItemAdded: TdxOfficeSearchBoxDropDownMenuItemAddedEvent read FOnDropDownMenuItemAdded write FOnDropDownMenuItemAdded;
    property OnDropDownMenuItemAdding: TdxOfficeSearchBoxDropDownMenuItemAddingEvent read FOnDropDownMenuItemAdding write FOnDropDownMenuItemAdding;
    property OnDropDownMenuPopulate: TdxOfficeSearchBoxDropDownMenuPopulateEvent read FOnDropDownMenuPopulate write FOnDropDownMenuPopulate;
  end;

  { TdxOfficeSearchBoxCustomSearchSourceHelper }

  TdxOfficeSearchBoxCustomSearchSourceHelper = class
  strict private
    FProperties: TdxOfficeSearchBoxProperties;
  protected
    function GetDefaultPath(ASourceItem: TObject): string; virtual;
    function GetPathDelimiter(APaths: TStringList): string; inline;
    //
    property Properties: TdxOfficeSearchBoxProperties read FProperties;
  public
    constructor Create(AProperties: TdxOfficeSearchBoxProperties); virtual;
    function AddToDropDownMenu(ASourceItem: TObject; ADropDownMenuItems: TdxBarItemLinks): TdxBarItemLink; virtual; abstract;
    function CanAddToDropDownMenu(const ASearchText: string; ASourceItem: TObject;
      ADropDownMenuItems: TdxBarItemLinks): Boolean; virtual; abstract;
    procedure DoForAllSearchSourceItems(ASearchSource: TComponent; AProcRef: TdxOfficeSearchBoxSourceItemEnumProcRef); virtual; abstract;
    procedure PopulatePaths(ASourceItem: TObject; var APaths: TStringList); virtual; abstract;
  end;

  { TdxOfficeSearchBoxBarManagerSearchSourceHelper }

  TdxOfficeSearchBoxBarManagerSearchSourceHelper = class(TdxOfficeSearchBoxCustomSearchSourceHelper)
  protected
    function CanProcessSourceItem(ASourceItem: TObject): Boolean; virtual;
    function DoProcessItemLinks(AItemLinks: TdxBarItemLinks; AProcRef: TdxOfficeSearchBoxSourceItemEnumProcRef): Boolean; virtual;
    procedure GetPathsBy(AOwner: TComponent; var APaths: TStringList; const ABasePath: string); virtual;
    procedure GetPathsByBar(AOwnerBar: TdxBar; var APaths: TStringList; const ABasePath: string); virtual;
    procedure GetPathsByBarItem(AOwnerBarItem: TdxBarItem; var APaths: TStringList; const ABasePath: string); virtual;
    procedure GetPathsByPopupMenu(AOwnerPopupMenu: TdxBarCustomPopupMenu; var APaths: TStringList;
      const ABasePath: string; ABarManager: TdxBarManager); virtual;
  public
    function AddToDropDownMenu(ASourceItem: TObject; ADropDownMenuItems: TdxBarItemLinks): TdxBarItemLink; override;
    function CanAddToDropDownMenu(const ASearchText: string; ASourceItem: TObject;
      ADropDownMenuItems: TdxBarItemLinks): Boolean; override;
    procedure DoForAllSearchSourceItems(ASearchSource: TComponent; AProcRef: TdxOfficeSearchBoxSourceItemEnumProcRef); override;
    procedure PopulatePaths(ASourceItem: TObject; var APaths: TStringList); override;
  end;

  { TdxOfficeSearchBoxRibbonSearchSourceHelper }

  TdxOfficeSearchBoxRibbonSearchSourceHelper = class(TdxOfficeSearchBoxBarManagerSearchSourceHelper)
  protected
    procedure GetPathsBy(AOwner: TComponent; var APaths: TStringList; const ABasePath: string); override;
    procedure GetPathsByBar(AOwnerBar: TdxBar; var APaths: TStringList; const ABasePath: string); override;
  public
    procedure DoForAllSearchSourceItems(ASearchSource: TComponent; AProcRef: TdxOfficeSearchBoxSourceItemEnumProcRef); override;
  end;

  { TdxOfficeSearchBoxPopupMenuSubMenuControl }

  TdxOfficeSearchBoxPopupMenuSubMenuControl = class(TdxRibbonPopupMenuControl)
  strict private
    FInnerEdit: TdxOfficeSearchBoxInnerEdit;
  protected
    procedure DoHideAll(AReason: TdxBarCloseUpReason); override;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function IsInternal: Boolean; override;
    function IsNavigationKey(AKey: Word; AShift: TShiftState): Boolean; virtual;
    function PreProcessKey(AKey: Word; AShift: TShiftState): Boolean; override;
    //
    property InnerEdit: TdxOfficeSearchBoxInnerEdit read FInnerEdit write FInnerEdit;
  end;

  { TdxOfficeSearchBoxPopupMenuSubMenuControlAccessibilityHelper }

  TdxOfficeSearchBoxPopupMenuSubMenuControlAccessibilityHelper = class(TdxBarSubMenuControlAccessibilityHelper)
  strict private
    function GetBarControl: TdxOfficeSearchBoxPopupMenuSubMenuControl;
  protected
    function CanNavigateToChildren(AKey: Word): Boolean; override;
    //
    property BarControl: TdxOfficeSearchBoxPopupMenuSubMenuControl read GetBarControl;
  end;

  { TdxOfficeSearchBoxPopupMenu }

  TdxOfficeSearchBoxPopupMenu = class(TdxRibbonPopupMenu)
  strict private
    FInnerEdit: TdxOfficeSearchBoxInnerEdit;
    //
    function GetSubMenuControl: TdxOfficeSearchBoxPopupMenuSubMenuControl;
    //
    procedure SubMenuControlMouseEnterHandler(Sender: TObject);
  protected
    function CreateBarControl: TCustomdxBarControl; override;
    function GetControlClass: TCustomdxBarControlClass; override;
    //
    property InnerEdit: TdxOfficeSearchBoxInnerEdit read FInnerEdit write FInnerEdit;
    property SubMenuControl: TdxOfficeSearchBoxPopupMenuSubMenuControl read GetSubMenuControl;
  end;

  { TdxOfficeSearchBoxInnerEdit }

  TdxOfficeSearchBoxInnerEdit = class(TcxCustomInnerTextEdit, IdxScaleFactor)
  strict private
    FDropDownMenu: TdxOfficeSearchBoxPopupMenu;
    FInternalFocused: Boolean;
    FStoredBarMenuAnimation: TdxBarMenuAnimations;
    //
    function GetActiveProperties: TdxOfficeSearchBoxProperties;
    function GetContainer: TdxOfficeSearchBox;
    procedure SetInternalFocused(const AValue: Boolean);
    //
    procedure DropDownMenuCloseUpExHandler(Sender: TObject; AReason: TdxBarCloseUpReason);
    // IdxScaleFactor
    function GetScaleFactor: TdxScaleFactor;
  protected
    FShiftIsEmpty: Boolean;
    //
    procedure Change; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    //
    procedure CreateDropDownMenu; virtual;
    procedure HideDropDownMenu; virtual;
    procedure ShowDropDownMenu; virtual;
    procedure UpdateDropDownMenu; virtual;
    //
    property ActiveProperties: TdxOfficeSearchBoxProperties read GetActiveProperties;
    property Container: TdxOfficeSearchBox read GetContainer;
    property DropDownMenu: TdxOfficeSearchBoxPopupMenu read FDropDownMenu;
    property InternalFocused: Boolean read FInternalFocused write SetInternalFocused;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TdxOfficeSearchBox }

  TdxOfficeSearchBox = class(TcxCustomTextEdit)
  strict private
    function GetActiveProperties: TdxOfficeSearchBoxProperties;
    function GetProperties: TdxOfficeSearchBoxProperties;
    function GetViewInfo: TdxOfficeSearchBoxViewInfo;
    procedure SetProperties(AValue: TdxOfficeSearchBoxProperties);
  protected
    procedure AdjustInnerEditPosition; override;
    procedure Click; override;
    procedure DoFocusChanged; override;
    function GetEditValue: Variant; override;
    function GetInnerEditClass: TControlClass; override;
    function GetViewInfoClass: TcxContainerViewInfoClass; override;
    procedure SetEditValue(const Value: Variant); override;
    //
    property ViewInfo: TdxOfficeSearchBoxViewInfo read GetViewInfo;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function HasPopupWindow: Boolean; override;
    //
    property ActiveProperties: TdxOfficeSearchBoxProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property Properties: TdxOfficeSearchBoxProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

implementation

uses
  dxRibbonGallery, cxBarEditItem, dxTypeHelpers, dxBarStrs;

type
  TdxCustomRibbonGalleryItemAccess = class(TdxCustomRibbonGalleryItem);

{ TdxOfficeSearchBoxViewInfo }

procedure TdxOfficeSearchBoxViewInfo.Calculate;
var
  AGlyphSize: TSize;
  R: TRect;
begin
  AGlyphSize := dxGetImageSize(Properties.Glyph, nil, -1, ScaleFactor);
  if not cxSizeIsEmpty(AGlyphSize) then
  begin
    R := ContentDisplayRect;
    FGlyphRect := cxRectSetWidth(R, AGlyphSize.cx);
    FGlyphRect := cxRectCenterVertically(FGlyphRect, AGlyphSize.cy);
    TextRect.Left := FGlyphRect.Right + GetScaledInnerEditOffset;
    if UseRightToLeftAlignment then
    begin
      FGlyphRect := TdxRightToLeftLayoutConverter.ConvertRect(FGlyphRect, R);
      TextRect := TdxRightToLeftLayoutConverter.ConvertRect(TextRect, R);
    end;
  end;
  InnerEditRect := TextRect;
end;

function TdxOfficeSearchBoxViewInfo.CalculateEditConstantPartSize: TSize;
begin
  Result := GetDefaultEditContentSize;
  Inc(Result.cx, GetScaledContentOffset * 2);
  Inc(Result.cy, GetScaledContentOffset * 2);
end;

function TdxOfficeSearchBoxViewInfo.GetDefaultEditContentSize: TSize;
var
  AGlyphSize: TSize;
begin
  Result := cxTextSize(Font, dxMeasurePattern);
  AGlyphSize := dxGetImageSize(Properties.Glyph, nil, -1, ScaleFactor);
  if not cxSizeIsEmpty(AGlyphSize) then
  begin
    Result.cx := AGlyphSize.cx + GetScaledInnerEditOffset + Result.cx;
    Result.cy := Max(AGlyphSize.cy, Result.cy);
  end;
end;

function TdxOfficeSearchBoxViewInfo.GetGlyphColorPalette: IdxColorPalette;
begin
  if Properties.Ribbon <> nil then
    Result := TdxSimpleColorPalette.Create(dxColorToAlphaColor(TextColor), TdxAlphaColors.Empty)
  else
    Result := nil;
end;

procedure TdxOfficeSearchBoxViewInfo.InternalPaint(ACanvas: TcxCanvas);
begin
  inherited InternalPaint(ACanvas);

  cxDrawImage(ACanvas, FGlyphRect, Properties.Glyph, nil, -1, Enabled, GetGlyphColorPalette, ScaleFactor);
end;

function TdxOfficeSearchBoxViewInfo.GetContentDisplayRect: TRect;
begin
  Result := cxRectInflate(ClientRect, -GetScaledContentOffset);
end;

function TdxOfficeSearchBoxViewInfo.GetProperties: TdxOfficeSearchBoxProperties;
begin
  Result := TdxOfficeSearchBoxProperties(EditProperties);
end;

function TdxOfficeSearchBoxViewInfo.GetScaledContentOffset: Integer;
begin
  Result := ScaleFactor.Apply(ContentOffset);
end;

function TdxOfficeSearchBoxViewInfo.GetScaledInnerEditOffset: Integer;
begin
  Result := ScaleFactor.Apply(cxTextOffset);
end;

{ TdxOfficeSearchBoxViewData }

procedure TdxOfficeSearchBoxViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  TdxOfficeSearchBoxViewInfo(AViewInfo).Calculate;
end;

function TdxOfficeSearchBoxViewData.InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
  AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize;
begin
  Result :=
    inherited InternalGetEditConstantPartSize(ACanvas, AIsInplace, AEditSizeProperties, MinContentSize, AViewInfo);
  if (AViewInfo <> nil) and (TdxOfficeSearchBoxViewInfo(AViewInfo).Painter <> nil) then
    MinContentSize := cxSizeMax(MinContentSize, TdxOfficeSearchBoxViewInfo(AViewInfo).CalculateEditConstantPartSize);
end;

function TdxOfficeSearchBoxViewData.GetProperties: TdxOfficeSearchBoxProperties;
begin
  Result := TdxOfficeSearchBoxProperties(FProperties);
end;

{ TdxOfficeSearchBoxProperties }

constructor TdxOfficeSearchBoxProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FBarManagerHolder := TcxComponentHolder.Create;
  BarManager := TdxBarManager(GetComponentForOwner(TdxBarManager));
  if (BarManager = nil) and (dxBarManagerList.Count > 0) then
    BarManager := TdxBarManager(dxBarManagerList[0]);
  FGlyph := TdxSmartGlyph.CreateSize(0, 0);
  FGlyph.OnChange := GlyphChangeHandler;
  FMaxResultCount := dxOfficeSearchBoxDefaultMaxResultCount;
  FRecursiveSearch := bDefault;
  FRibbonHolder := TcxComponentHolder.Create;
  Ribbon := TdxCustomRibbon(GetComponentForOwner(TdxCustomRibbon));
  FSearchSourceHolder := TcxComponentHolder.Create;
  FShowResultPaths := False;
end;

destructor TdxOfficeSearchBoxProperties.Destroy;
begin
  FreeAndNil(FSearchSourceHolder);
  FreeAndNil(FRibbonHolder);
  FreeAndNil(FGlyph);
  FreeAndNil(FBarManagerHolder);
  inherited Destroy;
end;

class function TdxOfficeSearchBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxOfficeSearchBox;
end;

class function TdxOfficeSearchBoxProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxOfficeSearchBoxViewInfo;
end;

function TdxOfficeSearchBoxProperties.GetPaths(ASourceItem: TObject;
  const APathDelimiter: Char = dxOfficeSearchBoxDefaultPathDelimiter): TStringList;
var
  ANeedHelper: Boolean;
begin
  Result := TStringList.Create;
  Result.NameValueSeparator := APathDelimiter;
  ANeedHelper := FSearchSourceHelper = nil;
  if ANeedHelper then
    FSearchSourceHelper := GetSearchSourceHelperClass.Create(Self);
  try
    FSearchSourceHelper.PopulatePaths(ASourceItem, Result);
  finally
    if ANeedHelper then
      FSearchSourceHelper.Free;
  end;
end;

function TdxOfficeSearchBoxProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := [esoAlwaysHotTrack, esoAutoHeight, esoEditing, esoNeedHandle];
end;

function TdxOfficeSearchBoxProperties.IsResetEditClass: Boolean;
begin
  Result := False;
end;

procedure TdxOfficeSearchBoxProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited DoAssign(AProperties);
  if AProperties is TdxOfficeSearchBoxProperties then
  begin
    BarManager := TdxOfficeSearchBoxProperties(AProperties).BarManager;
    Glyph := TdxOfficeSearchBoxProperties(AProperties).Glyph;
    MaxResultCount := TdxOfficeSearchBoxProperties(AProperties).MaxResultCount;
    RecursiveSearch := TdxOfficeSearchBoxProperties(AProperties).RecursiveSearch;
    Ribbon := TdxOfficeSearchBoxProperties(AProperties).Ribbon;
    SearchSource := TdxOfficeSearchBoxProperties(AProperties).SearchSource;
    ShowResultPaths := TdxOfficeSearchBoxProperties(AProperties).ShowResultPaths;
    //
    OnDropDownMenuItemAdded := TdxOfficeSearchBoxProperties(AProperties).OnDropDownMenuItemAdded;
    OnDropDownMenuItemAdding := TdxOfficeSearchBoxProperties(AProperties).OnDropDownMenuItemAdding;
    OnDropDownMenuPopulate := TdxOfficeSearchBoxProperties(AProperties).OnDropDownMenuPopulate;
  end;
end;

class function TdxOfficeSearchBoxProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxOfficeSearchBoxViewData;
end;

class function TdxOfficeSearchBoxProperties.IsValidSearchSource(AValue: TComponent): Boolean;
begin
  Result := (AValue = nil) or (AValue is TdxCustomRibbon) or (AValue is TdxBarManager);
end;

function TdxOfficeSearchBoxProperties.UseLookupData: Boolean;
begin
  Result := False;
end;

procedure TdxOfficeSearchBoxProperties.DoDropDownMenuItemAdded(ASourceItem: TObject;
  ADropDownMenuItems: TdxBarItemLinks);
var
  ADropDownMenuItem: TdxBarItemLink;
begin
  ADropDownMenuItem := nil;
  if FSearchSourceHelper <> nil then
    ADropDownMenuItem := FSearchSourceHelper.AddToDropDownMenu(ASourceItem, ADropDownMenuItems);
  if Assigned(OnDropDownMenuItemAdded) then
    OnDropDownMenuItemAdded(Self, ASourceItem, ADropDownMenuItem);
end;

function TdxOfficeSearchBoxProperties.DoDropDownMenuItemAdding(const ASearchText: string; ASourceItem: TObject;
  ADropDownMenuItems: TdxBarItemLinks): Boolean;
begin
  Result := False;
  if FSearchSourceHelper <> nil then
    Result := FSearchSourceHelper.CanAddToDropDownMenu(ASearchText, ASourceItem, ADropDownMenuItems);
  if Assigned(OnDropDownMenuItemAdding) then
    OnDropDownMenuItemAdding(Self, ASearchText, ASourceItem, Result);
end;

procedure TdxOfficeSearchBoxProperties.DoDropDownMenuPopulate(const ASearchText: string;
  ADropDownMenuItems: TdxBarItemLinks);
begin
  if Assigned(OnDropDownMenuPopulate) then
    OnDropDownMenuPopulate(Self, ASearchText, ADropDownMenuItems);
end;

procedure TdxOfficeSearchBoxProperties.PopulateDropDownMenu(const ASearchText: string;
  ADropDownMenuItems: TdxBarItemLinks);
var
  ACount: Integer;
begin
  FSearchSourceHelper := GetSearchSourceHelperClass.Create(Self);
  try
    ADropDownMenuItems.BeginUpdate;
    try
      ACount := 0;
      ADropDownMenuItems.Clear;
      if FSearchSourceHelper <> nil then
        FSearchSourceHelper.DoForAllSearchSourceItems(SearchSource,
          procedure(ASourceItem: TObject; out ACanContinueProcessing: Boolean)
          begin
            if DoDropDownMenuItemAdding(ASearchText, ASourceItem, ADropDownMenuItems) then
            begin
              DoDropDownMenuItemAdded(ASourceItem, ADropDownMenuItems);
              Inc(ACount);
            end;
            ACanContinueProcessing := ACount < MaxResultCount;
          end);
      DoDropDownMenuPopulate(ASearchText, ADropDownMenuItems);
    finally
      ADropDownMenuItems.EndUpdate;
    end;
  finally
    FSearchSourceHelper.Free;
  end;
end;

function TdxOfficeSearchBoxProperties.GetBarManager: TdxBarManager;
begin
  Result := TdxBarManager(FBarManagerHolder.Component);
end;

function TdxOfficeSearchBoxProperties.GetComponentForOwner(AComponentClass: TComponentClass): TComponent;
var
  AOwnerComponent: TComponent;
  AOwnerForm: TCustomForm;
begin
  AOwnerForm := nil;
  AOwnerComponent := Owner as TComponent;
  while (AOwnerComponent <> nil) and (AOwnerForm = nil) do
  begin
    if AOwnerComponent is TCustomForm then
      AOwnerForm := TCustomForm(AOwnerComponent);
    AOwnerComponent := AOwnerComponent.Owner;
  end;
  if AOwnerForm <> nil then
    Result := cxFindComponent(AOwnerForm, AComponentClass)
  else
    Result := nil;
end;

function TdxOfficeSearchBoxProperties.GetRibbon: TdxCustomRibbon;
begin
  Result := TdxCustomRibbon(FRibbonHolder.Component);
end;

function TdxOfficeSearchBoxProperties.GetSearchSource: TComponent;
begin
  Result := FSearchSourceHolder.Component;
end;

function TdxOfficeSearchBoxProperties.GetSearchSourceHelperClass: TdxOfficeSearchBoxCustomSearchSourceHelperClass;
begin
  Result := nil;
  if SearchSource is TdxCustomRibbon then
    Result := TdxOfficeSearchBoxRibbonSearchSourceHelper;
  if SearchSource is TdxBarManager then
    Result := TdxOfficeSearchBoxBarManagerSearchSourceHelper;
end;

procedure TdxOfficeSearchBoxProperties.GlyphChangeHandler(Sender: TObject);
begin
  Changed;
end;

procedure TdxOfficeSearchBoxProperties.SetBarManager(AValue: TdxBarManager);
begin
  FBarManagerHolder.Component := AValue;
  Changed;
end;

procedure TdxOfficeSearchBoxProperties.SetGlyph(AValue: TdxSmartGlyph);
begin
  FGlyph.Assign(AValue);
end;

procedure TdxOfficeSearchBoxProperties.SetMaxResultCount(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if FMaxResultCount <> AValue then
  begin
    FMaxResultCount := AValue;
    Changed;
  end;
end;

procedure TdxOfficeSearchBoxProperties.SetRecursiveSearch(AValue: TdxDefaultBoolean);
begin
  if FRecursiveSearch <> AValue then
  begin
    FRecursiveSearch := AValue;
    Changed;
  end;
end;

procedure TdxOfficeSearchBoxProperties.SetRibbon(AValue: TdxCustomRibbon);
begin
  FRibbonHolder.Component := AValue;
  Changed;
end;

procedure TdxOfficeSearchBoxProperties.SetSearchSource(AValue: TComponent);
begin
  if IsValidSearchSource(AValue) then
  begin
    FSearchSourceHolder.Component := AValue;
    Changed;
  end;
end;

procedure TdxOfficeSearchBoxProperties.SetShowResultPaths(AValue: Boolean);
begin
  if FShowResultPaths <> AValue then
  begin
    FShowResultPaths := AValue;
    Changed;
  end;
end;

{ TdxOfficeSearchBoxCustomSearchSourceHelper }

constructor TdxOfficeSearchBoxCustomSearchSourceHelper.Create(AProperties: TdxOfficeSearchBoxProperties);
begin
  inherited Create;
  FProperties := AProperties;
end;

function TdxOfficeSearchBoxCustomSearchSourceHelper.GetDefaultPath(ASourceItem: TObject): string;

  function GetPathLength(APaths: TStringList; const APathIndex: Integer): Integer;
  var
    APath, APathDelimiter: string;
  begin
    Result := 0;
    APath := APaths[APathIndex];
    APathDelimiter := GetPathDelimiter(APaths);
    while AnsiContainsStr(APath, APathDelimiter) do
    begin
      Inc(Result);
      APath := StringReplace(APath, APathDelimiter, ' ', [rfIgnoreCase]);
    end;
  end;

var
  APaths: TStringList;
  ALength, ANewLength, AIndex, I: Integer;
begin
  Result := '';
  APaths := Properties.GetPaths(ASourceItem);
  try
    if APaths.Count > 0 then
    begin
      AIndex := 0;
      ALength := GetPathLength(APaths, AIndex);
      for I := AIndex + 1 to APaths.Count - 1 do
      begin
        ANewLength := GetPathLength(APaths, I);
        if ANewLength < ALength then
        begin
          AIndex := I;
          ALength := ANewLength;
        end;
      end;
      Result := APaths[AIndex];
    end;
  finally
    APaths.Free;
  end;
end;

function TdxOfficeSearchBoxCustomSearchSourceHelper.GetPathDelimiter(APaths: TStringList): string;
begin
  Result := ' ' + APaths.NameValueSeparator + ' ';
end;

{ TdxOfficeSearchBoxBarManagerSearchSourceHelper }

function TdxOfficeSearchBoxBarManagerSearchSourceHelper.AddToDropDownMenu(ASourceItem: TObject;
  ADropDownMenuItems: TdxBarItemLinks): TdxBarItemLink;
begin
  Result := ADropDownMenuItems.Add;
  Result.Assign(TdxBarItemLink(ASourceItem));
  Result.BeginGroup := False;
  if Properties.ShowResultPaths then
    Result.UserCaption := GetDefaultPath(ASourceItem) + Result.Caption;
end;

function TdxOfficeSearchBoxBarManagerSearchSourceHelper.CanAddToDropDownMenu(const ASearchText: string;
  ASourceItem: TObject; ADropDownMenuItems: TdxBarItemLinks): Boolean;
var
  ALink: TdxBarItemLink;
begin
  ALink := TdxBarItemLink(ASourceItem);
  Result := (ALink.Item <> nil) and (ADropDownMenuItems.FindByItem(ALink.Item) = nil) and ALink.Item.ActuallyVisible and
    AnsiContainsText(RemoveAccelChars(ALink.Caption), ASearchText) and
    ((ALink.Item is TdxBarCustomButton) or (ALink.Item is TdxBarSubItem)
    or ((ALink.Item is TdxCustomRibbonGalleryItem) and
    TdxCustomRibbonGalleryItemAccess(ALink.Item).GalleryInMenuOptions.CollapsedInSubmenu));
end;

procedure TdxOfficeSearchBoxBarManagerSearchSourceHelper.DoForAllSearchSourceItems(ASearchSource: TComponent;
  AProcRef: TdxOfficeSearchBoxSourceItemEnumProcRef);
var
  ABarManager: TdxBarManager;
  ACanContinueProcessing: Boolean;
  I, J: Integer;
begin
  ACanContinueProcessing := True;
  ABarManager := TdxBarManager(ASearchSource);
  if Properties.RecursiveSearch = bDefault then
    for I := 0 to ABarManager.ItemCount - 1 do
      for J := 0 to ABarManager.Items[I].LinkCount - 1 do
      begin
        if CanProcessSourceItem(ABarManager.Items[I].Links[J]) then
          AProcRef(ABarManager.Items[I].Links[J], ACanContinueProcessing);
        if not ACanContinueProcessing then
          Exit;
      end
  else
    for I := 0 to ABarManager.Bars.Count - 1 do
      if not DoProcessItemLinks(ABarManager.Bars[I].ItemLinks, AProcRef) then
        Exit;
end;

procedure TdxOfficeSearchBoxBarManagerSearchSourceHelper.PopulatePaths(ASourceItem: TObject; var APaths: TStringList);
var
  ABarItem: TdxBarItem;
  I: Integer;
begin
  APaths.Clear;
  ABarItem := (ASourceItem as TdxBarItemLink).Item;
  for I := 0 to ABarItem.LinkCount - 1 do
    GetPathsBy(ABarItem.Links[I].Collection.Owner, APaths, '')
end;

function TdxOfficeSearchBoxBarManagerSearchSourceHelper.CanProcessSourceItem(ASourceItem: TObject): Boolean;
var
  AItem: TdxBarItem;
  AItemLinks: TdxBarItemLinks;
  ALinksOwner: IdxBarLinksOwner;
  I: Integer;
begin
  AItem := TdxBarItemLink(ASourceItem).Item;
  Result := not (AItem is TcxBarEditItem);
  if Result and Supports(AItem, IdxBarLinksOwner, ALinksOwner) then
  begin
    AItemLinks := ALinksOwner.GetItemLinks;
    if AItemLinks <> nil then
      for I := 0 to AItemLinks.Count - 1 do
      begin
        Result := CanProcessSourceItem(AItemLinks[I]);
        if not Result then
          Break;
      end;
  end;
end;

function TdxOfficeSearchBoxBarManagerSearchSourceHelper.DoProcessItemLinks(AItemLinks: TdxBarItemLinks;
  AProcRef: TdxOfficeSearchBoxSourceItemEnumProcRef): Boolean;
var
  ALinksOwner: IdxBarLinksOwner;
  I: Integer;
begin
  Result := True;
  for I := 0 to AItemLinks.Count - 1 do
  begin
    if CanProcessSourceItem(AItemLinks[I]) then
      AProcRef(AItemLinks[I], Result);
    if Result and (Properties.RecursiveSearch = bTrue) and
      Supports(AItemLinks[I].Item, IdxBarLinksOwner, ALinksOwner) and (ALinksOwner.GetItemLinks <> nil) then
      Result := DoProcessItemLinks(ALinksOwner.GetItemLinks, AProcRef);
    if not Result then
      Exit;
  end;
end;

procedure TdxOfficeSearchBoxBarManagerSearchSourceHelper.GetPathsBy(AOwner: TComponent; var APaths: TStringList;
  const ABasePath: string);
begin
  if AOwner is TdxBarItem then
    GetPathsByBarItem(AOwner as TdxBarItem, APaths, ABasePath)
  else if AOwner is TdxBarCustomPopupMenu then
    GetPathsByPopupMenu(AOwner as TdxBarCustomPopupMenu, APaths, ABasePath, Properties.SearchSource as TdxBarManager)
  else if AOwner is TdxBar then
    GetPathsByBar(AOwner as TdxBar, APaths, ABasePath);
end;

procedure TdxOfficeSearchBoxBarManagerSearchSourceHelper.GetPathsByBar(AOwnerBar: TdxBar; var APaths: TStringList;
  const ABasePath: string);
begin
  APaths.Add(AOwnerBar.Caption + GetPathDelimiter(APaths) + ABasePath);
end;

procedure TdxOfficeSearchBoxBarManagerSearchSourceHelper.GetPathsByBarItem(AOwnerBarItem: TdxBarItem;
  var APaths: TStringList; const ABasePath: string);
var
  I: Integer;
begin
  for I := 0 to AOwnerBarItem.LinkCount - 1 do
    GetPathsBy(AOwnerBarItem.Links[I].Collection.Owner, APaths,
      AOwnerBarItem.Caption + GetPathDelimiter(APaths) + ABasePath)
end;

procedure TdxOfficeSearchBoxBarManagerSearchSourceHelper.GetPathsByPopupMenu(AOwnerPopupMenu: TdxBarCustomPopupMenu;
  var APaths: TStringList; const ABasePath: string; ABarManager: TdxBarManager);
var
  I: Integer;
begin
  for I := 0 to ABarManager.ItemCount - 1 do
    if (ABarManager.Items[I] is TdxBarCustomButton) and
      (TdxBarCustomButton(ABarManager.Items[I]).DropDownMenu = AOwnerPopupMenu) then
      GetPathsByBarItem(ABarManager.Items[I], APaths, ABasePath);
end;

{ TdxOfficeSearchBoxRibbonSearchSourceHelper }

procedure TdxOfficeSearchBoxRibbonSearchSourceHelper.DoForAllSearchSourceItems(ASearchSource: TComponent;
  AProcRef: TdxOfficeSearchBoxSourceItemEnumProcRef);
var
  ARibbon: TdxCustomRibbon;
  ATab: TdxRibbonTab;
  I, J: Integer;
begin
  ARibbon := TdxCustomRibbon(ASearchSource);
  for I := 0 to ARibbon.TabCount - 1 do
  begin
    ATab := ARibbon.Tabs[I];
    if (ATab.Context = nil) or ATab.Context.Visible then
      for J := 0 to ATab.Groups.Count - 1 do
        if ATab.Groups[J].ToolBar <> nil then
          if not DoProcessItemLinks(ATab.Groups[J].ToolBar.ItemLinks, AProcRef) then
            Exit;
  end;
  if (ARibbon.QuickAccessToolbar.Toolbar <> nil) and ARibbon.QuickAccessToolbar.Visible then
    if not DoProcessItemLinks(ARibbon.QuickAccessToolbar.Toolbar.ItemLinks, AProcRef) then
      Exit;
  if (ARibbon.TabAreaToolbar.Toolbar <> nil) and ARibbon.TabAreaToolbar.Visible then
    if not DoProcessItemLinks(ARibbon.TabAreaToolbar.Toolbar.ItemLinks, AProcRef) then
      Exit;
  if (ARibbon.TabAreaSearchToolbar.Toolbar <> nil) and ARibbon.TabAreaSearchToolbar.Visible then
    if not DoProcessItemLinks(ARibbon.TabAreaSearchToolbar.Toolbar.ItemLinks, AProcRef) then
      Exit;
end;

procedure TdxOfficeSearchBoxRibbonSearchSourceHelper.GetPathsBy(AOwner: TComponent; var APaths: TStringList;
  const ABasePath: string);
begin
  if AOwner is TdxBarCustomPopupMenu then
    GetPathsByPopupMenu(TdxBarCustomPopupMenu(AOwner), APaths, ABasePath,
      TdxCustomRibbon(Properties.SearchSource).BarManager)
  else
    inherited GetPathsBy(AOwner, APaths, ABasePath);
end;

procedure TdxOfficeSearchBoxRibbonSearchSourceHelper.GetPathsByBar(AOwnerBar: TdxBar; var APaths: TStringList;
  const ABasePath: string);
var
  AGroup: TdxRibbonTabGroup;
  APath: string;
  ARibbon: TdxCustomRibbon;
  ATab: TdxRibbonTab;
  I: Integer;
begin
  ARibbon := TdxCustomRibbon(Properties.SearchSource);
  for I := 0 to ARibbon.TabCount - 1 do
  begin
    ATab := ARibbon.Tabs[I];
    AGroup := ATab.Groups.FindByToolBar(AOwnerBar);
    if AGroup <> nil then
    begin
      APath := ATab.Caption + GetPathDelimiter(APaths) + AGroup.Caption + GetPathDelimiter(APaths) + ABasePath;
      if ATab.Context <> nil then
        APath := ATab.Context.Caption + GetPathDelimiter(APaths) + APath;
      APaths.Add(APath);
    end;
  end;
  if (ARibbon.QuickAccessToolbar.Toolbar = AOwnerBar) and ARibbon.QuickAccessToolbar.Visible then
    APaths.Add(cxGetResourceString(@dxSBAR_RIBBON_QUICKACCESSTOOLBARNAME) + GetPathDelimiter(APaths) + ABasePath);
  if (ARibbon.TabAreaToolbar.Toolbar = AOwnerBar) and ARibbon.TabAreaToolbar.Visible then
    APaths.Add(cxGetResourceString(@dxSBAR_RIBBON_TABAREATOOLBARNAME) + GetPathDelimiter(APaths) + ABasePath);
  if (ARibbon.TabAreaSearchToolbar.Toolbar = AOwnerBar) and ARibbon.TabAreaSearchToolbar.Visible then
    APaths.Add(cxGetResourceString(@dxSBAR_RIBBON_TABAREASEARCHTOOLBARNAME) + GetPathDelimiter(APaths) + ABasePath);
end;

{ TdxOfficeSearchBoxPopupMenuSubMenuControl }

procedure TdxOfficeSearchBoxPopupMenuSubMenuControl.DoHideAll(AReason: TdxBarCloseUpReason);
begin
  inherited DoHideAll(AReason);
  if AReason in [bcrCancel, bcrEnter] then
    Windows.SetFocus(GetActiveWindow);
end;

function TdxOfficeSearchBoxPopupMenuSubMenuControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxOfficeSearchBoxPopupMenuSubMenuControlAccessibilityHelper;
end;

function TdxOfficeSearchBoxPopupMenuSubMenuControl.IsInternal: Boolean;
begin
  Result := True;
end;

function TdxOfficeSearchBoxPopupMenuSubMenuControl.IsNavigationKey(AKey: Word; AShift: TShiftState): Boolean;
begin
  Result := AKey in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_TAB, VK_RETURN];
end;

function TdxOfficeSearchBoxPopupMenuSubMenuControl.PreProcessKey(AKey: Word; AShift: TShiftState): Boolean;
begin
  if AKey = VK_MENU then
  begin
    if InnerEdit.FShiftIsEmpty then
    begin
      Hide;
      Windows.SetFocus(GetActiveWindow);
    end
    else
      InnerEdit.FShiftIsEmpty := True;
  end;
  Result := InnerEdit.InternalFocused or not IsNavigationKey(AKey, AShift) or inherited PreProcessKey(AKey, AShift);
end;

{ TdxOfficeSearchBoxPopupMenuSubMenuControlAccessibilityHelper }

function TdxOfficeSearchBoxPopupMenuSubMenuControlAccessibilityHelper.CanNavigateToChildren(AKey: Word): Boolean;
begin
  if not BarControl.InnerEdit.InternalFocused and (AKey = VK_UP) and (BarControl.TopItemControl <> nil) and
    BarControl.TopItemControl.IsSelected then
    BarControl.InnerEdit.InternalFocused := True;
  Result := not BarControl.InnerEdit.InternalFocused;
end;

function TdxOfficeSearchBoxPopupMenuSubMenuControlAccessibilityHelper.GetBarControl: TdxOfficeSearchBoxPopupMenuSubMenuControl;
begin
  Result := inherited GetBarControl as TdxOfficeSearchBoxPopupMenuSubMenuControl;
end;

{ TdxOfficeSearchBoxPopupMenu }

function TdxOfficeSearchBoxPopupMenu.CreateBarControl: TCustomdxBarControl;
var
  ASubMenuControl: TdxOfficeSearchBoxPopupMenuSubMenuControl;
begin
  Result := inherited CreateBarControl;
  ASubMenuControl := TdxOfficeSearchBoxPopupMenuSubMenuControl(Result);
  ASubMenuControl.InnerEdit := FInnerEdit;
  ASubMenuControl.OnMouseEnter := SubMenuControlMouseEnterHandler;
end;

function TdxOfficeSearchBoxPopupMenu.GetControlClass: TCustomdxBarControlClass;
begin
  Result := TdxOfficeSearchBoxPopupMenuSubMenuControl;
end;

function TdxOfficeSearchBoxPopupMenu.GetSubMenuControl: TdxOfficeSearchBoxPopupMenuSubMenuControl;
begin
  Result := inherited SubMenuControl as TdxOfficeSearchBoxPopupMenuSubMenuControl;
end;

procedure TdxOfficeSearchBoxPopupMenu.SubMenuControlMouseEnterHandler(Sender: TObject);
begin
  InnerEdit.InternalFocused := False;
end;

{ TdxOfficeSearchBoxInnerEdit }

constructor TdxOfficeSearchBoxInnerEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInternalFocused := True;
end;

destructor TdxOfficeSearchBoxInnerEdit.Destroy;
begin
  HideDropDownMenu;
  FreeAndNil(FDropDownMenu);
  inherited Destroy;
end;

procedure TdxOfficeSearchBoxInnerEdit.Change;
begin
  inherited Change;
  InternalFocused := True;
  if Container.InternalFocused then
    UpdateDropDownMenu;
end;

procedure TdxOfficeSearchBoxInnerEdit.DoEnter;
begin
  inherited DoEnter;
  InternalFocused := True;
end;

procedure TdxOfficeSearchBoxInnerEdit.DoExit;
begin
  inherited DoExit;
  HideDropDownMenu;
end;

procedure TdxOfficeSearchBoxInnerEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DOWN, VK_RETURN:
      if (DropDownMenu <> nil) and DropDownMenu.Visible then
      begin
        InternalFocused := False;
        Key := 0;
      end
      else
        if Key = VK_RETURN then
          Windows.SetFocus(GetActiveWindow);

    VK_ESCAPE:
      if Text <> '' then
      begin
        Clear;
        HideDropDownMenu;
        Key := 0;
      end;
  end;
  if Key <> 0 then
  begin
    FShiftIsEmpty := [ssAlt, ssCtrl] * Shift = [];
    inherited KeyDown(Key, Shift);
  end;
end;

procedure TdxOfficeSearchBoxInnerEdit.CreateDropDownMenu;
begin
  if ActiveProperties.BarManager <> nil then
  begin
    FDropDownMenu := TdxOfficeSearchBoxPopupMenu.Create(nil);
    DropDownMenu.InnerEdit := Self;
    DropDownMenu.BarManager := ActiveProperties.BarManager;
    DropDownMenu.Ribbon := ActiveProperties.Ribbon;
    DropDownMenu.OnCloseUpEx := DropDownMenuCloseUpExHandler;
  end;
end;

procedure TdxOfficeSearchBoxInnerEdit.HideDropDownMenu;
begin
  if (DropDownMenu <> nil) and DropDownMenu.Visible then
    DropDownMenu.SubMenuControl.Hide;
end;

procedure TdxOfficeSearchBoxInnerEdit.ShowDropDownMenu;
begin
  if (DropDownMenu <> nil) and not DropDownMenu.Visible then
  begin
    FStoredBarMenuAnimation := DropDownMenu.BarManager.MenuAnimations;
    DropDownMenu.BarManager.MenuAnimations := TdxBarMenuAnimations(0);
    DropDownMenu.FMinWidth := Container.VisibleBounds.Width;
    DropDownMenu.Popup(
      Container.ClientToScreen(Container.VisibleBounds.TopLeft).X,
      Container.ClientToScreen(Container.VisibleBounds.BottomRight).Y);
  end;
end;

procedure TdxOfficeSearchBoxInnerEdit.UpdateDropDownMenu;
begin
  if (ActiveProperties.BarManager = nil) or ActiveProperties.BarManager.IsDesigning then
    Exit;
  if (Text <> '') and (ActiveProperties.SearchSource <> nil) then
  begin
    if DropDownMenu = nil then
      CreateDropDownMenu;
    if DropDownMenu <> nil then
    begin
      ActiveProperties.PopulateDropDownMenu(Text, DropDownMenu.ItemLinks);
      if DropDownMenu.ItemLinks.Count > 0 then
        ShowDropDownMenu
      else
        HideDropDownMenu;
    end;
  end
  else
    HideDropDownMenu;
end;

function TdxOfficeSearchBoxInnerEdit.GetActiveProperties: TdxOfficeSearchBoxProperties;
begin
  Result := Container.ActiveProperties;
end;

function TdxOfficeSearchBoxInnerEdit.GetContainer: TdxOfficeSearchBox;
begin
  Result := inherited Container as TdxOfficeSearchBox;
end;

procedure TdxOfficeSearchBoxInnerEdit.SetInternalFocused(const AValue: Boolean);
begin
  if FInternalFocused <> AValue then
  begin
    FInternalFocused := AValue;
    if (DropDownMenu <> nil) and DropDownMenu.Visible then
    begin
      if FInternalFocused then
        DropDownMenu.SubMenuControl.SetKeySelectedItem(nil)
      else
        DropDownMenu.SubMenuControl.SetKeySelectedItem(DropDownMenu.SubMenuControl.TopItemControl);
    end;
  end;
end;

procedure TdxOfficeSearchBoxInnerEdit.DropDownMenuCloseUpExHandler(Sender: TObject; AReason: TdxBarCloseUpReason);
begin
  TdxOfficeSearchBoxPopupMenu(Sender).BarManager.MenuAnimations := FStoredBarMenuAnimation;
end;

function TdxOfficeSearchBoxInnerEdit.GetScaleFactor: TdxScaleFactor;
begin
  Result := Container.ScaleFactor;
end;

{ TdxOfficeSearchBox }

class function TdxOfficeSearchBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxOfficeSearchBoxProperties;
end;

function TdxOfficeSearchBox.HasPopupWindow: Boolean;
begin
  Result := HasInnerEdit and ((InnerEdit.Control as TdxOfficeSearchBoxInnerEdit).DropDownMenu <> nil) and
    (InnerEdit.Control as TdxOfficeSearchBoxInnerEdit).DropDownMenu.Visible;
end;

procedure TdxOfficeSearchBox.AdjustInnerEditPosition;
var
  ARect: TRect;
begin
  inherited AdjustInnerEditPosition;
  if HasInnerEdit and HandleAllocated then
  begin
    ARect := cxRectCenterVertically(ViewInfo.InnerEditRect, InnerEdit.Control.Height);
    InnerEdit.Control.SetBounds(ARect.Left, ARect.Top, cxRectWidth(ARect), cxRectHeight(ARect));
    AlignControls(InnerEdit.Control, ARect);
  end;
end;

procedure TdxOfficeSearchBox.Click;
begin
  inherited Click;
  if HasInnerEdit then
    TdxOfficeSearchBoxInnerEdit(InnerEdit.Control).Change;
end;

procedure TdxOfficeSearchBox.DoFocusChanged;
begin
  inherited DoFocusChanged;
  EditValue := Null;
end;

function TdxOfficeSearchBox.GetEditValue: Variant;
begin
  Result := Null;
end;

function TdxOfficeSearchBox.GetInnerEditClass: TControlClass;
begin
  Result := TdxOfficeSearchBoxInnerEdit;
end;

function TdxOfficeSearchBox.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxOfficeSearchBoxViewInfo;
end;

procedure TdxOfficeSearchBox.SetEditValue(const Value: Variant);
begin
  inherited SetEditValue(Null);
end;

function TdxOfficeSearchBox.GetActiveProperties: TdxOfficeSearchBoxProperties;
begin
  Result := TdxOfficeSearchBoxProperties(InternalGetActiveProperties);
end;

function TdxOfficeSearchBox.GetProperties: TdxOfficeSearchBoxProperties;
begin
  Result := inherited Properties as TdxOfficeSearchBoxProperties;
end;

function TdxOfficeSearchBox.GetViewInfo: TdxOfficeSearchBoxViewInfo;
begin
  Result := inherited ViewInfo as TdxOfficeSearchBoxViewInfo;
end;

procedure TdxOfficeSearchBox.SetProperties(AValue: TdxOfficeSearchBoxProperties);
begin
  Properties.Assign(AValue);
end;

initialization
  GetRegisteredEditProperties.Register(TdxOfficeSearchBoxProperties, 'OfficeSearchBox');

finalization
  GetRegisteredEditProperties.Unregister(TdxOfficeSearchBoxProperties);
end.
