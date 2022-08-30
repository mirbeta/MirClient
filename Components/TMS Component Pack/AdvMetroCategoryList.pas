{***************************************************************************}
{ TAdvMetroCategoryList component                                           }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvMetroCategoryList;

{$I TMSDEFS.INC}

{$R AdvMetroCategoryList.RES}

interface

uses
  Windows, Messages, Forms, StdCtrls, SysUtils, Classes, Controls, Graphics, AdvStyleIF,
  GDIPicture, ImgList, PictureContainer, Types, Math
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

type
  TAdvMetroCategoryList = class;
  TCategory = class;
  TCategoryItem = class;
  TCheckBoxCategory = (cNone, cCategory, cItems);
  TItemPagingMode = (ipmCategory, ipmItem);
  TBeforeCategoryDrawEvent = procedure(Sender: TObject; Canvas: TCanvas; PaintRect: TRect; Item: TCategory; var Allow: Boolean) of object;
  TAfterCategoryDrawEvent = procedure(Sender: TObject; Canvas: TCanvas; PaintRect: TRect; Item: TCategory) of object;
  TBeforeItemDrawEvent = procedure(Sender: TObject; Canvas: TCanvas; PaintRect: TRect; Item: TCategoryItem; var Allow: Boolean) of object;
  TAfterItemDrawEvent = procedure(Sender: TObject; Canvas: TCanvas; PaintRect: TRect; Item: TCategoryItem) of object;
  TItemPropEvent = procedure(Sender: TObject; Font: TFont; Brush: TBrush; Item: TCategoryItem) of object;
  TAnchorClick = procedure(Sender: TObject; Text: string) of object;
  TItemSelect = procedure(Sender: TObject; CategoryIndex, ItemIndex: Integer; var Allow: Boolean) of object;
  TCategoryCollapseExpandEvent = procedure(Sender: TObject; CategoryIndex: Integer; var Allow: Boolean) of object;

  TCategoryItem = class(TCollectionItem)
  private
    { Private declarations }
    FChecked: Boolean;
    FOnChange: TNotifyEvent;
    FSelected: Boolean;
    FText: string;
    FTextHeight: Integer;
    FDirty: Boolean;
    FCheckDirty: Boolean;
    FHTMLDirty: Boolean;
    FStartY: Integer;
    FEndY: Integer;
    FPaint: Boolean;
    FShowCheckBox: Boolean;
    FHint: string;
    FAfterDraw: TAfterItemDrawEvent;
    FBeforeDraw: TBeforeItemDrawEvent;
    procedure Changed(Dirty: Boolean = True);
    function GetPaintRect(OffsetY: Integer; ClientRect: TRect): TRect;
    function GetCheckBoxRect(PaintRect: TRect; CheckBoxImage: TGDIPPicture): TRect;
    function GetTextRect(PaintRect: TRect): TRect;
    procedure SetChecked(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetShowCheckBox(const Value: Boolean);
    function GetCategory: TCategory;
  protected
    { Protected declarations }
  public
    { Public declarations }
    property Category: TCategory read GetCategory;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure CheckUncheck;
  published
    { Published declarations }
    property Checked: Boolean read FChecked write SetChecked default False;
    property Hint: string read FHint write FHint;
    property ShowCheckBox: Boolean read FShowCheckBox write SetShowCheckBox default True;
    property Text: string read FText write SetText;

    property AfterDraw: TAfterItemDrawEvent read FAfterDraw write FAfterDraw;
    property BeforeDraw: TBeforeItemDrawEvent read FBeforeDraw write FBeforeDraw;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TCategoryItems = class(TOwnedCollection)
  private
    { Private declarations }
    FOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
  protected
    { Protected declarations }
    function GetItem(Index: Integer): TCategoryItem;
    procedure SetItem(Index: Integer; Value: TCategoryItem);
    procedure Update(Item: TCollectionItem); override;
  public
    { Public declarations }
    constructor Create(AOwner: TCategory);
    destructor Destroy; override;
    function Add: TCategoryItem;
    function Insert(Index: Integer): TCategoryItem;
    property Items[Index: Integer]: TCategoryItem read GetItem write SetItem; default;
    procedure Clear;
  published
    { Published declarations }
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TCategory = class(TCollectionItem)
  private
    { Private declarations }
    FCheckedState: TCheckBoxState;
    FImageLeft: TGDIPPicture;
    FImageRight: TGDIPPicture;
    FItems: TCategoryItems;
    FOnChange: TNotifyEvent;
    FText: string;
    FTextHeight: Integer;
    FDirty: Boolean;
    FCheckDirty: Boolean;
    FHTMLDirty: Boolean;
    FStartY: Integer;
    FEndY: Integer;
    FPaint: Boolean;
    FCollapse: Boolean;
    FChecked: Boolean;
    FShowCheckBox: Boolean;
    FSelected: Boolean;
    FCollapseDirty: Boolean;
    FItemsHeight: Integer;
    FHint: string;
    FAfterDraw: TAfterCategoryDrawEvent;
    FBeforeDraw: TBeforeCategoryDrawEvent;
    procedure ApplyCheckState(Range: TCheckBoxCategory);
    procedure SetCheckState(Range: TCheckBoxCategory);
    procedure Changed(Dirty: Boolean = True);
    function GetPaintRect(OffsetY, MinHeight: Integer; ClientRect: TRect; Sticky: Boolean; MaxStickySize: Integer = -1): TRect;
    function GetCheckBoxRect(PaintRect: TRect; CheckBoxImage: TGDIPPicture): TRect;
    function GetCollapseRect(PaintRect: TRect; Image: TGDIPPicture): TRect;
    function GetTextRect(PaintRect: TRect): TRect;
    procedure SetImageLeft(const Value: TGDIPPicture);
    procedure SetImageRight(const Value: TGDIPPicture);
    procedure SetItems(const Value: TCategoryItems);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetText(const Value: string);
    procedure ItemsChanged(Sender: TObject);
    procedure SetChecked(const Value: Boolean);
    procedure SetShowCheckBox(const Value: Boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property CheckedState: TCheckBoxState read FCheckedState;
    procedure Collapse;
    procedure ChangeCheck(Range: TCheckBoxCategory);
    procedure Expand;
    procedure CollapseExpand;
  published
    { Published declarations }
    property Checked: Boolean read FChecked write SetChecked default False;
    property Hint: string read FHint write FHint;
    property ImageLeft: TGDIPPicture read FImageLeft write SetImageLeft;
    property ImageRight: TGDIPPicture read FImageRight write SetImageRight;
    property Items: TCategoryItems read FItems write SetItems;
    property ShowCheckBox: Boolean read FShowCheckBox write SetShowCheckBox default True;
    property Text: string read FText write SetText;

    property AfterDraw: TAfterCategoryDrawEvent read FAfterDraw write FAfterDraw;
    property BeforeDraw: TBeforeCategoryDrawEvent read FBeforeDraw write FBeforeDraw;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TCategories = class(TOwnedCollection)
  private
    { Private declarations }
    FOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
  protected
    { Protected declarations }
    function GetItem(Index: Integer): TCategory;
    procedure SetItem(Index: Integer; Value: TCategory);
    procedure Update(Item: TCollectionItem); override;
  public
    { Public declarations }
    constructor Create(AOwner: TAdvMetroCategoryList);
    destructor Destroy; override;
    function Add: TCategory;
    function Insert(Index: Integer): TCategory;
    procedure Clear;
    property Items[Index: Integer]: TCategory read GetItem write SetItem; default;
  published
    { Published declarations }
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TMetroCategoryListAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FBackgroundTextColor: TColor;
    FBackgroundColor: TColor;
    FCategoryTextColor: TColor;
    FCategoryColor: TColor;
    FDisabledColor: TColor;
    FDisabledTextColor: TColor;
    FSelectedItemTextColor: TColor;
    FSelectedItemColor: TColor;
    FScrollBarColor: TColor;
    procedure SetCategoryColor(const Value: TColor);
    procedure SetCategoryTextColor(const Value: TColor);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBackgroundTextColor(const Value: TColor);
    procedure SetDisabledColor(const Value: TColor);
    procedure SetDisabledTextColor(const Value: TColor);
    procedure SetSelectedItemColor(const Value: TColor);
    procedure SetSelectedItemTextColor(const Value: TColor);
    procedure SetScrollBarColor(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clWhite;
    property BackgroundTextColor: TColor read FBackgroundTextColor write SetBackgroundTextColor default clBlack;
    property CategoryColor: TColor read FCategoryColor write SetCategoryColor default $00B0A374;
    property CategoryTextColor: TColor read FCategoryTextColor write SetCategoryTextColor default clWhite;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clSilver;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGray;
    property SelectedItemColor: TColor read FSelectedItemColor write SetSelectedItemColor default clSkyBlue;
    property SelectedItemTextColor: TColor read FSelectedItemTextColor write SetSelectedItemTextColor default clWhite;
    property ScrollBarColor: TColor read FScrollBarColor write SetScrollBarColor default clGray;
  end;

  TAdvMetroCategoryList = class(TCustomControl, ITMSTones)
  private
    { Private declarations }
    FAppearance: TMetroCategoryListAppearance;
    FCategories: TCategories;
    FCategoryFont: TFont;
    FMaxY: Integer;
    FOffsetY: Integer;
    FItemFont: TFont;
    FMouseUpLast: Boolean;
    FLastStickyIndex: Integer;
    FHalfStickyIndex: Integer;
    FFirstStickyIndex: Integer;
    FStickyEncountered: Boolean;
    FHalfStickyEncountered: Boolean;
    FFirstStickyEncountered: Boolean;
    FMouseUpX: Integer;
    FMouseUpY: Integer;
    FMouseDownX: Integer;
    FMouseDownY: Integer;
    FMouseDownLast: Boolean;
    FMouseDownInScrollRect: Boolean;
    FMouseDownOffsetY: Integer;
    FMouseDownScrollbarOffset: Integer;
    FUpdateCount: Integer;
    FScrollRect: TRect;
    FShowScroll: Boolean;
    FCategoryMinHeight: Integer;
    FImages: TCustomImageList;
    FPictureContainer: TPictureContainer;
    FSticky: Boolean;
    FMaxOffSetY: Integer;
    FItemMinHeight: Integer;
    FTopCategoryIndex: Integer;
    FTopItemIndex: Integer;
    FLastCatIndex: Integer;
    FLastItemIndex: Integer;
    FSelectedCategoryIndex: Integer;
    FSelectedItemIndex: Integer;
    FMoveToSelection: Boolean;
    FPageMode: TItemPagingMode;
    FPageSize: Integer;
    FMaxStickySize: Integer;
    FCategoryCheckboxRange: TCheckBoxCategory;
    FAutoCalcOnResize: Boolean;
    FImageGrayed: TGDIPPicture;
    FImageChecked: TGDIPPicture;
    FImageUnChecked: TGDIPPicture;
    FBeforeItemDraw: TBeforeItemDrawEvent;
    FBeforeCategoryDraw: TBeforeCategoryDrawEvent;
    FAfterItemDraw: TAfterItemDrawEvent;
    FAfterCategoryDraw: TAfterCategoryDrawEvent;
    FForceFullCalcOnce: Boolean;
    FHoverCat: Integer;
    FHoverItem: Integer;
    FImageCollapse: TGDIPPicture;
    FImageExpand: TGDIPPicture;
    FOnAnchorClick: TAnchorClick;
    FScrollDistance: Integer;
    FOnItemSelect: TItemSelect;
    FOnCollapseCategory: TCategoryCollapseExpandEvent;
    FOnExpandCategory: TCategoryCollapseExpandEvent;
    FOnScroll: TNotifyEvent;
    // WM
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    //CM
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    //normal procedures/functions
    procedure AppearanceChanged(Sender: TObject);
    procedure CalculateSizes(ForceFullCalc: Boolean = False);
    procedure CheckOffsets; overload;
    procedure CheckOffsets(CategoryIndex, ItemIndex: Integer); overload;
    procedure ItemsToBePainted;
    procedure CategoriesChanged(Sender: TObject);
    function DesignTime: Boolean;
    function GetVersion: string;
    function GetVersionNr: Integer;
    procedure Scrolled;
    procedure SetAppearance(const Value: TMetroCategoryListAppearance);
    procedure SetCategories(const Value: TCategories);
    procedure SetCategoryFont(const Value: TFont);
    procedure SetItemFont(const Value: TFont);
    procedure SetVersion(const Value: string);
    procedure SetCategoryMinHeight(const Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetPictureContainer(const Value: TPictureContainer);
    procedure CalculateScrollRect;
    procedure SetSticky(const Value: Boolean);
    procedure SetItemMinHeight(const Value: Integer);
    procedure Previous;
    procedure Next;
    procedure PageUp;
    procedure PageDown;
    procedure First(GoToItem: Boolean = True);
    procedure Last(GoToItem: Boolean = True);
    procedure Deselect;
    procedure Select;
    procedure DeselectAll;
    procedure SetPageSize(const Value: Integer);
    procedure SetCategoryCheckboxRange(const Value: TCheckBoxCategory);
    procedure SetAutoCalcOnResize(const Value: Boolean);
    procedure SetImageGrayed(const Value: TGDIPPicture);
    procedure SetImageChecked(const Value: TGDIPPicture);
    procedure SetImageUnChecked(const Value: TGDIPPicture);
    procedure CheckSelection;
    procedure CollapseSelection;
    procedure ExpandSelection;
    procedure SetCheckBoxes;
    procedure SetImageCollapse(const Value: TGDIPPicture);
    procedure SetImageExpand(const Value: TGDIPPicture);
    function AnchorTextAt(X, Y: Integer; Category: TCategory; TextRect: TRect): string; overload;
    function AnchorTextAt(X, Y: Integer; Item: TCategoryItem; TextRect: TRect): string; overload;
    procedure SetScrollDistance(const Value: Integer);
    procedure CollapseExpand(Category: TCategory);
    procedure SetSelectedCategoryIndex(const Value: Integer);
    procedure SetSelectedItemIndex(const Value: Integer);
    procedure SetTopCategoryIndex(const Value: Integer);
    procedure SetTopItemIndex(const Value: Integer);
  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;

    class function XYInRect(X,Y: Integer; r: TRect): Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure PaintCategory(Category: TCategory);
    procedure PaintItem(Item: TCategoryItem);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetColorTones(ATones: TColorTones);
    procedure MouseWheelHandler(var Message: TMessage); override;
    procedure ForceSizeCalculations(IgnoreUpdateStatus: Boolean = False);
    procedure MoveToSelection;
  published
    { Published declarations }
    property Align;
    property AutoCalcOnResize: Boolean read FAutoCalcOnResize write SetAutoCalcOnResize default False;
    property Anchors;
    property Appearance: TMetroCategoryListAppearance read FAppearance write SetAppearance;
    property Categories: TCategories read FCategories write SetCategories;
    property CategoryCheckboxRange: TCheckBoxCategory read FCategoryCheckboxRange write SetCategoryCheckboxRange default cItems;
    property CategoryFont: TFont read FCategoryFont write SetCategoryFont;
    property CategoryMinHeight: Integer read FCategoryMinHeight write SetCategoryMinHeight default 50;
    property Constraints;
    property DoubleBuffered default True;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Height default 200;
    property ImageChecked: TGDIPPicture read FImageChecked write SetImageChecked;
    property ImageCollapse: TGDIPPicture read FImageCollapse write SetImageCollapse;
    property ImageExpand: TGDIPPicture read FImageExpand write SetImageExpand;
    property ImageGrayed: TGDIPPicture read FImageGrayed write SetImageGrayed;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageUnChecked: TGDIPPicture read FImageUnChecked write SetImageUnChecked;
    property ItemFont: TFont read FItemFont write SetItemFont;
    property ItemMinHeight: Integer read FItemMinHeight write SetItemMinHeight default 26;
    property PageMode: TItemPagingMode read FPageMode write FPageMode default ipmCategory;
    property PageSize: Integer read FPageSize write SetPageSize default 1;
    property PictureContainer: TPictureContainer read FPictureContainer write SetPictureContainer;
    property PopupMenu;
    property ScrollDistance: Integer read FScrollDistance write SetScrollDistance default 50;
    property SelectedCategoryIndex: Integer read FSelectedCategoryIndex write SetSelectedCategoryIndex default -1;
    property SelectedItemIndex: Integer read FSelectedItemIndex write SetSelectedItemIndex default -1;
    property ShowHint default True;
    property Sticky: Boolean read FSticky write SetSticky default True;
    property Tag;
    property TabOrder;
    property TabStop default True;
    property Top;
    property TopCategoryIndex: Integer read FTopCategoryIndex write SetTopCategoryIndex default -1;
    property TopItemIndex: Integer read FTopItemIndex write SetTopItemIndex default -1;
    property Touch;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property Width default 150;

    property AfterCategoryDraw: TAfterCategoryDrawEvent read FAfterCategoryDraw write FAfterCategoryDraw;
    property AfterItemDraw: TAfterItemDrawEvent read FAfterItemDraw write FAfterItemDraw;
    property BeforeCategoryDraw: TBeforeCategoryDrawEvent read FBeforeCategoryDraw write FBeforeCategoryDraw;
    property BeforeItemDraw: TBeforeItemDrawEvent read FBeforeItemDraw write FBeforeItemDraw;
    property OnAnchorClick: TAnchorClick read FOnAnchorClick write FOnAnchorClick;
    property OnClick;
    property OnCollapseCategory: TCategoryCollapseExpandEvent read FOnCollapseCategory write FOnCollapseCategory;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnExpandCategory: TCategoryCollapseExpandEvent read FOnExpandCategory write FOnExpandCategory;
    property OnGesture;
    property OnItemSelect: TItemSelect read FOnItemSelect write FOnItemSelect;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  ComObj, Commctrl, ShellApi;

{$I HTMLEngo.pas}

{ TCategories }

function TCategories.Add: TCategory;
begin
  Result := TCategory(inherited Add);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCategories.Clear;
begin
  (Owner as TAdvMetroCategoryList).BeginUpdate;
  try
    while Count > 0 do
      TCollectionItem(Items[Count - 1]).Free;
  finally
    (Owner as TAdvMetroCategoryList).EndUpdate;
  end;
end;

constructor TCategories.Create(AOwner: TAdvMetroCategoryList);
begin
  inherited Create(AOwner, TCategory);
end;

destructor TCategories.Destroy;
begin

  inherited;
end;

function TCategories.GetItem(Index: Integer): TCategory;
begin
  Result := TCategory(inherited GetItem(Index));
end;

function TCategories.Insert(Index: Integer): TCategory;
begin
  Result := TCategory(inherited Insert(Index));
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCategories.SetItem(Index: Integer; Value: TCategory);
begin
  inherited SetItem(Index, Value);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCategories.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TCategories.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TCategoryItems }

function TCategoryItems.Add: TCategoryItem;
begin
  Result := TCategoryItem(inherited Add);
  if Assigned(FOnChange) then
    FOnChange(Self);
  Update(Result);
end;

procedure TCategoryItems.Clear;
var
  RootOwner: TAdvMetroCategoryList;
begin
  RootOwner := (Owner as TCategory).Collection.Owner as TAdvMetroCategoryList;
  RootOwner.BeginUpdate;
  try
    (Owner as TCategory).FDirty := True;
    while Count > 0 do
      TCollectionItem(Items[Count - 1]).Free;
  finally
    RootOwner.EndUpdate;
  end;
end;

constructor TCategoryItems.Create(AOwner: TCategory);
begin
  inherited Create(AOwner, TCategoryItem);
end;

destructor TCategoryItems.Destroy;
begin

  inherited;
end;

function TCategoryItems.GetItem(Index: Integer): TCategoryItem;
begin
  Result := TCategoryItem(inherited GetItem(Index));
end;

function TCategoryItems.Insert(Index: Integer): TCategoryItem;
begin
  Result := TCategoryItem(inherited Insert(Index));
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCategoryItems.SetItem(Index: Integer; Value: TCategoryItem);
begin
  inherited SetItem(Index, Value);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCategoryItems.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TCategoryItems.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TCategory }

procedure TCategory.ChangeCheck(Range: TCheckBoxCategory);
begin
  if Range <> cNone then
  begin
    if FCheckedState = cbChecked then
      FCheckedState := cbUnchecked
    else
      FCheckedState := cbChecked;

    ApplyCheckState(Range);
  end
end;

procedure TCategory.Changed(Dirty: Boolean = True);
begin
  FDirty := Dirty;
  if Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
  (Collection as TCategories).Update(Self);
end;

procedure TCategory.Collapse;
begin
  if not FCollapse then
  begin
    FCollapse := True;
    FCollapseDirty := True;
    Changed;
  end;
end;

procedure TCategory.CollapseExpand;
begin
  FCollapse := not FCollapse;
  FCollapseDirty := True;
  Changed;
end;

constructor TCategory.Create(Collection: TCollection);
begin
  inherited;
  FImageLeft := TGDIPPicture.Create;
  FImageRight := TGDIPPicture.Create;
  FItems := TCategoryItems.Create(Self);
  FItems.OnChange := ItemsChanged;
  FChecked := False;
  FCheckedState := cbUnchecked;
  FSelected := False;
  FDirty := True;
  FHTMLDirty := True;
  FCollapseDirty := False;
  FShowCheckBox := True;
  FPaint := False;
  FStartY := 0;
  FItemsHeight := 0;
end;

destructor TCategory.Destroy;
begin
  FImageLeft.Free;
  FImageRight.Free;
  FItems.Free;
  inherited;
end;

procedure TCategory.Expand;
begin
  if FCollapse then
  begin
    FCollapse := False;
    FCollapseDirty := True;
    Changed;
  end;
end;

function TCategory.GetCheckBoxRect(PaintRect: TRect; CheckBoxImage: TGDIPPicture): TRect;
var
  Height: Integer;
begin
  Height := FEndY - FStartY;
  CheckBoxImage.GetImageSizes;
  Result.Left := PaintRect.Left + 5;
  Result.Right := Result.Left + CheckBoxImage.Width;
  Result.Top := PaintRect.Top + (Height - CheckBoxImage.Height) div 2;
  Result.Bottom := Result.Top + CheckBoxImage.Height;
end;

function TCategory.GetCollapseRect(PaintRect: TRect; Image: TGDIPPicture): TRect;
var
  Height: Integer;
begin
  Height := FEndY - FStartY;
  Image.GetImageSizes;
  Result.Right := PaintRect.Right - 20;
  Result.Left := Result.Right - Image.Width;
  Result.Top := PaintRect.Top + (Height - Image.Height) div 2;
  Result.Bottom := Result.Top + Image.Height;
end;

function TCategory.GetPaintRect(OffsetY, MinHeight: Integer; ClientRect: TRect; Sticky: Boolean; MaxStickySize: Integer = -1): TRect;
var
  r: TRect;
  StickyOffset: Integer;
begin
  r := ClientRect;
  if Sticky then
  begin
    StickyOffset := Max(MinHeight, FEndY - FStartY);
    r.Top := Max(FStartY - OffsetY, 0);
    r.Bottom := Max(StickyOffset, FEndY - OffsetY);

    if (MaxStickySize > 0) and (r.Bottom = StickyOffset) then
    begin
      r.Bottom := Min(MaxStickySize, StickyOffset);
      r.Top := r.Bottom - StickyOffset;
    end
  end
  else
  begin
    r.Top := FStartY - OffsetY;
    r.Bottom := FEndY - OffsetY;
  end;
  Result := r;
end;

function TCategory.GetTextRect(PaintRect: TRect): TRect;
begin
  Result := PaintRect;
  InflateRect(Result, -5, -5);
  Result.Right := Result.Right - 50;
  if Assigned(FImageLeft) then
  begin
    FImageLeft.GetImageSizes;
    Result.Left := Result.Left + FImageLeft.Width;
  end;
  if Assigned(FImageRight) then
  begin
    FImageLeft.GetImageSizes;
    Result.Right := Result.Right - FImageRight.Width;
  end;
  if FShowCheckBox then
    Result.Left := Result.Left + 21;
  Result.Top := PaintRect.Top + (FEndY - FStartY - FTextHeight) div 2;
end;

procedure TCategory.ItemsChanged(Sender: TObject);
begin
  (Collection as TCategories).Update(Self);
end;

procedure TCategory.ApplyCheckState(Range: TCheckBoxCategory);
var
  I: Integer;
begin
  if Range = cItems then
  begin
    if FCheckedState = cbChecked then
    begin
      for I := 0 to FItems.Count - 1 do
        FItems[I].FChecked := True;
    end
    else if FCheckedState = cbUnchecked then
    begin
      for I := 0 to FItems.Count - 1 do
        FItems[I].FChecked := False;
    end;
  end;
  Changed;
end;

procedure TCategory.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    FCheckDirty := True;
    if FChecked then
      FCheckedState := cbChecked
    else
      FCheckedState := cbUnchecked;
    Changed(False);
  end;
end;

procedure TCategory.SetCheckState(Range: TCheckBoxCategory);
var
  FoundChecked, FoundUnchecked: Boolean;
  I, iCount: Integer;
  Item: TCategoryItem;
begin
  if Range = cItems then
  begin
    FoundChecked := False;
    FoundUnchecked := False;
    iCount := 0;
    if Items <> nil then
      iCount := FItems.Count;
    if iCount > 0 then
    begin
      for I := 0 to iCount - 1 do
      begin
        Item := FItems[I];
        if not FoundChecked then
          FoundChecked := Item.FChecked;
        if not FoundUnchecked then
          FoundUnchecked := not Item.FChecked;
      end;
      if FoundChecked and FoundUnchecked then
        FCheckedState := cbGrayed
      else if FoundChecked then
        FCheckedState := cbChecked
      else
        FCheckedState := cbUnchecked;
    end;
  end;
end;

procedure TCategory.SetImageLeft(const Value: TGDIPPicture);
begin
  if Value <> FImageLeft then
  begin
    FImageLeft.Assign(Value);
    Changed;
  end;
end;

procedure TCategory.SetImageRight(const Value: TGDIPPicture);
begin
  if Value <> FImageRight then
  begin
    FImageRight.Assign(Value);
    Changed;
  end;
end;

procedure TCategory.SetItems(const Value: TCategoryItems);
begin
  if Value <> FItems then
  begin
    FItems.Assign(Value);
  end;
end;

procedure TCategory.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TCategory.SetShowCheckBox(const Value: Boolean);
begin
  if FShowCheckBox <> Value then
  begin
    FShowCheckBox := Value;
    Changed;
  end;
end;

procedure TCategory.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    FDirty := True;
    FHTMLDirty := True;
    Changed;
  end;
end;

{ TAdvMetroCategoryList }

class function TAdvMetroCategoryList.XYInRect(X,Y: Integer; r: TRect): Boolean;
var
  xmin, xmax, ymin, ymax: Integer;
begin
  xmin := r.Left;
  xmax := r.Right;
  ymin := r.Top;
  ymax := r.Bottom;
  Result := (X >= xmin) and (X <= xmax) and (Y >= ymin) and (Y <= ymax);
end;

function TAdvMetroCategoryList.AnchorTextAt(X, Y: Integer; Category: TCategory; TextRect: TRect): string;
var
  xs, ys, HyperLinks, MouseLink: Integer;
  a, s, focusanchor: string;
  res: boolean;
  r: TRect;
begin
  Result := '';
  a := '';
  Canvas.Font := FCategoryFont;
  res := HTMLDrawEx(Canvas, Category.Text, TextRect, FImages, X, Y, -1, -1, 1, True, False, False, False, False, False, True, 1.0,
             clBlue, clNone, clNone, clGray, a, s, focusanchor, xs, ys, HyperLinks, MouseLink, r, nil, FPictureContainer, 0);

  if res then
    Result := a;
end;

function TAdvMetroCategoryList.AnchorTextAt(X, Y: Integer; Item: TCategoryItem; TextRect: TRect): string;
var
  xs, ys, HyperLinks, MouseLink: Integer;
  a, s, focusanchor: string;
  res: boolean;
  r: TRect;
begin
  Result := '';
  a := '';
  Canvas.Font := FItemFont;
  res := HTMLDrawEx(Canvas, Item.Text, TextRect, FImages, X, Y, -1, -1, 1, True, False, False, False, False, False, True, 1.0,
             clBlue, clNone, clNone, clGray, a, s, focusanchor, xs, ys, HyperLinks, MouseLink, r, nil, FPictureContainer, 0);

  if res then
    Result := a;
end;

procedure TAdvMetroCategoryList.AppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvMetroCategoryList.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TAdvMetroCategoryList.CalculateScrollRect;
var
  w, h, x, y: Integer;
begin
  if FShowScroll then
  begin
    if FMaxY > Height then
    begin
      FOffsetY := Min(FMaxY - Height, FOffsetY);
      FOffsetY := Max(0, FOffsetY);
      w := 5;
      x := Width - 10;
      h := Trunc((Height / FMaxY) * Height);
      y := Trunc((FOffsetY / FMaxY) * (Height - 10 + min(10, h)));
      h := Max (10, h);
      FScrollRect := Bounds(x, y, w, h);
    end
    else
      FScrollRect := Rect(0,0,0,0);
  end;
end;

procedure TAdvMetroCategoryList.CalculateSizes(ForceFullCalc: Boolean = False);
var
  CatCount, ItemCount, I, J, MaxY, xs, ys, links, mlinks, prevCount, TextOffset: Integer;
  tr, hr: TRect;
  Category: TCategory;
  Item, prevItem: TCategoryItem;
  a, s, fs: string;
  ReachedFirstDirty: Boolean;
begin
  MaxY := 0;
  ReachedFirstDirty := False;
  CatCount := FCategories.Count;
  for I := 0 to CatCount - 1 do
  begin
    Category := FCategories[I];
    with Category do
    begin
      if (not ForceFullCalc) and FDirty then
      begin
        if not ReachedFirstDirty then
        begin
          ReachedFirstDirty := True;
          if (I <> 0) and (MaxY <> 0) then
            MaxY := FStartY
          else if (I <> 0) then
          begin
            prevCount := FCategories[I - 1].Items.Count;
            if FCategories[I - 1].FCollapse or (prevCount < 1) then
            begin
              MaxY := FCategories[I - 1].FEndY;
            end
            else
            begin
              prevItem := FCategories[I - 1].Items[prevCount -1];
              MaxY := prevItem.FEndY;
            end;
          end;
        end;
      end;
      if ForceFullCalc or ReachedFirstDirty then
      begin
        if FCollapseDirty then
        begin
          FMoveToSelection := False;
          FCollapseDirty := False;
          if FOffsetY > FStartY then
            FOffsetY := MaxY;
        end;
        FStartY := MaxY;
        if ForceFullCalc or FHTMLDirty then
        begin
          TextOffset := 40;
          if FShowCheckBox then
          begin
            case FCheckedState of
              cbUnchecked:
              begin
                FImageUnChecked.GetImageSizes;
                Inc(TextOffset, FImageUnChecked.Width + 5);
              end;
              cbChecked:
              begin
                FImageChecked.GetImageSizes;
                Inc(TextOffset, FImageChecked.Width + 5);
              end;
              cbGrayed:
              begin
                FImageGrayed.GetImageSizes;
                Inc(TextOffset, FImageGrayed.Width + 5);
              end;
            end;
          end;
          if FShowScroll then
            Inc(TextOffset, 15);
          if Assigned(FImageLeft) then
          begin
            FImageLeft.GetImageSizes;
            Inc(TextOffset, FImageLeft.Width + 5);
          end;
          if Assigned(FImageRight) then
          begin
            FImageRight.GetImageSizes;
            Inc(TextOffset, FImageRight.Width + 5);
          end;
          tr := Bounds(0, 0, Width - TextOffset, $FFF);
          Canvas.Font := FCategoryFont;

          HTMLDrawEx(Canvas, text, tr, FImages, -1, -1, -1, -1, -1, True, True, False, False, False, False, True, 1.0,
              clBlue, clNone, clNone, clGray, a, s, fs, xs, ys, links, mlinks, hr, nil, FPictureContainer, 0);

          tr.Bottom := tr.Top + ys;
          FTextHeight := tr.Bottom;
          FHTMLDirty := False;
        end;
        FEndY := FStartY + Max (FCategoryMinHeight, FTextHeight + 10);

        if Assigned(FImageLeft) then
        begin
          FImageLeft.GetImageSizes;
          FEndY := Max(FEndY, FStartY + FImageLeft.Height + 10);
        end;
        if Assigned(FImageRight) then
        begin
          FImageRight.GetImageSizes;
          FEndY := Max(FEndY, FStartY + FImageRight.Height + 10);
        end;

        MaxY := FEndY;
        FDirty := false;
      end;
    end;
    if (Category.Items <> nil) and (not Category.FCollapse) then
    begin
      ItemCount := Category.Items.Count;
      for J := 0 to ItemCount - 1 do
      begin
        Item := Category.Items[J];
        with Item do
        begin
          if (not ForceFullCalc) and FDirty then
          begin
            if not ReachedFirstDirty then
            begin
              ReachedFirstDirty := True;
              if J = 0 then
                MaxY := FCategories[I].FEndY
              else
                MaxY := FCategories[I].Items[J - 1].FEndY;
            end;
          end;
          if ForceFullCalc or ReachedFirstDirty then
          begin
            FStartY := MaxY;
            if ForceFullCalc or FHTMLDirty then
            begin
              TextOffset := 0;
              if Item.FShowCheckBox then
              begin
                if Item.FChecked then
                begin
                  FImageChecked.GetImageSizes;
                  Inc(TextOffset, FImageChecked.Width + 5);
                end
                else
                begin
                  FImageUnChecked.GetImageSizes;
                  Inc(TextOffset, FImageUnChecked.Width + 5);
                end;
              end;
              if FShowScroll then 
                Inc(TextOffset, 15);
              tr := Bounds(0, 0, Width - TextOffset, $FFF);
              Canvas.Font := FItemFont;
              HTMLDrawEx(Canvas, Text, tr, FImages, -1, -1, -1, -1, -1, True, True, False, False, False, False, True, 1.0,
                    clBlue, clNone, clNone, clGray, a, s, fs, xs, ys, links, mlinks, hr, nil, FPictureContainer, 0);

              FTextHeight := ys;
              FHTMLDirty := False;
            end;
            FEndY := FStartY + Max (FItemMinHeight, FTextHeight + 10);
            MaxY := FEndY;
            Category.FItemsHeight := FEndY - Category.FEndY;
            FDirty := false;
          end;
        end;
      end;
    end;
  end;
  if (Categories.Count = 0) or ReachedFirstDirty then
  begin
    FMaxY := MaxY;
    FMaxOffSetY := Max(0, FMaxy - Height);
  end;
end;

procedure TAdvMetroCategoryList.CategoriesChanged(Sender: TObject);
begin
  if FUpdateCount = 0 then
  begin
    Inc(FUpdateCount);
    CalculateSizes(FAutoCalcOnResize or FForceFullCalcOnce);
    Scrolled;
    SetCheckBoxes;
    Invalidate;
    FForceFullCalcOnce := False;
    Dec(FUpdateCount);
  end;
end;

procedure TAdvMetroCategoryList.CheckOffsets;
var
  Item: TCategoryItem;
  Category: TCategory;
  CatHeight: Integer;
begin
  if FCategories.Count = 0 then
    Exit;
  if FMoveToSelection and (FCategories.Count > 0) then
  begin
    FSelectedCategoryIndex := Min(FSelectedCategoryIndex, FCategories.Count - 1);
    Category := FCategories[FSelectedCategoryIndex];
    if (not Category.FCollapse) and (Category.Items <> nil) and (Category.Items.Count > 0) and (FSelectedItemIndex <> -1) then
    begin
      if FSticky then
        CatHeight := Category.FEndY - Category.FStartY
      else
        CatHeight := 0;
      Item := Category.Items[FSelectedItemIndex];
      FOffsetY := Max(Item.FEndY - Height, FOffsetY);
      FOffsetY := Min(Item.FStartY - CatHeight, FOffsetY);
    end
    else
    begin
      FOffsetY := Max(Category.FEndY - Height, FOffsetY);
      FOffsetY := Min(Category.FStartY, FOffsetY);
    end;
    Scrolled;
  end;
end;

procedure TAdvMetroCategoryList.CheckOffsets(CategoryIndex, ItemIndex: Integer);
var
  Item: TCategoryItem;
  Category: TCategory;
  CatHeight: Integer;
begin
  if FCategories.Count > 0 then
  begin
    CategoryIndex := Min(CategoryIndex, FCategories.Count - 1);
    Category := FCategories[CategoryIndex];
    if (not Category.FCollapse) and (Category.Items <> nil) and (Category.Items.Count > 0) and (ItemIndex > -1) then
    begin
      if FSticky then
        CatHeight := Category.FEndY - Category.FStartY
      else
        CatHeight := 0;
      if ItemIndex < Category.Items.Count then
        Item := Category.Items[ItemIndex]
      else
        Item := Category.Items[Category.Items.Count - 1];

      FOffsetY := Max(Item.FEndY - Height, FOffsetY);
      FOffsetY := Min(Item.FStartY - CatHeight, FOffsetY);
    end
    else
    begin
      FOffsetY := Max(Category.FEndY - Height, FOffsetY);
      FOffsetY := Min(Category.FStartY, FOffsetY);
    end;
    Scrolled;
  end;
end;

procedure TAdvMetroCategoryList.CheckSelection;
var
  Category: TCategory;
  Item: TCategoryItem;
begin
  if (FSelectedCategoryIndex < FCategories.Count) and (FSelectedCategoryIndex >= 0)  then
  begin
    Category := FCategories[FSelectedCategoryIndex];
    if Category <> nil then
    begin
      if FSelectedItemIndex = -1 then
        Category.ChangeCheck(FCategoryCheckboxRange)
      else
      begin
        if FSelectedItemIndex < Category.Items.Count then
        begin
          Item := Category.Items[FSelectedItemIndex];
          if Item <> nil then
          begin
            Item.CheckUncheck;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvMetroCategoryList.CMEnter(var Message: TCMEnter);
begin
  Invalidate;
end;

procedure TAdvMetroCategoryList.CMExit(var Message: TCMExit);
begin
  Invalidate;
end;

procedure TAdvMetroCategoryList.CMHintShow(var Msg: TMessage);
var
  Category: TCategory;
  Item: TCategoryItem;
begin
  with TCMHintShow(Msg).HintInfo^ do
  begin
    if FHoverCat <> -1 then
    begin
      Category := FCategories[FHoverCat];
      if FHoverItem <> -1 then
      begin
        Item := Category.Items[FHoverItem];
        HintStr := Item.Hint;
      end
      else
        HintStr := Category.Hint;
    end
    else
      HintStr := Hint;
  end;
end;

procedure TAdvMetroCategoryList.CollapseSelection;
var
  Category: TCategory;
  Allow: Boolean;
begin
  if (FSelectedCategoryIndex < FCategories.Count) and (FSelectedCategoryIndex >= 0)  then
  begin
    Allow := True;
    if Assigned(FOnCollapseCategory) then
      FOnCollapseCategory(Self, FSelectedCategoryIndex, Allow);
    if Allow then
    begin
      Deselect;
      Category := FCategories[FSelectedCategoryIndex];
      FSelectedItemIndex := -1;
      Category.FSelected := True;
      if Category <> nil then
      begin
        FOffsetY := Min(FOffsetY, Category.FStartY);
        Category.Collapse;
      end;
    end;
  end;
end;

constructor TAdvMetroCategoryList.Create(AOwner: TComponent);
begin
  inherited;
  FMaxY := 0;
  FOffsetY := 0;
  FMaxOffSetY := 0;
  FUpdateCount := 0;
  FShowScroll := True;
  FMouseUpLast := False;
  FMouseDownLast := False;
  FCategoryMinHeight := 50;
  FItemMinHeight := 26;
  FScrollDistance := 50;
  FCategories := TCategories.Create(Self);
  FCategories.OnChange := CategoriesChanged;
  DoubleBuffered := True;
  Height := 200;
  Width := 150;
  TabStop := True;
  FAppearance := TMetroCategoryListAppearance.Create;
  FAppearance.OnChange := AppearanceChanged;
  FCategoryFont := TFont.Create;
  FItemFont := TFont.Create;
  FSticky := True;
  FTopCategoryIndex := 0;
  FTopItemIndex := 0;
  FLastCatIndex := 0;
  FLastItemIndex := 0;
  FPageMode := ipmCategory;
  FPageSize := 1;
  FSelectedCategoryIndex := -1;
  FSelectedItemIndex := -1;
  FTopCategoryIndex := -1;
  FTopItemIndex := -1;
  FCategoryCheckboxRange := cItems;
  FAutoCalcOnResize := False;
  FForceFullCalcOnce := False;
  FImageChecked := TGDIPPicture.Create;
  FImageChecked.LoadFromResourceName(HInstance, 'TMSMCLCHECK');
  FImageUnChecked := TGDIPPicture.Create;
  FImageUnChecked.LoadFromResourceName(HInstance, 'TMSMCLUNCHECK');
  FImageGrayed := TGDIPPicture.Create;
  FImageGrayed.LoadFromResourceName(HInstance, 'TMSMCLGRAY');
  FImageCollapse := TGDIPPicture.Create;
  FImageCollapse.LoadFromResourceName(HInstance, 'TMSMCLCOLLAPSE');
  FImageExpand := TGDIPPicture.Create;
  FImageExpand.LoadFromResourceName(HInstance, 'TMSMCLEXPAND');
  ShowHint := True;
  FMaxStickySize := -1;
  if DesignTime then
  begin
    BeginUpdate;
    with FCategories.Add do
    begin
      Text := 'Countries';
      FCheckedState := cbGrayed;
      with Items.Add do
      begin
        Text := 'Germany';
        FChecked := True;
      end;
      Items.Add.Text := '<a href="htpp://www.tmssoftware.com">Belgium</a>';
      Items.Add.Text := 'United Kingdom';
      Items.Add.Text := 'United States';
    end;
    FUpdateCount := 0;
  end
end;

procedure TAdvMetroCategoryList.Deselect;
var Category: TCategory;
begin
  if (FCategories <> nil) and (FSelectedCategoryIndex < FCategories.Count) and (FSelectedCategoryIndex > -1) then
  begin
    Category := FCategories[FSelectedCategoryIndex];
    if (Category.Items <> nil) and (FSelectedItemIndex < Category.Items.Count) and (FSelectedItemIndex > -1) then
      Category.Items[FSelectedItemIndex].FSelected := False
    else
      Category.FSelected := False;
  end;
end;

procedure TAdvMetroCategoryList.DeselectAll;
var
  I, J: Integer;
  Category: TCategory;
  Item: TCategoryItem;
begin
  for I := 0 to FCategories.Count - 1 do
  begin
    Category := FCategories[I];
    Category.FSelected := False;
    if Category.Items <> nil then
    begin
      for J := 0 to Category.Items.Count - 1 do
      begin
        Item := Category.Items[J];
        Item.FSelected := False;
      end;
    end;
  end;
end;

function TAdvMetroCategoryList.DesignTime: Boolean;
begin
  Result := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
end;

destructor TAdvMetroCategoryList.Destroy;
begin
  FImageCollapse.Free;
  FImageExpand.Free;
  FImageGrayed.Free;
  FImageChecked.Free;
  FImageUnChecked.Free;
  FCategoryFont.Free;
  FItemFont.Free;
  FCategories.Free;
  FAppearance.Free;
  inherited;
end;

procedure TAdvMetroCategoryList.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    CategoriesChanged(Self);
  end;
  FUpdateCount := Max (0, FUpdateCount);
end;

procedure TAdvMetroCategoryList.CollapseExpand(Category: TCategory);
var
  Allow: Boolean;
begin
  Allow := True;
  if Category.FCollapse then
  begin
    if Assigned(FOnExpandCategory) then
      FOnExpandCategory(Self, Category.Index, Allow);
    if Allow then
      Category.Expand;
  end
  else
  begin
    if Assigned(FOnCollapseCategory) then
      FOnCollapseCategory(Self, Category.Index, Allow);
    if Allow then
      Category.Collapse;
  end;
end;

procedure TAdvMetroCategoryList.ExpandSelection;
var
  Category: TCategory;
  var Allow: Boolean;
begin
  if (FSelectedCategoryIndex < FCategories.Count) and (FSelectedCategoryIndex >= 0)  then
  begin
    Allow := True;
    if Assigned(FOnExpandCategory) then
      FOnExpandCategory(Self, FSelectedCategoryIndex, Allow);
    if Allow then
    begin
      Category := FCategories[FSelectedCategoryIndex];
      if Category <> nil then
      begin
        Category.Expand;
      end;
    end;
  end;
end;

procedure TAdvMetroCategoryList.First(GoToItem: Boolean = True);
var
  Category: TCategory;
  Allow: Boolean;
begin
  FOffsetY := 0;
  if FCategories.Count > 0 then
  begin
    Allow := True;
    if Assigned(FOnItemSelect) and (FUpdateCount = 0) then
    begin
      FOnItemSelect(Self, 0, -1, Allow);
    end;
    if Allow then
    begin
      Category := FCategories[0];
      DeselectAll;
      FMoveToSelection := True;
      Category.FSelected := True;
      FSelectedCategoryIndex := 0;
      FSelectedItemIndex := -1;
      CheckOffsets;
    end;
  end;
  Invalidate;
end;

procedure TAdvMetroCategoryList.ForceSizeCalculations(IgnoreUpdateStatus: Boolean = False);
begin
  if (FUpdateCount = 0) or IgnoreUpdateStatus then
  begin
    CalculateSizes(True);
    Scrolled;
    Invalidate;
  end
  else
    FForceFullCalcOnce := True;
end;

function TAdvMetroCategoryList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvMetroCategoryList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvMetroCategoryList.ItemsToBePainted;
var
  I, J: Integer;
  Category: TCategory;
  Item: TCategoryItem;
  FirstReached, LastReached: Boolean;
  r: TRect;
begin
  FirstReached := False;
  LastReached := False;
  FStickyEncountered := False;
  FHalfStickyEncountered := False;
  FFirstStickyEncountered := False;
  FMaxStickySize := -1;
  for I := 0 to Categories.Count - 1 do
  begin
    Category := Categories[I];
    if not LastReached then
    begin
      if not FirstReached then
      begin
        if Category.FEndY - FOffsetY > 0 then
        begin
          Category.FPaint := True;
          FirstReached := True;
          FTopCategoryIndex := I;
          FTopItemIndex := -1;
          if FSticky then
          begin
            FHalfStickyIndex := I;
            FHalfStickyEncountered := True;
            FStickyEncountered := False;
            FMaxStickySize := Category.FStartY - Category.FEndY;
          end;
        end
        else
        begin
          Category.FPaint := False;
          if FSticky then
          begin
            FLastStickyIndex := I;
            FStickyEncountered := True;
          end;
        end;
      end
      else
      begin
        if Category.FStartY - FOffsetY > Height then
        begin
          LastReached := True;
          FLastCatIndex := I - 1;
          FLastItemIndex := Categories[I - 1].Items.Count - 1;
          Category.FPaint := False;
        end
        else
          Category.FPaint := True;
          if FSticky and (not FFirstStickyEncountered) then
          begin
            FFirstStickyIndex := I;
            FFirstStickyEncountered := True;
            FMaxStickySize := Category.FStartY - FOffsetY;
          end;
      end;
    end
    else
      Category.FPaint := False;
    if Category.Items <> nil then
    begin
      for J := 0 to Category.Items.Count -1 do
      begin
        Item := Category.Items[J];
        if Category.FCollapse then
          Item.FPaint := False
        else
        begin
          if not LastReached then
          begin
            if not FirstReached then
            begin
              if Item.FEndY - FOffsetY > 0 then
              begin
                FTopCategoryIndex := I;
                FTopItemIndex := J;
                FirstReached := True;
                Item.FPaint := True;
              end
              else
                Item.FPaint := False;
            end
            else
            begin
              if Item.FStartY - FOffsetY > Height then
              begin
                LastReached := True;
                FLastCatIndex := I;
                FLastItemIndex := J - 1;
                Item.FPaint := False;
              end
              else
                Item.FPaint := True;
            end;
          end
          else
            Item.FPaint := False;
        end;
      end;
    end;
  end;
  if FSticky and FirstReached then
  begin
    if FHalfStickyEncountered then
      FCategories[FHalfStickyIndex].FPaint := True;
    if FFirstStickyEncountered then
      FCategories[FFirstStickyIndex].FPaint := True;
  end;
  if FirstReached and not LastReached then
  begin
    FLastCatIndex := FCategories.Count - 1;
    if FCategories[FLastCatIndex].Items <> nil then
      FLastItemIndex := FCategories[FLastCatIndex].Items.Count - 1
    else
      FLastItemIndex := -1;
  end;

  if FirstReached and (FStickyEncountered) then
  begin
    FirstReached := False;
    Category := FCategories[FLastStickyIndex];
    Category.FPaint := True;
    r := Category.GetPaintRect(FOffsetY, FCategoryMinHeight, ClientRect, FSticky, FMaxStickySize);
    if r.Bottom >= FMaxStickySize then
    begin
      Category := FCategories[FFirstStickyIndex];
      r := Category.GetPaintRect(FOffsetY, FCategoryMinHeight, ClientRect, FSticky, FMaxStickySize);
    end;
    for I := FTopCategoryIndex to FLastCatIndex do
    begin
      Category := FCategories[I];
      if (not FirstReached) and (Category.FEndY - FOffsetY > r.Bottom) then
      begin
        FirstReached := True;
        FTopCategoryIndex := I;
        FTopItemIndex := -1;
      end
      else
      begin
        if Category.Items <> nil then
        begin
          for J := 0 to Category.Items.Count - 1 do
          begin
            Item := Category.Items[J];
            if (not FirstReached) and (Item.FEndY - FOffsetY > r.Bottom) then
            begin
              FirstReached := True;
              FTopCategoryIndex := I;
              FTopItemIndex := J;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvMetroCategoryList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    vk_Up: Previous;
    vk_Down: Next;
    vk_Prior: PageUp;
    vk_Next: PageDown;
    vk_Home: First;
    vk_End: Last;
    VK_SPACE: CheckSelection;
    VK_LEFT: CollapseSelection;
    VK_RIGHT: ExpandSelection;
  end;
  Invalidate;
end;

procedure TAdvMetroCategoryList.Last(GoToItem: Boolean = True);
var
  I, J, OldCat, OldItem: Integer;
  Category: TCategory;
  Item: TCategoryItem;
  Allow: Boolean;
begin
  FOffsetY := FMaxOffSetY;
  Category := nil;
  Item := nil;
  OldCat := FSelectedCategoryIndex;
  OldItem := FSelectedItemIndex;
  Allow := True;
  for I := 0 to FCategories.Count - 1 do
  begin
    Category := FCategories[I];
    Category.FSelected := False;
    if Category.Items <> nil then
    begin
      for J := 0 to Category.Items.Count - 1 do
      begin
        Item := Category.Items[J];
        Item.FSelected := False;
      end;
    end;
  end;
  if (Category <> nil) then
  begin
    if GoToItem and (Category.Items.Count > 0) and (not Category.FCollapse) then
    begin
      if Assigned(FOnItemSelect) and (FUpdateCount = 0) then
      begin
        FOnItemSelect(Self, Category.Index, Item.Index, Allow);
      end;
      if Allow then
      begin
        Item.FSelected := True;
        FMoveToSelection := True;
        FSelectedCategoryIndex := Category.Index;
        FSelectedItemIndex := Item.Index;
      end;
    end
    else
    begin
      if Assigned(FOnItemSelect) then
      begin
        FOnItemSelect(Self, Category.Index, -1, Allow);
      end;
      if Allow then
      begin
        Category.FSelected := True;
        FSelectedCategoryIndex := Category.Index;
        FSelectedItemIndex := -1;
        FMoveToSelection := True;
      end;
    end;
  end
  else
  begin
    FMoveToSelection := False;
    FSelectedCategoryIndex := -1;
    FSelectedItemIndex := -1;
  end;
  if not Allow then
  begin
    FSelectedCategoryIndex := OldCat;
    FSelectedItemIndex := OldItem;
  end;
  CheckOffsets;
  Invalidate;
end;

procedure TAdvMetroCategoryList.Loaded;
begin
  inherited;
  CalculateSizes(True);
  ItemsToBePainted;
  CalculateScrollRect;
  Scrolled;
  Invalidate;
end;

procedure TAdvMetroCategoryList.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not Focused then
  begin
    SetFocus;
  end;
  if not FMouseDownLast then
  begin
    FMouseUpLast := False;
    FMouseDownLast := True;
    FMouseDownX := X;
    FMouseDownY := Y;
    FMouseDownInScrollRect := PtInRect(FScrollRect, Point(FMouseDownX, FMouseDownY)); {XYInRect(FMouseDownX,FMouseDownY, FScrollRect)};
    if FMouseDownInScrollRect then
    begin
      FMouseDownOffsetY := FOffsetY;
      FMouseDownScrollbarOffset := Y - FScrollRect.Top;
    end;
    Invalidate;
  end;
end;

procedure TAdvMetroCategoryList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  h, I, J, minJ, maxJ: Integer;
  Item: TCategoryItem;
  Category: TCategory;
  r: TRect;
  ReachedItem: Boolean;
  text: string;
begin
  inherited;
  if ssLeft in Shift then
  begin
    if FMouseDownLast and FMouseDownInScrollRect then
    begin
      h := Trunc((Height / FMaxY) * Height);
      FOffsetY := Trunc(((Y - FMouseDownScrollbarOffset) / (Height - 10 + min(10, h))) * FMaxY);
      Scrolled;
    end;
  end
  else if not FMouseDownInScrollRect then
  begin
    ReachedItem := False;
    Category := nil;
    r := Bounds(0,0,0,0);
    if FSticky and (Categories.Count > 0) then
    begin
      if FFirstStickyEncountered then
        Category := FCategories[FFirstStickyIndex];
      if Category <> nil then
      begin
        r := Category.GetPaintRect(FOffsetY, FCategoryMinHeight, ClientRect, FSticky, FMaxStickySize);
        if PtInRect(r, Point(X, Y)) then
        begin
          ReachedItem := True;
          text := AnchorTextAt(X, Y, Category, Category.GetTextRect(r));
          if text <> '' then
            Cursor := crHandPoint
          else
            Cursor := crDefault;
          if (FHoverCat <> Category.Index) or (FHoverItem <> -1) then
          begin
            Application.CancelHint;
            FHoverCat := Category.Index;
            FHoverItem := -1;
          end;
        end
        else
        begin
          if FHalfStickyEncountered then
            Category := FCategories[FHalfStickyIndex]
          else if FStickyEncountered then
            Category := FCategories[FLastStickyIndex];

          if Category <> nil then
          begin
            r := Category.GetPaintRect(FOffsetY, FCategoryMinHeight, ClientRect, FSticky, FMaxStickySize);
            if PtInRect(r, Point(X, Y)) then
            begin
              ReachedItem := True;
              text := AnchorTextAt(X, Y, Category, Category.GetTextRect(r));
              if text <> '' then
                Cursor := crHandPoint
              else
                Cursor := crDefault;
              if (FHoverCat <> Category.Index) or (FHoverItem <> -1) then
              begin
                Application.CancelHint;
                FHoverCat := Category.Index;
                FHoverItem := -1;
              end;
            end;
          end;
        end;
      end;
    end;

    if (not ReachedItem) and (Categories.Count > 0) then
    begin
      for I := FTopCategoryIndex to FLastCatIndex do
      begin
        Category := FCategories[I];
        r := Category.GetPaintRect(FOffsetY, FCategoryMinHeight, ClientRect, FSticky, FMaxStickySize);
        if PtInRect(r, Point(X, Y)) then
        begin
          ReachedItem := True;
          text := AnchorTextAt(X, Y, Category, Category.GetTextRect(r));
          if text <> '' then
            Cursor := crHandPoint
          else
            Cursor := crDefault;
          if (FHoverCat <> Category.Index) or (FHoverItem <> -1) then
          begin
            Application.CancelHint;
            FHoverCat := Category.Index;
            FHoverItem := -1;
          end;
        end
        else if (Category.Items <> nil) and (not Category.FCollapse) and (Category.Items.Count > 0) then
        begin
          if I = FTopCategoryIndex then
            minJ := Max(0, FTopItemIndex)
          else
            minJ := 0;
          if I = FLastCatIndex then
            maxJ := Min(FLastItemIndex, Category.Items.Count - 1)
          else
            maxJ := Category.Items.Count - 1;

          if maxJ >= minJ then
          begin
          for J := minJ to maxJ do
            begin
              Item := Category.Items[J];
              r := Item.GetPaintRect(FOffsetY, ClientRect);
              if PtInRect(r, Point(X, Y)) then
              begin
                ReachedItem := True;
                text := AnchorTextAt(X, Y, Item, Item.GetTextRect(r));
                if text <> '' then
                  Cursor := crHandPoint
                else
                  Cursor := crDefault;
                if (FHoverCat <> I) or (FHoverItem <> J) then
                begin
                  Application.CancelHint;
                  FHoverCat := I;
                  FHoverItem := J;
                end;
              end;
            end;
          end;
        end;
      end;
    end;

    if not ReachedItem then
    begin
      Application.CancelHint;
      FHoverCat := -1;
      FHoverItem := -1;
    end;
  end;
end;

procedure TAdvMetroCategoryList.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  r, fr: TRect;
  I, J, minJ, maxJ: Integer;
  Category: TCategory;
  Item: TCategoryItem;
  ReachedItem, Allow: Boolean;
  text: string;
begin
  inherited;
  FMouseDownLast := False;
  FMouseUpLast := True;
  FMouseUpX := X;
  FMouseUpY := Y;
  ReachedItem := False;
  Category := nil;
  Allow := True;
  if FSticky then
  begin
    if Categories.Count = 0 then
      Exit;
    if FFirstStickyEncountered then
      Category := FCategories[FFirstStickyIndex];
    if Category <> nil then
      fr := Category.GetPaintRect(FOffsetY, FCategoryMinHeight, ClientRect, FSticky, FMaxStickySize)
    else
      fr := Bounds(0, 0, 0, 0);
    if FHalfStickyEncountered then
      Category := FCategories[FHalfStickyIndex]
    else if FStickyEncountered then
      Category := FCategories[FLastStickyIndex];

    if Category <> nil then
    begin
      r := Category.GetPaintRect(FOffsetY, FCategoryMinHeight, ClientRect, FSticky, FMaxStickySize);
      if (FCategoryCheckboxRange <> cNone) and (not PtInRect(fr, Point(X,Y))) and PtInRect(r, Point(X, Y)) and PtInRect(r, Point(FMouseDownX, FMouseDownY))
          and not FMouseDownInScrollRect then
      begin
        ReachedItem := True;
        if Category.FShowCheckBox then
          case Category.FCheckedState of
            cbUnchecked: r := Category.GetCheckBoxRect(r, FImageUnChecked);
            cbChecked: r := Category.GetCheckBoxRect(r, FImageChecked);
            cbGrayed: r := Category.GetCheckBoxRect(r, FImageGrayed);
          end;
        if Category.FShowCheckBox and (FCategoryCheckboxRange <> cNone) and PtInRect(r, Point(X, Y)) and PtInRect(r, Point(FMouseDownX, FMouseDownY))
            and not FMouseDownInScrollRect then
          Category.ChangeCheck(FCategoryCheckboxRange)
        else
        begin
          r := Category.GetPaintRect(FOffsetY, FCategoryMinHeight, ClientRect, FSticky, FMaxStickySize);
          text := AnchorTextAt(X, Y, Category, Category.GetTextRect(r));
          if text <> '' then
          begin
            if Assigned(FOnAnchorClick) then
              FOnAnchorClick(Category, text);
          end
          else if PtInRect(Category.GetCollapseRect(r, FImageCollapse), Point(X, Y)) then
            CollapseExpand(Category)
          else
          begin
            if Assigned(FOnItemSelect) then
            begin
              FOnItemSelect(Self, Category.Index, -1, Allow);
            end;
            if Allow then
            begin
              Deselect;
              Category.FSelected := True;
              FSelectedCategoryIndex := Category.Index;
              FSelectedItemIndex := -1;
              FMoveToSelection := True;
            end;
          end;
        end;
      end
      else if PtInRect(fr, Point(X, Y)) and (not FMouseDownInScrollRect) then
      begin
        Category := FCategories[FFirstStickyIndex];
        if Category <> nil then
        begin
          ReachedItem := True;
          if Category.FShowCheckBox then
          begin
             case Category.FCheckedState of
              cbUnchecked: r := Category.GetCheckBoxRect(fr, FImageUnChecked);
              cbChecked: r := Category.GetCheckBoxRect(fr, FImageChecked);
              cbGrayed: r := Category.GetCheckBoxRect(fr, FImageGrayed);
             end;
          end;
          if (FCategoryCheckboxRange <> cNone) and Category.FShowCheckBox and PtInRect(r, Point(X, Y)) and PtInRect(r, Point(FMouseDownX, FMouseDownY))
              and not FMouseDownInScrollRect then
            Category.ChangeCheck(FCategoryCheckboxRange)
          else
          begin
            text := AnchorTextAt(X, Y, Category, Category.GetTextRect(fr));
            if text <> '' then
            begin
              if Assigned(FOnAnchorClick) then
                FOnAnchorClick(Category, text);
            end
            else if PtInRect(Category.GetCollapseRect(fr, FImageCollapse), Point(X, Y)) then
              CollapseExpand(Category)
            else
            begin
              if Assigned(FOnItemSelect) then
              begin
                FOnItemSelect(Self, Category.Index, -1, Allow);
              end;
              if Allow then
              begin
                Deselect;
                Category.FSelected := True;
                FSelectedCategoryIndex := Category.Index;
                FSelectedItemIndex := -1;
                FMoveToSelection := True;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  if not ReachedItem then
  begin
    if FTopCategoryIndex = FLastCatIndex then
    begin

    end;
    for I := FTopCategoryIndex to FLastCatIndex do
    begin
      Category := FCategories[I];
      if Category <> nil then
      begin
        r := Category.GetPaintRect(FOffsetY, FCategoryMinHeight, ClientRect, FSticky, FMaxStickySize);
        if PtInRect(r, Point(X, Y)) and PtInRect(r, Point(FMouseDownX, FMouseDownY))
            and not FMouseDownInScrollRect then
        begin
          ReachedItem := True;
          if Category.FShowCheckBox then
          begin
            case Category.FCheckedState of
              cbUnchecked: r := Category.GetCheckBoxRect(r, FImageUnChecked);
              cbChecked: r := Category.GetCheckBoxRect(r, FImageChecked);
              cbGrayed: r := Category.GetCheckBoxRect(r, FImageGrayed);
            end;
          end;
          if (FCategoryCheckboxRange <> cNone) and Category.FShowCheckBox and PtInRect(r, Point(X, Y)) and PtInRect(r, Point(FMouseDownX, FMouseDownY))
              and not FMouseDownInScrollRect then
            Category.ChangeCheck(FCategoryCheckboxRange)
          else if not FMouseDownInScrollRect then
          begin
            r := Category.GetPaintRect(FOffsetY, FCategoryMinHeight, ClientRect, FSticky, FMaxStickySize);
            text := AnchorTextAt(X, Y, Category, Category.GetTextRect(r));
            if text <> '' then
            begin
              if Assigned(FOnAnchorClick) then
                FOnAnchorClick(Category, text);
            end
            else if PtInRect(Category.GetCollapseRect(r, FImageCollapse), Point(X, Y)) then
              CollapseExpand(Category)
            else
            begin
              if Assigned(FOnItemSelect) then
              begin
                FOnItemSelect(Self, Category.Index, -1, Allow);
              end;
              if Allow then
              begin
                Deselect;
                Category.FSelected := True;
                FSelectedCategoryIndex := Category.Index;
                FSelectedItemIndex := -1;
                FMoveToSelection := True;
              end;
            end;
          end;
        end;
      end;

      if not ReachedItem then
      begin
        if I = FTopCategoryIndex then
          minJ := Max(0, FTopItemIndex)
        else
          minJ := 0;
        if I = FLastCatIndex then
          maxJ := FLastItemIndex
        else
          maxJ := Category.Items.Count - 1;

        if (maxJ >= minJ) and (not Category.FCollapse) then
        begin
          for J := minJ to maxJ do
          begin
            Item := Category.Items[J];
            r := Item.GetPaintRect(FOffsetY, ClientRect);
            if PtInRect(r, Point(X, Y)) and PtInRect(r, Point(FMouseDownX, FMouseDownY)) and not FMouseDownInScrollRect then
            begin
              if Item.FShowCheckBox then
              begin
                if Item.FChecked then
                  r := Item.GetCheckBoxRect(r, FImageChecked)
                else
                  r := Item.GetCheckBoxRect(r, FImageUnChecked);
              end;
              if Item.FShowCheckBox and PtInRect(r, Point(X, Y)) and PtInRect(r, Point(FMouseDownX, FMouseDownY)) and not FMouseDownInScrollRect then
              begin
                Item.Checked := not Item.FChecked;
              end
              else
              begin
                r := Item.GetPaintRect(FOffsetY, ClientRect);
                text := AnchorTextAt(X, Y, Item, Item.GetTextRect(r));
                if text <> '' then
                begin
                  if Assigned(FOnAnchorClick) then
                    FOnAnchorClick(Item, AnchorTextAt(X, Y, Item, Item.GetTextRect(r)))
                end
                else
                begin
                  if Assigned(FOnItemSelect) then
                  begin
                    FOnItemSelect(Self, I, J, Allow);
                  end;
                  if Allow then
                  begin
                    Deselect;
                    Item.FSelected := True;
                    FSelectedCategoryIndex := I;
                    FSelectedItemIndex := J;
                    FMoveToSelection := True;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  CheckOffsets;
  FMoveToSelection := False;
  Invalidate;
end;

procedure TAdvMetroCategoryList.MouseWheelHandler(var Message: TMessage);
begin
  inherited;
  if (FOffsetY > 0) and (FOffsetY < FMaxOffsetY) then
  begin
    if Assigned(FOnScroll) then
      FOnScroll(Self);
  end;
  case Message.Msg of
    WM_MOUSEWHEEL:
    begin
      if integer(Message.WParam) < 0 then
        FOffsetY := FOffsetY + FScrollDistance
      else
        FOffsetY := FOffsetY - FScrollDistance;
      Scrolled;
    end;
  end;
end;

procedure TAdvMetroCategoryList.MoveToSelection;
begin
  FMoveToSelection := True;
  CheckOffsets;
  Invalidate;
end;

procedure TAdvMetroCategoryList.Next;
var
  Item: TCategoryItem;
  Category: TCategory;
  FoundNext, Allow: Boolean;
  oldCat, oldItem: Integer;
begin
  FoundNext := False;
  oldCat := FSelectedCategoryIndex;
  oldItem := FSelectedItemIndex;
  Deselect;
  while not FoundNext do
  begin
    Allow := True;
    if FSelectedCategoryIndex < FCategories.Count then
    begin
      if FSelectedCategoryIndex >= 0 then
      begin
        Category := FCategories[FSelectedCategoryIndex];
        Inc(FSelectedItemIndex);
        if (Category.Items <> nil) and (FSelectedItemIndex < Category.Items.Count) and (not Category.FCollapse) then
        begin
          FoundNext := True;
          if Assigned(FOnItemSelect) and (FUpdateCount = 0) then
            FOnItemSelect(Self, FSelectedCategoryIndex, FSelectedItemIndex, Allow);
          if Allow then
          begin
            Item := Category.Items[FSelectedItemIndex];
            Item.FSelected := True;
          end;
        end
        else
        begin
          Inc(FSelectedCategoryIndex);
          FSelectedItemIndex := -1;
          if FSelectedCategoryIndex < FCategories.Count then
          begin
            FoundNext := True;
            if Assigned(FOnItemSelect) and (FUpdateCount = 0) then
              FOnItemSelect(Self, FSelectedCategoryIndex, FSelectedItemIndex, Allow);
            if Allow then
            begin
              Category := FCategories[FSelectedCategoryIndex];
              Category.FSelected := True;
            end;
          end;
        end;
      end
      else
      begin
        First;
        FoundNext := True;
      end;
    end
    else
    begin
      FoundNext := True;
      Last;
    end;
  end;
  if Allow then
  begin
    FMoveToSelection := True;
    CheckOffsets;
    Invalidate;
  end
  else
  begin
    FSelectedCategoryIndex := oldCat;
    FSelectedItemIndex := oldItem;
    Select;
  end;
end;

procedure TAdvMetroCategoryList.PageDown;
var
  Category: TCategory;
  I: Integer;
  oldCat, oldItem: Integer;
  Allow: Boolean;
begin
  oldCat := fSelectedCategoryIndex;
  oldItem := FSelectedItemIndex;
  Allow := True;
  Inc(FUpdateCount);
  for I := 0 to FPageSize - 1 do
  begin
    if FPageMode = ipmCategory then
    begin
      Deselect;
      Inc(FSelectedCategoryIndex);
      if FSelectedCategoryIndex < FCategories.Count then
      begin
        FSelectedItemIndex := 0;
        Category := FCategories[FSelectedCategoryIndex];
        FSelectedItemIndex := -1;
        Category.FSelected := True;
      end
      else
        Last;
    end
    else
      Next;
  end;
  Dec(FUpdateCount);
  if Assigned(FOnItemSelect) then
    FOnItemSelect(Self, FSelectedCategoryIndex, FSelectedItemIndex, Allow);
  if Allow then
  begin
    FMoveToSelection := True;
    CheckOffsets;
  end
  else
  begin
    FSelectedCategoryIndex := oldCat;
    FSelectedItemIndex := oldItem;
  end;
end;

procedure TAdvMetroCategoryList.PageUp;
var
  Category: TCategory;
  I: Integer;
  oldCat, oldItem: Integer;
  Allow: Boolean;
begin
  Allow := True;
  oldCat := FSelectedCategoryIndex;
  oldItem := FSelectedItemIndex;
  Inc(FUpdateCount);
  for I := 0 to FPageSize - 1 do
  begin
    if FPageMode = ipmCategory then
    begin
      if FSelectedCategoryIndex <= 0 then
        First(False)
      else
      begin
        if FSelectedCategoryIndex < FCategories.Count then
        begin
          Deselect;
          Dec(FSelectedCategoryIndex);
          FSelectedItemIndex := -1;
          Category := FCategories[FSelectedCategoryIndex];
          Category.FSelected := True;
        end;
      end;
    end
    else
      Previous;
  end;
  Dec(FUpdateCount);
  if Assigned(FOnItemSelect) then
    FOnItemSelect(Self, FSelectedCategoryIndex, FSelectedItemIndex, Allow);
  if Allow then
  begin
    FMoveToSelection := True;
    CheckOffsets;
  end
  else
  begin
    FSelectedCategoryIndex := oldCat;
    FSelectedItemIndex := oldItem;
  end;
end;

procedure TAdvMetroCategoryList.Paint;
var
  I, J, cCount, iCount, MaxBottom, MaxRight: Integer;
  Category: TCategory;
  Item: TCategoryItem;
begin
  cCount := FCategories.Count;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Appearance.BackgroundColor;
  Canvas.FillRect(ClientRect);
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := Appearance.CategoryColor;
  if cCount <> 0 then
  begin
    for I := 0 to cCount - 1 do
    begin
      Category := FCategories[I];
      if Category.FPaint then
      begin
        PaintCategory(Category);
      end;
      iCount := Category.Items.Count;
      for J := 0 to iCount - 1 do
      begin
        Item := Category.Items[J];
        PaintItem(Item);
      end;
    end;

    if FStickyEncountered then
    begin
      Category := FCategories[FLastStickyIndex];
      PaintCategory(Category);
    end;
    if FHalfStickyEncountered then
    begin
      Category := FCategories[FHalfStickyIndex];
      PaintCategory(Category);
    end;
    if FFirstStickyEncountered then
    begin
      Category := FCategories[FFirstStickyIndex];
      PaintCategory(Category);
    end;

    if FShowScroll then
    begin
      Canvas.Brush.Color := Appearance.FScrollBarColor;
      Canvas.FillRect(FScrollRect);
    end;
  MaxBottom := ClientRect.Bottom - Canvas.Pen.Width;
  MaxRight := ClientRect.Right - Canvas.Pen.Width;
  Canvas.PenPos := Point(ClientRect.Left, ClientRect.Top);
  Canvas.LineTo(ClientRect.Left, MaxBottom);
  Canvas.LineTo(MaxRight, MaxBottom);
  Canvas.LineTo(MaxRight, ClientRect.Top);
  Canvas.LineTo(ClientRect.Left, ClientRect.Top);
  end;
end;

procedure TAdvMetroCategoryList.PaintCategory(Category: TCategory);
var
  r, tr, hr, cbr, cer, imgr: TRect;
  a, s, fs: string;
  xs, ys, links, mlinks, CatHeight: Integer;
  Pic: TGDIPPicture;
  Allow: Boolean;
begin
  r := Category.GetPaintRect(FOffsetY, FCategoryMinHeight, ClientRect, FSticky, FMaxStickySize);
  Allow := True;
  if Assigned(Category.FBeforeDraw) then
    Category.FBeforeDraw(Category, Canvas, r, Category, Allow);
  if not Allow then
    Exit;
  if Assigned(BeforeCategoryDraw) then
    BeforeCategoryDraw(Self, Canvas, r, Category, Allow);
  if not Allow then
    Exit;
  CatHeight := Category.FEndY - Category.FStartY;
  if Category.FSelected then
    Canvas.Brush.Color := Appearance.SelectedItemColor
  else
    Canvas.Brush.Color := Appearance.CategoryColor;
  text := Category.Text;
  tr := Category.GetTextRect(r);
  Canvas.FillRect(r);
  if (FSelectedCategoryIndex = Category.Index) and (FSelectedItemIndex = -1) and Focused then
  begin
    InflateRect(r, -2, -2);
    DrawFocusRect(Canvas.Handle, r);
  end;
  cbr := Bounds(0, 0, 0, 0);
  if Category.FShowCheckBox and (FCategoryCheckboxRange <> cNone) then
  begin
    Pic := nil;
    case Category.FCheckedState of
      cbUnchecked: Pic := FImageUnChecked;
      cbChecked: Pic := FImageChecked;
      cbGrayed: Pic := FImageGrayed;
    end;
    if (Pic <> nil) then
    begin
      cbr := Category.GetCheckBoxRect(r, Pic);
      Canvas.StretchDraw(cbr, Pic);
    end;
  end;

  if Assigned(Category.FImageLeft) then
  begin
    imgr.Left := cbr.Right + 5;
    imgr.Right := imgr.Left + Category.FImageLeft.Width;
    imgr.Top := r.Top + (CatHeight - Category.FImageLeft.Height) div 2;
    imgr.Bottom := imgr.Top + Category.FImageLeft.Height;
    Canvas.StretchDraw(imgr, Category.FImageLeft);
  end;

  cer := Category.GetCollapseRect(r, FImageCollapse);
  if Category.FCollapse then
    Pic := FImageExpand
  else
    Pic := FImageCollapse;

  Canvas.StretchDraw(cer, Pic);

  if Assigned(Category.FImageRight) then
  begin
    imgr.Right := cer.Left - 5;
    imgr.Left := imgr.Right - Category.FImageRight.Width;
    imgr.Top := r.Top + (CatHeight - Category.FImageRight.Height) div 2;
    imgr.Bottom := imgr.Top + Category.FImageRight.Height;
    Canvas.StretchDraw(imgr, Category.FImageRight);
  end;

  Canvas.Font := FCategoryFont;
  Canvas.Font.Color := Appearance.CategoryTextColor;
  if Category.FTextHeight < FCategoryMinHeight - 10 then
    tr.Top := r.Top + (CatHeight - Category.FTextHeight) div 2;
  HTMLDrawEx(Canvas, text, tr, FImages, -1, -1, -1, -1, -1, False, False, False, False, False, False, True, 1.0,
        clBlue, clNone, clNone, clGray, a, s, fs, xs, ys, links, mlinks, hr, nil, FPictureContainer, 0);
  if Assigned(Category.FAfterDraw) then
    Category.FAfterDraw(Category, Canvas, r, Category);
  if Assigned(AfterCategoryDraw) then
    AfterCategoryDraw(Self, Canvas, r, Category);
end;

procedure TAdvMetroCategoryList.PaintItem(Item: TCategoryItem);
var
  r, tr, hr, cbr: TRect;
  a, s, fs, text: string;
  xs, ys, links, mlinks: Integer;
  Allow: Boolean;
begin
  if Item.FPaint then
  begin
    r := Item.GetPaintRect(FOffsetY, ClientRect);
    Allow := True;
    if Assigned(Item.FBeforeDraw) then
      Item.FBeforeDraw(Item, Canvas, r, Item, Allow);
    if not Allow then
      Exit;
    if Assigned(FBeforeItemDraw) then
      FBeforeItemDraw(Self, Canvas, r, Item, Allow);
    if not Allow then
      Exit;
    tr := Item.GetTextRect(r);
    text := Item.Text;

    if Item.FSelected then
    begin
      Canvas.Brush.Color := Appearance.SelectedItemColor;
      Canvas.Font.Color := Appearance.SelectedItemTextColor;
    end
    else
    begin
      Canvas.Font.Color := Appearance.BackgroundTextColor;
      Canvas.Brush.Color := Appearance.BackgroundColor;
    end;

    Canvas.FillRect(r);
    if (FSelectedCategoryIndex = Item.Category.Index) and (FSelectedItemIndex = Item.Index) and Focused then
    begin
      Canvas.Font.Color := clBlack;
      InflateRect(r, -1, -1);
      DrawFocusRect(Canvas.Handle, r);
    end;
    Canvas.Font := FItemFont;
    if Item.FShowCheckBox then
    begin
      if Item.FChecked then
        cbr := Item.GetCheckBoxRect(r, FImageChecked)
      else
        cbr := Item.GetCheckBoxRect(r, FImageUnChecked);
      if Item.FChecked then
        Canvas.StretchDraw(cbr, FImageChecked)
      else
        Canvas.StretchDraw(cbr, FImageUnChecked);
    end;
    HTMLDrawEx(Canvas, text, tr, FImages, -1, -1, -1, -1, -1, False, False, False, False, False, False, True, 1.0,
        clBlue, clNone, clNone, clGray, a, s, fs, xs, ys, links, mlinks, hr, nil, FPictureContainer, 0);
    if Assigned(Item.FAfterDraw) then
      Item.FAfterDraw(Item, Canvas, Item.GetPaintRect(FOffsetY, ClientRect), Item);
    if Assigned(FAfterItemDraw) then
      AfterItemDraw(Self, Canvas, Item.GetPaintRect(FOffsetY, ClientRect), Item);
  end;
end;

procedure TAdvMetroCategoryList.Previous;
var
  Item: TCategoryItem;
  Category: TCategory;
  FirstBack, Allow: Boolean;
  oldCat, oldItem: Integer;
begin
  OldCat := FSelectedCategoryIndex;
  OldItem := FSelectedItemIndex;
  Deselect;
  Allow := True;

  if (FSelectedCategoryIndex >= 0) and (FCategories.Count > 0) then
    Category := FCategories[FSelectedCategoryIndex]
  else
    Category := nil;


  if (Category <> nil) and (FSelectedItemIndex >= 0) then
  begin
    Dec(FSelectedItemIndex);
    if (FSelectedItemIndex >= 0) and (not Category.FCollapse) then
    begin
      if Assigned(FOnItemSelect) and (FUpdateCount = 0) then
        FOnItemSelect(Self, FSelectedCategoryIndex, FSelectedItemIndex, Allow);
      if Allow then
      begin
        Item := Category.Items[FSelectedItemIndex];
        Item.FSelected := True;
        FMoveToSelection := True;
        CheckOffsets;
      end;
    end
    else
    begin
      if Assigned(FOnItemSelect) and (FUpdateCount = 0) then
        FOnItemSelect(Self, FSelectedCategoryIndex, FSelectedItemIndex, Allow);
      if Allow then
      begin
        Category.FSelected := True;
        FSelectedItemIndex := -1;
        FMoveToSelection := True;
        CheckOffsets;
      end;
    end;
  end
  else
  begin
    FirstBack := False;
    while not Firstback do
    begin
      if FSelectedCategoryIndex > 0 then
      begin
        Dec(FSelectedCategoryIndex);
        Category := FCategories[FSelectedCategoryIndex];
        if (not Category.FCollapse) and (Category.Items.Count > 0) then
        begin
          FirstBack := True;
          if Assigned(FOnItemSelect) and (FUpdateCount = 0) then
            FOnItemSelect(Self, FSelectedCategoryIndex, FSelectedItemIndex, Allow);
          if Allow then
          begin
            Item := Category.Items[Category.Items.Count - 1];
            FMoveToSelection := True;
            Item.FSelected := True;
            FSelectedItemIndex := Item.Index;
            CheckOffsets;
          end;
        end
        else
        begin
          FirstBack := True;
          if Assigned(FOnItemSelect) and (FUpdateCount = 0) then
            FOnItemSelect(Self, FSelectedCategoryIndex, FSelectedItemIndex, Allow);
          if Allow then
          begin
            FMoveToSelection := True;
            Category.FSelected := True;
            FSelectedItemIndex := -1;
            CheckOffsets;
          end;
        end;
      end
      else
      begin
        FirstBack := True;
        First;
      end;
    end;
  end;
  if not Allow then
  begin
    FSelectedCategoryIndex := OldCat;
    FSelectedItemIndex := OldItem;
    Select;
  end;
  Invalidate;
end;

procedure TAdvMetroCategoryList.Resize;
begin
  inherited;
  if FUpdateCount = 0 then
  begin
    Inc(FUpdateCount);
    CalculateSizes(FAutoCalcOnResize);
    ItemsToBePainted;
    Scrolled;
    Dec(FUpdateCount);
  end;
end;

procedure TAdvMetroCategoryList.Scrolled;
begin
  FOffsetY := Min(FMaxOffSetY, FOffsetY);
  FOffsetY := Max (0, FOffsetY);
  ItemsToBePainted;
  CalculateScrollRect;
  Invalidate;
end;

procedure TAdvMetroCategoryList.Select;
var Category: TCategory;
begin
  if (FCategories <> nil) and (FSelectedCategoryIndex < FCategories.Count) and (FSelectedCategoryIndex > -1) then
  begin
    Category := FCategories[FSelectedCategoryIndex];
    if (Category.Items <> nil) and (FSelectedItemIndex < Category.Items.Count) and (FSelectedItemIndex > -1) then
      Category.Items[FSelectedItemIndex].FSelected := True
    else
      Category.FSelected := True;
  end;
end;

procedure TAdvMetroCategoryList.SetAppearance(
  const Value: TMetroCategoryListAppearance);
begin
  FAppearance.Assign(Value);
end;

procedure TAdvMetroCategoryList.SetAutoCalcOnResize(const Value: Boolean);
begin
  FAutoCalcOnResize := Value;
  if Value then
    ForceSizeCalculations(False);
end;

procedure TAdvMetroCategoryList.SetCategories(const Value: TCategories);
begin
  if Value <> FCategories then
  begin
    FCategories.Assign(Value);
  end;
end;

procedure TAdvMetroCategoryList.SetCategoryCheckboxRange(
  const Value: TCheckBoxCategory);
var
  I: Integer;
  Category: TCategory;
begin
  if FCategoryCheckboxRange <> Value then
  begin
    BeginUpdate;
    FCategoryCheckboxRange := Value;
    if Value = cCategory then
    begin
      for I := 0 to Categories.Count - 1 do
      begin
        Category := Categories[I];
        if Category.FCheckedState = cbGrayed then
          Category.FCheckedState := cbChecked;
      end;
    end
    else if Value = cItems then
    begin
      for I := 0 to Categories.Count -1 do
      begin
        Category := Categories[I];
        Category.SetCheckState(Value);
      end;  
    end;
    EndUpdate;
  end;
end;

procedure TAdvMetroCategoryList.SetCategoryFont(const Value: TFont);
begin
  FCategoryFont.Assign(Value);
  Invalidate;
end;

procedure TAdvMetroCategoryList.SetCategoryMinHeight(const Value: Integer);
begin
  if FCategoryMinHeight <> Value then
  begin
    FCategoryMinHeight := Value;
    BeginUpdate;
    try
      if FCategories.Count > 0 then
        FCategories[0].FDirty := True;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TAdvMetroCategoryList.SetCheckBoxes;
var
  I, J: Integer;
  Category: TCategory;
  Item: TCategoryItem;
begin
  if FCategories.Count > 0 then
  begin
    for I := 0 to FCategories.Count - 1 do
    begin
      Category := FCategories[I];
      if (FCategoryCheckboxRange <> cNone) and (Category.FCheckDirty) then
      begin
        Category.ApplyCheckState(FCategoryCheckboxRange);
        Category.FCheckDirty := False;
      end;
      if (FCategoryCheckboxRange = cItems) and (Category.Items <> nil) and (Category.Items.Count > 0) then
      for J := 0 to Category.Items.Count - 1 do
      begin
        Item := Category.Items[J];
        if Item.FShowCheckBox and Item.FCheckDirty then
        begin
          Category.SetCheckState(FCategoryCheckboxRange);
          Item.FCheckDirty := False;
        end;
      end;
    end;
  end;
end;

procedure TAdvMetroCategoryList.SetColorTones(ATones: TColorTones);
begin
  FAppearance.FBackgroundColor := ATones.Background.BrushColor;
  FAppearance.FBackgroundTextColor := ATones.Background.TextColor;
  FAppearance.FCategoryColor := ATones.Selected.BrushColor;
  FAppearance.FCategoryTextColor := ATones.Selected.TextColor;
  FAppearance.FDisabledColor := ATones.Disabled.BrushColor;
  FAppearance.FDisabledTextColor := ATones.Disabled.TextColor;
  FAppearance.FSelectedItemColor := ATones.Selected.TextColor;
  FAppearance.FSelectedItemTextColor := ATones.Selected.TextColor;
  Invalidate;
end;

procedure TAdvMetroCategoryList.SetImageGrayed(const Value: TGDIPPicture);
begin
  if FImageGrayed <> Value then
  begin
    FImageGrayed.Assign(Value);
    if FUpdateCount = 0 then
      Invalidate;
  end;
end;

procedure TAdvMetroCategoryList.SetImageChecked(const Value: TGDIPPicture);
begin
  if FImageChecked <> Value then
  begin
    FImageChecked.Assign(Value);
    if FUpdateCount = 0 then
      Invalidate;
  end;
end;

procedure TAdvMetroCategoryList.SetImageCollapse(const Value: TGDIPPicture);
begin
  if FImageCollapse <> Value then
  begin
    FImageCollapse.Assign(Value);
    if FUpdateCount = 0 then
      Invalidate;
  end;
end;

procedure TAdvMetroCategoryList.SetImageExpand(const Value: TGDIPPicture);
begin
  if FImageExpand <> Value then
  begin
    FImageExpand.Assign(Value);
    if FUpdateCount = 0 then
      Invalidate;
  end;
end;

procedure TAdvMetroCategoryList.SetImages(const Value: TCustomImageList);
begin
  BeginUpdate;
  try
    FImages := Value;
    if FCategories.Count > 0 then
        FCategories[0].FDirty := True;
  finally
    EndUpdate;
  end;
end;

procedure TAdvMetroCategoryList.SetImageUnChecked(const Value: TGDIPPicture);
begin
  if FImageUnChecked <> Value then
  begin
    FImageUnChecked.Assign(Value);
    if FUpdateCount = 0 then
      Invalidate;
  end;
end;

procedure TAdvMetroCategoryList.SetItemFont(const Value: TFont);
begin
  FItemFont.Assign(Value);
  if FAutoCalcOnResize then
  begin
    Inc(FUpdateCount);
    EndUpdate;
  end;
  Invalidate;
end;

procedure TAdvMetroCategoryList.SetItemMinHeight(const Value: Integer);
begin
  if FItemMinHeight <> Value then
  begin
    FItemMinHeight := Value;
    BeginUpdate;
    try
      if FCategories.Count > 0 then
        FCategories[0].FDirty := True;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TAdvMetroCategoryList.SetPageSize(const Value: Integer);
begin
  if FPageSize <> Value then
  begin
    FPageSize := Max(1, Value);
  end;
end;

procedure TAdvMetroCategoryList.SetPictureContainer(
  const Value: TPictureContainer);
begin
  BeginUpdate;
  try
    FPictureContainer := Value;
    if FCategories.Count > 0 then
        FCategories[0].FDirty := True;
  finally
    EndUpdate;
  end;
end;

procedure TAdvMetroCategoryList.SetScrollDistance(const Value: Integer);
begin
  if (FScrollDistance <> Value) and (Value > 0) then
    FScrollDistance := Value;
end;

procedure TAdvMetroCategoryList.SetSelectedCategoryIndex(const Value: Integer);
var
  Category: TCategory;
begin
  if FSelectedCategoryIndex <> Value then
  begin
    Deselect;
    if (Value >= -1) then
    begin
      if Value < FCategories.Count then
        FSelectedCategoryIndex := Value
      else
        FSelectedCategoryIndex := FCategories.Count - 1;
    end
    else
    begin
      FSelectedCategoryIndex := -1;
      FSelectedItemIndex := -1;
    end;
    if (FSelectedCategoryIndex > -1) and (FCategories.Count > FSelectedCategoryIndex) then
    begin
      Category := FCategories[FSelectedCategoryIndex];
      if (Category.Items = nil) or (Category.Items.Count = 0) then
        FSelectedItemIndex := -1
      else if Category.Items.Count <= FSelectedItemIndex then
        FSelectedItemIndex := Category.Items.Count - 1;
    end
    else if FSelectedCategoryIndex = -1 then
      FSelectedItemIndex := -1;
    Select;
    Invalidate;
  end;
end;

procedure TAdvMetroCategoryList.SetSelectedItemIndex(const Value: Integer);
var
  Category: TCategory;
begin
  if FSelectedItemIndex <> Value then
  begin
    Deselect;
    if (FSelectedCategoryIndex > -1) and (FCategories.Count > FSelectedCategoryIndex) then
    begin
      Category := FCategories[FSelectedCategoryIndex];
      if Category.Items <> nil then
      begin
        if Value >= -1 then
        begin
          if Value < Category.Items.Count then
            FSelectedItemIndex := Value
          else
            FSelectedItemIndex := Category.Items.Count -1;
        end
        else
          FSelectedItemIndex := -1;
      end;
    end;
    Select;
    Invalidate;
  end;
end;

procedure TAdvMetroCategoryList.SetSticky(const Value: Boolean);
begin
  FSticky := Value;
  if FUpdateCount = 0 then
  begin
    ItemsToBePainted;
    Invalidate;
  end;
end;

procedure TAdvMetroCategoryList.SetTopCategoryIndex(const Value: Integer);
var
  Category: TCategory;
  Item: TCategoryItem;
begin
  if (FTopCategoryIndex <> Value) and not DesignTime then
  begin
    if FCategories.Count > Value then
    begin
      Category := FCategories[Value];
      if (FTopItemIndex <= -1) or (Category.Items = nil) or (FTopItemIndex >= Category.Items.Count) then
      begin
        FOffsetY := Min(FMaxOffSetY, Category.FStartY);
        CheckOffsets(Category.Index, -1);
      end
      else
      begin
        Item := Category.Items[FTopItemIndex];
        FOffsetY := Min(FMaxOffSetY, Item.FStartY);
        CheckOffsets(Category.Index, Item.Index);
      end;
      Invalidate;
    end;
  end;
end;

procedure TAdvMetroCategoryList.SetTopItemIndex(const Value: Integer);
var
  Category: TCategory;
  Item: TCategoryItem;
begin
  if (FTopItemIndex <> Value) and not DesignTime then
  begin
    if FCategories.Count > FTopCategoryIndex then
    begin
      Category := FCategories[FTopCategoryIndex];
      if (Value <= -1) or (Category.Items = nil) or (Value >= Category.Items.Count) then
      begin
        FOffsetY := Min(FMaxOffSetY, Category.FStartY);
        CheckOffsets(Category.Index, -1);
      end
      else
      begin
        Item := Category.Items[Value];
        FOffsetY := Min(Item.FStartY, FMaxOffSetY);
        CheckOffsets(Category.Index, Item.Index);
      end;
      Invalidate;
    end;
  end;
end;

procedure TAdvMetroCategoryList.SetVersion(const Value: string);
begin

end;

procedure TAdvMetroCategoryList.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS;
end;

{ TCategoryItem }

procedure TCategoryItem.Changed(Dirty: Boolean = True);
begin
  FDirty := Dirty;
  if Assigned(FOnChange) then
    FOnChange(Self);
  (Collection as TCategoryItems).Update(Self);
end;

procedure TCategoryItem.CheckUncheck;
begin
  FChecked := not FChecked;
  FCheckDirty := True;
  Changed;
end;

constructor TCategoryItem.Create(Collection: TCollection);
begin
  inherited;
  FChecked := False;
  FSelected := False;
  FPaint := False;
  FDirty := True;
  FShowCheckBox := True;
  FHTMLDirty := True;
  FCheckDirty := True;
end;

destructor TCategoryItem.Destroy;
begin
  inherited;
end;

function TCategoryItem.GetCategory: TCategory;
begin
  Result := Collection.Owner as TCategory;
end;

function TCategoryItem.GetCheckBoxRect(PaintRect: TRect; CheckBoxImage: TGDIPPicture): TRect;
var
  Height: Integer;
begin
  Height := FEndY - FStartY;
  Result.Left := PaintRect.Left + 5;
  Result.Right := Result.Left + CheckBoxImage.Width;
  Result.Top := PaintRect.Top + (Height - CheckBoxImage.Height) div 2;
  Result.Bottom := Result.Top + CheckBoxImage.Height;
end;

function TCategoryItem.GetPaintRect(OffsetY: Integer; ClientRect: TRect): TRect;
var
  r: TRect;
begin
  r := ClientRect;
  r.Top := FStartY - OffsetY;
  r.Bottom := FEndY - OffsetY;
  Result := r;
end;

function TCategoryItem.GetTextRect(PaintRect: TRect): TRect;
begin
  Result := PaintRect;
  InflateRect(Result, -5, -5);
  Result.Right := Result.Right - 20;
  if FShowCheckBox then
    Result.Left := Result.Left + 21;
end;

procedure TCategoryItem.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    FCheckDirty := True;
    Changed(False);
  end;
end;

procedure TCategoryItem.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TCategoryItem.SetShowCheckBox(const Value: Boolean);
begin
  if FShowCheckBox <> Value then
  begin
    FShowCheckBox := Value;
    Changed;
  end;
end;

procedure TCategoryItem.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    FDirty := true;
    FHTMLDirty := true;
    Changed;
  end;
end;

{ TMetroCategoryListAppearance }

procedure TMetroCategoryListAppearance.Assign(Source: TPersistent);
var
  Item: TMetroCategoryListAppearance;
begin
  if Source is TMetroCategoryListAppearance then
  begin
    Item := Source as TMetroCategoryListAppearance;
    FBackgroundColor := Item.FBackgroundColor;
    FBackgroundTextColor := Item.FBackgroundTextColor;
    FCategoryColor := Item.FCategoryColor;
    FCategoryTextColor := Item.FCategoryTextColor;
    FDisabledColor := Item.FDisabledColor;
    FDisabledTextColor := Item.FDisabledTextColor;
    FSelectedItemColor := Item.FSelectedItemColor;
    FSelectedItemTextColor := Item.FSelectedItemTextColor;
    Changed;
  end;
end;

procedure TMetroCategoryListAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TMetroCategoryListAppearance.Create;
begin
  FBackgroundColor := clWhite;
  FBackgroundTextColor := clBlack;
  FCategoryColor := $00B0A374;
  FCategoryTextColor := clWhite;
  FDisabledColor := clSilver;
  FDisabledTextColor := clGray;
  FSelectedItemColor := clSkyBlue;
  FSelectedItemTextColor := clWhite;
  FScrollBarColor := clGray;
end;

procedure TMetroCategoryListAppearance.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

procedure TMetroCategoryListAppearance.SetBackgroundTextColor(
  const Value: TColor);
begin
  if FBackgroundTextColor <> Value then
  begin
    FBackgroundTextColor := Value;
    Changed;
  end;
end;

procedure TMetroCategoryListAppearance.SetCategoryColor(const Value: TColor);
begin
  if FCategoryColor <> Value then
  begin
    FCategoryColor := Value;
    Changed;
  end;
end;

procedure TMetroCategoryListAppearance.SetCategoryTextColor(
  const Value: TColor);
begin
  if FCategoryTextColor <> Value then
  begin
    FCategoryTextColor := Value;
    Changed;
  end;
end;

procedure TMetroCategoryListAppearance.SetDisabledColor(const Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

procedure TMetroCategoryListAppearance.SetDisabledTextColor(
  const Value: TColor);
begin
  if FDisabledTextColor <> Value then
  begin
    FDisabledTextColor := Value;
    Changed;
  end;
end;

procedure TMetroCategoryListAppearance.SetScrollBarColor(const Value: TColor);
begin
  if FScrollBarColor <> Value then
  begin
    FScrollBarColor := Value;
    Changed;
  end;
end;

procedure TMetroCategoryListAppearance.SetSelectedItemColor(
  const Value: TColor);
begin
  if FSelectedItemColor <> Value then
  begin
    FSelectedItemColor := Value;
    Changed;
  end;
end;

procedure TMetroCategoryListAppearance.SetSelectedItemTextColor(
  const Value: TColor);
begin
  if FSelectedItemTextColor <> Value then
  begin
    FSelectedItemTextColor := Value;
    Changed;
  end;
end;


end.
