{ *************************************************************************** }
{ TAdvPolyPager component                                                     }
{ for Delphi & C++Builder                                                     }
{                                                                             }
{ written by TMS Software                                                     }
{ copyright © 2010 - 2015                                                     }
{ Email : info@tmssoftware.com                                                }
{ Web : http://www.tmssoftware.com                                            }
{                                                                             }
{ The source code is given as is. The author is not responsible               }
{ for any possible damage done due to the use of this code.                   }
{ The component can be freely used in any application. The complete           }
{ source code remains property of the author and may not be distributed,      }
{ published, given or sold in any form as such. No parts of the source        }
{ code can be included in any other component or application without          }
{ written authorization of the author.                                        }
{ *************************************************************************** }

unit AdvPolyPager;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Math, Menus,
  Dialogs, Forms, ExtCtrls, GDIPFill, AdvStyleIF, AdvGDIP, AdvVerticalPolyList,
  GDIPBase, GDIPCustomItem, GDIPPictureContainer, ImgList
  , AxCtrls, CustomItemsContainer;

type
  TAdvPolyPager = class;

  TAdvDesignList = class(TAdvVerticalPolyList)
  private
    FOwner: TAdvPolyPager;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  TAdvPolyPage = class;

  TDbgList = class(TList)
  private
    function GetItemsEx(Index: Integer): Pointer;
    procedure SetItemsEx(Index: Integer; const Value: Pointer);
  public
    property Items[Index: Integer]: Pointer read GetItemsEx write SetItemsEx;
    default;
  end;

  TProWinControl = class(TWinControl);

  TAdvPolyPage = class(TCustomControl, ITMSStyle)
  private
    FTMSStyle: TTMSStyle;
    FPageIndex: Integer;
    FTabVisible: Boolean;
    FAdvPolyPager: TAdvPolyPager;
    FCaption: TCaption;
    FTabEnabled: Boolean;
    FImageIndex: Integer;
    FUpdatingParent: Boolean;
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FPageAppearance: TGDIPFill;
    FItemLink: TCustomItem;
    FPlaceHolderBorderWidth: integer;
    procedure CMShowingChanged(var Message: TMessage);
      message CM_SHOWINGCHANGED;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure OnPageAppearanceChanged(Sender: TObject);
    procedure SetAdvPolyPager(const Value: TAdvPolyPager);
    procedure SetTabVisible(const Value: Boolean);
    procedure SetCaption(const Value: TCaption);
    procedure SetTabEnabled(const Value: Boolean);
    procedure SetImageIndex(const Value: Integer);
    function GetPageIndex: Integer;
    procedure SetPageIndex(const Value: Integer);
    procedure SetPageAppearance(const Value: TGDIPFill);
    procedure SetItemLink(const Value: TCustomItem);
    procedure SetPlaceHolderBorderWidth(const Value: integer);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Paint; override;
    procedure ReadState(Reader: TReader); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure DrawPlaceHolder(g: TGPGraphics);
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AdvPolyPager: TAdvPolyPager read FAdvPolyPager write
      SetAdvPolyPager;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    function GetThemeID: String;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -
      1;
    property PlaceHolderBorderWidth: integer read FPlaceHolderBorderWidth write SetPlaceHolderBorderWidth default 5;
    property PageAppearance: TGDIPFill read FPageAppearance write
      SetPageAppearance;
    property TabVisible
      : Boolean read FTabVisible write SetTabVisible default true;
    property TabEnabled
      : Boolean read FTabEnabled write SetTabEnabled default true;
    property ShowHint;
    property PageIndex
      : Integer read GetPageIndex write SetPageIndex stored false;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property ItemLink: TCustomItem read FItemLink write SetItemLink;
    property PopupMenu;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnStartDrag;
    property OnExit;
    property OnEnter;
  end;

  TTabChangingEvent = procedure(Sender: TObject; FromPage, ToPage: Integer;
    var AllowChange: Boolean) of object;
  TOnClosePage = procedure(Sender: TObject; PageIndex: Integer;
    var Allow: Boolean) of object;
  TOnClosedPage = procedure(Sender: TObject; PageIndex: Integer) of object;

  TOnPageListClick = procedure(Sender: TObject; X, Y: Integer) of object;
  TTabMovedEvent = procedure(Sender: TObject; FromPos: Integer;
    ToPos: Integer) of object;
  TDrawTabEvent = procedure(Sender: TObject; TabIndex: Integer;
    TabRect: TRect) of object;

  TAdvPolyPosition = (tpTopRight, tpTopCenter, tpTopLeft, tpLeftTop,
    tpLeftBottom, tpLeftCenter, tpRightTop, tpRightBottom, tpRightCenter,
    tpBottomLeft, tpBottomRight, tpBottomCenter);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvPolyPager = class(TCustomControl, ITMSStyle, IGDIPBase)
  private
    FSelItem: TCustomItem;
    BlockChange: Boolean;
    FList: TAdvDesignList;
    Ffocused: Boolean;
    FPageMargin: Integer;
    FDownItem: TCustomItem;
    FOffSetY: Integer;
    FOffSetX: Integer;
    FAdvPolyPages: TDbgList;
    FPropertiesLoaded: Boolean;
    FActivePageIndex: Integer;
    FHotPageIndex: Integer;
    FDownPageIndex: Integer;
    FOldHotPageIndex: Integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TTabChangingEvent;
    FTabOffSet: Integer;
    FFormWndProc: TWndMethod;
    FTabReorder: Boolean;
    FOnTabMoved: TTabMovedEvent;
    FOnDrawTab: TDrawTabEvent;
    FConstructed, FDesignTime: Boolean;
    FFill: TGDIPFill;
    FOnListItemSelect: TItemSelectEvent;
    FOnListItemDeSelect: TItemSelectEvent;
    FListItemReorder: TItemReorderEvent;
    FShowDesignTimeMesage: Boolean;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMVisibleChanged(var Message: TMessage);
      message CM_VISIBLECHANGED;
    procedure CMShowingChanged(var Message: TMessage);
      message CM_SHOWINGCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetPagePosition(Poly: TAdvPolyPage);
    procedure SetAllPagesPosition;
    function GetAdvPolyPageCount: Integer;
    function GetAdvPolyPages(index: Integer): TAdvPolyPage;
    function GetPopupMenuEx: TPopupMenu;
    procedure SetPopupMenuEx(const Value: TPopupMenu);
    function GetActivePage: TAdvPolyPage;
    function GetActivePageIndex: Integer;
    procedure SetActivePage(const Value: TAdvPolyPage);
    procedure SetActivePageIndex(const Value: Integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetPageMargin(const Value: Integer);
    procedure SetList(const Value: TCustomBaseList);
    function GetList: TCustomBaseList;
    procedure SetFill(const Value: TGDIPFill);
    function GetAutoSizeMode: TAutoSizeMode;
    function GetAutoSizeType: TAutoSizeType;
    function GetBorderMode: TListBorderMode;
    function GetBorderTypes: TListBorderTypes;
    function GetHandleAppearance: THandleAppearance;
    function GetHorizontalSpacing: integer;
    function GetImages: TCustomImageList;
    function GetListFill: TGDIPFill;
    function GetListMargins: TMargins;
    function GetPictureContainer: TGDIPPictureContainer;
    function GetReadOnly: Boolean;
    function GetReorder: Boolean;
    function GetScrollType: TScrollType;
    function GetShowFocus: Boolean;
    function GetThumbTracking: Boolean;
    function GetVerticalSpacing: integer;
    procedure SetAutoSizeMode(const Value: TAutoSizeMode);
    procedure SetAutoSizeType(const Value: TAutoSizeType);
    procedure SetBorderMode(const Value: TListBorderMode);
    procedure SetBorderTypes(const Value: TListBorderTypes);
    procedure SetHandleAppearance(const Value: THandleAppearance);
    procedure SetHorizontalSpacing(const Value: integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetListFill(const Value: TGDIPFill);
    procedure SetListMargins(const Value: TMargins);
    procedure SetPictureContainer(const Value: TGDIPPictureContainer);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetReorder(const Value: Boolean);
    procedure SetScrollType(const Value: TScrollType);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetThumbTracking(const Value: Boolean);
    procedure SetVerticalSpacing(const Value: integer);
    function GetItem(Index: Integer): TCustomItem;
    procedure SetItem(Index: Integer; const Value: TCustomItem);
    function GetListWidth: Integer;
    procedure SetListWidth(const Value: Integer);
    procedure SetShowDesignTimeMessage(const Value: Boolean);
  protected
    procedure FillChanged(Sender: TObject);
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetParent(AParent: TWinControl); override;
    procedure WndProc(var Msg: TMessage); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure Paint; override;
    procedure ListItemSelect(Sender: TObject; Item: TCustomItem; var Allow: Boolean);
    procedure ListItemReorder(Sender: TObject; AItem, ADropItem: TCustomItem; var Allow: Boolean);
    procedure ListItemDesignTimeSelect(Sender: TObject; Index: Integer);
    procedure ListItemDeSelect(Sender: TObject; Item: TCustomItem; var Allow: Boolean);
    procedure ListItemExpand(Sender: TObject; Item: TCustomItem; Expand: Boolean);
    procedure ListEndDraw(Sender: TObject; AGraphics: TGPGraphics; ARect: TGPRectF);
    procedure ListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure ListScroll(Sender: TObject; ScrollPos: Integer);
    function PageFromItem(Item: TCustomItem): Integer;
    procedure ListItemDestroy(Sender: TObject; Item: TCustomItem);

    procedure SetChildOrder(Child: TComponent; Order: Integer); override;

    procedure UpdateMe(PropID: Integer);
    procedure ChangeActivePage(PageIndex: Integer);

    function CanShowTab(PageIndex: Integer): Boolean;
    function GetVisibleTabCount: Integer;

    procedure DoExit; override;
    procedure DoEnter; override;
    function IsActivePageNeighbour(PageIndex: Integer): Integer;
    function UseOldDrawing: Boolean;
    function GetPolyRect: TRect;
    procedure ReadItemState(Reader: TReader; Item: TCustomItem);
    procedure SetItemParentComponent(AParent: TComponent; Item: TCustomItem);
    function GetSelectedItem: TCustomItem;
    procedure ShowControl(AControl: TControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Init;
    function GetVersionNr: Integer;
    function AddAdvPolyPage(Poly: TAdvPolyPage; Item: TCustomItem = nil): Integer; overload;
    function AddAdvPolyPage(PageCaption: TCaption; Item: TCustomItem = nil): Integer; overload;
    procedure RemoveAdvPolyPage(Poly: TAdvPolyPage);
    procedure MoveAdvPolyPage(CurIndex, NewIndex: Integer);
    function FindNextPage(CurPage: TAdvPolyPage;
      GoForward, CheckTabVisible: Boolean): TAdvPolyPage;
    procedure SelectNextPage(GoForward: Boolean);
    function IndexOfPage(Poly: TAdvPolyPage): Integer;
    property ActivePageIndex: Integer read GetActivePageIndex write
      SetActivePageIndex;
    property AdvPolyPageCount: Integer read GetAdvPolyPageCount;
    property AdvPolyPages[index: Integer]: TAdvPolyPage read GetAdvPolyPages;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetThemeID: String;

    function AddItem(AClass: TCustomItemClass): TCustomItem;
    procedure RemoveItem(Index: integer);
    procedure SelectItem(Index: integer);
    procedure VisualizeItem(Index: integer; AllowScrollItem: Boolean = true; AllowSelectItem: Boolean = true);
    procedure ScrollToItem(Index: integer);
    function InsertItem(Index: integer; AClass: TCustomItemClass): TCustomItem;
    procedure ClearItems;
    procedure ClearPages;
    property Items[Index: Integer]: TCustomItem read GetItem write SetItem; default;
  published
    property ShowDesignTimeMessage: Boolean read FShowDesignTimeMesage write SetShowDesignTimeMessage default True;
    property ListWidth: Integer read GetListWidth write SetListWidth;
    property List: TCustomBaseList read GetList write SetList;
    property Align;
    property Anchors;
    property ActivePage: TAdvPolyPage read GetActivePage write SetActivePage;
    property Constraints;
    property Color;
    property PageMargin: Integer read FPageMargin write SetPageMargin default 1;
    property PopupMenu: TPopupMenu read GetPopupMenuEx write SetPopupMenuEx;
    property ShowHint;
    property TabReorder: Boolean read FTabReorder write FTabReorder;
    property Version: string read GetVersion write SetVersion stored false;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnDrawTab: TDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property Fill: TGDIPFill read FFill write SetFill;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnTabMoved: TTabMovedEvent read FOnTabMoved write FOnTabMoved;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property TabOrder;
    property TabStop default true;
{$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
{$ENDIF}

    property OnListItemReorder: TItemReorderEvent read FListItemReorder write FListItemReorder;
    property ListAutoSizeMode: TAutoSizeMode read GetAutoSizeMode write SetAutoSizeMode;
    property ListAutoSizeType: TAutoSizeType read GetAutoSizeType write SetAutoSizeType;
    property ListFill: TGDIPFill read GetListFill write SetListFill;
    property ListHorizontalSpacing: integer read GetHorizontalSpacing write SetHorizontalSpacing;
    property ListVerticalSpacing: integer read GetVerticalSpacing write SetVerticalSpacing;
    property ListMargins: TMargins read GetListMargins write SetListMargins;
    property ListReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property ListReorder: Boolean read GetReorder write SetReorder;
    property ListShowFocus: Boolean read GetShowFocus write SetShowFocus;
    property ListBorderMode: TListBorderMode read GetBorderMode write SetBorderMode;
    property ListBorderTypes: TListBorderTypes read GetBorderTypes write SetBorderTypes;
    property ListPictureContainer: TGDIPPictureContainer read GetPictureContainer write SetPictureContainer;
    property ListImages: TCustomImageList read GetImages write SetImages;

    property OnListItemSelect: TItemSelectEvent read FOnListItemSelect write FOnListItemSelect;
    property OnListItemDeSelect: TItemSelectEvent read FOnListItemDeSelect write FOnListItemDeSelect;

    property ListHandleAppearance: THandleAppearance read GetHandleAppearance write SetHandleAppearance;
    property ListScrollType: TScrollType read GetScrollType write SetScrollType;
    property ListThumbTracking: Boolean read GetThumbTracking write SetThumbTracking;

  end;

implementation

procedure DrawFocus(g: TGPGraphics; r: TGPRectF; rn: Integer;
  rt: TFillRoundingType);
var
  pathfocus: TGPGraphicsPath;
  pfocus: TGPPen;
begin
  pathfocus := GDIPFill.CreateRoundRectangle(r, rn, rt, false);
  g.SetSmoothingMode(SmoothingModeDefault);
  pfocus := TGPPen.Create(MakeColor(255, clBlack), 1);
  pfocus.SetDashStyle(DashStyleDot);
  g.DrawPath(pfocus, pathfocus);
  pfocus.Free;
  pathfocus.Free;
end;

{ TDbgList }

function TDbgList.GetItemsEx(Index: Integer): Pointer;
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list read access');
  end;

  if Index < Count then
    Result := inherited Items[Index]
  else
    Result := nil;
end;

procedure TDbgList.SetItemsEx(Index: Integer; const Value: Pointer);
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list write access');
  end;
  if Index < Count then
    inherited Items[Index] := Value;
end;

{ TAdvPolyPage }

constructor TAdvPolyPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls] - [csOpaque];
  FCaption := '';
  FTabVisible := true;
  FTabEnabled := true;
  FImageIndex := -1;
  FPlaceHolderBorderWidth := 5;

  FPageAppearance := TGDIPFill.Create;
  FPageAppearance.OnChange := OnPageAppearanceChanged;

  DoubleBuffered := true;

  FPageAppearance.Color := clWhite;
  FPageAppearance.ColorTo := clWhite;
  FPageAppearance.BorderColor := clSilver;

end;

// ------------------------------------------------------------------------------

destructor TAdvPolyPage.Destroy;
begin
  if (FAdvPolyPager <> nil) then
  begin
    FAdvPolyPager.RemoveAdvPolyPage(Self);
  end;

  FPageAppearance.Free;
  inherited;
end;

procedure TAdvPolyPage.DrawPlaceHolder(g: TGPGraphics);
var
  itr, plr, plrinner, flrtop, flrbottom, flr: TGPRectF;
  it: TCustomItem;
  f: TGDIPFill;
  p: TGPPen;
  pth, pthinner: TGPGraphicsPath;
  rgn: TGPRegion;
  oldbdr: TColor;
  oldgm: TGlowMode;
  oldr: Integer;
  btop, bbottom: TGPSolidBrush;
  oldrtype: TFillRoundingType;
  phbr, phbw: Integer;
begin

  phbr := 0;
  phbw := PlaceHolderBorderWidth;
  if AdvPolyPager.GetSelectedItem <> nil then
  begin
    f := AdvPolyPager.List.Appearance.Selected;
    it := AdvPolyPager.List.SelectedItem;
    itr := MakeRect(it.X, it.Y, it.Width, it.Height);
    plr := MakeRect(0, 0, Width - 1, Height - 1);

    pth := GDIPFill.CreateRoundRectangle(plr, phbr, rtBoth, false);
    plrinner := MakeRect(plr.X + phbw, plr.Y + phbw, plr.Width - phbw * 2,
      plr.Height - phbw * 2);
    pthinner := GDIPFill.CreateRoundRectangle(plrinner, phbr, rtBoth, false);

    rgn := TGPRegion.Create(pth);
    rgn.Exclude(pthinner);

    g.SetClip(rgn);

    flrtop := MakeRect(plr.X, plr.Y, plr.Width, itr.Y - plr.Y + itr.Height / 2);
    flrbottom := MakeRect(flrtop.X, flrtop.Y + flrtop.Height, flrtop.Width,
      plr.Height - flrtop.Height);

    btop := TGPSolidBrush.Create(MakeColor(f.Opacity, f.Color));
    if (f.ColorMirror <> clNone) and (f.ColorMirrorTo <> clNone) then
      bbottom := TGPSolidBrush.Create(MakeColor(f.OpacityMirrorTo,
          f.ColorMirrorTo))
    else if (f.ColorMirror <> clNone) then
      bbottom := TGPSolidBrush.Create(MakeColor(f.OpacityMirror, f.ColorMirror))
    else if (f.ColorMirror = clNone) and (f.ColorTo <> clNone) then
      bbottom := TGPSolidBrush.Create(MakeColor(f.OpacityTo, f.ColorTo))
    else if (f.Color = clNone) then
      bbottom := TGPSolidBrush.Create(MakeColor(f.Opacity, f.Color))
    else
      bbottom := TGPSolidBrush.Create(MakeColor(f.Opacity, f.Color));

    if f.GradientType <> gtNone then
    begin
      g.FillRectangle(btop, flrtop);
      g.FillRectangle(bbottom, flrbottom);
    end;
    btop.Free;
    bbottom.Free;
    g.ResetClip;
    rgn.Free;

    p := TGPPen.Create(MakeColor(f.BorderOpacity, f.BorderColor),
      f.BorderWidth);
    g.DrawPath(p, pth);
    oldbdr := f.BorderColor;
    oldr := f.Rounding;
    oldrtype := f.RoundingType;
    oldgm := f.Glow;
    f.BeginUpdate;
    f.Rounding := 0;
    f.Glow := gmNone;
    f.RoundingType := rtTop;
    f.BorderColor := clNone;
    flr := MakeRect(plr.X - f.BorderWidth, itr.Y, phbw + f.BorderWidth,
      itr.Height);
    f.Fill(g, flr);
    g.DrawLine(p, flr.X, flr.Y, flr.X + (f.BorderWidth div 2), flr.Y);
    g.DrawLine(p, flr.X, flr.Y + flr.Height, flr.X + (f.BorderWidth div 2),
      flr.Y + flr.Height);
    f.Fill(g, MakeRect(plr.X + plr.Width - phbw, itr.Y, phbw - f.BorderWidth,
        itr.Height));
    f.RoundingType := oldrtype;
    f.BorderColor := oldbdr;
    f.Rounding := oldr;
    f.Glow := oldgm;
    f.EndUpdate;

    g.DrawPath(p, pthinner);
    p.Free;

    pth.Free;
    pthinner.Free;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPage.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing then
  begin
    if Assigned(FOnShow) then
      FOnShow(Self);
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPage.Paint;
var
  r: TRect;
  g: TGPGraphics;
  itSel: TCustomItem;
begin
  r := ClientRect;
  with PageAppearance do
  begin
    g := TGPGraphics.Create(Canvas.Handle);
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    PageAppearance.Fill(g, MakeRect(0, 0, r.Right - r.Left - 1,
        r.Bottom - r.Top - 1));

    if not Assigned(FAdvPolyPager) then
      Exit;

    itSel := AdvPolyPager.GetSelectedItem;
    if Assigned(itSel) then
      if ItemLink = itSel then
        DrawPlaceHolder(g);

    g.Free;
  end;
end;

procedure TAdvPolyPage.SetAdvPolyPager(const Value: TAdvPolyPager);
begin
  if (FAdvPolyPager <> Value) then
  begin
    if FAdvPolyPager <> nil then
      FAdvPolyPager.RemoveAdvPolyPage(Self);
    Parent := Value;
    if (Value <> nil) then
    begin
      Value.AddAdvPolyPage(Self);
      if not(csLoading in ComponentState) then
      begin
        if Value.AdvPolyPageCount > 1 then
        begin
          PageAppearance.Assign(Value.AdvPolyPages[Value.AdvPolyPageCount - 2]
              .PageAppearance);
        end;
      end;
    end;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPage.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvPolyPage.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvPolyPage.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvPolyPage.SetParent(AParent: TWinControl);
var
  ci, ni: Integer;
  AOfficePager: TAdvPolyPager;
begin
  if ((AParent is TAdvPolyPage) or (AParent is TAdvPolyPager)) and not
    (FUpdatingParent) then
  begin
    AOfficePager := nil;
    if (AParent is TAdvPolyPage) then
    begin
      AOfficePager := TAdvPolyPage(AParent).FAdvPolyPager;
    end
    else if (AParent is TAdvPolyPager) then
    begin
      AOfficePager := TAdvPolyPager(AParent);
    end;

    if Assigned(FAdvPolyPager) and Assigned(AOfficePager) then
    begin

      if (FAdvPolyPager <> AOfficePager) then
      begin
        FUpdatingParent := true;
        AdvPolyPager := AOfficePager;
        FUpdatingParent := false;
      end;

      if (FAdvPolyPager = AOfficePager) then
      begin
        if (AParent is TAdvPolyPage) then
        begin
          ci := FAdvPolyPager.IndexOfPage(Self);
          ni := FAdvPolyPager.IndexOfPage(TAdvPolyPage(AParent));
          AParent := AOfficePager;
          if (ci >= 0) and (ci < FAdvPolyPager.FAdvPolyPages.Count) and
            (ni >= 0) and (ni < FAdvPolyPager.FAdvPolyPages.Count) then
          begin
            FAdvPolyPager.MoveAdvPolyPage(ci, ni);
          end
          else
            raise Exception.Create('Invalid Parent ' + inttostr(ci)
                + ':' + inttostr(ni));
        end
        else if (AParent is TAdvPolyPager) then
        begin
          AParent := AOfficePager;
        end;

        FAdvPolyPager.Invalidate;
        Invalidate;
      end
      else
        raise Exception.Create('Invalid Parent');
    end;
    // else
    // raise Exception.Create('Invalid Parent3');
  end;
  inherited;
end;

procedure TAdvPolyPage.SetPlaceHolderBorderWidth(const Value: integer);
begin
  if FPlaceHolderBorderWidth <> Value then
  begin
    FPlaceHolderBorderWidth := Value;
    Invalidate;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPage.SetTabVisible(const Value: Boolean);
begin
  if (FTabVisible <> Value) then
  begin
    FTabVisible := Value;
    if Assigned(FAdvPolyPager) then
    begin
      if Assigned(FAdvPolyPager.ActivePage) then
        FAdvPolyPager.ActivePage.Invalidate;
      FAdvPolyPager.Invalidate;
    end;
    Invalidate;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPage.WMEraseBkGnd(var Message: TWMEraseBkGnd);
{ var
  DC: HDC;
  i: Integer;
  p: TPoint; }
begin
  if { FTransparent } false then
  begin
    if Assigned(Parent) then
    begin
      { DC := Message.DC;
        i := SaveDC(DC);
        p := ClientOrigin;
        Windows.ScreenToClient(Parent.Handle, p);
        p.x := -p.x;
        p.y := -p.y;
        MoveWindowOrg(DC, p.x, p.y);
        SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
        SendMessage(Parent.Handle, WM_PAINT, DC, 0);
        if (Parent is TWinCtrl) then
        (Parent as TWinCtrl).PaintCtrls(DC, nil);
        RestoreDC(DC, i); }
    end;
  end
  else
  begin
    inherited;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPage.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TAdvPolyPager then
    AdvPolyPager := TAdvPolyPager(Reader.Parent);
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPage.SetCaption(const Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Invalidate;
    if Assigned(FAdvPolyPager) then
    begin
      FAdvPolyPager.Invalidate;
    end;
  end;
end;

procedure TAdvPolyPage.SetComponentStyle(AStyle: TTMSStyle);
begin
  inherited;
  FTMSStyle := AStyle;

  case AStyle of
    tsOffice2003Blue:
      begin
        PageAppearance.Color := $FCE1CB;
        PageAppearance.ColorTo := $E0A57D;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $962D00;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Silver:
      begin
        PageAppearance.Color := $ECE2E1;
        PageAppearance.ColorTo := $B39698;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $947C7C;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Olive:
      begin
        PageAppearance.Color := $CFF0EA;
        PageAppearance.ColorTo := $8CC0B1;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $588060;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Classic:
      begin
        PageAppearance.Color := clWhite;
        PageAppearance.ColorTo := $C9D1D5;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $808080;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Luna:
      begin
        PageAppearance.Color := $FFEFE3;
        PageAppearance.ColorTo := $FFDDC4;
        PageAppearance.ColorMirror := $FFD1AD;
        PageAppearance.ColorMirrorTo := $FFDBC0;
        PageAppearance.BorderColor := $FFD1AD;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Obsidian:
      begin
        PageAppearance.Color := $F9F8F8;
        PageAppearance.ColorTo := $E4E2DF;
        PageAppearance.ColorMirror := $D1CBC7;
        PageAppearance.ColorMirrorTo := $E2DEDB;
        PageAppearance.BorderColor := clBlack; // $D1CBC7;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsWindowsXP:
      begin
        PageAppearance.Color := clWhite;
        PageAppearance.ColorTo := clBtnFace;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := clBlack;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsWhidbey:
      begin
        PageAppearance.Color := $F5F9FA;
        PageAppearance.ColorTo := $A8C0C0;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $962D00;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsCustom:
      ;
    tsOffice2007Silver:
      begin
        PageAppearance.Color := $FAEEEB;
        PageAppearance.ColorTo := $E5DBD7;
        PageAppearance.ColorMirror := $E2D8D4;
        PageAppearance.ColorMirrorTo := $D1C7C5;
        PageAppearance.BorderColor := clBlack; // $E2D8D4;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsWindowsVista:
      begin
        PageAppearance.Color := $FFFDF9;
        PageAppearance.ColorTo := $FFFAF0;
        PageAppearance.ColorMirror := $FEF9F0;
        PageAppearance.ColorMirrorTo := $FDF0D7;
        PageAppearance.BorderColor := $FCF2DA;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsWindows7:
      begin
        PageAppearance.Color := $FCEBDC;
        PageAppearance.ColorTo := $FCDBC1;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $CEA27D;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsTerminal:
      begin
        PageAppearance.Color := clBtnFace;
        PageAppearance.ColorTo := clBtnFace;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := clGray;
      end;
    tsWindows8, tsWindows10:
      begin
        PageAppearance.BorderColor := $E4E3E2;
        PageAppearance.Color := $F7F6F5;
        PageAppearance.ColorTo := $F7F6F5;
        PageAppearance.ColorMirror := $F7F6F5;
        PageAppearance.ColorMirrorTo := $F7F6F5;
      end;
    tsOffice2013White:
      begin
        PageAppearance.BorderColor := $D4D4D4;
        PageAppearance.Color := clWhite;
        PageAppearance.ColorTo := clWhite;
        PageAppearance.ColorMirror := clWhite;
        PageAppearance.ColorMirrorTo := clWhite;
      end;
    tsOffice2013LightGray:
      begin
        PageAppearance.BorderColor := $C6C6C6;
        PageAppearance.Color := $FAFAFA;
        PageAppearance.ColorTo := $FAFAFA;
        PageAppearance.ColorMirror := $FAFAFA;
        PageAppearance.ColorMirrorTo := $FAFAFA;
      end;
    tsOffice2013Gray:
      begin
        PageAppearance.BorderColor := $ABABAB;
        PageAppearance.Color := $F3F3F3;
        PageAppearance.ColorTo := $F3F3F3;
        PageAppearance.ColorMirror := $F3F3F3;
        PageAppearance.ColorMirrorTo := $F3F3F3;
      end;
   tsOffice2016White:
      begin
        PageAppearance.BorderColor := $D4D4D4;
        PageAppearance.Color := clWhite;
        PageAppearance.ColorTo := clWhite;
        PageAppearance.ColorMirror := clWhite;
        PageAppearance.ColorMirrorTo := clWhite;
      end;
    tsOffice2016Gray:
      begin
        PageAppearance.BorderColor := $444444;
        PageAppearance.Color := $B2B2B2;
        PageAppearance.ColorTo := $B2B2B2;
        PageAppearance.ColorMirror := $B2B2B2;
        PageAppearance.ColorMirrorTo := $B2B2B2;
      end;
    tsOffice2016Black:
      begin
        PageAppearance.BorderColor := $4E4E4E;
        PageAppearance.Color := $363636;
        PageAppearance.ColorTo := $363636;
        PageAppearance.ColorMirror := $363636;
        PageAppearance.ColorMirrorTo := $363636;
      end;

  end;

  Invalidate;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPage.SetTabEnabled(const Value: Boolean);
begin
  if (FTabEnabled <> Value) then
  begin
    FTabEnabled := Value;
    Invalidate;
    if Assigned(FAdvPolyPager) then
      FAdvPolyPager.Invalidate;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPage.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  if Assigned(FAdvPolyPager) then
    FAdvPolyPager.Invalidate;
end;

procedure TAdvPolyPage.SetItemLink(const Value: TCustomItem);
begin
  if FItemLink <> Value then
  begin
    FItemLink := Value;
    if Assigned(FAdvPolyPager) then
    begin
      if FAdvPolyPager.ActivePageIndex = Self.PageIndex then
      begin
        if (FAdvPolyPager.ActivePageIndex >= 0) and (FAdvPolyPager.ActivePageIndex <= FAdvPolyPager.List.Items.Count - 1) then
        begin
          if Assigned(FItemLink) then
            FAdvPolyPager.List.SelectItem(FItemLink.Index);
        end;
      end;
    end;
  end;
end;

// ------------------------------------------------------------------------------

function TAdvPolyPage.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvPolyPage.GetPageIndex: Integer;
begin
  if Assigned(FAdvPolyPager) then
    Result := FAdvPolyPager.IndexOfPage(Self)
  else
    Result := -1;
end;

function TAdvPolyPage.GetThemeID: String;
begin
  Result := ClassName;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPage.SetPageIndex(const Value: Integer);
begin
  if Assigned(FAdvPolyPager) and (Value >= 0) and
    (Value < FAdvPolyPager.AdvPolyPageCount) then
  begin
    FAdvPolyPager.MoveAdvPolyPage(FAdvPolyPager.IndexOfPage(Self), Value);
    FAdvPolyPager.Invalidate;
    Invalidate;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPage.AdjustClientRect(var Rect: TRect);
begin
  inherited;
  InflateRect(Rect, -PlaceHolderBorderWidth - 1, -PlaceHolderBorderWidth - 1);
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPage.OnPageAppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPage.SetPageAppearance(const Value: TGDIPFill);
begin
  FPageAppearance.Assign(Value);
end;

{ TAdvPolyPager }

constructor TAdvPolyPager.Create(AOwner: TComponent);
begin
  FConstructed := false;
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls] - [csOpaque];

  FOffSetX := 0;
  FOffSetY := 0;

  FShowDesignTimeMesage := True;

  FTabOffSet := 4;
  FPageMargin := 1;

  FAdvPolyPages := TDbgList.Create;

  FActivePageIndex := -1;
  FHotPageIndex := -1;
  FOldHotPageIndex := -1;
  FDownPageIndex := -1;

  DoubleBuffered := true;
  Height := 250;
  Width := 500;

  FTabReorder := false;

  TabStop := true;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState)
    );

  FList := TAdvDesignList.Create(Self);
  FList.List.OnNotifyItemDesignTimeSelect := ListItemDesignTimeSelect;
  FList.FOwner := Self;
  FList.OnEndDraw := ListEndDraw;
  FList.Width := 180;
  FList.ScrollType := stHandles;
  FList.BorderStyle := bsNone;
  FList.OnItemReorder := ListItemReorder;
  FList.OnItemSelect := ListItemSelect;
  FList.OnItemDeSelect := ListItemDeSelect;
  FList.List.OnItemDestroy := ListItemDestroy;
  FList.OnInternalExpand := ListItemExpand;
  FList.OnVerticalScroll := ListScroll;
  FList.OnHorizontalScroll := ListScroll;
  FList.OnMouseUp := ListMouseUp;
  FList.OnMouseDown := ListMouseDown;
  FList.OnKeyDown := ListKeyDown;
  FList.Parent := Self;

  FFill := TGDIPFill.Create;
  FFill.Assign(FList.Fill);
  FFill.OnChange := FillChanged;

  FList.Fill.BeginUpdate;
  FList.Fill.BorderWidth := 0;
  FList.Fill.EndUpdate;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.CreateWnd;
var
  p: TWinControl;
//  t: TAdvPolyPage;
  gotpages: Boolean;
  i: Integer;

begin
  inherited;

  if FConstructed then
    Exit;

  gotpages := false;

  if not(csDesigning in ComponentState) then
  begin
    p := Self;

    repeat
      p := p.Parent;
    until (p is TForm)or (p is TActiveForm)
    or not Assigned(p);

    if Assigned(p) then
    begin
      for i := 0 to p.ComponentCount - 1 do
      begin
        if p.Components[i].Name = Name + '1' then
          gotpages := true;
      end;
    end;

  end;

  if FDesignTime and (Name <> '') and not gotpages then
  begin
  {
    t := TAdvPolyPage.Create(Owner);
    t.AdvPolyPager := Self;
    t.Name := Name + '1';
    t.Caption := t.Name;
    t := TAdvPolyPage.Create(Owner);
    t.AdvPolyPager := Self;
    t.Name := Name + '2';
    t.Caption := t.Name;
    t := TAdvPolyPage.Create(Owner);
    t.AdvPolyPager := Self;
    t.Name := Name + '3';
    t.Caption := t.Name;
    ActivePageIndex := 0;
    }
  end;

  FConstructed := true;
end;

// ------------------------------------------------------------------------------

destructor TAdvPolyPager.Destroy;
var
  i: Integer;
begin
  FFill.Free;
  FList.Free;
  for i := 0 to FAdvPolyPages.Count - 1 do
    TAdvPolyPage(FAdvPolyPages[i]).FAdvPolyPager := nil;

  FAdvPolyPages.Free;
  inherited;
end;

procedure TAdvPolyPager.DoEnter;
begin
  inherited;
  Ffocused := true;
  Changed;
end;

procedure TAdvPolyPager.DoExit;
begin
  inherited;
  Ffocused := false;
  Changed;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.MoveAdvPolyPage(CurIndex, NewIndex: Integer);
var
  OldActivePage: TAdvPolyPage;
begin
  if (CurIndex >= 0) and (CurIndex < FAdvPolyPages.Count) and (NewIndex >= 0)
    and (NewIndex < FAdvPolyPages.Count) then
  begin
    OldActivePage := ActivePage;
    FAdvPolyPages.Move(CurIndex, NewIndex);
    ActivePage := OldActivePage;

    if Assigned(FOnTabMoved) then
      FOnTabMoved(Self, CurIndex, NewIndex);
  end;
end;

// ------------------------------------------------------------------------------

function TAdvPolyPager.AddAdvPolyPage(Poly: TAdvPolyPage; Item: TCustomItem = nil): Integer;
begin
  Result := FAdvPolyPages.IndexOf(Poly);
  if (FAdvPolyPages.IndexOf(Poly) < 0) then
  begin
    FAdvPolyPages.Add(Poly);
    Poly.FPageIndex := FAdvPolyPages.Count - 1;
    Poly.FItemLink := Item;
    Result := FAdvPolyPages.Count - 1;
  end;

  if (Poly.Parent <> Self) then
    Poly.Parent := Self;
  Poly.FAdvPolyPager := Self;
  SetPagePosition(Poly);
  if (Poly <> ActivePage) then
    Poly.Visible := false;

  if Assigned(ActivePage) then
  begin
    ActivePage.BringToFront;
    ActivePage.Invalidate;
  end;
end;

// ------------------------------------------------------------------------------

function TAdvPolyPager.AddAdvPolyPage(PageCaption: TCaption; Item: TCustomItem = nil): Integer;
var
  aPage: TAdvPolyPage;
begin
  aPage := TAdvPolyPage.Create(Self);
  aPage.Caption := PageCaption;
  aPage.FPageIndex := FAdvPolyPages.Count - 1;
  aPage.FItemLink := Item;
  Result := AddAdvPolyPage(aPage, Item);
end;

function TAdvPolyPager.AddItem(AClass: TCustomItemClass): TCustomItem;
begin
  Result := FList.AddItem(AClass);
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.AlignControls(AControl: TControl; var ARect: TRect);
var
  r: TRect;
begin
  inherited;
  r := ClientRect;
  FList.Top := r.Top + FPageMargin + 1;
  FList.Left := r.Left + 1;
  FList.Height := r.Bottom - r.Top - FPageMargin * 2 - 2;
  SetAllPagesPosition;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.ListItemDeSelect(Sender: TObject; Item: TCustomItem;
  var Allow: Boolean);
begin
{
  if FDownItem = Item then
  begin
    Allow := False;
    Exit;
  end;
}
  Allow := (FSelItem <> Item);

  if Assigned(OnListItemDeSelect) then
    OnListItemDeSelect(Sender, Item, Allow);

  if (FSelItem = Item) and Allow then
    ActivePageIndex := -1
end;

procedure TAdvPolyPager.ListItemDesignTimeSelect(Sender: TObject;
  Index: Integer);
begin
  if (Index >= 0) and (Index <= List.Items.Count - 1) then
    FSelItem := Items[Index];
  Changed;
end;

procedure TAdvPolyPager.ListItemDestroy(Sender: TObject; Item: TCustomItem);
var
  p: integer;
begin
  if Assigned(Item) then
  begin
    p := PageFromItem(Item);
    if (p >= 0) and (p <= AdvPolyPageCount - 1) then
    begin
      AdvPolyPages[p].ItemLink := nil;
      AdvPolyPages[p].Invalidate;
    end;
  end;
end;

procedure TAdvPolyPager.ListItemExpand(Sender: TObject; Item: TCustomItem;
  Expand: Boolean);
var
  p: integer;
begin
  if Item = List.SelectedItem then
  begin
    p := PageFromItem(Item);
    if (p >= 0) and (p <= AdvPolyPageCount - 1) then
      AdvPolyPages[p].Visible := Expand
  end;
end;

procedure TAdvPolyPager.ListItemReorder(Sender: TObject; AItem,
  ADropItem: TCustomItem; var Allow: Boolean);
begin
  if Assigned(OnListItemReorder) then
    OnListItemReorder(Self, AItem, ADropItem, Allow);
end;

procedure TAdvPolyPager.ListEndDraw(Sender: TObject; AGraphics: TGPGraphics;
  ARect: TGPRectF);
var
  itSel: TCustomItem;
  ri: TGPRectF;
  rd: Integer;
  gm: TGlowMode;
  br: integer;
begin
  itSel := GetSelectedItem;
  if Assigned(itSel) then
  begin
    if PageFromItem(itSel) <> -1 then
    begin
      ri := MakeRect(itSel.X + itSel.Width - 1, itSel.Y,
        FList.ListMargins.Right + 4, itSel.Height);
      List.Appearance.Selected.BeginUpdate;
      rd := List.Appearance.Selected.Rounding;
      gm := List.Appearance.Selected.Glow;
      br := List.Appearance.Selected.BorderWidth;
      List.Appearance.Selected.Rounding := 0;
      List.Appearance.Selected.Glow := gmNone;
      List.Appearance.Selected.Fill(AGraphics, ri);
      List.Appearance.Selected.BorderWidth := 0;
      List.Appearance.Selected.Fill(AGraphics, MakeRect(itSel.X + itSel.Width - 2, itSel.Y,
        2, itSel.Height));

      List.Appearance.Selected.BorderWidth := br;
      List.Appearance.Selected.Rounding := rd;
      List.Appearance.Selected.Glow := gm;
      List.Appearance.Selected.EndUpdate;
    end;
  end;
end;

procedure TAdvPolyPager.ListItemSelect(Sender: TObject; Item: TCustomItem; var Allow: Boolean);
var
  i, p: Integer;
  AllowChange: Boolean;
  pt: integer;
begin
  pt := PageFromItem(Item);
  AllowChange := true;
  if (pt >= 0) and (pt <= AdvPolyPageCount - 1) then
  begin
    if Assigned(FOnChanging) and FPropertiesLoaded and not
      (csDestroying in ComponentState) then
      FOnChanging(Self, ActivePageIndex, pt, AllowChange);
  end;

  if AllowChange then
  begin
    FSelItem := Item;
    if Assigned(OnListItemSelect) then
      OnListItemSelect(Sender, Item, Allow);

    if Allow then
    begin
      if BlockChange then
        Exit;

      if Assigned(Item) then
      begin
        p := PageFromItem(Item);
        SetActivePageIndex(p);
      end;

      for i := 0 to AdvPolyPageCount - 1 do
        AdvPolyPages[i].Invalidate;
    end
    else
      ActivePageIndex := -1;
  end
  else
  begin
    Allow := False;
  end;
end;

procedure TAdvPolyPager.ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  FSelItem := nil;
end;

procedure TAdvPolyPager.ListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDownItem := list.ItemAtXY(X, Y);
end;

procedure TAdvPolyPager.ListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FSelItem := list.ItemAtXY(X, Y);
end;

procedure TAdvPolyPager.ListScroll(Sender: TObject; ScrollPos: Integer);
var
  i: Integer;
begin
  for i := 0 to AdvPolyPageCount - 1 do
    AdvPolyPages[i].Invalidate;
end;

procedure TAdvPolyPager.Loaded;
begin
  inherited;
  FPropertiesLoaded := true;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if not (csDestroying in ComponentState) then
  begin
    if (Operation = opRemove)  then
    begin
      if (AComponent = PopupMenu) then
        PopupMenu := nil;
    end
    else
    begin
    //  ActivePageIndex :=
    end;
  end;

  inherited;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
end;

// ------------------------------------------------------------------------------

function TAdvPolyPager.PageFromItem(Item: TCustomItem): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to AdvPolyPageCount - 1 do
  begin
    if AdvPolyPages[i].ItemLink = Item then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TAdvPolyPager.Paint;
var
  r: TRect;
  th: Integer;
  g: TGPGraphics;
begin
  inherited;

  r := ClientRect;

  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  Fill.Fill(g, MakeRect(0, 0, Width - 1, Height - 1));
  g.Free;

  if (csDesigning in ComponentState) and (FAdvPolyPages.Count = 0) and ShowDesignTimeMessage then
  begin
    Canvas.Font.Assign(Self.Font);
    Canvas.Brush.Style := bsClear;
    th := Canvas.TextHeight('gh');
    Canvas.TextOut(FList.Width + 10, Height div 2,
      'Right-click and choose "New Page"');
    Canvas.TextOut(FList.Width + 10, (Height div 2) + th,
      'to insert a new tabsheet');

    Canvas.TextOut(FList.Width + 10, Height div 2 + 3 * th,
      'Link the item to the tabsheet through the ItemLink property');

    Canvas.Font.Style := [fsItalic];
    Canvas.TextOut(FList.Width + 10, Height div 2 + 5 * th,
      'If no such right-click menu option appears');
    Canvas.TextOut(FList.Width + 10, Height div 2 + 6 * th,
      'please install designtime package!');
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.ReadItemState(Reader: TReader; Item: TCustomItem);
begin
  if Reader.Parent = Self then
  begin
    Item.ItemOwner := FList;
    List.AssignEvents(Item);
    List.Items.Add(Item);
  end;
end;

procedure TAdvPolyPager.RemoveAdvPolyPage(Poly: TAdvPolyPage);
var
  i, ni: Integer;
begin
  i := FAdvPolyPages.IndexOf(Poly);
  if (i >= 0) then
  begin
    if i < ActivePageIndex then
      ni := ActivePageIndex - 1
    else
      ni := ActivePageIndex;

    if (ActivePage = Poly) then
      SelectNextPage(true);

    FAdvPolyPages.Delete(i);
    Poly.FAdvPolyPager := nil;

    ActivePageIndex := ni;
    Invalidate;
    if Assigned(ActivePage) then
      ActivePage.Invalidate;
  end;
end;

procedure TAdvPolyPager.RemoveItem(Index: integer);
begin
  FList.RemoveItem(Index);
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.SetParent(AParent: TWinControl);
begin
  if (AParent is TAdvPolyPager) then
    raise Exception.Create('Invalid Parent');

  inherited;

  if (not FPropertiesLoaded) and not(csDesigning in ComponentState) and not
    (csLoading in ComponentState) then
  begin
    Init;
  end;
end;

procedure TAdvPolyPager.SetPictureContainer(const Value: TGDIPPictureContainer);
begin
  FList.PictureContainer := Value;
end;

// ------------------------------------------------------------------------------

function TAdvPolyPager.GetAdvPolyPageCount: Integer;
begin
  Result := FAdvPolyPages.Count;
end;

// ------------------------------------------------------------------------------

function TAdvPolyPager.GetAdvPolyPages(index: Integer): TAdvPolyPage;
begin
  Result := TAdvPolyPage(FAdvPolyPages[index]);
end;

function TAdvPolyPager.GetAutoSizeMode: TAutoSizeMode;
begin
  Result := FList.AutoSizeMode;
end;

function TAdvPolyPager.GetAutoSizeType: TAutoSizeType;
begin
  Result := FList.AutoSizeType;
end;

function TAdvPolyPager.GetBorderMode: TListBorderMode;
begin
  Result := FList.BorderMode;
end;

function TAdvPolyPager.GetBorderTypes: TListBorderTypes;
begin
  Result := FList.BorderTypes;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.SetChildOrder(Child: TComponent; Order: Integer);
begin
  inherited SetChildOrder(Child, Order);
end;

procedure TAdvPolyPager.SetComponentStyle(AStyle: TTMSStyle);
var
  i: Integer;
begin
  inherited;
  FList.SetComponentStyle(AStyle);
  FFill.Assign(FList.Fill);
  for i := 0 to FAdvPolyPages.Count - 1 do
    AdvPolyPages[i].SetComponentStyle(AStyle);
end;

procedure TAdvPolyPager.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvPolyPager.SetHandleAppearance(const Value: THandleAppearance);
begin
  FList.HandleAppearance := Value;
end;

procedure TAdvPolyPager.SetHorizontalSpacing(const Value: integer);
begin
  FList.HorizontalSpacing := Value;
end;

procedure TAdvPolyPager.SetImages(const Value: TCustomImageList);
begin
  FList.ImageList := Value;
end;

procedure TAdvPolyPager.SetItem(Index: Integer; const Value: TCustomItem);
begin
  List.Items[index] := Value;
end;

procedure TAdvPolyPager.SetItemParentComponent(AParent: TComponent;
  Item: TCustomItem);
begin
  if AParent = Self then
  begin
    Item.ItemOwner := FList;
    List.AssignEvents(Item);
    List.Items.Add(Item);
  end;
end;

procedure TAdvPolyPager.SetList(const Value: TCustomBaseList);
begin
  if FList.List <> Value then
  begin
    FList.List.Assign(Value);
  end;
end;

procedure TAdvPolyPager.SetListFill(const Value: TGDIPFill);
begin
  FList.Fill := Value;
end;

procedure TAdvPolyPager.SetListMargins(const Value: TMargins);
begin
  FList.ListMargins := Value;
end;

procedure TAdvPolyPager.SetListWidth(const Value: Integer);
begin
  if FList.Width <> Value then
  begin
    FList.Width := Value;
    Changed;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.WMSize(var Message: TWMSize);
begin
  inherited;
  SetAllPagesPosition;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.SetPopupMenuEx(const Value: TPopupMenu);
begin
  inherited PopupMenu := Value;
end;

procedure TAdvPolyPager.SetReadOnly(const Value: Boolean);
begin
  FList.ReadOnly := Value;
end;

procedure TAdvPolyPager.SetReorder(const Value: Boolean);
begin
  FList.Reorder := Value;
end;

procedure TAdvPolyPager.SetScrollType(const Value: TScrollType);
begin
  FList.ScrollType := Value;
end;

procedure TAdvPolyPager.SetShowDesignTimeMessage(const Value: Boolean);
begin
  if FShowDesignTimeMesage <> Value then
  begin
    FShowDesignTimeMesage := Value;
    FList.ShowDesignTimeMessage := Value;
    Repaint;
  end;
end;

procedure TAdvPolyPager.SetShowFocus(const Value: Boolean);
begin
  FList.ShowFocus := Value;
end;

procedure TAdvPolyPager.SetThumbTracking(const Value: Boolean);
begin
  FList.ThumbTracking := Value;
end;

procedure TAdvPolyPager.CMShowingChanged(var Message: TMessage);
begin
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
end;

// ------------------------------------------------------------------------------

function TAdvPolyPager.GetPopupMenuEx: TPopupMenu;
begin
  Result := Inherited PopupMenu;
end;

function TAdvPolyPager.GetReadOnly: Boolean;
begin
  Result := FList.ReadOnly;
end;

function TAdvPolyPager.GetReorder: Boolean;
begin
  Result := FList.Reorder;
end;

function TAdvPolyPager.GetScrollType: TScrollType;
begin
  Result := FList.ScrollType;
end;

function TAdvPolyPager.GetSelectedItem: TCustomItem;
begin
  Result := List.SelectedItem;
  if Assigned(Result) then
  begin
    if not Result.Visible then
      Result := nil;
  end;
end;

function TAdvPolyPager.GetShowFocus: Boolean;
begin
  Result := FList.ShowFocus;
end;

procedure TAdvPolyPager.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if TabStop and not(csDesigning in ComponentState) then
    SetFocus;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
  Control: TControl;
begin
  for i := 0 to FAdvPolyPages.Count - 1 do
    Proc(TComponent(FAdvPolyPages[i]));

  for i := 0 to ControlCount - 1 do
  begin
    Control := Controls[i];
    if (Control.Owner = Root) and (FAdvPolyPages.IndexOf(Control) < 0) then
      Proc(Control);
  end;

  FList.GetChildren(Proc, Root);
end;

function TAdvPolyPager.GetHandleAppearance: THandleAppearance;
begin
  Result := FList.HandleAppearance;
end;

function TAdvPolyPager.GetHorizontalSpacing: integer;
begin
  Result := FList.HorizontalSpacing;
end;

function TAdvPolyPager.GetImages: TCustomImageList;
begin
  Result := FList.ImageList;
end;

function TAdvPolyPager.GetItem(Index: Integer): TCustomItem;
begin
  if (List.Items[Index] is TCustomItem) then
    Result := TCustomItem(List.Items[index])
  else
    Result := nil;
end;

function TAdvPolyPager.GetList: TCustomBaseList;
begin
  Result := FList.List;
end;

function TAdvPolyPager.GetListFill: TGDIPFill;
begin
  Result := FList.Fill;
end;

function TAdvPolyPager.GetListMargins: TMargins;
begin
  Result := FList.ListMargins;
end;

function TAdvPolyPager.GetListWidth: Integer;
begin
  Result := FList.Width;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.FillChanged(Sender: TObject);
begin
  Invalidate;
end;

function TAdvPolyPager.FindNextPage(CurPage: TAdvPolyPage;
  GoForward, CheckTabVisible: Boolean): TAdvPolyPage;
var
  i, j, CurIndex: Integer;
begin
  Result := nil;
  CurIndex := FAdvPolyPages.IndexOf(CurPage);

  if (CurPage = nil) or (CurIndex < 0) then
  begin

    if FAdvPolyPages.Count > 0 then
    begin
      if GoForward then
        Result := FAdvPolyPages[0]
      else
        Result := FAdvPolyPages[FAdvPolyPages.Count - 1];
    end;
    Exit;
  end;

  if GoForward then
  begin
    i := CurIndex;
    j := 0; // 1;
    while (j < FAdvPolyPages.Count) do
    begin
      Inc(i);
      if (i >= FAdvPolyPages.Count) then
        i := 0;
      if (CheckTabVisible and AdvPolyPages[i].TabVisible)
        or not CheckTabVisible then
      begin
        Result := AdvPolyPages[i];
        break;
      end;
      Inc(j);
    end;
  end
  else // BackWard
  begin
    i := CurIndex;
    j := 0; // 1;
    while (j < FAdvPolyPages.Count) do
    begin
      dec(i);
      if (i >= FAdvPolyPages.Count) then
        i := 0;
      if (i < 0) then
        i := FAdvPolyPages.Count - 1;
      if (CheckTabVisible and AdvPolyPages[i].TabVisible)
        or not CheckTabVisible then
      begin
        Result := AdvPolyPages[i];
        break;
      end;
      Inc(j);
    end;
  end;
end;

// ------------------------------------------------------------------------------

function TAdvPolyPager.GetActivePage: TAdvPolyPage;
begin
  Result := nil;
  if (ActivePageIndex >= 0) and (ActivePageIndex < FAdvPolyPages.Count) then
    Result := AdvPolyPages[FActivePageIndex];
end;

// ------------------------------------------------------------------------------

function TAdvPolyPager.GetActivePageIndex: Integer;
begin
  Result := FActivePageIndex;
end;

procedure TAdvPolyPager.ScrollToItem(Index: integer);
begin
  FList.ScrollToItem(Index);
end;

procedure TAdvPolyPager.SelectItem(Index: integer);
begin
  FList.SelectItem(Index);
end;

procedure TAdvPolyPager.SelectNextPage(GoForward: Boolean);
var
  i, j: Integer;
begin
  if (ActivePageIndex < 0) then
    Exit;

  if GoForward then
  begin
    i := ActivePageIndex;
    j := 0; // 1;
    while (j < FAdvPolyPages.Count) do
    begin
      Inc(i);
      if (i >= FAdvPolyPages.Count) then
        i := 0;
      if (ActivePage <> AdvPolyPages[i]) and AdvPolyPages[i]
        .TabVisible and AdvPolyPages[i].TabEnabled then
      begin
        ActivePageIndex := i;
        break;
      end;
      Inc(j);
    end;
  end
  else // BackWard
  begin
    i := ActivePageIndex;
    j := 0; // 1;
    while (j < FAdvPolyPages.Count) do
    begin
      dec(i);
      if (i >= FAdvPolyPages.Count) then
        i := 0;
      if (i < 0) then
        i := FAdvPolyPages.Count - 1;
      if (ActivePage <> AdvPolyPages[i]) and AdvPolyPages[i]
        .TabVisible and AdvPolyPages[i].TabEnabled then
      begin
        ActivePageIndex := i;
        break;
      end;
      Inc(j);
    end;
  end;
end;

// ------------------------------------------------------------------------------

function TAdvPolyPager.IndexOfPage(Poly: TAdvPolyPage): Integer;
begin
  Result := FAdvPolyPages.IndexOf(Poly);
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.SetActivePage(const Value: TAdvPolyPage);
begin
  if (FAdvPolyPages.IndexOf(Value) >= 0) then
  begin
    ActivePageIndex := FAdvPolyPages.IndexOf(Value);
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.ChangeActivePage(PageIndex: Integer);
var
  aForm: TCustomForm;
  AllowChange: Boolean;
  i: Integer;
begin
  if (PageIndex >= 0) and (PageIndex < FAdvPolyPages.Count) and
    (PageIndex <> ActivePageIndex) then
  begin
    AllowChange := true;
    if Assigned(FOnChanging) and FPropertiesLoaded and not
      (csDestroying in ComponentState) then
      FOnChanging(Self, ActivePageIndex, PageIndex, AllowChange);

    if not AllowChange then
    begin
      Exit;
    end;

    if (ActivePageIndex >= 0) and (ActivePageIndex < FAdvPolyPages.Count) then
    begin
      AdvPolyPages[FActivePageIndex].Visible := false;

      if Assigned(AdvPolyPages[FActivePageIndex].FOnHide) then
        AdvPolyPages[FActivePageIndex].FOnHide(AdvPolyPages[FActivePageIndex]);
    end;

    FActivePageIndex := PageIndex;
    AdvPolyPages[FActivePageIndex].Visible := true;
    AdvPolyPages[FActivePageIndex].BringToFront;
    BlockChange := true;
    if (ActivePageIndex >= 0) and (ActivePageIndex <= List.Items.Count - 1) then
    begin
      if Assigned(AdvPolyPages[ActivePageIndex].ItemLink) then
      begin
        List.SelectItem(AdvPolyPages[ActivePageIndex].ItemLink.Index);
      end;
    end;
    BlockChange := false;

    if Assigned(FOnChange) and not(csDestroying in ComponentState) and not
      (csLoading in ComponentState) then
      FOnChange(Self);

    if (csDesigning in ComponentState) and not(csLoading in ComponentState) then
    begin
      aForm := GetParentForm(Self);
      if (aForm <> nil) and (aForm.Designer <> nil) then
        aForm.Designer.Modified;
    end;
  end
  else if PageIndex = -1 then
  begin
    for i := 0 to AdvPolyPageCount - 1 do
    begin
      AdvPolyPages[i].Visible := false;
    end;
  end
  else if (PageIndex = ActivePageIndex) and (PageIndex >= 0) and (PageIndex <= AdvPolyPageCount - 1) then
    AdvPolyPages[PageIndex].Visible := true;
end;

procedure TAdvPolyPager.ClearItems;
begin
  FList.ClearItems;
end;

procedure TAdvPolyPager.ClearPages;
var
  I: Integer;
begin
  for I := AdvPolyPageCount - 1 downto 0 do
    AdvPolyPages[i].Free;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.SetActivePageIndex(const Value: Integer);
begin
  if (Value >= 0) and (Value <= AdvPolyPageCount - 1) then
    FSelItem := Self.AdvPolyPages[Value].ItemLink;

  ChangeActivePage(Value);
  Invalidate;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.SetAllPagesPosition;
var
  i: Integer;
begin
  for i := 0 to FAdvPolyPages.Count - 1 do
  begin
    SetPagePosition(TAdvPolyPage(FAdvPolyPages[i]));
  end;
end;

function TAdvPolyPager.GetPictureContainer: TGDIPPictureContainer;
begin
  Result := FList.PictureContainer;
end;

procedure TAdvPolyPager.SetAutoSizeMode(const Value: TAutoSizeMode);
begin
  FList.AutoSizeMode := Value;
end;

procedure TAdvPolyPager.SetAutoSizeType(const Value: TAutoSizeType);
begin
  FList.AutoSizeType := Value;
end;

procedure TAdvPolyPager.SetBorderMode(const Value: TListBorderMode);
begin
  FList.BorderMode := Value;
end;

procedure TAdvPolyPager.SetBorderTypes(const Value: TListBorderTypes);
begin
  FList.BorderTypes := Value;
end;

function TAdvPolyPager.GetPolyRect: TRect;
begin
  Result := ClientRect;
  Result.Top := Result.Top + FPageMargin + 1;
  Result.Left := Result.Left + FList.Width;
  Result.Right := Result.Right - FPageMargin;
  Result.Bottom := Result.Bottom - FPageMargin - 1;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.SetPagePosition(Poly: TAdvPolyPage);
var
  r: TRect;
begin
  if (Poly <> nil) and (FAdvPolyPages.IndexOf(Poly) >= 0) then
  begin
    r := GetPolyRect;
    {
      Poly.Left := R.Left;
      Poly.Top := R.Top;
      Poly.Width := R.Right - R.Left;
      Poly.Height := R.Bottom - R.Top;
      }
    Poly.SetBounds(r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top);
  end;
end;

// ------------------------------------------------------------------------------

function TAdvPolyPager.GetThemeID: String;
begin
  Result := ClassName;
end;

function TAdvPolyPager.GetThumbTracking: Boolean;
begin
  Result := Flist.ThumbTracking;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.UpdateMe(PropID: Integer);
begin
  Invalidate;
  if Assigned(ActivePage) then
    ActivePage.Invalidate;
end;

// ------------------------------------------------------------------------------

function TAdvPolyPager.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := inttostr(Hi(Hiword(vn))) + '.' + inttostr(Lo(Hiword(vn)))
    + '.' + inttostr(Hi(Loword(vn))) + '.' + inttostr(Lo(Loword(vn)));
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.SetVersion(const Value: string);
begin

end;

procedure TAdvPolyPager.SetVerticalSpacing(const Value: integer);
begin
  Flist.VerticalSpacing := Value;
end;

procedure TAdvPolyPager.ShowControl(AControl: TControl);
begin
  if (AControl is TAdvPolyPage) and (TAdvPolyPage(AControl).AdvPolyPager = Self) then
    SetActivePage(TAdvPolyPage(AControl));
  inherited ShowControl(AControl);
end;

// ------------------------------------------------------------------------------

function TAdvPolyPager.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

function TAdvPolyPager.GetVerticalSpacing: integer;
begin
  Result := FList.VerticalSpacing;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.CMDialogChar(var Message: TCMDialogChar);
var
  i: Integer;
begin
  for i := 0 to FAdvPolyPages.Count - 1 do
    if IsAccel(Message.CharCode, AdvPolyPages[i].Caption) and CanShowTab(i)
      and CanFocus then
    begin
      Message.Result := 1;
      ActivePageIndex := i;
      Exit;
    end;
  inherited;
end;

// ------------------------------------------------------------------------------

function TAdvPolyPager.CanShowTab(PageIndex: Integer): Boolean;
begin
  Result := (PageIndex >= 0) and (PageIndex < FAdvPolyPages.Count) and
    (AdvPolyPages[PageIndex].TabVisible);
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.SetPageMargin(const Value: Integer);
begin
  if FPageMargin <> Value then
  begin
    FPageMargin := Value;
    Changed;
  end;
end;

function TAdvPolyPager.GetVisibleTabCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FAdvPolyPages.Count - 1 do
  begin
    if (AdvPolyPages[i].TabVisible) then
      Result := Result + 1;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.Init;
begin
  FPropertiesLoaded := true;
end;

function TAdvPolyPager.InsertItem(Index: integer;
  AClass: TCustomItemClass): TCustomItem;
begin
  Result := FList.InsertItem(Index, AClass);
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.WMKeyDown(var Message: TWMKeyDown);
var
  Ctrl: TWinControl;
begin
  case Message.CharCode of
    VK_LEFT, VK_UP:
      begin
        SelectNextPage(false);
      end;
    VK_RIGHT, VK_DOWN:
      begin
        SelectNextPage(true);
      end;
    VK_TAB:
      begin
        if Assigned(Self.Parent) then
        begin
          Ctrl := TProWinControl(Self.Parent).FindNextControl(Self, true, true,
            true);
          if Assigned(Ctrl) and Ctrl.CanFocus then
          begin
            Ctrl.SetFocus;
          end;
        end;
      end;
  end;
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS { + DLGC_WANTTAB } ;
  { using DLGC_WANTTAB, disabled default Tab key functioning }
end;

// ------------------------------------------------------------------------------

procedure TAdvPolyPager.WndProc(var Msg: TMessage);
var
  p: TWinControl;
begin
  if (Msg.Msg = WM_DESTROY) then
  begin
    // restore subclassed proc
    if not(csDesigning in ComponentState) and Assigned(FFormWndProc) then
    begin
      p := Self;
      repeat
        p := p.Parent;
      until (p is TForm) or (p is TActiveForm)
      or not Assigned(p);

      if (p <> nil) then
      begin
        p.WindowProc := FFormWndProc;
        FFormWndProc := nil;
      end;
    end;
  end;

  inherited;
end;

function TAdvPolyPager.UseOldDrawing: Boolean;
begin
  Result := true;
end;

procedure TAdvPolyPager.VisualizeItem(Index: integer; AllowScrollItem,
  AllowSelectItem: Boolean);
begin
  FList.VisualizeItem(Index, AllowScrollItem, AllowSelectItem);
end;

// ------------------------------------------------------------------------------

function TAdvPolyPager.IsActivePageNeighbour(PageIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  if (PageIndex = ActivePageIndex) or (PageIndex < 0) or
    (PageIndex >= AdvPolyPageCount) then
    Exit;

  if (PageIndex < ActivePageIndex) then
  begin
    for i := ActivePageIndex - 1 downto PageIndex do
    begin
      if AdvPolyPages[i].TabVisible then
      begin
        if (i = PageIndex) then
          Result := -1;
        break;
      end;
    end;
  end
  else // if (PageIndex > ActivePageIndex) then
  begin
    for i := ActivePageIndex + 1 to PageIndex do
    begin
      if AdvPolyPages[i].TabVisible then
      begin
        if (i = PageIndex) then
          Result := 1;
        break;
      end;
    end;
  end;
end;

{ TAdvDesignList }

procedure TAdvDesignList.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  p: TPoint;
  it: TCustomItem;
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    it := List.ItemAtXY(p.X, p.Y);
    if Assigned(it) then
      Msg.Result := 1;
  end;
end;

procedure TAdvDesignList.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  it: TCustomItem;
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    it := List.ItemAtXY(X, Y);
    if Assigned(it) and Assigned(FOwner) then
    begin
      FOwner.SetActivePageIndex(FOwner.PageFromItem(it));
    end;
  end;
end;

end.
