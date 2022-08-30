{************************************************************************}
{ TTREELIST component                                                    }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{            copyright © 2000 - 2015                                     }
{            Email : info@tmssoftware.com                                }
{            Web : http://www.tmssoftware.com                            }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit TreeList;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Comctrls, ExtCtrls, CommCtrl, Types;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 4; // Build nr.

  // version history
  // 0.8.1.3 : Fixed issue with HeaderImage position
  // 1.0.0.0 : Added support for Delphi 2006, C++Builder 2006
  // 1.0.0.1 : Fixed painting issue for TreeList with zero columns
  // 1.0.0.2 : Fixed issue with RightClickSelect
  // 1.0.0.3 : Fixed issue with HeaderSettings.AllowResize
  // 1.0.0.4 : Fixed issue with VCL.NET
  // 1.0.0.5 : Fixed image drawing issue with XP themes
  // 1.0.1.0 : New : Exposed property TreeList.Header
  //         : New : Exposed property TreeList.Header.IsSizing
  // 1.0.1.1 : Fixed : issue with creating Treelist at runtime
  // 1.0.1.2 : Fixed : issue with painting & scrolling
  // 1.0.1.3 : Fixed : issue with header painting during horizontal scroll
  // 1.0.1.4 : Fixed : issue with setting header visibility false
  // 1.0.1.5 : Fixed : issue with persisting OnHeaderSizeChanged event
  // 1.0.1.6 : Fixed : issue with OnGetImageIndex
  // 1.1.0.0 : New : Event OnDrawNodeCell added to customize node cells
  //         : New : Method MoveColumn() added
  //         : New : HeaderSettings.AllowMove added
  // 1.1.0.1 : Fixed : Issue with auto node selection
  // 1.1.0.2 : Fixed : Issue with righ-alignment of items in last column
  // 1.1.1.0 : New : Added ColumnLineColor property
  // 1.1.1.1 : Fixed : Issue with treeline drawing when DoubleBuffered = true
  // 1.1.1.2 : Fixed : Issue with ShowRoot = false
  // 1.1.1.3 : Fixed : Issue with handling OnCustomDrawItem()
  // 1.1.1.4 : Fixed : Issue with persisting ItemHeight property value


type
  TTreeList = class;

  TColumnItem = class(TCollectionItem)
  private
    FWidth: Integer;
    FAlignment: TAlignment;
    FColumnHeader: string;
    FFont: TFont;
    FImage: Boolean;
    FHeaderAlign: TAlignment;
    FHeaderImage: Integer;
    procedure SetWidth(const value:integer);
    procedure SetAlignment(const value:tAlignment);
    procedure SetColumnHeader(const value:string);
    procedure SetFont(const value:TFont);
    procedure SetImage(const Value: boolean);
    procedure SetHeaderAlign(const Value: TAlignment);
    procedure SetHeaderImage(const Value: integer);
  protected
    procedure FontChanged(Sender: TObject);
  public
    constructor Create(Collection:TCollection); override;
    destructor Destroy; override;
    procedure Assign(source :TPersistent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Font: TFont read FFont write SetFont;
    property Header:string read FColumnHeader write SetColumnHeader;
    property HeaderAlign: TAlignment read FHeaderAlign write SetHeaderAlign default taLeftJustify;
    property HeaderImage: Integer read FHeaderImage write SetHeaderImage default -1;
    property Image: Boolean read FImage write SetImage default false;
    property Width: Integer read FWidth write SetWidth;
  end;

  TColumnCollection = class(TCollection)
  private
    FOwner:TTreeList;
    function GetItem(Index: Integer): TColumnItem;
    procedure SetItem(Index: Integer; const Value: TColumnItem);
  protected
    procedure Update(Item: TCollectionItem); override;
    procedure Move(FromIndex, ToIndex: integer);
  public
    constructor Create(AOwner: TTreeList);
    function GetOwner: TPersistent; override;
    function Add: TColumnItem;
    function Insert(Index: Integer): TColumnItem;
    property Items[Index: Integer]: TColumnItem read GetItem write SetItem; default;
 end;

  THeaderSizeChangedEvent = procedure(Sender:TObject; Section:integer) of object;
  TTLHeaderClickEvent = procedure(Sender:TObject; SectionIdx:integer) of object;
  TSectionMoveEvent = procedure(Sender: TObject; FromIndex, ToIndex: integer; var Allow: boolean) of object;

  TTLHeader = class(THeader)
  private
    FOldCursor: TCursor;
    FColor: TColor;
    FOnClick: TTLHeaderClickEvent;
    FOnRightClick:TTLHeaderClickEvent;
    FIsSizing: boolean;
    FIsMoving: boolean;
    FHitTest: TPoint;
    FAllowMove: boolean;
    FMouseDown: boolean;
    FMouseDownPos: TPoint;
    FOnSectionMove: TSectionMoveEvent;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMLButtonDown); message WM_RBUTTONDOWN;
    procedure SetColor(const Value: TColor);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoSectionMove(FromIndex, ToIndex: integer; var Allow: boolean); virtual;
    function XToSection(x: integer): integer;
    function XToSizePos(x: integer): boolean;
  public
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Sizing(ASection, AWidth: Integer); override;
    procedure Sized(ASection, AWidth: Integer); override;
    property IsSizing: boolean read FIsSizing;
  protected
    procedure Paint; override;
  published
    property AllowMove: Boolean read FAllowMove write FAllowMove;
    property Color: TColor read FColor write SetColor;
    property OnClick: TTLHeaderClickEvent read FOnClick write FOnClick;
    property OnRightClick: TTLHeaderClickEvent read FOnRightClick write FOnRightClick;
    property OnSectionMove: TSectionMoveEvent read FOnSectionMove write FOnSectionMove;
  end;

  THeaderSettings = class(TPersistent)
  private
    FOwner:TTreelist;
    FOldHeight: Integer;
    FHeight: Integer;
    FVisible: Boolean;
    FFont: TFont;
    FAllowResize: boolean;
    FAllowMove: boolean;
    function GetFont: tFont;
    procedure SetFont(const Value: TFont);
    function GetFlat: boolean;
    procedure SetFlat(const Value: boolean);
    function GetAllowResize: boolean;
    procedure SetAllowResize(const Value: boolean);
    function GetColor: tColor;
    procedure SetColor(const Value: tColor);
    function GetHeight: integer;
    procedure SetHeight(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure FontChanged(Sender: TObject);
    procedure SetAllowMove(const Value: boolean);
  public
    constructor Create(aOwner:TTreeList);
    destructor Destroy; override;
  published
    property AllowMove: boolean read FAllowMove write SetAllowMove default False;
    property AllowResize: boolean read GetAllowResize write SetAllowResize default False;
    property Color:tColor read GetColor write SetColor;
    property Flat:boolean read GetFlat write SetFlat default False;
    property Font:tFont read GetFont write SetFont;
    property Height:integer read GetHeight write SetHeight;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TDrawCellEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AColumn: Integer; ANode: TTreeNode;
    var AValue: string; AState: TCustomDrawState; var DefaultDraw: boolean) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TTreeList = class(TTreeview)
  private
    { Private declarations }
    FHeader: TTLHeader;
    FHeaderSettings: THeaderSettings;
    FFlatHeader: boolean;
    FColumnCollection: TColumnCollection;
    FColumnLines: boolean;
    FColumnLineColor: TColor;
    FColumnSpace: integer;
    FOldScrollPos: integer;
    FSeparator: string;
    FItemHeight: integer;
    FOnClick: TTLHeaderClickEvent;
    FOnRightClick: TTLHeaderClickEvent;
    FClipRegion: HRGN;
    FOnHeaderSizeChanged : THeaderSizeChangedEvent;
    FOnDrawCell: TDrawCellEvent;
    FOnHeaderSectionMove: TSectionMoveEvent;
    procedure WMHScroll(var message:TMessage); message WM_HSCROLL;
    procedure WMLButtonDown(var message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var message: TWMLButtonDown); message WM_RBUTTONDOWN;    
    procedure WMPaint(var message: TWMPaint); message WM_PAINT;
    procedure CNNotify(var message: TWMNotify); message CN_NOTIFY;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure SetColumnCollection(const Value: TColumnCollection);
    procedure SetColumnLines(const Value: boolean);

    procedure SectionSize(sender:TObject; ASection, AWidth:integer);
    procedure HeaderClick(sender:TObject; ASection:integer); procedure HeaderRightClick(sender:TObject; ASection:integer);
    function GetColImage(idx:integer):boolean;
    function GetColWidth(idx:integer):integer;
    function GetColFont(idx:integer):TFont;
    function GetAlignment(idx:integer):integer;
    procedure SetSeparator(const Value: string);
    function GetItemHeight: integer;
    procedure SetItemHeight(const Value: integer);
    function GetVisible: boolean;
    procedure SetVisible(const Value: boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetColumnLineColor(const Value: TColor);
  protected
    { Protected declarations }
    function GetVersionNr: Integer; virtual;
    procedure WndProc(var Message:tMessage); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function GetClientRect:TRect; override;
    procedure DoHeaderSectionMove(Sender: TObject; FromIndex, ToIndex: Integer; var Allow: boolean); virtual;
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetNodeColumn(tn: TTreeNode;idx:integer; Value:string);
    function GetNodeColumn(tn: TTreeNode;idx:integer):string;
    procedure Loaded; override;
    procedure UpdateColumns;
    property Header: TTLHeader read FHeader;
    procedure CreateHeader;
    procedure MoveColumn(FromIndex, ToIndex: integer);
    procedure TestFill;
  published
    { Pubished declarations }

    // New introduced properties
    property Columns: TColumnCollection read FColumnCollection write SetColumnCollection;
    property ColumnLines: Boolean read FColumnLines write SetColumnLines default true;
    property ColumnLineColor: TColor read FColumnLineColor write SetColumnLineColor default clSilver;
    property Separator: string read FSeparator write SetSeparator;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property OnDrawNodeCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnHeaderClick: TTLHeaderClickEvent read fOnClick write fOnClick;
    property OnHeaderRightClick: TTLHeaderClickEvent read fOnRightClick write fOnRightClick;
    property OnHeaderSizeChanged: THeaderSizeChangedEvent read FOnHeaderSizeChanged write FOnHeaderSizeChanged;
    property OnHeaderSectionMove: TSectionMoveEvent read FOnHeaderSectionMove write FOnHeaderSectionMove;
    property Visible: Boolean read GetVisible write SetVisible;
    property HeaderSettings: THeaderSettings read FHeaderSettings write FHeaderSettings;
    property Version: string read GetVersion write SetVersion;

    {
    // Default properties
    property Align;
    property Anchors;
    property AutoExpand;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Ctl3D;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HotTrack;
    property Images;
    property Indent;
    property MultiSelect;
    property MultiSelectStyle;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property RowSelect;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    property OnAddition;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnCreateNodeClass;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    }
    { Items must be published after OnGetImageIndex and OnGetSelectedIndex }
   // property Items;
  end;

{$IFDEF VER100}
const
  NM_CUSTOMDRAW            = NM_FIRST-12;

  CDDS_PREPAINT           = $00000001;
  CDDS_POSTPAINT          = $00000002;
  CDDS_PREERASE           = $00000003;
  CDDS_POSTERASE          = $00000004;
  CDDS_ITEM               = $00010000;
  CDDS_ITEMPREPAINT       = CDDS_ITEM or CDDS_PREPAINT;
  CDDS_ITEMPOSTPAINT      = CDDS_ITEM or CDDS_POSTPAINT;
  CDDS_ITEMPREERASE       = CDDS_ITEM or CDDS_PREERASE;
  CDDS_ITEMPOSTERASE      = CDDS_ITEM or CDDS_POSTERASE;
  CDDS_SUBITEM            = $00020000;

  // itemState flags
  CDIS_SELECTED       = $0001;
  CDIS_GRAYED         = $0002;
  CDIS_DISABLED       = $0004;
  CDIS_CHECKED        = $0008;
  CDIS_FOCUS          = $0010;
  CDIS_DEFAULT        = $0020;
  CDIS_HOT            = $0040;
  CDIS_MARKED         = $0080;
  CDIS_INDETERMINATE  = $0100;

  CDRF_DODEFAULT          = $00000000;
  CDRF_NEWFONT            = $00000002;
  CDRF_SKIPDEFAULT        = $00000004;
  CDRF_NOTIFYPOSTPAINT    = $00000010;
  CDRF_NOTIFYITEMDRAW     = $00000020;
  CDRF_NOTIFYSUBITEMDRAW  = $00000020;  // flags are the same, we can distinguish by context
  CDRF_NOTIFYPOSTERASE    = $00000040;

  TVM_GETITEMHEIGHT         = TV_FIRST + 28;
  TVM_SETITEMHEIGHT         = TV_FIRST + 27;

type
  tagNMCUSTOMDRAWINFO = packed record
    hdr: TNMHdr;
    dwDrawStage: DWORD;
    hdc: HDC;
    rc: TRect;
    dwItemSpec: DWORD;  // this is control specific, but it's how to specify an item.  valid only with CDDS_ITEM bit set
    uItemState: UINT;
    lItemlParam: LPARAM;
  end;
  PNMCustomDraw = ^TNMCustomDraw;
  TNMCustomDraw = tagNMCUSTOMDRAWINFO;


  tagNMTVCUSTOMDRAW = packed record
    nmcd: TNMCustomDraw;
    clrText: COLORREF;
    clrTextBk: COLORREF;
    iLevel: Integer;
  end;
  PNMTVCustomDraw = ^TNMTVCustomDraw;
  TNMTVCustomDraw = tagNMTVCUSTOMDRAW;

function TreeView_SetItemHeight(hwnd: HWND; iHeight: Integer): Integer;

function TreeView_GetItemHeight(hwnd: HWND): Integer;

{$ENDIF}


implementation

uses
  Math;

type
  PHeaderSection = ^THeaderSection;
  THeaderSection = record
    FObject: TObject;
    Width: Integer;
    Title: string;                                   
  end;


{ TColumnItem }

constructor TColumnItem.Create(Collection: TCollection);
begin
  inherited;
  FWidth := 50;
  FFont := TFont.Create;
  FFont.Assign((TColumnCollection(Collection).FOwner).Font);
  FFont.OnChange := FontChanged;
  FHeaderImage := -1;
  if (Index = 0) and
     (csDesigning in (TColumnCollection(Collection).FOwner).ComponentState) and
    not (csLoading in (TColumnCollection(Collection).FOwner).ComponentState) then
    FImage := True;
end;

destructor TColumnItem.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TColumnItem.FontChanged(Sender: TObject);
begin
  TColumnCollection(Collection).Update(Self);
end;

procedure TColumnItem.SetWidth(const value:integer);
begin
  FWidth := Value;
  TColumnCollection(Collection).Update(Self);
end;

procedure TColumnItem.SetAlignment(const value:tAlignment);
begin
  FAlignment := Value;
  TColumnCollection(Collection).Update(Self);
end;

procedure TColumnItem.SetColumnHeader(const Value:string);
begin
  FColumnHeader := Value;
  TColumnCollection(Collection).Update(Self);
end;

procedure TColumnItem.SetFont(const value:TFont);
begin
  FFont.Assign(Value);
  TColumnCollection(Collection).Update(Self);
end;


procedure TColumnItem.Assign(source: TPersistent);
begin
  if (Source is TColumnItem) then
  begin
    Width := TColumnItem(Source).Width;
    Alignment := TColumnItem(Source).Alignment;
    Header := TColumnItem(Source).Header;
    HeaderAlign := TColumnItem(Source).HeaderAlign;
    Font.Assign(TColumnItem(Source).Font);
    Image := TColumnItem(Source).Image;
  end;
end;

procedure TColumnItem.SetImage(const Value: boolean);
begin
  FImage := Value;
  TColumnCollection(Collection).Update(Self);
end;

procedure TColumnItem.SetHeaderAlign(const Value: TAlignment);
begin
  FHeaderAlign := Value;
  TColumnCollection(Collection).Update(Self);
end;

procedure TColumnItem.SetHeaderImage(const Value: integer);
begin
  FHeaderImage := Value;
  TColumnCollection(Collection).Update(Self);
end;

{ TColumnCollection }

function TColumnCollection.Add: TColumnItem;
begin
  Result := TColumnItem(inherited Add);
end;

constructor TColumnCollection.Create(aOwner:TTreeList);
begin
  inherited Create(TColumnItem);
  FOwner := AOwner;
end;

function TColumnCollection.GetItem(Index: Integer): TColumnItem;
begin
  Result := TColumnItem(inherited Items[Index]);
end;

function TColumnCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TColumnCollection.Insert(Index: Integer): TColumnItem;
begin
  Result := TColumnItem(inherited Insert(Index));
end;

procedure TColumnCollection.Move(FromIndex, ToIndex: integer);
begin
  if (FromIndex >= 0) and (FromIndex < Count) and (ToIndex >= 0) and (ToIndex < Count) and (ToIndex <> FromIndex) then
  begin
    Items[FromIndex].SetIndex(ToIndex);
  end;
end;

procedure TColumnCollection.SetItem(Index: Integer;
  const Value: TColumnItem);
begin
  inherited Items[Index] := Value;
end;

procedure TColumnCollection.Update(Item:TCollectionItem);
begin
  inherited Update(Item);
  // reflect changes
  FOwner.UpdateColumns;
end;

{ TTLHeader }

procedure TTLHeader.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;

  if Assigned(FOnClick) then
    FOnClick(Self,XToSection(Message.XPos));

  if not XToSizePos(Message.XPos) then
  begin
    FMouseDown := true;
    FMouseDownPos := Point(Message.XPos, Message.YPos);
  end;
end;

procedure TTLHeader.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  inherited;
  FHitTest := SmallPointToPoint(Msg.Pos);
end;

procedure TTLHeader.WMRButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  if Assigned(FOnRightClick) then
    FOnRightClick(self,XToSection(Message.XPos));
end;

function TTLHeader.XToSizePos(x: Integer): boolean;
var
  I,XI,Ofs: integer;
begin
  Result := false;
  XI := 2;
  for I := 0 to Sections.Count - 2 do  // don't count last section
  begin
    Inc(XI, SectionWidth[I]);
    Ofs := XI - (X + 2);
    if Abs(Ofs) < 4 then
    begin
      Result := true;
      Break;
    end;
  end;
end;

procedure TTLHeader.WMSetCursor(var Msg: TWMSetCursor);
var
  I: Integer;
  X: Integer;
  FMouseOffset: Integer;

begin
  inherited;

  FIsSizing := false;

  FHitTest := ScreenToClient(FHitTest);

  X := 2;

  with Msg do
  begin
    if HitTest = HTCLIENT then
      for I := 0 to Sections.Count - 2 do  { don't count last section }
      begin
        Inc(X, SectionWidth[I]);
        FMouseOffset := X - (FHitTest.X + 2);
        if Abs(FMouseOffset) < 4 then
        begin
          FIsSizing := true;
          Break;
        end;
      end;
  end;

  FIsSizing := (AllowResize or (csDesigning in ComponentState)) and (FIsSizing);


  if FIsMoving then
  begin
    SetCursor(Screen.Cursors[crDrag]);
  end;

end;

function TTLHeader.XToSection(x: integer): integer;
var
  xi, i: integer;
begin
  xi := 0;
  i := 0;
  while (xi < x) and (i < Sections.Count) do
  begin
    xi := xi + SectionWidth[i];
    Inc(i);
  end;
  Dec(i);

  Result := i;
end;

procedure TTLHeader.Paint;
var
  I, W: Integer;
  S: string;
  R: TRect;
  PR: TRect;
  halign:word;

begin
  with Canvas do
  begin
    Font := Self.Font;
    Brush.Color := fColor;
    I := 0;
    R := Rect(0, 0, 0, ClientHeight);
    W := 0;
    S := '';

    halign := DT_RIGHT;

    R.Right := R.Right - GetScrollPos((Owner as TTreeList).Handle, SB_HORZ);

    repeat
      with Owner as TTreeList do
      begin
        if (Columns.Count > I) then
          case (Columns.Items[I] as TColumnItem).HeaderAlign of
          taleftJustify:halign := DT_LEFT;
          taRightJustify:halign := DT_RIGHT;
          taCenter:halign := DT_CENTER;
          else
            halign := DT_LEFT;
          end;
      end;

      if I < Sections.Count then
      begin
        W := SectionWidth[i];

        if (i < Sections.Count) then
          s := Sections[i]
        else
          s := Sections[0];

        Inc(I);
      end;

      R.Left := R.Right;

      Inc(R.Right, W);
      if (ClientWidth - R.Right < 2) or (I = Sections.Count) then
        R.Right := ClientWidth;

      pr := r;
      FillRect(r);
      InflateRect(pr,-2,-2);

      with (Owner as TTreeList) do
      begin
        if (Columns.Count > 0) then
        begin
          if (Columns.Count > I - 1) and Assigned(Images) then
          if ((Columns.Items[I - 1] as TColumnItem).HeaderImage >= 0) then
          begin
            Images.Draw(self.Canvas,pr.Left,pr.Top,(Columns.Items[I - 1] as TColumnItem).HeaderImage);
            pr.Left := pr.Left+Images.Width + 2;
          end;
        end;
      end;

      DrawText(canvas.handle,pchar(s),length(s),PR,DT_NOPREFIX or DT_END_ELLIPSIS or halign);
      DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_TOPLEFT);
      DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_BOTTOMRight);
    until R.Right = ClientWidth;
  end;
end;



constructor TTLHeader.Create(aOwner: TComponent);
begin
  inherited;
  FColor := clBtnFace;
end;

procedure TTLHeader.SetColor(const Value: TColor);
begin
  FColor := Value;
  Invalidate;
end;


procedure TTLHeader.Sized(ASection, AWidth: Integer);
begin
  inherited;
  FIsSizing := false;
end;

procedure TTLHeader.Sizing(ASection, AWidth: Integer);
begin
  inherited;
  FIsSizing := true;
end;

procedure TTLHeader.CreateWnd;
begin
  inherited;
end;

destructor TTLHeader.Destroy;
begin
  inherited;
end;

procedure TTLHeader.DestroyWnd;
begin
  inherited;
end;

procedure TTLHeader.DoSectionMove(FromIndex, ToIndex: integer;
  var Allow: boolean);
begin
  if Assigned(OnSectionMove) then
    OnSectionMove(Self, FromIndex, ToIndex, Allow);
end;

procedure TTLHeader.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FMouseDown and not FIsSizing and not FIsMoving then
  begin
    if (Abs(FMouseDownPos.X - X ) > 4) and AllowMove then
    begin
      FOldCursor := Cursor;
      FIsMoving := true;
      Cursor := crDrag;
      SetCursor(Screen.Cursors[crDrag]);
      SetCapture(Handle);
    end;
  end;
end;

procedure TTLHeader.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Allow: boolean;
  FromIndex, ToIndex: integer;
begin
  inherited;

  FMouseDown := false;

  if FIsMoving then
  begin
    ReleaseCapture;
    Cursor := FOldCursor;
    FromIndex := Max(0,XToSection(FMouseDownPos.X));
    ToIndex := Max(0,XToSection(X));
    Allow := true;
    if (FromIndex <> ToIndex) then
      DoSectionMove(FromIndex, ToIndex, Allow);
  end;

  FIsMoving := false;
end;

{ TTreeList }

constructor TTreeList.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  FOnHeaderSizeChanged := nil;
  FHeader := nil;
  FColumnCollection := TColumnCollection.Create(self);
  FHeaderSettings := THeaderSettings.Create(self);
  FHeaderSettings.AllowResize := false;
  FSeparator := ';';
  FItemHeight := 16;
  FColumnLines := true;
  FColumnLineColor := clSilver;
  FColumnSpace := 4;
  FOldScrollPos := -1;
end;

procedure TTreeList.CreateHeader;
const
  hdr: array[boolean] of TBorderStyle = (bsSingle,bsNone);
begin
  if not Assigned(FHeader) then
  begin
    FHeader := TTLHeader.Create(self);
    FHeader.Parent := Parent;
    FHeader.top := Top - 16;
    FHeader.left := Left;
    FHeader.Width := Width {- 1};
    FHeader.Height := 18;
    Fheader.borderstyle := hdr[FFlatHeader];
    FHeader.OnSized := SectionSize;
    FHeader.OnClick := HeaderClick;
    FHeader.OnRightClick := HeaderRightClick;
    FHeader.OnSectionMove := DoHeaderSectionMove;

    if (Top < 16) then
      Top := 16;
    UpdateColumns;

    FHeader.Visible := self.Visible and FHeaderSettings.Visible;
    if self.Visible and FHeaderSettings.Visible then
    begin
      ShowWindow(FHeader.Handle, SW_SHOW);
    end
    else
      ShowWindow(FHeader.Handle, SW_HIDE);
  end;
  end;

procedure TTreeList.CreateWnd;
var
  oldheight: integer;
begin
  inherited;
  CreateHeader;

  oldheight := FItemHeight;
  FItemHeight := 0; // force an update of ItemHeight always
  ItemHeight := oldheight;
end;


procedure TTreeList.SectionSize(sender:TObject; ASection, AWidth:integer);
var
  FIndent: Integer;
begin
  FIndent := TreeView_GetIndent(self.Handle);
  if Assigned(Images) then
    FIndent := FIndent + Images.Width;

  if (ASection = 0) and (AWidth < FIndent) then
  begin
    AWidth := FIndent;
    if Assigned(FHeader) then
      FHeader.SectionWidth[ASection] := FIndent;
  end;

  if (ASection >= 0) and (ASection < FColumnCollection.Count) then
  TColumnItem(fColumnCollection.Items[ASection]).FWidth := AWidth;
  
  Invalidate;
  if ((csDesigning in ComponentState) or (csDestroying in ComponentState)) then 
    Exit;

  if not (csDestroying in ComponentState) then
  begin
    if Assigned (FOnHeaderSizeChanged) and Assigned (FHeader) and Assigned (FColumnCollection) then
      FOnHeaderSizeChanged (Self, ASection);
  end;
end;

procedure TTreeList.HeaderClick(sender:TObject; ASection:integer);
begin
  if Assigned(OnHeaderClick) then
    OnHeaderClick(self,ASection);
end;

procedure TTreeList.HeaderRightClick(sender:TObject; ASection:integer);
begin
  if Assigned(OnHeaderRightClick) then
    OnHeaderRightClick(self,ASection);
end;

procedure TTreeList.DestroyWnd;
begin
  inherited;
end;

procedure TTreeList.DoHeaderSectionMove(Sender: TObject; FromIndex,
  ToIndex: Integer; var Allow: boolean);
begin
  if Assigned(FOnHeaderSectionMove) then
    FOnHeaderSectionMove(Self, FromIndex, ToIndex, Allow);

  if Allow then
  begin
    MoveColumn(FromIndex, ToIndex);
  end;
end;

destructor TTreeList.Destroy;
begin
  FHeaderSettings.Free;
  FColumnCollection.Free;
  inherited;
end;

procedure TTreeList.SetColumnLineColor(const Value: TColor);
begin
  if (FColumnLineColor <> Value) then
  begin
    FColumnLineColor := Value;
    Invalidate;
  end;
end;

procedure TTreeList.SetColumnLines(const value :boolean);
begin
  if (FColumnLines <> Value) then
  begin
    FColumnLines := Value;
    if FColumnLines then
      FColumnSpace := 4
    else
      FColumnSpace := 2;
    Invalidate;
  end;
end;

procedure TTreeList.SetColumnCollection(const value :TColumnCollection);
begin
  FColumnCollection.Assign(Value);
end;

procedure TTreeList.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  HeaderHeight: Integer;
begin
  if not Assigned(FHeaderSettings) then
  begin
    inherited;
    Exit;
  end;

  if not  FHeaderSettings.Visible then
    HeaderHeight := 2
  else
    HeaderHeight := FHeaderSettings.FHeight;

  if Align in [alClient, alTop, alRight, alLeft] then
  begin
    inherited SetBounds(ALeft,ATop + HeaderHeight - 2,AWidth,AHeight - HeaderHeight + 2);

    if not FHeaderSettings.Visible then
      Exit;

    if Assigned(FHeader) then
    begin
      FHeader.Top := ATop;
      FHeader.Left := ALeft;
      FHeader.Width := AWidth;
      FHeader.Height := FHeaderSettings.Height;
    end;
  end
  else
  begin
    inherited SetBounds(ALeft,ATop,AWidth,AHeight);

    if not FHeaderSettings.Visible then
      Exit;

    if Assigned(FHeader) then
    begin
      FHeader.Top := ATop - (FHeaderSettings.Height - 2);
      FHeader.Left := ALeft;
      FHeader.Width := AWidth;
      FHeader.Height := FHeaderSettings.Height;
    end;
  end;
end;


procedure TTreeList.UpdateColumns;
var
  i,cw: Integer;
  s:string;
begin
  if Assigned(FHeader) then
  begin
    FHeader.Sections.Clear;
    for i := 1 to FColumnCollection.Count do
    begin
      s := TColumnItem(FColumnCollection.Items[i - 1]).Header;

      FHeader.Sections.Add(s);

      cw := TColumnItem(FColumnCollection.Items[i - 1]).Width;

      if (i = 1) then
      begin
        if (BorderStyle = bsNone) and (HeaderSettings.Flat = false) then
          dec(cw);

        if (HeaderSettings.Flat) and (BorderStyle = bsSingle) then
          inc(cw);
      end;

      FHeader.SectionWidth[i-1] := cw;
    end;
    Invalidate;
  end;
end;

function TTreeList.GetColImage(idx:Integer):boolean;
begin
  if idx >= FColumnCollection.Count then
    Result := False
  else
    Result := TColumnItem(FColumnCollection.Items[idx]).FImage;
end;

function TTreeList.GetColWidth(idx:integer):integer;
begin
  if idx >= FColumnCollection.Count then
    Result := self.Width
  else
    Result := TColumnItem(FColumnCollection.Items[idx]).FWidth;
end;

function TTreeList.GetColFont(idx:integer):TFont;
begin
  if idx >= FColumnCollection.Count then
    Result := self.Font
  else
    Result := TColumnItem(FColumnCollection.Items[idx]).FFont;
end;

function TTreeList.GetAlignment(idx:integer):integer;
begin
  if idx >= FColumnCollection.Count then
    Result := DT_LEFT
  else
    case TColumnItem(FColumnCollection.Items[idx]).FAlignment of
    taLeftJustify:Result := DT_LEFT;
    taCenter:Result := DT_CENTER;
    taRightJustify:Result:=DT_RIGHT
    else
      Result := DT_LEFT;
  end;
end;

procedure TTreeList.CMVisibleChanged(var Message: TMessage);
begin
  if Assigned(FHeader) then
    FHeader.Visible := self.Visible and FHeaderSettings.Visible;
end;

procedure TTreeList.CNNotify(var message: TWMNotify);
var
  TVcd:TNMTVCustomDraw;
  TVdi:TTVDISPINFO;
  ACanvas: TCanvas;
  s,su:string;
  tn:ttreenode;
  r,nr:trect;
  fIndent,fIdx,fImgWidth:integer;
  defdraw: boolean;
  scpos: integer;
  paintimg: boolean;
  TmpItem: TTVItem;
  code: longbool;
  cnt,cntvis: integer;

  function GetNodeFromItem(const Item: TTVItem): TTreeNode;
  begin
    Result := nil;
    if Items <> nil then
    with Item do
      if (state and TVIF_PARAM) <> 0 then
        Result := Pointer(lParam)
      else
        Result := Items.GetNode(hItem);
  end;

begin
  if message.NMHdr^.code = TVN_GETDISPINFO then
  begin
    TVDi := PTVDispInfo(pointer(message.nmhdr))^;
    if (tvdi.item.mask and TVIF_TEXT = TVIF_TEXT) then
    begin
      tn := Items.GetNode(tvdi.item.hitem);
      s := tn.Text;
      StrPLCopy(tvdi.item.pszText,s,255);
      tvdi.Item.Mask := tvdi.item.mask or TVIF_DI_SETITEM;
      message.Result := 0;
      Exit;
    end;
  end;

  if message.NMHdr^.code = NM_CUSTOMDRAW then
  begin
    FIndent := TreeView_GetIndent(self.handle);
    TVcd := PNMTVCustomDraw(Pointer(message.NMHdr))^;
    case TVcd.nmcd.dwDrawStage of
    CDDS_PREPAINT:
      begin
        if Assigned(OnAdvancedCustomDraw) then
          OnAdvancedCustomDraw(self, TVcd.nmcd.rc, TCustomDrawStage(message.NMHdr^.code), defdraw);

        message.Result := CDRF_NOTIFYITEMDRAW or CDRF_NOTIFYPOSTPAINT;
      end;
    CDDS_ITEMPREPAINT:
      begin
        defdraw := true;

        FillChar(TmpItem, SizeOf(TmpItem), 0);
        with PNMCustomDraw(message.NMHdr)^ do
          TmpItem.hItem := HTREEITEM(dwItemSpec);

        tn := GetNodeFromItem(TmpItem);

        if (tn = nil) then
          Exit;

        //tn := Items.GetNode(tvdi.item.hitem);

        paintimg := true;
        if Assigned(OnAdvancedCustomDrawItem) then
          OnAdvancedCustomDrawItem(self, tn, TCustomDrawState(Word(TVcd.nmcd.uItemState)), TCustomDrawStage(message.NMHdr^.code), paintimg, defdraw);

        if defdraw then
        begin

          if  (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) then
          begin
            TVcd.nmcd.uItemState := TVcd.nmcd.uItemState and (not CDIS_SELECTED);
            SetTextColor(TVcd.nmcd.hdc,ColorToRGB(Color));
            SetBkColor(TVcd.nmcd.hdc,ColorToRGB(Color));
            TVcd.clrTextBk := ColorToRgb(Color);
            TVcd.clrText := ColorToRgb(Color);
          end
          else
          begin
            SetTextColor(TVcd.nmcd.hdc,ColorToRGB(Color));
            SetBkColor(TVcd.nmcd.hdc,ColorToRGB(Color));
          end;
        end;

          // use a clip region here to avoid drawing of tree nodes in 2nd column
        if not DoubleBuffered then
        begin
          FClipRegion := CreateRectRgn(TVcd.nmcd.rc.Left,TVcd.nmcd.rc.Top,TVcd.nmcd.rc.Left + GetColWidth(0) - 1,TVcd.nmcd.rc.Bottom);
          SelectClipRgn(TVcd.nmcd.hdc,FClipRegion);
        end;

        message.Result := CDRF_NOTIFYPOSTPAINT;
      end;
    CDDS_ITEMPOSTPAINT:
      begin
        if not DoubleBuffered then
        begin
          SelectClipRgn(TVcd.nmcd.hdc,0);
          DeleteObject(FClipRegion);
        end;

        defdraw := true;

        ACanvas := TCanvas.Create;
        ACanvas.Handle := TVcd.nmcd.HDC;

        FillChar(TmpItem, SizeOf(TmpItem), 0);
        with PNMCustomDraw(message.NMHdr)^ do
        begin
          TmpItem.hItem := HTREEITEM(dwItemSpec);

        tn := GetNodeFromItem(TmpItem);

          code := true;
          if Assigned(tn) then
            TreeView_GetItemRect(handle, HTREEITEM(dwItemSpec), nr, code);
        end;

        if (tn = nil) then
          Exit;

        // tn := Items.GetNode(tvdi.item.hitem);

        paintimg := true;
        if Assigned(OnAdvancedCustomDrawItem) then
          OnAdvancedCustomDrawItem(self, tn, TCustomDrawState(word(TVcd.nmcd.uItemState)), TCustomDrawStage(message.NMHdr^.code), paintimg, defdraw);

        scpos := GetScrollPos(handle,SB_HORZ);

        if defdraw then
        begin
          tn := Items.GetNode(HTREEITEM(TVcd.nmcd.dwitemSpec));

          if Assigned(OnGetImageIndex) then
            OnGetImageIndex(Self, tn);

          // get left pos from tree level
          TVcd.nmcd.rc.Left := TVcd.nmcd.rc.Left + FIndent * (tn.Level + 1) - scpos;

          if not ShowRoot then
            TVcd.nmcd.rc.Left := TVcd.nmcd.rc.Left - FIndent;

          // paint image in first column
          FImgWidth := 0;

          ACanvas.Brush.Color := Color;
          ACanvas.Pen.Color := Color;

          if Assigned(OnCustomDrawItem) then
          begin
            Canvas.Font.Assign(ACanvas.Font);
            Canvas.Pen.Assign(ACanvas.Pen);
            Canvas.Brush.Assign(ACanvas.Brush);
            OnCustomDrawItem(Self, tn, TCustomDrawState(word(TVcd.nmcd.uItemState)), defdraw);
            ACanvas.Font.Assign(Canvas.Font);
            ACanvas.Pen.Assign(Canvas.Pen);
            ACanvas.Brush.Assign(Canvas.Brush);
          end;

          if Assigned(Images) and GetColImage(0) and
            ((tn.ImageIndex >= 0) or (tn.SelectedIndex >= 0)) then
          begin
            FImgWidth := Images.Width;

            ACanvas.Rectangle(TVcd.nmcd.rc.left,TVcd.nmcd.rc.top,TVcd.nmcd.rc.left + FImgWidth,TVcd.nmcd.rc.bottom);

            if (TVcd.nmcd.rc.left + FImgWidth < GetColWidth(0)) then
            begin
              if (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) and
                (tn.SelectedIndex >= 0) then
                Images.Draw(ACanvas, TVcd.nmcd.rc.left, nr.Top {TVcd.nmcd.rc.top}, tn.SelectedIndex)
              else
                if (tn.ImageIndex >= 0) then
                  Images.Draw(ACanvas, TVcd.nmcd.rc.left, nr.Top {TVcd.nmcd.rc.top}, tn.ImageIndex);
            end;

            if tn.OverlayIndex >= 0 then
              Images.Draw(ACanvas,TVcd.nmcd.rc.left,nr.Top {TVcd.nmcd.rc.top},tn.OverlayIndex);
          end;

          TVcd.nmcd.rc.Left := TVcd.nmcd.rc.Left + FImgWidth;

          r := TVcd.nmcd.rc;

          fIdx := ACanvas.TextHeight('gh');
          if (fIdx < r.Bottom - r.Top) then
            r.Top := r.Top + ((r.Bottom - r.Top - FIdx) shr 1);

          if  (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) then
          begin
            ACanvas.Brush.Color := clHighLight;
            ACanvas.Pen.Color := clHighLight;
            with TVcd.nmcd.rc do
              ACanvas.Rectangle(left,top,right,bottom);

            ACanvas.Font.Color := clHighLightText;

            if (TVcd.nmcd.uItemState and CDIS_FOCUS = CDIS_FOCUS) then
            begin
              ACanvas.Pen.Color := Color;
              ACanvas.Brush.Color := Color;
              ACanvas.DrawFocusRect(TVcd.nmcd.rc);
            end;
            TVcd.nmcd.rc := r;
            TVcd.nmcd.rc.Left := TVcd.nmcd.rc.Left + 4;
          end
          else
          begin
            with TVcd.nmcd.rc do
            begin
              ACanvas.Rectangle(Left, Top, Right, Bottom);
            end;
          end;

          TVcd.nmcd.rc := r;

          if  (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) then
          begin
            ACanvas.Brush.color := clHighLight;
            ACanvas.Pen.color := clHighLight;
          end;

          s := tn.text;
          fIdx := 0;

          SetBkMode(TVcd.nmcd.hdc,TRANSPARENT);

          repeat
            ACanvas.Font.Assign(GetColFont(fIdx));

            if  (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) then
            begin
              ACanvas.Font.Color := clHighLightText;
            end;

            if (fIdx = 0) then
            begin
              r.Right := GetColWidth(fIdx);
              r.Right := r.Right - scpos;
            end
            else
              r.Right := r.left + GetColWidth(fIdx);

            if fIdx = FColumnCollection.Count - 1 then
            begin
              r.Right := Width;

              cntvis := SendMessage(Handle, TVM_GETVISIBLECOUNT,0,0);
              cnt := SendMessage(Handle, TVM_GETCOUNT,0,0);

              if cntvis < cnt then
              begin
                r.Right := r.Right - GetSystemMetrics(SM_CXVSCROLL) - 2;
              end;
            end;

            if (pos(FSeparator,s) > 0) then
            begin
              su := Copy(s,1,pos(FSeparator,s)-1);
              System.Delete(s,1,pos(FSeparator,s) + Length(FSeparator) - 1);
            end
            else
            begin
              su := s;
              s := '';
              if (fIdx >= FColumnCollection.Count) then
                r.right := TVcd.nmcd.rc.right;
            end;

            r.right := r.right - FColumnSpace;

            if Assigned(Images) and GetColImage(fIdx) and (tn.ImageIndex >= 0) and (fIdx>0) then
            begin
              FImgWidth := Images.Width;
              if (fImgWidth < GetColWidth(fIdx)) then
                Images.draw(ACanvas,r.left,TVcd.nmcd.rc.top,tn.ImageIndex);

              r.left := r.left + fImgWidth + 2;
            end;

            if Assigned(Images) and (fidx = 0) and GetColImage(fIdx) then
            begin
              r.left := r.left + 2;
            end;

            defdraw := true;
            if Assigned(FOnDrawCell) then
              FOnDrawCell(Self, ACanvas, r, fidx, tn, su, TCustomDrawState(word(TVcd.nmcd.uItemState)), defdraw);

            if defdraw then
            begin
              DrawText(ACanvas.Handle, PChar(su), Length(su),r,DT_NOPREFIX or DT_END_ELLIPSIS or GetAlignment(fIdx));
            end;

            r.right := r.right + FColumnSpace;
            r.left := r.right;

            inc(fIdx);
          until (length(s) = 0);
         end;

          ACanvas.Free;
        end;
      else
        message.Result := 0;
    end;
  end
  else
    inherited;
end;


procedure TTreeList.SetSeparator(const Value: string);
begin
  FSeparator := Value;
  Self.Invalidate;
end;

procedure TTreeList.WMLButtonDown(var message: TWMLButtonDown);
var
  Node : TTreeNode;
begin
  Node := GetNodeAt(message.XPos,message.YPos);

  if not (csDesigning in ComponentState) then
    inherited
  else
    Exit;

  if Assigned(Node) then
    Node.selected := True;
end;

procedure TTreeList.WMRButtonDown(var message: TWMLButtonDown);
var
 Node : TTreeNode;
begin
  if RightClickSelect then
  begin
    Node := GetNodeAt(message.XPos,message.YPos);
    if Assigned(Node) then
    begin
      Node.selected := True;
    end;
  end;

  if not (csDesigning in ComponentState) then
    inherited
  else
    Exit;

end;


procedure TTreeList.WMHScroll(var message:TMessage);
begin
  inherited;

  if (FOldScrollPos <> GetScrollPos(handle,SB_HORZ)) then
  begin
    Invalidate;
    FHeader.Invalidate;
    FOldScrollPos := GetScrollPos(handle,SB_HORZ);
  end;
end;

procedure TTreeList.WMPaint(var message: TWMPaint);
var
  Canvas: TCanvas;
  i,xp,sp: Integer;
begin
  inherited;

  if FColumnLines and not (csDestroying in ComponentState) then
  begin
    Canvas := TCanvas.Create;
    Canvas.Handle := GetDC(Handle);
    xp := 0;
    Canvas.Pen.Color := clSilver;
    sp := GetScrollPos(Handle,SB_HORZ);

    for i := 1 to FColumnCollection.Count - 1 do
    begin
      xp := xp + TColumnItem(FColumnCollection.Items[i - 1]).Width;
      Canvas.MoveTo(xp - 2 - sp,0);
      Canvas.Lineto(xp - 2 - sp,Height);
    end;
    ReleaseDC(Handle,Canvas.Handle);
    Canvas.free;
  end;
end;

function TTreeList.GetItemHeight: integer;
begin
  Result := TreeView_GetItemHeight(Handle);
end;

procedure TTreeList.SetItemHeight(const Value: integer);
begin
  if (Value <> FItemHeight) then
  begin
    FItemHeight := Value;
    TreeView_SetItemHeight(Handle,FItemHeight);
  end;
end;

{$IFDEF VER100}
function TreeView_SetItemHeight(hwnd: HWND; iHeight: Integer): Integer;
begin
  Result := SendMessage(hwnd, TVM_SETITEMHEIGHT, iHeight, 0);
end;

function TreeView_GetItemHeight(hwnd: HWND): Integer;
begin
  Result := SendMessage(hwnd, TVM_GETITEMHEIGHT, 0, 0);
end;
{$ENDIF}

function TTreeList.GetVisible: boolean;
begin
  Result := inherited Visible;
end;

procedure TTreeList.SetVisible(const Value: boolean);
begin
  inherited Visible := Value;
  if Assigned(FHeader) then
    FHeader.Visible := Value and FHeaderSettings.Visible;
end;

procedure TTreeList.TestFill;
var
  i: integer;
begin
  Columns.Clear;
  Items.Clear;

  Columns.Add.Header := 'Column 1';
  Columns.Add.Header := 'Column 2';
  Columns.Add.Header := 'Column 3';

  for i := 1 to 10 do
  begin
    Items.Add(nil,'item '+inttostr(i)+';'+chr(ord('A')-1+i)+';'+inttostr(i));
  end;
end;

function TTreeList.GetClientRect: TRect;
var
  r: TRect;
begin
  r := inherited GetClientRect;
  r.bottom := r.bottom + FHeaderSettings.Height;
  Result := r;
end;

function VarPos(su,s:string;var vp: Integer):Integer;
begin
  vp := Pos(su,s);
  Result := vp;
end;

function TTreeList.GetNodeColumn(tn: TTreeNode; idx: integer):string;
var
  s: string;
  i,vp: Integer;

begin
  Result := '';
  if Assigned(tn) then
    s := tn.Text
  else
    Exit;

  i := 0;
  while (i <= idx) and (s <> '') do
  begin
    if VarPos(Separator,s,vp) > 0 then
    begin
     if idx = i then
       Result := Copy(s,1,vp-1);

     System.Delete(s,1,vp);
     inc(i);
    end
    else
    begin
      if i = idx then
        Result := s;
      s := '';
    end;
  end;

end;

procedure TTreeList.SetNodeColumn(tn: TTreeNode; idx: integer; value: string);
var
  s,su: string;
  i,vp: Integer;

begin
  if Assigned(tn) then
    s := tn.Text
  else
    Exit;

  su := s;
  for i := 1 to Columns.Count do
  begin
    if VarPos(Separator,su,vp) > 0 then
      system.Delete(su,1,vp)
    else
      s := s + Separator;
  end;

  i := 0;
  su := '';
  while (i <= idx) and (s <> '') do
  begin
    if VarPos(Separator,s,vp) > 0 then
    begin
      if i < idx then
        su := su + copy(s,1,vp);
      if i = idx then
        su := su + Value + Separator;
      System.Delete(s,1,vp);
      Inc(i);
    end
    else
    begin
      s := '';
      if i = idx then
        su := su + Value;
      Inc(i);
    end;
  end;
  
  su := su + s;
  tn.Text := su;
end;

procedure TTreeList.WndProc(var Message: tMessage);
begin
  inherited;
end;

procedure TTreeList.Loaded;
begin
  inherited;
  UpdateColumns;
  if Assigned(FHeader) then
  begin
    FHeader.Font.Assign(FHeaderSettings.Font);
    FHeader.AllowResize := FHeaderSettings.AllowResize;
    FHeader.Visible := self.Visible and FHeaderSettings.Visible;
    if not FHeader.Visible then
      ShowWindow(FHeader.Handle, SW_HIDE);
  end;
end;


procedure Split(s:string; sl: TStringList; delimiter: string);
var
  vp: integer;
begin
  sl.Clear;
  while VarPos(delimiter, s, vp) > 0 do
  begin
    sl.Add(copy(s, 1, vp - 1));
    delete(s, 1, vp);
  end;

  if s <> '' then
    sl.Add(s)
end;

function Compose(sl: TStringList; delimiter: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to sl.Count - 1 do
  begin
    Result := Result + sl.Strings[i];
    if i < sl.Count - 1 then
      Result := Result + delimiter;
  end;
end;

procedure TTreeList.MoveColumn(FromIndex, ToIndex: integer);
var
  tn: TTreeNode;
  sl: TStringList;

begin
  if (FromIndex < 0) or (FromIndex >= Columns.Count) then
    raise Exception.Create('Invalid FromIndex column index');

  if (ToIndex < 0) or (ToIndex >= Columns.Count) then
    raise Exception.Create('Invalid ToIndex column index');

  Columns.Move(FromIndex, ToIndex);

  tn := Items[0];

  sl := TStringList.Create;

  while Assigned(tn) do
  begin
    Split(tn.Text, sl, Separator);

    if (FromIndex < sl.Count) and (ToIndex < sl.Count) then
      sl.Move(FromIndex, ToIndex);

    tn.Text := Compose(sl, Separator);
    tn := tn.GetNext;
  end;
  sl.Free;
end;

function TTreeList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TTreeList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TTreeList.SetVersion(const Value: string);
begin

end;

{ THeaderSettings }
constructor THeaderSettings.Create(aOwner: TTreeList);
begin
  inherited Create;
  FOwner := AOwner;
  FHeight := 18;
  FOldHeight := FHeight;
  FVisible := True;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
end;

destructor THeaderSettings.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure THeaderSettings.FontChanged(Sender: TObject);
begin
  if assigned (FOwner) then
  begin
    if assigned(Fowner.FHeader) then
      FOwner.FHeader.Font.Assign(FFont);
  end;
end;

function THeaderSettings.GetAllowResize: Boolean;
begin
  Result := FAllowResize;
end;

function THeaderSettings.GetColor: TColor;
begin
  Result := clBtnFace;
  if Assigned(FOwner.FHeader) then
    Result := FOwner.FHeader.Color;
end;

function THeaderSettings.GetFlat: Boolean;
begin
  if Assigned(FOwner.FHeader) then
    Result := (FOwner.FHeader.BorderStyle = bsNone)
  else
    Result := False;
end;

function THeaderSettings.GetFont: TFont;
begin
  Result := FFont;
end;

function THeaderSettings.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure THeaderSettings.SetAllowMove(const Value: boolean);
begin
  if (FAllowMove <> Value) then
  begin
    FAllowMove := Value;
    if Assigned(FOwner.Header) then
      FOwner.Header.AllowMove := Value;
  end;
end;

procedure THeaderSettings.SetAllowResize(const Value: Boolean);
begin
  if (FAllowResize <> Value) then
  begin
    FAllowResize := value;
    if Assigned(FOwner.FHeader) then
      FOwner.FHeader.AllowResize := Value;
  end;
end;

procedure THeaderSettings.SetColor(const Value: TColor);
begin
  if Assigned(FOwner.FHeader) then
    FOwner.FHeader.Color := Value;
end;

procedure THeaderSettings.SetFlat(const Value: Boolean);
begin
  if Assigned(FOwner.FHeader) then
  begin
    if Value then
      FOwner.FHeader.BorderStyle := bsNone
    else
      FOwner.FHeader.BorderStyle := bsSingle;
  end;
end;

procedure THeaderSettings.SetFont(const Value: TFont);
begin
(*
  if Assigned(FOwner.FHeader) then
    FOwner.FHeader.Font.Assign(Value);
*)
  FFont.Assign(Value);
end;

procedure THeaderSettings.SetHeight(const Value: Integer);
begin
  if Assigned(FOwner.FHeader) then
    FOwner.FHeader.Height := Value;
  FHeight := Value;
  FOldHeight := FHeight;
  FOwner.Top := FOwner.Top;
end;

procedure THeaderSettings.SetVisible(const Value: Boolean);
begin
  FVisible := Value;

  if Assigned(FOwner.FHeader) then
  begin
    if (csDesigning in FOwner.ComponentState) then
    begin
      if Value then
        Height := FOldHeight
      else
      begin
        FOldHeight := Height;
        FOwner.FHeader.Height := 0;
      end;
    end;

    FOwner.FHeader.Visible := Value;
  end;
end;

end.

