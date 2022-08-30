{**************************************************************************}
{ TParamCheckList component                                                )
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2000 - 2012                                       }
{            Email : info@tmssoftware.com                                  }
{            Website : http://www.tmssoftware.com/                         }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

{$I TMSDEFS.INC}
{$DEFINE REMOVESTRIP}
{$DEFINE REMOVEDRAW}
{$DEFINE PARAMS}

unit paramchklist;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, forms,
  StdCtrls, Dialogs, Menus, Spin, ComCtrls, ParHTML, PictureContainer,
  ParXPVS, Contnrs, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 3; // Release nr.
  BLD_VER = 4; // Build nr.

  // version history
  // 1.3.0.1 : improved mask editor property handling
  // 1.3.0.2 : improved disabled checkbox drawing
  // 1.3.0.3 : fixed issue with empty date/time handling
  // 1.3.0.4 : fixed issue with popup menu selection
  // 1.3.1.0 : improved positioning of directory select dialog on multimonitor machines
  // 1.3.2.0 : New : property WordWrap added
  // 1.3.2.1 : Fixed checkbox painting issue on Delphi 2007
  // 1.3.3.0 : Fixed issue with spinedit
  // 1.3.3.1 : Fixed painting issue with DoubleBuffered
  // 1.3.3.2 : Improved : Improved : Date format compatibility for inplace date picker
  // 1.3.3.3 : Fixed : Issue with popup menu handling on old Delphi versions
  // 1.3.3.4 : Fixed : Memory leak


type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TParamClickEvent = procedure (Sender:TObject;idx:integer; href:string;var value:string) of object;
  TParamPopupEvent = procedure (Sender:TObject;idx:integer; href:string;values:TStringlist;var DoPopup:boolean) of object;
  TParamSelectEvent = procedure (Sender:TObject;idx:integer; href,value:string) of object;
  TParamChangedEvent = procedure (Sender:TObject;idx:integer; href,oldvalue,newvalue:string) of object;
  TParamHintEvent = procedure (Sender:TObject; href:string; var hintvalue:string; var showhint:boolean) of object;

  TParamCustomEditEvent = procedure(Sender: TObject; idx: Integer; href, value, props: string; EditRect: TRect) of object;

  TParamListBoxEditEvent = procedure (Sender:TObject;idx: Integer; href: string;var value: string) of object;

  TParamItemControlEvent = procedure(Sender: TObject; X,Y: Integer;
    Item: Integer; ControlID, ControlType, ControlValue:string) of object;

  TParamControlHintEvent = procedure(Sender: TObject; ControlID: string; var Hint: string; var CanShow: Boolean) of object;

  TParamCheckList = class;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TParamCheckList = class(TCustomListBox)
  private
    FOldParam:string;
    FOldCursor:TCursor;
    FParamColor:TColor;
    FAllowGrayed: Boolean;
    FDuplicates: Boolean;
    FFlat: Boolean;
    FFocusLink: Integer;
    FFocusItem: Integer;
    FNumHyperLinks: Integer;
    FStandardItemHeight: Integer;
    FOnCheckClick: TNotifyEvent;
    FOnParamClick: TParamClickEvent;
    FOnParamPopup: TParamPopupEvent;
    FOnParamList:TParamPopupEvent;
    FOnParamSelect:TParamSelectEvent;
    FOnParamChanged:TParamChangedEvent;
    FOnParamExit: TParamSelectEvent;
    FOnParamEnter: TParamSelectEvent;
    FOnParamHint: TParamHintEvent;
    FSaveStates: TList;
    FShowSelection: Boolean;
    FDuplicateList : TStringList;
    FHover: Boolean;
    FHoverIdx: Integer;
    FHoverHyperLink: Integer;
    FHoverColor: TColor;
    FHoverFontColor: TColor;
    FCurrHoverRect: TRect;
    FParamPopup: TPopupMenu;
    FImages: TImageList;
    FParamList:TPopupListBox;
    FParamDatePicker: TPopupDatePicker;
    FParamSpinEdit: TPopupSpinEdit;
    FParamEdit: TPopupEdit;
    FParamMaskEdit: TPopupMaskEdit;
    FShadowColor:TColor;
    FShadowOffset:integer;
    FUpdateCount:integer;
    FUpdateIndex:integer;
    FParamHint: Boolean;
    FOnParamPrepare: TParamClickEvent;
    FParamListSorted: Boolean;
    FContainer: TPictureContainer;
    FImageCache:  THTMLPictureCache;
    FMouseDown: Boolean;
    FCurrCtrlID: string;
    FCurrCtrlRect: TRect;
    FCurrCtrlDown: TRect;
    FOnControlClick: TParamItemControlEvent;
    FOnControlHint: TParamControlHintEvent;
    FEditAutoSize: Boolean;
    FLineSpacing: Integer;
    FOnParamEditStart: TParamListBoxEditEvent;
    FOnParamEditDone: TParamListBoxEditEvent;
    FEmptyParam: string;
    FOldAnchor: string;
    FOnParamQuery: TParamListBoxEditEvent;
    FSelectionColor: TColor;
    FSelectionFontColor: TColor;
    FEditValue: string;
    FEditPos: TPoint;
    FIsEditing: Boolean;
    FOnParamCustomEdit: TParamCustomEditEvent;
    FAdvanceOnReturn: Boolean;
    FWordWrap: Boolean;
    WrapperList: TObjectList;
    procedure ResetItemHeight;
    procedure DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
    procedure SetChecked(Index: Integer; Checked: Boolean);
    function GetChecked(Index: Integer): Boolean;
    procedure SetState(Index: Integer; AState: TCheckBoxState);
    function GetState(Index: Integer): TCheckBoxState;
    procedure SetIndent(Index: Integer; AIndent: integer);
    function GetIndent(Index: Integer): integer;
    procedure ToggleClickCheck(Index: Integer);
    procedure InvalidateCheck(Index: Integer);
    procedure InvalidateItem(Index: Integer);
    function CreateWrapper(Index: Integer): TObject;
    function ExtractWrapper(Index: Integer): TObject;
    function GetWrapper(Index: Integer): TObject;
    function HaveWrapper(Index: Integer): Boolean;
    procedure SetFlat(Value: Boolean);
    procedure SetDuplicates(Value: Boolean);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMHintShow(Var Msg: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(Var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMDestroy(var Msg : TWMDestroy);message WM_DESTROY;
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure WMKillFocus(var Message:TMessage); message WM_KILLFOCUS;
    procedure WMVScroll(var Message: TMessage); message WM_VSCROLL;
    function GetItemEnabled(Index: Integer): Boolean;
    procedure SetItemEnabled(Index: Integer; const Value: Boolean);
    function GetComment(Index: Integer): Boolean;
    procedure SetComment(Index: Integer; AComment: Boolean);
    function GetSelectCount: Integer;
    function GetSelected(Index: Integer): string;
    procedure QuickSortItems(left,right:integer);
    function IsParam(x,y:integer;GetFocusRect: Boolean;var idx:integer;var hoverrect,ctrlrect:TRect; var CID,CT,CV:string):string;
    procedure SetParamColor(const Value: TColor);
    function GetParam(href: string): string;
    procedure SetParam(href: string; const Value: string);
    function GetItemParam(idx: integer; href: string): string;
    procedure SetItemParam(idx: integer; href: string; const Value: string);
    procedure HandlePopup(Sender:TObject);
    procedure SetHover(const Value: boolean);
    procedure SetImageList(const Value: TImageList);
    function GetItemIndent(const Index:integer):integer;
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: integer);
    function GetCheckWidth: Integer;
    procedure SetParamHint(const Value: Boolean);
    procedure UpdateParam(href:string; value:string);
    procedure PrepareParam(Param:string; var Value:string);
    function HTMLPrep(s:string):string;
    function InvHTMLPrep(s:string):string;
    function GetParamRefCount: Integer;
    function GetParamRefs(Index: Integer): string;
    function GetParamRefIndex(href: string): Integer;
    procedure ControlUpdate(Sender: TObject; Param,Text:string);
    procedure AdvanceEdit(Sender: TObject);
    procedure FreeWrapper(Wrapper: TObject);
    procedure SetLineSpacing(const Value: Integer);
    procedure SetSelectionColor(const Value: TColor);
    procedure SetSelectionFontColor(const Value: TColor);
    function GetParamIndex(href: string): Integer;
    function GetParamRect(href: string): TRect;
    procedure StartParamEdit(param:string;Index: Integer; hr: TRect);
    procedure StartParamDir(idx: integer; param,curdir:string; hr: TRect);
    function GetParamItemIndex(Index: Integer; href: string): Integer;
    function GetParamItemRefs(Item,Index: Integer): string;
    function GetParamItemRefCount(Item: Integer): Integer;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    function GetVersionNr: Integer; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;

    {$IFDEF DELPHIXE2_LVL}
    function InternalGetItemData(Index: Integer): TListBoxItemData; override;
    procedure InternalSetItemData(Index: Integer; AData: TListBoxItemData); override;
    procedure SetItemData(Index: Integer; AData: TListBoxItemData); override;
    function GetItemData(Index: Integer): TListBoxItemData; override;
    {$ENDIF}
    {$IFNDEF DELPHIXE2_LVL}
    function InternalGetItemData(Index: Integer): Longint; override;
    procedure InternalSetItemData(Index: Integer; AData: Longint); override;
    procedure SetItemData(Index: Integer; AData: LongInt); override;
    function GetItemData(Index: Integer): LongInt; override;
    {$ENDIF}

    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure ResetContent; override;
    procedure DeleteString(Index: Integer); override;
    procedure ClickCheck; dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
    property Indent[Index: Integer]: Integer read GetIndent write SetIndent;
    property Comment[Index: Integer]: Boolean read GetComment write SetComment;
    property Selected[Index: Integer]: string read GetSelected;
    property SelectCount: Integer read GetSelectCount;
    procedure SortAllComments;
    procedure SortComment(Index:integer);
    procedure EditParam(href: string);
    property ParamListBox:TPopupListbox read FParamList;
    property Parameter[href:string]:string read GetParam write SetParam;
    property ItemParameter[idx: Integer;href: string]: string read GetItemParam write SetItemParam;
    property ParamRefCount: Integer read GetParamRefCount;
    property ParamRefs[Index: Integer]:string read GetParamRefs;
    property ParamItemRefCount[Item: Integer]: Integer read GetParamItemRefCount;
    property ParamItemRefs[Item,Index: Integer]:string read GetParamItemRefs;
    property ParamRefIndex[href: string]: Integer read GetParamRefIndex;
    property ParamItemIndex[Index: Integer; href: string]: Integer read GetParamItemIndex;
    property ParamIndex[href: string]: Integer read GetParamIndex;
    function GetParamInfo(Index: Integer; HRef:string; var AValue, AClass, AProp, AHint: string): Boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    property DateTimePicker: TPopupDatePicker read FParamDatePicker;
    property SpinEdit: TPopupSpinEdit read FParamSpinEdit;
    property Editor: TPopupEdit read FParamEdit;
    property ListBox: TPopupListBox read FParamList;
  published
    property AdvanceOnReturn: Boolean read FAdvanceOnReturn write FAdvanceOnReturn;
    property Align;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property EditAutoSize: Boolean read FEditAutoSize write FEditAutoSize default False;
    property EmptyParam: string read FEmptyParam write FEmptyParam;
    property Hover: Boolean read FHover write SetHover default True;
    property HoverColor: TColor read FHoverColor write FHoverColor default clGreen;
    property HoverFontColor: TColor read FHoverFontColor write FHoverFontColor default clWhite;
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing default 0;
    property ParamColor: TColor read FParamColor write SetParamColor default clGreen;
    property ParamHint: Boolean read FParamHint write SetParamHint;
    property ParamListSorted: Boolean read FParamListSorted write FParamListSorted;
    property SelectionColor:TColor read FSelectionColor write SetSelectionColor;
    property SelectionFontColor:TColor read FSelectionFontColor write SetSelectionFontColor;
    property ShadowColor:TColor read FShadowColor write SetShadowColor;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset;
    property ShowSelection: Boolean read FShowSelection write FShowSelection;
    property Images: TImageList read FImages write SetImageList;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
    property BorderStyle;
    property Color;
    {property Columns; no multicolumn support}
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Duplicates: Boolean read FDuplicates write SetDuplicates;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default True;
    //property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    //property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    //property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    //property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnCheckClick: TNotifyEvent read FOnCheckClick write FOnCheckClick;
    property OnControlClick: TParamItemControlEvent read FOnControlClick write FOnControlClick;
    property OnControlHint: TParamControlHintEvent read FOnControlHint write FOnControlHint;
    property OnParamClick: TParamClickEvent read FOnParamClick write FOnParamClick;
    property OnParamPrepare: TParamClickEvent read FOnParamPrepare write FOnParamPrepare;
    property OnParamPopup: TParamPopupEvent read FOnParamPopup write FOnParamPopup;
    property OnParamList: TParamPopupEvent read FOnParamList write FOnParamList;
    property OnParamSelect: TParamSelectEvent read FOnParamSelect write FOnParamSelect;
    property OnParamChanged: TParamChangedEvent read FOnParamChanged write FOnParamChanged;
    property OnParamHint: TParamHintEvent read FOnParamHint write FOnParamHint;
    property OnParamEnter: TParamSelectEvent read FOnParamEnter write FOnParamEnter;
    property OnParamExit: TParamSelectEvent read FOnParamExit write FOnParamExit;
    property OnParamEditStart: TParamListBoxEditEvent read FOnParamEditStart write FOnParamEditStart;
    property OnParamEditDone: TParamListBoxEditEvent read FOnParamEditDone write FOnParamEditDone;
    property OnParamCustomEdit: TParamCustomEditEvent read FOnParamCustomEdit write FOnParamCustomEdit;
    property OnParamQuery: TParamListBoxEditEvent read FOnParamQuery write FOnParamQuery;
    property Version: string read GetVersion write SetVersion;
    property WordWrap: boolean read FWordWrap write FWordWrap default true;
  end;

implementation

uses
  Consts, Shellapi, ShlObj, ActiveX, ImgList, CommCtrl
  {$IFDEF DELPHI6_LVL}
  , Variants
  {$ENDIF}
  ;

type
   TSaveObject = class(TObject)
   private
     FIndent: Integer;
     FObject: TObject;
   public
     constructor Create(AIndent:Integer;AObject:TObject);
   end;

  TParamCheckListDataWrapper = class
  private
    FData: LongInt;
    FState: TCheckBoxState;
    FDisabled: Boolean;
    FIndent : Integer;
    FComment : Boolean;
    procedure SetChecked(Check: Boolean);
    function GetChecked: Boolean;
  public
    class function GetDefaultState: TCheckBoxState;
    property Checked: Boolean read GetChecked write SetChecked;
    property State: TCheckBoxState read FState write FState;
    property Disabled: Boolean read FDisabled write FDisabled;
    property Indent: integer read FIndent write FIndent;
    property Comment: Boolean read FComment write FComment;
  end;

var
  FCheckWidth, FCheckHeight: Integer;


procedure GetCheckSize;
begin
  with TBitmap.Create do
    try
      Handle := LoadBitmap(0, PChar(32759));
      FCheckWidth := Width div 4;
      FCheckHeight := Height div 3;
    finally
      Free;
    end;
end;

function MakeSaveState(State: TCheckBoxState; Disabled: Boolean): TObject;
begin
  Result := TObject((Byte(State) shl 16) or Byte(Disabled));
end;

function GetSaveState(AObject: TObject): TCheckBoxState;
begin
  Result := TCheckBoxState(Integer(AObject) shr 16);
end;

function GetSaveDisabled(AObject: TObject): Boolean;
begin
  Result := Boolean(Integer(AObject) and $FF);
end;

{ $TSaveObject }

constructor TSaveObject.Create(AIndent: Integer; AObject: TObject);
begin
  inherited Create;
  FIndent := AIndent;
  FObject := AObject;
end;

{ TParamCheckListDataWrapper }

procedure TParamCheckListDataWrapper.SetChecked(Check: Boolean);
begin
  if Check then FState := cbChecked else FState := cbUnchecked;
end;

function TParamCheckListDataWrapper.GetChecked: Boolean;
begin
  Result := (FState = cbChecked) and not FComment;
end;

class function TParamCheckListDataWrapper.GetDefaultState: TCheckBoxState;
begin
  Result := cbUnchecked;
end;

{ TParamCheckList }

constructor TParamCheckList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFlat := True;
  FDuplicates:=True;
  FDuplicateList:=TStringList.Create;
  FSelectionColor := clHighLight;
  FSelectionFontColor := clHighLightText;
  FShowSelection := True;

  FIsEditing := False;
  DoubleBuffered := True;
  FParamColor := clGreen;
  Style := lbOwnerDrawFixed;
  FHoverColor := clGreen;
  FHoverFontColor := clWhite;
  FHover := True;
  FImages := nil;
  FEmptyParam := '?';
  FParamPopup := TPopupMenu.Create(self);
  FParamList := TPopuplistbox.create(self);
  FParamDatePicker := TPopupDatePicker.Create(Self);
  FParamSpinEdit := TPopupSpinEdit.Create(Self);
  FParamEdit := TPopupEdit.Create(Self);
  FParamMaskEdit := TPopupMaskEdit.Create(Self);

  FParamList.Cursor := crDefault;
  FParamList.Width := 100;
  FParamList.Height := 100;
  FParamList.Top := 0;
  FParamList.Left := 0;
  FParamList.Visible := False;

  FShadowColor := clGray;
  FShadowOffset := 1;
  FUpdateCount := 0;
  FImageCache :=  THTMLPictureCache.Create;
  FMouseDown := False;
  FWordWrap := true;

  WrapperList := TObjectList.Create;
end;

destructor TParamCheckList.Destroy;
begin
  FSaveStates.Free;
  FDuplicateList.Free;
  FParamPopup.Free;
  FParamList.Free;
  FParamDatePicker.Free;
  FParamSpinEdit.Free;
  FParamEdit.Free;
  FParamMaskEdit.Free;
  FImageCache.Free;
  WrapperList.Free;
  inherited;
end;

procedure TParamCheckList.CreateWnd;
begin
  inherited CreateWnd;
  if FSaveStates <> nil then
  begin
    FSaveStates.Free;
    FSaveStates := nil;
  end;
  ResetItemHeight;
end;

procedure TParamCheckList.DestroyWnd;
var
  I: Integer;
begin
  if Items.Count > 0 then
  begin
    FSaveStates := TList.Create;
    for I := 0 to Items.Count - 1 do
      FSaveStates.Add(MakeSaveState(State[I], not ItemEnabled[I]));
  end;
  inherited DestroyWnd;
end;

procedure TParamCheckList.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  Style := Style or LBS_OWNERDRAWFIXED;
end;

function TParamCheckList.GetCheckWidth: Integer;
begin
  Result := FCheckWidth + 2;
end;

function TParamCheckList.GetItemIndent(const Index:integer): Integer;
begin
  Result := (FCheckWidth + 2) * (Indent[Index] + 1);
  if (Comment[Index]) then Result:=0;
end;

procedure TParamCheckList.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
end;

procedure TParamCheckList.CMMouseLeave(Var Msg: TMessage);
begin
  inherited;
  if FHover and (FHoverHyperLink <> -1) then
    InvalidateRect(Handle,@FCurrHoverRect,True);

  FHoverHyperLink := -1;

  if (FOldParam <> '') and Assigned(FOnParamExit) then
    FOnParamExit(Self,-1,FOldParam,Parameter[FOldParam]);

  FOldAnchor := '';
end;

Procedure TParamCheckList.CMHintShow(Var Msg: TMessage);
{$IFDEF DELPHI2_LVL}
type
  PHintInfo = ^THintInfo;
{$ENDIF}
var
  CanShow: Boolean;
  hi: PHintInfo;
  hr,cr:trect;
  anchor:string;
  res:integer;
  CID,CV,CT: string;
  v,c,p,h: string;
Begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);

  if FParamHint and not FIsEditing then
  begin
    Anchor := IsParam(hi^.cursorPos.x,hi^.cursorpos.y,False,res,hr,cr,CID,CV,CT);

    GetParamInfo(res,Anchor,v,c,p,h);

    if h <> '' then
      Anchor := h;

    if (Anchor <> '') then
    begin
      hi^.HintPos := clienttoscreen(hi^.CursorPos);
      hi^.hintpos.y:=hi^.hintpos.y-10;
      hi^.hintpos.x:=hi^.hintpos.x+10;

      if Assigned(FOnParamHint) then
        FOnParamHint(self,anchor,anchor,CanShow);

     hi^.HintStr := anchor;
    end;
  end;
  Msg.Result := Ord(Not CanShow);
end;


procedure TParamCheckList.ResetItemHeight;
begin
  if HandleAllocated and (Style = lbStandard) then
  begin
    Canvas.Font := Font;
    FStandardItemHeight := Canvas.TextHeight('Wg');
    Perform(LB_SETITEMHEIGHT, 0, FStandardItemHeight);
  end;
end;

procedure TParamCheckList.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  r,hr:TRect;
  ACheckWidth: Integer;
  Enable: Boolean;
  a,s,f:string;
  xs,ys:integer;
  ml,hl,fl:integer;
  paramcolor:tcolor;
  pt:tpoint;
  fHyperlink:integer;
  CID,CV,CT: string;
  cr,hrect: TRect;
  FHC, FHFC: TColor;

begin
  if (FUpdateCount>0) then
    Exit;

  if (odSelected in State) and FShowSelection and
     (SelectionColor <> clNone) and (SelectionFontColor <> clNone) then
  begin
    Canvas.Brush.Color := SelectionColor;
    Canvas.Pen.Color := SelectionColor;
    Canvas.Font.Color := SelectionFontColor;
    paramcolor := SelectionFontColor;
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Font.Color := Font.Color;
    paramcolor := FParamColor;
   end;

  Canvas.Rectangle(0,rect.Top,Width,rect.Bottom);


  ACheckWidth := GetCheckWidth;
  if (Index < Items.Count) and (Index>=0) then
  begin
    R := Rect;
    if not UseRightToLeftAlignment then
    begin
      R.Right := Rect.Left;
      R.Left := R.Right - ACheckWidth;
    end
    else
    begin
      R.Left := Rect.Right;
      R.Right := R.Left + ACheckWidth;
    end;
    Enable := Self.Enabled and GetItemEnabled(Index);

    if not GetComment(Index) then
      DrawCheck(R, GetState(Index), Enable);

    if not Enable then
      Canvas.Font.Color := clGrayText;
  end;


  if Index = Items.Count - 1 then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Rectangle(0,rect.bottom,Width,ClientRect.bottom);
  end;

  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  if (FHoverIdx <> Index) then
    FHyperlink := -1
  else
    FHyperLink := FHoverHyperLink;

  if {(FFocusItem = Index) and} (odFocused in State) then
  begin
    fl := FFocusLink;
  end
  else
    fl := -1;

  hrect := Rect;
  hrect.Top := hrect.Top + 2;

  if not FHover then
  begin
    FHC := clNone;
    FHFC := clNone;
  end
  else
  begin
    FHC := FHoverColor;
    FHFC := FHoverFontColor;
  end;

  HTMLDrawEx(Canvas,Items[index],hrect,FImages,pt.x,pt.y,fl,FHyperLink,FShadowOffset,
    False,False,False,False,(odSelected in State) and FShowSelection,fHover,WordWrap,FMouseDown,False,1.0, ParamColor,
    FHC,FHFC,fShadowColor,a,s,f,xs,ys,hl,ml,hr,cr,CID,CV,CT,
    FImageCache,FContainer,Handle,FLineSpacing);

  { redraw when current number of hyperlinks is smaller than FFocusLink }
  if (fl >= hl) then
  begin
    fl := hl - 1;
    HTMLDrawEx(Canvas,Items[Index],hrect,FImages,pt.x,pt.y,fl,FHyperLink,FShadowOffset,
      False,False,False,False,(odSelected in State) and FShowSelection,FHover ,WordWrap,FMouseDown,False,1.0,ParamColor,
      FHC,FHFC,clGray,a,s,f,xs,ys,hl,ml,hr,cr,CID,CV,CT,
      FImageCache,FContainer,Handle,FLineSpacing);
  end;

  if (odFocused in State) and FShowSelection then
    Canvas.DrawFocusRect(Rect);

  if (Index = Items.Count-1) then
  begin
    r := GetClientRect;
    if r.Bottom > rect.Bottom then
    begin
      Canvas.Brush.color := self.Color;
      Canvas.Pen.color := self.Color;
      Canvas.Rectangle(rect.left,rect.bottom+1,rect.right,r.bottom);
    end;
  end;
end;

procedure TParamCheckList.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
   begin
    if integer(ItemID)<0 then exit;

    if not Comment[ItemID] then
     begin
      if not UseRightToLeftAlignment then
        rcItem.Left := rcItem.Left + GetItemIndent(ItemID)
      else
        rcItem.Right := rcItem.Right - GetItemIndent(ItemID);
     end;

    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if Integer(itemID) >= 0 then DrawItem(itemID, rcItem, State);
    Canvas.Handle := 0;
   end;
end;

procedure TParamCheckList.DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
var
  DrawState: Integer;
  DrawRect: TRect;
  OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  OldPenColor: TColor;
  Rgn, SaveRgn: HRgn;
  HTHeme: THandle;
  UseWinXP: Boolean;
begin
  SaveRgn := 0;
  DrawRect.Left := R.Left + (R.Right - R.Left - FCheckWidth) div 2;
  DrawRect.Top := R.Top + (R.Bottom - R.Top - FCheckWidth) div 2;
  DrawRect.Right := DrawRect.Left + FCheckWidth;
  DrawRect.Bottom := DrawRect.Top + FCheckHeight;
  case AState of
    cbChecked:
      DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED;
    cbUnchecked:
      DrawState := DFCS_BUTTONCHECK;
    else // cbGrayed
    begin
      DrawState := DFCS_BUTTON3STATE or DFCS_CHECKED;
    end;
  end;

  if not AEnabled then
    DrawState := DrawState or DFCS_INACTIVE;

  with Canvas do
  begin

    if Flat then
    begin
      { Remember current clipping region }
      SaveRgn := CreateRectRgn(0,0,0,0);
      GetClipRgn(Handle, SaveRgn);
      { Clip 3d-style checkbox to prevent flicker }
      with DrawRect do
        Rgn := CreateRectRgn(Left + 2, Top + 2, Right - 2, Bottom - 2);
      SelectClipRgn(Handle, Rgn);
      DeleteObject(Rgn);
    end;

    if IsWinXP then
      UseWinXP := IsThemeActive
    else
      UseWinXP := False;

    if UseWinXP then
    begin
      HTHeme := OpenThemeData(self.Handle,'button');

      case AState of
      cbChecked:
        if AEnabled then
          DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL,@DrawRect,nil)
        else
          DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDDISABLED,@DrawRect,nil);

      cbUnChecked:
        if AEnabled then
          DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL,@DrawRect,nil)
        else
          DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDDISABLED,@DrawRect,nil);
      end;

      CloseThemeData(HTHeme);
    end
    else
    begin
      DrawFrameControl(Handle, DrawRect, DFC_BUTTON, DrawState);
    end;

    if Flat then
    begin
      SelectClipRgn(Handle, SaveRgn);
      DeleteObject(SaveRgn);
      { Draw flat rectangle in-place of clipped 3d checkbox above }
      OldBrushStyle := Brush.Style;
      OldBrushColor := Brush.Color;
      OldPenColor := Pen.Color;
      Brush.Style := bsClear;
      Pen.Color := clBtnShadow;
      with DrawRect do
        Rectangle(Left + 1, Top + 1, Right - 1, Bottom - 1);
      Brush.Style := OldBrushStyle;
      Brush.Color := OldBrushColor;
      Pen.Color := OldPenColor;
    end;
  end;
end;

procedure TParamCheckList.SetChecked(Index: Integer; Checked: Boolean);
begin
  if Checked <> GetChecked(Index) then
  begin
    TParamCheckListDataWrapper(GetWrapper(Index)).SetChecked(Checked);
    InvalidateCheck(Index);
  end;
end;

procedure TParamCheckList.SetItemEnabled(Index: Integer; const Value: Boolean);
begin
  if Value <> GetItemEnabled(Index) then
  begin
    TParamCheckListDataWrapper(GetWrapper(Index)).Disabled := not Value;
    InvalidateCheck(Index);
  end;
end;

procedure TParamCheckList.SetState(Index: Integer; AState: TCheckBoxState);
begin
  if AState <> GetState(Index) then
  begin
    TParamCheckListDataWrapper(GetWrapper(Index)).State := AState;
    InvalidateCheck(Index);
  end;
end;

procedure TParamCheckList.SetIndent(Index: Integer; AIndent: integer);
begin
  if AIndent <> GetIndent(Index) then
  begin
    TParamCheckListDataWrapper(GetWrapper(Index)).Indent := AIndent;
    InvalidateItem(Index);
  end;
end;

procedure TParamCheckList.SetComment(Index: Integer; AComment: Boolean);
begin
  if AComment <> GetComment(Index) then
  begin
    TParamCheckListDataWrapper(GetWrapper(Index)).Comment := AComment;
    InvalidateItem(Index);
  end;
end;


procedure TParamCheckList.InvalidateCheck(Index: Integer);
var
  R: TRect;
begin
  R := ItemRect(Index);

  if not UseRightToLeftAlignment then
    R.Right := R.Left + GetItemIndent(Index)
  else
    R.Left := R.Right - GetItemIndent(Index);

  Invalidate;

  //InvalidateRect(Handle, @R, not (csOpaque in ControlStyle));
  UpdateWindow(Handle);
end;

procedure TParamCheckList.InvalidateItem(Index: Integer);
var
  R: TRect;
begin
  R := ItemRect(Index);
  InvalidateRect(Handle, @R, not (csOpaque in ControlStyle));

//  UpdateWindow(Handle);
end;

function TParamCheckList.GetChecked(Index: Integer): Boolean;
begin
  if HaveWrapper(Index) then
    Result := TParamCheckListDataWrapper(GetWrapper(Index)).GetChecked
  else
    Result := False;
end;

function TParamCheckList.GetItemEnabled(Index: Integer): Boolean;
begin
  if HaveWrapper(Index) then
    Result := not TParamCheckListDataWrapper(GetWrapper(Index)).Disabled
  else
    Result := True;
end;

function TParamCheckList.GetState(Index: Integer): TCheckBoxState;
begin
  if HaveWrapper(Index) then
    Result := TParamCheckListDataWrapper(GetWrapper(Index)).State
  else
    Result := TParamCheckListDataWrapper.GetDefaultState;
end;

function TParamCheckList.GetIndent(Index: Integer): Integer;
begin
  if HaveWrapper(Index) then
    Result := TParamCheckListDataWrapper(GetWrapper(Index)).Indent
  else
    Result := 0;
end;

function TParamCheckList.GetComment(Index: Integer): Boolean;
begin
  if HaveWrapper(Index) then
    Result := TParamCheckListDataWrapper(GetWrapper(Index)).Comment
  else
    Result := False;
end;

procedure TParamCheckList.KeyPress(var Key: Char);
begin
  if (Key = #32) then
    ToggleClickCheck(ItemIndex);
  inherited;

  if (FFocusLink <> -1) and (ItemIndex >= 0) and (ItemIndex < Items.Count) and (Key = #13) then
  begin
    {$IFDEF TMSDEBUG}
    outputdebugstring(pchar('start edit for : '+inttostr(ffocuslink)));
    {$ENDIF}
    if GetParamItemRefCount(ItemIndex) > 0 then
      EditParam(GetParamItemRefs(ItemIndex,FFocusLink));
  end;
end;

procedure TParamCheckList.HandlePopup(Sender:TObject);
var
  newvalue,oldvalue:string;

begin
  if (Sender is TMenuItem) then
  begin
    newvalue := (Sender as TMenuItem).Caption;

    while (pos('&',newvalue)>0) do
      system.Delete(newvalue,pos('&',newvalue),1);

    oldvalue := Parameter[FOldParam];

    if Assigned(FOnParamSelect) then
      FOnParamSelect(self,ItemIndex,fOldParam,newvalue);

    Parameter[FOldParam] := newvalue;

    if (oldvalue <> newvalue) then
      if Assigned(FOnParamChanged) then
        FOnParamChanged(self,ItemIndex,fOldParam,oldvalue,newvalue);
  end;
end;

procedure TParamCheckList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  hr,cr: TRect;
  CID,CT,CV,s: string;
  Idx: Integer;
begin
  inherited;
  FMouseDown := False;

  IsParam(X,Y,False,Idx,hr,cr,CID,CT,CV);

  if CID <> '' then
  begin

    if CT = 'CHECK' then
    begin
      BeginUpdate;
      s := Items[Idx];

      if Uppercase(CV) = 'TRUE' then
        SetControlValue(s,CID,'FALSE')
      else
        SetControlValue(s,CID,'TRUE');

      Items[Idx] := s;
      EndUpdate;
    end;

    if Assigned(FOnControlClick) then
       FOnControlClick(Self,X,Y,Idx,CID,CT,CV);

    if FCurrCtrlDown.Left <> -1 then
      InvalidateRect(Handle,@FCurrCtrlDown,true);
   end;

  FCurrCtrlDown := Rect(-1,-1,-1,-1);
end;

procedure TParamCheckList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
  CID,CV,CT: string;
  hr,cr: TRect;
  param: string;

begin
  inherited;

  if Button = mbLeft then
  begin
    Index := ItemAtPos(Point(X,Y),True);
    if (Index <> -1) and GetItemEnabled(Index) and not GetComment(Index) then
      if not UseRightToLeftAlignment then
      begin
        if (X - ItemRect(Index).Left < GetItemIndent(Index)) and
	   (X - ItemRect(Index).Left > (GetCheckWidth * (GetIndent(Index)))) then
          ToggleClickCheck(Index)
      end
      else
      begin
        Dec(X, ItemRect(Index).Right - GetCheckWidth);
        if (X > 0) and (X < GetCheckWidth) then
          ToggleClickCheck(Index)
      end;
  end;

  param := IsParam(X,Y,False,Index,hr,cr,CID,CV,CT);

  if CID <> '' then
  begin
    InvalidateRect(Handle,@cr,true);
    FCurrCtrlDown := cr;
  end
  else
    FCurrCtrlDown := Rect(-1,-1,-1,-1);

  FMouseDown := true;

  if param <> '' then
    StartParamEdit(param,Index,hr);
end;


procedure TParamCheckList.StartParamEdit(param:string;Index: Integer; hr: TRect);
var
  I: Integer;
  oldvalue,newvalue,v,c,p,h:string;
  pt: TPoint;
  doPopup,doList:boolean;
  newvalues:TStringList;
  newmenu:TMenuItem;

  function Max(a,b:Integer): Integer;
  begin
    if a > b then
      Result := a
    else
      Result := b;
  end;

begin
  GetParamInfo(Index,Param,v,c,p,h);

  FFocusItem := Index;
  FFocusLink := ParamItemIndex[Index,param];

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('set focus link to : '+inttostr(ffocuslink)));
  {$ENDIF}

  if Assigned(FOnParamClick) and (c = '') then
  begin
    FIsEditing := True;
    PrepareParam(Param,v);
    oldvalue := v;
    FOnParamClick(Self,ItemIndex,param,v);
    if (v <> oldvalue) then
      ControlUpdate(self,Param,v);
    FIsEditing := False;
  end;

  if (c = 'TOGGLE') then
  begin
    NewValues := TStringList.Create;
    PropToList(InvHTMLPrep(p),NewValues);

    if NewValues.Count > 1 then
    begin
      if v = NewValues[0] then
        v := NewValues[1]
      else
        v := NewValues[0];
      ControlUpdate(self,Param,v);
    end;
    NewValues.Free;
  end;

  if (c = 'MENU') then
  begin
    FIsEditing := True;
    GetHRefValue(self.Items[Index],param,oldvalue);
    newvalue := oldvalue;
    NewValues := TStringList.Create;
    NewValues.Sorted := FParamListSorted;
    doPopup := True;

    PropToList(InvHTMLPrep(p),NewValues);

    if Assigned(FOnParamPopup) then
      FOnParamPopup(self,Index,param,newvalues,doPopup);

    if doPopup then
    begin
      pt := ClientToScreen(point(hr.left,hr.bottom));

      while FParamPopup.Items.Count>0 do fParamPopup.Items[0].Free;

      FParamPopup.AutoHotkeys := maManual;
      for i := 1 to NewValues.Count do
      begin
        newmenu := TMenuItem.Create(Self);
        newmenu.Caption := NewValues.Strings[i-1];
        newmenu.OnClick := HandlePopup;
        FParamPopup.Items.Add(newmenu);
       end;

      PrepareParam(Param,OldValue);
      FOldParam := param;
      FParamPopup.Popup(pt.x + GetItemIndent(Index),pt.y+2);
    end;
    NewValues.Free;
    FIsEditing := False;
  end;

  if (c = 'LIST') then
  begin
    FIsEditing := True;
    doList := True;
    GetHRefValue(self.Items[Index],param,oldvalue);
    newvalue:=oldvalue;
    NewValues:=TStringList.Create;
    NewValues.Sorted:=fParamListSorted;
    dolist := True;

    PropToList(InvHTMLPrep(p),NewValues);

    if Assigned(FOnParamList) then
      FOnParamList(self,Index,param,newvalues,dolist);

    if doList then
    begin
      pt := ClientToScreen(point(hr.left,hr.bottom));
      FParamList.top:=pt.y;
      FParamList.left:=pt.x + GetItemIndent(Index);
      FParamlist.OnUpdate := ControlUpdate;
      FParamList.OnReturn := AdvanceEdit;
      FParamlist.Param := param;
      FParamList.Parent := Self;

      SetWindowLong( FParamList.Handle, GWL_EXSTYLE,
                 GetWindowLong(FParamList.Handle, GWL_EXSTYLE) or
                 WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);

      PrepareParam(Param,OldValue);
      FParamlist.visible:=true;
      FParamList.Items.Assign(NewValues);
      FParamList.Ctl3D:= False;
      FParamList.SizeDropDownWidth;
      FParamList.ItemIndex:=fParamList.Items.IndexOf(oldvalue);
      FParamList.SetFocus;
    end;
    NewValues.Free;
  end;

  if c = 'DATE' then
  begin
    FIsEditing := True;
    pt := Clienttoscreen(Point(hr.left,hr.top));

    FParamDatePicker.Top := pt.Y - 2;
    FParamDatePicker.Left := pt.X + GetItemIndent(Index);
    FParamDatePicker.Width := Max(64,hr.Right - hr.Left);

    FParamDatePicker.Cancelled := False;
    FParamDatePicker.Parent := Self;
    FParamDatePicker.OnUpdate := ControlUpdate;
    FParamDatePicker.OnReturn := AdvanceEdit;
    FParamDatePicker.Kind := dtkDate;
    FParamDatePicker.Param := Param;
    FParamDatePicker.Visible := True;

    PrepareParam(Param,v);

    try
      {$IFDEF DELPHI6_LVL}
      FParamDatePicker.Date := VarToDateTime(v);
      {$ENDIF}
      {$IFNDEF DELPHI6_LVL}
      FParamDatePicker.Date := StrToDate(v);
      {$ENDIF}
    except
    end;
    FParamDatePicker.SetFocus;
  end;

  if c = 'TIME' then
  begin
    FIsEditing := True;
    pt := Clienttoscreen(Point(hr.left,hr.top));

    FParamDatePicker.Top := pt.Y - 2;
    FParamDatePicker.Left := pt.X + GetItemIndent(Index);
    FParamDatePicker.Width := Max(64,hr.Right - hr.Left);
    FParamDatePicker.ReInit;

    FParamDatePicker.Cancelled := False;
    FParamDatePicker.Parent := Self;
    FParamDatePicker.OnUpdate := ControlUpdate;
    FParamDatePicker.OnReturn := AdvanceEdit;
    FParamDatePicker.Kind := dtkTime;
    FParamDatePicker.Param := Param;
    FParamDatePicker.Visible := True;

    PrepareParam(Param,v);
    try
      FParamDatePicker.DateTime := EncodeDate(2005,1,1) + StrToTime(v);
    except
    end;  
    FParamDatePicker.SetFocus;
  end;


  if c = 'SPIN' then
  begin
    FIsEditing := True;
    pt := Clienttoscreen(Point(hr.left,hr.top));
    FParamSpinEdit.Top := pt.Y - 2;
    FParamSpinEdit.Left := pt.X + GetItemIndent(Index);
    FParamSpinEdit.Width := Max(16,hr.Right - hr.Left) + 24;

    FParamSpinEdit.Cancelled := False;
    FParamSpinEdit.Parent := Self;
    FParamSpinEdit.OnUpdate := ControlUpdate;
    FParamSpinEdit.OnReturn := AdvanceEdit;
    FParamSpinEdit.Param := Param;
    FParamSpinEdit.Visible := True;

    PrepareParam(Param,v);

    try
      FParamSpinEdit.Value := StrToInt(Trim(v));
    except
      FParamSpinEdit.Value := 0;
    end;
    FParamSpinEdit.SetFocus;
  end;

  if c = 'EDIT' then
  begin
    FIsEditing := True;
    pt := Clienttoscreen(Point(hr.left,hr.top));

    FParamEdit.Top := pt.Y - 2;
    FParamEdit.Left := pt.X + GetItemIndent(Index);
    FParamEdit.Width := Max(16,hr.Right - hr.Left) + 16;

    FParamEdit.AutoSize := EditAutoSize;
    FParamEdit.Cancelled := False;
    FParamEdit.Parent := Self;
    FParamEdit.OnUpdate := ControlUpdate;
    FParamEdit.OnReturn := AdvanceEdit; 
    FParamEdit.Param := Param;
    FParamEdit.Visible := True;

    PrepareParam(Param,v);

    FParamEdit.Text := v;
    FParamEdit.SetFocus;
  end;

  if (c = 'MASK') then
  begin
    FIsEditing := True;
    pt := Clienttoscreen(Point(hr.left,hr.top));

    FParamMaskEdit.Top := pt.Y - 2;
    FParamMaskEdit.Left := pt.X + GetItemIndent(Index);
    FParamMaskEdit.Width := Max(16,hr.Right - hr.Left) + 16;

    FParamMaskEdit.Cancelled := False;
    FParamMaskEdit.Parent := Self;
    FParamMaskEdit.OnUpdate := ControlUpdate;
    FParamMaskEdit.OnReturn := AdvanceEdit;
    FParamMaskEdit.Param := Param;
    FParamMaskEdit.Visible := True;

    PrepareParam(Param,v);

    FParamMaskEdit.EditMask := InvHTMLPrep(p);
    FParamMaskEdit.Text := v;
    FParamMaskEdit.SetFocus;
  end;

  if c = 'DIR' then
  begin
    FIsEditing := True;
    PrepareParam(Param,v);
    StartParamDir(index, param,v,hr);
    FIsEditing := False;
  end;

  if  (c = 'QUERY') then
  begin
    FIsEditing := True;
    PrepareParam(Param,v);
    if Assigned(OnParamQuery) then
      OnParamQuery(Self,ItemIndex,Param,v);
    ControlUpdate(self,Param,v);
  end;
  
  if  (c = 'CUSTOM') then
  begin
    PrepareParam(Param,v);
    pt := ClientToScreen(Point(hr.left,hr.top));
    FIsEditing := True;
    if Assigned(OnParamCustomEdit) then
      OnParamCustomEdit(Self,ItemIndex,Param,v,p,Rect(pt.x,pt.Y,pt.X + hr.Right - hr.Left,pt.Y + hr.Bottom - hr.Top));
  end;

end;

procedure TParamCheckList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
 param:string;
 idx:integer;
 hr,cr:TRect;
 CID,CV,CT: string;

begin
  inherited;

  if FIsEditing then
    Exit;

  param := IsParam(x,y,False,idx,hr,cr,CID,CT,CV);

  if param <> FOldAnchor then
    Application.CancelHint;

  FOldAnchor := param;

  if (param = '') and (FHoverIdx <> -1) and (FHover) then
  begin
    InvalidateRect(self.handle,@FCurrHoverRect,true);

    FHoverIdx:=-1;
  end;

  if (CID = '') and (FCurrCtrlID <> '') then
  begin
    {$IFDEF TMSDEBUG}
    outputdebugstring(pchar('out : '+FCurrCtrlID));
    {$ENDIF}
    InvalidateRect(Handle,@FCurrCtrlRect,True);
    FCurrCtrlID := CID;
  end;

  if (CID <> FCurrCtrlID) and (CID <> '') then
  begin
    {$IFDEF TMSDEBUG}
    outputdebugstring(pchar('in : '+cid));
    {$ENDIF}
    InvalidateRect(Handle,@cr,True);
    FCurrCtrlID := CID;
    FCurrCtrlRect := cr;
  end;


 if (param='') and (fHoverIdx<>-1) and (fHover) then
  begin
   InvalidateRect(self.handle,@fCurrHoverRect,true);
   fHoverIdx:=-1;
  end;

 if (param<>'') then
    begin
     if (fHover) then
      begin
       if (idx<>fHoverIdx) or not equalrect(hr,fCurrHoverRect) then
            InvalidateRect(self.handle,@fCurrHoverRect,true);

      end;

     fHoverIdx:=idx;

     if (fOldParam<>param) then
       begin
         if assigned(fOnParamEnter) then fOnParamEnter(self,idx,Param,Parameter[Param]);
         Application.Cancelhint;
       end;

     if (self.Cursor<>crHandPoint) then
       begin
        fOldCursor:=self.Cursor;
        self.Cursor:=crHandPoint;
        if fHover then InvalidateRect(self.handle,@hr,true);
        fCurrHoverRect:=hr;
       end;
     fOldParam:=param;
    end
   else
    if (self.Cursor=crHandPoint) and (fOldParam<>'') then
     begin
      if assigned(fOnParamExit) then fOnParamExit(self,idx,fOldParam,Parameter[fOldParam]);
      Application.CancelHint;
      self.Cursor:=fOldCursor;
      fOldParam:='';
      if fHover then InvalidateRect(self.handle,@fCurrHoverRect,true);
     end;
end;

procedure TParamCheckList.ToggleClickCheck;
var
  State: TCheckBoxState;
begin
  if (Index >= 0) and (Index < Items.Count) and GetItemEnabled(Index) then
  begin

    State := Self.State[Index];
    case State of
      cbUnchecked:
        if AllowGrayed then State := cbGrayed else State := cbChecked;
      cbChecked: State := cbUnchecked;
      cbGrayed: State := cbChecked;
    end;
    Self.State[Index] := State;

    ClickCheck;
  end;
end;

procedure TParamCheckList.ClickCheck;
begin
  if Assigned(FOnCheckClick) then FOnCheckClick(Self);
end;


function TParamCheckList.GetWrapper(Index: Integer): TObject;
begin
  Result := ExtractWrapper(Index);
  if Result = nil then
  begin
    Result := CreateWrapper(Index);
    WrapperList.Add(Result);
  end;
end;

function TParamCheckList.ExtractWrapper(Index: Integer): TObject;
begin
  result:=nil;
  if (Index<0) then exit;
  Result := TParamCheckListDataWrapper(inherited GetItemData(Index));
{$IFNDEF DELPHI6_LVL}
  if LB_ERR = Integer(Result) then
    raise EListError.CreateResFmt(@SListIndexError, [Index]);
{$ENDIF}
  if (Result <> nil) and (not (Result is TParamCheckListDataWrapper)) then
    Result := nil;
end;

{$IFNDEF DELPHIXE2_LVL}
function TParamCheckList.GetItemData(Index: Integer): LongInt;
{$ENDIF}
{$IFDEF DELPHIXE2_LVL}
function TParamCheckList.GetItemData(Index: Integer): TListBoxItemData;
{$ENDIF}
begin
  Result := 0;
  if HaveWrapper(Index) then
    Result := TParamCheckListDataWrapper(GetWrapper(Index)).FData;
end;

{$IFNDEF DELPHIXE2_LVL}
function TParamCheckList.InternalGetItemData(Index: Integer): LongInt;
{$ENDIF}
{$IFDEF DELPHIXE2_LVL}
function TParamCheckList.InternalGetItemData(Index: Integer): TListBoxItemData;
{$ENDIF}
begin
  Result := inherited GetItemData(Index);
end;

{$IFNDEF DELPHIXE2_LVL}
procedure TParamCheckList.SetItemData(Index: Integer; AData: LongInt);
{$ENDIF}
{$IFDEF DELPHIXE2_LVL}
procedure TParamCheckList.SetItemData(Index: Integer; AData: TListBoxItemData);
{$ENDIF}
var
  Wrapper: TParamCheckListDataWrapper;
  SaveState: TObject;
begin
  Wrapper := TParamCheckListDataWrapper(GetWrapper(Index));
  Wrapper.FData := AData;
  if FSaveStates <> nil then
    if FSaveStates.Count > 0 then
    begin
      SaveState := FSaveStates[0];
      Wrapper.FState := GetSaveState(SaveState);
      Wrapper.FDisabled := GetSaveDisabled(SaveState);
      FSaveStates.Delete(0);
    end;
end;


{$IFNDEF DELPHIXE2_LVL}
procedure TParamCheckList.InternalSetItemData(Index: Integer; AData: LongInt);
{$ENDIF}
{$IFDEF DELPHIXE2_LVL}
procedure TParamCheckList.InternalSetItemData(Index: Integer; AData: TListBoxItemData);
{$ENDIF}
begin
  inherited SetItemData(Index, AData);
end;

function TParamCheckList.CreateWrapper(Index: Integer): TObject;
begin
  Result := TParamCheckListDataWrapper.Create;
  inherited SetItemData(Index, LongInt(Result));
end;

procedure TParamCheckList.FreeWrapper(Wrapper: TObject);
begin
  WrapperList.Remove(Wrapper);
end;

function TParamCheckList.HaveWrapper(Index: Integer): Boolean;
begin
  Result := ExtractWrapper(Index) <> nil;
end;


procedure TParamCheckList.ResetContent;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    if HaveWrapper(I) then
      FreeWrapper(GetWrapper(I));
  inherited;
end;

procedure TParamCheckList.DeleteString(Index: Integer);
begin
  if HaveWrapper(Index) then
    FreeWrapper(GetWrapper(Index));
  inherited;
end;

procedure TParamCheckList.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TParamCheckList.SetDuplicates(Value: Boolean);
var
  i,j: Integer;
  FTempList: TStringList;

begin
  if Value <> FDuplicates then
  begin
    FTempList := TStringList.Create;
    FTempList.Sorted := True;

    FDuplicates := Value;
    if not FDuplicates then
    begin
      i := 1;
      FDuplicateList.Clear;
      while (i <= self.Items.Count) do
      begin
        if FTempList.IndexOf(Items[i - 1]) >= 0 then
        begin
          j := FDuplicateList.Add(Items[i - 1]);
          FDuplicateList.Objects[j] := TSaveObject.Create(GetIndent(i - 1),Items.Objects[i - 1]);
          Items.Delete(i - 1);
        end
        else
        begin
          FTempList.Add(Items[i-1]);
          inc(i);
        end;
      end;

      while (i < Items.Count) do
      begin
        if (AnsiCompareText(Items[i-1],Items[i])=0) then
        begin
          j := FDuplicateList.Add(Items[i]);
          FDuplicateList.Objects[j] := TSaveObject.Create(GetIndent(i),Items.Objects[i]);
          Items.Delete(i);
        end
        else inc(i);
      end;
    end
    else
    begin
      for i := 1 to FDuplicateList.Count do
      begin
        j := Items.Add(FDuplicateList.Strings[i - 1]);
        Indent[j] := TSaveObject(FDuplicateList.Objects[i - 1]).FIndent;
        Items.Objects[j] := TSaveObject(FDuplicateList.Objects[i - 1]).FObject;
      end;
    end;

    FTempList.Free;
  end;
end;

function TParamCheckList.GetSelectCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Items.Count do
  begin
    if Checked[i-1] then
      Result := Result + 1;
  end;
end;

procedure TParamCheckList.QuickSortItems(left,right:integer);
var
  i,j: Integer;
  s: string;
  o: TObject;
  w1,w2: Integer;
begin
  i := Left;
  j := Right;

  s := Items[(left+right) shr 1];

  repeat
    while (s > Items[i]) do Inc(i);
    while (s < Items[j]) do Dec(j);

    if (i <= j) then
    begin
      if (i <> j) then
      begin
        s := Items[i];
        o := Items.Objects[i];
        w1 := inherited GetItemData(i);
        w2 := inherited GetItemData(j);

        Items[i] := Items[j];
        Items.Objects[i] := Items.Objects[j];
        Items[j] := s;
        Items.Objects[j] := o;
        inherited SetItemData(i,w2);
        inherited SetItemData(j,w1);

      end;
      inc(i);
      dec(j);
    end;
  until (i > j);

  if (left < j) then QuicksortItems(left,j);
  if (i < right) then QuickSortItems(i,right);
end;

procedure TParamCheckList.SortComment(Index:integer);
var
  j:Integer;
begin
  if (Items.Count = 0) then
    Exit;
  j := Index + 1;
  while (j <= Items.Count) and not Comment[j] do
    inc(j);

  QuickSortItems(Index + 1,j - 1);
end;

procedure TParamCheckList.SortAllComments;
var
  i,j: Integer;
begin
  if Items.Count = 0 then
    Exit;

  i := 0;
  j := 0;

  repeat
    if Comment[j] then
    begin
      if (j > i) then
        QuickSortItems(i,j-1);
      i := j + 1;
    end;
    inc(j);
  until (j = self.Items.Count);
  QuickSortItems(i,j-1);
end;


function TParamCheckList.GetSelected(Index: Integer): string;
var
  i,j: Integer;
begin
  Result := '';
  j := 0;
  for i := 1 to Items.Count do
  if (Checked[i-1]) then
  begin
    if (j = Index) then
    begin
      Result := Items[i-1];
      Break;
    end
    else
      j := j + 1;
  end;
end;

procedure TParamCheckList.WMDestroy(var Msg: TWMDestroy);
var
  i: Integer;
begin
  for i := 0 to Items.Count -1 do
    FreeWrapper(ExtractWrapper(i));
  inherited;
end;

function TParamCheckList.IsParam(x,y:integer;GetFocusRect: Boolean;var idx:integer;var hoverrect,ctrlrect:TRect; var CID,CT,CV:string):string;
var
  res: Integer;
  r: TRect;
  Anchor,stripped,f: string;
  xsize,ysize: Integer;

begin
  Result:='';

  CID := '';

  if GetFocusRect then
    res := ItemIndex
  else
  begin
    idx := -1;
    res := loword(SendMessage(Handle,lb_itemfrompoint,0,makelparam(X,Y)));
  end;

  if (res >= 0) and (res < Items.Count) then
  begin
    if not GetFocusRect then
      idx := res;

    SendMessage(Handle,lb_getitemrect,res,LParam(@r));

    r.Left := r.Left + GetItemIndent(res);
    r.Top := r.Top + 2;

    if HTMLDrawEx(Canvas,self.items[res],r,fImages,X,Y,idx,-1,FShadowOffset,
      true,false,False,True,True,False,WordWrap,False,GetFocusRect,1.0,fParamColor,
      clNone,clNone,fShadowColor,anchor,stripped,f,xsize,ysize,FNumHyperlinks,FHoverHyperLink,hoverrect,ctrlrect,
      CID,CV,CT,FImageCache,FContainer,Handle,FLineSpacing) then

    Result := Anchor;
  end;
end;

procedure TParamCheckList.SetParamColor(const Value: TColor);
begin
  FParamColor := Value;
  invalidate;
end;

function TParamCheckList.GetItemParam(idx: integer; href: string): string;
var
  h: string;
begin
  Result := '';

  if (idx < 0) or (idx > Items.Count - 1) then
    raise Exception.Create('Invalid item index');

  if GetHRefvalue(Items[idx],href,h) then
    Result := InvHTMLPrep(h);
end;


procedure TParamCheckList.SetItemParam(idx: integer; href: string;
  const Value: string);
var
  h,v,s: string;
  sel: boolean;
  selidx: Integer;

begin
  if (idx<0) or (idx>Items.Count-1) then
    raise Exception.Create('Invalid item index');

  FHoverIdx := -1;
  v := value;
  sel := false;
  selidx := 0;

  v := HTMLPrep(Value);

  s := Items[idx];
  if GetHRefValue(s,href,h) then
  begin
    SetHRefValue(s,href,v);

    if MultiSelect then
      sel := inherited Selected[idx]
    else
      selidx := ItemIndex;

    Items[idx]:=s;

    if MultiSelect then
      inherited Selected[idx] := sel
    else
      ItemIndex := selidx;
  end;
end;

function TParamCheckList.GetParam(href: string): string;
var
 i:integer;
 h:string;
begin
 for i:=1 to Items.Count do
  begin
   if gethrefvalue(Items[i-1],href,h) then
     begin
      result:=h;
      break;
     end;
  end;
end;

procedure TParamCheckList.SetParam(href: string; const Value: string);
var
  i: Integer;
  h,s: string;
  chk: Boolean;
  ind: Integer;
  com: Boolean;

begin
  for i := 1 to Items.Count do
  begin
    s := Items[i - 1];
    if GetHRefValue(s,href,h) then
    begin
      chk := Checked[i-1];
      com := Comment[i-1];
      ind := Indent[i-1];
      SetHrefValue(s,href,value);
      FHoverHyperLink := -1;
      FUpdateIndex := i - 1;
      BeginUpdate;
      Items[i - 1] := s;
      Checked[i - 1] := chk;
      Comment[i - 1] := com;
      Indent[i - 1] := ind;
      EndUpdate;
      Break;
    end;
  end;
end;



procedure TParamCheckList.SetHover(const Value: boolean);
begin
  FHover := Value;
  Invalidate;
end;

procedure TParamCheckList.SetImageList(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;

procedure TParamCheckList.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;
  inherited;
end;

procedure TParamCheckList.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  Invalidate;
end;

procedure TParamCheckList.SetShadowOffset(const Value: integer);
begin
  FShadowOffset := Value;
  Invalidate;
end;

procedure TParamCheckList.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SendMessage(Handle,WM_SETREDRAW,integer(False),0);
  Inc(FUpdateCount);
end;

procedure TParamCheckList.EndUpdate;
begin
  if FUpdateCount>0 then
  begin
    dec(FUpdateCount);
    if FUpdateCount = 0 then
      SendMessage(Handle,WM_SETREDRAW,integer(True),0);
  end;
end;

procedure TParamCheckList.WMEraseBkGnd(var Message: TMessage);
begin
 if (fUpdateCount)>0 then
  Message.Result := 0 else inherited;

 fUpdateCount := 0;
end;

procedure TParamCheckList.WMKillFocus(var Message: TMessage);
begin
  inherited;
  if self.Cursor <> FOldCursor then
    self.Cursor := FOldCursor;
end;

procedure TParamCheckList.WMVScroll(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TParamCheckList.SetParamHint(const Value: Boolean);
begin
  fParamHint := Value;
  if fParamHint and not ShowHint then ShowHint := true;
end;

procedure TParamCheckList.UpdateParam(href, value: string);
var
  OldValue: string;
begin
  if Itemindex >= 0 then
  begin
    Value := HTMLPrep(Value);
    OldValue := ItemParameter[ItemIndex,href];

    if Assigned(FOnParamSelect) then
      FOnParamSelect(Self,ItemIndex,href,Value);

    ItemParameter[ItemIndex,href] := Value;

    if OldValue <> Value then
     if Assigned(FOnParamChanged) then
       FOnParamChanged(Self,ItemIndex,href,OldValue,Value);
  end;
end;

function TParamCheckList.GetParamInfo(Index: Integer; HRef: string;
  var AValue, AClass, AProp,AHint: string): Boolean;
begin
  Result := ExtractParamInfo(Items[Index],HRef,AClass,AValue,AProp,AHint);
end;

procedure TParamCheckList.PrepareParam(Param: string; var Value: string);
begin
  if (Value = EmptyParam) and (EmptyParam <> '') then
    Value := '';

  Value := InvHTMLPrep(value);

  if Assigned(FOnParamPrepare) then
    FOnParamPrepare(Self,ItemIndex,Param,Value);

  if Assigned(FOnParamEditStart) then
    FOnParamEditStart(Self,ItemIndex, Param, Value);
end;

function TParamCheckList.HTMLPrep(s: string): string;
begin
  s := StringReplace(s,'&','&amp;',[rfReplaceAll]);
  s := StringReplace(s,'<','&lt;',[rfReplaceAll]);
  s := StringReplace(s,'>','&gt;',[rfReplaceAll]);
  s := StringReplace(s,'"','&quot;',[rfReplaceAll]);
  Result := s;
end;

function TParamCheckList.InvHTMLPrep(s: string): string;
begin
  s := StringReplace(s,'&lt;','<',[rfReplaceAll]);
  s := StringReplace(s,'&gt;','>',[rfReplaceAll]);
  s := StringReplace(s,'&amp;','&',[rfReplaceAll]);
  s := StringReplace(s,'&quot;','"',[rfReplaceAll]);

  Result := s;
end;

function TParamCheckList.GetParamRefCount: Integer;
var
  i: Integer;
  s: string;
begin
  Result := 0;

  for i := 1 to Items.Count do
  begin
    s := Uppercase(Items[i - 1]);
    while (pos('HREF=',s) > 0) do
    begin
      Result := Result  + 1 ;
      Delete(s,1, pos('HREF=',s) + 5);
    end;
  end;

end;

function TParamCheckList.GetParamRefs(Index: Integer): string;
var
  i,j: Integer;
  s: string;
begin
  j := 0;
  Result := '';

  for i := 1 to Items.Count do
  begin
    s := Uppercase(Items[i - 1]);
    while (pos('HREF="',s) > 0) do
    begin
      if (Index = j) then
      begin
        Delete(s,1, pos('HREF="',s) + 5);
        if pos('"',s) > 0 then
        begin
          Delete(s,pos('"',s), length(s));
          Result := s;
        end;
        Exit;
      end
      else
        j := j + 1;
      Delete(s,1, pos('HREF=',s) + 5);
    end;
  end;
end;

function TParamCheckList.GetParamRefIndex(href: string): Integer;
var
  i: Integer;
  s,su: string;
begin
  Result := -1;

  for i := 1 to Items.Count do
  begin
    s := Uppercase(Items[i - 1]);
    while (pos('HREF="',s) > 0) do
    begin
      Delete(s,1, pos('HREF="',s) + 5);
      if pos('"',s) > 0 then
      begin
        su := s;
        Delete(su,pos('"',su), Length(su));
        if Uppercase(href) = su then
        begin
          Result := i - 1;
          Exit;
        end;
      end;
    end;
  end;
end;


procedure TParamCheckList.ControlUpdate(Sender: TObject; Param,
  Text: string);
var
  s: string;
begin
  s := Text;
  if (s = '') and (EmptyParam <> '') then
    s := EmptyParam;

  if Assigned(FOnParamEditDone) then
    FOnParamEditDone(Self, ItemIndex, Param, s);

  UpdateParam(Param, s);
  FIsEditing := False;
end;

procedure TParamCheckList.SetLineSpacing(const Value: Integer);
begin
  FLineSpacing := Value;
  Invalidate;
end;

procedure TParamCheckList.SetSelectionColor(const Value: TColor);
begin
  FSelectionColor := Value;
  Invalidate;
end;

procedure TParamCheckList.SetSelectionFontColor(const Value: TColor);
begin
  FSelectionFontColor := Value;
  Invalidate;
end;

procedure TParamCheckList.KeyDown(var Key: Word; Shift: TShiftState);
var
  idx: Integer;
  cr,hr,ir: TRect;
  CID,CV,CT: string;
begin
  if key in [VK_LEFT, VK_RIGHT] then
  begin
    idx := ItemIndex;
    if (idx >= 0) and (idx < Items.Count) then
    begin
      SendMessage(Handle,LB_GETITEMRECT,ItemIndex,LParam(@ir));

      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar(items[itemindex]));
      {$ENDIF}

      IsParam(ir.Left + 2,ir.Top + 2,False,idx,cr,hr,CID,CV,CT);

      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('num:'+inttostr(fnumhyperlinks)+':'+inttostr(ffocuslink)));
      {$ENDIF}

      if FNumHyperLinks > 1 then
      begin
        if key = VK_LEFT then
        begin
          if FFocusLink > 0 then
            Dec(FFocusLink)
          else
            FFocusLink := FNumHyperLinks - 1;
          Key := 0;
          InvalidateRect(Handle,@ir,True);
        end;

        if key = VK_RIGHT then
        begin
          if FFocusLink < FNumHyperLinks - 1 then
            Inc(FFocusLink)
          else
            FFocusLink := 0;
          Key := 0;
          InvalidateRect(Handle,@ir,True);
        end;
      end;

      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('key focus link to : '+inttostr(ffocuslink)));
      {$ENDIF}
    end;
  end;

  inherited;

  if Key = VK_F2 then
  begin
    if (FFocusLink <> -1) and (ItemIndex >= 0) and (ItemIndex < Items.Count) then
    begin
      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('start edit for : '+inttostr(ffocuslink)));
      {$ENDIF}
      EditParam(GetParamItemRefs(ItemIndex,FFocusLink));
    end;
  end;

end;

function TParamCheckList.GetParamIndex(href: string): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 1 to ParamRefCount do
  begin
    if StrIComp(pchar(ParamRefs[i - 1]),pchar(href))=0 then
    begin
      Result := i - 1;
      Break;
    end;
  end;
end;

function TParamCheckList.GetParamRect(href: string): TRect;
var
  i: integer;
  cr: TRect;
  CID,CV,CT: string;
begin
  i := FFocusLink + 1;
  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('get rect for '+inttostr(i)));
  {$ENDIF}
  IsParam(0,0,True,i,Result,cr,CID,CV,CT);
end;

procedure TParamCheckList.EditParam(href: string);
var
  idx: Integer;
begin
  idx := GetParamRefIndex(href);
  if idx < 0 then
    Exit;
  ItemIndex := idx;

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('edit param : '+href));
  {$ENDIF}
  StartParamEdit(href, ItemIndex, GetParamRect(href));
end;

{$WARNINGS OFF}
function EditCallBack (Wnd: HWND; uMsg: UINT; wParam, lpData: LPARAM): Integer; stdcall;
var
  Temp: String;
  pt: TPoint;
  r: TRect;
begin
  if uMsg = BFFM_INITIALIZED then
  begin
    with TParamCheckList(lpData) Do
    begin

      // avoid platform specific warning
      if FEditValue = '' then
        Temp := GetCurrentDir
      else
        Temp := ExcludeTrailingBackslash (FEditValue);

      SendMessage (Wnd, BFFM_SETSELECTION, 1, LParam(PChar(Temp)));

      with TParamCheckList(lpData) do
      begin
        pt := FEditPos;
        pt := ClientToScreen(pt);
        GetWindowRect(Wnd,r);

        if pt.X + (r.Right - r.Left) > Screen.DesktopWidth then
          pt.X := pt.X - (r.Right - r.Left);

        if pt.Y + (r.Bottom - r.Top) < Screen.DesktopHeight then
          SetWindowPos(wnd,HWND_NOTOPMOST,pt.X,pt.Y,0,0,SWP_NOSIZE or SWP_NOZORDER)
        else
          SetWindowPos(wnd,HWND_NOTOPMOST,pt.X,pt.Y-(r.Bottom - r.Top)-Height,0,0,SWP_NOSIZE or SWP_NOZORDER)
      end;
    end;
  end;
  Result := 0;
end;
{$WARNINGS ON}

procedure TParamCheckList.StartParamDir(idx: integer; param, curdir: string; hr: TRect);
var
  bi: TBrowseInfo;
  iIdList: PItemIDList;
  ResStr: array[0..MAX_PATH] of char;
  MAlloc: IMalloc;
  s:string;

  // BIF_NONEWFOLDERBUTTON
begin
  FillChar(bi, sizeof(bi), #0);

  with bi do
  begin
    if curdir <> '' then
      StrPCopy(ResStr,curdir)
    else
      StrPCopy(ResStr,GetCurrentDir);

    FEditValue := resstr;
    FEditPos := Point(hr.Left,hr.Bottom);

    hwndOwner := Application.Handle;
    pszDisplayName := ResStr;

    lpszTitle := PChar('Select directory');

    ulFlags := BIF_RETURNONLYFSDIRS;
    lpfn := EditCallBack;
    lParam := Integer(Self);
  end;

  iIdList := Nil;
  try
    iIdList := SHBrowseForFolder(bi);
  except
  end;

  if iIdList <> Nil then
  begin
    try
      FillChar(ResStr,sizeof(ResStr),#0);
      if SHGetPathFromIDList (iIdList, ResStr) then
      begin
        s := resstr;
        if Assigned(OnParamChanged) then
          OnParamChanged(Self, idx, param, curdir, s);

        SetParam(Param,s);
      end;
    finally
      SHGetMalloc(MAlloc);
      Malloc.Free(iIdList);
    end;
  end;
end;

function TParamCheckList.GetParamItemRefs(Item,Index: Integer): string;
var
  j: Integer;
  s: string;
begin
  j := 0;
  Result := '';

  s := Uppercase(Items[Item]);

  while (pos('HREF="',s) > 0) do
  begin
    if (Index = j) then
    begin
      Delete(s,1, pos('HREF="',s) + 5);
      if pos('"',s) > 0 then
      begin
        Delete(s,pos('"',s), length(s));
        Result := s;
      end;
      Exit;
    end
    else
      j := j + 1;
    Delete(s,1, pos('HREF=',s) + 5);
  end;

  if Index >= j then
  begin
    Result := GetParamItemRefs(Item,0);
    if Result <> '' then
      FFocusLink := 0;
  end;    

end;

function TParamCheckList.GetParamItemIndex(Index: Integer; href: string): Integer;
var
  j: Integer;
  s,u: string;
begin
  j := 0;
  Result := -1;

  s := Uppercase(Items[Index]);

  while (pos('HREF="',s) > 0) do
  begin
    Delete(s,1, pos('HREF="',s) + 5);
    if pos('"',s) > 0 then
    begin
      u := s;
      Delete(u,pos('"',u), length(u));
      if UpperCase(href) = u then
      begin
        Result := j;
        Exit;
      end;
    end;
    j := j + 1;
    Delete(s,1, pos('"',s));
  end;
end;


function TParamCheckList.GetParamItemRefCount(Item: Integer): Integer;
var
  s: string;
begin
  Result := 0;

  s := Uppercase(Items[Item]);
  while (pos('HREF=',s) > 0) do
  begin
    Result := Result  + 1 ;
    Delete(s,1, pos('HREF=',s) + 5);
  end;

end;

procedure TParamCheckList.AdvanceEdit(Sender: TObject);
var
  idx: Integer;
  s,v,c,p,h: string;
begin
  if not FAdvanceOnReturn then
    Exit;

  if FFocusLink = -1 then
    Exit;

  idx := FFocusLink;
  s  := ParamItemRefs[ItemIndex,idx];
  idx := ParamIndex[s];

  if idx < ParamRefCount - 1 then
    inc(idx)
  else
    idx := 0;

  s := ParamRefs[idx];

  if (s <> '') then
  begin
    idx := ParamRefIndex[s];
    ItemIndex := idx;
    FFocusLink := ParamItemIndex[idx, s];
    GetParamInfo(idx,s,v,c,p,h);
    if c <> '' then
      StartParamEdit(s,idx,GetParamRect(s));
  end;
end;

function TParamCheckList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TParamCheckList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TParamCheckList.SetVersion(const Value: string);
begin

end;

initialization
  GetCheckSize;

end.
