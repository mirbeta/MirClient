
{**************************************************************************}
{ TParamListBox component                                                  }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ Copyright © 2000-2013                                                    }
{   TMS Software                                                           }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit ParamListbox;

{$I TMSDEFS.INC}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Menus,
  Spin, ComCtrls, ParHTML, PictureContainer, Dialogs, Mask, ExtCtrls, Types
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
  // 1.3.1.0 : improved positioning of directory select dialog on multimonitor machines
  // 1.3.3.0 : Fixed issue with spinedit
  // 1.3.3.1 : Fixed issue with OnParamChanged for DIR type param
  // 1.3.3.2 : Fixed issue in combination with use on a TAdvToolPanel
  // 1.3.3.3 : Fixed issue with spin edit inplace editor and AdvFocusHelper
  // 1.3.3.4 : Improved : Date format compatibility for inplace date picker


type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}
  
  EHTMListBoxError = class(Exception);

  {$IFDEF USEBARSTYLE}
  TBarStyle = (tbsLowered, tbsRaised, tbsColor);
  {$ENDIF}


  TAnchorClick = procedure(Sender:TObject;index: Integer;anchor: string) of object;

  TParamListBoxClickEvent = procedure (Sender:TObject;idx: Integer; href: string;var value: string) of object;
  TParamListBoxPopupEvent = procedure (Sender:TObject;idx: Integer; href: string;values: TStringlist;var DoPopup: Boolean) of object;
  TParamListBoxSelectEvent = procedure (Sender:TObject;idx: Integer; href,value: string) of object;
  TParamListBoxChangedEvent = procedure (Sender:TObject;idx: Integer; href,oldvalue:string; var newvalue: string) of object;
  TParamListBoxHintEvent = procedure (Sender:TObject; idx:Integer; href: string; var hintvalue: string; var showhint: Boolean) of object;

  TParamCustomEditEvent = procedure(Sender: TObject; idx: Integer; href, value, props: string; EditRect: TRect) of object;

  TParamListBoxEditEvent = procedure (Sender:TObject;idx: Integer; href: string;var value: string) of object;

  TParamItemControlEvent = procedure(Sender: TObject; X,Y: Integer;
    Item: Integer; ControlID, ControlType, ControlValue:string) of object;

  TParamControlHintEvent = procedure(Sender: TObject; ControlID: string; var Hint: string; var CanShow: Boolean) of object;

  TParamListBox = class;

  TOwnerDrawState = Windows.TOwnerDrawState;
  {$NODEFINE TOwnerDrawState}

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TParamListBox = class(TCustomListBox)
  private
    { Private declarations }
    FBlinking: Boolean;
    FOldCursor: Integer;
    FOldAnchor: string;
    FOnParamClick: TParamListBoxClickEvent;
    FOnParamPopup: TParamListBoxPopupEvent;
    FOnParamList: TParamListBoxPopupEvent;
    FOnParamSelect: TParamListBoxSelectEvent;
    FOnParamChanged: TParamListBoxChangedEvent;
    FOnParamHint: TParamListBoxHintEvent;
    FImages: TImageList;
    FMultiLine: Boolean;
    FParamColor: TColor;
    FSelectionColor: TColor;
    FSelectionFonTColor: TColor;
    FIsMeasuring: Boolean;
    FTimerID: Integer;
    FEnableBlink: boolean;
    FShadowOffset: integer;
    FShadowColor: TColor;
    FParamHint: Boolean;
    FParamPopup: TPopupMenu;
    FParamList: TPopupListBox;
    FParamDatePicker: TPopupDatePicker;
    FParamSpinEdit: TPopupSpinEdit;
    FParamEdit: TPopupEdit;
    FParamMaskEdit: TPopupMaskEdit;
    FParamListSorted: Boolean;
    FOldParam: string;
    FHover: Boolean;
    FHoverIdx: Integer;
    FHoverHyperLink: Integer;
    FHoverColor: TColor;
    FHoverFontColor: TColor;
    FCurrHoverRect: TRect;
    FShowSelection: Boolean;
    FFocusLink: Integer;
    FFocusItem: Integer;
    FNumHyperLinks: Integer;
    Redraw: Boolean;
    FUpdateCount: Integer;
    FMouseDown: Boolean;
    FCurrCtrlID: string;
    FCurrCtrlRect: TRect;
    FCurrCtrlDown: TRect;
    FContainer: TPictureContainer;
    FImageCache:  THTMLPictureCache;
    FOnParamExit: TParamListBoxSelectEvent;
    FOnParamEnter: TParamListBoxSelectEvent;
    FOnParamPrepare: TParamListBoxClickEvent;
    FOnControlClick: TParamItemControlEvent;
    FOnControlHint: TParamControlHintEvent;
    FEditAutoSize: Boolean;
    FLineSpacing: Integer;
    FOnParamEditStart: TParamListBoxEditEvent;
    FOnParamEditDone: TParamListBoxEditEvent;
    FEmptyParam: string;
    FEditValue: string;
    FEditPos: TPoint;
    FIsEditing: Boolean;
    FOnParamQuery: TParamListBoxEditEvent;
    FOnParamCustomEdit: TParamCustomEditEvent;
    FAdvanceOnReturn: Boolean;
    {$IFDEF USEBARSTYLE}
    FBarStyle: TBarStyle;
    {$ENDIF}
    procedure ReMeasure;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMTimer(var Msg: TWMTimer); message WM_Timer;
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure WMKillFocus(var Message:TMessage); message WM_KILLFOCUS;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMHintShow(Var Msg: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(Var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetImages(value : TImageList);
    procedure SetMultiLine(value : boolean);
    procedure SetParamColor(const Value : TColor);
    procedure SetSelectionColor(const Value : TColor);
    procedure SetSelectionFonTColor(const Value : TColor);
    function GetTextItem(index: Integer): string;
    procedure SetEnableBlink(const Value: boolean);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: integer);
    function IsParam(x,y: Integer;GetFocusRect: Boolean;var Idx: Integer;var hoverrect,ctrlrect:TRect;var CID,CT,CV: string): string;
    function GetParam(href: string): string;
    procedure SetParam(href: string; const Value: string);
    procedure HandlePopup(Sender:TObject);
    procedure SetHoverColor(const Value: TColor);
    procedure SetHoverFonTColor(const Value: TColor);
    procedure SetParamHint(const Value: boolean);
    function GetItemParam(idx: integer; href: string): string;
    procedure SetItemParam(idx: integer; href: string; const Value: string);
    procedure UpdateParam(href:string; value:string);
    procedure PrepareParam(Param:string; var Value:string);

    function GetParamRefCount: Integer;
    function GetParamRefs(Index: Integer): string;
    function GetParamItemRefs(Item,Index: Integer): string;
    function GetParamRefIndex(href: string): Integer;
    procedure ControlUpdate(Sender: TObject; Param,Text:string);
    procedure AdvanceEdit(Sender: TObject);
    procedure SetLineSpacing(const Value: Integer);
    function GetParamIndex(href: string): Integer;
    function GetParamRect(href: string): TRect;
    procedure StartParamEdit(param:string;Index: Integer; hr: TRect);
    procedure StartParamDir(idx: integer; param,curdir:string; hr: TRect);
    function GetParamItemIndex(Index: Integer; href: string): Integer;
    function GetParamItemRefCount(Item: Integer): Integer;
    {$IFDEF USEBARSTYLE}
    procedure SetBarStyle( S: TBarStyle );
    {$ENDIF}
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    { Protected declarations }
    function GetVersionNr: Integer; virtual;
    procedure WndProc(var Message: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMLButtonDown( var Msg : TWMLButtonDown ); message WM_LBUTTONDOWN;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DrawItem(Index: Integer; Rect: TRect;State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    {$IFDEF DELPHI6_LVL}
    procedure SelectAll; override;
    {$ENDIF}

    procedure EditParam(idx:Integer; href: string);
    property TextItems[index: Integer]: string read GetTextItem;
    property Parameter[href: string]: string read GetParam write SetParam;
    property ParamRefCount: Integer read GetParamRefCount;
    property ParamRefs[Index: Integer]:string read GetParamRefs;
    property ParamItemRefCount[Item: Integer]: Integer read GetParamItemRefCount;
    property ParamItemRefs[Item,Index: Integer]:string read GetParamItemRefs;
    property ParamRefIndex[href: string]: Integer read GetParamRefIndex;
    property ParamIndex[href: string]: Integer read GetParamIndex;
    property ParamItemIndex[Index: Integer; href: string]: Integer read GetParamItemIndex;
    property ParamRect[href: string]: TRect read GetParamRect;
    function GetParamInfo(Index: Integer; HRef:string; var AValue, AClass, AProp, AHint: string): Boolean;
    property ItemParameter[idx: Integer;href: string]: string read GetItemParam write SetItemParam;
    procedure BeginUpdate;
    procedure EndUpdate;
    property DateTimePicker: TPopupDatePicker read FParamDatePicker;
    property SpinEdit: TPopupSpinEdit read FParamSpinEdit;
    property Editor: TPopupEdit read FParamEdit;
    property MaskEditor: TPopupMaskEdit read FParamMaskEdit;
    property ListBox: TPopupListBox read FParamList;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property Items;
    property ItemHeight;
    property MultiSelect;
    property ParentCtl3D;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;

    property AdvanceOnReturn: Boolean read FAdvanceOnReturn write FAdvanceOnReturn;
    property EditAutoSize: Boolean read FEditAutoSize write FEditAutoSize default False;
    property EmptyParam: string read FEmptyParam write FEmptyParam;
    property EnableBlink: Boolean read FEnableBlink write SetEnableBlink default False;
    property Hover: Boolean read FHover write FHover default True;
    property HoverColor: TColor read FHoverColor write SetHoverColor default clGreen;
    property HoverFontColor: TColor read FHoverFonTColor write SetHoverFontColor default clWhite;
    property Images:TImageList read FImages write SetImages;
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing default 0;
    property Multiline: Boolean read FMultiLine write SetMultiline;
    property ParamHint: Boolean read FParamHint write SetParamHint;
    property ParamColor:TColor read FParamColor write SetParamColor default clGreen;
    {$IFDEF USEBARSTYLE}
    property BarStyle: TBarStyle read FBarStyle write SetBarStyle;
    {$ENDIF}
    property ParamListSorted: Boolean read FParamListSorted write FParamListSorted default False;
    property SelectionColor:TColor read FSelectionColor write SetSelectionColor default clHighlight;
    property SelectionFontColor:TColor read fSelectionFontColor write SetSelectionFontColor default clHighLightText;
    property ShadowColor:TColor read fShadowColor write SetShadowColor;
    property ShadowOffset: Integer read fShadowOffset write SetShadowOffset;
    property ShowSelection: Boolean read fShowSelection write fShowSelection;
    property Visible;
    property Version: string read GetVersion write SetVersion;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnParamClick: TParamListBoxClickEvent read FOnParamClick write FOnParamClick;
    property OnParamPrepare: TParamListBoxClickEvent read FOnParamPrepare write FOnParamPrepare;
    property OnParamEditStart: TParamListBoxEditEvent read FOnParamEditStart write FOnParamEditStart;
    property OnParamEditDone: TParamListBoxEditEvent read FOnParamEditDone write FOnParamEditDone;
    property OnParamQuery: TParamListBoxEditEvent read FOnParamQuery write FOnParamQuery;
    property OnParamPopup: TParamListBoxPopupEvent read FOnParamPopup write FOnParamPopup;
    property OnParamList: TParamListBoxPopupEvent read FOnParamList write FOnParamList;
    property OnParamSelect: TParamListBoxSelectEvent read FOnParamSelect write FOnParamSelect;
    property OnParamChanged: TParamListBoxChangedEvent read FOnParamChanged write FOnParamChanged;
    property OnParamHint: TParamListBoxHintEvent read FOnParamHint write FOnParamHint;
    property OnParamEnter: TParamListBoxSelectEvent read FOnParamEnter write FOnParamEnter;
    property OnParamExit: TParamListBoxSelectEvent read FOnParamExit write FOnParamExit;
    property OnParamCustomEdit: TParamCustomEditEvent read FOnParamCustomEdit write FOnParamCustomEdit;
    property OnControlClick: TParamItemControlEvent read FOnControlClick write FOnControlClick;
    property OnControlHint: TParamControlHintEvent read FOnControlHint write FOnControlHint;
  end;

implementation

uses
  CommCtrl, ShellApi, Forms,  ShlObj, ActiveX, ImgList
  {$IFDEF DELPHI6_LVL}
  , Variants
  {$ENDIF}
  ;

procedure TParamListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if Integer(itemID) >= 0 then DrawItem(itemID, rcItem, State);
    Canvas.Handle := 0;
  end;
end;

{$IFDEF DELPHI6_LVL}
procedure TParamListBox.SelectAll;
    var topIdx, focus : Integer;
begin
    BeginUpdate();
    focus := ItemIndex;
    topIdx := TopIndex;
    inherited SelectAll();
    TopIndex := topIdx;
    ItemIndex := focus;
    EndUpdate();
end;
{$ENDIF}

{$IFDEF USEBARSTYLE}
procedure TParamListBox.SetBarStyle( S: TBarStyle );
var r: TRect;
begin
   if s <> FBarStyle then begin
     FBarStyle := S;
     if ItemIndex > -1 then begin
        if MultiSelect and (SelCount > 1) then invalidate
        else begin
          r := ItemRect( ItemIndex );
          InvalidateRect( Handle, @r, false );
        end;
     end;
   end;
end;
{$ENDIF}

procedure TParamListBox.DrawItem(Index: Integer; Rect: TRect;State: TOwnerDrawState);
var
  a,s,f: string;
  xsize,ysize,ml,hl: Integer;
  urlcol: TColor;
  hrect,hr,cr: TRect;
  pt: TPoint;
  FHyperlink,fl: Integer;
  CID,CV,CT: string;
  FHC, FHFC: TColor;
begin

  if (odSelected in State) and FShowSelection and
    (SelectionColor <> clNone) and (SelectionFontColor <> clNone) then
  begin
    Canvas.Brush.Color := FSelectionColor;
    Canvas.Pen.Color := FSelectionColor;
    Canvas.Font.Color := FSelectionFontColor;
    urlcol := FSelectionFontColor;
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Font.Color := Font.Color;
    urlcol := FParamColor;
  end;

  if not redraw then
  begin
    Canvas.Rectangle(rect.left,rect.top,rect.right,rect.bottom);

    if Index = Items.Count - 1 then
    begin
      Canvas.Brush.Color := Color;
      Canvas.Pen.Color := Color;
      Canvas.Rectangle(rect.left,rect.bottom,rect.right,ClientRect.bottom);
    end;
  end;

  {$IFDEF USEBARSTYLE}
    If (odSelected in State) then Begin
    if FBarStyle = tbsLowered then
       Frame3d(Canvas, Rect, clBtnShadow, clBtnHighLight, 1 )
    else if FBarStyle = tbsRaised then
       Frame3d(Canvas, Rect, clBtnHighLight, clBtnShadow, 1 )
    end;
  {$ENDIF}

  hrect := Rect;
  hrect.Top := hrect.Top + 2;

  GetCursorPos(pt);
  pt := self.ScreenToClient(pt);

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


  HTMLDrawEx(Canvas,Items[Index],hrect,FImages,pt.x,pt.y,fl,FHyperLink,FShadowOffset,
    False,False,False,False,(odSelected in State) and FShowSelection, FHover ,Multiline,FMouseDown,False,1.0,urlCol,
    FHC,FHFC,clGray,a,s,f,xsize,ysize,hl,ml,hr,cr,CID,CV,CT,
    FImageCache,FContainer,Handle,FLineSpacing);

  // redraw when current number of hyperlinks is smaller than FFocusLink
  if (fl >= hl) then
  begin
    fl := hl - 1;

    HTMLDrawEx(Canvas,Items[Index],hrect,FImages,pt.x,pt.y,fl,FHyperLink,FShadowOffset,
      False,False,False,False,(odSelected in State) and FShowSelection,FHover ,Multiline,FMouseDown,False,1.0,urlCol,
      FHC,FHFC,clGray,a,s,f,xsize,ysize,hl,ml,hr,cr,CID,CV,CT,
      FImageCache,FContainer,Handle,FLineSpacing);
  end;

  if (odFocused in State) and
    FShowSelection then DrawFocusRect(Canvas.Handle,Rect);
end;

procedure TParamListBox.CMMouseLeave(Var Msg: TMessage);
begin
  inherited;
  if FHover and (FHoverHyperLink<>-1) then
    InvalidateRect(self.handle,@fCurrHoverRect,true);
  FHoverHyperLink:=-1;

  if (FOldAnchor <> '') and Assigned(FOnParamExit) then
    FOnParamExit(self,-1,FOldParam,Parameter[fOldParam]);

  FOldAnchor := '';
end;

Procedure TParamListBox.CMHintShow(Var Msg: TMessage);
var
  CanShow: Boolean;
  hi: PHintInfo;
  Anchor,CID,CV,CT: string;
  res: Integer;
  hr,cr:trect;
  v,c,p,h: string;

Begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);

  if FParamHint and not FIsEditing then
  begin
    Anchor := IsParam(hi^.cursorPos.x,hi^.cursorpos.y,False,res,hr,cr,CID,CT,CV);
    if (Anchor <> '') then begin
        GetParamInfo(res,Anchor,v,c,p,h);

        if h <> '' then
          anchor := h;

        if (CID <> '') then
        begin
          Anchor := '';
          hi^.HintPos := ClientToScreen(hi^.CursorPos);
          hi^.hintpos.y := hi^.hintpos.y-10;
          hi^.hintpos.x := hi^.hintpos.x+10;
          if Assigned(FOnControlHint) then
            FOnControlHint(self,CID,Anchor,CanShow);

         hi^.HintStr := Anchor;
        end;

        if (Anchor <> '') and (CID = '') then
        begin
          hi^.HintPos := ClientToScreen(hi^.CursorPos);
          hi^.hintpos.y := hi^.hintpos.y-10;
          hi^.hintpos.x := hi^.hintpos.x+10;
          if Assigned(FOnParamHint) then
            FOnParamHint(Self, res, Anchor,Anchor,CanShow);

         hi^.HintStr := anchor;
        end;
    end;
  end;
  Msg.Result := Ord(Not CanShow);
end;


procedure TParamListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if not FMultiline then
  begin
    Height := ItemHeight;
    Exit;
  end;
end;

constructor TParamListBox.Create(aOwner: tComponent);
begin
  inherited Create(aOwner);
  Style := lbOwnerDrawVariable;
  FIsMeasuring := False;
  FParamColor := clGreen;
  FSelectionColor := clHighLight;
  FSelectionFontColor := clHighLightText;
  FHover := True;
  FHoverColor := clGreen;
  FHoverFontColor := clWhite;
  FTimerID := 0;
  FEnableBlink := False;
  FShadowColor := clGray;
  FShadowOffset := 1;
  FOldAnchor := '';
  FShowSelection := True;
  FUpdateCount := 0;
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

  FImageCache :=  THTMLPictureCache.Create;
  FMouseDown := False;
  DoubleBuffered := True;
end;

destructor TParamListBox.Destroy;
begin
  FImageCache.Free;
  FParamPopup.Free;
  FParamList.Free;
  FParamDatePicker.Free;
  FParamSpinEdit.Free;
  FParamEdit.Free;
  FParamMaskEdit.Free;
  inherited;
end;

procedure TParamListBox.Loaded;
begin
  inherited;
  FOldCursor := self.Cursor;
  if FEnableBlink and (FTimerID = 0) then
    FTimerID := SetTimer(Self.Handle,1,500,nil);

  if not FEnableBlink and (FTimerID <> 0) then
    KillTimer(Self.Handle,FTimerID);
end;

procedure TParamListBox.SetImages(Value: TImagelist);
begin
  FImages := Value;
  ReMeasure;
end;

procedure TParamListBox.SetParamColor(const Value:TColor);
begin
  if (value <> FParamColor) then
  begin
    FParamColor := Value;
    Invalidate;
  end;
end;

procedure TParamListBox.SetSelectionColor(const Value: TColor);
begin
  if (value <> FSelectionColor) then
  begin
    FSelectionColor := value;
    Invalidate;
  end;
end;

procedure TParamListBox.SetSelectionFonTColor(const Value: TColor);
begin
  if (value <> FSelectionFonTColor) then
  begin
    FSelectionFonTColor := Value;
    Invalidate;
  end;
end;


procedure TParamListBox.SetMultiLine(value: Boolean);
begin
  if (value <> FMultiline) then
  begin
    FMultiline := value;
    ReMeasure;
  end;
end;

function TParamListBox.GetTextItem(index: Integer): string;
begin
  if (index >= 0) and (index < Self.Items.Count) then
  begin
    Result := HTMLStrip(Items[Index]);
  end
  else
    raise EHTMListboxError.Create('Item index out of range');
end;

procedure TParamListBox.WndProc(var Message: TMessage);
var
  r,hr,cr:trect;
  xsize,ysize,ml,hl: Integer;
  a,s,f: string;
  CID,CV,CT: string;
  NewHeight: Integer;
begin
  inherited;

  if (message.msg = WM_DESTROY) then
  begin
    if FEnableBlink and (FTimerID <> 0) then
      KillTimer(handle,fTimerID);
  end;

  if (message.msg = LB_ADDSTRING) or
     (message.msg = LB_INSERTSTRING) then
  begin
    SendMessage(Handle,lb_getitemrect,Message.Result,LParam(@r));

    r.bottom := r.top + 1000;
    HTMLDrawEx(Canvas,Items[Message.result],r,fImages,0,0,0,-1,fShadowOffset,
      True,True,False,True,True,False,Multiline,False,False,1.0,
      FParamColor,clNone,clNone,fShadowColor,a,s,f,xsize,ysize,hl,ml,hr,cr,
      CID,CV,CT,FImageCache,FContainer,Handle,FLineSpacing);

    NewHeight := YSize + 4;

    if NewHeight < ItemHeight then
      NewHeight := ItemHeight;

    SendMessage(Handle,lb_setitemheight,Message.Result,NewHeight);
  end;
end;

procedure TParamListBox.ReMeasure;
var
  i: Integer;
  {$IFDEF DELPHI6_LVL}
  sel: Boolean;
  {$ENDIF}
begin
  fIsMeasuring:=true;
  for i:=1 to self.Items.Count do
  begin
    {$IFDEF DELPHI6_LVL}
    sel := inherited Selected[i-1];
    {$ENDIF}
    self.Items[i-1]:=self.Items[i-1];
    {$IFDEF DELPHI6_LVL}
    inherited Selected[i-1] := sel;
    {$ENDIF}
  end;
  fIsMeasuring:= False;
end;

function TParamListBox.IsParam(x,y: Integer;GetFocusRect: Boolean;var idx: Integer;var hoverrect,ctrlrect:TRect; var CID,CT,CV:string): string;
var
  res: Integer;
  r:trect;
  anchor,stripped,f: string;
  xsize,ysize: Integer;
begin
  Result:='';

  CID := '';
  FNumHyperLinks := 0;

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

    r.Top := r.Top + 2;

    if HTMLDrawEx(Canvas,Items[res],r,FImages,X,Y,idx,-1,FShadowOffset,
      True,False,False,True,True,False,Multiline,False,GetFocusRect,1.0,FParamColor,
      clNone,clNone,fShadowColor,anchor,stripped,f,xsize,ysize,FNumHyperLinks,FHoverHyperLink,hoverrect,ctrlrect,
      CID,CV,CT,FImageCache,FContainer,Handle,FLineSpacing) then
        Result := Anchor;
  end;
end;

procedure TParamListBox.HandlePopup(Sender:TObject);
var
  newvalue,oldvalue: string;
begin
  with (Sender as TMenuItem) do
  begin
    newvalue := Caption;

    while (pos('&',newvalue)>0) do
      system.Delete(newvalue,pos('&',newvalue),1);

    oldvalue := ItemParameter[ItemIndex,FOldParam];

    if Assigned(FOnParamSelect) then
      FOnParamSelect(self,ItemIndex,FOldParam,NewValue);

    if (oldvalue <> newvalue) then begin
      if Assigned(FOnParamChanged) then begin
        FOnParamChanged(self,ItemIndex,FOldParam,oldvalue,newvalue);
      end;
    end;
    ItemParameter[ItemIndex,FOldParam] := newvalue;
  end;
end;

procedure TParamListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
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


//procedure TParamListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
//  X, Y: Integer);
procedure TParamListBox.WMLButtonDown( var Msg : TWMLButtonDown );
var
  hr,cr:trect;
  Index, X, Y: Integer;
  CID,CT,CV: string;
  param:string;
begin
  X := Msg.XPos; Y := Msg.YPos;

  param := IsParam(X,Y,False,Index,hr,cr,CID,CT,CV);

  if param = '' then
  begin
    inherited;
  end;

  FMouseDown := true;

  if CID <> '' then
  begin
    InvalidateRect(Handle,@cr,true);
    FCurrCtrlDown := cr;
  end
  else
    FCurrCtrlDown := Rect(-1,-1,-1,-1);

  if param <> '' then
  begin
    ItemIndex := index;
    StartParamEdit(param,Index,hr);
  end;
end;


procedure TParamListBox.StartParamEdit(param: string; Index: Integer; hr: TRect);
var
  I: Integer;
  oldvalue,newvalue,v,c,p,h: string;
  cr: TRect;
  pt: TPoint;
  doPopup,doList: Boolean;
  newvalues:TStringList;
  newmenu:TMenuItem;
  CID,CT,CV: string;

  function Max(a,b:Integer): Integer;
  begin
    if a > b then
      Result := a
    else
      Result := b;
  end;

begin
  OffsetRect(hr,0,LineSpacing);


  if (hr.Left > Width) then
  begin
    hr := ItemRect(Index);
  end;

  if param <> '' then
  begin
    GetParamInfo(Index,Param,v,c,p,h);

    FFocusItem := Index;
    FFocusLink := ParamItemIndex[Index,param];
    {$IFDEF TMSDEBUG}
    outputdebugstring(pchar('set focus link to : '+inttostr(ffocuslink)));
    {$ENDIF}
    {
    if Assigned(FOnParamClick) and (c = '') then
    begin
      FIsEditing := True;
      PrepareParam(Param,v);
      oldvalue := v;
      FOnParamClick(Self,Index,param,v);
      if (v <> oldvalue) then
        ControlUpdate(self,Param,v);
      FIsEditing := False;
    end;
    }

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


    if Assigned(FOnParamClick) then
    begin
      GetHRefValue(self.Items[Index],param,oldvalue);
      newvalue := oldvalue;
      FIsEditing := True;
      PrepareParam(Param,v);

      FOnParamClick(self,Index,param,newvalue);
      if (newvalue<>oldvalue) then
      begin
        if Assigned(FOnParamChanged) then
          FOnParamChanged(self,Index,param,oldvalue,newvalue);

        ItemParameter[Index,param] := NewValue;
        GetCursorPos(pt);
        pt := Self.screentoclient(pt);
        IsParam(pt.x,pt.y,False,index,fCurrHoverRect,cr,CID,CT,CV);
      end;
      FIsEditing := False;
    end;

    if (c = 'MENU') then
    begin
      FIsEditing := True;
      GetHRefValue(self.Items[Index],param,oldvalue);
      newvalue:=oldvalue;
      NewValues:=TStringList.Create;
      NewValues.Sorted:=fParamListSorted;

      doPopup := True;

      if c = 'MENU' then
        PropToList(InvHTMLPrep(p),NewValues);

      if Assigned(FOnParamPopup) then
        FOnParamPopup(self,Index,param,newvalues,dopopup);

      if doPopup then
      begin
        pt := ClientToScreen(point(hr.left,hr.bottom));

        while FParamPopup.Items.Count > 0 do
          FParamPopup.Items[0].Free;
          

        FParamPopup.AutoHotkeys := maManual;

        PrepareParam(Param,OldValue);

        for i := 1 to NewValues.Count do
        begin
          newmenu := TMenuItem.Create(Self);
          newmenu.Caption := NewValues.Strings[i-1];
          newmenu.OnClick := HandlePopup;
          FParamPopup.Items.Add(newmenu);
        end;

        FOldParam := param;
        FParamPopup.Popup(pt.x,pt.y + 2);
      end;
      NewValues.Free;
      FIsEditing := False;      
    end;

    if (c = 'LIST') then
    begin
      FIsEditing := True;
      doList := True;
      GetHRefValue(self.Items[Index],param,oldvalue);
      newvalue := oldvalue;
      NewValues := TStringList.Create;
      NewValues.Sorted := FParamListSorted;

      dolist := True;

      if c = 'LIST' then
        PropToList(InvHTMLPrep(p),NewValues);

      if Assigned(FOnParamList) then
        FOnParamList(self,Index,param,newvalues,dolist);

      if doList then
      begin
        pt := ClientToScreen(point(hr.left,hr.bottom));

        //outputdebugstring(pchar(inttostr(pt.x) + ':' + inttostr(pt.y)));

        FParamList.Top := pt.y;
        FParamList.Left := pt.x;
        FParamlist.OnUpdate := ControlUpdate;
        FParamList.OnReturn := AdvanceEdit;
        FParamlist.Param := param;
        FParamList.Parent := Self;

        SetWindowLong( FParamList.Handle, GWL_EXSTYLE,
                 GetWindowLong(FParamList.Handle, GWL_EXSTYLE) or
                 WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);

        PrepareParam(Param,OldValue);

        FParamlist.Visible := True;
        FParamList.Items.Assign(NewValues);
        FParamList.Ctl3D := False;
        FParamList.SizeDropDownWidth;
        FParamList.ItemIndex := FParamList.Items.IndexOf(oldvalue);
        FParamList.SetFocus;
      end;
      NewValues.Free;
    end;

    if c = 'DATE' then
    begin
      FIsEditing := True;
      pt := ClientToScreen(Point(hr.left,hr.top));

      FParamDatePicker.Top := pt.Y - 2;
      FParamDatePicker.Left := pt.X;
      FParamDatePicker.Width := Max(64,hr.Right - hr.Left);
      FParamDatePicker.ReInit;

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
      FParamDatePicker.Left := pt.X;
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
      FParamSpinEdit.Left := pt.X;
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

    if (c = 'EDIT') then
    begin
      FIsEditing := True;
      pt := ClientToScreen(Point(hr.left,hr.top));

      FParamEdit.Top := pt.Y - 2;
      FParamEdit.Left := pt.X;
      FParamEdit.Width := Max(16,hr.Right - hr.Left) + 16;

      FParamEdit.Cancelled := False;
      FParamEdit.AutoSize := EditAutoSize;
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
      FParamMaskEdit.Left := pt.X;
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
end;

procedure TParamListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  param,CID,CT,CV: string;
  idx: Integer;
  hr,cr:trect;
begin
  inherited MouseMove(Shift,X,Y);

  if FIsEditing then
    Exit;

  param := IsParam(x,y,False,idx,hr,cr,CID,CT,CV);

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

  if (param<>'') then
  begin
    if (FHover) then
    begin
      if (idx<>fHoverIdx) or not equalrect(hr,fCurrHoverRect) then
        InvalidateRect(self.handle,@fCurrHoverRect,true);
    end;

    FHoverIdx := idx;

    if (FOldAnchor <> param) then
    begin
      if Assigned(FOnParamEnter) then
        FOnParamEnter(self,idx,Param,ItemParameter[idx,Param]);
      Application.Cancelhint;
    end;

    if (self.Cursor<>crHandPoint) then
    begin
      fOldCursor:=self.Cursor;
      self.Cursor:=crHandPoint;
      if fHover then InvalidateRect(self.handle,@hr,true);
      fCurrHoverRect:=hr;
    end;
    fOldAnchor:=param;
  end
  else
    if (self.Cursor=crHandPoint) and (fOldAnchor<>'') then
    begin
      if Assigned(FOnParamExit) then
        FOnParamExit(self,idx,fOldParam,ItemParameter[idx,fOldParam]);
      Application.CancelHint;
      Cursor := FOldCursor;
      FOldAnchor := '';
      if fHover then InvalidateRect(self.handle,@fCurrHoverRect,true);
    end;
end;

procedure TParamListBox.WMSize(var Msg: TWMSize);
begin
  inherited;
  if not fIsMeasuring and MultiLine then self.ReMeasure;
end;

procedure TParamListBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (aOperation=opRemove) and (aComponent=fImages) then fImages:=nil;
  inherited;
end;

procedure TParamListBox.WMTimer(var Msg: TWMTimer);
var
  i,i1,i2: Integer;
  r,cr,hr:trect;
  a,s,fa: string;
  xsize,ysize: Integer;
  sel: Boolean;
  hl,ml: Integer;
  CID,CV,CT:string;

begin
  if (Items.Count = 0) or not FEnableBlink then
    Exit;

  r := GetClientRect;
  i1 := SendMessage(Handle,lb_itemfrompoint,0,makelparam(0,r.Top));
  i2 := SendMessage(Handle,lb_itemfrompoint,0,makelparam(0,r.Bottom));

  if i1 < 0 then
    i1 := 0;

  if i2 > items.Count - 1 then
    i2 := Items.Count - 1;

  for i := i1 to i2 do
  begin
    // only redraw items with blinking

    if Pos('<BLINK',Items[i]) > 0 then
    begin
      SendMessage(handle,lb_getitemrect,i,LParam(@r));

      sel := SendMessage(Handle,lb_getsel,i,0)>0;

      if not sel then
      begin
        Canvas.Brush.Color := Color;
        Canvas.Font.Color := Font.Color;
      end
      else
      begin
        Canvas.Brush.Color := SelectionColor;
        Canvas.Font.Color := SelectionFontColor;
      end;
      
      HTMLDrawEx(Canvas,items[i],r,FImages,0,0,0,-1,2,False,False,False,sel,FBlinking,FHover,Multiline,FMouseDown,False,1.0,FParamColor,
        HoverColor,HoverFontColor,clGray, a,s,fa,xsize,ysize,hl,ml,hr,cr,CID,CV,CT,FImageCache,FContainer,Handle,FLineSpacing);
    end;
  end;
  FBlinking := not FBlinking;
end;

procedure TParamListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if FIsEditing then
    SetFocus;
end;

procedure TParamListBox.SetEnableBlink(const Value: boolean);
begin
  FEnableBlink := Value;

  if not (csLoading in ComponentState) then
  begin
    if FEnableBlink and (FTimerID = 0) then
      FTimerID := SetTimer(Handle,1,500,nil);
      
    if not FEnableBlink and (FTimerID <> 0) then
    begin
      KillTimer(Handle,FTimerID);
      FTimerID := 0;
      FBlinking := False;
      Invalidate;
    end;
  end;
end;

procedure TParamListBox.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  Invalidate;
end;

procedure TParamListBox.SetShadowOffset(const Value: integer);
begin
  FShadowOffset := Value;
  Invalidate;
end;


function TParamListBox.GetItemParam(idx: integer; href: string): string;
var
  h: string;
begin
  Result := '';

  if (idx < 0) or (idx > Self.Items.Count - 1) then
    raise Exception.Create('Invalid item index');

  if GetHRefValue(Items[idx],href,h) then
    Result := InvHTMLPrep(h);
end;

procedure TParamListBox.SetItemParam(idx: integer; href: string;
  const Value: string);
var
  h,v,s: string;
  sel: boolean;
  selidx: Integer;
begin
  if (idx < 0) or (idx > Items.Count - 1) then
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


function TParamListBox.GetParam(href: string): string;
var
  i: Integer;
  h: string;
begin
  for i := 1 to Items.Count do
  begin
    if GetHRefValue(Items[i-1],href,h) then
    begin
      Result := InvHTMLPrep(h);
      Break;
    end;
  end;
end;

procedure TParamListBox.SetParam(href: string; const Value: string);
var
  i: Integer;
  h,s,v: string;
begin
  BeginUpdate;

  FHoverIdx:=-1;

  v := HTMLPrep(Value);

  for i:=1 to Items.Count do
  begin
    s := Items[i-1];
    if GetHRefvalue(s,href,h) then
    begin
      SetHRefValue(s,href,v);
      Items[i-1] := s;
      Break;
    end;
  end;

  EndUpdate;
end;

procedure TParamListBox.SetHoverColor(const Value: TColor);
begin
  FHoverColor := Value;
  Invalidate;
end;

procedure TParamListBox.SetHoverFontColor(const Value: TColor);
begin
  FHoverFonTColor := Value;
  Invalidate;
end;

procedure TParamListBox.BeginUpdate;
begin
  Items.BeginUpdate();
  if FUpdateCount = 0 then
    SendMessage(Handle,WM_SETREDRAW,integer(False),0);
  Inc(FUpdateCount);
end;

procedure TParamListBox.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Items.EndUpdate();
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      SendMessage(Handle,WM_SETREDRAW,integer(True),0);
  end;
end;


procedure TParamListBox.WMEraseBkGnd(var Message: TMessage);
begin
  if FUpdateCount > 0 then
    message.Result := 0
  else
    inherited;
  // FUpdateCount := 0;
end;


procedure TParamListBox.SetParamHint(const Value: boolean);
begin
  FParamHint := Value;
  if FParamHint and not ShowHint then
    ShowHint := True;
end;

procedure TParamListBox.WMKillFocus(var Message: TMessage);
begin
  inherited;
  if Cursor <> FOldCursor then
    Cursor := FOldCursor;
end;

procedure TParamListBox.ControlUpdate(Sender: TObject; Param,Text:string);
var
  s: string;
begin
  s := Text;
  if (s = '') and (EmptyParam <> '') then
    s := EmptyParam;

  if Assigned(FOnParamEditDone) then
    FOnParamEditDone(Self,ItemIndex, Param, s);

  UpdateParam(Param,s);
  FIsEditing := False;
end;

procedure TParamListBox.UpdateParam(href, value: string);
var
  OldValue, NewValue: string;
begin
  if Itemindex >= 0 then
  begin
    NewValue := HTMLPrep(Value);

    OldValue := ItemParameter[ItemIndex,href];

    if Assigned(FOnParamSelect) then
      FOnParamSelect(Self,ItemIndex,href,NewValue);

    if NewValue <> HTMLPrep(Value) then
      Value := NewValue;

    if OldValue <> Value then begin
        if Assigned(FOnParamChanged) then begin
            FOnParamChanged(Self,ItemIndex,href,OldValue,Value);
        end;
    end;

    ItemParameter[ItemIndex,href] := Value;
  end;
end;

function TParamListBox.GetParamInfo(Index: Integer; HRef: string;
  var AValue, AClass, AProp, AHint: string): Boolean;
begin
  Result := ExtractParamInfo(Items[Index],HRef,AClass,AValue,AProp,AHint);
end;

procedure TParamListBox.PrepareParam(Param: string; var Value: string);
begin
  if (Value = EmptyParam) and (EmptyParam <> '') then
    Value := '';

  Value := InvHTMLPrep(value);

  if Assigned(FOnParamPrepare) then
    FOnParamPrepare(Self,ItemIndex,Param,Value);

  if Assigned(FOnParamEditStart) then
    FOnParamEditStart(Self,ItemIndex, Param, Value);
end;


function TParamListBox.GetParamRefCount: Integer;
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

function TParamListBox.GetParamRefs(Index: Integer): string;
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

function TParamListBox.GetParamRefIndex(href: string): Integer;
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
        Delete(su,pos('"',su), length(su));
        if Uppercase(href) = su then
        begin
          Result := i - 1;
          Exit;
        end;
      end;

    end;
  end;
end;

procedure TParamListBox.SetLineSpacing(const Value: Integer);
begin
  FLineSpacing := Value;
  Invalidate;
end;

procedure TParamListBox.KeyDown(var Key: Word; Shift: TShiftState);
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
      EditParam(ItemIndex, GetParamItemRefs(ItemIndex,FFocusLink));
    end;
  end;

end;

function TParamListBox.GetParamIndex(href: string): Integer;
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

function TParamListBox.GetParamRect(href: string): TRect;
var
  i: integer;
  cr: TRect;
  CID,CV,CT: string;
begin
{$IFDEF TMSDEBUG}
  outputdebugstring(pchar('get rect:'+inttostr(i)));
{$ENDIF}
  i := FFocusLink + 1;
  IsParam(0,0,True,i,Result,cr,CID,CV,CT);
end;

procedure TParamListBox.EditParam(idx: Integer; href: string);
begin
  if idx < 0 then Exit;
  ItemIndex := idx;
  StartParamEdit(href, idx, GetParamRect(href));
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
    with TParamListBox (lpData) Do
    begin
      // avoid platform specific warning
      if FEditValue = '' then
        Temp := GetCurrentDir
      else
        Temp := ExcludeTrailingBackslash(FEditValue);

      SendMessage (Wnd, BFFM_SETSELECTION, 1, LParam(PChar(Temp)));

      with TParamListBox(lpData) do
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

procedure TParamListBox.StartParamDir(idx: integer; param, curdir: string; hr: TRect);
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
      SHGetMalloc(Malloc);
      Malloc.Free(iIdList);
    end;
  end;
end;

procedure TParamListBox.KeyPress(var Key: Char);
begin
  inherited;
  if (FFocusLink <> -1) and (ItemIndex >= 0) and (ItemIndex < Items.Count) and (Key = #13) then
  begin
{$IFDEF TMSDEBUG}
    outputdebugstring(pchar('start edit for : '+inttostr(ffocuslink)));
{$ENDIF}
    if GetParamItemRefCount(ItemIndex) > 0 then
      EditParam(ItemIndex, GetParamItemRefs(ItemIndex,FFocusLink));
  end;
end;

function TParamListBox.GetParamItemRefs(Item,Index: Integer): string;
var
  j: Integer;
  s: string;
  flg: boolean;
begin
  j := 0;
  Result := '';

  if (ParamItemRefCount[Item] = 0) then
    Exit;

  s := Uppercase(Items[Item]);

  flg := false;

  while (pos('HREF="',s) > 0) do
  begin
    flg := true;
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

  if (Index >= j) and flg then
  begin
    Result := GetParamItemRefs(Item,0);
    if Result <> '' then
      FFocusLink := 0;
  end;

end;

function TParamListBox.GetParamItemIndex(Index: Integer; href: string): Integer;
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


function TParamListBox.GetParamItemRefCount(Item: Integer): Integer;
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

procedure TParamListBox.AdvanceEdit(Sender: TObject);
var
  idx: Integer;
  s,v,c,p,h:string;
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
    GetParamInfo(idx,s, v,c,p,h);
    if c <> '' then
      StartParamEdit(s,idx,GetParamRect(s));
  end;
end;

function TParamListBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TParamListBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TParamListBox.SetVersion(const Value: string);
begin

end;

end.
