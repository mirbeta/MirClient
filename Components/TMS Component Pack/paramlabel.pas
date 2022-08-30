{*************************************************************************}
{ TParamLabel component                                                   }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 1999-2012                                        }
{            Email : info@tmssoftware.com                                 }
{            Website : http://www.tmssoftware.com/                        }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit ParamLabel;

{$I TMSDEFS.INC}

{$DEFINE REMOVESTRIP}
{$DEFINE REMOVEDRAW}
{$DEFINE PARAMS}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ShellApi, ExtCtrls, Menus, ParHTML, PictureContainer,
  Math, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 3; // Release nr.
  BLD_VER = 6; // Build nr.

  // version history
  // 1.3.0.1 : improved mask editor property handling
  // 1.3.1.0 : improved positioning of directory select dialog on multimonitor machines
  // 1.3.3.0 : Fixed issue with spinedit
  // 1.3.3.1 : Fixed issue with initialization of Transparent property for Delphi 2007
  // 1.3.3.2 : Fixed issue with handling Alt key when inplace editor is active
  // 1.3.3.3 : Fixed compatibility issue with TAdvFocusHelper
  // 1.3.3.4 : Fixed issue with end editing in Delphi 2007 and older Delphi versions
  // 1.3.3.5 : Fixed : transparent initialization for Delphi 2009+
  // 1.3.3.6 : Improved : Date format compatibility for inplace date picker

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}
  
  TParamLabelClickEvent = procedure (Sender:TObject; href:string;var value:string) of object;
  TParamLabelPopupEvent = procedure (Sender:TObject; href:string;values:TStringlist;var DoPopup:boolean) of object;
  TParamLabelSelectEvent = procedure (Sender:TObject; href,value:string) of object;
  TParamLabelChangedEvent = procedure (Sender:TObject; href,oldvalue,newvalue:string) of object;
  TParamLabelHintEvent = procedure (Sender:TObject; href:string; var hintvalue:string; var showhint:boolean) of object;

  TParamCustomEditEvent = procedure(Sender: TObject; href, value, props: string; EditRect: TRect) of object;

  TParamLabelEditEvent = procedure (Sender:TObject; href: string;var value: string) of object;

  TParamItemControlEvent = procedure(Sender: TObject; X,Y: Integer;
     ControlID, ControlType, ControlValue:string) of object;

  TParamControlHintEvent = procedure(Sender: TObject; ControlID: string; var Hint: string; var CanShow: Boolean) of object;

  TParamLabel = class;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TParamLabel = class(TStaticText)
  private
    { Private declarations }
    FAnchor: String;
    FAutoSizing: boolean;
    FHTMLText: TStringList;
    FOnParamClick: TParamLabelClickEvent;
    FOnParamPopup: TParamLabelPopupEvent;
    FOnParamList: TParamLabelPopupEvent;
    FOnParamSelect: TParamLabelSelectEvent;
    FOnParamChanged: TParamLabelChangedEvent;
    FOnParamHint: TParamLabelHintEvent;
    FParamHint: boolean;
    FImages: TImageList;
    Fupdatecount: Integer;
    FParamColor: TColor;
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBevelWidth: TBevelWidth;
    FBorderWidth: TBorderWidth;
    FBorderStyle: TBorderStyle;
    FShadowOffset: Integer;
    FShadowColor: TColor;
    FHover: boolean;
    FHoverHyperLink: Integer;
    FHyperLink: Integer;
    FHoverColor: TColor;
    FHoverFontColor: TColor;
    FCurrHoverRect: TRect;
    FParamPopup: TPopupMenu;
    FParamList: TPopupListBox;
    FParamDatePicker: TPopupDatePicker;
    FParamSpinEdit: TPopupSpinEdit;
    FParamEdit: TPopupEdit;
    FParamMaskEdit: TPopupMaskEdit;
    FOldParam: string;
    FEditValue: string;
    FEditPos: TPoint;
    FIsEditing: Boolean;
    FMouseDown: Boolean;
    FCurrCtrlID: string;
    FCurrCtrlRect: TRect;
    FCurrCtrlDown: TRect;
    FContainer: TPictureContainer;
    FImageCache:  THTMLPictureCache;
    FOnParamExit: TParamLabelSelectEvent;
    FOnParamEnter: TParamLabelSelectEvent;
    FOnControlClick: TParamItemControlEvent;
    FOnControlHint: TParamControlHintEvent;
    FParamListSorted: Boolean;
    FEditAutoSize: Boolean;
    FLineSpacing: Integer;
    FFocusLink: Integer;
    FNumLinks: Integer;
    FOnParamEditStart: TParamLabelEditEvent;
    FOnParamEditDone: TParamLabelEditEvent;
    FEmptyParam: string;
    FOldAnchor: string;
    FOnParamQuery: TParamLabelEditEvent;
    FOnParamCustomEdit: TParamCustomEditEvent;
    FAdvanceOnReturn: Boolean;
    procedure SetHTMLText(value : TStringList);
    procedure SetImages(value : TImageList);
    procedure SetParamColor(value : TColor);
    procedure SetAutoSizing(value : boolean);
    procedure HTMLChanged(Sender: TObject);
    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBevelWidth(Value: TBevelWidth);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetBorderStyle(Value: TBorderStyle);
    function IsParam(x,y,l:integer;GetFocusRect: Boolean; var hoverrect,cr:TRect; var CID, CT, CV: string):string;
    procedure CMHintShow(Var Msg: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(Var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure WMKeyDown(var Msg:TWMKeydown); message WM_KEYDOWN;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetShadowColor(const Value: tColor);
    procedure SetShadowOffset(const Value: integer);
    procedure SetHover(const Value: boolean);
    procedure SetHoverColor(const Value: TColor);
    procedure SetHoverFontColor(const Value: TColor);
    procedure HoverInvalidate(r:trect);
    function GetParam(href: string): string;
    procedure SetParam(href: string; const Value: string);
    procedure HandlePopup(Sender:TObject);
    procedure SetParamHint(const Value: boolean);
    procedure DoPaint(bkg: Boolean);
    function GetParamRefCount: Integer;
    function GetParamRefs(Index: Integer): string;

    procedure SetLineSpacing(const Value: Integer);
    procedure PrepareParam(Param:string; var Value:string);
    procedure SetFocusParam(const Value: Integer);
    function GetParamIndex(href: string): Integer;
    function GetParamRect(href: string): TRect;
    procedure StartParamEdit(param:string; hr: TRect);
    procedure StartParamDir(param,curdir:string; hr: TRect);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    function GetVersionNr: Integer; virtual;
    { Protected declarations }
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function GetDisplText:string; virtual;
    procedure UpdateDisplText; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure ControlUpdate(Sender: TObject; Param,Text: string);
    procedure AdvanceEdit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure EditParam(href: string);
    property Parameter[href: string]:string read GetParam write SetParam;
    property ParamRefCount: Integer read GetParamRefCount;
    property ParamRefs[Index: Integer]:string read GetParamRefs;
    property FocusParam: Integer read FFocusLink write SetFocusParam;
    property ParamIndex[href: string]: Integer read GetParamIndex;
    property ParamRect[href: string]: TRect read GetParamRect;
    function GetParamInfo(HRef:string; var AValue, AClass, AProp,AHint: string): Boolean;
    property ParamListBox: TPopupListbox read FParamList;
    property ParamEdit: TPopupEdit read FParamEdit;
    property ParamSpinEdit: TPopupSpinEdit read FParamSpinEdit;
    property ParamMaskEdit: TPopupMaskEdit read FParamMaskEdit;
    property ParamPopup: TPopupMenu read FParamPopup;
    property ParamDatePicker: TPopupDatePicker read FParamDatePicker;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentShowHint;
    property ParentColor;
    property ParentFont;                                                
    property PopupMenu;
    property ShowHint;
    property Color;
    {$IFDEF DELPHI7_LVL}
    property Transparent;
    {$ENDIF}
    property Hint;
    property Visible;

    property AdvanceOnReturn: Boolean read FAdvanceOnReturn write FAdvanceOnReturn;
    property AutoSizing: Boolean read FAutoSizing write SetAutoSizing;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvNone;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property EditAutoSize: Boolean read FEditAutoSize write FEditAutoSize default False;
    property EmptyParam: string read FEmptyParam write FEmptyParam;
    property Hover: Boolean read FHover write SetHover default True;
    property HoverColor: TColor read FHoverColor write SetHoverColor default clGreen;
    property HoverFontColor: TColor read FHoverFontColor write SetHoverFontColor default clWhite;
    property HTMLText: TStringList read FHTMLText write SetHTMLText;
    property Images: TImageList read FImages write SetImages;
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing default 0;
    property ParamColor: TColor read FParamColor write SetParamColor default clGreen;
    property ParamListSorted: Boolean read FParamListSorted write FParamListSorted default False;
    property ParamHint: Boolean read FParamHint write SetParamHint;
    property ShadowColor: TColor read FShadowColor write SetShadowColor;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset;
    property Version: string read GetVersion write SetVersion;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnParamClick: TParamLabelClickEvent read FOnParamClick write FOnParamClick;
    property OnParamPopup: TParamLabelPopupEvent read FOnParamPopup write FOnParamPopup;
    property OnParamList: TParamLabelPopupEvent read FOnParamList write FOnParamList;
    property OnParamSelect: TParamLabelSelectEvent read FOnParamSelect write FOnParamSelect;
    property OnParamChanged: TParamLabelChangedEvent read FOnParamChanged write FOnParamChanged;
    property OnParamHint: TParamLabelHintEvent read FOnParamHint write FOnParamHint;
    property OnParamEnter: TParamLabelSelectEvent read FOnParamEnter write FOnParamEnter;
    property OnParamExit: TParamLabelSelectEvent read FOnParamExit write FOnParamExit;
    property OnParamEditStart: TParamLabelEditEvent read FOnParamEditStart write FOnParamEditStart;
    property OnParamEditDone: TParamLabelEditEvent read FOnParamEditDone write FOnParamEditDone;
    property OnParamQuery: TParamLabelEditEvent read FOnParamQuery write FOnParamQuery;
    property OnParamCustomEdit: TParamCustomEditEvent read FOnParamCustomEdit write FOnParamCustomEdit;
    property OnControlClick: TParamItemControlEvent read FOnControlClick write FOnControlClick;
    property OnControlHint: TParamControlHintEvent read FOnControlHint write FOnControlHint;
  end;


implementation
uses
  Commctrl, ShlObj, ActiveX, ImgList
  {$IFDEF DELPHI6_LVL}
  , Variants
  {$ENDIF}
  ;



procedure TParamLabel.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TParamLabel.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    if (FUpdateCount = 0) then
    begin
      Invalidate;
    end;
  end;
end;

{$WARNINGS OFF}
procedure TParamLabel.DoPaint(bkg: boolean);
var
  r,mr,cr: TRect;
  x,y,mouselink,fl: Integer;
  s,anchor,stripped,focusanchor: string;
  TopColor, BottomColor: TColor;
  pt: TPoint;
  Canvas: TCanvas;
  CID,CV,CT: string;
  bmp: TBitmap;
  FHC,FHFC: TColor;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  if FUpdateCount > 0 then
    Exit;

  {$IFDEF DELPHI7_LVL}
  if not Transparent then
  begin
  {$ENDIF}
    bmp := TBitmap.Create;
    bmp.Width := Width;
    bmp.Height := Height;
    Canvas := bmp.Canvas;
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
  {$IFDEF DELPHI7_LVL}
  end
  else
  begin
    Canvas := TCanvas.Create;
    Canvas.Handle := GetDC(self.Handle);
    Canvas.Brush.Color := Self.Color;
  end;
  {$ENDIF}

  if Assigned(self.Font) then
    Canvas.Font.Assign(self.Font);

  R := GetClientRect;
  {$IFDEF DELPHI7_LVL}
  if not Transparent then
  {$ENDIF}
    Canvas.Rectangle(r.Left,r.Top,r.Right,r.Bottom);

  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, R, TopColor, BottomColor, BevelWidth);
  end;

  Frame3D(Canvas, R, Color, Color, BorderWidth);

  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, R, TopColor, BottomColor, BevelWidth);
  end;

  if (FBorderStyle = bsSingle) and (FBorderWidth > 0) then
  begin
    Canvas.Pen.Width := FBorderWidth;
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(r.left,r.top,r.right,r.bottom);
  end;

  if (bevelInner <> bvNone) or (bevelOuter <> bvNone) then
  begin
    InflateRect(r,-BevelWidth,-BevelWidth);
  end;

  if (FBorderStyle = bsSingle) then
  begin
    InflateRect(r,-BorderWidth,-BorderWidth);
  end;

  s := GetDisplText;

  Canvas.Brush.Color := self.Color;

  if FAutoSizing then
  begin
    if (Align = alLeft) or (Align = alRight) or (Align = alNone) then
      r.Right := r.Right + $FFFF;
    if (Align = alTop) or (Align = alBottom) or (Align = alNone) then
      r.Bottom := r.Bottom + 250;
  end;

  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('paint:'+inttostr(fhyperlink)));
  {$ENDIF}

  if GetFocus = Handle then
    fl := FFocusLink
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

  HTMLDrawEx(Canvas,s,r,fImages,pt.x,pt.y,fl,FHyperLink,FShadowOffset,false,false,false,false,false,FHover,
    True,FMouseDown,False,1.0,FParamColor,FHC,FHFC,fShadowColor,anchor,stripped,focusanchor,x,y,FNumLinks,
    mouselink,mr,cr,CID,CV,CT,FImageCache,FContainer,Handle,FLineSpacing);

  if FAutoSizing then
  begin
    if (Align=alTop) or (Align=alBottom) or (Align=alNone) then
      if (y+6<>height) then height:=y+6;
    if (Align=alLeft) or (Align=alRight) or (Align=alNone) then
      if (x+6<>width) then width:=x+6;
  end;

  {$IFDEF DELPHI7_LVL}
  if not Transparent then
  begin
  {$ENDIF}
    Canvas := TCanvas.Create;
    Canvas.Handle := GetDC(self.Handle);
    Canvas.Draw(0,0,bmp);
    ReleaseDC(self.Handle,Canvas.Handle);
    Canvas.Free;
    bmp.Free;
  {$IFDEF DELPHI7_LVL}
  end
  else
  begin
    ReleaseDC(self.Handle,Canvas.Handle);
    Canvas.Free;
  end;
  {$ENDIF}
end;
{$WARNINGS ON}

constructor TParamLabel.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSizing := False;
  FHTMLText := TStringList.Create;
  FHTMLText.OnChange := HTMLChanged;
  Caption := '';
  AutoSize := False;
  FUpdateCount := 0;
  FParamColor := clGreen;
  FShadowColor := clGray;
  FShadowOffset := 2;
  BevelWidth := 1;
  FEmptyParam := '?';
  FBorderStyle := bsNone;
  FHover := True;
  FHoverHyperLink := -1;
  FHoverColor := clGreen;
  FHoverFontColor := clWhite;
  FParamPopup := TPopupMenu.Create(Self);

  DoubleBuffered := true;

  // default focus parameter is first parameter
  FFocusLink := 0;

  FParamList := TPopupListbox.Create(Self);
  FParamList.Cursor := crDefault;
  FParamList.Width := 100;
  FParamList.Height := 100;
  FParamList.Top := 0;
  FParamList.Left := 0;
  FParamList.Visible := False;
  FIsEditing := False;
  FImageCache := THTMLPictureCache.Create;

  FParamMaskEdit := TPopupMaskEdit.Create(Self);
  FParamEdit := TPopupEdit.Create(Self);
  FParamSpinEdit := TPopupSpinEdit.Create(Self);
  FParamDatePicker := TPopupDatePicker.Create(Self);

  {$IFDEF DELPHI7_LVL}

  {$IFNDEF DELPHI_UNICODE}
  Transparent := False;
  {$ENDIF}

  {$IFDEF DELPHI_UNICODE}
  Transparent := True;
  {$ENDIF}

  {$ENDIF}

  Width := 100;
  Height := 32;
end;

destructor TParamLabel.Destroy;
begin
  FHTMLText.Free;
  FParamPopup.Free;
  if not FIsEditing then
    FParamList.Free;
  FParamMaskEdit.Free;
  FParamEdit.Free;
  FParamSpinEdit.Free;
  FParamDatePicker.Free;
  FImageCache.Free;
  inherited;
end;

procedure TParamLabel.HTMLChanged(sender:TObject);
begin
  Invalidate;
end;

procedure TParamLabel.SetAutoSizing(value : boolean);
begin
  FAutoSizing := Value;
  if FAutoSizing then
  begin
    if (Align=alLeft) or (Align=alRight) then Width:=6;
    if (Align=alTop) or (Align=alBottom) then Height:=6;
  end;
  Invalidate;
end;

procedure TParamLabel.SetHTMLText(value:TStringlist);
begin
  if assigned(value) then
  FHTMLText.Assign(value);
  UpdateDisplText;
end;

procedure TParamLabel.UpdateDisplText;
begin
  Invalidate;
end;

procedure TParamLabel.SetImages(value:TImagelist);
begin
  FImages := Value;
  Invalidate;
end;

procedure TParamLabel.SetParamColor(value:TColor);
begin
  if value <> FParamColor then
  begin
    FParamColor := value;
    Invalidate;
  end;
end;

procedure TParamLabel.Loaded;
begin
  inherited;
  Caption := '';
end;

function TParamLabel.IsParam(x,y,l:integer;GetFocusRect: Boolean; var hoverrect,cr:TRect; var CID,CT,CV:string):string;
var
  r: TRect;
  xsize,ysize:integer;
  s:string;
  anchor,stripped,focusanchor:string;
  hl:integer;
  Canvas: TCanvas;

begin
  Canvas := TCanvas.Create;
  Canvas.Handle := GetDC(self.Handle);

  r := Clientrect;

  if (BevelInner <> bvNone) or (BevelOuter <> bvNone) then
  begin
    InflateRect(r,-BevelWidth,-BevelWidth);
  end;

  if (FBorderStyle = bsSingle) then
  begin
    InflateRect(r,-BorderWidth,-BorderWidth);
  end;

  s := GetDisplText;

  Anchor := '';

  Canvas.Font := Font;

  if HTMLDrawEx(canvas,s,r,FImages,x,y,l,-1,FShadowOffset,True,False,False,False,False,FHover,True,FMouseDown,GetFocusRect,
    1.0, clWhite,clNone,clNone,clNone,anchor,stripped,focusanchor,xsize,ysize,hl,FHoverHyperlink,hoverrect,cr,
    CID,CV,CT,FImageCache,FContainer,Handle,FLineSpacing) then
  begin
    Result := Anchor;
    Inflaterect(hoverrect,1,1);
  end;

  ReleaseDC(self.Handle,Canvas.Handle);
  Canvas.Free;
end;

procedure TParamLabel.HoverInvalidate(r: TRect);
begin
  InvalidateRect(Handle,@r,true);
end;

procedure TParamLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  anchor:string;
  hr,cr: TRect;
  CID,CV,CT: string;
begin
  Anchor := IsParam(x,y,FFocusLink,False,hr,cr,CID,CT,CV);

  if Anchor <> FOldAnchor then
    Application.CancelHint;

  FOldAnchor := Anchor;

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

  if (Anchor <> '') then
  begin
    if not FIsEditing then
    begin
      FHyperLink := FHoverHyperLink;
      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('set hyperlink here to :'+inttostr(fhyperlink)));
      {$ENDIF}
    end;

    if (FAnchor <> Anchor) or not EqualRect(FCurrHoverRect,hr) or (FHoverHyperlink = -1) then
    begin
      if FHover then
        HoverInvalidate(hr);
    end;

    if  (FAnchor <> Anchor) then
    begin
      if Assigned(FOnParamEnter) then
        FOnParamEnter(self,Anchor,Parameter[Anchor]);

      if FParamHint then Application.CancelHint;
      if (Cursor = crDefault) then
        Cursor := crHandPoint;
      if FHover then
        HoverInvalidate(FCurrHoverRect);
     end;

     FAnchor := Anchor;
     FCurrHoverRect := hr;
  end
  else
  begin
    if (Cursor = crHandPoint) then
    begin
      if Assigned(FOnParamExit) then
        FOnParamEnter(self,FAnchor,Parameter[fAnchor]);
      Cursor := crDefault;
      if FHover then
        HoverInvalidate(FCurrHoverRect);
      FAnchor := '';
      FHyperlink := -1;
    end;
  end;
end;

procedure TParamLabel.HandlePopup(Sender:TObject);
var
  newvalue,oldvalue:string;
begin
  with (Sender as TMenuItem) do
  begin
    newvalue := Caption;
    while (pos('&',newvalue)>0) do
      system.delete(newvalue,pos('&',newvalue),1);

    oldvalue := Parameter[fOldParam];

    if Assigned(FOnParamSelect) then
      FOnParamSelect(self,fOldParam,newvalue);

    Parameter[FOldParam] := newvalue;

    if (oldvalue <> newvalue) then
      if Assigned(FOnParamChanged) then
        FOnParamChanged(self,fOldParam,oldvalue,newvalue);
  end;
end;


procedure TParamLabel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  hr,cr: TRect;
  CID,CT,CV,s: string;

begin
  inherited;
  FMouseDown := False;

  IsParam(X,Y,FFocusLink,False,hr,cr,CID,CT,CV);

  if CID <> '' then
  begin
    if CT = 'CHECK' then
    begin
      s := HTMLText.Text;

      if Uppercase(CV) = 'TRUE' then
        SetControlValue(s,CID,'FALSE')
      else
        SetControlValue(s,CID,'TRUE');

      HTMLText.Text := s;
    end;

    if Assigned(FOnControlClick) then
       FOnControlClick(Self,X,Y,CID,CT,CV);

    if FCurrCtrlDown.Left <> -1 then
      InvalidateRect(Handle,@FCurrCtrlDown,true);
  end;

  FCurrCtrlDown := Rect(-1,-1,-1,-1);
end;



procedure TParamLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  param:string;
  hr,cr:trect;
  CID,CV,CT:string;

begin
  inherited MouseDown(Button,Shift,X,Y);

  if TabStop then
    SetFocus;

  param := IsParam(X,Y,FFocusLink,False,hr,cr,CID,CV,CT);

  FMouseDown := true;

  if CID <> '' then
  begin
    InvalidateRect(Handle,@cr,true);
    FCurrCtrlDown := cr;
  end
  else
    FCurrCtrlDown := Rect(-1,-1,-1,-1);

  if (param <> '') then
     StartParamEdit(param,hr);
end;

procedure TParamLabel.StartParamEdit(param: string; hr: TRect);
var
  oldvalue,newvalue:string;
  pt:tpoint;
  doPopup,doList:boolean;
  newvalues:TStringList;
  newmenu:TMenuItem;
  i:integer;
  v,c,p,h: string;


begin
  OffsetRect(hr,0,LineSpacing);

  GetParamInfo(Param,v,c,p,h);
  FHyperLink := -1;

  FFocusLink := ParamIndex[param];

  if Assigned(FOnParamClick) and (c = '') then
  begin
    FIsEditing := True;
    PrepareParam(Param,v);
    oldvalue := v;
    FOnParamClick(Self,param,v);
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

    GetHRefValue(HTMLText.Text,param,oldvalue);
    newvalue := oldvalue;
    NewValues := TStringList.Create;
    NewValues.Sorted := FParamListSorted;

    doPopup := True;

    PropToList(InvHTMLPrep(p),NewValues);

    if Assigned(FOnParamPopup) then
      FOnParamPopup(self,param,newvalues,dopopup);

    if doPopup then
    begin
      pt := ClientToScreen(point(hr.left,hr.bottom));

      while FParamPopup.Items.Count > 0 do
        FParamPopup.Items[0].Free;

      FParamPopup.AutoHotkeys := maManual;

      for i := 1 to NewValues.Count do
      begin
        newmenu := TMenuItem.Create(Self);
        newmenu.Caption := NewValues.Strings[i-1];
        newmenu.OnClick := HandlePopup;
        FParamPopup.Items.Add(newmenu);
      end;

      PrepareParam(Param,oldvalue);

      FOldParam := param;
      FParamPopup.Popup(pt.x,pt.y+2);
    end;
    NewValues.Free;
  end;

  if (c = 'LIST') then
  begin
    FIsEditing := True;

    doList := True;
    GetHRefValue(HTMLText.Text,param,oldvalue);
    newvalue := oldvalue;
    NewValues := TStringList.Create;
    NewValues.Sorted := FParamListSorted;

    dolist := True;

    PropToList(InvHTMLPrep(p),NewValues);

    if Assigned(FOnParamList) then
      FOnParamList(self,param,newvalues,dolist);

    if doList then
    begin
      pt := ClientToScreen(point(hr.left,hr.bottom));
      FParamList.Top := pt.y;
      FParamList.Left := pt.x;
      FParamlist.OnUpdate := ControlUpdate;
      FParamList.OnReturn := AdvanceEdit;
      FParamlist.Param := param;
      FParamList.Parent := Self;

      SetWindowLong( FParamList.Handle, GWL_EXSTYLE,
                 GetWindowLong(FParamList.Handle, GWL_EXSTYLE) or
                 WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);

      PrepareParam(Param,oldvalue);

      FParamlist.visible := True;
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

  if c = 'EDIT' then
  begin
    FIsEditing := True;

    pt := ClientToScreen(Point(hr.left,hr.top));

    FParamEdit.Top := pt.Y - 2;
    FParamEdit.Left := pt.X;
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

  if c = 'DIR' then
  begin
    FIsEditing := True;
    PrepareParam(Param,v);
    StartParamDir(param,v,hr);
    FIsEditing := False;
  end;

  if c = 'MASK' then
  begin
    FIsEditing := True;

    pt := ClientToScreen(Point(hr.left,hr.top));

    FParamMaskEdit.Top := pt.Y - 2;
    FParamMaskEdit.Left := pt.X + 2;
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

  if  (c = 'QUERY') then
  begin
    PrepareParam(Param,v);
    if Assigned(OnParamQuery) then
      OnParamQuery(Self,Param,v);
    ControlUpdate(self,Param,v);
  end;

  if  (c = 'CUSTOM') then
  begin
    PrepareParam(Param,v);

    pt := ClientToScreen(Point(hr.left,hr.top));

    //pt := point(hr.Left,hr.Top);

    FIsEditing := True;

    if Assigned(OnParamCustomEdit) then
      OnParamCustomEdit(Self,Param,v,p,Rect(pt.x,pt.Y,pt.X + hr.Right - hr.Left,pt.Y + hr.Bottom - hr.Top));
  end;
end;

procedure TParamLabel.SetBevelInner(Value: TPanelBevel);
begin
  FBevelInner := Value;
  Invalidate;
end;

procedure TParamLabel.SetBevelOuter(Value: TPanelBevel);
begin
  FBevelOuter := Value;
  Invalidate;
end;

procedure TParamLabel.SetBevelWidth(Value: TBevelWidth);
begin
  FBevelWidth := Value;
  Invalidate;
end;

procedure TParamLabel.SetBorderWidth(Value: TBorderWidth);
begin
  FBorderWidth := Value;
  Invalidate;
end;

procedure TParamLabel.SetBorderStyle(Value: TBorderStyle);
begin
 FBorderStyle := Value;
 Invalidate;
end;

Procedure TParamLabel.CMHintShow(Var Msg: TMessage);
{$IFDEF DELPHI2_LVL}
type
 PHintInfo = ^THintInfo;
{$ENDIF}
var
  CanShow: Boolean;
  hi: PHintInfo;
  hr,cr:trect;
  anchor:string;
  CID,CV,CT: string;
  v,c,p,h: string;

Begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);

  if FParamHint and not FIsEditing then
  begin

    Anchor := IsParam(hi^.cursorPos.x,hi^.cursorpos.y,FFocusLink,False,hr,cr,CID,CV,CT);

    GetParamInfo(Anchor,v,c,p,h);

    if h <> '' then
      Anchor := h;


    if (Anchor <> '') then
    begin

      hi^.HintPos := clienttoscreen(hi^.CursorPos);
      hi^.hintpos.y := hi^.hintpos.y - 10;
      hi^.hintpos.x := hi^.hintpos.x + 10;

      if Assigned(FOnParamHint) then
        FOnParamHint(self,anchor,anchor,CanShow);

      hi^.HintStr := anchor;
    end;
  end;
  Msg.Result := Ord(Not CanShow);
end;


procedure TParamLabel.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (aOperation=opRemove) and (aComponent=fImages) then
    FImages := nil;
  inherited;
end;

procedure TParamLabel.SetShadowColor(const Value: tColor);
begin
  FShadowColor := Value;
  Invalidate;
end;

procedure TParamLabel.SetShadowOffset(const Value: integer);
begin
  FShadowOffset := Value;
  Invalidate;
end;

procedure TParamLabel.SetHover(const Value: boolean);
begin
  FHover := Value;
  Invalidate;
end;

procedure TParamLabel.SetHoverColor(const Value: TColor);
begin
  FHoverColor := Value;
  Invalidate;
end;

procedure TParamLabel.SetHoverFontColor(const Value: TColor);
begin
  FHoverFontColor := Value;
  Invalidate;
end;

procedure TParamLabel.CMMouseLeave(var Msg: TMessage);
begin
  inherited;

  if FHover and (FHoverHyperLink <> -1) then
    HoverInvalidate(FCurrHoverRect);

  FHoverHyperLink := -1;
  FHyperLink := -1;

  if (FAnchor<>'') and Assigned(FOnParamExit) then
    FOnParamExit(self,fAnchor,Parameter[fAnchor]);

  FAnchor := '';
end;

function TParamLabel.GetDisplText: string;
var
  i:integer;
begin
  Result := '';
  for i := 1 to FHTMLText.Count do
    Result := Result + FHTMLText.Strings[i - 1];
end;

function TParamLabel.GetParam(href: string): string;
var
  h:string;
begin
  if GetHRefValue(fHTMLText.text,href,h) then
  begin
    h := StringReplace(h,'&lt;','<',[rfReplaceAll]);
    h := StringReplace(h,'&gt;','>',[rfReplaceAll]);
    h := StringReplace(h,'&amp;','&',[rfReplaceAll]);
    h := StringReplace(h,'&quot;','"',[rfReplaceAll]);
    Result := h
  end
  else
    Result := '';
end;

procedure TParamLabel.SetParam(href: string; const Value: string);
var
  s,v: string;
begin
  s := FHTMLText.Text;

  v := value;
  v := StringReplace(v,'&','&amp;',[rfReplaceAll]);
  v := StringReplace(v,'<','&lt;',[rfReplaceAll]);
  v := StringReplace(v,'>','&gt;',[rfReplaceAll]);
  v := StringReplace(v,'"','&quot;',[rfReplaceAll]);

  if SetHRefValue(s,href,v) then
    FHTMLText.Text := s;
end;

procedure TParamLabel.SetParamHint(const Value: boolean);
begin
  fParamHint := Value;
  if fParamHint and not ShowHint then ShowHint := true;
end;

procedure TParamLabel.WMPaint(var Message: TWMPaint);
{$IFDEF DELPHI7_LVL}
var
  lpPaint: tagPaintStruct;
{$ENDIF}
begin
  {$IFNDEF DELPHI7_LVL}
  inherited;
  DoPaint(True);
  {$ELSE}
  if Transparent then
    inherited;

  if FUpdateCount > 0 then
    Exit;

  if not Transparent then
    BeginPaint(Handle,lpPaint);

  DoPaint(True);

  if not Transparent then
    EndPaint(Handle,lpPaint);
  {$ENDIF}
end;


procedure TParamLabel.WMEraseBkGnd(var Message: TMessage);
begin
  {$IFDEF DELPHI7_LVL}
  if not Transparent then
    Message.Result := 1
  else
  {$ENDIF}
    inherited;
end;

function TParamLabel.GetParamRefCount: Integer;
var
  s: string;
begin
  Result := 0;

  s := Uppercase(HTMLText.Text);
  while (pos('HREF=',s) > 0) do
  begin
    Result := Result  + 1 ;
    Delete(s,1, pos('HREF=',s) + 5);
  end;
end;

function TParamLabel.GetParamRefs(Index: Integer): string;
var
  j: Integer;
  s: string;
begin
  j := 0;
  Result := '';

  s := Uppercase(HTMLText.Text);
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

function TParamLabel.GetParamInfo(HRef: string;
  var AValue, AClass, AProp,AHint: string): Boolean;
begin
  Result := ExtractParamInfo(HTMLText.Text,HRef,AClass,AValue,AProp,AHint);
end;

procedure TParamLabel.ControlUpdate(Sender: TObject; Param, Text: string);
var
  s: string;
begin
  s := Text;
  if (s = '') and (EmptyParam <> '') then
    s := EmptyParam;

  if Assigned(FOnParamEditDone) then
    FOnParamEditDone(Self, Param, s);

  SetParam(Param,s);
  FIsEditing := False;
end;

procedure TParamLabel.AdvanceEdit(Sender: TObject);
var
  idx: Integer;
  s,v,c,p,h: string;
begin
  if not FAdvanceOnReturn then
    Exit;

  if FFocusLink = -1 then
    Exit;

  idx := FFocusLink;
  if idx < ParamRefCount - 1 then
    inc(idx)
  else
    idx := 0;

  s := ParamRefs[idx];

  if (s <> '') then
  begin
    GetParamInfo(s,v,c,p,h);
    if c <> '' then
      StartParamEdit(s,GetParamRect(s));
  end;    
end;

procedure TParamLabel.SetLineSpacing(const Value: Integer);
begin
  FLineSpacing := Value;
  Invalidate;
end;

procedure TParamLabel.PrepareParam(Param: string; var Value: string);
begin
  if (Value = EmptyParam) and (EmptyParam <> '') then
    Value := '';

  Value := InvHTMLPrep(Value);  

  if Assigned(FOnParamEditStart) then
    FOnParamEditStart(Self, Param, Value);
end;

procedure TParamLabel.DoEnter;
begin
  inherited;
  if TabStop then
    Invalidate;
end;

procedure TParamLabel.DoExit;
begin
  inherited;
  if TabStop then
    Invalidate;
end;

procedure TParamLabel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_F2 then
  begin
    if (FFocusLink <> -1) then
    begin
      EditParam(ParamRefs[FFocusLink]);
    end;
  end;
end;

procedure TParamLabel.WMKeyDown(var Msg: TWMKeydown);
begin
  if TabStop then
  begin
    if Msg.CharCode in [VK_LEFT,VK_UP] then
    begin
      if FFocusLink > 0 then
        dec(FFocusLink)
      else
        FFocusLink := FNumLinks - 1;
      Invalidate;
      Msg.Result := 1;
      Msg.CharCode := 0;
      Exit;
    end;

    if Msg.CharCode in [VK_RIGHT,VK_DOWN] then
    begin
      if FFocusLink < FNumLinks - 1 then
        inc(FFocusLink)
      else
        FFocusLink := 0;
      Invalidate;
      Msg.Result := 1;
      Msg.CharCode := 0;
      Exit;
    end;

  end;
  inherited;
end;

procedure TParamLabel.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if (Msg.CharCode in [VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN]) then
    Msg.Result := 1;
end;

procedure TParamLabel.SetFocusParam(const Value: Integer);
begin
  FFocusLink := Value;
  Invalidate;
end;

function TParamLabel.GetParamIndex(href: string): Integer;
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

procedure TParamLabel.EditParam(href: string);
begin
  StartParamEdit(href, GetParamRect(href));
end;

function TParamLabel.GetParamRect(href: string): TRect;
var
  i: integer;
  cr: TRect;
  CID,CV,CT: string;
begin
  i := GetParamIndex(href);
  IsParam(0,0,i + 1,True,Result,cr,CID,CV,CT);
end;

procedure TParamLabel.KeyPress(var Key: Char);
begin
  inherited;
  if (FFocusLink <> -1) and ((Key = #13) or (Key = #32)) and (ParamRefCount >  0) then
  begin
    EditParam(ParamRefs[FFocusLink]);
  end;
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
    with TParamLabel (lpData) Do
    begin

      // avoid platform specific warning
      if FEditValue = '' then
        Temp := GetCurrentDir
      else
        Temp := ExcludeTrailingBackslash (FEditValue);

      SendMessage (Wnd, BFFM_SETSELECTION, 1, LParam(PChar(Temp)));

      with TParamLabel(lpData) do
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

procedure TParamLabel.StartParamDir(param,curdir:string; hr: TRect);
var
  bi: TBrowseInfo;
  iIdList: PItemIDList;
  ResStr: array[0..MAX_PATH] of char;
  MAlloc: IMalloc;
  s: string;
  // BIF_NONEWFOLDERBUTTON
begin
  if curdir <> '' then
    StrPCopy(ResStr,curdir)
  else
    StrPCopy(ResStr,GetCurrentDir);

  FEditValue := resstr;
  FEditPos := Point(hr.Left,hr.Bottom);

  FillChar(bi, sizeof(bi), #0);
  bi.hwndOwner := Application.Handle;
  bi.pszDisplayName := ResStr;

  bi.lpszTitle := PChar('Select directory');
  bi.ulFlags := BIF_RETURNONLYFSDIRS;
  bi.lpfn := EditCallBack;
  bi.lParam := LParam(Self);

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
          OnParamChanged(Self, param, curdir, s);

        SetParam(Param,s);
      end;
    finally
      SHGetMalloc(MAlloc);
      Malloc.Free(iIdList);
    end;
  end;
end;

function TParamLabel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TParamLabel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TParamLabel.SetVersion(const Value: string);
begin
end;

end.
