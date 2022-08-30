{*******************************************************}
{                                                       }
{              MiTeC MDI Tab Component                  }
{                                                       }
{       version 1.61 for Delphi 5,6,7,2005,2006         }
{                                                       }
{       Copyright © 1999-2006 Michal Mutl               }
{                                                       }
{*******************************************************}

unit MDITab;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CommCtrl;

const
  cAbout = 'MDI Tab Control 1.61 - Copyright © 1999-2006 MichaL MutL';

type
  PNotifyEvent = ^TNotifyEvent;

  TMDITab = class(TCustomTabControl)
  private
    FCaption: string;
    FOnChange: TNotifyEvent;
    FOnGetImageIndex: TTabGetImageEvent;
    GlyphList, ActivateList, DestroyList :TList;
    HintList: TStringList;
    FShow: boolean;
    FAbout: string;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    procedure WMSetText(var Message: TWMSetText); message WM_SETTEXT;
    procedure WMGetText(var Message: TWMGetText); message WM_GETTEXT;
    procedure WMGetTextLength(var Message: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure SetAbout(const Value: string);
    function GetChild(index: integer): TForm;
    function GetCount: integer;
    function GetActiveChild: TForm;
    function GetMinimized: integer;
    function GetCaption(index: integer): string;
    procedure SetCaption(index: integer; const Value: string);
    function GetGlyph(index: integer): integer;
    procedure SetGlyph(index: integer; const Value: integer);
    function GetHint(index: integer): string;
    procedure SetHint(index: integer; const Value: string);
  protected
    procedure Change; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,
      Y: Integer); override;  
    function GetImageIndex(TabIndex: Integer): Integer; override;
    procedure ChildActivate(Sender :TObject);
    procedure ChildDestroy(Sender: TObject);
    procedure RemoveTab(AChild :TForm);
  public
    property Canvas;

    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;
    function GetTabAtPos(P :TPoint) :integer;
    procedure AddTab(AChild :TForm; ImageIndex :Integer; AHint: string);
    procedure MinimizeAll;
    procedure CloseAll;
    function GetChildIndex(AChild: TForm): integer;

    property ActiveMDIChild :TForm read GetActiveChild;
    property DisplayRect;
    property MDIChildCount :integer read GetCount;
    property MDIChildren[index :integer] :TForm read GetChild;
    property MinimizedCount :integer read GetMinimized;
    property Captions[index :integer]: string read GetCaption write SetCaption;
    property Glyphs[index :integer]: integer read GetGlyph write SetGlyph;
    property Hints[index :integer]: string read GetHint write SetHint;
    property TabIndex;
  published
    property About :string read FAbout write SetAbout;
    property Align;
    property Anchors;
    property BiDiMode;
    property Caption :string read FCaption write FCaption;
    property Constraints;
    property DockSite;
    property DragKind;
    property HotTrack;
    property Images;
    property OwnerDraw;
    property ParentBiDiMode;
    property RaggedRight;
    property Style;
    property OnDockDrop;
    property OnDockOver;
    property OnDrawTab;
    property OnEndDock;
    property OnGetImageIndex: TTabGetImageEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetSiteInfo;
    property OnResize;
    property OnStartDock;
    property OnUnDock;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MultiLine;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollOpposite;
    property ShowHint;
    property ShowOnChange :boolean read FShow write FShow;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

var
  MDITabIndex: integer;

implementation

procedure Register;
begin
  RegisterComponents('MiTeC', [TMDITab]);
end;

{ TMDITab }

procedure TMDITab.AddTab;
var
  i :integer;
  ae,de :pnotifyevent;
begin
  if assigned(achild) then begin
    for i:=0 to tabs.count-1 do
      if tform(tabs.objects[i])=achild then
        exit;
    glyphlist.add(pointer(imageindex));
    hintlist.Add(ahint);
    if assigned(achild.onactivate) then begin
      new(ae);
      ae^:=achild.onactivate;
      activatelist.add(ae)
    end else
      activatelist.add(nil);
    if assigned(achild.ondestroy) then begin
      new(de);
      de^:=achild.ondestroy;
      destroylist.add(de);
    end else
      destroylist.add(nil);
    achild.Hint:=AHint;  
    achild.onactivate:=childactivate;
    achild.ondestroy:=childdestroy;
    tabs.addobject(achild.caption,achild);
    tabindex:=tabs.count-1;
    change;
  end;
end;

procedure TMDITab.Change;
var
  child :tform;
begin
  if tabindex>-1 then begin
    child:=tform(tabs.objects[tabindex]);
    sendmessage(child.handle,wm_NCActivate,wa_Active,0);
    child.setfocus;
    child.bringtofront;
    if child.windowstate=wsminimized then
       child.windowstate:=wsnormal;
    if fshow then
      visible:=true;
  end;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TMDITab.ChildActivate(Sender: TObject);
var
  i :integer;
begin
  if not(sender is tmditab) then
    for i:=0 to tabs.count-1 do
      if tabs.objects[i]=sender then begin
        tabindex:=i;
        break;
      end;
  change;
  if (tabindex>-1) and assigned(activatelist[tabindex]) then
    tnotifyevent(activatelist[tabindex]^)(tform(tabs.objects[tabindex]));
end;

procedure TMDITab.ChildDestroy(Sender: TObject);
var
  i :integer;
begin
  try
  if assigned(sender) then begin
    if (sender is tform) then begin
      if Assigned(Tabs) then begin
        for i:=0 to tabs.count-1 do
          if tform(tabs.objects[i])=tform(sender) then
            break;
      end else
        i:=-1
    end else
      i:=-1;
  end else
    i:=-1;
  if (i>-1) then begin
    if assigned(destroylist[i]) then
      tnotifyevent(destroylist[i]^)(tform(tabs.objects[i]));
    tabs.Delete(i);
    activatelist.delete(i);
    destroylist.delete(i);
    glyphlist.delete(i);
    hintlist.Delete(i);
  end;
  except
  end;
end;

procedure TMDITab.CloseAll;
var
  i,n :integer;
begin
  n:=tform(owner).mdichildcount;
  for i:=n-1 downto 0 do
    tform(owner).mdichildren[i].close;
end;

procedure TMDITab.MinimizeAll;
var
  i,n :integer;
begin
  n:=tform(owner).mdichildcount;
  for i:=n-1 downto 0 do
    tform(owner).mdichildren[i].windowstate:=wsminimized;
  tform(owner).arrangeicons;
end;

constructor TMDITab.Create(AOwner: TComponent);
var
  i :integer;
begin
  fabout:=cabout;
  if not(aowner is tcustomform) then begin
    raise exception.create('MDITab can be put only on Form.');
    inherited destroy;
  end else
    if tform(aowner).formstyle<>fsmdiform then begin
      raise exception.create('MDITab can be put only on MDIForm.');
      inherited destroy;
    end;
  for i:=0 to aowner.componentcount-1 do
    if aowner.components[i] is tmditab then begin
      raise exception.create('Only one MDITab can be put on MDIForm.');
      inherited destroy;
    end;
  activatelist:=tlist.create;
  destroylist:=tlist.create;
  glyphlist:=tlist.create;
  hintlist:=tstringlist.Create;
  inherited;
  if csdesigning in componentstate then begin
    style:=tsflatbuttons;
    dragkind:=dkdock;
    hottrack:=true;
    showhint:=true;
    height:=22;
    align:=altop;
    cursor:=crhandpoint;
    fshow:=true;
  end;
end;

function TMDITab.GetImageIndex(TabIndex: Integer): Integer;
begin
  result:=integer(glyphlist[tabindex]);
  if Assigned(FOnGetImageIndex) then FOnGetImageIndex(Self, TabIndex, Result);
end;

procedure TMDITab.RemoveTab(AChild: TForm);
var
  i :integer;
begin
  if assigned(achild) then
    for i:=0 to tabs.count-1 do
      if tform(tabs.objects[i])=achild then begin
        tabs.Delete(i);
        activatelist.delete(i);
        destroylist.delete(i);
        glyphlist.delete(i);
        hintlist.delete(i);
        break;
      end;
end;

procedure TMDITab.WMGetText(var Message: TWMGetText);
begin
  with Message do
    Result := StrLen(StrLCopy(PChar(Text), PChar(FCaption), TextMax - 1));
end;

procedure TMDITab.WMGetTextLength(var Message: TWMGetTextLength);
begin
  Message.Result := Length(FCaption);
end;

procedure TMDITab.WMSetText(var Message: TWMSetText);
begin
  with Message do
    SetString(FCaption, Text, StrLen(Text));
end;

procedure TMDITab.SetAbout(const Value: string);
begin

end;

function TMDITab.GetChild(index: integer): TForm;
begin
  if (index>-1) and (index<tabs.count) then
    result:=tform(tabs.objects[index])
  else
    result:=nil;
end;

function TMDITab.GetCount: integer;
begin
  result:=tabs.count;
end;

function TMDITab.GetActiveChild: TForm;
begin
  result:=getchild(tabindex);
end;

function TMDITab.GetMinimized: integer;
var
  i :integer;
begin
  result:=0;
  for i:=0 to tabs.count-1 do
    if getchild(i).windowstate=wsminimized then
      inc(result);
end;

function TMDITab.GetTabAtPos(P: TPoint): integer;
var
  ht :PTCHitTestInfo;
begin
  new(ht);
  ht^.pt:=p;
  result:=sendmessage(handle,TCM_HITTEST,0,integer(ht));
  dispose(ht);
end;

destructor TMDITab.Destroy;
begin
  PopupMenu:=nil;
  activatelist.free;
  destroylist.free;
  glyphlist.free;
  hintlist.Free;
  inherited;
end;

function TMDITab.GetCaption(index: integer): string;
begin
  if (index>-1) and (index<tabs.count) then
    result:=tabs[index]
  else
    result:='';
end;

procedure TMDITab.SetCaption(index: integer; const Value: string);
begin
  if (index>-1) and (index<tabs.count) then
    tabs[index]:=value;
end;

function TMDITab.GetGlyph(index: integer): integer;
begin
  if (index>-1) and (index<glyphlist.count) then
    result:=integer(glyphlist[index])
  else
    result:=-1;
end;

procedure TMDITab.SetGlyph(index: integer; const Value: integer);
begin
  if (index>-1) and (index<glyphlist.count) then
    glyphlist[index]:=pointer(value);
end;

function TMDITab.GetChildIndex(AChild: TForm): integer;
var
  i: integer;
begin
  result:=-1;
  for i:=0 to MDIChildCount-1 do
    if MDIChildren[i]=AChild then begin
      result:=i;
      break;
    end;
end;

procedure TMDITab.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  p :tpoint;
begin
  p.x:=x;
  p.y:=y;
  MDITabIndex:=GetTabAtPos(p);
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X, Y);
end;

function TMDITab.GetHint(index: integer): string;
begin
  if (index>-1) and (Index<hintlist.count) then
    result:=hintlist[Index]
  else
    result:='';
end;

procedure TMDITab.SetHint(index: integer; const Value: string);
begin
  if (index>-1) and (Index<hintlist.count) then
    hintlist[index]:=Value;
end;

procedure TMDITab.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  p :tpoint;
begin
  p.x:=x;
  p.y:=y;
  Hint:=Hints[GetTabAtPos(p)];
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

end.
