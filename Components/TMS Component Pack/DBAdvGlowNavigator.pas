{*************************************************************************}
{ TDBADVGLOWNAVIGATOR component                                           }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2008 - 2015                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit DBAdvGlowNavigator;

interface

{$R DBADVGLOWNAV.RES}
{$I TMSDEFS.INC}

uses
  DBCtrls, Classes, Windows, Messages, StdCtrls, AdvGlowButton, DB, Controls,
  ExtCtrls, Dialogs, Buttons, Graphics, ImgList, AdvStyleIF, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 5; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.0.0.0   : First release
  // 1.1.0.0   : New: Terminal, Vista, Windows 7 styles
  // 1.1.1.0   : Improved : issue with transparency
  // 1.1.2.0   : Improved : handling of bookmarks in Unicode Delphi
  // 1.1.3.0   : New : Built in support for Office 2010 colors
  // 1.1.4.0   : New : Windows 8, Office 2013 styles added
  // 1.1.4.1   : Fixed : Issue with loading resource for button hot state
  // 1.1.4.2   : Fixed : Issue with flickering when used on ribbon
  // 1.1.5.0   : New : Windows 10, Office 2016 styles added

type
  TAdvGlowNavButton = class;

  TAdvNavDataLink = class;

  TNavigatorOrientation = (noHorizontal, noVertical);

  TAdvNavigateBtn = (nbFirst, nbPrior, nbNext, nbLast,
    nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh, nbSearch, nbSetBookmark, nbGotoBookMark);

  TButtonSet = set of TAdvNavigateBtn;

  ENavClick = procedure (Sender: TObject; Button: TAdvNavigateBtn) of object;

  TGlyphSize = (gsSmall, gsLarge, gsCustom);

  TDBAdvGlowNavigator = class (TCustomPanel, ITMSStyle)
  private
    FDataLink: TAdvNavDataLink;
    FVisibleButtons: TButtonSet;
    FHints: TStrings;
    FDefHints: TStrings;
    ButtonWidth: Integer;
    ButtonHeight: Integer;
    MinBtnSize: TPoint;
    FOnNavClick: ENavClick;
    FBeforeAction: ENavClick;
    FocusedButton: TAdvNavigateBtn;
    FConfirmDelete: Boolean;
    FDeleteDisabled: Boolean;
    FInsertDisabled: Boolean;
    FOnBtnPrior : TNotiFyEvent;
    FOnBtnNext : TNotiFyEvent;
    FOnBtnFirst : TNotiFyEvent;
    FOnBtnLast : TNotiFyEvent;
    FOnBtnInsert : TNotiFyEvent;
    FOnBtnEdit : TNotiFyEvent;
    FOnBtnCancel : TNotiFyEvent;
    FOnBtnPost : TNotiFyEvent;
    FOnBtnRefresh : TNotiFyEvent;
    FOnBtnDelete : TNotiFyEvent;
    FOrientation: TNavigatorOrientation;
    FGlyphSize: TGlyphSize;
    FOnBtnSearch: TNotifyEvent;
    FOnBtnGotoBookmark: TNotifyEvent;
    FOnBtnSetBookmark: TNotifyEvent;
    FBookmarkStr: string;
    FBookmark: TBookmark;
    FLook: Integer;
    FGlyphCustomSize: Integer;
    FGlyphResName: string;
    FTransparent: Boolean;
    FAppearance: TGlowButtonAppearance;
    FStyle: TTMSStyle;
    //FRounded: Boolean;
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ClickHandler(Sender: TObject);
    function GetDataSource: TDataSource;
    function GetHints: TStrings;
    procedure HintsChanged(Sender: TObject);
    procedure OnAppearanceChanged(Sender: TObject);
    procedure InitButtons;
    procedure InitHints;
    procedure InitStyles;
    procedure SetDataSource(Value: TDataSource);
    procedure SetHints(Value: TStrings);
    procedure SetSize(var W: Integer; var H: Integer);
    procedure SetVisible(Value: TButtonSet);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure SetDeleteDisabled(const Value: Boolean);
    procedure SetInsertDisabled(const Value: Boolean);
    //procedure SetRounded(const Value: Boolean);
    procedure SetOrientation(const Value: TNavigatorOrientation);
    procedure SetGlyphSize(const Value: TGlyphSize);
    procedure SetLook(const Value: Integer);
    procedure SetGlyphCustomSize(const Value: Integer);
    procedure SetGlyphResName(const Value: string);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetButtons(index: TAdvNavigateBtn): TAdvGlowNavButton;
    procedure SetTransparent(const Value: Boolean);
    procedure SetAppearance(const Value: TGlowButtonAppearance);
    procedure SetStyle(const Value: TTMSStyle);
  protected
    FButtons: array[TAdvNavigateBtn] of TAdvGlowNavButton;
    function GetVersionNr: Integer; virtual;
    procedure DataChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
    procedure CalcMinSize(var W, H: Integer);
    procedure SetButtonsShape;
    {$IFNDEF DELPHI_UNICODE}
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF DELPHI_UNICODE}
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    {$ENDIF}
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BtnClick(Index: TAdvNavigateBtn); virtual;
    property Look: Integer read FLook write SetLook;
    property BookMarkStr: string read FBookmarkStr write FBookmarkStr;
    property BookMark: TBookmark read FBookmark write FBookmark;
    property Buttons[index: TAdvNavigateBtn]: TAdvGlowNavButton read GetButtons;
    procedure SetComponentStyle(AStyle: TTMSStyle);    
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property VisibleButtons: TButtonSet read FVisibleButtons write SetVisible
      default [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete,
        nbEdit, nbPost, nbCancel, nbRefresh];
    property Align;
    property Anchors;
    property Appearance: TGlowButtonAppearance read FAppearance write SetAppearance;
    property Constraints;
    property DeleteDisabled: Boolean read FDeleteDisabled write SetDeleteDisabled;
    property InsertDisabled: Boolean read FInsertDisabled write SetInsertDisabled;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property GlyphSize: TGlyphSize read FGlyphSize write SetGlyphSize default gsLarge;
    property GlyphCustomSize: Integer read FGlyphCustomSize write SetGlyphCustomSize;
    property GlyphResName: string read FGlyphResName write SetGlyphResName;
    property Ctl3D;
    property Hints: TStrings read GetHints write SetHints;

    property Orientation: TNavigatorOrientation read FOrientation write SetOrientation;
    //property Rounded: Boolean read FRounded write SetRounded default False;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property ShowHint;
    property Style: TTMSStyle read FStyle write SetStyle default tsCustom;
    property TabOrder;
    property TabStop;
    property Visible;

    property BeforeAction: ENavClick read FBeforeAction write FBeforeAction;
    property OnClick: ENavClick read FOnNavClick write FOnNavClick;
    property OnBtnPrior : TNotifyEvent read FOnBtnPrior write FOnBtnPrior;
    property OnBtnNext : TNotifyEvent read FOnBtnNext write FOnBtnNext;
    property OnBtnFirst : TNotifyEvent read FOnBtnFirst write FOnBtnFirst;
    property OnBtnLast : TNotifyEvent read FOnBtnLast write FOnBtnLast;
    property OnBtnInsert : TNotifyEvent read FOnBtnInsert write FOnBtnInsert;
    property OnBtnEdit : TNotifyEvent read FOnBtnEdit write FOnBtnEdit;
    property OnBtnCancel : TNotifyEvent read FOnBtnCancel write FOnBtnCancel;
    property OnBtnPost : TNotifyEvent read FOnBtnPost write FOnBtnPost;
    property OnBtnRefresh : TNotifyEvent read FOnBtnRefresh write FOnBtnRefresh;
    property OnBtnDelete : TNotifyEvent read FOnBtnDelete write FOnBtnDelete;
    property OnBtnSearch: TNotifyEvent read FOnBtnSearch write FOnBtnSearch;
    property OnBtnSetBookmark: TNotifyEvent read FOnBtnSetBookmark write FOnBtnSetBookmark;
    property OnBtnGotoBookmark: TNotifyEvent read FOnBtnGotoBookmark write FOnBtnGotoBookmark;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property Version: string read GetVersion write SetVersion;
  end;

{ TAdvGlowNavButton }

  TAdvGlowNavButton = class(TAdvGlowButton)
  private
    FIndex: TAdvNavigateBtn;
    FNavStyle: TNavButtonStyle;
    FGlyph: TBitmap;
    FGlyphHot: TBitmap;
    FGlyphDisabled: TBitmap;
    procedure SetGlyph(const Value: TBitmap);
    procedure SetGlyphDisabled(const Value: TBitmap);
    procedure SetGlyphHot(const Value: TBitmap);
    procedure SetNavStyle(const Value: TNavButtonStyle);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure GetToolImage(bmp: TBitmap); override;  
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property NavStyle: TNavButtonStyle read FNavStyle write SetNavStyle;
    property Index : TAdvNavigateBtn read FIndex write FIndex;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphHot: TBitmap read FGlyphHot write SetGlyphHot;
    property GlyphDisabled: TBitmap read FGlyphDisabled write SetGlyphDisabled;
  end;

{ TNavDataLink }

  TAdvNavDataLink = class(TDataLink)
  private
    FNavigator: TDBAdvGlowNavigator;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: TDBAdvGlowNavigator);
    destructor Destroy; override;
  end;

implementation

uses
  Math, SysUtils,
  {$IFDEF DELPHI6_LVL}
  VDBConsts
  {$ELSE}
  DBConsts
  {$ENDIF}
  ;

//------------------------------------------------------------------------------

{ TDBAdvGlowNavigator }

var
  BtnTypeAdvName: array[TAdvNavigateBtn] of PChar = ('TMSGLOWNAVFIRST', 'TMSGLOWNAVPREVIOUS', 'TMSGLOWNAVNEXT',
    'TMSGLOWNAVLAST', 'TMSGLOWNAVINSERT', 'TMSGLOWNAVDELETE', 'TMSGLOWNAVEDIT', 'TMSGLOWNAVPOST', 'TMSGLOWNAVCANCEL', 'TMSGLOWNAVREFRESH',
    'TMSGLOWNAVSEARCH','TMSGLOWNAVBOOK','TMSGLOWNAVGOTO');

constructor TDBAdvGlowNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if (csDesigning in ComponentState) then
     ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption];

  ControlStyle := ControlStyle + [csOpaque];
  if not NewStyleControls then ControlStyle := ControlStyle + [csFramed];
  FDataLink := TAdvNavDataLink.Create(Self);
  FVisibleButtons := [nbFirst, nbPrior, nbNext, nbLast, nbInsert,
    nbDelete, nbEdit, nbPost, nbCancel, nbRefresh];
  FHints := TStringList.Create;
  TStringList(FHints).OnChange := HintsChanged;
  FTransparent := False;

  FAppearance := TGlowButtonAppearance.Create;
  FAppearance.OnChange := OnAppearanceChanged;

  InitButtons;
  InitHints;

  BevelOuter := bvNone;
  BevelInner := bvNone;
  Width := 241;
  Height := 25;
  ButtonWidth := 0;
  ButtonHeight := 0;
  FocusedButton := nbFirst;
  FConfirmDelete := True;
  FullRepaint := False;

  {FColor := clBtnFace;
  FColorTo := clNone;
  FColorHot := RGB(199,199,202);
  FColorHotTo := clNone;
  FColorDown := RGB(210,211,216);
  FColorDownTo := clNone;
  FBorderColor := clNone;
  FBorderColorHot := clBlack;
  FBorderColorDown := clBlack;}

  FDeleteDisabled := False;
  FInsertDisabled := False;
  FBookmarkStr := '';
  FBookmark := nil;

  GlyphSize := gsLarge;
  FStyle := tsCustom;
  DoubleBuffered := true;
end;

//------------------------------------------------------------------------------

destructor TDBAdvGlowNavigator.Destroy;
begin
  FDefHints.Free;
  FDataLink.Free;
  FHints.Free;
  FDataLink := nil;
  FAppearance.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.InitButtons;
var
  I: TAdvNavigateBtn;
  Btn: TAdvGlowNavButton;
  X: Integer;
  ResName: string;
begin
  case GlyphSize of
  gsSmall:
    begin
      MinBtnSize := Point(20, 18);
    end;
  gsLarge:
    begin
      MinBtnSize := Point(36, 36);
    end;
  gsCustom:
    begin
      MinBtnSize := Point(GlyphCustomSize, GlyphCustomSize - 2);
    end;
  end;

  X := 0;
  for I := Low(FButtons) to High(FButtons) do
  begin
    Btn := TAdvGlowNavButton.Create (Self);
    Btn.Transparent := FTransparent;
    Btn.Index := I;
    Btn.Visible := I in FVisibleButtons;
    Btn.Enabled := True;

    Btn.SetBounds (X, 0, MinBtnSize.X, MinBtnSize.Y);

    case GlyphSize of
    gsSmall:
      FmtStr(ResName, '%sE', [BtnTypeAdvName[I]]);
    gsLarge:
      FmtStr(ResName, '%sLE', [BtnTypeAdvName[I]]);
    gsCustom:
      FmtStr(ResName, '%s'+GlyphResname+'E', [BtnTypeAdvName[I]]);
    end;

    if (GlyphSize = gsLarge) and (FindResource(HInstance, PChar(ResName), RT_RCDATA) <> 0) then
      Btn.Picture.LoadFromResourceName(Hinstance, ResName)
    else if FindResource(HInstance,PChar(ResName),RT_BITMAP) <> 0 then
    begin
      Btn.Glyph.LoadFromResourceName(Hinstance, ResName);
      Btn.Picture.Assign(nil);
    end;  

    case GlyphSize of
    gsSmall:
      FmtStr(ResName, '%sH', [BtnTypeAdvName[I]]);
    gsLarge:
      FmtStr(ResName, '%sLH', [BtnTypeAdvName[I]]);
    gsCustom:
      FmtStr(ResName, '%s'+GlyphResname+'H', [BtnTypeAdvName[I]]);
    end;

    if (FindResource(HInstance,PChar(ResName), RT_RCDATA) <> 0) then
      FButtons[I].HotPicture.LoadFromResourceName(Hinstance,ResName)
    else
      if (FindResource(HInstance,PChar(ResName), RT_BITMAP) <> 0) then
      begin
        FButtons[I].GlyphHot.LoadFromResourceName(Hinstance,ResName);
        FButtons[I].HotPicture.Assign(nil);
      end;

    case GlyphSize of
    gsSmall:
      FmtStr(ResName, '%sD', [BtnTypeAdvName[I]]);
    gsLarge:
      FmtStr(ResName, '%sLD', [BtnTypeAdvName[I]]);
    gsCustom:
      FmtStr(ResName, '%s'+GlyphResname+'D', [BtnTypeAdvName[I]]);
    end;

    if (GlyphSize = gsLarge) and (FindResource(HInstance, PChar(ResName), RT_RCDATA) <> 0) then
      Btn.DisabledPicture.LoadFromResourceName(Hinstance, ResName)
    else if FindResource(HInstance,PChar(ResName), RT_BITMAP) <> 0 then
    begin
      Btn.GlyphDisabled.LoadFromResourceName(Hinstance, ResName);
      Btn.DisabledPicture.Assign(nil);
    end;

    Btn.Enabled := False;
    Btn.Enabled := True;
    Btn.OnClick := ClickHandler;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.Parent := Self;
    {if (Orientation = noHorizontal) then
    begin
      if (I = Low(FButtons)) then
        Btn.Position := bpLeft
      else if (I = High(FButtons)) then
        Btn.Position := bpRight
      else
        Btn.Position := bpMiddle;
    end
    else
    begin
      Btn.Position := bpStandalone;
    end;}
    FButtons[I] := Btn;
    X := X + MinBtnSize.X;
  end;
  Buttons[nbPrior].NavStyle := Buttons[nbPrior].NavStyle + [nsAllowTimer];
  Buttons[nbNext].NavStyle  := Buttons[nbNext].NavStyle + [nsAllowTimer];
  SetButtonsShape;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.InitStyles;
var
  J: TAdvNavigateBtn;
begin
  for J := Low(FButtons) to High(FButtons) do
  begin
    FButtons[J].Appearance.Assign(Self.Appearance);
    //FButtons[J].Rounded := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.InitHints;
var
  I: Integer;
  J: TAdvNavigateBtn;
begin
  if not Assigned(FDefHints) then
  begin
    FDefHints := TStringList.Create;

    FDefHints.Add(SFirstRecord);
    FDefHints.Add(SPriorRecord);
    FDefHints.Add(SNextRecord);
    FDefHints.Add(SLastRecord);
    FDefHints.Add(SInsertRecord);
    FDefHints.Add(SDeleteRecord);
    FDefHints.Add(SEditRecord);
    FDefHints.Add(SPostEdit);
    FDefHints.Add(SCancelEdit);
    FDefHints.Add(SRefreshRecord);

    FDefHints.Add('Find record');
    FDefHints.Add('Set bookmark');
    FDefHints.Add('Goto bookmark');

  end;
  for J := Low(FButtons) to High(FButtons) do
    FButtons[J].Hint := FDefHints[Ord(J)];
  J := Low(FButtons);
  for I := 0 to (FHints.Count - 1) do
  begin
    if FHints.Strings[I] <> '' then Buttons[J].Hint := FHints.Strings[I];
    if J = High(FButtons) then Exit;
    Inc(J);
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.HintsChanged(Sender: TObject);
begin
  InitHints;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetHints(Value: TStrings);
begin
  if Value.Text = FDefHints.Text then
    FHints.Clear else
    FHints.Assign(Value);
end;

//------------------------------------------------------------------------------

function TDBAdvGlowNavigator.GetHints: TStrings;
begin
  if (csDesigning in ComponentState) and not (csWriting in ComponentState) and
     not (csReading in ComponentState) and (FHints.Count = 0) then
    Result := FDefHints else
    Result := FHints;
end;

//------------------------------------------------------------------------------

function TDBAdvGlowNavigator.GetButtons(index: TAdvNavigateBtn): TAdvGlowNavButton;
begin
  Result := FButtons[index];
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetVisible(Value: TButtonSet);
var
  I: TAdvNavigateBtn;
  W, H: Integer;
begin
  W := Width;
  H := Height;
  FVisibleButtons := Value;
  for I := Low(FButtons) to High(FButtons) do
    FButtons[I].Visible := I in FVisibleButtons;
  SetSize(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
  SetButtonsShape;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.CalcMinSize(var W, H: Integer);
var
  Count: Integer;
  I: TAdvNavigateBtn;
begin
  if (csLoading in ComponentState) then
    Exit;

  if Buttons[nbFirst] = nil then
    Exit;

  Count := 0;
  for I := Low(FButtons) to High(FButtons) do
    if FButtons[I].Visible then
      Inc(Count);

  if Count = 0 then
    Inc(Count);

  if Orientation = noHorizontal then
  begin
    W := Max(W, Count * MinBtnSize.X);
    H := Max(H, MinBtnSize.Y);
  end
  else
  begin
    W := Max(W, MinBtnSize.X);
    H := Max(H, Count * MinBtnSize.Y);
  end;

  if Align = alNone then W := (W div Count) * Count;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetSize(var W: Integer; var H: Integer);
var
  Count: Integer;
  I: TAdvNavigateBtn;
  Space, Temp, Remain: Integer;
  X, Y: Integer;
begin
  if (csLoading in ComponentState) then
    Exit;
  if FButtons[nbFirst] = nil then
    Exit;

  CalcMinSize(W, H);

  Count := 0;
  for I := Low(FButtons) to High(FButtons) do
    if FButtons[I].Visible then
      Inc(Count);
  if Count = 0 then Inc(Count);

  if Orientation = noHorizontal then
  begin
    ButtonWidth := W div Count;
    Temp := Count * ButtonWidth;
    if Align = alNone then W := Temp;

    X := 0;
    Remain := W - Temp;
    Temp := Count div 2;
    for I := Low(FButtons) to High(FButtons) do
    begin
      if FButtons[I].Visible then
      begin
        Space := 0;
        if Remain <> 0 then
        begin
          Dec(Temp, Remain);
          if Temp < 0 then
          begin
            Inc(Temp, Count);
            Space := 1;
          end;
        end;
        FButtons[I].SetBounds(X, 0, ButtonWidth + Space, Height);
        Inc(X, ButtonWidth + Space);
      end
      else
        FButtons[I].SetBounds (Width + 1, 0, ButtonWidth, Height);
    end;
  end
  else
  begin
    ButtonHeight := H div Count;
    Temp := Count * ButtonHeight;
    if Align = alNone then
      H := Temp;

    Y := 0;
    Remain := H - Temp;
    Temp := Count div 2;
    for I := Low(FButtons) to High(FButtons) do
    begin
      if FButtons[I].Visible then
      begin
        Space := 0;
        if Remain <> 0 then
        begin
          Dec(Temp, Remain);
          if Temp < 0 then
          begin
            Inc(Temp, Count);
            Space := 1;
          end;
        end;
        FButtons[I].SetBounds(0, Y, Width, ButtonHeight + Space);
        Inc(Y, ButtonHeight + Space);
      end
      else
        FButtons[I].SetBounds(0,Height + 1, Width, ButtonHeight);
    end;
  end;
end;

procedure TDBAdvGlowNavigator.SetStyle(const Value: TTMSStyle);
begin
  FStyle := Value;
  SetComponentStyle(Value);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  if not HandleAllocated then SetSize(W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  W := Width;
  H := Height;
  SetSize(W, H);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetButtonsShape;
var
  FirstBtn, LastBtn: TAdvGlowNavButton;
  I: TAdvNavigateBtn;
  Count: Integer;
begin
  if (Orientation = noHorizontal) and not Transparent then
  begin
    Count := 0;
    for I := Low(FButtons) to High(FButtons) do
      if FButtons[I].Visible then
        Inc(Count);

    FirstBtn := nil;
    LastBtn := nil;

    for I := Low(FButtons) to High(FButtons) do
    begin
      if FButtons[I].Visible then
      begin
       if (Count = 1) then
       begin
         FButtons[I].Position := bpStandalone;
         Break;
       end;

       if not Assigned(FirstBtn) then
       begin
         FirstBtn := FButtons[I];
         FirstBtn.Position :=  AdvGlowButton.bpLeft;
         Continue;
       end;

       FButtons[I].Position := AdvGlowButton.bpMiddle;
       LastBtn := FButtons[I];
     end;
    end;

    if Assigned(LastBtn) then
      LastBtn.Position := AdvGlowButton.bpRight;
  end
  else
  begin
    for I := Low(FButtons) to High(FButtons) do
      if FButtons[I].Visible then
      begin
        FButtons[I].Position := bpStandalone;
      end;
  end;
end;

procedure TDBAdvGlowNavigator.SetComponentStyle(AStyle: TTMSStyle);
var
  I: TAdvNavigateBtn;
begin
  for I := Low(FButtons) to High(FButtons) do
  begin
    FButtons[I].SetComponentStyle(AStyle);
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  inherited;
  if (SWP_NOSIZE and Message.WindowPos.Flags) = 0 then
    CalcMinSize(Message.WindowPos.cx, Message.WindowPos.cy);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.ClickHandler(Sender: TObject);
begin
  BtnClick (TAdvGlowNavButton (Sender).Index);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.BtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldFocus: TAdvNavigateBtn;
begin
  OldFocus := FocusedButton;
  FocusedButton := TAdvGlowNavButton (Sender).Index;
  if TabStop and (GetFocus <> Handle) and CanFocus then
  begin
    SetFocus;
    if (GetFocus <> Handle) then
      Exit;
  end
  else if TabStop and (GetFocus = Handle) and (OldFocus <> FocusedButton) then
  begin
    FButtons[OldFocus].Invalidate;
    FButtons[FocusedButton].Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.BtnClick(Index: TAdvNavigateBtn);
begin
  if (DataSource <> nil) and (DataSource.State <> dsInactive) then
  begin
    if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
      FBeforeAction(Self, Index);
    with DataSource.DataSet do
    begin
      case Index of
      nbPrior:
        if Assigned(FOnBtnPrior) then
          FOnBtnPrior(Self)
        else
          Prior;
      nbNext:
        if Assigned(FOnBtnNext) then
          FOnBtnNext(Self)
        else
          Next;
      nbFirst:
        if Assigned(FOnBtnFirst) then
          FOnBtnFirst(Self)
        else
          First;
      nbLast:
        if Assigned(FOnBtnLast) then
          FOnBtnLast(Self)
        else
          Last;
      nbInsert:
        if Assigned(FOnBtnInsert) then
          FOnBtnInsert(Self)
        else
          Insert;
      nbEdit:
        if Assigned(FOnBtnEdit) then
          FOnBtnEdit(Self)
        else
          Edit;
      nbCancel:
        if Assigned(FOnBtnCancel) then
          FOnBtnCancel(Self)
        else
          Cancel;
      nbPost:
        if Assigned(FOnBtnPost) then
          FOnBtnPost(Self)
        else
          Post;
      nbRefresh:
        if Assigned(FOnBtnRefresh) then
          FOnBtnRefresh(Self)
        else
          Refresh;
      nbDelete:
        if Assigned(FOnBtnDelete) then
          FOnBtnDelete(Self)
        else
        begin
          if not FConfirmDelete or
            (MessageDlg(SDeleteRecordQuestion, mtConfirmation,
              mbOKCancel, 0) <> idCancel) then
            Delete;
        end;
      nbSearch:
        if Assigned(FOnBtnSearch) then
          FOnBtnSearch(Self);
      nbSetBookmark:
        if Assigned(FOnBtnSetBookmark) then
          FOnBtnSetBookmark(Self)
        else
        begin
          {$IFNDEF DELPHI_UNICODE}
          FBookmarkStr := Bookmark;
          {$ENDIF}
          {$IFDEF DELPHI_UNICODE}
          FBookmark := GetBookmark;
          {$ENDIF}
          Buttons[nbGotoBookmark].Enabled := True;
        end;
      nbGotoBookmark:
        if Assigned(FOnBtnGotoBookmark) then
          FOnBtnGotoBookmark(Self)
        else
        begin
          {$IFNDEF DELPHI_UNICODE}
          if FBookmarkStr <> '' then
            BookmarkStr := FBookmarkStr;
          {$ENDIF}
          {$IFDEF DELPHI_UNICODE}
          if Assigned(FBookmark) then
          begin
            GotoBookmark(FBookmark);
            FreeBookmark(FBookmark);
            FBookmark := nil;
          end;
          {$ENDIF}
          Buttons[nbGotoBookmark].Enabled := false;
        end;
      end;
    end;
  end;
  if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then
    FOnNavClick(Self, Index);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.WMSetFocus(var Message: TWMSetFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.WMKillFocus(var Message: TWMKillFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewFocus: TAdvNavigateBtn;
  OldFocus: TAdvNavigateBtn;
begin
  OldFocus := FocusedButton;
  case Key of
    VK_RIGHT:
      begin
        if OldFocus < High(FButtons) then
        begin
          NewFocus := OldFocus;
          repeat
            NewFocus := Succ(NewFocus);
          until (NewFocus = High(FButtons)) or (FButtons[NewFocus].Visible);
          if FButtons[NewFocus].Visible then
          begin
            FocusedButton := NewFocus;
            FButtons[OldFocus].Invalidate;
            FButtons[NewFocus].Invalidate;
          end;
        end;
      end;
    VK_LEFT:
      begin
        NewFocus := FocusedButton;
        repeat
          if NewFocus > Low(FButtons) then
            NewFocus := Pred(NewFocus);
        until (NewFocus = Low(FButtons)) or (FButtons[NewFocus].Visible);
        if NewFocus <> FocusedButton then
        begin
          FocusedButton := NewFocus;
          FButtons[OldFocus].Invalidate;
          FButtons[FocusedButton].Invalidate;
        end;
      end;
    VK_SPACE:
      begin
        if FButtons[FocusedButton].Enabled then
          FButtons[FocusedButton].Click;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.DataChanged;
var
  UpEnable, DnEnable: Boolean;
begin
  UpEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
  Buttons[nbFirst].Enabled := UpEnable;
  Buttons[nbPrior].Enabled := UpEnable;
  Buttons[nbNext].Enabled := DnEnable;
  Buttons[nbLast].Enabled := DnEnable;
  Buttons[nbDelete].Enabled := Enabled and FDataLink.Active and
    FDataLink.DataSet.CanModify and
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF) and (not DeleteDisabled);
  Buttons[nbInsert].Enabled := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify and (not InsertDisabled);
  Buttons[nbSearch].Enabled := Enabled and FDataLink.Active;
  Buttons[nbSetBookmark].Enabled := Enabled and FDataLink.Active;
  {$IFNDEF DELPHI_UNICODE}
  Buttons[nbGotoBookmark].Enabled := Enabled and FDataLink.Active and (FBookmarkStr <> '');
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  Buttons[nbGotoBookmark].Enabled := Enabled and FDataLink.Active and Assigned(FBookmark);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.EditingChanged;
var
  CanModify: Boolean;
begin
  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  Buttons[nbInsert].Enabled := CanModify;
  Buttons[nbEdit].Enabled := CanModify and not FDataLink.Editing;
  Buttons[nbPost].Enabled := CanModify and FDataLink.Editing;
  Buttons[nbCancel].Enabled := CanModify and FDataLink.Editing;
  Buttons[nbRefresh].Enabled := CanModify and not FDataLink.Editing;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.ActiveChanged;
var
  I: TAdvNavigateBtn;
begin
  if not (Enabled and FDataLink.Active) then
  begin
    for I := Low(FButtons) to High(FButtons) do
      FButtons[I].Enabled := False;
    FBookmarkStr := '';
    FBookmark := nil;
  end
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
    ActiveChanged;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
    ActiveChanged;
  if Value <> nil then Value.FreeNotification(Self);
end;

//------------------------------------------------------------------------------

function TDBAdvGlowNavigator.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  SetSize(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
  InitHints;
  ActiveChanged;

  InitStyles;
  SetButtonsShape;

  Style := Style;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetDeleteDisabled(const Value: Boolean);
begin
  FDeleteDisabled := Value;
  DataChanged;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetInsertDisabled(const Value: Boolean);
begin
  FInsertDisabled := Value;
  DataChanged;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetOrientation(const Value: TNavigatorOrientation);
var
  I: TAdvNavigateBtn;
  Count: Integer;
  W,H: Integer;
begin
  if (csDesigning in ComponentState) and (Value <> FOrientation)
    and not (csLoading in ComponentState) then
  begin
    FOrientation := Value;

    Count := 0;
    for I := Low(FButtons) to High(FButtons) do
      if FButtons[I].Visible then
        Inc(Count);

    if Value = noHorizontal then
      SetBounds(Left,Top,Count * MinBtnSize.X, MinBtnSize.Y)
    else
      SetBounds(Left,Top,MinBtnSize.X, MinBtnSize.Y * Count);


    if Value = noHorizontal then
    begin
      Width := Count * MinBtnSize.X;
      Height := MinBtnSize.Y;
    end
    else
    begin
      Width := MinBtnSize.X;
      Height := Count * MinBtnSize.Y;
    end;

    W := Width;
    H := Height;

    SetSize(W,H);
  end;

  FOrientation := Value;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetGlyphSize(const Value: TGlyphSize);
var
  I: TAdvNavigateBtn;
  ResName: string;
  W, H: Integer;
begin
  if (FGlyphSize <> Value) or (Value = gsCustom) then
  begin
    FGlyphSize := Value;

    case GlyphSize of
    gsSmall:
      begin
        MinBtnSize := Point(20, 18);
      end;
    gsLarge:
      begin
        MinBtnSize := Point(36, 36);
      end;
    gsCustom:
      begin
        MinBtnSize := Point(GlyphCustomSize, GlyphCustomSize - 2);
      end;
    end;

    for I := Low(FButtons) to High(FButtons) do
    begin
      case GlyphSize of
      gsSmall:
        FmtStr(ResName, '%sE', [BtnTypeAdvName[I]]);
      gsLarge:
        FmtStr(ResName, '%sLE', [BtnTypeAdvName[I]]);
      gsCustom:
        FmtStr(ResName, '%s'+GlyphResname+'E', [BtnTypeAdvName[I]]);
      end;

      if (FindResource(HInstance,PChar(ResName), RT_RCDATA) <> 0) then
        FButtons[I].Picture.LoadFromResourceName(Hinstance,ResName)
      else if (FindResource(HInstance,PChar(ResName), RT_BITMAP) <> 0) then
      begin
        FButtons[I].Glyph.LoadFromResourceName(Hinstance,ResName);
        FButtons[I].Picture.Assign(nil);
      end;

      case GlyphSize of
      gsSmall:
        FmtStr(ResName, '%sH', [BtnTypeAdvName[I]]);
      gsLarge:
        FmtStr(ResName, '%sLH', [BtnTypeAdvName[I]]);
      gsCustom:
        FmtStr(ResName, '%s'+GlyphResname+'H', [BtnTypeAdvName[I]]);
      end;

      if (FindResource(HInstance,PChar(ResName), RT_RCDATA) <> 0) then
        FButtons[I].HotPicture.LoadFromResourceName(Hinstance,ResName)
      else
        if (FindResource(HInstance,PChar(ResName), RT_BITMAP) <> 0) then
        begin
          FButtons[I].GlyphHot.LoadFromResourceName(Hinstance,ResName);
          FButtons[I].HotPicture.Assign(nil);
        end;

      case GlyphSize of
      gsSmall:
        FmtStr(ResName, '%sD', [BtnTypeAdvName[I]]);
      gsLarge:
        FmtStr(ResName, '%sLD', [BtnTypeAdvName[I]]);
      gsCustom:
        FmtStr(ResName, '%s'+GlyphResname+'D', [BtnTypeAdvName[I]]);
      end;

      if (FindResource(HInstance,PChar(ResName), RT_RCDATA) <> 0) then
        FButtons[I].DisabledPicture.LoadFromResourceName(Hinstance,ResName)
      else if FindResource(HInstance,PChar(ResName), RT_BITMAP) <> 0 then
      begin
        FButtons[I].GlyphDisabled.LoadFromResourceName(Hinstance,ResName);
        FButtons[I].DisabledPicture.Assign(nil);
      end;
    end;

    W := Width;
    H := Height;
    SetSize(W, H);
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetLook(const Value: Integer);
begin
  FLook := Value;
  with Appearance do
  begin
    case Value of
    // Windows XP
    0:begin
        Color := $EDF1F1;
        ColorTo := $DFEBEB;
        ColorHot := $FAFCFC;
        ColorHotTo := $E5ECED;
        ColorDown := $E0E6E7;
        ColorDownTo := $D8E0E1;
        BorderColorDown := $AF987A;
        BorderColorHot := $C3CECE;
        BorderColor := clNone;
  //      self.Rounded := True;
        Transparent := True;
      end;
    // Office 2002
    1:begin
        Color := clBtnFace;
        ColorTo := clNone;
        ColorHot := $EED2C1;
        ColorHotTo := clNone;
        ColorDown := $E2B598;
        ColorDownTo := clNone;
        BorderColorDown := $C56A31;
        BorderColorHot := $C56A31;
        BorderColor := clNone;
  //      self.Rounded := False;
        Transparent := True;
      end;
    // XP (Blue)
    2:begin
        Color := $FDEADA;
        ColorTo := $E4AE88;
        ColorHot := $CCF4FF;
        ColorHotTo := $91D0FF;
        ColorDown := $4E91FE;
        ColorDownTo := $8ED3FF;
        BorderColorDown := clBlack;
        BorderColorHot := clBlack;
        BorderColor := clNone;
  //      self.Rounded := False;
        Transparent := True;
      end;
    // XP (Olive)
    3:begin
        Color := $CFF0EA;
        ColorTo := $8CC0B1;
        ColorHot := $CCF4FF;
        ColorHotTo := $91D0FF;
        ColorDown := $4E91FE;
        ColorDownTo := $8ED3FF;
        BorderColorDown := clBlack;
        BorderColorHot := clBlack;
        BorderColor := clNone;
  //      self.Rounded := False;
        Transparent := True;
      end;
    // XP (Silver)
    4:begin
        Color := $ECE2E1;
        ColorTo := $B39698;
        ColorHot := $CCF4FF;
        ColorHotTo := $91D0FF;
        ColorDown := $4E91FE;
        ColorDownTo := $8ED3FF;
        BorderColorDown := clBlack;
        BorderColorHot := clBlack;
        BorderColor := clNone;
  //      self.Rounded := False;
        Transparent := True;
      end;
    // Flat style
    5:begin
        Color := clBtnFace;
        ColorTo := clNone;
        ColorHot := clBtnFace;
        ColorHotTo := clNone;
        ColorDown := $00D8D3D2;
        ColorDownTo := clNone;
        BorderColorDown := clNone;
        BorderColorHot := clNone;
        BorderColor := clNone;
  //      self.Rounded := false;
        Transparent := True;
      end;
    // Avant garde
    6:begin
        Color := $00CAFFFF;
        ColorTo := $00A6FFFF;
        ColorHot := $00A8F0FD;
        ColorHotTo := $007CE9FC;
        ColorDown := $004DE0FB;
        ColorDownTo := $007AE9FC;
        BorderColorDown := clGray;
        BorderColorHot := clGray;
        BorderColor := clNone;
  //      self.Rounded := false;
        Transparent := True;
      end;
      // WindowsVista
    7:begin
        Color := $FDF8F1;
        ColorTo := $FCEFD5;
        ColorHot := $FFFDF9;
        ColorHotTo := $FFFAF0;
        ColorDown := $FEF9F0;
        ColorDownTo := $FDF0D7;
        BorderColorDown := $FEDF9A;
        BorderColorHot := $FCF2DA;
        BorderColor := $FDDE99;
  //      self.Rounded := True;
        Transparent := True;
      end;
      // Windows7
    8:begin
        Color := $FCEBDC;
        ColorTo := $FCDBC1;
        ColorHot := $FDFBFA;
        ColorHotTo := $FDF3EB;
        ColorDown := $FCEBDC;
        ColorDownTo := $FCDBC1;
        BorderColorDown := $CEA27D;
        BorderColorHot := $FBD6B8;
        BorderColor := $CEA27D;
  //      self.Rounded := True;
        Transparent := True;
      end;
      // Terminal
    9:begin
        Color := clBtnFace;
        ColorTo := clBtnFace;
        ColorHot := clSilver;
        ColorHotTo := clSilver;
        ColorDown := clHighLight;
        ColorDownTo := clHighLight;
        BorderColorDown := clGray;
        BorderColorHot := clGray;
        BorderColor := clGray;
  //    self.Rounded := True;
        Transparent := True;
    end;

  // Office2010Blue
    10:begin
        Color := $FDF6EF;
        ColorTo := $F0DAC7;
        ColorHot := $8AE3FD;
        ColorHotTo := $D9F9FD;
        ColorDown := $6CD0FF;
        ColorDownTo := $7BEEFF;
        BorderColorDown := $308AC2;
        BorderColorHot := $58CAF1;
        BorderColor := $C7B29F;
        Transparent := True;
    end;

    // Office2010Silver
    11:begin
        Color := $FFFFFF;
        ColorTo := $EDE5E0;
      	BorderColor := $D2CDC8;
        ColorHot := $8AE3FD;
        ColorHotTo := $D9F9FD;
        BorderColorHot := $58CAF1;
        ColorDown := $6CD0FF;
        ColorDownTo := $7BEEFF;
        BorderColorDown := $308AC2;
        Transparent := True;
    end;
    // Office2010Black
    12:begin
        Color := $BFBFBF;
        ColorTo := $919191;
        BorderColor := $6D6D6D;
        ColorHot := $8AE3FD;
        ColorHotTo := $D9F9FD;
        BorderColorHot := $58CAF1;
        ColorDown := $6CD0FF;
        ColorDownTo := $7BEEFF;
        BorderColorDown := $308AC2;
        Transparent := True;
    end;
    // Windows8
    13:begin
        Color := $F7F6F5;
        ColorTo := clNone;
        BorderColor := $E4E3E2;
        ColorHot := $F7EFE8;
        ColorHotTo := clNone;
        BorderColorHot := $F9CEA4;
        ColorDown := $F7E0C9;
        ColorDownTo := clNone;
        BorderColorDown := $E4A262;
        Transparent := True;
    end;
    // Office2013White
    14:begin
        Color := clWhite;
        ColorTo := clNone;
        BorderColor := $D4D4D4;
        ColorHot := $FCF0E4;
        ColorHotTo := clNone;
        BorderColorHot := $EAB47E;
        ColorDown := $FCE2C8;
        ColorDownTo := clNone;
        BorderColorDown := $E59D56;
        Transparent := True;
    end;
    // Office2013LightGray
    15:begin
        Color := clWhite;
        ColorTo := clNone;
        BorderColor := $C6C6C6;
        ColorHot := $FCF0E4;
        ColorHotTo := clNone;
        BorderColorHot := $EAB47E;
        ColorDown := $FCE2C8;
        ColorDownTo := clNone;
        BorderColorDown := $E59D56;
        Transparent := True;
    end;
    // Office2013Gray
    16:begin
        Color := $E5E5E5;
        ColorTo := clNone;
        BorderColor := $ABABAB;
        ColorHot := $FCF0E4;
        ColorHotTo := clNone;
        BorderColorHot := $EAB47E;
        ColorDown := $FCE2C8;
        ColorDownTo := clNone;
        BorderColorDown := $E59D56;
        Transparent := True;
    end;
   // Windows10
    17:begin
        Color := $F7F6F5;
        ColorTo := clNone;
        BorderColor := $E4E3E2;
        ColorHot := $F7EFE8;
        ColorHotTo := clNone;
        BorderColorHot := $F9CEA4;
        ColorDown := $F7E0C9;
        ColorDownTo := clNone;
        BorderColorDown := $E4A262;
        Transparent := True;
    end;
    // Office2016White
    18:begin
        Color := clWhite;
        ColorTo := clNone;
        BorderColor := $D4D4D4;
        ColorHot := $F2E1D5;
        ColorHotTo := clNone;
        BorderColorHot := $F2E1D5;
        ColorDown := $E3BDA3;
        ColorDownTo := clNone;
        BorderColorDown := $E3BDA3;
        Transparent := True;
    end;
    // Office2016Gray
    19:begin
        Color := $B2B2B2;
        ColorTo := clNone;
        BorderColor := $444444;
        ColorHot := $F2E1D5;
        ColorHotTo := clNone;
        BorderColorHot := $F2E1D5;
        ColorDown := $E3BDA3;
        ColorDownTo := clNone;
        BorderColorDown := $E3BDA3;
        Transparent := True;
    end;
    // Office2016Black
    20:begin
        Color := $363636;
        ColorTo := clNone;
        BorderColor := $444444;
        ColorHot := $6A6A6A;
        ColorHotTo := clNone;
        BorderColorHot := $6A6A6A;
        ColorDown := $444444;
        ColorDownTo := clNone;
        BorderColorDown := $444444;
        Transparent := True;
    end;
    end;

  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetGlyphCustomSize(const Value: Integer);
begin
  FGlyphCustomSize := Value;
  GlyphSize := GlyphSize;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetGlyphResName(const Value: string);
begin
  FGlyphResName := Value;
  GlyphSize := GlyphSize;
end;

//------------------------------------------------------------------------------

function TDBAdvGlowNavigator.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TDBAdvGlowNavigator.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetTransparent(const Value: Boolean);
var
  I: TAdvNavigateBtn;
begin
  if (FTransparent <> Value) then
  begin
    FTransparent := Value;
    for I := Low(FButtons) to High(FButtons) do
    begin
      FButtons[I].Transparent := FTransparent;
      SetButtonsShape;
      FButtons[I].Invalidate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.SetAppearance(
  const Value: TGlowButtonAppearance);
begin
  FAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowNavigator.OnAppearanceChanged(Sender: TObject);
var
  J: TAdvNavigateBtn;
begin
  for J := Low(FButtons) to High(FButtons) do
    FButtons[J].Appearance.Assign(Self.Appearance);
end;

procedure TDBAdvGlowNavigator.Paint;
var
  R: TRect;
  P: TPoint;
  DC: Integer;
begin

  R := ClientRect;

  if Transparent then
  begin
    DC := SaveDC(Canvas.Handle);
    P := ClientOrigin;
    Windows.ScreenToClient(Parent.Handle, P);
    P.x := -P.x;
    P.y := -P.y;
    MoveWindowOrg(Canvas.Handle, P.x, P.y);
    SendMessage(Parent.Handle, WM_ERASEBKGND, Canvas.Handle, 0);
    // transparency ?
    SendMessage(Parent.Handle, WM_PAINT, Canvas.Handle, 0);
    if (Parent is TWinCtrl) then
      (Parent as TWinCtrl).PaintCtrls(Canvas.Handle, nil);
    RestoreDC(Canvas.Handle, DC);
  end
  else

    inherited;
end;

//------------------------------------------------------------------------------

{TAdvGlowNavButton}

constructor TAdvGlowNavButton.Create(AOwner: TComponent);
begin
  inherited;
  FGlyph := TBitmap.Create;
  FGlyph.Transparent := True;
  FGlyphHot := TBitmap.Create;
  FGlyphHot.Transparent := True;
  FGlyphDisabled := TBitmap.Create;
  FGlyphDisabled.Transparent := True;
end;

//------------------------------------------------------------------------------

destructor TAdvGlowNavButton.Destroy;
begin
  //if FRepeatTimer <> nil then
    //FRepeatTimer.Free;
  FGlyph.Free;
  FGlyphHot.Free;
  FGlyphDisabled.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TAdvGlowNavButton.GetToolImage(bmp: TBitmap);
begin
  inherited;
  if not Picture.Empty then
    Exit;
    
  if Enabled or GlyphDisabled.Empty then
  begin
    if FHot and not GlyphHot.Empty then
      bmp.Assign(GlyphHot)
    else
      bmp.Assign(Glyph);
  end
  else
    bmp.Assign(GlyphDisabled);
end;

//------------------------------------------------------------------------------

procedure TAdvGlowNavButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if nsAllowTimer in FNavStyle then
  begin

  end;
end;

//------------------------------------------------------------------------------

procedure TAdvGlowNavButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
end;

//------------------------------------------------------------------------------

procedure TAdvGlowNavButton.Paint;
var
  R: TRect;
begin
  inherited Paint;

  if (GetFocus = Parent.Handle) and
     (FIndex = TDBAdvGlowNavigator (Parent).FocusedButton) then
  begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    if State = absDown then
      OffsetRect(R, 1, 1);
    Canvas.Brush.Style := bsSolid;
    Font.Color := clBtnShadow;
    DrawFocusRect(Canvas.Handle, R);
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvGlowNavButton.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvGlowNavButton.SetGlyphDisabled(const Value: TBitmap);
begin
  FGlyphDisabled.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvGlowNavButton.SetGlyphHot(const Value: TBitmap);
begin
  FGlyphHot.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvGlowNavButton.SetNavStyle(const Value: TNavButtonStyle);
begin
  if (FNavStyle <> Value) then
  begin
    FNavStyle := Value;
    RepeatClick := (nsAllowTimer in FNavStyle);
  end;
end;

//------------------------------------------------------------------------------

{ TAdvNavDataLink }

constructor TAdvNavDataLink.Create(ANav: TDBAdvGlowNavigator);
begin
  inherited Create;
  FNavigator := ANav;
  VisualControl := True;
end;

//------------------------------------------------------------------------------

destructor TAdvNavDataLink.Destroy;
begin
  FNavigator := nil;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TAdvNavDataLink.EditingChanged;
begin
  if FNavigator <> nil then FNavigator.EditingChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvNavDataLink.DataSetChanged;
begin
  if FNavigator <> nil then FNavigator.DataChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvNavDataLink.ActiveChanged;
begin
  if FNavigator <> nil then FNavigator.ActiveChanged;
end;

//------------------------------------------------------------------------------

end.
