{*************************************************************************}
{ TDBADVNAVIGATOR component                                               }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2002 - 2013                                       }
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

unit DBAdvNavigator;

interface

{$R DBADVNAV.RES}
{$I TMSDEFS.INC}

uses
  DBCtrls, Classes, Windows, Messages, StdCtrls, AdvToolBtn, DB, Controls,
  ExtCtrls, Dialogs, Buttons, Graphics
  , Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 2; // Build nr.

  // version history
  // 1.3.0.0 : New property DeleteDisabled added
  // 1.3.1.0 : New property InsertDisabled added
  // 1.3.1.1 : Fixed : update issue with InsertDisabled, DeleteDisabled property setters
  // 1.3.1.2 : Fixed : Issue with bookmark handling in unicode Delphi

type
  TAdvNavButton = class;

  TAdvNavDataLink = class;

  TNavigatorOrientation = (noHorizontal, noVertical);

  TAdvNavigateBtn = (nbFirst, nbPrior, nbNext, nbLast,
    nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh, nbSearch, nbSetBookmark, nbGotoBookMark);

  TButtonSet = set of TAdvNavigateBtn;

  ENavClick = procedure (Sender: TObject; Button: TAdvNavigateBtn) of object;

  TGlyphSize = (gsSmall, gsLarge, gsCustom);

  TDBAdvNavigator = class (TCustomPanel)
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
    FFlat: Boolean;
    FBorderColor: TColor;
    FBorderDownColor: TColor;
    FBorderHotColor: TColor;
    FColorDown: TColor;
    FColorDownTo: TColor;    
    FColorHot: TColor;
    FColorHotTo: TColor;
    FColor: TColor;
    FColorTo: TColor;    
    FDeleteDisabled: Boolean;
    FInsertDisabled: Boolean;
    FShaded: Boolean;
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
    FBookmark: string;
    FUBookmark: TBookmark;
    FAutoThemeAdapt: Boolean;
    FLook: Integer;
    FGlyphCustomSize: Integer;
    FGlyphResName: string;
    //FRounded: Boolean;
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ClickHandler(Sender: TObject);
    function GetDataSource: TDataSource;
    function GetHints: TStrings;
    procedure HintsChanged(Sender: TObject);
    procedure InitButtons;
    procedure InitHints;
    procedure InitStyles;
    procedure SetDataSource(Value: TDataSource);
    procedure SetFlat(Value: Boolean);
    procedure SetHints(Value: TStrings);
    procedure SetSize(var W: Integer; var H: Integer);
    procedure SetVisible(Value: TButtonSet);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderDownColor(const Value: TColor);
    procedure SetBorderHotColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetColorDown(const Value: TColor);
    procedure SetColorDownTo(const Value: TColor);
    procedure SetColorHot(const Value: TColor);
    procedure SetColorHotTo(const Value: TColor);
    procedure SetDeleteDisabled(const Value: Boolean);
    procedure SetInsertDisabled(const Value: Boolean);
    procedure SetShaded(const Value: Boolean);
    //procedure SetRounded(const Value: Boolean);
    procedure SetOrientation(const Value: TNavigatorOrientation);
    procedure SetGlyphSize(const Value: TGlyphSize);
    procedure SetLook(const Value: Integer);
    procedure SetGlyphCustomSize(const Value: Integer);
    procedure SetGlyphResName(const Value: string);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetButtons(index: TAdvNavigateBtn): TAdvNavButton;
  protected
    FButtons: array[TAdvNavigateBtn] of TAdvNavButton;
    function GetVersionNr: Integer; virtual;
    procedure DataChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    {$IFNDEF DELPHI_UNICODE}
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    {$ENDIF}
    procedure CalcMinSize(var W, H: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF DELPHI_UNICODE}
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    {$ENDIF}
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BtnClick(Index: TAdvNavigateBtn); virtual;
    property Look: Integer read FLook write SetLook;
    property BookMark: string read FBookmark write FBookmark;
    property Buttons[index: TAdvNavigateBtn]:TAdvNavButton read GetButtons;
  published
    property AutoThemeAdapt: Boolean read FAutoThemeAdapt write FAutoThemeAdapt;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property VisibleButtons: TButtonSet read FVisibleButtons write SetVisible
      default [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete,
        nbEdit, nbPost, nbCancel, nbRefresh];
    property Align;
    property Anchors;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property BorderDownColor: TColor read FBorderDownColor write SetBorderDownColor default clBlack;
    property BorderHotColor: TColor read FBorderHotColor write SetBorderHotColor default clBlack;
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property ColorDown: TColor read FColorDown write SetColorDown;
    property ColorDownTo: TColor read FColorDownTo write SetColorDownTo;
    property ColorHot: TColor read FColorHot write SetColorHot;
    property ColorHotTo: TColor read FColorHotTo write SetColorHotTo;
    property Constraints;
    property DeleteDisabled: Boolean read FDeleteDisabled write SetDeleteDisabled;
    property InsertDisabled: Boolean read FInsertDisabled write SetInsertDisabled;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default True;
    property GlyphSize: TGlyphSize read FGlyphSize write SetGlyphSize;
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
    property Shaded: Boolean read FShaded write SetShaded default True;
    property ShowHint;
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

{ TAdvNavButton }

  TAdvNavButton = class(TAdvToolButton)
  private
    FIndex: TAdvNavigateBtn;
    FNavStyle: TNavButtonStyle;
    FRepeatTimer: TTimer;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    destructor Destroy; override;
    property NavStyle: TNavButtonStyle read FNavStyle write FNavStyle;
    property Index : TAdvNavigateBtn read FIndex write FIndex;
  end;

{ TNavDataLink }

  TAdvNavDataLink = class(TDataLink)
  private
    FNavigator: TDBAdvNavigator;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: TDBAdvNavigator);
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

{ TDBAdvNavigator }

var
  BtnTypeAdvName: array[TAdvNavigateBtn] of PChar = ('TMSNAVFIRST', 'TMSNAVPREVIOUS', 'TMSNAVNEXT',
    'TMSNAVLAST', 'TMSNAVINSERT', 'TMSNAVDELETE', 'TMSNAVEDIT', 'TMSNAVPOST', 'TMSNAVCANCEL', 'TMSNAVREFRESH',
    'TMSNAVSEARCH','TMSNAVBOOK','TMSNAVGOTO');

constructor TDBAdvNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if not NewStyleControls then ControlStyle := ControlStyle + [csFramed];
  FDataLink := TAdvNavDataLink.Create(Self);
  FVisibleButtons := [nbFirst, nbPrior, nbNext, nbLast, nbInsert,
    nbDelete, nbEdit, nbPost, nbCancel, nbRefresh];
  FHints := TStringList.Create;
  TStringList(FHints).OnChange := HintsChanged;
  FFlat := True;

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
  FShaded := True;
 
  FColor := clBtnFace;
  FColorTo := clNone;
  FColorHot := RGB(199,199,202);
  FColorHotTo := clNone;
  FColorDown := RGB(210,211,216);
  FColorDownTo := clNone;
  FBorderColor := clNone;
  FBorderHotColor := clBlack;
  FBorderDownColor := clBlack;

  FDeleteDisabled := False;
  FInsertDisabled := False;
  FBookmark := '';
end;

destructor TDBAdvNavigator.Destroy;
begin
  FDefHints.Free;
  FDataLink.Free;
  FHints.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TDBAdvNavigator.InitButtons;
var
  I: TAdvNavigateBtn;
  Btn: TAdvNavButton;
  X: Integer;
  ResName: string;
begin
  case GlyphSize of
  gsSmall:
    MinBtnSize := Point(20, 18);
  gsLarge:
    MinBtnSize := Point(32, 30);
  gsCustom:
    MinBtnSize := Point(GlyphCustomSize, GlyphCustomSize - 2);
  end;

  X := 0;
  for I := Low(FButtons) to High(FButtons) do
  begin
    Btn := TAdvNavButton.Create (Self);
    Btn.Flat := Flat;
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

    Btn.Glyph.LoadFromResourceName(Hinstance,ResName);

    case GlyphSize of
    gsSmall:
      FmtStr(ResName, '%sH', [BtnTypeAdvName[I]]);
    gsLarge:
      FmtStr(ResName, '%sLH', [BtnTypeAdvName[I]]);
    gsCustom:
      FmtStr(ResName, '%s'+GlyphResname+'H', [BtnTypeAdvName[I]]);
    end;

    if FindResource(HInstance,PChar(ResName),RT_BITMAP) <> 0 then
      Btn.GlyphHot.LoadFromResourceName(Hinstance,ResName);

    Btn.Shaded := FShaded;

    case GlyphSize of
    gsSmall:
      FmtStr(ResName, '%sD', [BtnTypeAdvName[I]]);
    gsLarge:
      FmtStr(ResName, '%sLD', [BtnTypeAdvName[I]]);
    gsCustom:
      FmtStr(ResName, '%s'+GlyphResname+'D', [BtnTypeAdvName[I]]);
    end;

    if FindResource(HInstance,PChar(ResName),RT_BITMAP) <> 0 then
      Btn.GlyphDisabled.LoadFromResourceName(Hinstance,ResName);

    Btn.Enabled := False;
    Btn.Enabled := True;
    Btn.OnClick := ClickHandler;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.Parent := Self;
    FButtons[I] := Btn;
    X := X + MinBtnSize.X;
  end;
  Buttons[nbPrior].NavStyle := Buttons[nbPrior].NavStyle + [nsAllowTimer];
  Buttons[nbNext].NavStyle  := Buttons[nbNext].NavStyle + [nsAllowTimer];
end;

procedure TDBAdvNavigator.InitStyles;
var
  J: TAdvNavigateBtn;
begin
  for J := Low(FButtons) to High(FButtons) do
  begin
    FButtons[J].Color := Color;
    FButtons[J].ColorTo := ColorTo;
    FButtons[J].ColorDown := ColorDown;
    FButtons[J].ColorDownTo := ColorDownTo;
    FButtons[J].ColorHot := ColorHot;
    FButtons[J].ColorHotTo := ColorHotTo;
    FButtons[J].BorderColor := BorderColor;
    FButtons[J].BorderDownColor := BorderDownColor;
    FButtons[J].BorderHotColor := BorderHotColor;
    FButtons[J].AutoThemeAdapt := FAutoThemeAdapt;
    FButtons[J].Rounded := False;
  end;

end;

procedure TDBAdvNavigator.InitHints;
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

procedure TDBAdvNavigator.HintsChanged(Sender: TObject);
begin
  InitHints;
end;

procedure TDBAdvNavigator.SetFlat(Value: Boolean);
var
  I: TAdvNavigateBtn;
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    for I := Low(FButtons) to High(FButtons) do
      FButtons[I].Flat := Value;
  end;
end;

procedure TDBAdvNavigator.SetHints(Value: TStrings);
begin
  if Value.Text = FDefHints.Text then
    FHints.Clear else
    FHints.Assign(Value);
end;

function TDBAdvNavigator.GetHints: TStrings;
begin
  if (csDesigning in ComponentState) and not (csWriting in ComponentState) and
     not (csReading in ComponentState) and (FHints.Count = 0) then
    Result := FDefHints else
    Result := FHints;
end;

function TDBAdvNavigator.GetButtons(index: TAdvNavigateBtn): TAdvNavButton;
begin
  Result := FButtons[index];
end;

procedure TDBAdvNavigator.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TDBAdvNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TDBAdvNavigator.SetVisible(Value: TButtonSet);
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
  Invalidate;
end;

procedure TDBAdvNavigator.CalcMinSize(var W, H: Integer);
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

procedure TDBAdvNavigator.SetSize(var W: Integer; var H: Integer);
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

procedure TDBAdvNavigator.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  if not HandleAllocated then SetSize(W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TDBAdvNavigator.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  W := Width;
  H := Height;
  SetSize(W, H);
end;

procedure TDBAdvNavigator.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  inherited;
  if (SWP_NOSIZE and Message.WindowPos.Flags) = 0 then
    CalcMinSize(Message.WindowPos.cx, Message.WindowPos.cy);
end;

procedure TDBAdvNavigator.ClickHandler(Sender: TObject);
begin
  BtnClick(TAdvNavButton(Sender).Index);
end;

procedure TDBAdvNavigator.BtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldFocus: TAdvNavigateBtn;
begin
  OldFocus := FocusedButton;
  FocusedButton := TAdvNavButton (Sender).Index;
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

procedure TDBAdvNavigator.BtnClick(Index: TAdvNavigateBtn);
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
          FBookmark := Bookmark;
          {$ENDIF}
          {$IFDEF DELPHI_UNICODE}
          FUBookmark := GetBookmark;
          {$ENDIF}

          Buttons[nbGotoBookmark].Enabled := True;
        end;
      nbGotoBookmark:
        if Assigned(FOnBtnGotoBookmark) then
          FOnBtnGotoBookmark(Self)
        else
        begin
          {$IFNDEF DELPHI_UNICODE}
          if FBookmark <> '' then
            Bookmark := FBookmark
          {$ENDIF}

          {$IFDEF DELPHI_UNICODE}
          if Assigned(FUBookmark) then          
            GotoBookmark(FUBookmark);
          {$ENDIF}
        end;
      end;
    end;
  end;
  if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then
    FOnNavClick(Self, Index);
end;

procedure TDBAdvNavigator.WMSetFocus(var Message: TWMSetFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TDBAdvNavigator.WMKillFocus(var Message: TWMKillFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TDBAdvNavigator.KeyDown(var Key: Word; Shift: TShiftState);
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

procedure TDBAdvNavigator.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TDBAdvNavigator.DataChanged;
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

  {$IFDEF DELPHI_UNICODE}
  Buttons[nbGotoBookmark].Enabled := Enabled and FDataLink.Active and Assigned(FUBookmark);
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  Buttons[nbGotoBookmark].Enabled := Enabled and FDataLink.Active and (FBookmark <> '');
  {$ENDIF}
end;

procedure TDBAdvNavigator.EditingChanged;
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

procedure TDBAdvNavigator.ActiveChanged;
var
  I: TAdvNavigateBtn;
begin
  if not (Enabled and FDataLink.Active) then
  begin
    for I := Low(FButtons) to High(FButtons) do
      FButtons[I].Enabled := False;
    FBookmark := '';
    FUBookmark := nil;
  end
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TDBAdvNavigator.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
    ActiveChanged;
end;

procedure TDBAdvNavigator.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
    ActiveChanged;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDBAdvNavigator.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBAdvNavigator.Loaded;
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
  Shaded := FShaded;

  InitStyles;
end;

procedure TDBAdvNavigator.SetColor(const Value: TColor);
var
  J: TAdvNavigateBtn;
begin
  FColor := Value;
  for J := Low(FButtons) to High(FButtons) do
    FButtons[J].Color := Value;
end;

procedure TDBAdvNavigator.SetColorTo(const Value: TColor);
var
  J: TAdvNavigateBtn;
begin
  FColorTo := Value;
  for J := Low(FButtons) to High(FButtons) do
    FButtons[J].ColorTo := Value;
end;

procedure TDBAdvNavigator.SetColorDown(const Value: TColor);
var
  J: TAdvNavigateBtn;
begin
  FColorDown := Value;
  for J := Low(FButtons) to High(FButtons) do
    FButtons[J].ColorDown := Value;
end;

procedure TDBAdvNavigator.SetColorDownTo(const Value: TColor);
var
  J: TAdvNavigateBtn;
begin
  FColorDownTo := Value;
  for J := Low(FButtons) to High(FButtons) do
    FButtons[J].ColorDownTo := Value;
end;


procedure TDBAdvNavigator.SetColorHot(const Value: TColor);
var
  J: TAdvNavigateBtn;
begin
  FColorHot := Value;
  for J := Low(FButtons) to High(FButtons) do
    FButtons[J].ColorHot := Value;
end;

procedure TDBAdvNavigator.SetColorHotTo(const Value: TColor);
var
  J: TAdvNavigateBtn;
begin
  FColorHotTo := Value;
  for J := Low(FButtons) to High(FButtons) do
    FButtons[J].ColorHotTo := Value;
end;
procedure TDBAdvNavigator.SetDeleteDisabled(const Value: Boolean);
begin
  FDeleteDisabled := Value;
  DataChanged;
end;

procedure TDBAdvNavigator.SetInsertDisabled(const Value: Boolean);
begin
  FInsertDisabled := Value;
  DataChanged;
end;

{
procedure TDBAdvNavigator.SetRounded(const Value: Boolean);
var
  I: TAdvNavigateBtn;
begin
  FRounded := Value;
  for I := Low(Buttons) to High(Buttons) do
  begin
    Buttons[I].Rounded := FRounded;
  end;
end;
}

procedure TDBAdvNavigator.SetShaded(const Value: Boolean);
var
  I: TAdvNavigateBtn;
begin
  FShaded := Value;
  for I := Low(FButtons) to High(FButtons) do
  begin
    FButtons[I].Shaded := FShaded;
  end;
end;

procedure TDBAdvNavigator.SetOrientation(const Value: TNavigatorOrientation);
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

procedure TDBAdvNavigator.SetGlyphSize(const Value: TGlyphSize);
var
  I: TAdvNavigateBtn;
  ResName: string;
begin
  if (FGlyphSize <> Value) or (Value = gsCustom) then
  begin
    FGlyphSize := Value;

    case GlyphSize of
    gsSmall:
      MinBtnSize := Point(20, 18);
    gsLarge:
      MinBtnSize := Point(32, 30);
    gsCustom:
      MinBtnSize := Point(GlyphCustomSize, GlyphCustomSize - 2);
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

      FButtons[I].Glyph.LoadFromResourceName(Hinstance,ResName);

      case GlyphSize of
      gsSmall:
        FmtStr(ResName, '%sH', [BtnTypeAdvName[I]]);
      gsLarge:
        FmtStr(ResName, '%sLH', [BtnTypeAdvName[I]]);
      gsCustom:
        FmtStr(ResName, '%s'+GlyphResname+'H', [BtnTypeAdvName[I]]);
      end;

      if FindResource(HInstance,PChar(ResName), RT_BITMAP) <> 0 then
        FButtons[I].GlyphHot.LoadFromResourceName(Hinstance,ResName);

      case GlyphSize of
      gsSmall:
        FmtStr(ResName, '%sD', [BtnTypeAdvName[I]]);
      gsLarge:
        FmtStr(ResName, '%sLD', [BtnTypeAdvName[I]]);
      gsCustom:
        FmtStr(ResName, '%s'+GlyphResname+'D', [BtnTypeAdvName[I]]);
      end;

      if FindResource(HInstance,PChar(ResName), RT_BITMAP) <> 0 then
        FButtons[I].GlyphDisabled.LoadFromResourceName(Hinstance,ResName);
    end;
  end;
end;

procedure TDBAdvNavigator.SetBorderColor(const Value: TColor);
var
  J: TAdvNavigateBtn;
begin
  FBorderColor := Value;
  for J := Low(FButtons) to High(FButtons) do
    FButtons[J].BorderColor := Value;
end;

procedure TDBAdvNavigator.SetBorderDownColor(const Value: TColor);
var
  J: TAdvNavigateBtn;
begin
  FBorderDownColor := Value;
  for J := Low(FButtons) to High(FButtons) do
    FButtons[J].BorderDownColor := Value;
end;

procedure TDBAdvNavigator.SetBorderHotColor(const Value: TColor);
var
  J: TAdvNavigateBtn;
begin
  FBorderHotColor := Value;
  for J := Low(FButtons) to High(FButtons) do
    FButtons[J].BorderHotColor := Value;
end;

procedure TDBAdvNavigator.SetLook(const Value: Integer);
begin
  FLook := Value;
  case Value of
  // Windows XP
  0:begin
      self.Color := $EDF1F1;
      self.ColorTo := $DFEBEB;
      self.ColorHot := $FAFCFC;
      self.ColorHotTo := $E5ECED;
      self.ColorDown := $E0E6E7;
      self.ColorDownTo := $D8E0E1;
      self.BorderDownColor := $AF987A;
      self.BorderHotColor := $C3CECE;
      self.BorderColor := clNone;
//      self.Rounded := True;
      self.Flat := True;
    end;
  // Office 2002
  1:begin
      self.Color := clBtnFace;
      self.ColorTo := clNone;
      self.ColorHot := $EED2C1;
      self.ColorHotTo := clNone;
      self.ColorDown := $E2B598;
      self.ColorDownTo := clNone;
      self.BorderDownColor := $C56A31;
      self.BorderHotColor := $C56A31;
      self.BorderColor := clNone;
//      self.Rounded := False;
      self.Flat := True;
    end;
  // XP (Blue)
  2:begin
      self.Color := $FDEADA;
      self.ColorTo := $E4AE88;
      self.ColorHot := $CCF4FF;
      self.ColorHotTo := $91D0FF;
      self.ColorDown := $4E91FE;
      self.ColorDownTo := $8ED3FF;
      self.BorderDownColor := clBlack;
      self.BorderHotColor := clBlack;
      self.BorderColor := clNone;
//      self.Rounded := False;
      self.Flat := True;
    end;
  // XP (Olive)
  3:begin
      self.Color := $CFF0EA;
      self.ColorTo := $8CC0B1;
      self.ColorHot := $CCF4FF;
      self.ColorHotTo := $91D0FF;
      self.ColorDown := $4E91FE;
      self.ColorDownTo := $8ED3FF;
      self.BorderDownColor := clBlack;
      self.BorderHotColor := clBlack;
      self.BorderColor := clNone;
//      self.Rounded := False;
      self.Flat := True;
    end;
  // XP (Silver)
  4:begin
      self.Color := $ECE2E1;
      self.ColorTo := $B39698;
      self.ColorHot := $CCF4FF;
      self.ColorHotTo := $91D0FF;
      self.ColorDown := $4E91FE;
      self.ColorDownTo := $8ED3FF;
      self.BorderDownColor := clBlack;
      self.BorderHotColor := clBlack;
      self.BorderColor := clNone;
//      self.Rounded := False;
      self.Flat := True;
    end;
  // Flat style
  5:begin
      self.Color := clBtnFace;
      self.ColorTo := clNone;
      self.ColorHot := clBtnFace;
      self.ColorHotTo := clNone;
      self.ColorDown := $00D8D3D2;
      self.ColorDownTo := clNone;
      self.BorderDownColor := clNone;
      self.BorderHotColor := clNone;
      self.BorderColor := clNone;
//      self.Rounded := false;
      self.Flat := True;
    end;
  // Avant garde
  6:begin
      self.Color := $00CAFFFF;
      self.ColorTo := $00A6FFFF;
      self.ColorHot := $00A8F0FD;
      self.ColorHotTo := $007CE9FC;
      self.ColorDown := $004DE0FB;
      self.ColorDownTo := $007AE9FC;
      self.BorderDownColor := clGray;
      self.BorderHotColor := clGray;
      self.BorderColor := clNone;
//      self.Rounded := false;
      self.Flat := True;
    end;
  end;

end;

procedure TDBAdvNavigator.SetGlyphCustomSize(const Value: Integer);
begin
  FGlyphCustomSize := Value;
  GlyphSize := GlyphSize;
end;

procedure TDBAdvNavigator.SetGlyphResName(const Value: string);
begin
  FGlyphResName := Value;
  GlyphSize := GlyphSize;
end;

function TDBAdvNavigator.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TDBAdvNavigator.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TDBAdvNavigator.SetVersion(const Value: string);
begin

end;

{TAdvNavButton}

destructor TAdvNavButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TAdvNavButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if nsAllowTimer in FNavStyle then
  begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled  := True;
  end;
end;

procedure TAdvNavButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;

procedure TAdvNavButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FState = bsDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TAdvNavButton.Paint;
var
  R: TRect;
begin
  inherited Paint;
  if (GetFocus = Parent.Handle) and
     (FIndex = TDBAdvNavigator (Parent).FocusedButton) then
  begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    if FState = bsDown then
      OffsetRect(R, 1, 1);
    Canvas.Brush.Style := bsSolid;
    Font.Color := clBtnShadow;
    DrawFocusRect(Canvas.Handle, R);
  end;
end;

{ TAdvNavDataLink }

constructor TAdvNavDataLink.Create(ANav: TDBAdvNavigator);
begin
  inherited Create;
  FNavigator := ANav;
  VisualControl := True;
end;

destructor TAdvNavDataLink.Destroy;
begin
  FNavigator := nil;
  inherited Destroy;
end;

procedure TAdvNavDataLink.EditingChanged;
begin
  if FNavigator <> nil then FNavigator.EditingChanged;
end;

procedure TAdvNavDataLink.DataSetChanged;
begin
  if FNavigator <> nil then FNavigator.DataChanged;
end;

procedure TAdvNavDataLink.ActiveChanged;
begin
  if FNavigator <> nil then FNavigator.ActiveChanged;
end;


end.
