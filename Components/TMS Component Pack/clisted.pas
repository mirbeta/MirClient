{********************************************************************}
{ TCheckListEdit component                                           }
{ for Delphi  & C++Builder                                           }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1999-2014                                   }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the author and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit clisted;

{$I TMSDEFS.INC}

interface


uses
  Windows, Classes, StdCtrls, ExtCtrls, Controls, Messages, SysUtils,
  Forms, Graphics, Buttons, CheckLst, Menus, CEXPVS, Types;

const
  CL_CHECKED = $8;
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 9; // Release nr.
  BLD_VER = 0; // Build nr.


  // version history
  // 1.2.1.0 : Added property DropDownPopupMenu
  // 1.2.2.0 : Added XP style dropdownbutton
  // 1.2.2.1 : fixed issue with Checked[] setting from OnChange event
  // 1.2.2.2 : improved lineup of dropdown part
  // 1.2.3.0 : Added support for multicharacter delimiters
  // 1.3.0.0 : Added capability to enable/disable individual checkboxes with ItemEnabled[index]: boolean
  //         : Added OnShowCheckList event
  // 1.3.0.1 : Fix for QC-36372 issue with Refresh call in ShowCheckList
  // 1.3.1.0 : Added property Selected[i]: boolean to get/set selected items in dropdown
  // 1.3.2.0 : Added : selection of checkbox & closing dropdown with return key
  // 1.3.2.1 : Fixed : issue with TextStartChar & TextEndChar being equal
  // 1.3.2.2 : Fixed : disabled painting of dropdown button when Enabled = false
  // 1.3.2.3 : Fixed : issue with setting items at design time
  // 1.3.2.4 : Fixed : issue with setting dropdown width
  // 1.3.3.0 : Improved : show dropdown with focus based on key pressed in edit control
  //         : Improved : allows to set TextStartChar, TextEndChar to empty string at design time
  // 1.3.3.1 : Fixed : issue with handling Return key
  // 1.3.4.0 : Improved : created virtual protected methods StringToCheck, CheckToString, DoClickCheck
  // 1.3.5.0 : New : Exposed ShowCheckList/HideCheckList methods
  // 1.3.5.1 : Fixed : Issue with ReadOnly = true
  // 1.3.6.0 : New : DropDownEnabled property added
  // 1.3.6.1 : Fixed : Issue with OnChange triggered during loading
  // 1.3.6.2 : Fixed : Issue with text drawing when Ctl3D = false
  // 1.3.6.3 : Fixed : Issue with retrieving State[index] value
  // 1.3.6.4 : Fixed : Issue with persisting checked state
  // 1.3.7.0 : New : Added protected virtual method DoOnShowCheckList
  // 1.3.7.1 : Fixed : Issue with dropdown when items list is empty
  // 1.3.8.0 : New : Added BeginUpdate/EndUpdate methods for faster operation of programmatically checking/unchecking large nr. of items
  // 1.3.9.0 : New : Methods CheckAll/UncheckAll added

type
  TCheckListEdit = class;

{TDropForm}
  TDropForm = class(TForm)
  private
    procedure WMClose(var Msg: TMessage); message WM_CLOSE;
  end;

{TIntList}
  TIntList = class(TList)
  private
    procedure SetInteger(Index: Integer; Value: Integer);
    function GetInteger(Index: Integer): Integer;
  public
    constructor Create;
    property Items[index: Integer]: Integer read GetInteger write SetInteger; default;
    procedure Add(Value: integer);
    procedure Delete(Index: Integer);
  end;

{TInplaceCheckListBox}

  TInplaceCheckListBox = class(TCheckListBox)
  private
    FParentEdit: TCheckListEdit;
    procedure WMKeyDown(var Msg: TWMKeydown); message wm_keydown;
  protected
    procedure DoExit; override;
    function GetDropDownWidth: Integer;
    property ParentEdit: TCheckListEdit read fParentEdit write fParentEdit;
  end;

{ TDropCheckListButton }
  TDropCheckListButton = class(TSpeedButton)
  private
    FFocusControl: TWinControl;
    FMouseClick: TNotifyEvent;
    FIsWinXP: Boolean;
    FHover: Boolean;
    procedure WMLButtonDown(var Msg: TMessage); message WM_LBUTTONDOWN;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  protected
    procedure Paint; override;
    property Hover: Boolean read FHover write FHover;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    property IsWinXP: Boolean read FIsWinXP write FIsWinXP;
  published
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    property MouseClick: TNotifyEvent read FMouseClick write FMouseClick;
  end;

  TCheckListItemToText = procedure(sender: TObject; var aText: string) of object;
  TTextToCheckListItem = procedure(sender: TObject; var aItem: string) of object;

  TDropDirection = (ddDown, ddUp);


{ TCheckListEdit }
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCheckListEdit = class(TCustomEdit)
  private
    FButton: TDropCheckListButton;
    FEditorEnabled: Boolean;
    FOnClickBtn: TNotifyEvent;
    FCheckListBox: TInplaceCheckListBox;
    FItems: TStringList;
    FDropHeight: integer;
    FDropWidth: integer;
    FDropColumns: integer;
    FDropColor: TColor;
    FDropFont: TFont;
    FDropFlat: Boolean;
    FDropSorted: Boolean;
    FDropDirection: TDropDirection;
    FDroppedDown: Boolean;
    FDropDownEnabled: Boolean;
    CheckFlag: Boolean;
    FChkForm: TForm;
    FIntList: TIntList;
    FChkClosed: Boolean;
    FCloseClick: Boolean;
    FTextDelimiter: string;
    FTextStartChar: string;
    FTextEndChar: string;
    FUpdateCount: integer;
    FOnCheckListItemToText: TCheckListItemToText;
    FOnTextToCheckListItem: TTextToCheckListItem;
    FOnClose: TNotifyEvent;
    FAutoDropWidthSize: Boolean;
    FAppNtfy: TNotifyEvent;
    FDropDownPopupMenu: TPopupMenu;
    FOnShowCheckList: TNotifyEvent;
    FOnClickCheck: TNotifyEvent;  
    function GetItemEnabled(ItemIndex: Integer): Boolean;
    procedure SetItemEnabled(ItemIndex: Integer; const Value: Boolean);
    function GetMinHeight: Integer;
    procedure SetEditRect;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMSysKeyDown(var Msg: TWMKeydown); message WM_SYSKEYDOWN;
    procedure ItemChange(Sender: TObject);
    procedure CheckClick(Sender: TObject);
    procedure CheckClickCheck(Sender: TObject);
    procedure SetItems(value: tstringlist);
    procedure ShowCheckListInt(Focus: boolean;startchar: string);

    function GetCheck(i: integer): boolean;
    procedure SetCheck(i: integer; value: boolean);
    function GetState(i: integer): TCheckBoxState;
    procedure SetState(i: integer; value: TCheckBoxState);
    procedure FormDeactivate(Sender: TObject);
    procedure AppDeactivate(Sender: TObject);
    procedure SetTextDelimiter(const Value: string);
    procedure SetTextEndChar(const Value: string);
    procedure SetTextStartChar(const Value: string);
    procedure MouseClick(Sender: TObject);
    procedure DownClick(Sender: TObject);
    procedure SetDropFont(const Value: TFont);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetSelected(Index: Integer): Boolean;
    procedure SetSelected(Index: Integer; const Value: Boolean);
  protected
    procedure Change; override;
    function GetVersionNr: Integer; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function GetParentForm(Control: TControl): TCustomForm; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DoClickCheck(Sender: TObject); virtual;
    function CheckToString: string; virtual;
    procedure StringToCheck(s: string); virtual;
    procedure DoOnShowCheckList; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property ItemEnabled[ItemIndex: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    procedure CheckAll;
    procedure UnCheckAll;
    property Button: TDropCheckListButton read FButton;
    property Checked[i: Integer]: Boolean read GetCheck write SetCheck;
    property State[i: Integer]: TCheckBoxState read GetState write SetState;
    property Text: string read GetText write SetText;
    property DroppedDown: Boolean read FDroppedDown;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    procedure ShowCheckList;
    procedure HideCheckList;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DragKind;
    property AutoSelect;
    property AutoSize;
    property AutoDropWidthSize: Boolean read FAutoDropWidthSize write FAutoDropWidthSize;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Height;
    property Width;
    property OnChange;
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
    property OnEndDock;
    property OnStartDock;
    property Items: TStringList read FItems write SetItems;
    property DropWidth: integer read FDropWidth write FDropWidth;
    property DropHeight: integer read FDropHeight write FDropHeight default 100;
    property DropColumns: integer read FDropColumns write FDropColumns default 0;
    property DropColor: tColor read FDropColor write FDropColor default clWindow;
    property DropFlat: boolean read FDropFlat write FDropFlat default true;
    property DropFont: TFont read FDropFont write SetDropFont;
    property DropDirection: TDropDirection read fDropDirection write FDropDirection default ddDown;
    property DropSorted: boolean read FDropSorted write FDropSorted default false;
    property DropDownEnabled: boolean read FDropDownEnabled write FDropDownEnabled default true;
    property DropDownPopupMenu: TPopupMenu read FDropDownPopupMenu write FDropDownPopupMenu;

    property TextDelimiter: string read fTextDelimiter write SetTextDelimiter nodefault;
    property TextEndChar: string read fTextEndChar write SetTextEndChar nodefault;
    property TextStartChar: string read fTextStartChar write SetTextStartChar nodefault;

    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnClickBtn: TNotifyEvent read FOnClickBtn write FOnClickBtn;
    property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck; 
    property OnTextToCheckListItem: TTextToCheckListItem read FOnTextToCheckListItem write FOnTextToCheckListItem;
    property OnCheckListItemToText: TCheckListItemToText read FOnCheckListItemToText write FOnCheckListItemToText;
    property Version: string read GetVersion write SetVersion;
    property OnShowCheckList: TNotifyEvent read FOnShowCheckList write FOnShowCheckList;
  end;

procedure Register;

implementation
{$R clisted.res}

procedure Register;
begin
  RegisterComponents('TMS', [TCheckListEdit]);
end;

function GetFileVersion(const AFileName: string): Cardinal;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := Cardinal(-1);
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          Result:= FI.dwFileVersionMS;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

{ TDropCheckListButton }

constructor TDropCheckListButton.Create(AOwner: TComponent);
var i: integer;
begin
  inherited Create(AOwner);
  Cursor := crArrow;
  Glyph.Handle := LoadBitmap(HInstance, 'ARROW_DOWN');
  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsWinXP := (i > 5);
end;

procedure TDropCheckListButton.Paint;
var htheme: THandle;
    ARect: TRect;
begin
  if not (IsWinXP and IsThemeActive) then
    inherited Paint
  else
  begin
    htheme := OpenThemeData(Parent.Handle,'combobox');
    ARect := ClientRect;
    InflateRect(ARect,1,1);
    //InflateRect(ARect,4,4);
    ARect.Left := ARect.Left + 2;

    if not Enabled then
    begin
      DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_DISABLED,@ARect,nil)
    end
    else
      if Down then
        DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_PRESSED,@ARect,nil)
      else
      begin
        if Hover then
          DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_HOT,@ARect,nil)
        else
          DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_NORMAL,@ARect,nil);
      end;

    CloseThemeData(htheme);
  end;
end;

procedure TDropCheckListButton.Click;
begin
  if (FFocusControl <> nil) and FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
    FFocusControl.SetFocus;
  inherited Click;
end;

procedure TDropCheckListButton.WMLButtonDown(var Msg: TMessage);
begin
  if Assigned(FMouseClick) then
    FMouseClick(self);
  inherited;
end;

procedure TDropCheckListButton.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FHover := true;
  Invalidate;
end;

procedure TDropCheckListButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FHover := false;
  Invalidate;
end;

{ TCheckListEdit }

constructor TCheckListEdit.Create(AOwner: TComponent);
var
  FDesignTime: boolean;
begin
  inherited Create(AOwner);
  FButton := TDropCheckListButton.Create(Self);
  FButton.Width := 17;
  FButton.Height := 17;
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.FocusControl := Self;
  FButton.MouseClick := MouseClick;
  FButton.OnClick := DownClick;
  ControlStyle := ControlStyle - [csSetCaption];
  FEditorEnabled := True;
  FCheckListBox := nil;
  FItems := TStringList.Create;
  FItems.OnChange := ItemChange;
  FDropHeight := 100;
  FDropWidth := self.Width;
  FDropSorted := False;
  FDropFlat := True;
  FDropColor := clWindow;
  FDropFont := TFont.Create;
  FDropDownEnabled := true;
  FIntList := TIntList.Create;
  FChkClosed := True;
  FUpdateCount := 0;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
  begin
    Text := '[]';
    FTextStartChar := '[';
    FTextEndChar := ']';
  end;
  
  FTextDelimiter := ',';

  FDroppedDown := False;
end;

destructor TCheckListEdit.Destroy;
begin
  FButton.Free;
  FItems.Free;
  FIntList.Free;
  FDropFont.Free;
  inherited Destroy;
end;

function TCheckListEdit.GetParentForm(Control: TControl): TCustomForm;
begin
  Result := nil;
  if Assigned(Control) then
    if Control is TCustomForm then
    begin
      Result := Control as TCustomForm;
      Exit;
    end else
    begin
      if Assigned(Control.Parent) then
        Result := GetParentForm(Control.Parent);
    end;
end;

procedure TCheckListEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TCheckListEdit.DestroyWnd;
begin
  inherited;
end;

procedure TCheckListEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
  ReadOnly := true;
end;

procedure QuickSortList(List: TStringList; IntList: TIntList; left, right: integer);
var
  i, j, si: integer;
  s, sw: string;
  so: TObject;

begin
  i := left;
  j := right;

 {get middle item here}
  s := List.Strings[(left + right) shr 1];

  repeat
    while (AnsiStrComp(pchar(s), pchar(List.Strings[i])) > 0) and (i < right) do inc(i);
    while (AnsiStrComp(pchar(s), pchar(List.Strings[j])) < 0) and (j > left) do dec(j);

    if (i <= j) then
    begin
      if (i <> j) then
      begin
        if AnsiStrComp(pchar(List.Strings[i]), pchar(List.Strings[j])) <> 0 then
        begin
          sw := List.Strings[i];
          so := List.Objects[i];

          si := IntList.Items[i];

          List.Strings[i] := List.Strings[j];
          List.Objects[i] := List.Objects[j];
          IntList.Items[i] := IntList.Items[j];
          List.Strings[j] := sw;
          List.Objects[j] := so;
          IntList.Items[j] := si;
        end;
      end;
      inc(i);
      dec(j);
    end;
  until (i > j);

  if (left < j) then QuicksortList(List, IntList, left, j);
  if (i < right) then QuickSortList(List, IntList, i, right);
end;


procedure TCheckListEdit.ShowCheckListInt(focus: boolean;startchar: string);
var
  P: TPoint;
  FOldDropDirection: TDropDirection;
  i: integer;
  s: string;
begin
  FOldDropDirection := FDropDirection;

  P := Point(0, 0);
  P := Self.ClientToScreen(P);

  if P.y + FDropHeight >= Screen.DesktopHeight then
    FDropDirection := ddUp;

  if P.y - FDropHeight <= 0 then
    FDropDirection := ddDown;

  FChkForm := TDropForm.CreateNew(self, 0);
  FChkForm.BorderStyle := bsNone;
  FChkForm.FormStyle := fsStayOnTop;
  FChkForm.Visible := False;
  FChkForm.Width := FDropWidth;
  FChkForm.Height := FDropHeight;
  FChkForm.OnDeactivate := FormDeactivate;

  FCheckListBox := TInplaceCheckListBox.Create(FChkForm);
  FCheckListBox.Parent := FChkForm;
  FCheckListBox.Left := 0;
  FCheckListBox.Top := 0;
  FCheckListBox.Align := alClient;
  FCheckListBox.Width := FDropWidth;
  FCheckListBox.Height := FDropHeight;
  FCheckListBox.Color := FDropColor;
  FCheckListBox.Columns := FDropColumns;
  FCheckListBox.Flat := fDropFlat;
  FCheckListBox.Font.Assign(FDropFont);
  FCheckListBox.Sorted := FDropSorted;
  FCheckListBox.PopupMenu := FDropDownPopupMenu;
  FCheckListBox.HelpContext := HelpContext;

  if FDropSorted and (Items.Count > 0) then
     QuickSortList(Items, FIntList, 0, Items.Count - 1);

  FCheckListBox.Font.Assign(FDropFont);
  FCheckListBox.Items.Assign(self.Items);

  if FAutoDropWidthSize then
    FChkForm.Width := FCheckListBox.GetDropDownWidth
  else
    FChkForm.Width := FCheckListBox.Width;

  StringToCheck(Text);

  for i := 1 to FIntList.Count do
  begin
    case (FIntList.Items[i - 1] and $3) of
    1: FChecklistbox.State[i - 1] := cbChecked;
    2: FChecklistbox.State[i - 1] := cbGrayed;
    end;
    FChecklistbox.Checked[i - 1] := FIntList.Items[i - 1] and CL_CHECKED = CL_CHECKED;
    FCheckListBox.ItemEnabled[i - 1] := FDropDownEnabled;
  end;


  FCheckListBox.Ctl3D := false;
  FCheckListBox.OnClick := CheckClick;
  FCheckListBox.OnClickCheck := CheckClickCheck;
  FCheckListBox.ParentEdit := self;

  StringToCheck(self.Text);

  FCheckListBox.Visible := true;

  if startchar = #0 then
    FCheckListBox.ItemIndex := 0
  else
  begin
    for i := 0 to FCheckListBox.Items.Count - 1 do
    begin
      s := FCheckListBox.Items[i];

      if (length(s) > 0) and (uppercase(s[1]) = uppercase(startchar)) then
      begin
        FCheckListBox.ItemIndex := i;
        break;
      end;
    end;
  end;

  FCheckListBox.TabStop := true;

  P := Point(0, 0);
  P := ClientToScreen(P);

  if P.x + FChkForm.Width > Screen.DesktopWidth then
    FChkForm.Left := Screen.DesktopWidth - FChkForm.Width
  else
    FChkForm.Left := P.x - 2;

  if (fDropDirection = ddDown) then
    FChkForm.Top := P.y + Height - 2
  else
    FChkForm.Top := P.y - FDropHeight;

  FChkForm.Show;

  FAppNtfy := Application.OnDeactivate;

  Application.OnDeactivate := AppDeactivate;

  if Focus and not ReadOnly then
    FCheckListBox.SetFocus;

  FCheckListBox.Height := FCheckListBox.Height + 1;
  FCheckListBox.Height := FCheckListBox.Height - 1;

  FDropDirection := fOldDropDirection;

  FChkClosed := False;
  FDroppedDown := True;

  DoOnShowCheckList;

  FCheckListBox.Refresh;
end;

procedure TCheckListEdit.ShowCheckList;
begin
  ShowCheckListInt(true,#0);
end;

procedure TCheckListEdit.HideCheckList;
begin
  PostMessage(FChkForm.Handle, WM_CLOSE, 0, 0);
  FChkClosed := true;
  Application.OnDeactivate := FAppNtfy;
  FDroppedDown := False;

  StringToCheck(Text);

  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TCheckListEdit.AppDeactivate(Sender: TObject);
begin
  HideCheckList;
end;


procedure TCheckListEdit.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
  Loc.Bottom := ClientHeight + 1; {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 3;
  if self.BorderStyle = bsNone then
  begin
    Loc.Top := 2;
    Loc.Left := 2;
  end
  else
  begin
    Loc.Top := 1;
    Loc.Left := 2;
  end;
  SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
end;

procedure TCheckListEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
  Dist: integer;
begin
  inherited;
  if BorderStyle = bsNone then Dist := 2
  else
    Dist := 4;

  MinHeight := GetMinHeight;
    { text edit bug: if size to less than minheight, then edit ctrl does
      not display the text }

  if Height < MinHeight then
    Height := MinHeight
  else if FButton <> nil then
  begin
    if NewStyleControls and Ctl3D and not (BorderStyle = bsNone) then
      FButton.SetBounds(Width - 21, 0, 17 , Height - Dist)
    else
      FButton.SetBounds(Width - FButton.Width - 1, 1, FButton.Width, Height - 3);
    SetEditRect;
  end;
end;

function TCheckListEdit.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  {Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 +2;}
  Result := Metrics.tmHeight + I div 4 {+ GetSystemMetrics(SM_CYBORDER) * 4};
end;
{
procedure TCheckListEdit.Click (Sender: TObject);
begin
end;
}

procedure TCheckListEdit.WMPaste(var Message: TWMPaste);
begin
  // disable paste
  Message.Result := 0;
end;

procedure TCheckListEdit.WMChar(var Message: TWMChar);
begin
  if FEditorEnabled then
    inherited;
end;

procedure TCheckListEdit.WMCut(var Message: TWMPaste);
begin
  Exit;
  inherited;
end;

procedure TCheckListEdit.CMExit(var Message: TCMExit);
begin
  inherited;
end;

function GetCharFromVirtualKey(Key: Word): string;
var
   keyboardState: TKeyboardState;
   asciiResult: Integer;
begin
   GetKeyboardState(keyboardState) ;

   SetLength(Result, 2) ;
   asciiResult := ToAscii(key, MapVirtualKey(key, 0), keyboardState, @Result[1], 0) ;
   case asciiResult of
     0: Result := '';
     1: SetLength(Result, 1) ;
     2:;
     else
       Result := '';
   end;
end;


procedure TCheckListEdit.WMKeyDown(var Msg: TWMKeydown);
begin
  inherited;

  if not (msg.CharCode in [VK_LEFT,VK_RIGHT,VK_HOME,VK_END,VK_NEXT,VK_PRIOR]) then

  if (msg.CharCode = VK_DOWN) or (msg.CharCode = VK_F4) then
  begin
    //outputdebugstring(pchar(inttostr(ToAscii(msg.Charcode)));
    ShowCheckListInt(true,GetCharFromVirtualKey(Msg.CharCode));

  end;
end;

procedure TCheckListEdit.WMSysKeyDown(var Msg: TWMKeydown);
begin
  inherited;
  if (msg.CharCode = VK_DOWN) and (GetKeyState(VK_MENU) and $8000 = $8000) then
    ShowCheckListInt(true,#0);
end;

procedure TCheckListEdit.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  FButton.Enabled := self.Enabled;
end;

procedure TCheckListEdit.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TCheckListEdit.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TCheckListEdit.Change;
begin
  if not (csLoading in ComponentState) then
    inherited;
end;

procedure TCheckListEdit.CheckAll;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Checked[i] := true;
end;

procedure TCheckListEdit.CheckClick(Sender: TObject);
begin
  inherited;
  checkflag := false;
end;

procedure TCheckListEdit.DoClickCheck(Sender: TObject);
begin
  if Assigned(FOnClickCheck) then
  begin
    FOnClickCheck(Self);
    FCheckListBox.Refresh;            // Workaround Delphi TCustomChecklist-bug
  end;
end;

procedure TCheckListEdit.DoOnShowCheckList;
begin
  if Assigned(FOnShowCheckList) then
    FOnShowCheckList(Self);
end;

procedure TCheckListEdit.CheckClickCheck(Sender: TObject);
var
  i: integer;
begin
  checkflag := true;
  for i := 1 to FCheckListBox.Items.Count do
  begin
    if FCheckListBox.Checked[i - 1] then
    begin
      FIntList.Items[i - 1] := FIntList.Items[i - 1] or CL_CHECKED or $1;
    end
    else
      FIntList.Items[i - 1] := FIntList.Items[i - 1] and not CL_CHECKED;
  end;

  self.Text := CheckToString;

  DoClickCheck(Self);
end;


procedure TCheckListEdit.ItemChange(Sender: TObject);
begin
  inherited;
  
  while FItems.Count > FIntList.Count do
    FIntList.Add(0);
  while FItems.Count < FIntList.Count do
    FIntList.Delete(fIntList.Count - 1);

end;

procedure TCheckListEdit.SetItems(value: tStringList);
var
  i: Integer;
begin
  if Value <> nil then
    FItems.Assign(value);

  // Initialze enabled!
  if not (csDesigning in ComponentState) and Assigned(FCheckListBox) then
    for i := 0 to FItems.Count - 1 do
    begin
      FCheckListBox.ItemEnabled[i - 1] := True;
    end;
end;

function TCheckListEdit.CheckToString: string;
var
  i: integer;
  s, sli: string;

begin
  s := '';
  for i := 1 to FIntList.Count do
  begin
    if (FIntList.Items[i - 1] and CL_CHECKED = CL_CHECKED) then
    begin
      sli := FItems[i - 1];      
      if Assigned(FOnCheckListItemToText) then
        FOnCheckListItemToText(self, sli);

      if(length(s)>0) then
        s := s + FTextDelimiter;
      s := s + sli;
    end;
  end;
  Result := FTextStartChar + s + FTextEndChar;
end;

procedure TCheckListEdit.StringToCheck(s: string);
var
  su, sli: string;
  i: integer;
begin
  if not Assigned(FItems) then
    Exit;

  if (fTextEndChar <> fTextStartChar) then
  begin
    if pos(fTextEndChar, s) > 0 then delete(s, pos(fTextEndChar, s), 1);
    if pos(fTextStartChar, s) > 0 then delete(s, pos(fTextStartChar, s), 1);
  end
  else
  begin
    if pos(fTextStartChar, s) > 0 then delete(s, pos(fTextStartChar, s), 1);
    if (fTextEndChar <> '') and (Pos(fTextEndChar,s) > 0) then
    begin
      delete(s, length(s) - length(fTextEndChar) + 1, length(fTextEndChar));
    end;
  end;

  for i := 1 to FIntList.Count do
  begin
    FIntList.Items[i - 1] := FIntList.Items[i - 1] and not CL_CHECKED and not $1;
  end;

  while (length(s) > 0) do
  begin
    if (pos(fTextDelimiter, s) > 0) then
      su := copy(s, 1, pos(fTextDelimiter, s) - 1)
    else su := s;

    sli := su;
    if Assigned(FOnTextToCheckListItem) then
      FOnTextToCheckListItem(self, sli);

    i := FItems.Indexof(sli);

    if (i >= 0) then
      FIntList.Items[i] := FIntList.Items[i] or CL_CHECKED or $1;

    delete(s, 1, length(su));
    if length(s) >= length(FTextDelimiter) then
      delete(s, 1, length(FTextDelimiter));
  end;
end;

procedure TCheckListEdit.UnCheckAll;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Checked[i] := false;
end;

function TCheckListEdit.GetCheck(i: integer): boolean;
begin
  ItemChange(self);

  if (i >= fItems.Count) or (i >= fIntList.Count) or (i < 0) then
    raise Exception.Create('Index out of bounds');

  Result := FIntList.Items[i] and CL_CHECKED = CL_CHECKED;
end;

procedure TCheckListEdit.SetCheck(i: integer; value: boolean);
begin
  if FUpdateCount = 0 then
    ItemChange(self);

  if (i >= FItems.Count) or (i >= FIntList.Count) or (i < 0) then
    raise Exception.Create('Index out of bounds');

  if Value then
    FIntList.Items[i] := FIntList.Items[i] or CL_CHECKED or $1
  else
    FIntList.Items[i] := FIntList.Items[i] and not CL_CHECKED;

  if FUpdateCount = 0 then
    Text := CheckToString;

  if not FChkClosed then
    FChecklistbox.Checked[i] := Value;
end;

function TCheckListEdit.GetSelected(Index: Integer): Boolean;
begin
  Result := Assigned(FCheckListBox) and FCheckListBox.Selected[Index];
end;

function TCheckListEdit.GetState(i: integer): TCheckBoxState;
begin
  ItemChange(self);

  if (i >= fItems.Count) or (i >= fIntList.Count) or (i < 0) then
    raise Exception.Create('Index out of bounds');

  case fIntList.Items[i] and $3 of
    0: result := cbUnchecked;
    1: result := cbChecked;
    2: result := cbGrayed;
  else
    result := cbUnchecked;
  end;
end;

procedure TCheckListEdit.SetSelected(Index: Integer; const Value: Boolean);    
begin
  FCheckListBox.Selected[Index] := Value;
end;

procedure TCheckListEdit.SetState(i: integer; value: TCheckBoxState);
begin
  ItemChange(self);

  if (i >= fItems.Count) or (i >= fIntList.Count) or (i < 0) then
    raise Exception.Create('Index out of bounds');

  fIntList.Items[i] := fIntList.Items[i] and not $3;

  case value of
  cbChecked: fIntList.Items[i] := fIntList.Items[i] or $1;
  cbGrayed: fIntList.Items[i] := fIntList.Items[i] or $2;
  end;

  self.Text := CheckToString;
end;

procedure TCheckListEdit.FormDeactivate(Sender: TObject);
var
  pt: TPoint;
  r: TRect;
begin
  {check cursor here...}
  GetCursorPos(pt);
  pt := screentoclient(pt);
  r := clientrect;
  r.left := r.right - 16;
  FCloseClick := ptinrect(r, pt);
  HideCheckList;
end;

procedure TCheckListEdit.SetTextDelimiter(const Value: string);
begin
  fTextDelimiter := Value;
  if not (csLoading in ComponentState) then
    self.Text := CheckToString;
end;

procedure TCheckListEdit.SetTextEndChar(const Value: string);
begin
  FTextEndChar := Value;
  if not (csLoading in ComponentState) then
    self.Text := CheckToString;
end;

procedure TCheckListEdit.SetTextStartChar(const Value: string);
begin
  FTextStartChar := Value;
  if not (csLoading in ComponentState) then
    self.Text := CheckToString;
end;

procedure TCheckListEdit.Loaded;
begin
  self.Text := CheckToString;
  inherited;
end;

procedure TCheckListEdit.DownClick(Sender: TObject);
begin
  if FChkClosed then
  begin
    if not FCloseClick then
    begin
      if Assigned(FOnClickBtn) then
        FOnClickBtn(Self);
      ShowCheckListInt(true,#0);
    end;
  end;
  FCloseClick := False;
end;

procedure TCheckListEdit.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    if FUpdateCount = 0 then
      Text := CheckToString;
  end;
end;

procedure TCheckListEdit.MouseClick(Sender: TObject);
begin
  if not FChkClosed then
  begin
    HideCheckList;
  end;
end;

procedure TCheckListEdit.SetDropFont(const Value: TFont);
begin
  FDropFont.Assign(Value);
end;

function TCheckListEdit.GetText: string;
begin
  Result := inherited Text;
end;

procedure TCheckListEdit.SetText(const Value: string);
begin
  inherited Text := value;
  if not (csLoading in ComponentState) then
  begin
    StringToCheck(value);
  end;
end;

function TCheckListEdit.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TCheckListEdit.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TCheckListEdit.SetVersion(const Value: string);
begin

end;

procedure TCheckListEdit.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FDropDownPopupMenu) then
    FDropDownPopupMenu := nil;
  inherited;
end;

function TCheckListEdit.GetItemEnabled(ItemIndex: Integer): Boolean;
begin
  Result := True;
  if not Assigned(FCheckListBox) then Exit;

  if (ItemIndex > -1) and (ItemIndex < FCheckListBox.Items.Count) then
    Result := FCheckListBox.ItemEnabled[ItemIndex];
end;

procedure TCheckListEdit.SetItemEnabled(ItemIndex: Integer;
  const Value: Boolean);
begin
  if not Assigned(FCheckListBox) then
    Exit;

  if (ItemIndex > -1) and (ItemIndex < FCheckListBox.Items.Count) then
    FCheckListBox.ItemEnabled[ItemIndex] := Value;
  Invalidate;
end;

{ TInplaceCheckListBox }

procedure TInplaceCheckListBox.WMKeyDown(var Msg: TWMKeydown);
begin
  if (msg.charcode = VK_TAB) then
    Exit;

  if (msg.charcode = vk_escape) or (msg.charcode = vk_F4) or
    ((msg.CharCode = vk_up) and (getkeystate(vk_menu) and $8000 = $8000)) then
  begin
    postmessage((Parent as TForm).Handle, WM_CLOSE, 0, 0);
  end;

  if (msg.charcode = VK_RETURN) then
  begin
    if (ItemIndex > -1) then
      Checked[ItemIndex] := true;

    if Assigned(OnClickCheck) then
      OnClickCheck(self);

    PostMessage((Parent as TForm).Handle, WM_CLOSE, 0, 0);
  end;

  inherited;
end;

procedure TInplaceCheckListBox.DoExit;
begin
  inherited;
  if Visible then ParentEdit.HideCheckList;
end;


{TIntList helper object}

constructor TIntList.Create;
begin
  inherited Create;
end;

procedure TIntList.SetInteger(Index: Integer; Value: Integer);
begin
  inherited Items[Index] := Pointer(Value);
end;

function TIntList.GetInteger(Index: Integer): Integer;
begin
  Result := Integer(inherited Items[Index]);
end;

procedure TIntList.Add(Value: Integer);
begin
  inherited Add(Pointer(Value));
end;

procedure TIntList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

function TInplaceCheckListBox.GetDropDownWidth: Integer;
const
  CheckBoxWidth: integer = 15;
var
  i: integer;
  Width, Height: integer;
  MaxWidth: integer;
begin
  MaxWidth := self.Width;
  Height := 0;

  for i := 0 to Items.Count - 1 do
  begin
    Width := CheckBoxWidth + 2 +
      2 * GetSystemMetrics(SM_CXBORDER) + Canvas.TextWidth(Items[i]);

    Height := Height + Self.Canvas.TextHeight(Items[i]);
    if Width > MaxWidth then begin
      MaxWidth := Width;
    end;
  end;

  if Height + 2 * GetSystemMetrics(SM_CYBORDER) > self.Height then
    MaxWidth := MaxWidth + GetSystemMetrics(SM_CXVSCROLL);

  Result := MaxWidth;
end;

{ TDropForm }

procedure TDropForm.WMClose(var Msg: TMessage);
begin
  inherited;
  self.Free;
end;

end.
