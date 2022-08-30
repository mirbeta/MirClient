{***************************************************************************}
{ Unicode editor component                                                  }
{ for Delphi & C++Builder                                                   }
{ version 1.0.0.0                                                           }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2005 - 2006                                        }
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

unit planuni;

interface

{$I TMSDEFS.INC}

{$WARNINGS OFF}

uses
  Windows, Messages, Classes, Controls, StdCtrls, Buttons, Graphics;

type
  TCustomPlanUNIEdit = class(TCustomEdit)
  private
    function GetSelText: WideString; reintroduce;
    procedure SetSelText(const Value: WideString);
    function GetText: WideString;
    procedure SetText(const Value: WideString);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent);override;
    property SelText: WideString read GetSelText write SetSelText;
    property Text: WideString read GetText write SetText;
  published
  end;

  TPlanUniEdit = class(TCustomPlanUNIEdit)
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TPlanUniCustomMemo = class(TCustomPlanUNIEdit)
  private
    FScrollBars: TScrollStyle;
    FWordWrap: Boolean;
    FWantReturns: Boolean;
    FWantTabs: Boolean;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetWordWrap(const Value: Boolean);
  protected
    {$IFDEF DELPHI6_LVL}
    procedure SetAutoSize(Value: Boolean); override;
    {$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssNone;
    property WantReturns: Boolean read FWantReturns write FWantReturns default True;
    property WantTabs: Boolean read FWantTabs write FWantTabs default False;   
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
  public
  end;

  TPlanUniMemo = class(TPlanUniCustomMemo)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property WordWrap;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TAnsiStrings = TStrings;

  TWideStrings = class(TPersistent)
  private
    FAnsiStrings: TAnsiStrings;
    FUpdateCount: integer;
    FLanguage: LCID;
    FSaved,
    FSaveUnicode: boolean;
    function GetCommaText: WideString;
    function GetName(Index: integer): WideString;
    function GetValue(const Name: WideString): WideString;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: WideString);
    procedure SetValue(const Name, Value: WideString);
    procedure WriteData(Writer: TWriter);
    procedure SetAnsiStrings(const Value: TAnsiStrings);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: WideString; Data: integer);
    function Get(Index: integer): WideString; virtual; abstract;
    function GetCapacity: integer; virtual;
    function GetCount: integer; virtual; abstract;
    function GetObject(Index: integer): TObject; virtual;
    function GetTextStr: WideString; virtual;
    procedure Put(Index: integer; const S: WideString); virtual;
    procedure PutObject(Index: integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: integer); virtual;
    procedure SetTextStr(const Value: WideString); virtual;
    procedure SetUpdateState(Updating: boolean); virtual;
    procedure SetLanguage(Value: LCID); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: WideString): integer; virtual;
    function AddObject(const S: WideString; AObject: TObject): integer; virtual;
    procedure Append(const S: WideString);
    procedure AddStrings(Strings: TAnsiStrings); overload; virtual;
    procedure AddStrings(Strings: TWideStrings); overload; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TWideStrings): boolean;
    procedure Exchange(Index1, Index2: integer); virtual;
    function GetText: PWideChar; virtual;
    function IndexOf(const S: WideString): integer; virtual;
    function IndexOfName(const Name: WideString): integer;
    function IndexOfObject(AObject: TObject): integer;
    procedure Insert(Index: integer; const S: WideString); virtual; abstract;
    procedure InsertObject(Index: integer; const S: WideString; AObject: TObject);
    procedure Move(CurIndex, NewIndex: integer); virtual;
    procedure SetText(Text: PWideChar); virtual;

    property Capacity: integer read GetCapacity write SetCapacity;
    property CommaText: WideString read GetCommaText write SetCommaText;
    property Count: integer read GetCount;
    property Language: LCID read FLanguage write SetLanguage;
    property Names[Index: integer]: WideString read GetName;
    property Objects[Index: integer]: TObject read GetObject write PutObject;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property Saved: boolean read FSaved;
    property SaveUnicode: boolean read FSaveUnicode write FSaveUnicode;
    property Strings[Index: integer]: WideString read Get write Put; default;
    property Text: WideString read GetTextStr write SetTextStr;

  published
    property AnsiStrings: TAnsiStrings read FAnsiStrings write SetAnsiStrings stored False;
  end;

implementation

uses
  Forms, SysUtils, Consts, Imm;

const
  UNICODE_CLASS_EXT = '.UnicodeClass';
  ANSI_UNICODE_HOLDER = $FF;

  WideNull = widechar(#0);
  Tabulator = widechar(#9);
  Space = widechar(#32);
  LF = widechar($A);
  LineFeed = widechar($A);
  VerticalTab = widechar($B);
  FormFeed = widechar($C);
  CR = widechar($D);
  CarriageReturn = widechar($D);
  CRLF: WideString = #$D#$A;
  LineSeparator = widechar($2028);
  ParagraphSeparator = widechar($2029);

var
  AtomText: array[0..127] of AnsiChar;

type
  TComboBoxStrings = class(TWideStrings)
  protected
    function Get(Index: integer): WideString; override;
    function GetCount: integer; override;
    function GetObject(Index: integer): TObject; override;
    //procedure Put(Index: integer; const S: WideString); override;
    procedure PutObject(Index: integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: boolean); override;
  public
    ComboBox: TCustomComboBox; {TCustomComboBox}
    function Add(const S: WideString): integer; override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    function IndexOf(const S: WideString): integer; override;
    procedure Insert(Index: integer; const S: WideString); override;
  end;


  TAnsiStringsForWideStrings = class(TAnsiStrings)
  private
    FWideStrings: TWideStrings;
  protected
    function Get(Index: integer): AnsiString; override;
    function GetCount: integer; override;
  public
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure Insert(Index: integer; const S: AnsiString); override;
  end;

  TWinControlTrap = class
  private
    ObjectInstance: Pointer;
    DefObjectInstance: Pointer;
    FControl: TWinControl;
    Handle: THandle;
    PrevWin32Proc: Pointer;
    PrevDefWin32Proc: Pointer;
    PrevWindowProc: TWndMethod;
    LastWin32Msg: UINT;
    procedure Win32Proc(var Message: TMessage);
    procedure DefWin32Proc(var Message: TMessage);
    procedure WindowProc(var Message: TMessage);
    procedure HandleWMDestroy(var Message: TMessage);
  end;

  TAccessWinControl = class(TWinControl);
  
  TWideCaptionHolder = class(TComponent)
  private
    WideCaption: WideString;
  end;
  
  TCompareFunc = function(W1, W2: WideString; Locale: LCID): integer;

var
  WinControlTrap_Atom: TAtom = 0;
  WideCompareText: TCompareFunc;

{ TAnsiStringsForWideStrings }

procedure TAnsiStringsForWideStrings.Clear;
begin
  FWideStrings.Clear;
end;

procedure TAnsiStringsForWideStrings.Delete(Index: integer);
begin
  FWideStrings.Delete(Index);
end;

function TAnsiStringsForWideStrings.Get(Index: integer): AnsiString;
begin
  Result := FWideStrings.Get(Index);
end;

function TAnsiStringsForWideStrings.GetCount: integer;
begin
  Result := FWideStrings.Count;
end;

procedure TAnsiStringsForWideStrings.Insert(Index: integer; const S: AnsiString);
begin
  FWideStrings.Insert(Index, S);
end;


{ General Fuctions And Procedures}

procedure WideSetWindowText(Control: TWinControl; const Text: WideString); forward;
function WideGetWindowText(Control: TWinControl): WideString; forward;
procedure DestroyUnicodeHandle(Control: TWinControl); forward;

function Is_IntResource(ResStr: LPCWSTR): boolean;
begin
  Result := HiWord(cardinal(ResStr)) = 0;
end;

function IsTextMessage(Msg: UINT): boolean;
begin
  // WM_CHAR is omitted because of the special handling it receives
  Result := (Msg = WM_SETTEXT) or (Msg = WM_GETTEXT) or (Msg = WM_GETTEXTLENGTH);
end;

// returns a pointer to first occurrence of a specified character in a string
function StrScanW(Str: PWideChar; Chr: widechar): PWideChar;
asm
         PUSH EDI
         PUSH EAX
         MOV EDI, Str
         MOV ECX, 0FFFFFFFFH
         XOR AX, AX
         REPNE SCASW
         NOT ECX
         POP EDI
         MOV AX, Chr
         REPNE SCASW
         MOV EAX, 0
         JNE @@1
         MOV EAX, EDI
         SUB EAX, 2
@@1:     POP EDI
end;

// returns a pointer to the end of a null terminated string
function StrEndW(Str: PWideChar): PWideChar;
asm
         MOV EDX, EDI
         MOV EDI, EAX
         MOV ECX, 0FFFFFFFFH
         XOR AX, AX
         REPNE SCASW
         LEA EAX, [EDI - 2]
         MOV EDI, EDX
end;

// works like QuotedStr from SysUtils.pas but can insert any quotation character
function WideQuotedStr(const S: WideString; Quote: widechar): WideString;
var
  P, Src, Dest: PWideChar;
  AddCount: integer;
begin
  AddCount := 0;
  P := StrScanW(PWideChar(S), Quote);
  while Assigned(P) do
  begin
    Inc(P);
    Inc(AddCount);
    P := StrScanW(P, Quote);
  end;

  if AddCount = 0 then Result := Quote + S + Quote
  else
  begin
    SetLength(Result, Length(S) + AddCount + 2);
    Dest := PWideChar(Result);
    Dest^ := Quote;
    Inc(Dest);
    Src := PWideChar(S);
    P := StrScanW(Src, Quote);
    repeat
      Inc(P);
      Move(Src^, Dest^, (P - Src) * SizeOf(widechar));
      Inc(Dest, P - Src);
      Dest^ := Quote;
      Inc(Dest);
      Src := P;
      P := StrScanW(Src, Quote);
    until P = nil;
    P := StrEndW(Src);
    Move(Src^, Dest^, (P - Src) * SizeOf(widechar));
    Inc(Dest, P - Src);
    Dest^ := Quote;
  end;  
end;

// returns number of characters in a string excluding the null terminator
function StrLenW(Str: PWideChar): cardinal;
asm
         MOV EDX, EDI
         MOV EDI, EAX
         MOV ECX, 0FFFFFFFFH
         XOR AX, AX
         REPNE SCASW
         MOV EAX, 0FFFFFFFEH
         SUB EAX, ECX
         MOV EDI, EDX
end;

// Copies the specified number of characters to the destination string and returns Dest
// also as result. Dest must have enough room to store at least Count characters.
function StrMoveW(Dest, Source: PWideChar; Count: cardinal): PWideChar;
asm
         PUSH ESI
         PUSH EDI
         MOV ESI, EDX
         MOV EDI, EAX
         MOV EDX, ECX
         CMP EDI, ESI
         JG @@1
         JE @@2
         SHR ECX, 1
         REP MOVSD
         MOV ECX, EDX
         AND ECX, 1
         REP MOVSW
         JMP @@2

@@1:     LEA ESI, [ESI + 2 * ECX - 2]
         LEA EDI, [EDI + 2 * ECX - 2]
         STD
         AND ECX, 1
         REP MOVSW
         SUB EDI, 2
         SUB ESI, 2
         MOV ECX, EDX
         SHR ECX, 1
         REP MOVSD
         CLD
@@2:     POP EDI
         POP ESI
end;

// Allocates a buffer for a null-terminated wide string and returns a pointer
// to the first character of the string.
function StrAllocW(Size: cardinal): PWideChar;
begin
  Size := SizeOf(widechar) * Size + SizeOf(cardinal);
  GetMem(Result, Size);
  FillChar(Result^, Size, 0);
  cardinal(Pointer(Result)^) := Size;
  Inc(Result, SizeOf(cardinal) div SizeOf(widechar));
end;

// Duplicates the given string (if not nil) and returns the address of the new string.
function StrNewW(Str: PWideChar): PWideChar;
var
  Size: cardinal;
begin
  if Str = nil then Result := nil
  else
  begin
    Size := StrLenW(Str) + 1;
    Result := StrMoveW(StrAllocW(Size), Str, Size);
  end;
end;

// extracts a string enclosed in quote characters given by Quote
function WideExtractQuotedStr(var Src: PWideChar; Quote: widechar): WideString;
var
  P, Dest: PWideChar;
  DropCount: integer;    
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then Exit;

  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := StrScanW(Src, Quote);

  while Assigned(Src) do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then Break;
    Inc(Src);
    Inc(DropCount);
    Src := StrScanW(Src, Quote);
  end;

  if Src = nil then Src := StrEndW(P);
  if (Src - P) <= 1 then Exit;
  
  if DropCount = 1 then SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PWideChar(Result);
    Src := StrScanW(P, Quote);
    while Assigned(Src) do
    begin
      Inc(Src);
      if Src^ <> Quote then Break;
      Move(P^, Dest^, (Src - P) * SizeOf(widechar));
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := StrScanW(Src, Quote);
    end;
    if Src = nil then Src := StrEndW(P);
    Move(P^, Dest^, (Src - P - 1) * SizeOf(widechar));
  end;
end;

// determines the code page for a given locale
function CodePageFromLocale(Language: LCID): integer;
var
  Buf: array[0..6] of AnsiChar;
begin
  GetLocaleInfo(Language, LOCALE_IDefaultAnsiCodePage, Buf, 6);
  Result := StrToIntDef(Buf, GetACP);
end;

// special comparation function for Win9x since there's no system defined comparation function,
// returns -1 if W1 < W2, 0 if W1 = W2 or 1 if W1 > W2
function CompareTextWin95(W1, W2: WideString; Locale: LCID): integer;
var
  S1, S2: AnsiString;
  CP: integer;
  L1, L2: integer;
begin
  L1 := Length(W1);
  L2 := Length(W2);
  SetLength(S1, L1);
  SetLength(S2, L2);
  CP := CodePageFromLocale(Locale);
  WideCharToMultiByte(CP, 0, PWideChar(W1), L1, PAnsiChar(S1), L1, nil, nil);
  WideCharToMultiByte(CP, 0, PWideChar(W2), L2, PAnsiChar(S2), L2, nil, nil);
  Result := CompareStringA(Locale, NORM_IGNORECASE, PAnsiChar(S1),
    Length(S1), PAnsiChar(S2), Length(S2)) - 2;
end;

// Wrapper function for WinNT since there's no system defined comparation function in Win9x and
// we need a central comparation function for TWideStringList.
// Returns -1 if W1 < W2, 0 if W1 = W2 or 1 if W1 > W2
function CompareTextWinNT(W1, W2: WideString; Locale: LCID): integer;
begin
  Result := CompareStringW(Locale, NORM_IGNORECASE, PWideChar(W1),
    Length(W1), PWideChar(W2), Length(W2)) - 2;
end;

procedure MakeWMCharMsgSafeForAnsi(var Message: TMessage);
begin
  with TWMChar(Message) do 
  begin
    Assert(Msg = WM_CHAR);
    Assert(Unused = 0);
    if (CharCode > word(High(AnsiChar))) then 
    begin
      Unused := CharCode;
      CharCode := ANSI_UNICODE_HOLDER;
    end;
  end;
end;

procedure RestoreWMCharMsg(var Message: TMessage);
begin
  with TWMChar(Message) do 
  begin
    Assert(Message.Msg = WM_CHAR);
    if (Unused > 0) and (CharCode = ANSI_UNICODE_HOLDER) then
      CharCode := Unused;
    Unused := 0;
  end;
end;

function HandleIMEComposition(hWnd: THandle; Message: TMessage): boolean;
var
  IMC: HIMC;
  Buff: widestring;
  i: integer;
begin
  Result := False;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Message.Msg = WM_IME_COMPOSITION)
    and ((Message.lParam and GCS_RESULTSTR) <> 0) then
  begin
    IMC := ImmGetContext(hWnd);
    if IMC <> 0 then 
    begin
      try
        Result := True;
        // Get the result string
        SetLength(Buff, ImmGetCompositionStringW(IMC, GCS_RESULTSTR,
          nil, 0) div SizeOf(widechar));
        ImmGetCompositionStringW(IMC, GCS_RESULTSTR, PWideChar(Buff),
          Length(Buff) * SizeOf(widechar));
      finally
        ImmReleaseContext(hWnd, IMC);
      end;
      // send WM_CHAR messages for each char in string
      for i := 1 to Length(Buff) do 
      begin
        SendMessageW(hWnd, WM_CHAR, integer(Buff[i]), 0);
      end;
    end;
  end;
end;

{ TComboBoxStrings }

function TComboBoxStrings.GetCount: integer;
begin
  Result := ComboBox.Items.Count;
end;

function TComboBoxStrings.Get(Index: integer): WideString;
var
  Len: integer;
begin
  if (not IsWindowUnicode(ComboBox.Handle)) then
    Result := ComboBox.Items[Index]
  else 
  begin
    Len := SendMessageW(ComboBox.Handle, CB_GETLBTEXTLEN, Index, 0);
    if Len = CB_ERR then
      Result := ''
    else 
    begin
      SetLength(Result, Len + 1);
      Len := SendMessageW(ComboBox.Handle, CB_GETLBTEXT, Index, LParam(PWideChar(Result)));
      if Len = CB_ERR then
        Result := ''
      else
        Result := PWideChar(Result);
    end;
  end;
end;

function TComboBoxStrings.GetObject(Index: integer): TObject;
begin
  Result := ComboBox.Items.Objects[Index];
end;

procedure TComboBoxStrings.PutObject(Index: integer; AObject: TObject);
begin
  ComboBox.Items.Objects[Index] := AObject;
end;

function TComboBoxStrings.Add(const S: WideString): integer;
begin
  if (not IsWindowUnicode(ComboBox.Handle)) then
    Result := ComboBox.Items.Add(S)
  else 
  begin
    Result := SendMessageW(ComboBox.Handle, CB_ADDSTRING, 0, LParam(PWideChar(S)));
    if Result < 0 then
      raise EOutOfResources.Create(SInsertLineError);
  end;
end;

procedure TComboBoxStrings.Insert(Index: integer; const S: WideString);
begin
  if (not IsWindowUnicode(ComboBox.Handle)) then
    ComboBox.Items.Insert(Index, S)
  else 
  begin
    if SendMessageW(ComboBox.Handle, CB_INSERTSTRING, Index, LParam(PWideChar(S))) < 0 then
      raise EOutOfResources.Create(SInsertLineError);
  end;
end;

procedure TComboBoxStrings.Delete(Index: integer);
begin
  ComboBox.Items.Delete(Index);
end;

procedure TComboBoxStrings.Clear;
var
  S: widestring;
begin
  S := WideGetWindowText(ComboBox);
  SendMessage(ComboBox.Handle, CB_RESETCONTENT, 0, 0);
  WideSetWindowText(ComboBox, S);
  ComboBox.Update;
end;

procedure TComboBoxStrings.SetUpdateState(Updating: boolean);
begin
  if Updating then
    ComboBox.Items.BeginUpdate
  else
    ComboBox.Items.EndUpdate
end;

function TComboBoxStrings.IndexOf(const S: WideString): integer;
begin
  if (not IsWindowUnicode(ComboBox.Handle)) then
    Result := ComboBox.Items.IndexOf(S)
  else
    Result := SendMessageW(ComboBox.Handle, CB_FINDSTRINGEXACT, - 1, LParam(PWideChar(S)));
end;

{ TWinControlTrap }
procedure TWinControlTrap.HandleWMDestroy(var Message: TMessage);
var
  ThisPrevWin32Proc: Pointer;
  ThisHandle: THandle;
begin
  with Message do
  begin
    Assert(Msg = WM_DESTROY);
    // store local copies of values, since this object is about to be freed
    ThisPrevWin32Proc := PrevWin32Proc;
    ThisHandle := Handle;

    // handle destruction
    DestroyUnicodeHandle(FControl);

    // pass on the WM_DESTROY message
    Result := CallWindowProc(ThisPrevWin32Proc, ThisHandle, Msg, wParam, lParam);
  end;
end;

procedure TWinControlTrap.Win32Proc(var Message: TMessage);
begin
  with Message do
  begin
    if Msg = WM_DESTROY then
    begin
      HandleWMDestroy(Message);
      exit; { Do not access any data in object. Object is freed. }
    end;
    if not HandleIMEComposition(Handle, Message) then
    begin
      LastWin32Msg := Msg;
      Result := CallWindowProcW(PrevWin32Proc, Handle, Msg, wParam, lParam);
    end;
  end;
end;

procedure TWinControlTrap.DefWin32Proc(var Message: TMessage);
begin
  with Message do
  begin
    if (Msg = WM_CHAR) then
    begin
      RestoreWMCharMsg(Message)
    end;
    Result := CallWindowProcW(PrevDefWin32Proc, Handle, Msg, wParam, lParam);
  end;
end;

procedure TWinControlTrap.WindowProc(var Message: TMessage);
var
  CameFromWindows: boolean;
begin
  CameFromWindows := LastWin32Msg <> WM_NULL;
  LastWin32Msg := WM_NULL;
  with Message do
  begin
    if (not CameFromWindows) and (IsTextMessage(Msg)) then
      Result := SendMessageA(Handle, Msg, wParam, lParam)
    else
    begin
      if (Msg = WM_CHAR) then
      begin
        MakeWMCharMsgSafeForAnsi(Message);
      end;
      PrevWindowProc(Message)
    end;
  end;
end;

procedure SubClassUnicodeControl(Control: TWinControl);
var
  WinControlTrap: TWinControlTrap;
begin
  if IsWindowUnicode(Control.Handle) then
  begin
    // create trap object, save reference
    WinControlTrap := TWinControlTrap.Create;
    SetProp(Control.Handle, MakeIntAtom(WinControlTrap_Atom), cardinal(WinControlTrap));

    with WinControlTrap do 
    begin
      // initialize trap object
      FControl := Control;
      Handle := Control.Handle;
      PrevWin32Proc := Pointer(GetWindowLong(Control.Handle, GWL_WNDPROC));
      PrevDefWin32Proc := TAccessWinControl(Control).DefWndProc;
      PrevWindowProc := Control.WindowProc;

      // subclass Window Procedures
      ObjectInstance := MakeObjectInstance(Win32Proc);
      SetWindowLongW(Control.Handle, GWL_WNDPROC, integer(ObjectInstance));
      DefObjectInstance := MakeObjectInstance(DefWin32Proc);
      TAccessWinControl(Control).DefWndProc := DefObjectInstance;
      Control.WindowProc := WindowProc;
    end;
  end;
end;

procedure UnSubClassUnicodeControl(Control: TWinControl);
var
  WinControlTrap: TWinControlTrap;
begin
  if IsWindowUnicode(Control.Handle) then 
  begin
    // get referenct to trap object
    WinControlTrap := TWinControlTrap(GetProp(Control.Handle,
      MakeIntAtom(WinControlTrap_Atom)));
    RemoveProp(Control.Handle, MakeIntAtom(WinControlTrap_Atom));

    with WinControlTrap do 
    begin
      // restore window procs
      Control.WindowProc := PrevWindowProc;
      TAccessWinControl(Control).DefWndProc := PrevDefWin32Proc;
      SetWindowLongW(Control.Handle, GWL_WNDPROC, integer(PrevWin32Proc));
      FreeObjectInstance(ObjectInstance);
      FreeObjectInstance(DefObjectInstance);

      // free trap object
      Free;
    end;
  end;
end;

//----------------------------------------------- CREATE/DESTROY UNICODE HANDLE

function FindWideCaptionHolder(Control: TWinControl;
  Default: WideString = ''): TWideCaptionHolder;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Control.ComponentCount - 1 do 
  begin
    if (Control.Components[i] is TWideCaptionHolder) then 
    begin
      Result := TWideCaptionHolder(Control.Components[i]);
      Exit; // found it!
    end;
  end;
  if Result = nil then
  begin
    Result := TWideCaptionHolder.Create(Control);
    Result.WideCaption := Default;
  end;
end;

procedure CreateUnicodeHandle(Control: TWinControl; const Params: TCreateParams;
  const SubClass: WideString);
var
  WideSubClass: TWndClassW;
  WideWinClassName: widestring;
  WideClass: TWndClassW;
  TempClass: TWndClassW;
  Handle: THandle;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
  begin
    with Params do
      TAccessWinControl(Control).WindowHandle := CreateWindowEx(ExStyle, WinClassName,
        Caption, Style, X, Y, Width, Height, WndParent, 0, WindowClass.hInstance, Param);
  end
  else
  begin
    // SubClass the unicode version of this control by getting the correct DefWndProc
    if SubClass <> '' then
    begin
      GetClassInfoW(hInstance, PWideChar(SubClass), WideSubClass);
      TAccessWinControl(Control).DefWndProc := WideSubClass.lpfnWndProc;
    end
    else
      TAccessWinControl(Control).DefWndProc := @DefWindowProcW;

    with Params do 
    begin
      WideWinClassName := WinClassName + UNICODE_CLASS_EXT;
      if not GetClassInfoW(Params.WindowClass.hInstance,
        PWideChar(WideWinClassName), TempClass) then 
      begin
        // Prepare a TWndClassW record
        WideClass := TWndClassW(WindowClass);
        if not Is_IntResource(PWideChar(WindowClass.lpszMenuName)) then 
        begin
          WideClass.lpszMenuName := PWideChar(WideString(WindowClass.lpszMenuName));
        end;
        WideClass.lpszClassName := PWideChar(WideWinClassName);

        // Register the UNICODE class
        if RegisterClassW(WideClass) = 0 then   RaiseLastWin32Error;
      end;

      // Create UNICODE window
      Handle := CreateWindowExW(ExStyle, PWideChar(WideWinClassName), nil,
        Style, X, Y, Width, Height, WndParent, 0, WindowClass.hInstance, Param);

      // SetWindowLongW needs to be called because InitWndProc converts control to ANSI
      //  CallingSetWindowLongA(.., GWL_WNDPROC) makes Windows think it is an ANSI window
      //    But CallingSetWindowLongW(.., GWL_WNDPROC) make Windows think it is a UNICODE window.
      SetWindowLongW(Handle, GWL_WNDPROC, GetWindowLong(Handle, GWL_WNDPROC));

      // set handle for control
      TAccessWinControl(Control).WindowHandle := Handle;

      // sub-class
      SubClassUnicodeControl(Control);

      // For some reason, caption gets garbled after calling SetWindowLongW(.., GWL_WNDPROC).
      WideSetWindowText(Control, FindWideCaptionHolder(Control, Caption).WideCaption);
    end;
  end;
end;

procedure DestroyUnicodeHandle(Control: TWinControl);
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then 
  begin
    // remember caption for future window creation
    if not (csDestroying in Control.ComponentState) then
      FindWideCaptionHolder(Control).WideCaption := WideGetWindowText(Control);
    // un sub-class
    UnSubClassUnicodeControl(Control);
  end;
end;

//----------------------------------------------- GET/SET WINDOW TEXT

function WideGetWindowText(Control: TWinControl): WideString;
begin
  if (not Control.HandleAllocated) or (not IsWindowUnicode(Control.Handle)) then 
  begin
    // NO HANDLE -OR- NOT UNICODE
    Result := TAccessWinControl(Control).Text;
    if Win32Platform = VER_PLATFORM_WIN32_NT then
      Result := FindWideCaptionHolder(Control, Result).WideCaption
  end 
  else 
  begin
    // UNICODE & HANDLE
    SetLength(Result, GetWindowTextLengthW(Control.Handle) + 1);
    GetWindowTextW(Control.Handle, PWideChar(Result), Length(Result));
    SetLength(Result, Length(Result) - 1);
  end;
end;

procedure WideSetWindowText(Control: TWinControl; const Text: WideString);
begin
  if (not Control.HandleAllocated) or (not IsWindowUnicode(Control.Handle)) then 
  begin
    // NO HANDLE -OR- NOT UNICODE
    TAccessWinControl(Control).Text := Text;
    if Win32Platform = VER_PLATFORM_WIN32_NT then
      FindWideCaptionHolder(Control).WideCaption := Text;
  end 
  else if WideGetWindowText(Control) <> Text then 
  begin
    // UNICODE & HANDLE
    SetWindowTextW(Control.Handle, PWideChar(Text));
    Control.Perform(CM_TEXTCHANGED, 0, 0);
  end;
end;

//----------------- TWideStrings ---------------------------------------------------------------------------------------

constructor TWideStrings.Create;

begin
  inherited;
  FAnsiStrings := TAnsiStringsForWideStrings.Create;
  TAnsiStringsForWideStrings(FAnsiStrings).FWideStrings := Self;
  // there should seldom be the need to use a language other than the one of the system
  FLanguage := GetUserDefaultLCID;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TWideStrings.Destroy;

begin
  FreeAndNil(FAnsiStrings);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.SetAnsiStrings(const Value: TAnsiStrings);
begin
  FAnsiStrings.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.SetLanguage(Value: LCID);

begin
  FLanguage := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStrings.Add(const S: WideString): integer;

begin
  Result := GetCount;
  Insert(Result, S);
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStrings.AddObject(const S: WideString; AObject: TObject): integer;

begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.Append(const S: WideString);

begin
  Add(S);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.AddStrings(Strings: TAnsiStrings);

var
  I: integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.AddStrings(Strings: TWideStrings);
var
  I: integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;


// usual assignment routine, but able to assign wide and small strings
procedure TWideStrings.Assign(Source: TPersistent);
var
  I: integer;
begin
  if Source is TWideStrings then
  begin
    BeginUpdate;
    try
      Clear;
      AddStrings(TWideStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else if Source is TAnsiStrings then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TAnsiStrings(Source).Count - 1 do
        AddObject(TAnsiStrings(Source)[I], TAnsiStrings(Source).Objects[I]);
    finally
      EndUpdate;
    end;
  end
  else 
    inherited Assign(Source);
end;

// need to do also assignment to old style TStrings, but this class doesn't know TWideStrings, so
// we need to do it from here
procedure TWideStrings.AssignTo(Dest: TPersistent);
var
  I: integer;
begin
  if Dest is TAnsiStrings then
    with Dest as TAnsiStrings do
    begin
      BeginUpdate;
      try
        Clear;
        for I := 0 to Self.Count - 1 do
          AddObject(Self[I], Self.Objects[I]);
      finally
        EndUpdate;
      end;
    end
  else if Dest is TWideStrings then
    with Dest as TWideStrings do
    begin
      BeginUpdate;
      try
        Clear;
        AddStrings(Self);
      finally
        EndUpdate;
      end;
    end
  else 
    inherited;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.BeginUpdate;

begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TWideStrings.DefineProperties(Filer: TFiler);

  function DoWrite: boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TWideStrings then Result :=
          not Equals(TWideStrings(Filer.Ancestor))
    end
    else
      Result := Count > 0;
  end;

begin
  Filer.DefineProperty('WideStrings', ReadData, WriteData, DoWrite);
end;

//------------------------------------------------------------------------------

procedure TWideStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

//------------------------------------------------------------------------------

function TWideStrings.Equals(Strings: TWideStrings): boolean;
var
  I, Count: integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then
    Exit;
  for I := 0 to Count - 1 do
    if Get(I) <> Strings.Get(I) then
      Exit;
  Result := True;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.Error(const Msg: WideString; Data: integer);

  function ReturnAddr: Pointer;
  asm
          MOV EAX, [EBP + 4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.Exchange(Index1, Index2: integer);
var
  TempObject: TObject;
  TempString: widestring;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------
// descendants may optionally override/replace this default implementation
function TWideStrings.GetCapacity: integer;
begin
  Result := Count;
end;

//------------------------------------------------------------------------------

function TWideStrings.GetCommaText: WideString;
var
  S: widestring;
  P: PWideChar;
  I, Count: integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then Result := '""'
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PWideChar(S);
      while not (P^ in [WideNull..Space, widechar('"'), widechar(',')]) do Inc(P);
      if (P^ <> WideNull) then S := WideQuotedStr(S, '"');
      Result := Result + S + ',';
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

//------------------------------------------------------------------------------

function TWideStrings.GetName(Index: integer): WideString;
var
  P: integer;
begin
  Result := Get(Index);
  P := Pos('=', Result);
  if P > 0 then SetLength(Result, P - 1)
  else
    Result := '';
end;

//------------------------------------------------------------------------------

function TWideStrings.GetObject(Index: integer): TObject;
begin
  Result := nil;
end;

//------------------------------------------------------------------------------

function TWideStrings.GetText: PWideChar;
begin
  Result := StrNewW(PWideChar(GetTextStr));
end;

//------------------------------------------------------------------------------

function TWideStrings.GetTextStr: WideString;
var
  I, L, Size, Count: integer;
  P: PWideChar;
  S: widestring;
begin
  Count := GetCount;
  Size := 0;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + 2);
  SetLength(Result, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, 2 * L);
      Inc(P, L);
    end;
    P^ := CarriageReturn;
    Inc(P);
    P^ := LineFeed;
    Inc(P);
  end;
end;

//------------------------------------------------------------------------------

function TWideStrings.GetValue(const Name: WideString): WideString;
var
  I: integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then Result := Copy(Get(I), Length(Name) + 2, MaxInt)
  else
    Result := '';
end;

//------------------------------------------------------------------------------

function TWideStrings.IndexOf(const S: WideString): integer;
begin
  for Result := 0 to GetCount - 1 do
    if WideCompareText(Get(Result), S, FLanguage) = 0 then Exit;
  Result := -1;
end;

//------------------------------------------------------------------------------

function TWideStrings.IndexOfName(const Name: WideString): integer;
var
  P: integer;
  S: widestring;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos('=', S);
    if (P > 0) and (WideCompareText(Copy(S, 1, P - 1), Name, FLanguage) = 0) then
      Exit;
  end;
  Result := -1;
end;

//------------------------------------------------------------------------------

function TWideStrings.IndexOfObject(AObject: TObject): integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.InsertObject(Index: integer; const S: WideString;
  AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TWideStrings.Move(CurIndex, NewIndex: integer);
var
  TempObject: TObject;
  TempString: widestring;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.Put(Index: integer; const S: WideString);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

//------------------------------------------------------------------------------

procedure TWideStrings.PutObject(Index: integer; AObject: TObject);
begin
end;

//------------------------------------------------------------------------------

procedure TWideStrings.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do
      if Reader.NextValue in [vaString, vaLString] then
        Add(Reader.ReadString)
    else
      Add(Reader.ReadWideString);
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TWideStrings.SetCapacity(NewCapacity: integer);
begin
  // do nothing - descendants may optionally implement this method
end;

//------------------------------------------------------------------------------

procedure TWideStrings.SetCommaText(const Value: WideString);
var
  P, P1: PWideChar;
  S: widestring;
begin
  BeginUpdate;
  try
    Clear;
    P := PWideChar(Value);
    while P^ in [widechar(#1)..Space] do Inc(P);
    while P^ <> WideNull do
    begin
      if P^ = '"' then  S := WideExtractQuotedStr(P, '"')
      else
      begin
        P1 := P;
        while (P^ > Space) and (P^ <> ',') do Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);

      while P^ in [widechar(#1)..Space] do Inc(P);
      if P^ = ',' then
        repeat
          Inc(P);
        until not (P^ in [widechar(#1)..Space]);
    end;
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.SetText(Text: PWideChar);
begin
  SetTextStr(Text);
end;

//------------------------------------------------------------------------------

procedure TWideStrings.SetTextStr(const Value: WideString);
var
  Head, Tail: PWideChar;
  S: widestring;
begin
  BeginUpdate;
  try
    Clear;
    Head := PWideChar(Value);
    while Head^ <> WideNull do
    begin
      Tail := Head;
      while not (Tail^ in [WideNull, LineFeed, CarriageReturn, VerticalTab, FormFeed])
        and
        (Tail^ <> LineSeparator) and
        (Tail^ <> ParagraphSeparator) do Inc(Tail);
      SetString(S, Head, Tail - Head);
      Add(S);
      Head := Tail;
      if Head^ <> WideNull then
      begin
        Inc(Head);
        if (Tail^ = CarriageReturn) and
          (Head^ = LineFeed) then Inc(Head);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.SetUpdateState(Updating: boolean);
begin
end;

//------------------------------------------------------------------------------

procedure TWideStrings.SetValue(const Name, Value: WideString);
var
  I: integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + '=' + Value);
  end
  else if I >= 0 then Delete(I);
end;

//------------------------------------------------------------------------------

procedure TWideStrings.WriteData(Writer: TWriter);
var
  I: integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do
    Writer.WriteWideString(Get(I));
  Writer.WriteListEnd;
end;

{ TUNICustomEdit }

constructor TCustomPlanUNIEdit.Create(AOwner: TComponent);
begin
  inherited;
  Font.Charset := OEM_CHARSET;// THIS sets the Charset so the Edit accepts all UNICODE characters
end;

procedure TCustomPlanUNIEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TCustomPlanUNIEdit.CreateWindowHandle(const Params: TCreateParams);
begin
  if SysLocale.FarEast and (Win32Platform <> VER_PLATFORM_WIN32_NT) and
    ((Params.Style and ES_READONLY) <> 0) then
    inherited
  else
    CreateUnicodeHandle(Self, Params, 'EDIT');
end;

function TCustomPlanUNIEdit.GetSelText: WideString;
begin
  if (not IsWindowUnicode(Handle)) then
    Result := inherited SelText
  else
    Result := Copy(Text, SelStart + 1, SelLength);
end;

procedure TCustomPlanUNIEdit.SetSelText(const Value: WideString);
begin
  if (not IsWindowUnicode(Handle)) then
    inherited SelText := Value
  else
    SendMessageW(Handle, EM_REPLACESEL, 0, LParam(PWideChar(Value)));
end;

function TCustomPlanUNIEdit.GetText: WideString;
begin
  Result := WideGetWindowText(Self);
end;

procedure TCustomPlanUNIEdit.SetText(const Value: WideString);
begin
  WideSetWindowText(Self, Value);
end;


{ TPlanUniCustomMemo }

procedure TPlanUniCustomMemo.CreateParams(var Params: TCreateParams);
const
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style and not WordWraps[FWordWrap] or ES_MULTILINE
      or ScrollBar[FScrollBars];
  end;
end;

{$IFDEF DELPHI6_LVL}
procedure TPlanUniCustomMemo.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(False);
end;
{$ENDIF}

procedure TPlanUniCustomMemo.SetScrollBars(const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TPlanUniCustomMemo.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    RecreateWnd;
  end;
end;

procedure TPlanUniCustomMemo.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if FWantTabs then
    Message.Result := Message.Result or DLGC_WANTTAB
  else
    Message.Result := Message.Result and not DLGC_WANTTAB;

  if not FWantReturns then
    Message.Result := Message.Result and not DLGC_WANTALLKEYS;
end;

{ TUniMemo }

constructor TPlanUniMemo.Create(AOwner: TComponent);
begin
  inherited;
  Width := 200;
  Height := 100;
  AutoSize := false;
end;

{ TUNILabel }


initialization
  if (Win32Platform and VER_PLATFORM_WIN32_NT) <> 0 then
      @WideCompareText := @CompareTextWinNT
  else @WideCompareText := @CompareTextWin95;
  WinControlTrap_Atom := GlobalAddAtom(StrFmt(AtomText, 'WinControlTrap.UnicodeClass.%d',
    [GetCurrentProcessID]));

finalization
  GlobalDeleteAtom(WinControlTrap_Atom);

end.
