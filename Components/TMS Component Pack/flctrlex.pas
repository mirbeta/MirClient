{********************************************************************}
{ Extended filecontrol components                                    }
{ for Delphi & C++ Builder                                           }
{                                                                    }
{ written by :                                                       }
{           TMS Software                                             }
{           copyright © 1999-2013                                    }
{           Email : info@tmssoftware.com                             }
{           Website : http://www.tmssoftware.com                     }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

{$I TMSDEFS.INC}

unit FlCtrlEx;

{$R-,T-,H+,X+}

interface

{$WARNINGS OFF}
uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
  Menus, StdCtrls, FileCtrl, ShellApi, CommCtrl
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;
{$WARNINGS ON}

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.2.0.1 : Fixed issue with ShowGlyphs = false
  // v1.2.1.0 : Improved : handling of selection of invalid drives
  // v1.2.1.1 : Fixed issue with ShowGlyphs = false in TFileListBoxEx
  //          : Fixed issue with itemheight for non default font
   
type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TFileListBoxEx = class(TFileListBox)
  private
    FImages: TImageList;
    FWinNT: Boolean;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    function GetVersionNr: Integer; virtual;
    procedure ReadFileNames; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);  override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Version: string read GetVersion write SetVersion;
  end;

  TListNotifyEvent = procedure(Sender: TObject; Item: Integer) of object;

  TBoolList = class(TList)
  private
    FOnChange: TListNotifyEvent;
    function GetBoolean(index: Integer): Boolean;
    procedure SetBoolean(index: Integer; const Value: Boolean);
  public
    property Items[Index: Integer]: Boolean read GetBoolean write SetBoolean; default;
    procedure Add(Value: Boolean);
    procedure Insert(Index: Integer;Value: Boolean);
    procedure Delete(Index: Integer);

    property OnChange: TListNotifyEvent read FOnChange write FOnChange;
  end;

  TCheckClickEvent = procedure(Sender: TObject; Index: Integer) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCheckFileListBoxEx = class(TFileListBoxEx)
  private
    FBoolList: TBoolList;
    FFlat: Boolean;
    FOnClickCheck: TCheckClickEvent;
    function GetChecked(Index: Integer): Boolean;
    procedure SetChecked(Index: Integer; const Value: Boolean);
    procedure SyncLists;
    procedure SetFlat(const Value: Boolean);
  protected
    procedure DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);  override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyPress(var ch:char); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
  published
    property Flat: Boolean read FFlat write SetFlat;
    property OnClickCheck: TCheckClickEvent read FOnClickCheck write FOnClickCheck;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDirectoryListBoxEx = class(TDirectoryListBox)
  private
    FImages: TImageList;
    FDirOpen,FDirClosed:integer;
    FWinNT: Boolean;
    FDirectOpen: Boolean;
    FIndent: Integer;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    function GetVersionNr: Integer; virtual;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property Indent: Integer read FIndent write FIndent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BorderStyle;
    property DirectOpen: Boolean read FDirectOpen write FDirectOpen;
    property Version: string read GetVersion write SetVersion;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCheckDirectoryListBoxEx = class(TDirectoryListBoxEx)
  private
    FBoolList: TBoolList;
    FFlat: Boolean;
    FOnClickCheck: TCheckClickEvent;
    function GetChecked(Index: Integer): Boolean;
    procedure SetChecked(Index: Integer; const Value: Boolean);
    procedure SyncLists;
    procedure SetFlat(const Value: Boolean);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyPress(var ch:char); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
  published
    property BorderStyle;
    property Flat: Boolean read FFlat write SetFlat;
    property OnClickCheck: TCheckClickEvent read FOnClickCheck write FOnClickCheck;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDriveComboBoxEx = class(TDriveComboBox)
  private
    FImages: TImageList;
    FWinNT: Boolean;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    function GetVersionNr: Integer; virtual;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure BuildList; override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
  published
    property Version: string read GetVersion write SetVersion;
 end;


implementation

{$IFDEF DELPHI7_LVL}
uses
  Themes;
{$ENDIF}

var
  FCheckWidth, FCheckHeight: Integer;

{$I DELPHIXE.INC}

{$IFDEF DELPHIXE2_LVL}
function ThemeServices: TCustomStyleServices;
begin
  Result := StyleServices;
end;

function ThemeServicesThemesEnabled: boolean;
begin
  Result := StyleServices.Enabled;
end;
{$ENDIF}

{$IFDEF DELPHI7_LVL}
{$IFNDEF DELPHIXE2_LVL}
function ThemeServicesThemesEnabled: boolean;
begin
  Result := ThemeServices.ThemesEnabled;
end;
{$ENDIF}
{$ENDIF}

procedure GetCheckSize;
begin
  with TBitmap.Create do
    try
      Handle := LoadBitmap(0, PChar(OBM_CHECKBOXES));
      FCheckWidth := Width div 4;
      FCheckHeight := Height div 3;
    finally
      Free;
    end;
end;


function IsWinNTorME:Boolean;
var
  verinfo: TOSVersionInfo;
begin
  verinfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);

  GetVersionEx(verinfo);

  if (verinfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS) then
    Result := verinfo.dwMinorVersion = 90
  else
    Result := (verinfo.dwPlatformId = VER_PLATFORM_WIN32_NT);
end;


constructor TFileListBoxEx.Create(AOwner: TComponent);
var
  SHFileInfo:TSHFileInfo;
begin
  inherited;
  // only working for Windows 95,98
  FWinNT := IsWinNTorME;
  if not FWinNT then
  begin
    FImages := TImageList.CreateSize(16,16);
    FImages.ShareImages := TRUE;
    FImages.Handle := ShGetFileInfo('*.*', 0, SHFileInfo,
                                   SizeOf(SHFileInfo), SHGFI_SMALLICON or SHGFI_SYSICONINDEX);

  end;
  if (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState)) then
    ShowGlyphs := true;
end;

destructor TFileListBoxEx.Destroy;
begin
  if not FWinNT then
    FImages.Free;
  inherited;
end;

procedure TFileListBoxEx.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    Canvas.Font.Assign(Font);
    ItemHeight := Canvas.TextHeight('gh') + 2;
  end;
end;

procedure TFileListBoxEx.ReadFileNames;
var
  AttrIndex: TFileAttr;
  I: Integer;
  MaskPtr: PChar;
  Ptr: PChar;
  AttrWord: Word;
  FileInfo: TSearchRec;
  SaveCursor: TCursor;
  sfi : tshFileInfo;

const
{$WARNINGS OFF}
   Attributes: array[TFileAttr] of Word = (faReadOnly, faHidden, faSysFile,
     faVolumeID, faDirectory, faArchive, 0);
{$WARNINGS ON}
begin
      { if no handle allocated yet, this call will force
        one to be allocated incorrectly (i.e. at the wrong time.
        In due time, one will be allocated appropriately.  }
  AttrWord := DDL_READWRITE;

  if HandleAllocated then
  begin
    { Set attribute flags based on values in FileType }
    for AttrIndex := ftReadOnly to ftArchive do
      if AttrIndex in FileType then
        AttrWord := AttrWord or Attributes[AttrIndex];

    ChDir(FDirectory); { go to the directory we want }
    Clear; { clear the list }

    I := 0;
    SaveCursor := Screen.Cursor;
    try
      MaskPtr := PChar(FMask);
      while MaskPtr <> nil do
      begin
        Ptr := StrScan (MaskPtr, ';');
        if Ptr <> nil then
          Ptr^ := #0;
        if FindFirst(MaskPtr, AttrWord, FileInfo) = 0 then
        begin
          repeat            { exclude normal files if ftNormal not set }
            if (ftNormal in FileType) or (FileInfo.Attr and AttrWord <> 0) then
              if FileInfo.Attr and faDirectory <> 0 then
              begin
                I := Items.Add(Format('[%s]',[FileInfo.Name]));
                if ShowGlyphs then
                  Items.Objects[I] := DirBMP;
              end
              else
              begin
                if not fWinNT then
                begin
                  SHGetFileInfo(PChar(FileInfo.Name),FILE_ATTRIBUTE_NORMAL, SFI, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX  or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES);

                  I := Items.AddObject(FileInfo.Name,TObject( sfi.iIcon ));
                end
                else
                 Items.AddObject(FileInfo.Name,nil);


              end;
            if I = 100 then
              Screen.Cursor := crHourGlass;
          until FindNext(FileInfo) <> 0;
          FindClose(FileInfo);
        end;
        if Ptr <> nil then
        begin
          Ptr^ := ';';
          Inc (Ptr);
        end;
        MaskPtr := Ptr;
      end;
    finally
      Screen.Cursor := SaveCursor;
    end;
    Change;
  end;

end;


procedure TFileListBoxEx.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  offset: Integer;
  sfi : tshFileInfo;
  syshandle:thandle;

begin
  with Canvas do
  begin
    FillRect(Rect);
    offset := 0;

    if ShowGlyphs then
    begin
      offset:=17;

      if FWinNT then
      begin
        syshandle := SHGetFileInfo(PChar(Items[Index]), FILE_ATTRIBUTE_NORMAL, SFI, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX  or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES);
        ImageList_Draw(syshandle,sfi.iIcon,canvas.handle,rect.left,rect.top, ILD_TRANSPARENT);
      end
      else
        ImageList_Draw(fImages.Handle,integer(Items.Objects[Index]),canvas.handle,rect.left,rect.top, ILD_TRANSPARENT);
    end;

    TextOut(Rect.Left + offset, Rect.Top, Items[Index])
  end;
end;

function TFileListBoxEx.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TFileListBoxEx.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TFileListBoxEx.SetVersion(const Value: string);
begin

end;

function DirLevel(const PathName: string): Integer;  { counts '\' in path }
var
  P: PChar;
begin
  Result := 0;
  P := AnsiStrScan(PChar(PathName), '\');
  while P <> nil do
  begin
    Inc(Result);
    Inc(P);
    P := AnsiStrScan(P, '\');
  end;
end;

procedure TDirectoryListBoxEx.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Bitmap: TBitmap;
  bmpWidth: Integer;
  dirOffset: Integer;
  sfi : tshFileInfo;
  syshandle:thandle;

begin
  with Canvas do
  begin
    FillRect(Rect);
    bmpWidth  := 16;
    dirOffset := Index * 4 + 2;    {add 2 for spacing}

    Bitmap := TBitmap(Items.Objects[Index]);
    if Bitmap <> nil then
    begin
      if Bitmap = ClosedBMP then
        dirOffset := (DirLevel (Directory) + 1) * 4 + 2;
      bmpwidth:=16;
      if fWinNT then
       begin
        if Bitmap = ClosedBMP then
         syshandle:=ShGetFileInfo('*.*',FILE_ATTRIBUTE_DIRECTORY, sfi,
                         SizeOf(sfi), SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES)

        else
         syshandle:=ShGetFileInfo('*.*',FILE_ATTRIBUTE_DIRECTORY, sfi,
                         SizeOf(sfi), SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or SHGFI_OPENICON);

        ImageList_Draw(syshandle,sfi.iIcon,canvas.handle,rect.left+diroffset,rect.top, ILD_TRANSPARENT);
       end
      else
       begin
        if Bitmap = ClosedBMP then
         ImageList_Draw(fImages.Handle,fDirclosed,canvas.handle,rect.left+diroffset,rect.top, ILD_TRANSPARENT)
        else
         ImageList_Draw(fImages.Handle,fDirOpen,canvas.handle,rect.left+diroffset,rect.top, ILD_TRANSPARENT);
       end;
    end;
    TextOut(Rect.Left + bmpWidth + dirOffset + 4, Rect.Top, DisplayCase(Items[Index]))
  end;
end;

constructor TDirectoryListBoxEx.Create(AOwner: TComponent);
var
 SHFileInfo:TSHFileInfo;
begin
 inherited;
 {only working for Windows 95,98}
 fWinNT:=IsWinNTorME;
 if not fWinNT then
  begin
   fImages:=TImageList.CreateSize(16,16);
   fImages.ShareImages:=TRUE;
   fImages.Handle := ShGetFileInfo('*.*', 0, SHFileInfo,
                                   SizeOf(SHFileInfo), SHGFI_SMALLICON or SHGFI_SYSICONINDEX);
   ShGetFileInfo('',FILE_ATTRIBUTE_DIRECTORY, SHFileInfo,
                  SizeOf(SHFileInfo), SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or SHGFI_OPENICON);

   fDirOpen:=shfileinfo.iIcon;
   ShGetFileInfo('',FILE_ATTRIBUTE_DIRECTORY, SHFileInfo,
                  SizeOf(SHFileInfo), SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES );

   fDirClosed:=shfileinfo.iIcon;
  end;
  FIndent := 0;
end;

destructor TDirectoryListBoxEx.Destroy;
begin
 if not fWinNT then fImages.Free;
 inherited;
end;

function TDirectoryListBoxEx.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TDirectoryListBoxEx.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TDirectoryListBoxEx.SetVersion(const Value: string);
begin

end;

procedure TDriveComboBoxEx.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  bmpWidth: Integer;
  syshandle:integer;
  sfi:TSHFileInfo;
  s:string;
begin

  with Canvas do
  begin
    FillRect(Rect);
    bmpWidth  := 16;

    {draw image}
    if fWinNT then
     begin
      s:=Items[Index];
      if pos(' ',s)>0 then delete(s,pos(' ',s),255);
      syshandle:=ShGetFileInfo(pchar(s+'\'),0, sfi,
                                     SizeOf(sfi), SHGFI_SMALLICON or SHGFI_SYSICONINDEX);

      ImageList_Draw(syshandle,sfi.iIcon,canvas.handle,rect.left,rect.top, ILD_TRANSPARENT);
     end
    else
     begin
      ImageList_Draw(fImages.handle,integer(Items.Objects[Index]),canvas.handle,rect.left,rect.top, ILD_TRANSPARENT);
     end;

    Rect.Left := Rect.Left + bmpWidth + 6;
    DrawText(Canvas.Handle, PChar(Items[Index]), -1, Rect,
             DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
  end;
end;

procedure TDriveComboBoxEx.Click;
begin
  try
    inherited;
  except
    if Assigned(DirList) then
    begin
      DirList.Clear;
      if Assigned(DirList.FileList) then
        DirList.FileList.Clear;
    end;
  end;
end;

procedure TDriveComboBoxEx.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    itemHeight := 16;
  end;
end;

function VolumeID(DriveChar: Char): string;
var
  OldErrorMode: Integer;
  NotUsed, VolFlags: DWORD;
  Buf: array [0..MAX_PATH] of Char;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Buf[0] := #$00;
    if GetVolumeInformation(PChar(DriveChar + ':\'), Buf, DWORD(sizeof(Buf)),
      nil, NotUsed, VolFlags, nil, 0) then
      SetString(Result, Buf, StrLen(Buf))
    else Result := '';
    if DriveChar < 'a' then
      Result := AnsiUpperCase(Result)
    else
      Result := AnsiLowerCase(Result);
    Result := Format('[%s]',[Result]);
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

function NetworkVolume(DriveChar: Char): string;
var
  Buf: Array [0..MAX_PATH] of Char;
  DriveStr: array [0..3] of Char;
  BufferSize: DWORD;
begin
  BufferSize := sizeof(Buf);
  DriveStr[0] := UpCase(DriveChar);
  DriveStr[1] := ':';
  DriveStr[2] := #0;
  if WNetGetConnection(DriveStr, Buf, BufferSize) = WN_SUCCESS then
  begin
    SetString(Result, Buf, BufferSize);
    if DriveChar < 'a' then
      Result := AnsiUpperCase(Result)
    else
      Result := AnsiLowerCase(Result);
  end
  else
    Result := VolumeID(DriveChar);
end;

procedure TDriveComboBoxEx.BuildList;
var
  DriveNum: Integer;
  DriveChar: Char;
  DriveType: TDriveType;
  DriveBits: set of 0..25;
  sfi:TSHFileInfo;

  procedure AddDrive(const VolName: string; Obj: TObject);
  begin
    if not fWinNT then
      begin
       ShGetFileInfo(pchar(DriveChar+':\'),0, sfi,
                                     SizeOf(sfi), SHGFI_SMALLICON or SHGFI_SYSICONINDEX);
        Items.AddObject(Format('%s: %s',[DriveChar, VolName]), TObject(sfi.iIcon));
      end
    else

    Items.AddObject(Format('%s: %s',[DriveChar, VolName]), Obj);
  end;

begin
  { fill list }
  Clear;
  Integer(DriveBits) := GetLogicalDrives;
  for DriveNum := 0 to 25 do
  begin
    if not (DriveNum in DriveBits) then Continue;
    DriveChar := Char(DriveNum + Ord('a'));
    DriveType := TDriveType(GetDriveType(PChar(DriveChar + ':\')));
    if TextCase = tcUpperCase then
      DriveChar := Upcase(DriveChar);

    case DriveType of
      dtFloppy:   AddDrive('',FloppyBMP);
      dtFixed:    AddDrive(VolumeID(DriveChar), FixedBMP);
      dtNetwork:  AddDrive(NetworkVolume(DriveChar), NetworkBMP);
      dtCDROM:    AddDrive(VolumeID(DriveChar), CDROMBMP);
      dtRAM:      AddDrive(VolumeID(DriveChar), RAMBMP);
    end;
  end;
end;


constructor TDriveComboBoxEx.Create(AOwner: TComponent);
var
 SHFileInfo:TSHFileInfo;
begin
 inherited;
 {only working for Windows 95,98}
 fWinNT:=IsWinNTorME;
 if not fWinNT then
  begin
   fImages:=TImageList.CreateSize(16,16);
   fImages.ShareImages:=TRUE;
   fImages.Handle := ShGetFileInfo('*.*', 0, SHFileInfo,
                                   SizeOf(SHFileInfo), SHGFI_SMALLICON or SHGFI_SYSICONINDEX);
  end;
end;

destructor TDriveComboBoxEx.Destroy;
begin
 if not fWinNT then fImages.Free;
 inherited;
end;

function TDriveComboBoxEx.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TDriveComboBoxEx.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TDriveComboBoxEx.Refresh;
begin
  BuildList;
end;

procedure TDriveComboBoxEx.SetVersion(const Value: string);
begin

end;

{ TBoolList }

procedure TBoolList.Add(Value: Boolean);
begin
  inherited Add(Pointer(Value));

  if Assigned(OnChange) then
    OnChange(Self,Self.Count);
end;


procedure TBoolList.Delete(Index: Integer);
begin
  inherited Delete(Index);
  if Assigned(OnChange) then
    OnChange(Self,Index);
end;

function TBoolList.GetBoolean(Index: Integer): Boolean;
begin
   Result := Boolean(inherited Items[Index]);
end;

procedure TBoolList.Insert(Index: Integer; Value: Boolean);
begin
  inherited Insert(Index, Pointer(Value));

end;

procedure TBoolList.SetBoolean(Index: Integer; const Value: Boolean);
begin
  inherited Items[Index] := Pointer(Value);

  if Assigned(OnChange) then
    OnChange(Self,Index);
end;

{ TCheckFileListBox }

constructor TCheckFileListBoxEx.Create(AOwner: TComponent);
begin
  inherited;
  FBoolList := TBoolList.Create;
  FFlat := True;
end;

destructor TCheckFileListBoxEx.Destroy;
begin
  FBoolList.Free;
  inherited;
end;

procedure TCheckFileListBoxEx.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  DrawRect: TRect;
  OldColor: TColor;
begin
  with Canvas do
  begin
    DrawRect := Rect;
    DrawRect.Right := 18;

    OldColor := Canvas.Brush.Color;

    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Rectangle(DrawRect.Left,DrawRect.Top,DrawRect.Right,DrawRect.Bottom);
    Canvas.FillRect(DrawRect);

    DrawRect.Right := 16;

    InflateRect(DrawRect,-1,-1);

    if Checked[Index] then
      DrawCheck(DrawRect,cbChecked,Enabled)
    else
     DrawCheck(DrawRect,cbUnChecked,Enabled);

    Canvas.Brush.Color := OldColor;
  end;

  Rect.Left := Rect.Left + 18;
  inherited DrawItem(Index,Rect,State);
end;

function TCheckFileListBoxEx.GetChecked(Index: Integer): Boolean;
begin
  SyncLists;
  Result := FBoolList.Items[Index];
end;

procedure TCheckFileListBoxEx.Keypress(var ch: char);
begin
  if ch = #32 then
  begin
    if ItemIndex >= 0 then
    begin
      Checked[ItemIndex] := not Checked[ItemIndex];
      if Assigned(FOnClickCheck) then
        FOnClickCheck(Self,ItemIndex);
    end;
  end;
  inherited;
end;

procedure TCheckFileListBoxEx.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  idx: Integer;
begin
  inherited;

  if (X < 16) then
  begin
    idx := SendMessage(Handle,LB_ITEMFROMPOINT,0,MAKELPARAM(X,Y));
    if (idx >= 0) and (idx < Items.Count) then
    begin
      Checked[idx] := not Checked[idx];
      if Assigned(FOnClickCheck) then
        FOnClickCheck(Self,idx);
    end;
  end;

end;

procedure TCheckFileListBoxEx.SetChecked(Index: Integer;
  const Value: Boolean);
var
  r: TRect;
begin
  SyncLists;
  FBoolList.Items[Index] := Value;
  // force a repaint

  SendMessage(Handle,LB_GETITEMRECT,Index,LParam(@r));
  InvalidateRect(Handle,@r,false);
end;

procedure TCheckFileListBoxEx.SyncLists;
begin
  while FBoolList.Count < Items.Count do
    FBoolList.Add(False);

  while FBoolList.Count > Items.Count do
    FBoolList.Delete(FBoolList.Count - 1);
end;

{$IFNDEF DELPHI7_LVL}
procedure TCheckFileListBoxEx.DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
var
  State: DWORD;
begin
  State := DFCS_BUTTONCHECK;
  if not AEnabled then
    State := DFCS_INACTIVE;

  if AState = cbChecked then
    State := State or DFCS_CHECKED;

  if Flat then
    State := State or DFCS_FLAT;
  DrawFrameControl(Canvas.Handle,R, DFC_BUTTON, State);
end;
{$ENDIF}

{$IFDEF DELPHI7_LVL}
procedure TCheckFileListBoxEx.DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
var
  DrawState: Integer;
  DrawRect: TRect;
  OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  OldPenColor: TColor;
  Rgn, SaveRgn: HRgn;

  ElementDetails: TThemedElementDetails;

begin
  SaveRgn := 0;
  DrawRect.Left := R.Left + (R.Right - R.Left - FCheckWidth) div 2;
  DrawRect.Top := R.Top + (R.Bottom - R.Top - FCheckHeight) div 2;
  DrawRect.Right := DrawRect.Left + FCheckWidth;
  DrawRect.Bottom := DrawRect.Top + FCheckHeight;
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

   if ThemeServicesThemesEnabled then
   begin
      case AState of
        cbChecked:
          if AEnabled then
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal)
          else
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxCheckedDisabled);
        cbUnchecked:
          if AEnabled then
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal)
          else
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxUncheckedDisabled)
        else // cbGrayed
          if AEnabled then
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxMixedNormal)
          else
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxMixedDisabled);
      end;
      ThemeServices.DrawElement(Handle, ElementDetails, R);
    end
    else
    begin
      case AState of
        cbChecked:
          DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED;
        cbUnchecked:
          DrawState := DFCS_BUTTONCHECK;
        else // cbGrayed
          DrawState := DFCS_BUTTON3STATE or DFCS_CHECKED;
      end;
      if not AEnabled then
        DrawState := DrawState or DFCS_INACTIVE;
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
{$ENDIF}

procedure TCheckFileListBoxEx.WndProc(var Message: TMessage);
begin
  inherited;
  case message.msg of
  LB_INSERTSTRING: FBoolList.Insert(Message.WParam,False);
  LB_DELETESTRING: FBoolList.Delete(Message.Wparam);
  LB_RESETCONTENT: FBoolList.Clear;
  end;
end;


{ TCheckDirectoryListBox }

constructor TCheckDirectoryListBoxEx.Create(AOwner: TComponent);
begin
  inherited;
  FBoolList := TBoolList.Create;
  FFlat := True;
  FIndent := 16;
end;

destructor TCheckDirectoryListBoxEx.Destroy;
begin
  FBoolList.Free;
  inherited;
end;

procedure TCheckDirectoryListBoxEx.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  DrawRect: TRect;
  OldColor: TColor;
begin
  with Canvas do
  begin
    DrawRect := Rect;
    DrawRect.Right := 18;

    OldColor := Canvas.Brush.Color;

    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Rectangle(DrawRect.Left,DrawRect.Top,DrawRect.Right,DrawRect.Bottom);
    Canvas.FillRect(DrawRect);

    DrawRect.Right := 16;

    InflateRect(DrawRect,-1,-1);

    if Checked[Index] then
      DrawCheck(DrawRect,cbChecked,Enabled)
    else
     DrawCheck(DrawRect,cbUnChecked,Enabled);

    Canvas.Brush.Color := OldColor;
  end;

  Rect.Left := Rect.Left + 18;
  inherited DrawItem(Index,Rect,State);
end;

function TCheckDirectoryListBoxEx.GetChecked(Index: Integer): Boolean;
begin
  SyncLists;
  Result := FBoolList.Items[Index];
end;

procedure TCheckDirectoryListBoxEx.Keypress(var ch: char);
begin
  if ch = #32 then
  begin
    if ItemIndex >= 0 then
    begin
      Checked[ItemIndex] := not Checked[ItemIndex];
      if Assigned(FOnClickCheck) then
        FOnClickCheck(Self,ItemIndex);
    end;
  end;
  inherited;  
end;

procedure TCheckDirectoryListBoxEx.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  idx: Integer;
begin
  inherited;
  if (X < 16) then
  begin
    idx := SendMessage(Handle,LB_ITEMFROMPOINT,0,MAKELPARAM(X,Y));
    if (idx >= 0) and (idx < Items.Count) then
    begin
      Checked[idx] := not Checked[idx];
      if Assigned(FOnClickCheck) then
        FOnClickCheck(Self,idx);
    end;
  end;


end;

procedure TCheckDirectoryListBoxEx.SetChecked(Index: Integer;
  const Value: Boolean);
var
  r: TRect;
begin
  SyncLists;
  FBoolList.Items[Index] := Value;
  // force a repaint

  SendMessage(Handle,LB_GETITEMRECT,Index,LParam(@r));
  InvalidateRect(Handle,@r,false);
end;

procedure TCheckDirectoryListBoxEx.SyncLists;
begin
  while FBoolList.Count < Items.Count do
    FBoolList.Add(False);

  while FBoolList.Count > Items.Count do
    FBoolList.Delete(FBoolList.Count - 1);
end;

{$IFNDEF DELPHI7_LVL}
procedure TCheckDirectoryListBoxEx.DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
var
  State: DWORD;
begin
  State := DFCS_BUTTONCHECK;
  if not AEnabled then
    State := DFCS_INACTIVE;

  if AState = cbChecked then
    State := State or DFCS_CHECKED;

  if Flat then
    State := State or DFCS_FLAT;
  DrawFrameControl(Canvas.Handle,R, DFC_BUTTON, State);
end;
{$ENDIF}

{$IFDEF DELPHI7_LVL}
procedure TCheckDirectoryListBoxEx.DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
var
  DrawState: Integer;
  DrawRect: TRect;
  OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  OldPenColor: TColor;
  Rgn, SaveRgn: HRgn;
  ElementDetails: TThemedElementDetails;
begin
  SaveRgn := 0;
  DrawRect.Left := R.Left + (R.Right - R.Left - FCheckWidth) div 2;
  DrawRect.Top := R.Top + (R.Bottom - R.Top - FCheckHeight) div 2;
  DrawRect.Right := DrawRect.Left + FCheckWidth;
  DrawRect.Bottom := DrawRect.Top + FCheckHeight;
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

   if ThemeServicesThemesEnabled then
   begin
      case AState of
        cbChecked:
          if AEnabled then
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal)
          else
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxCheckedDisabled);
        cbUnchecked:
          if AEnabled then
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal)
          else
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxUncheckedDisabled)
        else // cbGrayed
          if AEnabled then
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxMixedNormal)
          else
            ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxMixedDisabled);
      end;
      ThemeServices.DrawElement(Handle, ElementDetails, R);
    end
    else
    begin
      case AState of
        cbChecked:
          DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED;
        cbUnchecked:
          DrawState := DFCS_BUTTONCHECK;
        else // cbGrayed
          DrawState := DFCS_BUTTON3STATE or DFCS_CHECKED;
      end;
      if not AEnabled then
        DrawState := DrawState or DFCS_INACTIVE;
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
{$ENDIF}

procedure TCheckDirectoryListBoxEx.WndProc(var Message: TMessage);
begin
  inherited;
  case message.msg of
  LB_INSERTSTRING: FBoolList.Insert(Message.WParam,False);
  LB_DELETESTRING: FBoolList.Delete(Message.Wparam);
  LB_RESETCONTENT: FBoolList.Clear;
  end;
end;


procedure TCheckFileListBoxEx.SetFlat(const Value: Boolean);
begin
  FFlat := Value;
  Invalidate;
end;

procedure TCheckDirectoryListBoxEx.SetFlat(const Value: Boolean);
begin
  FFlat := Value;
  Invalidate;
end;

procedure TDirectoryListBoxEx.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  idx: Integer;
begin
  inherited;
  if DirectOpen and (X > Indent) then
  begin
    idx := SendMessage(Handle,LB_ITEMFROMPOINT,0,MAKELPARAM(X,Y));
    if (idx >= 0) then
      DblClick;
  end;
end;

initialization
  GetCheckSize;

end.

