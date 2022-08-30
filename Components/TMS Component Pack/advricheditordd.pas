{************************************************************************}
{ Drag & drop interface support file                                     }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{            copyright © 2015                                            }
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

unit AdvRichEditorDD;

{$I TMSDEFS.INC}

{$H+}

{$IFDEF BCB}
{$HPPEMIT '#include <oleidl.h>'}
{$ENDIF}

interface

uses
  Windows, Messages, ActiveX, StdCtrls, Classes, Controls, SysUtils,
  Forms, ShlObj, Dialogs, Graphics;

const
  deNone   = DROPEFFECT_NONE;
  deMove   = DROPEFFECT_MOVE;
  deCopy   = DROPEFFECT_COPY;
  deLink   = DROPEFFECT_LINK;
  deScroll = DROPEFFECT_SCROLL;
  ddGet = DATADIR_GET;
  ddSet = DATADIR_SET;

  tsGlobal       = TYMED_HGLOBAL;   // handle to global memory clock
  tsFile         = TYMED_FILE;      // file
  tsStream       = TYMED_ISTREAM;   // stream interface
  tsStorage      = TYMED_ISTORAGE;  // storage interface
  tsGDI          = TYMED_GDI;       // gdi object
  tsMetafilePict = TYMED_MFPICT;    // metafilepict structure
  tsEnhMetafile  = TYMED_ENHMF;     // enhanced metafile
  tsNull         = TYMED_NULL;      // no storage

type
  TEnumFormats = class
  private
    FDataObject : IDataObject;
    FEnumerator : IEnumFormatEtc;
    FFormatEtc  : TFormatEtc;
    FValid      : boolean;
    FCount      : integer;
    FFiles      : TStringList;
    FMediumValid: boolean;
    FMedium     : TStgMedium;
    procedure SetDataObject(Value : IDataObject);
    function SomeText(Format: TClipFormat): string;
    function SomeRTF(Format: TClipFormat): string;
    function SomeFiles(var Files: TStringList): boolean;
    function SomeBMP(var bmp: TBitmap): boolean;
    function SomeStream(Format : TClipFormat): TMemoryStream;
    function GetAspect: Integer;
    procedure SetAspect(value: Integer);
    function GetcfFormat: TClipFormat;
    procedure SetcfFormat(Value: TClipFormat);
    function GetlIndex: Integer;
    procedure SetlIndex(Value: Integer);
    function GetTymed: Integer;
    procedure SetTymed(Value: Integer);
    procedure FreeMedium;
  public
    constructor Create (DataObject : IDataObject);
    destructor Destroy; override;
    function Reset: boolean;
    function Next: boolean;
    function HasFormat(ClipFormat : TClipFormat) : boolean;
    function Handle(Tymed: integer): HGlobal;
    function GlobalHandle: HGlobal;
    function GDIHandle: HGlobal;
    function HasText: boolean;
    function HasFile: boolean;
    function HasRTF: boolean;
    function HasRTE: boolean;
    function HasURL: boolean;
    function HasBMP: boolean;
    function Text: string;
    function RTF: string;
    function Stream: TMemoryStream;
    property Count : integer read FCount;
    property DataObject : IDataObject read FDataObject write SetDataObject;
    property Valid : boolean read FValid;
    property FormatEtc : TFormatEtc read FFormatEtc;
    property Aspect : integer read GetAspect write SetAspect;
    property Format : TClipFormat read GetcfFormat write SetcfFormat;
    property Index : integer read GetlIndex write SetlIndex;
    property Medium : integer read GetTymed write SetTymed;
  end;

  TDropFormat = (dfText, dfFile, dfRTF, dfURL, dfRTE, dfBMP);
  TDropFormats = set of TDropFormat;

  TRichEditorDropTarget = class (TInterfacedObject, IDropTarget)
    function DragEnter(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
  private
    FOk: boolean;
    FAcceptText: boolean;
    FAcceptFiles: boolean;
    FAcceptURLs: boolean;
    FDropFormats: TDropFormats;
  public
    constructor Create;
    procedure DropText(pt: TPoint;s:string; dwEffect: longint); virtual; abstract;
    procedure DropStream(pt: TPoint; AStream: TMemoryStream; dwEffect: longint); virtual; abstract;
    procedure DropRTF(pt: TPoint;s:string; dwEffect: longint); virtual; abstract;
    procedure DropFiles(pt: TPoint;files: TStrings; dwEffect: longint); virtual; abstract;
    procedure DropURL(pt: TPoint;s:string; dwEffect: longint); virtual; abstract;
    procedure DropBMP(pt: TPoint; bmp: Graphics.TBitmap; dwEffect: longint); virtual; abstract;
    procedure DragMouseMove(pt:TPoint;var Allow:boolean; DropFormats: TDropFormats); virtual;
    procedure DragMouseEnter; virtual;
    procedure DragMouseLeave; virtual;

    property AcceptText: boolean read FAcceptText write FAcceptText;
    property AcceptFiles: boolean read FAcceptFiles write FAcceptFiles;
    property AcceptURLs: boolean read FAcceptURLs write FAcceptURLs;
    property DropFormats:TDropFormats read FDropFormats;
  end;

  TRichEditorDropSource = class (TInterfacedObject, IDropSource)
  private
    FNoAccept:boolean;
  protected
    procedure DragDropStop; virtual;
  public
    constructor Create;
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
    procedure CurrentEffect(dwEffect: Longint); virtual;
    procedure QueryDrag; virtual;
  end;

  TSourceDataObject = class (TInterfacedObject, IDataObject)
  private
    textdata:string;
    rtfdata:string;
    memstr: TMemoryStream;
  public
    constructor Create(const stxt,srtf:string; ms: TMemoryStream);
  public
    function GetData(const formatetc: TFormatEtc; out medium: TStgMedium): HResult; stdcall;
    function GetDataHere (const formatetc: TFormatEtc; out medium: TStgMedium): HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult; stdcall;
    function GetCanonicalFormatEtc(const formatetcIn: TFormatEtc; out formatetcOut: TFormatEtc): HResult; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
  end;

  TSourceEnumFormatEtc = class (TInterfacedObject, IEnumFormatEtc)
  private
  protected
    FIndex : integer; // Next FormatEtc to get
    FHasRTE: boolean;
  public
    function Next (CountRequested: Longint; out FormatEtcArray; PCountFetched: PLongint): HResult; stdcall;
    function Skip (count: Longint) : HResult; stdcall;
    function Reset : HResult; stdcall;
    function Clone (out enumFmt : IEnumFormatEtc) : HResult; stdcall;
  end;

  TFormatEtcArray = array[0..19] of TFormatEtc;
  PFormatEtcArray = ^TFormatEtcArray;

function StandardEffect (Keys : TShiftState; DefEffect: Integer) : integer;
function StartTextDoDragDrop(DropSource:TRichEditorDropSource;stxt,srtf:string;ms: TMemoryStream;dwOKEffects: Longint; var dwEffect: Longint): HResult;
procedure SetRTFAware(value:boolean);

function ClipboardHasText(Format: word): boolean;
function ClipboardHasBitmap(Format: word): boolean;
function ClipboardHasPicture(Format: word): boolean;
function ClipboardHasRTE(Format: word): boolean;
function ClipboardRTEFMT: word;
function ClipboardHTMLFMT: word;
function ClipboardRTFFMT: word;

implementation

uses
  Clipbrd
{$IFDEF DELPHIXE4_LVL}
  , AnsiStrings
{$ENDIF}
  ;

var
  CF_RTF : Integer;
  CF_REFMTTXT: Integer;
  CF_HTML: Integer;
  CF_URL : Integer;
  RTFAware: Boolean;

function ClipboardRTEFMT: word;
begin
  Result := CF_REFMTTXT;
end;

function ClipboardRTFFMT: word;
begin
  Result := CF_RTF;
end;

function ClipboardHTMLFMT: word;
begin
  Result := CF_HTML;
end;

function ClipboardHasText(Format: word): boolean;
begin
  Result := Format = CF_TEXT;
end;

function ClipboardHasBitmap(Format: word): boolean;
begin
  Result := Format = CF_BITMAP;
end;

function ClipboardHasPicture(Format: word): boolean;
begin
  Result := Format = CF_PICTURE;
end;

function ClipboardHasRTE(Format: word): boolean;
begin
  Result := Format = CF_REFMTTXT;
end;

procedure SetRTFAware(Value: Boolean);
begin
  RTFAware := Value;
end;

function StandardEffect (Keys : TShiftState; DefEffect: Integer) : Integer;
begin
  if ssCtrl in Keys then
    Result := deCopy
  else
  begin
    if (DefEffect and deMove = 0) then
      Result := deCopy
    else
      Result := deMove;
  end;
end;

constructor TEnumFormats.Create (DataObject : IDataObject);
begin
  inherited Create;
  SetDataObject (DataObject);
  FFiles := TStringList.Create;
  FMediumValid := false;

  if OpenClipBoard(0) then
  begin
    CF_RTF := RegisterClipboardformat('Rich Text Format');
    CF_URL := RegisterClipboardformat(CFSTR_SHELLURL);
    CF_REFMTTXT := RegisterClipboardFormat('RichEditorText');
    CloseClipBoard;
  end;
end;

destructor TEnumFormats.Destroy;
begin
  SetDataObject(nil);
  FFiles.Free;
  inherited Destroy
end;

procedure TEnumFormats.FreeMedium;
begin
  if FMediumValid then
  begin
    ReleaseStgMedium (FMedium);
    ZeroMemory(@FMedium, SizeOf(TStgMedium))
  end;
  FMediumValid := false
end;

function TEnumFormats.Next: boolean;
var
  Returned : integer;
begin
  inc (FCount);
  FValid := FEnumerator.Next(1, FFormatEtc, @Returned) = S_OK;
  Result := FValid
end;

function TEnumFormats.Reset: boolean;
begin
  FValid := false;
  FCount := 0;
  Result := Succeeded(FEnumerator.Reset)
end;

function TEnumFormats.HasFormat(ClipFormat : TClipFormat) : boolean;
begin
  Result := false;
  if Reset then
    while (not Result) and Next do
    begin
//      outputdebugstring(pchar('clipfmt:'+inttostr(integer(format))));
      Result := (ClipFormat = Format);
    end;
end;

procedure TEnumFormats.SetDataObject (Value : IDataObject);
var
  Result : integer;
begin
  FDataObject := nil;
  FDataObject := Value;
  if Assigned (FDataObject) then
  begin
    Result := FDataObject.EnumFormatEtc(ddGet, FEnumerator);
    Assert (Succeeded (Result), 'Cannot get the format enumerator');
    Reset;
  end
end;

function TEnumFormats.Handle (Tymed : Integer): HGlobal;
var
  FormatEtc : TFormatEtc;

begin
  Result := 0;
  if FValid and (FFormatEtc.Tymed and Tymed = Tymed) then
  begin
    FormatEtc := FFormatEtc;
    FormatEtc.tymed := FormatEtc.tymed and Tymed; // use only the requested type
    if Succeeded (FDataObject.GetData (FormatEtc, FMedium)) then
    begin
      FMediumValid := true;
      Result := FMedium.hGlobal
    end;
  end
end;

function TEnumFormats.GlobalHandle : HGlobal;
begin
  Result := Handle(tsGlobal)
end;

function TEnumFormats.GDIHandle: HGlobal;
begin
  Result := Handle(tsGDI);
end;

function TEnumFormats.SomeStream(Format : TClipFormat): TMemoryStream;
var
  H: hGlobal;
  p: PChar;
  pi: PInteger;
begin
  Result := nil;

  if HasFormat(Format) then
  begin
    H := GlobalHandle;
    if H <> 0 then
    begin
      p := GlobalLock(H);
      try
        pi := PInteger(p);
        p := p + 4;
        Result := TMemoryStream.Create;
        Result.SetSize(pi^);
        Move(p^, Result.Memory^, pi^);
      finally
        GlobalUnlock(H);
      end;
    end;
  end;
end;

function TEnumFormats.SomeText(Format : TClipFormat) : string;
var
  H : hGlobal;
  P: PChar;
begin
  Result := '';
  if HasFormat(Format) then
  begin
    H := GlobalHandle;
    if H <> 0 then
    begin
      P := GlobalLock(H);
      try
        Result := string(p);
      finally
        GlobalUnLock (H)
      end
    end
  end
end;

function TEnumFormats.SomeRTF(Format : TClipFormat) : string;
var
  H : hGlobal;
  P: PAnsiChar;
  sa: ansistring;

begin
  Result := '';
  if HasFormat(Format) then
  begin
    H := GlobalHandle;
    if H <> 0 then
    begin
      P := GlobalLock(H);
      try
        sa := ansistring(p);
        Result := string(sa);
      finally
        GlobalUnLock (H)
      end
    end
  end
end;

function TEnumFormats.Stream: TMemoryStream;
begin
  Result := SomeStream(CF_REFMTTXT);
end;

function TEnumFormats.SomeBMP(var bmp: TBitmap): boolean;
begin
  Result := false;
  if HasFormat(CF_BITMAP) then
  begin
    try
      bmp.Handle := CopyImage(
                         GDIHandle,
                         IMAGE_BITMAP,
                         0,
                         0,
                         LR_COPYRETURNORG
                         );
      Result := bmp.Handle <> 0;
    finally
      FreeMedium;
    end;
  end;
end;

function TEnumFormats.SomeFiles(var Files: TStringList): boolean;
var
  DropFiles: PDropFiles;
  Filename: PChar;
  s: string;
  H: hGlobal;

begin
  FFiles.Clear;

  H := GlobalHandle;
  if H <> 0 then
  begin
    DropFiles := PDropFiles(GlobalLock(H));
    try
      Filename := PChar(DropFiles) + (DropFiles^.pFiles div 2);

      while (Filename^ <> #0) do
      begin
        if (DropFiles^.fWide) then // -> NT4 compatability
        begin
          s := PChar(Filename);
          inc(Filename, (Length(s) + 1));
        end
        else
        begin
          s := StrPas(Filename);
          inc(Filename, Length(s) + 1);
        end;
        Files.Add(s);
      end;
    finally
      GlobalUnlock(H);
    end;
  end;

  Result := (Files.count > 0);
end;

function TEnumFormats.RTF: string;
begin
  Result := SomeRTF(CF_RTF);
end;

function TEnumFormats.Text: string;
begin
  Result := SomeText(CF_UNICODETEXT)
end;

function TEnumFormats.HasText: boolean;
begin
  Result := HasFormat(CF_UNICODETEXT)
end;

function TEnumFormats.HasRTF: boolean;
begin
  Result := HasFormat(CF_RTF);
end;

function TEnumFormats.HasBMP: boolean;
begin
  Result := HasFormat(CF_BITMAP);
end;

function TEnumFormats.HasRTE: boolean;
begin
  Result := HasFormat(CF_REFMTTXT);
end;

function TEnumFormats.HasFile: boolean;
begin
  Result := HasFormat(CF_HDROP)
end;

function TEnumFormats.HasURL: boolean;
begin
  Result := HasFormat(CF_URL);
end;

//------------------------------------------------------------------------------

constructor TRichEditorDropTarget.Create;
begin
  inherited Create;
  FAcceptText := true;
  FAcceptFiles := true;
  FAcceptURLs := true;
end;

function TRichEditorDropTarget.DragEnter(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  with TEnumFormats.Create(DataObj) do
  try
    FOk := (HasText and FAcceptText) or (HasFile and FAcceptFiles) or (HasRTF and RTFAware and FAcceptText)
      or (HasURL and FAcceptURLs) or HasRTE or HasBMP;

    FDropFormats := [];
    if HasText then FDropFormats := [dfText];
    if HasFile then FDropFormats := FDropFormats + [dfFile];
    if HasRTF then FDropFormats := FDropFormats + [dfRTF];
    if HasURL then FDropFormats := FDropFormats + [dfURL];
    if HasRTE then FDropFormats := FDropFormats + [dfRTE];
    if HasBMP then FDropFormats := FDropFormats + [dfBMP];
  finally
    Free
  end;

  if FOk then
    dwEffect := StandardEffect(KeysToShiftState (grfKeyState),dwEffect)
  else
    dwEffect := deNone;

  Result := NOERROR;

  DragMouseEnter;
end;

function TRichEditorDropTarget.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
var
  Allow: Boolean;
begin
  if FOk then
    dwEffect := StandardEffect(KeysToShiftState (grfKeyState),dwEffect)
  else
    dwEffect := deNone;

  Allow := dwEffect <> deNone;

  DragMouseMove(pt,Allow,FDropFormats);

  if not Allow then
    dwEffect := deNone;

  Result := NOERROR;
end;

function TRichEditorDropTarget.DragLeave: HResult;
begin
  Result := NOERROR;
  DragMouseLeave;
end;

function TRichEditorDropTarget.Drop(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
var
  bmp: TBitmap;
begin
  if FOk then
  begin
    dwEffect := StandardEffect(KeysToShiftState(grfKeyState), dwEffect);

    with TEnumFormats.Create(DataObj) do
    try
      if HasRTE then
        DropStream(pt, Stream, dwEffect)
      else
      if HasFile then
      begin
        SomeFiles(FFiles);
        DropFiles(pt, FFiles, dwEffect);
      end
      else
      if HasBMP then
      begin
        bmp := TBitmap.Create;
        try
          if SomeBmp(bmp) then
            DropBMP(pt, bmp, dwEffect);
        finally
          bmp.Free;
        end;
      end
      else
      if HasURL then
        DropURL(pt, Text, dwEffect)
      else
      if HasText then
        DropText(pt, Text, dwEffect);
    finally
      Free
    end;
  end
  else
    dwEffect := deNone;

  Result := NOERROR
end;

procedure TRichEditorDropTarget.DragMouseMove(pt: TPoint; var Allow: boolean; DropFormats:TDropFormats);
begin
end;

procedure TRichEditorDropTarget.DragMouseEnter;
begin
end;

procedure TRichEditorDropTarget.DragMouseLeave;
begin
end;

//------------------------------------------------------------------------------

function TSourceEnumFormatEtc.Next(CountRequested: Longint; out FormatEtcArray; PCountFetched: PLongint): HResult;
Var
  N: integer;
  FormatEtcArrayOut: TFormatEtcArray absolute FormatEtcArray;
  TotFmt: integer;
Begin
  Result := S_FALSE;

  TotFmt := 3;  // CF_TEXT, CF_UNICODETEXT, CF_RTF

  if FHasRTE then inc(TotFmt);

  N := 0;
  while (N < CountRequested) and (N + FIndex < TotFmt) do
  begin
    case N + Findex of
     0:begin
        formatetcArrayOut[0].cfFormat := CF_TEXT;
        formatetcArrayOut[0].ptd      := nil;
        formatetcArrayOut[0].dwAspect := DVASPECT_CONTENT;
        formatetcArrayOut[0].lindex   := -1;
        formatetcArrayOut[0].tymed    := TYMED_HGLOBAL;
      end;
    1:begin
        formatetcArrayOut[0].cfFormat := CF_UNICODETEXT;
        formatetcArrayOut[0].ptd      := nil;
        formatetcArrayOut[0].dwAspect := DVASPECT_CONTENT;
        formatetcArrayOut[0].lindex   := -1;
        formatetcArrayOut[0].tymed    := TYMED_HGLOBAL;
      end;
    2:begin
        formatetcArrayOut[0].cfFormat := CF_RTF;
        formatetcArrayOut[0].ptd      := nil;
        formatetcArrayOut[0].dwAspect := DVASPECT_CONTENT;
        formatetcArrayOut[0].lindex   := -1;
        formatetcArrayOut[0].tymed    := TYMED_HGLOBAL;
      end;
    end;
    if (N + FIndex = TotFmt - 1) and (FHasRTE) then
    begin
      formatetcArrayOut[0].cfFormat := CF_REFMTTXT;
      formatetcArrayOut[0].ptd      := nil;
      formatetcArrayOut[0].dwAspect := DVASPECT_CONTENT;
      formatetcArrayOut[0].lindex   := -1;
      formatetcArrayOut[0].tymed    := TYMED_HGLOBAL;
    end;
    Inc(FIndex);
    Inc(N);
  end;

  if PCountFetched <> nil then
    PCountFetched^ := N;
  if N = CountRequested then
    Result := S_OK;
end;

function TSourceEnumFormatEtc.Skip(Count: Longint) : HResult;
var
  TotFmt: integer;
Begin
  TotFmt := 3;

  if FHasRTE then inc(TotFmt);

  FIndex := FIndex + Count;
  if FIndex > TotFmt then
  begin
    FIndex := TotFmt;
    Result := S_FALSE;
  end
  else
    Result := S_OK;
end;

function TSourceEnumFormatEtc.Reset : HResult;
Begin
  FIndex := 0;
  Result := S_OK;
end;

function TSourceEnumFormatEtc.Clone (out enumFmt : IEnumFormatEtc) : HResult; stdcall;
Var
  MyFormatEtc: TSourceEnumFormatEtc;
Begin
  MyFormatEtc := TSourceEnumFormatEtc.Create;
  enumFmt := MyFormatEtc;
  Result := S_OK;
end;

constructor TSourceDataObject.Create(const stxt,srtf:string; ms: TMemoryStream);
begin
  inherited Create;
  textdata := stxt;
  rtfdata := srtf;
  memstr := ms;
end;

function TSourceDataObject.EnumFormatEtc(dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult;
var
  SourceEnumFormatEtc:TSourceEnumFormatEtc;
Begin
  if dwDirection <> DATADIR_GET then
  begin
    enumFormatEtc := nil;
    Result := E_NOTIMPL;
    Exit;
  end;

  SourceEnumFormatEtc := TSourceEnumFormatEtc.Create;

  if (memstr <> nil) then
    SourceEnumFormatEtc.FHasRTE := true;

  enumFormatEtc := SourceEnumFormatEtc;

  Result := S_OK;
end;

function TSourceDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
begin
  Result := DV_E_FORMATETC;
  if (formatetc.dwAspect = DVASPECT_CONTENT) and (formatetc.tymed = TYMED_HGLOBAL) then
  begin
    if (textdata <> '') then
      if ((formatetc.cfFormat = CF_TEXT) or
          (formatetc.cfFormat = CF_BITMAP) or
          (formatetc.cfFormat = CF_UNICODETEXT) or
         ((formatetc.cfFormat = CF_RTF) and RTFAware)) then
           Result := S_OK;

    if (memstr <> nil) then
    begin
      if (formatetc.cfFormat = CF_REFMTTXT) then
        Result := S_OK;
    end;
  end;
end;

function TSourceDataObject.GetData(const formatetc: TFormatEtc; out medium: TStgMedium): HResult;
Var
  HGlobalData: HGlobal;
  PGlobalData: PChar;
  PGlobalInt: PInteger;
  AllocLen: Integer;
  RTFLen : Integer;
  TXTLen : Integer;
  rtfansi: ansistring;

Label the_end;
Begin
  Result := DV_E_FORMATETC;
  medium.tymed := 0;
  medium.hGlobal := 0;
  medium.unkForRelease := nil;

  If (QueryGetData(formatetc) <> S_OK) then
    goto the_end;

  medium.tymed := TYMED_HGLOBAL;

  TXTLen := Length(textdata);

  RTFLen := Length(rtfdata);

  case formatetc.cfFormat of
  CF_TEXT:AllocLen := TXTLen + 1;
  CF_UNICODETEXT:AllocLen := TXTLen * 2 + 2;
  else AllocLen := 0;
  end;

  if (formatetc.cfFormat = CF_RTF) and (RTFLen > 0) then
    AllocLen := RTFLen + 1;

  if formatetc.cfFormat = CF_REFMTTXT then
  begin
    // stream size + actual stream
    AllocLen := memstr.Size + 4;
  end;

  HGlobalData := GlobalAlloc((GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT), AllocLen);

  if (HGlobalData <> 0) then
  begin
    PGlobalData := GlobalLock(HGlobalData); // lock while we are using it

    case formatetc.cfFormat of
    CF_TEXT:StrCopy(PGlobalData, PChar(textdata));
    CF_UNICODETEXT:StringToWideChar(textdata, PWideChar(PGlobalData), AllocLen+1);
    end;

    if (formatetc.cfFormat = CF_REFMTTXT) then
    begin
      pointer(PGlobalInt) := pointer(PGlobalData);
      PGlobalInt^ := memstr.Size;

      PGlobalData := PGlobalData + 4;

      Move(memstr.Memory^, PGlobalData^, memstr.Size);
    end;

    rtfansi := ansistring(rtfdata);

    if (formatetc.cfFormat = CF_RTF) then
    {$IFDEF DELPHIXE4_LVL}
      AnsiStrings.StrCopy(PAnsiChar(PGlobalData), PAnsiChar(rtfansi));
    {$ENDIF}
    {$IFNDEF DELPHIXE4_LVL}
      StrCopy(PAnsiChar(PGlobalData), PAnsiChar(rtfansi));
    {$ENDIF}

    GlobalUnlock (HGlobalData);
    medium.hGlobal := HGlobalData;
    Result := S_OK;
  end;
the_end:

end;

function TSourceDataObject.GetDataHere (const formatetc: TFormatEtc; out medium: TStgMedium): HResult;
Begin
  Result := DV_E_FORMATETC;
end;

function TSourceDataObject.SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult;
Begin
  Result := DV_E_FORMATETC;
end;

function TSourceDataObject.GetCanonicalFormatEtc(const formatetcIn: TFormatEtc;   out formatetcOut: TFormatEtc): HResult;
Begin
  formatetcOut.ptd := nil;
  Result := E_NOTIMPL;
end;

function TSourceDataObject.DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
Begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TSourceDataObject.DUnadvise(dwConnection: Longint): HResult; stdcall;
Begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TSourceDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
Begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

constructor TRichEditorDropSource.Create;
begin
 inherited Create;
end;

procedure TRichEditorDropSource.CurrentEffect(dwEffect: Longint);
begin
end;

procedure TRichEditorDropSource.DragDropStop;
begin

end;

procedure TRichEditorDropSource.QueryDrag;
begin

end;

//------------------------------------------------------------------------------

function TRichEditorDropSource.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult;
Begin
  Result := S_OK;

  if fEscapePressed then
    DragDropStop;

  if fEscapePressed then
    Result := DRAGDROP_S_CANCEL
  else
    if ((grfKeyState and MK_LBUTTON) = 0) then // mouse-up
    begin
      Result := DRAGDROP_S_DROP;
      DragDropStop;
    end;
  QueryDrag;
end;

//------------------------------------------------------------------------------

function TRichEditorDropSource.GiveFeedback(dwEffect: Longint): HResult;
Begin
  FNoAccept := dwEffect = DROPEFFECT_NONE;
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
  CurrentEffect(dwEffect);
end;

//------------------------------------------------------------------------------

function StartTextDoDragDrop(DropSource:TRichEditorDropSource;stxt,srtf:string;ms: TMemoryStream; dwOKEffects: Longint; var dwEffect: Longint): HResult;
Var
  DataObject   : TSourceDataObject;
  MyIDropSource: IDropSource;     // Ixxx versions to generate references
  MyIDataObject: IDataObject;

Begin
  MyIDropSource  := DropSource;
  DataObject     := TSourceDataObject.Create(stxt, srtf, ms);
  MyIDataObject  := DataObject;

  Result := ActiveX.DoDragDrop(MyIDataObject, MyIDropSource, dwOKEffects, dwEffect);

  if DropSource.FNoAccept then
    Result := DRAGDROP_S_CANCEL;
end;


function TEnumFormats.GetAspect: integer;
begin
  Result := FFormatEtc.dwAspect;
end;

function TEnumFormats.GetcfFormat: TClipFormat;
begin
  Result := FFormatEtc.cfFormat;
end;

function TEnumFormats.GetlIndex: integer;
begin
  Result := FFormatEtc.lIndex;
end;

function TEnumFormats.GetTymed: integer;
begin
  Result := FFormatEtc.Tymed;
end;

procedure TEnumFormats.SetAspect(Value: Integer);
begin
  FFormatEtc.dwAspect := Value;
end;

procedure TEnumFormats.SetcfFormat(Value: TClipFormat);
begin
  FFormatEtc.cfFormat := Value;
end;

procedure TEnumFormats.SetlIndex(Value: Integer);
begin
  FFormatEtc.lIndex := Value;
end;

procedure TEnumFormats.SetTymed(value: Integer);
begin
 FFormatEtc.Tymed := Value;
end;

initialization
  RTFAware := True;
  CF_REFMTTXT := RegisterClipboardFormat('RichEditorText');
  CF_RTF := RegisterClipboardformat('Rich Text Format');
  CF_HTML := RegisterClipboardformat('HTML Format');

end.
