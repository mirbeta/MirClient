{*************************************************************************}
{ Drag'n'drop interface support file                                      }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 1996-2013                                        }
{            Email : info@tmssoftware.com                                 }
{            Web : http://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvEddd;

{$H+}

{$I TMSDEFS.INC}
{$H+}

{$IFDEF VER125}
 {$HPPEMIT '#include <oleidl.h>'}
{$ENDIF}

{$IFDEF VER130}
 {$HPPEMIT '#include <oleidl.h>'}
{$ENDIF}

{$IFDEF VER140}
  {$IFDEF BCB}
  {$HPPEMIT '#include <oleidl.h>'}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER180}
  {$HPPEMIT '#include <oleidl.h>'}
{$ENDIF}

interface

uses
  Windows, Messages, ActiveX, StdCtrls, Classes,
  Controls, SysUtils, Forms, ShlObj;

type
  TEnumFormats = class
  private
    FDataObject : IDataObject;
    FEnumerator : IEnumFormatEtc;
    FFormatEtc  : TFormatEtc;
    FValid      : boolean;
    FCount      : integer;
    FFiles      : TStringList;
    procedure SetDataObject (Value : IDataObject);
    function SomeText (Format : TClipFormat) : string;
    function SomeFiles(var Files:TStringList):boolean;
    function GetAspect:integer;
    procedure SetAspect(value:integer);
    function GetcfFormat:TClipFormat;
    procedure SetcfFormat(value:TClipFormat);
    function GetlIndex:integer;
    procedure SetlIndex(value:integer);
    function GetTymed:integer;
    procedure SetTymed(value:integer);
  public
    constructor Create (DataObject : IDataObject);
    destructor Destroy; override;
    function Reset : boolean;
    function Next : boolean;
    function HasFormat (ClipFormat : TClipFormat) : boolean;
    function Handle (Tymed : integer): hGlobal;
    function GlobalHandle : hGlobal;
    function HasText : boolean;
    function HasFile : boolean;
    function HasRTF : boolean;
    function Text : string;
    function RTF : string;
    property Count : integer read FCount;
    property DataObject : IDataObject read FDataObject write SetDataObject;
    property Valid : boolean read FValid;
    property FormatEtc : TFormatEtc read FFormatEtc;
    property Aspect : integer read GetAspect write SetAspect;
    property Format : TClipFormat read GetcfFormat write SetcfFormat;
    property Index : integer read GetlIndex write SetlIndex;
    property Medium : integer read GetTymed write SetTymed;
  end;

  TAEDropTarget = class (TInterfacedObject, IDropTarget)
    function DragEnter (const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragOver (grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragLeave : HResult; stdcall;
    function Drop (const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
  private
    FOk : boolean;
  public
    constructor Create;
    procedure DropText(pt:TPoint;s:string); virtual;
    procedure DropRTF(pt:TPoint;s:string); virtual;
    procedure DropFiles(pt:TPoint;Files:TStrings); virtual;
    procedure DragMouseMove(pt:TPoint;var Allow:boolean); virtual;
  end;

  TAEDropSource = class (TInterfacedObject, IDropSource)
  private
    FNoAccept:boolean;
  public
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
  end;

  TSourceDataObject = class (TInterfacedObject, IDataObject)
  private
    textdata:string;
    rtfdata:string;
  public
    constructor Create(const stxt,srtf:string);
  public
    function GetData(const formatetc: TFormatEtc; out medium: TStgMedium): HResult; stdcall;
    function GetDataHere (const formatetc: TFormatEtc; out medium: TStgMedium): HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult; stdcall;
    function GetCanonicalFormatEtc(const formatetcIn: TFormatEtc;   out formatetcOut: TFormatEtc): HResult; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
  end;

  TSourceEnumFormatEtc = class (TInterfacedObject, IEnumFormatEtc)
  protected
    FIndex : integer; // Next FormatEtc to get
  public
    function Next (CountRequested: Longint; out FormatEtcArray; PCountFetched: PLongint): HResult; stdcall;
    function Skip (count: Longint) : HResult; stdcall;
    function Reset : HResult; stdcall;
    function Clone (out enumFmt : IEnumFormatEtc) : HResult; stdcall;
  end;

  TFormatEtcArray = array[0..19] of TFormatEtc;
  PFormatEtcArray = ^TFormatEtcArray;


function StandardEffect (Keys : TShiftState) : integer;
function StartTextDoDragDrop(stxt,srtf:string;dwOKEffects: Longint; var dwEffect: Longint): HResult;
procedure SetRTFAware(value:boolean);
function ScaleFromSmallFontsDimension(const X: Integer): Integer;

implementation

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

  SmallFontsPixelsPerInch = 96;


var
  CF_RTF : integer;
  RTFAware:boolean;

function ScaleFromSmallFontsDimension(const X: Integer): Integer;
begin
  Result := MulDiv(X, Screen.PixelsPerInch, SmallFontsPixelsPerInch);
end;

procedure SetRTFAware(value:boolean);
begin
  RTFAware := Value;
end;

function StandardEffect (Keys : TShiftState) : integer;
begin
  Result := deMove;
  if ssCtrl in Keys then
    Result := deCopy
end;

constructor TEnumFormats.Create (DataObject : IDataObject);
begin
  inherited Create;
  SetDataObject (DataObject);
  FFiles:=TStringList.Create;

  if OpenClipBoard(0) then
    begin
     CF_RTF := RegisterClipboardformat('Rich Text Format');
     CloseClipBoard;
    end;
end;

destructor TEnumFormats.Destroy;
begin
  SetDataObject (nil);
  FFiles.Free;
  inherited Destroy
end;

function TEnumFormats.Next : boolean;
var
  Returned : integer;
begin
  inc (FCount);
  FValid := FEnumerator.Next (1, FFormatEtc, @Returned) = S_OK;
  Result := FValid
end;

function TEnumFormats.Reset : boolean;
begin
  FValid := false;
  FCount := 0;
  Result := Succeeded (FEnumerator.Reset)
end;

function TEnumFormats.HasFormat (ClipFormat : TClipFormat) : boolean;
begin
 Result:=false;
 if Reset then
   while (not Result) and Next do
      Result:=(ClipFormat=Format);
end;

procedure TEnumFormats.SetDataObject (Value : IDataObject);
var
  Result : integer;
begin
  FDataObject := nil;
  FDataObject := Value;
  if Assigned (FDataObject) then
  begin
    Result := FDataObject.EnumFormatEtc (ddGet, FEnumerator);
    Assert (Succeeded (Result), 'Cannot get the format enumerator');
    Reset
  end
end;

function TEnumFormats.Handle (Tymed : integer): hGlobal;
var
  FormatEtc : TFormatEtc;
  Medium : TStgMedium;
begin
  Result := 0;
  if FValid and (FFormatEtc.tymed and Tymed = Tymed) then
  begin
    FormatEtc := FFormatEtc;
    FormatEtc.tymed := FormatEtc.tymed and Tymed; // use only the requested type
    if Succeeded (FDataObject.GetData (FormatEtc, Medium)) then
      Result := Medium.hGlobal
  end
end;

function TEnumFormats.GlobalHandle : hGlobal;
begin
  Result := Handle (tsGlobal)
end;

function TEnumFormats.SomeText (Format : TClipFormat) : string;
var
  H : hGlobal;
  P : PChar;
begin
  Result := '';
  if HasFormat (Format) then
  begin
    H := GlobalHandle;
    if H <> 0 then
    begin
      P := GlobalLock (H);
      try
        Result := P
      finally
        GlobalUnLock (H)
      end
    end
  end
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
      {$IFNDEF DELPHI_UNICODE}
      Filename := PChar(DropFiles) + DropFiles^.pFiles;
      {$ENDIF}
      {$IFDEF DELPHI_UNICODE}
      Filename := PChar(DropFiles) + (DropFiles^.pFiles div 2);
      {$ENDIF}

      while (Filename^ <> #0) do
      begin
        if (DropFiles^.fWide) then // -> NT4 compatability
        begin
          {$IFNDEF DELPHI_UNICODE}
          s := WideCharToString(PWideChar(Filename));
          inc(Filename, (Length(s) + 1) * 2);
          {$ENDIF}
          {$IFDEF DELPHI_UNICODE}
          s := PChar(Filename);
          inc(Filename, (Length(s) + 1));
          {$ENDIF}
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

  Result := Files.count > 0;
end;

function TEnumFormats.RTF : string;
begin
  Result := SomeText(CF_RTF);
end;

function TEnumFormats.Text : string;
begin
  {$IFDEF DELPHI_UNICODE}
  Result := SomeText(CF_UNICODETEXT)
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  Result := SomeText(CF_TEXT)
  {$ENDIF}
end;

function TEnumFormats.HasText : boolean;
begin
  {$IFDEF DELPHI_UNICODE}
  Result := HasFormat(CF_UNICODETEXT)
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  Result := HasFormat(CF_TEXT)
  {$ENDIF}
end;

function TEnumFormats.HasRTF : boolean;
begin
  Result := HasFormat(CF_RTF);
end;

function TEnumFormats.HasFile : boolean;
begin
  Result := HasFormat(CF_HDROP)
end;

constructor TAEDropTarget.Create;
begin
  inherited Create;
end;

function TAEDropTarget.DragEnter(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  with TEnumFormats.Create (DataObj) do
  try
    FOk := HasText or HasFile or (HasRTF and RTFAware);
  finally
    Free
  end;

  if FOk then
    dwEffect := StandardEffect (KeysToShiftState (grfKeyState))
  else
    dwEffect := deNone;

  Result := NOERROR;
end;

function TAEDropTarget.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
var
  Allow:boolean;
begin
  if FOk then
    dwEffect := StandardEffect (KeysToShiftState (grfKeyState))
  else
    dwEffect := deNone;

  Allow := true;
  DragMouseMove(pt,allow);
  if not Allow then
    dwEffect := deNone;

  Result := NOERROR;
end;

function TAEDropTarget.DragLeave: HResult;
begin
  Result := NOERROR
end;

function TAEDropTarget.Drop(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  if FOk then
  begin
    with TEnumFormats.Create(DataObj) do
    try
      if HasRTF and RTFAware then
        DropRTF(pt,RTF)
      else
      if HasText then
        DropText(pt,Text);
      if HasFile then
      begin
        SomeFiles(fFiles);
        DropFiles(pt,fFiles);
      end;
    finally
      Free
    end;

    dwEffect := StandardEffect (KeysToShiftState (grfKeyState))
  end
  else
    dwEffect := deNone;

  Result := NOERROR
end;

procedure TAEDropTarget.DropText(pt:TPoint;s:string);
begin
end;

procedure TAEDropTarget.DropRTF(pt:TPoint;s:string);
begin
end;

procedure TAEDropTarget.DropFiles(pt:TPoint; Files:TStrings);
begin
end;

procedure TAEDropTarget.DragMouseMove(pt: TPoint; var Allow: boolean);
begin
  Allow := true;
end;

function TSourceEnumFormatEtc.Next (CountRequested: Longint; out FormatEtcArray; PCountFetched: PLongint): HResult;
Var
  N: integer;
  FormatEtcArrayOut: TFormatEtcArray absolute FormatEtcArray;
Label the_end;
Begin
  Result := S_FALSE;

  N := 0;
  while (N < CountRequested) and (N+FIndex < 3) do
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
    Inc(FIndex);
    Inc(N);
  end;

  if PCountFetched <> nil then
    PCountFetched^ := N;

  if N = CountRequested then
    Result := S_OK;
end;

function TSourceEnumFormatEtc.Skip (count: Longint) : HResult;
begin
  FIndex := FIndex + Count;
  if FIndex > 3 then {changed from 2 to 3}
  begin
    FIndex := 3;
    Result := S_FALSE;
  end
  else
    Result := S_OK;
end;

function TSourceEnumFormatEtc.Reset : HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TSourceEnumFormatEtc.Clone (out enumFmt : IEnumFormatEtc) : HResult; stdcall;
var
  MyFormatEtc: TSourceEnumFormatEtc;
begin
  MyFormatEtc := TSourceEnumFormatEtc.Create;
  enumFmt := MyFormatEtc;
  Result := S_OK;
end;

constructor TSourceDataObject.Create(const stxt,srtf:string);
begin
  inherited Create;
  TextData := stxt;
  RTFData := srtf;
end;

function TSourceDataObject.EnumFormatEtc(dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult;
begin
  if dwDirection <> DATADIR_GET then
  begin
    enumFormatEtc := nil;
    Result := E_NOTIMPL;
    exit;
  end;
  enumFormatEtc := TSourceEnumFormatEtc.Create;
  Result := S_OK;
end;


function TSourceDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
Begin
  Result := DV_E_FORMATETC;
  if (formatetc.dwAspect = DVASPECT_CONTENT) and
     ((formatetc.cfFormat = CF_TEXT) or (formatetc.cfFormat = CF_UNICODETEXT) or ((formatetc.cfFormat = CF_RTF) and RTFAware)) and
     (formatetc.tymed = TYMED_HGLOBAL) then Result := S_OK;
end;

function TSourceDataObject.GetData(const formatetc: TFormatEtc; out medium: TStgMedium): HResult;
var
  HGlobalData: HGlobal;
  PGlobalData: PChar;
  AllocLen: integer;
  RTFLen : integer;
  TXTLen : integer;
begin
  Result := DV_E_FORMATETC;
  medium.tymed := 0;
  medium.hGlobal := 0;
  medium.unkForRelease := nil;

  If (QueryGetData(formatetc)<>S_OK) then
    Exit;

  medium.tymed := TYMED_HGLOBAL;

  TXTLen := Length(TextData);
  RTFLen := Length(RTFData);

  case formatetc.cfFormat of
  CF_TEXT:AllocLen := TXTLen + 1;
  CF_UNICODETEXT:AllocLen := TXTLen * 2 + 2;
  else
    AllocLen := 0;
  end;

  if (formatetc.cfFormat = CF_RTF) then
    AllocLen := RTFLen;

  HGlobalData := GlobalAlloc((GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT), AllocLen);

  if (HGlobalData <> 0) then
  begin
    PGlobalData := GlobalLock(HGlobalData); // lock while we are using it
    case formatetc.cfFormat of
    CF_TEXT:StrCopy(PGlobalData, PChar(textdata));
    CF_UNICODETEXT:StringToWideChar(textdata, PWideChar(PGlobalData), AllocLen+1);
    end;

    if (formatetc.cfFormat = CF_RTF) then StrCopy(PGlobalData, PChar(rtfdata));

    GlobalUnlock (HGlobalData);
    medium.hGlobal := HGlobalData;
    Result := S_OK;
  end;
end;

function TSourceDataObject.GetDataHere (const formatetc: TFormatEtc; out medium: TStgMedium): HResult;
begin
  Result := DV_E_FORMATETC;
end;

function TSourceDataObject.SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult;
begin
  Result := DV_E_FORMATETC;
end;

function TSourceDataObject.GetCanonicalFormatEtc(const formatetcIn: TFormatEtc;   out formatetcOut: TFormatEtc): HResult;
begin
  formatetcOut.ptd := nil;
  Result := E_NOTIMPL;
end;

function TSourceDataObject.DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TSourceDataObject.DUnadvise(dwConnection: Longint): HResult; stdcall;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TSourceDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

//------------------------------------------------------------------------------

function TAEDropSource.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult;
begin
  Result := S_OK;
  If fEscapePressed then Result := DRAGDROP_S_CANCEL else
    if ((grfKeyState and MK_LBUTTON) = 0) then // mouse-up
      Result := DRAGDROP_S_DROP;
end;

function TAEDropSource.GiveFeedback(dwEffect: Longint): HResult;
begin
  fNoAccept := dwEffect = DROPEFFECT_NONE;
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

function StartTextDoDragDrop(stxt,srtf:string;dwOKEffects: Longint; var dwEffect: Longint): HResult;
var
  DropSource   : TAEDropSource;     // Pascal versions for debugging purposes
  DataObject   : TSourceDataObject;
  MyIDropSource: IDropSource;     // Ixxx versions to generate references
  MyIDataObject: IDataObject;

begin
  DropSource := TAEDropSource.Create;
  MyIDropSource := DropSource;

  DataObject := TSourceDataObject.Create(stxt,srtf);
  MyIDataObject := DataObject;

  Result := Activex.DoDragDrop(MyIDataObject, MyIDropSource, dwOKEffects, dwEffect);

  if DropSource.fNoAccept then
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

procedure TEnumFormats.SetAspect(value: integer);
begin
  FFormatEtc.dwAspect := Value;
end;

procedure TEnumFormats.SetcfFormat(value: TClipFormat);
begin
  FFormatEtc.cfFormat := Value;
end;

procedure TEnumFormats.SetlIndex(value: integer);
begin
  FFormatEtc.lIndex := Value;
end;

procedure TEnumFormats.SetTymed(value: integer);
begin
  FFormatEtc.Tymed := Value;
end;

initialization
  RTFAware := false;
  CF_RTF := 0;
end.
