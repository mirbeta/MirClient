{************************************************************************}
{ Drag & drop interface support file                                     }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{            copyright © 1996-2005                                       }
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

unit InspDD;

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
  Windows, Messages, ActiveX, StdCtrls, Classes, Controls, Sysutils,
  Forms, ShlObj;

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
    procedure SetDataObject (Value : IDataObject);
    function SomeText(Format : TClipFormat) : string;
    function SomeURL(Format : TClipFormat) : string;
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
    function Handle (Tymed : integer): HGlobal;
    function GlobalHandle : HGlobal;
    function HasText: Boolean;
    function HasFile: Boolean;
    function HasURL: Boolean;
    function Text : string;
    function URL: string;
    property Count : integer read FCount;
    property DataObject : IDataObject read FDataObject write SetDataObject;
    property Valid : boolean read FValid;
    property FormatEtc : TFormatEtc read FFormatEtc;
    property Aspect : integer read GetAspect write SetAspect;
    property Format : TClipFormat read GetcfFormat write SetcfFormat;
    property Index : integer read GetlIndex write SetlIndex;
    property Medium : integer read GetTymed write SetTymed;
  end;

  TDropFormat = (dfText,dfFile,dfURL);
  TDropFormats = set of TDropFormat;

  TInspectorDropTarget = class (TInterfacedObject, IDropTarget)
    function DragEnter (const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragOver (grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragLeave : HResult; stdcall;
    function Drop (const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
  private
    FOk: Boolean;
    FAcceptText: Boolean;
    FAcceptFiles: Boolean;
    FAcceptURL: Boolean;
    FDropFormats: TDropFormats;
  public
    constructor Create;
    procedure DropText(pt:TPoint;s:string); virtual;
    procedure DropURL(pt:TPoint;s:string); virtual;
    procedure DropFiles(pt:TPoint;files:tstrings); virtual;
    procedure DragMouseMove(pt:TPoint;var Allow:boolean; DropFormats: TDropFormats); virtual;
    procedure DragMouseEnter; virtual;
    procedure DragMouseLeave; virtual;

    property AcceptText: Boolean read FAcceptText write FAcceptText;
    property AcceptFiles: Boolean read FAcceptFiles write FAcceptFiles;
    property AcceptURL: Boolean read FAcceptURL write FAcceptURL;
    property DropFormats:TDropFormats read FDropFormats;
  end;

  TInspectorDropSource = class (TInterfacedObject, IDropSource)
  private
    FNoAccept:boolean;
  public
    constructor Create;
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
    procedure CurrentEffect(dwEffect: Longint); virtual;
    procedure QueryDrag; virtual;
  end;

  TSourceDataObject = class (TInterfacedObject, IDataObject)
  private
    TextData:string;
  public
    constructor Create(const stxt,srtf:string;sidx:integer);
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
  private
  protected
    FIndex : integer; // Next FormatEtc to get
    FHasCol: boolean;
  public
    function Next (CountRequested: Longint; out FormatEtcArray; PCountFetched: PLongint): HResult; stdcall;
    function Skip (count: Longint) : HResult; stdcall;
    function Reset : HResult; stdcall;
    function Clone (out enumFmt : IEnumFormatEtc) : HResult; stdcall;
  end;

  TFormatEtcArray = array[0..19] of TFormatEtc;
  PFormatEtcArray = ^TFormatEtcArray;

function StandardEffect(Keys: TShiftState) : integer;
function StartTextDoDragDrop(DropSource:TInspectorDropSource;stxt,srtf:string;dwOKEffects: Longint; var dwEffect: Longint): HResult;
function StartColDoDragDrop(DropSource:TInspectorDropSource;column:integer;dwOKEffects: Longint; var dwEffect: Longint): HResult;

implementation

var
  CF_URL : DWord;

function StandardEffect (Keys : TShiftState) : integer;
begin
  Result := deMove;
  if ssCtrl in Keys then Result := deCopy
end;

constructor TEnumFormats.Create (DataObject : IDataObject);
begin
  inherited Create;
  SetDataObject(DataObject);
  FFiles := TStringList.Create;
end;

destructor TEnumFormats.Destroy;
begin
  SetDataObject(nil);
  FFiles.Free;
  inherited Destroy
end;

function TEnumFormats.Next: Boolean;
var
  Returned : Integer;
begin
  inc(FCount);
  FValid := FEnumerator.Next(1, FFormatEtc, @Returned) = S_OK;
  Result := FValid
end;

function TEnumFormats.Reset: Boolean;
begin
  FValid := false;
  FCount := 0;
  Result := Succeeded(FEnumerator.Reset)
end;

function TEnumFormats.HasFormat(ClipFormat : TClipFormat): Boolean;
begin
  Result := False;
  if Reset then
    while (not Result) and Next do
      Result := (ClipFormat = Format);
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

function TEnumFormats.Handle (Tymed : Integer): HGlobal;
var
  FormatEtc : TFormatEtc;
  Medium : TStgMedium;
begin
  Result := 0;
  if FValid and (FFormatEtc.Tymed and Tymed = Tymed) then
  begin
    FormatEtc := FFormatEtc;
    FormatEtc.tymed := FormatEtc.tymed and Tymed; // use only the requested type
    if Succeeded (FDataObject.GetData (FormatEtc, Medium)) then
      Result := Medium.hGlobal
  end
end;

function TEnumFormats.GlobalHandle : HGlobal;
begin
  Result := Handle(tsGlobal)
end;

function TEnumFormats.SomeText(Format : TClipFormat) : string;
var
  H : hGlobal;
  P : PChar;
begin
  Result := '';
  if HasFormat(Format) then
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

function TEnumFormats.SomeURL(Format : TClipFormat): string;
var
  H : hGlobal;
  P : PChar;
begin
  Result := '';
  if HasFormat(Format) then
  begin
    H := GlobalHandle;
    if H <> 0 then
    begin
      P := GlobalLock (H);
      try
        Result := P;
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
     Filename := PChar(DropFiles) + DropFiles^.pFiles;
     while (Filename^ <> #0) do
     begin
       if (DropFiles^.fWide) then // -> NT4 compatability
       begin
         s := WideCharToString(PWideChar(Filename));
          inc(Filename, (Length(s) + 1) * 2);
        end else
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

  if (Files.Count > 0) then
    Result := True
  else
    Result := False;
end;

function TEnumFormats.Text: string;
begin
  Result := SomeText(CF_TEXT);
end;

function TEnumFormats.URL: string;
begin
  Result := SomeURL(CF_URL);
end;

function TEnumFormats.HasText: Boolean;
begin
  Result := HasFormat(CF_TEXT);
end;

function TEnumFormats.HasFile: Boolean;
begin
  Result := HasFormat(CF_HDROP);
end;

function TEnumFormats.HasURL: Boolean;
begin
  Result := HasFormat(CF_URL);
end;

constructor TInspectorDropTarget.Create;
begin
  inherited Create;
  FAcceptText := True;
  FAcceptFiles := True;
  FAcceptURL := True;
end;

function TInspectorDropTarget.DragEnter (const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  with TEnumFormats.Create(DataObj) do
  try
    FOk := (HasText and FAcceptText) or (HasFile and FAcceptFiles) or (HasURL and FAcceptURL);

    FDropFormats := [];
    if HasText then FDropFormats := [dfText];
    if HasFile then FDropFormats := FDropFormats + [dfFile];
    if HasURL then FDropFormats := FDropFormats + [dfURL];
  finally
    Free
  end;

  if FOk then
    if dfURL in FDropFormats then
      dwEffect := deLink
    else
      dwEffect := StandardEffect(KeysToShiftState (grfKeyState))
  else
    dwEffect := deNone;

  Result := NOERROR;

  DragMouseEnter;
end;

function TInspectorDropTarget.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
var
  Allow: Boolean;
begin
  if FOk then
    if dfURL in FDropFormats then
      dwEffect := deLink
    else
      dwEffect := StandardEffect(KeysToShiftState (grfKeyState))
  else
    dwEffect := deNone;

  Allow := dwEffect <> deNone;

  DragMouseMove(pt,Allow,FDropFormats);

  if not Allow then
    dwEffect := deNone;

  Result := NOERROR;
end;

function TInspectorDropTarget.DragLeave: HResult;
begin
  Result := NOERROR;
  DragMouseLeave;
end;

function TInspectorDropTarget.Drop(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  if FOk then
  begin
    with TEnumFormats.Create(DataObj) do
    try
     if HasURL then DropURL(pt,URL);
     if HasText then DropText(pt,Text);
     if HasFile then
       begin
        SomeFiles(FFiles);
        DropFiles(pt,FFiles);
       end;
    finally
      Free
    end;

    dwEffect := StandardEffect (KeysToShiftState (grfKeyState))
  end else
    dwEffect := deNone;

  Result := NOERROR
end;


procedure TInspectorDropTarget.DropText(pt: TPoint;s: string);
begin
end;

procedure TInspectorDropTarget.DropFiles(pt: TPoint; files:TStrings);
begin
end;

procedure TInspectorDropTarget.DropURL(pt: TPoint;s: string);
begin
end;

procedure TInspectorDropTarget.DragMouseMove(pt: TPoint; var Allow: boolean; DropFormats:TDropFormats);
begin
end;

procedure TInspectorDropTarget.DragMouseEnter;
begin
end;

procedure TInspectorDropTarget.DragMouseLeave;
begin
end;


function TSourceEnumFormatEtc.Next(CountRequested: Longint; out FormatEtcArray; PCountFetched: PLongint): HResult;
Var
  N: integer;
  FormatEtcArrayOut: TFormatEtcArray absolute FormatEtcArray;
  TotFmt: integer;
//Label the_end;
Begin
  Result := S_FALSE;

  if FHasCol then TotFmt:=4 else TotFmt:=3;

  N := 0;
  while (N < CountRequested) and (N+FIndex < TotFmt) do
  Begin
    Case N + Findex of
     0:Begin
        formatetcArrayOut[0].cfFormat := CF_TEXT;
        formatetcArrayOut[0].ptd      := nil;
        formatetcArrayOut[0].dwAspect := DVASPECT_CONTENT;
        formatetcArrayOut[0].lindex   := -1;
        formatetcArrayOut[0].tymed    := TYMED_HGLOBAL;
      end;
    1:Begin
        formatetcArrayOut[0].cfFormat := CF_UNICODETEXT;
        formatetcArrayOut[0].ptd      := nil;
        formatetcArrayOut[0].dwAspect := DVASPECT_CONTENT;
        formatetcArrayOut[0].lindex   := -1;
        formatetcArrayOut[0].tymed    := TYMED_HGLOBAL;
      end;
    end;
    Inc(FIndex);
    Inc(N);
  end;
  If PCountFetched <> nil then PCountFetched^ := N;
  if N = CountRequested then Result := S_OK;
end;

function TSourceEnumFormatEtc.Skip(Count: Longint) : HResult;
var
 TotFmt: integer;
Begin
  if FHasCol then TotFmt:=4 else TotFmt:=3;

  FIndex := FIndex + Count;
  If FIndex > TotFmt then
    Begin
      FIndex := TotFmt;
      Result := S_FALSE;
    end
  else Result := S_OK;
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

constructor TSourceDataObject.Create(const stxt,srtf:string;sidx:integer);
begin
  inherited Create;
  TextData := stxt;
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
  enumFormatEtc := SourceEnumFormatEtc;
  Result := S_OK;
end;

function TSourceDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
Begin
  Result := DV_E_FORMATETC;
  if (formatetc.dwAspect = DVASPECT_CONTENT) and (formatetc.tymed = TYMED_HGLOBAL) then
  begin
    if (TextData <> '') then
      if ((formatetc.cfFormat = CF_TEXT) or
          (formatetc.cfFormat = CF_UNICODETEXT))
         then Result := S_OK;
  end;
end;

function TSourceDataObject.GetData(const formatetc: TFormatEtc; out medium: TStgMedium): HResult;
var
  HGlobalData: HGlobal;
  PGlobalData: PChar;
  AllocLen: Integer;
  TXTLen : Integer;

label the_end;
begin
  Result := DV_E_FORMATETC;
  medium.tymed := 0;
  medium.hGlobal := 0;
  medium.unkForRelease := nil;

  if (QueryGetData(formatetc)<>S_OK) then goto the_end;

  medium.tymed:=TYMED_HGLOBAL;

  TXTLen := Length(textdata);

  case formatetc.cfFormat of
  CF_TEXT:AllocLen:=TXTLen+1;
  CF_UNICODETEXT:AllocLen:=TXTLen*2+2;
  else AllocLen:=0;
  end;

  HGlobalData:=GlobalAlloc((GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT), AllocLen);
  if (HGlobalData<>0) then
  begin
    PGlobalData := GlobalLock (HGlobalData); // lock while we are using it

    case formatetc.cfFormat of
    CF_TEXT:StrCopy(PGlobalData, PChar(textdata));
    CF_UNICODETEXT:StringToWideChar(textdata, PWideChar(PGlobalData), AllocLen+1);
    end;

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

constructor TInspectorDropSource.Create;
begin
 inherited Create;
end;

procedure TInspectorDropSource.CurrentEffect(dwEffect: Longint);
begin
end;

procedure TInspectorDropSource.QueryDrag;
begin
end;


function TInspectorDropSource.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult;
//-------------------------------------------------------
Begin
  Result := S_OK;
  If fEscapePressed then Result := DRAGDROP_S_CANCEL else
    if ((grfKeyState and MK_LBUTTON) = 0) then // mouse-up
      Result := DRAGDROP_S_DROP;

 QueryDrag;
end;

function TInspectorDropSource.GiveFeedback(dwEffect: Longint): HResult;
Begin
  FNoAccept := dwEffect = DROPEFFECT_NONE;
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
  CurrentEffect(dwEffect);
end;

function StartTextDoDragDrop(DropSource:TInspectorDropSource;stxt,srtf:string;dwOKEffects: Longint; var dwEffect: Longint): HResult;
Var
  DataObject   : TSourceDataObject;
  MyIDropSource: IDropSource;     // Ixxx versions to generate references
  MyIDataObject: IDataObject;

Begin
  MyIDropSource  := DropSource;
  DataObject     := TSourceDataObject.Create(stxt,srtf,-1);
  MyIDataObject  := DataObject;

  Result := ActiveX.DoDragDrop(MyIDataObject, MyIDropSource, dwOKEffects, dwEffect);

  if DropSource.FNoAccept then
    Result := DRAGDROP_S_CANCEL;
end;

function StartColDoDragDrop(DropSource:TInspectorDropSource;Column:integer;dwOKEffects: Longint; var dwEffect: Longint): HResult;
Var
  DataObject   : TSourceDataObject;
  MyIDropSource: IDropSource;     // Ixxx versions to generate references
  MyIDataObject: IDataObject;

Begin
  MyIDropSource  := DropSource;
  DataObject     := TSourceDataObject.Create('','',Column);
  MyIDataObject  := DataObject;

  Result := ActiveX.DoDragDrop(MyIDataObject, MyIDropSource, dwOKEffects, dwEffect);

  if DropSource.fNoAccept then Result:=DRAGDROP_S_CANCEL;
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

 CF_URL := RegisterClipboardFormat('UniformResourceLocator');
end.
