{************************************************************************}
{ Drag & drop interface support file                                     }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{            copyright © 20010                                           }
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

unit acldd;

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

{$IFDEF VER200}
  {$IFDEF BCB}
  {$HPPEMIT '#include <oleidl.h>'}
  {$ENDIF}
{$ENDIF}

interface

uses
  Windows, Messages, ActiveX, StdCtrls, Classes, Controls, Sysutils,
  Forms, ShlObj;

const
  deNone   = DROPEFFECT_NONE;
  deMove   = DROPEFFECT_MOVE;
  deCopy   = DROPEFFECT_COPY;
  ddGet = DATADIR_GET;
  ddSet = DATADIR_SET;

type
  TEnumFormats = class
  private
    FDataObject : IDataObject;
    FEnumerator : IEnumFormatEtc;
    FFormatEtc  : TFormatEtc;
    FValid      : boolean;
    FCount      : integer;
    procedure SetDataObject (Value : IDataObject);
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
    function Handle (Tymed : integer): HGlobal;
    property Count : integer read FCount;
    property DataObject : IDataObject read FDataObject write SetDataObject;
    property Valid : boolean read FValid;
    property FormatEtc : TFormatEtc read FFormatEtc;
    property Aspect : integer read GetAspect write SetAspect;
    property Format : TClipFormat read GetcfFormat write SetcfFormat;
    property Index : integer read GetlIndex write SetlIndex;
    property Medium : integer read GetTymed write SetTymed;
  end;

  TCustomACLDropTarget = class (TInterfacedObject, IDropTarget)
    function DragEnter (const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragOver (grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragLeave : HResult; stdcall;
    function Drop (const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
  private
    FOk: boolean;
  public
    constructor Create;
    procedure DropCard(pt:TPoint;s:string); virtual;
    procedure DragMouseMove(pt:TPoint;var Allow:boolean); virtual;
    procedure DragMouseEnter; virtual;
    procedure DragMouseLeave; virtual;
  end;

  TCustomACLDropSource = class (TInterfacedObject, IDropSource)
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
  public
    constructor Create(const stxt:string;sidx:integer);
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
  public
    function Next (CountRequested: Longint; out FormatEtcArray; PCountFetched: PLongint): HResult; stdcall;
    function Skip (count: Longint) : HResult; stdcall;
    function Reset : HResult; stdcall;
    function Clone (out enumFmt : IEnumFormatEtc) : HResult; stdcall;
  end;

  TFormatEtcArray = array[0..19] of TFormatEtc;
  PFormatEtcArray = ^TFormatEtcArray;


function StandardEffect (Keys : TShiftState; DefEffect: Integer) : integer;
function StartCardDoDragDrop(DropSource:TCustomACLDropSource;dwOKEffects: Longint; var dwEffect: Longint): HResult;


implementation

//------------------------------------------------------------------------------


function StandardEffect (Keys : TShiftState; DefEffect: Integer) : Integer;
begin
  Result := deMove;
  {
  if ssCtrl in Keys then
    Result := deCopy
  else
  begin
    if (DefEffect and deMove = 0) then
      Result := deCopy
    else
      Result := deMove;
  end;
  }
end;

//------------------------------------------------------------------------------

constructor TEnumFormats.Create (DataObject : IDataObject);
begin
  inherited Create;
  SetDataObject (DataObject);
end;

//------------------------------------------------------------------------------

destructor TEnumFormats.Destroy;
begin
  SetDataObject (nil);
  inherited Destroy
end;

//------------------------------------------------------------------------------

function TEnumFormats.Next : boolean;
var
  Returned : integer;
begin
  inc (FCount);
  FValid := FEnumerator.Next (1, FFormatEtc, @Returned) = S_OK;
  Result := FValid
end;

//------------------------------------------------------------------------------

function TEnumFormats.Reset : boolean;
begin
  FValid := false;
  FCount := 0;
  Result := Succeeded (FEnumerator.Reset)
end;

//------------------------------------------------------------------------------

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
    Reset;
  end
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

constructor TCustomACLDropTarget.Create;
begin
  inherited Create;
end;

//------------------------------------------------------------------------------

function TCustomACLDropTarget.DragEnter(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  FOk := True;

  if FOk then
    dwEffect := StandardEffect(KeysToShiftState (grfKeyState),dwEffect)
  else
    dwEffect := deNone;

  Result := NOERROR;

  DragMouseEnter;
end;

//------------------------------------------------------------------------------

function TCustomACLDropTarget.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
var
  Allow: Boolean;
begin
  if FOk then
    dwEffect := StandardEffect(KeysToShiftState (grfKeyState),dwEffect)
  else
    dwEffect := deNone;

  Allow := dwEffect <> deNone;

  DragMouseMove(pt,Allow);

  if not Allow then
    dwEffect := deNone;

  Result := NOERROR;
end;

//------------------------------------------------------------------------------

function TCustomACLDropTarget.DragLeave: HResult;
begin
  Result := NOERROR;
  DragMouseLeave;
end;

//------------------------------------------------------------------------------

function TCustomACLDropTarget.Drop(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  if FOk then
  begin
    with TEnumFormats.Create(DataObj) do
    try
      DropCard(pt, '');
    finally
      Free
    end;

    dwEffect := StandardEffect (KeysToShiftState (grfKeyState),dwEffect)
  end else
    dwEffect := deNone;

  Result := NOERROR
end;

//------------------------------------------------------------------------------

procedure TCustomACLDropTarget.DropCard(pt:tpoint;s:string);
begin
end;

//------------------------------------------------------------------------------

procedure TCustomACLDropTarget.DragMouseMove(pt: TPoint; var Allow: boolean);
begin
end;

//------------------------------------------------------------------------------

procedure TCustomACLDropTarget.DragMouseEnter;
begin
end;

//------------------------------------------------------------------------------

procedure TCustomACLDropTarget.DragMouseLeave;
begin
end;

//------------------------------------------------------------------------------

function TSourceEnumFormatEtc.Skip(Count: Longint) : HResult;
var
  TotFmt: integer;
Begin
  TotFmt:=3;

  FIndex := FIndex + Count;
  If FIndex > TotFmt then
    Begin
      FIndex := TotFmt;
      Result := S_FALSE;
    end
  else Result := S_OK;
end;

//------------------------------------------------------------------------------

function TSourceEnumFormatEtc.Next(CountRequested: Integer; out FormatEtcArray;
  PCountFetched: PLongint): HResult;
Var
  N: integer;
  FormatEtcArrayOut: TFormatEtcArray absolute FormatEtcArray;
  TotFmt: integer;
//Label the_end;
Begin
  Result := S_FALSE;

  TotFmt:=3;

  N := 0;
  while (N < CountRequested) and (N+FIndex < TotFmt) do
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
        formatetcArrayOut[0].cfFormat := 0;
        formatetcArrayOut[0].ptd      := nil;
        formatetcArrayOut[0].dwAspect := DVASPECT_CONTENT;
        formatetcArrayOut[0].lindex   := -1;
        formatetcArrayOut[0].tymed    := TYMED_HGLOBAL;
      end;
    3:begin
        formatetcArrayOut[0].cfFormat := 0;
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

//------------------------------------------------------------------------------

function TSourceEnumFormatEtc.Reset : HResult;
Begin
  FIndex := 0;
  Result := S_OK;
end;

//------------------------------------------------------------------------------

function TSourceEnumFormatEtc.Clone (out enumFmt : IEnumFormatEtc) : HResult; stdcall;
Var
  MyFormatEtc: TSourceEnumFormatEtc;
Begin
  MyFormatEtc := TSourceEnumFormatEtc.Create;
  enumFmt := MyFormatEtc;
  Result := S_OK;
end;

//------------------------------------------------------------------------------

constructor TSourceDataObject.Create(const stxt:string;sidx:integer);
begin
  inherited Create;
  textdata := stxt;
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function TSourceDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
begin
  Result := DV_E_FORMATETC;
  if (formatetc.dwAspect = DVASPECT_CONTENT) and (formatetc.tymed = TYMED_HGLOBAL) then
  begin
    if (textdata<>'') then
      Result := S_OK;
  end;
end;

//------------------------------------------------------------------------------

function TSourceDataObject.GetData(const formatetc: TFormatEtc; out medium: TStgMedium): HResult;
Var
  HGlobalData: HGlobal;
  PGlobalData: PChar;
  AllocLen: Integer;
  TXTLen : Integer;

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

  case formatetc.cfFormat of
  CF_TEXT:AllocLen := TXTLen+1;
  CF_UNICODETEXT:AllocLen := TXTLen * 2 + 2;
  else AllocLen := 0;
  end;

  HGlobalData := GlobalAlloc((GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT), AllocLen);

  if (HGlobalData <> 0) then
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

//------------------------------------------------------------------------------

function TSourceDataObject.GetDataHere (const formatetc: TFormatEtc; out medium: TStgMedium): HResult;
Begin
  Result := DV_E_FORMATETC;
end;

//------------------------------------------------------------------------------

function TSourceDataObject.SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult;
Begin
  Result := DV_E_FORMATETC;
end;

//------------------------------------------------------------------------------

function TSourceDataObject.GetCanonicalFormatEtc(const formatetcIn: TFormatEtc;   out formatetcOut: TFormatEtc): HResult;
Begin
  formatetcOut.ptd := nil;
  Result := E_NOTIMPL;
end;

//------------------------------------------------------------------------------

function TSourceDataObject.DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
Begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

//------------------------------------------------------------------------------

function TSourceDataObject.DUnadvise(dwConnection: Longint): HResult; stdcall;
Begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

//------------------------------------------------------------------------------

function TSourceDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
Begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

//------------------------------------------------------------------------------

constructor TCustomACLDropSource.Create;
begin
 inherited Create;
end;

//------------------------------------------------------------------------------

procedure TCustomACLDropSource.CurrentEffect(dwEffect: Longint);
begin
end;

//------------------------------------------------------------------------------

procedure TCustomACLDropSource.DragDropStop;
begin

end;

//------------------------------------------------------------------------------

procedure TCustomACLDropSource.QueryDrag;
begin
end;

//------------------------------------------------------------------------------

function TCustomACLDropSource.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult;
//-------------------------------------------------------
Begin
  Result := S_OK;
  If fEscapePressed then Result := DRAGDROP_S_CANCEL else
    if ((grfKeyState and MK_LBUTTON) = 0) then // mouse-up
    begin
      Result := DRAGDROP_S_DROP;
      DragDropStop;
    end;
  QueryDrag;
end;

//------------------------------------------------------------------------------

function TCustomACLDropSource.GiveFeedback(dwEffect: Longint): HResult;
Begin
  FNoAccept := dwEffect = DROPEFFECT_NONE;
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
  CurrentEffect(dwEffect);
end;

//------------------------------------------------------------------------------

function StartCardDoDragDrop(DropSource:TCustomACLDropSource;dwOKEffects: Longint; var dwEffect: Longint): HResult;
Var
  DataObject   : TSourceDataObject;
  MyIDropSource: IDropSource;     // Ixxx versions to generate references
  MyIDataObject: IDataObject;
  stxt,srtf    : string;
Begin
  stxt := '';
  srtf := '';
  MyIDropSource  := DropSource;
  DataObject     := TSourceDataObject.Create(stxt,-1);
  MyIDataObject  := DataObject;

  Result := ActiveX.DoDragDrop(MyIDataObject, MyIDropSource, dwOKEffects, dwEffect);

  if DropSource.FNoAccept then
    Result := DRAGDROP_S_CANCEL;
end;

//------------------------------------------------------------------------------

function TEnumFormats.GetAspect: integer;
begin
  Result := FFormatEtc.dwAspect;
end;

//------------------------------------------------------------------------------

function TEnumFormats.GetcfFormat: TClipFormat;
begin
  Result := FFormatEtc.cfFormat;
end;

//------------------------------------------------------------------------------

function TEnumFormats.GetlIndex: integer;
begin
  Result := FFormatEtc.lIndex;
end;

//------------------------------------------------------------------------------

function TEnumFormats.GetTymed: integer;
begin
  Result := FFormatEtc.Tymed;
end;

//------------------------------------------------------------------------------

procedure TEnumFormats.SetAspect(Value: Integer);
begin
  FFormatEtc.dwAspect := Value;
end;

//------------------------------------------------------------------------------

procedure TEnumFormats.SetcfFormat(Value: TClipFormat);
begin
  FFormatEtc.cfFormat := Value;
end;

//------------------------------------------------------------------------------

procedure TEnumFormats.SetlIndex(Value: Integer);
begin
  FFormatEtc.lIndex := Value;
end;

//------------------------------------------------------------------------------

procedure TEnumFormats.SetTymed(value: Integer);
begin
 FFormatEtc.Tymed := Value;
end;

//------------------------------------------------------------------------------


end.
