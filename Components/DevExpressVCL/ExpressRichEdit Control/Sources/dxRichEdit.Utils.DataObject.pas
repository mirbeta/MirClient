{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxRichEdit.Utils.DataObject;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Windows, ActiveX, Clipbrd, Generics.Defaults, Generics.Collections, dxCoreClasses,
  dxGenerics;

type
{$IFOPT M+}
  {$M-}
  {$DEFINE M_ON}
{$ENDIF}
  IdxDataObject = interface(IDataObject)
  ['{7DAB74C2-BC05-417B-BDA4-73DEC63D6D26}']
    function ContainsData(const AFormat: string; AutoConvert: Boolean = True): Boolean;
    function GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes;
    procedure SetData(const AFormat: string; const AData); overload;
    procedure SetData(const AFormat: string; AutoConvert: Boolean; const AData); overload;
    function GetFormats(AutoConvert: Boolean = True): TArray<string>;
    procedure Open;
    procedure Close;
  end;
{$IFDEF M_ON}
  {$M+}
{$ENDIF}

  TdxOfficeDataFormats = class
  public const
    Text = 'Text';
    UnicodeText = 'UnicodeText';
    Dib = 'DeviceIndependentBitmap';
    Bitmap = 'Bitmap';
    EnhancedMetafile = 'EnhancedMetafile';
    MetafilePict = 'MetaFilePict';
    SilverlightXaml = 'com.microsoft.xaml.silverlight';
    SymbolicLink = 'SymbolicLink';
    SuppressStoreImageSize = 'SuppressStoreImageSize';
    Dif = 'DataInterchangeFormat';
    Tiff = 'TaggedImageFileFormat';
    OemText = 'OEMText';
    Palette = 'Palette';
    PenData = 'PenData';
    Riff = 'RiffAudio';
    WaveAudio = 'WaveAudio';
    FileDrop = 'FileDrop';
    Locale = 'Locale';
    Html = 'HTML Format';
    Rtf = 'Rich Text Format';
    RtfWithoutObjects = 'Rich Text Format Without Objects';
    CommaSeparatedValue = 'Csv';
    XMLSpreadsheet = 'XML Spreadsheet';
    MsSourceUrl = 'msSourceUrl';
  end;

  { TdxDataObject }

  TdxDataObject = class(TInterfacedObject, IdxDataObject, IDataObject)
  strict private
    FInnerData: IdxDataObject;
    //IDataObject
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult; overload; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult; stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult; overload; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
  public
    constructor Create;
    destructor Destroy; override;

    //IdxDataObject
    function ContainsData(const AFormat: string; AutoConvert: Boolean = True): Boolean;
    function GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes; overload;
    procedure SetData(const AFormat: string; const AData); overload;
    procedure SetData(const AFormat: string; AutoConvert: Boolean; const AData); overload;
    function GetFormats(AutoConvert: Boolean = True): TArray<string>;
    procedure Open;
    procedure Close;
  end;

  { TdxDataObjectItem}

  TdxDataObjectItem = class abstract
  strict private
    FData: TBytes;
  protected
    function GetSize(const AData): Cardinal; virtual; abstract;
    function GetAsString: string; virtual; abstract;
  public
    destructor Destroy; override;

    procedure CopyData(P: Pointer; ASize: Cardinal);
    procedure GetData(AData: Pointer);
    procedure SetData(const AData);

    class function CreateDataObjectItem(const AFormat: string): TdxDataObjectItem;

    property AsString: string read GetAsString;
    property Data: TBytes read FData;
  end;
  TdxDataObjectItemClass = class of TdxDataObjectItem;

  { TdxDataObjectAnsiItem }

  TdxDataObjectAnsiItem = class(TdxDataObjectItem)
  protected
    function GetAsString: string; override;
    function GetSize(const AData): Cardinal; override;
  end;

  { TdxDataObjectUnicodeItem }

  TdxDataObjectUnicodeItem = class(TdxDataObjectItem)
  protected
    function GetAsString: string; override;
    function GetSize(const AData): Cardinal; override;
  end;

  { TdxDragAndDropDataObject }

  TdxDragAndDropDataObject = class(TInterfacedObject, IdxDataObject, IDataObject)
  strict private
    FLockCount: Integer;
    FDataObjects: TdxNamedObjectDictionary<TdxDataObjectItem>;
    procedure Check;
  protected
    //IDataObject
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult; overload; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult; stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult; overload; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
    //IdxDataObject
    function ContainsData(const AFormat: string; AutoConvert: Boolean = True): Boolean;
    function GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes; overload;
    procedure SetData(const AFormat: string; const AData); overload;
    procedure SetData(const AFormat: string; AutoConvert: Boolean; const AData); overload;
    function GetFormats(AutoConvert: Boolean = True): TArray<string>;
    procedure Open;
    procedure Close;
  end;

  { TdxDragAndDropExternalDataObject }

  TdxDragAndDropExternalDataObject = class(TInterfacedObject, IdxDataObject, IDataObject)
  strict private
    FDataObject: IDataObject;
    function GetFormatetc(const AFormat: string): TFormatEtc;
    //IDataObject
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult; overload; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult; stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult; overload; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
  public
    constructor Create(const ADataObject: IDataObject);
    //IdxDataObject
    function ContainsData(const AFormat: string; AutoConvert: Boolean = True): Boolean;
    function GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes; overload;
    procedure SetData(const AFormat: string; const AData); overload;
    procedure SetData(const AFormat: string; AutoConvert: Boolean; const AData); overload;
    function GetFormats(AutoConvert: Boolean = True): TArray<string>;
    procedure Open;
    procedure Close;
  end;

  { TdxDataStore }

  TdxDataStore = class(TInterfacedObject, IdxDataObject, IDataObject)
  protected
    //IDataObject
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult; overload; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult; stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult; overload; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
  public
    //IdxDataObject
    function ContainsData(const AFormat: string; AutoConvert: Boolean = True): Boolean;
    function GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes; overload;
    procedure SetData(const AFormat: string; const AData); overload;
    procedure SetData(const AFormat: string; AutoConvert: Boolean; const AData); overload;
    function GetFormats(AutoConvert: Boolean = True): TArray<string>;
    procedure Open;
    procedure Close;
  end;

  { IdxPasteSource }

  IdxPasteSource = interface
  ['{E9F7028F-CA43-4313-A359-55423530D104}']
    function GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes;
    function GetDataAsText(const AFormat: string; AutoConvert: Boolean = False): string;
    function ContainsData(const AFormat: string; AutoConvert: Boolean = False): Boolean;
  end;

  { TdxPasteSource }

  TdxPasteSource = class abstract(TInterfacedObject, IdxPasteSource)
  public
    //IdxPasteSource
    function GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes; virtual; abstract;
    function GetDataAsText(const AFormat: string; AutoConvert: Boolean = False): string; virtual;
    function ContainsData(const AFormat: string; AutoConvert: Boolean = False): Boolean; virtual; abstract;
  end;

  TdxDataObjectPasteSource = class(TdxPasteSource)
  private
    FDataObject: IdxDataObject;
  public
    constructor Create(const ADataObject: IdxDataObject);

    function GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes; override;
    function ContainsData(const AFormat: string; AutoConvert: Boolean = False): Boolean; override;

    property DataObject: IdxDataObject read FDataObject write FDataObject;
  end;

  { TdxClipboardPasteSource }

  TdxClipboardPasteSource = class(TdxPasteSource)
  public
    function GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes; override;
    function GetDataAsText(const AFormat: string; AutoConvert: Boolean = False): string; override;
    function ContainsData(const AFormat: string; AutoConvert: Boolean = False): Boolean; override;
  end;

  { TdxEmptyPasteSource }

  TdxEmptyPasteSource = class(TdxPasteSource)
  public
    function GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes; override;
    function ContainsData(const AFormat: string; AutoConvert: Boolean = False): Boolean; override;
  end;

  { TdxClipboard}

  TdxClipboard = class helper for TClipboard
  private
    function CanGetDataAsText(const AFormat: string): Boolean;
    function GetRtf: TArray<Byte>;
    function GetRtfWithoutObjects: TArray<Byte>;
    function GetSuppressStoreImageSize: AnsiString;
    function GetUnicodeText: string;
    procedure SetRtf(const Value: TArray<Byte>);
    procedure SetRtfWithoutObjects(const Value: TArray<Byte>);
    procedure SetSuppressStoreImageSize(const Value: AnsiString);
    procedure SetUnicoteText(const Value: string);
  protected
    function GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes;
    function GetDataAsText(const AFormat: string; AutoConvert: Boolean = False): string;
  public
    function ContainsData(const AFormat: string): Boolean; overload;
    function ContainsData(const AFormat: Word): Boolean; overload;
    class function RegisterFormat(const AFormat: string; ADataObjectClass: TdxDataObjectItemClass = nil): Word;

    property Rtf: TArray<Byte> read GetRtf write SetRtf;
    property RtfWithoutObjects: TArray<Byte> read GetRtfWithoutObjects write SetRtfWithoutObjects;
    property SuppressStoreImageSize: AnsiString read GetSuppressStoreImageSize write SetSuppressStoreImageSize;
    property UnicodeText: string read GetUnicodeText write SetUnicoteText;
  end;

  { TdxClipboardStringContent }

  TdxClipboardStringContent = class(TInterfacedObject)
  private
    FStringContent: string;
  public
    constructor Create(const AContent: string);

    property StringContent: string read FStringContent write FStringContent;
  end;

const
  {$EXTERNALSYM CF_RTF}
  CF_RTF: Word = $FF;
  {$EXTERNALSYM CF_RTFWithoutObjects}
  CF_RTFWithoutObjects: Word = $FF;
  {$EXTERNALSYM CF_XMLSpreadsheet}
  CF_XMLSpreadsheet: Word = $FF;
  {$EXTERNALSYM CF_SuppressStoreImageSize}
  CF_SuppressStoreImageSize: Word = $FF;
  {$EXTERNALSYM CF_MsSourceUrl}
  CF_MsSourceUrl: Word = $FF;
  CF_HTML: Word = $FF;

implementation

uses
  dxCore,
  dxEncoding,
  dxRichEdit.Utils.Exceptions;

var
  FFormats: TdxNamedOrdinalDictionary<Word>;
  FFormatDataObjectItems: TdxNamedOrdinalDictionary<TdxDataObjectItemClass>;

function CFToFormat(ACF: Word; out AFormat: string): Boolean;
var
  S: string;
begin
  AFormat := '';
  for S in FFormats.Keys do
    if FFormats[S] = ACF then
    begin
      AFormat := S;
      Break;
    end;
  Result := Length(AFormat) > 0;
end;

type
  TFormatList = array[0..255] of TFormatEtc;

  TdxEnumFormatEtc = class(TInterfacedObject, IEnumFORMATETC)
  private
    FDataObject: IdxDataObject;
    FDataObjectFormats: TArray<TFormatEtc>;
    FIndex: Integer;
    function GetCount: Integer;
  protected
    procedure PopulateDataObjectFormats;
    property Count: Integer read GetCount;
  public
    constructor Create(const ADataObject: IdxDataObject);

    function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumFormatEtc): HResult; stdcall;
  end;

{ TdxEnumFormatEtc }

constructor TdxEnumFormatEtc.Create(const ADataObject: IdxDataObject);
begin
  inherited Create;
  FDataObject := ADataObject;
  PopulateDataObjectFormats;
  Reset;
end;

function TdxEnumFormatEtc.Next(celt: Longint; out elt; pceltFetched: PLongint): HResult;
var
  I: Integer;
begin
  I := 0;
  while (I < celt) and (FIndex < Count) do
  begin
    TFormatList(elt)[I] := FDataObjectFormats[FIndex];
    Inc(FIndex);
    Inc(I);
  end;
  if pceltFetched <> nil then pceltFetched^ := I;
  if I = celt then Result := S_OK else Result := S_FALSE;
end;

function TdxEnumFormatEtc.Skip(celt: Longint): HResult;
begin
  if celt <= Count - FIndex then
  begin
    FIndex := FIndex + celt;
    Result := S_OK;
  end
  else
  begin
    FIndex := Count;
    Result := S_FALSE;
  end;
end;

function TdxEnumFormatEtc.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TdxEnumFormatEtc.Clone(out Enum: IEnumFormatEtc): HResult;
begin
  Enum := TdxEnumFormatEtc.Create(FDataObject);
  Result := S_OK;
end;

procedure TdxEnumFormatEtc.PopulateDataObjectFormats;
var
  AFormats: TArray<string>;
  I, J: Integer;
  ACount: Integer;
begin
  AFormats := FDataObject.GetFormats;
  ACount := Length(AFormats);
  SetLength(FDataObjectFormats, ACount);
  J := 0;
  for I := 0 to ACount - 1 do
  begin
    if not FFormats.ContainsKey(AFormats[I]) then
      Continue;
    FDataObjectFormats[J].cfFormat := FFormats[AFormats[I]];
    FDataObjectFormats[J].dwAspect := DVASPECT_CONTENT;
    FDataObjectFormats[J].lindex := -1;
    FDataObjectFormats[J].tymed := TYMED_HGLOBAL;
    Inc(J);
  end;
  SetLength(FDataObjectFormats, J);
end;

function TdxEnumFormatEtc.GetCount: Integer;
begin
  Result := Length(FDataObjectFormats);
end;

{ TdxDataObject }

constructor TdxDataObject.Create;
begin
  inherited Create;
  FInnerData := TdxDataStore.Create;
end;

destructor TdxDataObject.Destroy;
begin
  FInnerData := nil;
  inherited Destroy;
end;

function TdxDataObject.GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult;
begin
  Result := IDataObject(FInnerData).GetData(formatetcIn, medium);
end;

function TdxDataObject.GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HResult;
begin
  Result := IDataObject(FInnerData).GetDataHere(formatetc, medium);
end;

function TdxDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
begin
  Result := IDataObject(FInnerData).QueryGetData(formatetc);
end;

function TdxDataObject.GetCanonicalFormatEtc(const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HResult;
begin
  Result := IDataObject(FInnerData).GetCanonicalFormatEtc(formatetc, formatetcOut);
end;

function TdxDataObject.SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult;
begin
  Result := IDataObject(FInnerData).SetData(formatetc, medium, fRelease);
end;

function TdxDataObject.EnumFormatEtc(dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult;
begin
  Result := IDataObject(FInnerData).EnumFormatEtc(dwDirection, enumFormatEtc);
end;

function TdxDataObject.DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult;
begin
  Result := IDataObject(FInnerData).DAdvise(formatetc, advf, advSink, dwConnection);
end;

function TdxDataObject.DUnadvise(dwConnection: Longint): HResult;
begin
  Result := IDataObject(FInnerData).DUnadvise(dwConnection);
end;

function TdxDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result := IDataObject(FInnerData).EnumDAdvise(enumAdvise);
end;

procedure TdxDataObject.Close;
begin
  FInnerData.Close;
end;

function TdxDataObject.ContainsData(const AFormat: string; AutoConvert: Boolean = True): Boolean;
begin
  Result := FInnerData.ContainsData(AFormat, AutoConvert);
end;

function TdxDataObject.GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes;
begin
  Result := nil;
end;

function TdxDataObject.GetFormats(AutoConvert: Boolean): TArray<string>;
begin
  Result := nil;
end;

procedure TdxDataObject.SetData(const AFormat: string; const AData);
begin
  SetData(AFormat, True, AData);
end;

procedure TdxDataObject.SetData(const AFormat: string; AutoConvert: Boolean; const AData);
begin
  FInnerData.SetData(AFormat, AutoConvert, AData);
end;

procedure TdxDataObject.Open;
begin
  FInnerData.Open;
end;

{ TdxDataObjectItem}

destructor TdxDataObjectItem.Destroy;
begin
  FData := nil;
  inherited Destroy;
end;

class function TdxDataObjectItem.CreateDataObjectItem(const AFormat: string): TdxDataObjectItem;
var
  AItemClass: TdxDataObjectItemClass;
begin
  if FFormatDataObjectItems.ContainsKey(AFormat) then
    AItemClass := FFormatDataObjectItems[AFormat]
  else
    AItemClass := TdxDataObjectUnicodeItem;
  Result := AItemClass.Create;
end;

procedure TdxDataObjectItem.GetData(AData: Pointer);
begin
  Move(FData[0], AData^, Length(FData));
end;

procedure TdxDataObjectItem.SetData(const AData);
var
  P: Pointer absolute AData;
begin
  CopyData(P, GetSize(AData));
end;

procedure TdxDataObjectItem.CopyData(P: Pointer; ASize: Cardinal);
begin
  SetLength(FData, ASize);
  Move(P^, FData[0], ASize);
end;

{ TdxDataObjectAnsiItem }

function TdxDataObjectAnsiItem.GetAsString: string;
begin
  Result := TdxEncoding.ANSI.GetString(Data);
end;

function TdxDataObjectAnsiItem.GetSize(const AData): Cardinal;
begin
  Result := Length(PAnsiChar(AData)) * SizeOf(AnsiChar);
end;

{ TdxDataObjectUnicodeItem }

function TdxDataObjectUnicodeItem.GetAsString: string;
begin
  Result := TdxEncoding.Unicode.GetString(Data);
end;

function TdxDataObjectUnicodeItem.GetSize(const AData): Cardinal;
begin
  Result := Length(PChar(AData)) * SizeOf(Char);
end;

{ TdxDataStore }

function TdxDataStore.GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult;
begin
  Result := S_FALSE;
  Assert(False, 'not implemented');
end;

function TdxDataStore.GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HResult;
begin
  Result := S_FALSE;
  Assert(False, 'not implemented');
end;

function TdxDataStore.QueryGetData(const formatetc: TFormatEtc): HResult;
begin
  Result := S_FALSE;
  Assert(False, 'not implemented');
end;

function TdxDataStore.GetCanonicalFormatEtc(const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HResult;
begin
  Result := S_FALSE;
  Assert(False, 'not implemented');
end;

function TdxDataStore.SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult;
begin
  Result := S_FALSE;
  Assert(False, 'not implemented');
end;

function TdxDataStore.EnumFormatEtc(dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult;
begin
  Result := S_FALSE;
  Assert(False, 'not implemented');
end;

function TdxDataStore.DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult;
begin
  Result := S_FALSE;
  Assert(False, 'not implemented');
end;

function TdxDataStore.DUnadvise(dwConnection: Longint): HResult;
begin
  Result := S_FALSE;
  Assert(False, 'not implemented');
end;

function TdxDataStore.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result := S_FALSE;
  Assert(False, 'not implemented');
end;

function TdxDataStore.ContainsData(const AFormat: string; AutoConvert: Boolean = True): Boolean;
begin
  Result := Clipboard.ContainsData(AFormat);
end;

function TdxDataStore.GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes;
begin
  Result := nil;
  Assert(False, 'not implemented');
end;

procedure TdxDataStore.Close;
begin
  Clipboard.Close;
end;

procedure TdxDataStore.Open;
begin
  Clipboard.Open;
end;

function TdxDataStore.GetFormats(AutoConvert: Boolean = True): TArray<string>;
begin
  Result := nil;
end;

procedure TdxDataStore.SetData(const AFormat: string; AutoConvert: Boolean; const AData);
begin
  if AFormat = TdxOfficeDataFormats.Rtf then
    Clipboard.Rtf := TArray<Byte>(AData)
  else
  if AFormat = TdxOfficeDataFormats.UnicodeText then
    Clipboard.UnicodeText := string(AData)
  else
  if AFormat = TdxOfficeDataFormats.RtfWithoutObjects then
    Clipboard.RtfWithoutObjects := TArray<Byte>(AData)
  else
    Assert(False);
end;

procedure TdxDataStore.SetData(const AFormat: string; const AData);
begin
  SetData(AFormat, True, AData);
end;

{ TdxEmptyPasteSource }

function TdxEmptyPasteSource.ContainsData(const AFormat: string;
  AutoConvert: Boolean): Boolean;
begin
  Result := False;
end;

function TdxEmptyPasteSource.GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes;
begin
  Result := nil;
end;

{ TdxClipboard }

function TdxClipboard.GetRtf: TArray<Byte>;
var
  AData: THandle;
  ALength: Integer;
begin
  Open;
  AData := GetClipboardData(CF_RTF);
  try
    if AData <> 0 then
    begin
      ALength := GlobalSize(AData);
      SetLength(Result, ALength);
      Move(GlobalLock(AData)^, Result[0], ALength);
    end;
  finally
    if AData <> 0 then
      GlobalUnlock(AData);
    Close;
  end;
end;

function TdxClipboard.GetRtfWithoutObjects: TArray<Byte>;
var
  AData: THandle;
  ALength: Integer;
begin
  Open;
  AData := GetClipboardData(CF_RTFWithoutObjects);
  try
    if AData <> 0 then
    begin
      ALength := GlobalSize(AData);
      SetLength(Result, ALength);
      Move(GlobalLock(AData)^, Result[0], ALength);
    end;
  finally
    if AData <> 0 then
      GlobalUnlock(AData);
    Close;
  end;
end;

function TdxClipboard.GetSuppressStoreImageSize: AnsiString;
var
  AData: THandle;
  ADataSize: Cardinal;
begin
  Result := '';
  Open;
  try
    AData := GetClipboardData(CF_SuppressStoreImageSize);
    try
      if AData <> 0 then
      begin
        ADataSize := GlobalSize(AData);
        UniqueString(Result);
        SetString(Result, PAnsiChar(GlobalLock(AData)), ADataSize);
      end;
    finally
      if AData <> 0 then
        GlobalUnlock(AData);
    end;
  finally
    Close;
  end;
end;

function TdxClipboard.GetUnicodeText: string;
var
  AData: THandle;
begin
  Open;
  AData := GetClipboardData(CF_UNICODETEXT);
  try
    if AData <> 0 then
      Result := PChar(GlobalLock(AData))
    else
      Result := '';
  finally
    if AData <> 0 then
      GlobalUnlock(AData);
    Close;
  end;
end;

class function TdxClipboard.RegisterFormat(const AFormat: string; ADataObjectClass: TdxDataObjectItemClass = nil): Word;
begin
  Result := RegisterClipboardFormat(PChar(AFormat));
  if Result <> 0 then
  begin
    FFormats.AddOrSetValue(AFormat, Result);
    if ADataObjectClass <> nil then
      FFormatDataObjectItems.AddOrSetValue(AFormat, ADataObjectClass);
  end;
end;

function TdxClipboard.CanGetDataAsText(const AFormat: string): Boolean;
begin
  Result :=
    (AFormat = TdxOfficeDataFormats.Text) or
    (AFormat = TdxOfficeDataFormats.UnicodeText) or
    (AFormat = TdxOfficeDataFormats.Rtf) or
    (AFormat = TdxOfficeDataFormats.Text) or
    (AFormat = TdxOfficeDataFormats.Html) or
    (AFormat = TdxOfficeDataFormats.SuppressStoreImageSize);
end;

procedure TdxClipboard.SetRtf(const Value: TArray<Byte>);
begin
  SetBuffer(CF_RTF, Value[0], Length(Value));
end;

procedure TdxClipboard.SetRtfWithoutObjects(const Value: TArray<Byte>);
begin
  SetBuffer(CF_RTFWithoutObjects, Value[0], Length(Value));
end;

procedure TdxClipboard.SetSuppressStoreImageSize(const Value: AnsiString);
begin
  SetBuffer(CF_SuppressStoreImageSize, PAnsiChar(Value)^, Length(Value) + SizeOf(Char));
end;

procedure TdxClipboard.SetUnicoteText(const Value: string);
begin
  SetBuffer(CF_UNICODETEXT, PChar(Value)^, ByteLength(Value) + SizeOf(Char));
end;

function TdxClipboard.GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes;
var
  S: string;
  AHandle: THandle;
begin
  Result := nil;
  if not ContainsData(AFormat) then
    Exit;
  if CanGetDataAsText(AFormat) then
  begin
    S := GetDataAsText(AFormat, AutoConvert);
    Result := BytesOf(S);
  end
  else
  begin
    Open;
    AHandle := GetClipboardData(FFormats[AFormat]);
    try
      if AHandle <> 0 then
      begin
        SetLength(Result, SizeOf(AHandle));
        Move(AHandle, Result[0], SizeOf(AHandle));
      end;
    finally
      if AHandle <> 0 then
        GlobalUnlock(AHandle);
      Close;
    end;
  end;
end;

function TdxClipboard.GetDataAsText(const AFormat: string; AutoConvert: Boolean = False): string;
begin
  if AFormat = TdxOfficeDataFormats.Text then
    Result := AsText
  else
    if AFormat = TdxOfficeDataFormats.UnicodeText then
      Result := UnicodeText
    else
      if AFormat = TdxOfficeDataFormats.Rtf then
        Result := TdxEncoding.Unicode.GetString(TEncoding.Convert(TdxEncoding.ANSI, TdxEncoding.Unicode, Rtf))
      else
        if AFormat = TdxOfficeDataFormats.SuppressStoreImageSize then
          Result := dxAnsiStringToString(SuppressStoreImageSize)
        else
          Result := '';
end;

function TdxClipboard.ContainsData(const AFormat: string): Boolean;
begin
  Result := FFormats.ContainsKey(AFormat);
  Result := Result and ContainsData(FFormats[AFormat]);
end;

function TdxClipboard.ContainsData(const AFormat: Word): Boolean;
begin
  Result := HasFormat(AFormat);
end;

{ TdxClipboardPasteSource }

function TdxClipboardPasteSource.GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes;
begin
  Result := Clipboard.GetData(AFormat, AutoConvert);
end;

function TdxClipboardPasteSource.GetDataAsText(const AFormat: string; AutoConvert: Boolean = False): string;
begin
  if Clipboard.CanGetDataAsText(AFormat) then
    Result := Clipboard.GetDataAsText(AFormat, AutoConvert)
  else
    Result := inherited GetDataAsText(AFormat, AutoConvert);
end;

function TdxClipboardPasteSource.ContainsData(const AFormat: string;
  AutoConvert: Boolean): Boolean;
begin
  Result := Clipboard.ContainsData(AFormat);
end;

{ TdxClipboardStringContent }

constructor TdxClipboardStringContent.Create(const AContent: string);
begin
  inherited Create;
  FStringContent := AContent;
end;

{ TdxPasteSource }

function TdxPasteSource.GetDataAsText(const AFormat: string; AutoConvert: Boolean = False): string;
var
  ASize: Cardinal;
  AData: TBytes;
  AItem: TdxDataObjectItem;
begin
  Result := '';
  AData := GetData(AFormat);
  ASize := Length(AData);
  if ASize > 0 then
  try
    AItem := TdxDataObjectItem.CreateDataObjectItem(AFormat);
    try
      AItem.CopyData(Pointer(AData), ASize);
      Result := AItem.AsString;
    finally
      AItem.Free;
    end;
  finally
    AData := nil;
  end;
end;

{ TdxPlatformIndependentDataObject }

constructor TdxDragAndDropDataObject.Create;
begin
  inherited Create;
  FDataObjects := TdxNamedObjectDictionary<TdxDataObjectItem>.Create(True);
end;

destructor TdxDragAndDropDataObject.Destroy;
begin
  FreeAndNil(FDataObjects);
  inherited Destroy;
end;

function TdxDragAndDropDataObject.GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult;
var
  AFormat: string;
  ASize: Cardinal;
  AItem: TdxDataObjectItem;
  APointer: Pointer;
begin
  Result := S_FALSE;
  if (QueryGetData(formatetcIn) = S_OK) and CFToFormat(formatetcIn.cfFormat, AFormat) then
  begin
    FillChar(medium, SizeOf(medium), 0);
    medium.tymed := TYMED_HGLOBAL;
    AItem := FDataObjects[AFormat];
    ASize := Length(AItem.Data);
    medium.hGlobal := GlobalAlloc(GMEM_MOVEABLE, ASize);
    APointer := GlobalLock(medium.hGlobal);
    try
      AItem.GetData(APointer);
      Result := S_OK;
    finally
      GlobalUnlock(medium.hGlobal);
    end;
  end;
end;

function TdxDragAndDropDataObject.GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HResult;
begin
  Result := E_NOTIMPL;
end;

function TdxDragAndDropDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
const
  ResultMap: array [Boolean] of HResult = (S_FALSE, S_OK);
var
  AFormat: string;
begin
  if not CFToFormat(formatetc.cfFormat, AFormat) then
    Result := S_FALSE
  else
  begin
    Result := ResultMap[ContainsData(AFormat)];
  end;
end;

function TdxDragAndDropDataObject.GetCanonicalFormatEtc(const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HResult;
begin
  Result := E_NOTIMPL;
end;

function TdxDragAndDropDataObject.SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult;
var
  AFormat: string;
begin
  if CFToFormat(formatetc.cfFormat, AFormat) then
  begin
    Assert(false, 'not implemented');
    Result := S_OK;
  end
  else
    Result := E_UNEXPECTED;
end;

function TdxDragAndDropDataObject.EnumFormatEtc(dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult;
begin
  if dwDirection = DATADIR_GET then
  begin
    enumFormatEtc := TdxEnumFormatEtc.Create(Self);
    Result := S_OK;
  end
  else
  begin
    enumFormatEtc := nil;
    Result := E_NOTIMPL;
  end;
end;

function TdxDragAndDropDataObject.DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult;
begin
  Result := E_NOTIMPL;
end;

function TdxDragAndDropDataObject.DUnadvise(dwConnection: Longint): HResult;
begin
  Result := E_NOTIMPL;
end;

function TdxDragAndDropDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure TdxDragAndDropDataObject.Check;
begin
  if FLockCount = 0 then
    FDataObjects.Clear;
end;

procedure TdxDragAndDropDataObject.Close;
begin
  Dec(FLockCount);
end;

function TdxDragAndDropDataObject.ContainsData(const AFormat: string; AutoConvert: Boolean = True): Boolean;
begin
  Result := FDataObjects.ContainsKey(AFormat);
end;

function TdxDragAndDropDataObject.GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes;
var
  AItem: TdxDataObjectItem;
begin
  if FDataObjects.ContainsKey(AFormat) then
  begin
    AItem := FDataObjects[AFormat];
    Result := AItem.Data;
  end
  else
    Result := nil;
end;

function TdxDragAndDropDataObject.GetFormats(AutoConvert: Boolean = True): TArray<string>;
begin
  Result := FDataObjects.Keys;
end;

procedure TdxDragAndDropDataObject.Open;
begin
  Inc(FLockCount);
end;

procedure TdxDragAndDropDataObject.SetData(const AFormat: string; AutoConvert: Boolean; const AData);
var
  AItem: TdxDataObjectItem;
  P: Pointer absolute AData;
begin
  if P = nil then
    Exit;
  Check;
  AItem := TdxDataObjectItem.CreateDataObjectItem(AFormat);
  AItem.SetData(AData);
  if FDataObjects.ContainsKey(AFormat) then
  begin
    FDataObjects[AFormat].Free;
    FDataObjects.Remove(AFormat);
  end;
  FDataObjects.Add(AFormat, AItem);
end;

procedure TdxDragAndDropDataObject.SetData(const AFormat: string;
  const AData);
begin
  SetData(AFormat, True, AData);
end;

{ TdxDragAndDropExternalDataObject }

constructor TdxDragAndDropExternalDataObject.Create(const ADataObject: IDataObject);
begin
  inherited Create;
  FDataObject := ADataObject;
end;

function TdxDragAndDropExternalDataObject.GetFormatetc(const AFormat: string): TFormatEtc;
begin
  Result.cfFormat := FFormats[AFormat];
  Result.ptd := nil;
  Result.dwAspect := DVASPECT_CONTENT;
  Result.lindex := -1;
  Result.tymed := TYMED_HGLOBAL;
end;

function TdxDragAndDropExternalDataObject.GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult;
begin
  Result := FDataObject.GetData(formatetcIn, medium);
end;

function TdxDragAndDropExternalDataObject.GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HResult;
begin
  Result := FDataObject.GetDataHere(formatetc, medium);
end;

function TdxDragAndDropExternalDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
begin
  Result := FDataObject.QueryGetData(formatetc);
end;

function TdxDragAndDropExternalDataObject.GetCanonicalFormatEtc(const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HResult;
begin
  Result := FDataObject.GetCanonicalFormatEtc(formatetc, formatetcOut);
end;

function TdxDragAndDropExternalDataObject.SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult;
begin
  Result := FDataObject.SetData(formatetc, medium, fRelease);
end;

function TdxDragAndDropExternalDataObject.EnumFormatEtc(dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult;
begin
  Result := FDataObject.EnumFormatEtc(dwDirection, enumFormatEtc);
end;

function TdxDragAndDropExternalDataObject.DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult;
begin
  Result := FDataObject.DAdvise(formatetc, advf, advSink, dwConnection);
end;

function TdxDragAndDropExternalDataObject.DUnadvise(dwConnection: Longint): HResult;
begin
  Result := FDataObject.DUnadvise(dwConnection);
end;

function TdxDragAndDropExternalDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result := FDataObject.EnumDAdvise(enumAdvise);
end;

function TdxDragAndDropExternalDataObject.ContainsData(const AFormat: string; AutoConvert: Boolean = True): Boolean;
var
  AFormatetc: TFormatEtc;
begin
  Result := FFormats.ContainsKey(AFormat);
  if Result then
  begin
    AFormatetc := GetFormatetc(AFormat);
    Result := QueryGetData(AFormatetc) = S_OK;
  end;
end;

function TdxDragAndDropExternalDataObject.GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes;
var
  AFormatetc: TFormatEtc;
  AMedium: TStgMedium;
  AData: Pointer;
  ASize: Cardinal;
begin
  Result := nil;
  if ContainsData(AFormat, AutoConvert) then
  begin
    AFormatetc := GetFormatetc(AFormat);
    FillChar(AMedium, SizeOf(AMedium), 0);
    try
      if GetData(AFormatetc, AMedium) = S_OK then
      begin
        ASize := GlobalSize(AMedium.hGlobal);
        if ASize > 0 then
        try
          AData := GlobalLock(AMedium.hGlobal);
          SetLength(Result, ASize);
          Move(AData^, Result[0], ASize);
        finally
          GlobalUnlock(AMedium.hGlobal);
        end;
      end;
    finally
      ReleaseStgMedium(AMedium);
    end;
  end;
end;

procedure TdxDragAndDropExternalDataObject.SetData(const AFormat: string; const AData);
begin
  Assert(False, 'not used');
end;

procedure TdxDragAndDropExternalDataObject.SetData(const AFormat: string; AutoConvert: Boolean; const AData);
begin
  Assert(False, 'not used');
end;

function TdxDragAndDropExternalDataObject.GetFormats(AutoConvert: Boolean = True): TArray<string>;
begin
  Assert(False, 'not used');
end;

procedure TdxDragAndDropExternalDataObject.Open;
begin
  Assert(False, 'not used');
end;

procedure TdxDragAndDropExternalDataObject.Close;
begin
  Assert(False, 'not used');
end;

{ TdxDataObjectPasteSource }

constructor TdxDataObjectPasteSource.Create(const ADataObject: IdxDataObject);
begin
  inherited Create;
  FDataObject := ADataObject;
end;

function TdxDataObjectPasteSource.ContainsData(const AFormat: string;
  AutoConvert: Boolean): Boolean;
begin
  Result := (DataObject <> nil) and DataObject.ContainsData(AFormat, AutoConvert);
end;

function TdxDataObjectPasteSource.GetData(const AFormat: string; AutoConvert: Boolean = False): TBytes;
begin
  if FDataObject = nil then
    Result := nil
  else
    Result := FDataObject.GetData(AFormat, AutoConvert);
end;

procedure PopulateFormats;
begin
  FFormats.AddOrSetValue(TdxOfficeDataFormats.Text, CF_TEXT);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.UnicodeText, CF_UNICODETEXT);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.Dib, CF_DIB);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.Bitmap, CF_BITMAP);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.EnhancedMetafile, CF_ENHMETAFILE);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.MetafilePict, CF_METAFILEPICT);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.SuppressStoreImageSize, CF_SuppressStoreImageSize);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.Dif, CF_DIF);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.Tiff, CF_TIFF);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.OemText, CF_OEMTEXT);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.Palette, CF_PALETTE);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.PenData, CF_PENDATA);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.Riff, CF_RIFF);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.WaveAudio, CF_WAVE);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.FileDrop, CF_HDROP);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.Locale, CF_LOCALE);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.Html, CF_HTML);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.Rtf, CF_RTF);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.RtfWithoutObjects, CF_RTFWithoutObjects);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.XMLSpreadsheet, CF_XMLSpreadsheet);
  FFormats.AddOrSetValue(TdxOfficeDataFormats.MsSourceUrl, CF_MsSourceUrl);
end;

procedure PopulateFormatDataObjectItems;
begin
  FFormatDataObjectItems.AddOrSetValue(TdxOfficeDataFormats.Text, TdxDataObjectAnsiItem);
  FFormatDataObjectItems.AddOrSetValue(TdxOfficeDataFormats.UnicodeText, TdxDataObjectUnicodeItem);
  FFormatDataObjectItems.AddOrSetValue(TdxOfficeDataFormats.SuppressStoreImageSize, TdxDataObjectAnsiItem);
  FFormatDataObjectItems.AddOrSetValue(TdxOfficeDataFormats.FileDrop, TdxDataObjectUnicodeItem);
  FFormatDataObjectItems.AddOrSetValue(TdxOfficeDataFormats.Rtf, TdxDataObjectAnsiItem);
  FFormatDataObjectItems.AddOrSetValue(TdxOfficeDataFormats.RtfWithoutObjects, TdxDataObjectAnsiItem);
end;

initialization
  OleInitialize(nil);
  CF_RTF := RegisterClipboardFormat(TdxOfficeDataFormats.Rtf);
  CF_XMLSpreadsheet := RegisterClipboardFormat(TdxOfficeDataFormats.XMLSpreadsheet);
  CF_SuppressStoreImageSize := RegisterClipboardFormat(TdxOfficeDataFormats.SuppressStoreImageSize);
  CF_RTFWithoutObjects := RegisterClipboardFormat(TdxOfficeDataFormats.RtfWithoutObjects);
  CF_MsSourceUrl := RegisterClipboardFormat(TdxOfficeDataFormats.MsSourceUrl);
  FFormats := TdxNamedOrdinalDictionary<Word>.Create;
  PopulateFormats;
  FFormatDataObjectItems := TdxNamedOrdinalDictionary<TdxDataObjectItemClass>.Create;
  PopulateFormatDataObjectItems;

finalization
  FreeAndNil(FFormats);
  FreeAndNil(FFormatDataObjectItems);
  OleUninitialize;

end.
