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
unit dxRichEdit.Import.Doc.BlipContainer;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxHash, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.BorderDescriptor,
  dxRichEdit.Utils.OfficeImage;

type

  { TdxBlipTypeCodes }

  TdxBlipTypeCodes = class
  public const
    FileBlipStoreEntry = Integer($f007);
    BlipEmf            = Integer($f01a);
    BlipWmf            = Integer($f01b);
    BlipPict           = Integer($f01c);
    BlipJpeg           = Integer($f01d);
    BlipPng            = Integer($f01e);
    BlipDib            = Integer($f01f);
    BlipTiff           = Integer($f029);
    BlipJpeg2          = Integer($f02a);
  end;

  { TdxBlipTypes }

  TdxBlipTypes = class
  public const
    Error         = Integer($00);
    Unknown       = Integer($01);
    Emf           = Integer($02);
    Wmf           = Integer($03);
    MacintoshPict = Integer($04);
    Jpeg          = Integer($05);
    Png           = Integer($06);
    Dib           = Integer($07);
    Tiff          = Integer($11);
    CMYKJpeg      = Integer($12);
  end;

  { TdxMessageDigestCodes }

  TdxMessageDigestCodes = class
  public const
    EmfSingleMessage      = Integer($3d4);
    EmfDoubleMessage      = Integer($3d5);
    WmfSingleMessage      = Integer($216);
    WmfDoubleMessage      = Integer($217);
    PictSingleMessage     = Integer($542);
    PictDoubleMessage     = Integer($543);
    JpegRGBSingleMessage  = Integer($46a);
    JpegCMYKSingleMessage = Integer($6e2);
    JpegRGBDoubleMessage  = Integer($46b);
    JpegCMYKDoubleMessage = Integer($6e3);
    PngSingleMessage      = Integer($6e0);
    PngDoubleMessage      = Integer($6e1);
    DibSingleMessage      = Integer($7a8);
    DibDoubleMessage      = Integer($7a9);
    TiffSingleMessage     = Integer($6e4);
    TiffDoubleMessage     = Integer($6e5);
  end;

  { TdxDocMetafileHeader }

  TdxDocMetafileHeader = class
  public const
    EmusPerHundredthsOfMillimeter = Integer(360);
    DeflateCode                   = Byte($00);
    UncompressedCode              = Byte($fe);
    FilterCode                    = Byte($fe);
  strict private
    FUncompressedSize: Integer;
    FLeft: Integer;
    FTop: Integer;
    FRight: Integer;
    FBottom: Integer;
    FWidthInEmus: Integer;
    FHeightInEmus: Integer;
    FCompressedSize: Integer;
    FCompressed: Boolean;
    function GetWidthInHundredthsOfMillimeter: Integer;
    function GetHeightInHundredthsOfMillimeter: Integer;
  protected
    procedure Read(AReader: TBinaryReader);
  public
    function Clone: TdxDocMetafileHeader;

    class function FromStream(AReader: TBinaryReader): TdxDocMetafileHeader; static;
    procedure Write(AWriter: TBinaryWriter);

    property UncompressedSize: Integer read FUncompressedSize write FUncompressedSize;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Right: Integer read FRight write FRight;
    property Bottom: Integer read FBottom write FBottom;
    property WidthInEmus: Integer read FWidthInEmus write FWidthInEmus;
    property WidthInHundredthsOfMillimeter: Integer read GetWidthInHundredthsOfMillimeter;
    property HeightInEmus: Integer read FHeightInEmus write FHeightInEmus;
    property HeightInHundredthsOfMillimeter: Integer read GetHeightInHundredthsOfMillimeter;
    property CompressedSize: Integer read FCompressedSize write FCompressedSize;
    property Compressed: Boolean read FCompressed write FCompressed;
  end;

  { TdxDocMetafileReader }

  TdxDocMetafileReader = class
  strict private
    FMetafileHeader: TdxDocMetafileHeader;
    FImage: TdxOfficeImageReference;
  public
    destructor Destroy; override;
    procedure Read(AReader: TBinaryReader; const AImageCreator: IdxDocOfficeImageCreator);
    procedure CheckCompressedData(const ABuffer: TBytes);

    property MetafileHeader: TdxDocMetafileHeader read FMetafileHeader;
    property Image: TdxOfficeImageReference read FImage;
  end;

  { TdxOfficeArtRecordHeader }

  TdxOfficeArtRecordHeader = class
  public const
    Size = Integer(8);
  strict private
    FVersion: Integer;
    FInstanceInfo: Integer;
    FTypeCode: Integer;
    FLength: Integer;
  protected
    procedure Read(AReader: TBinaryReader);
  public
    class function FromStream(AReader: TBinaryReader): TdxOfficeArtRecordHeader; static;
    procedure Write(AWriter: TBinaryWriter);

    property Version: Integer read FVersion write FVersion;
    property InstanceInfo: Integer read FInstanceInfo write FInstanceInfo;
    property TypeCode: Integer read FTypeCode write FTypeCode;
    property Length: Integer read FLength write FLength;
  end;

  { TdxBlipBase }

  TdxBlipBase = class abstract(TcxIUnknownObject)
  public const
    MD4MessageSize     = Integer($10);
    TagSize            = Integer($1);
    MetafileHeaderSize = Integer($22);
    DefaultTagValue    = Byte($ff);
  strict private
    FImage: TdxOfficeImageReference;
    FImageBytesStream: TBytesStream;
    FMetafileHeader: TdxDocMetafileHeader;
    FTagValue: Byte;
    function GetImageBytesStream: TBytesStream;
    function GetImageBytesLength: Integer;
  protected
    function GetSingleMD4Message: Integer; virtual; abstract;
    function GetDoubleMD4Message: Integer; virtual; abstract;
    function GetFormat: TdxOfficeImageFormat; virtual;
    procedure SetImage(AImage: TdxOfficeImageReference); overload;
    procedure SetImage(ABlip: TdxBlipBase); overload; virtual;
    procedure SetImage(AReader: TdxDocMetafileReader); overload; virtual;
    procedure Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader;
      const AImageCreator: IdxDocOfficeImageCreator); virtual;
    procedure LoadImageFromStream(AImageStream: TdxMemoryStream; const AImageCreator: IdxDocOfficeImageCreator); virtual;
    function SaveImageToStream(AWriter: TBinaryWriter): TdxMD4Byte16; virtual;
    function CreateMetafileHeader: TdxDocMetafileHeader; virtual;
    function CalcMD4MessageOffset(AUidInfo: Integer): Integer; virtual;
    function CreateImageBytesStream: TBytesStream; virtual;

    property ImageBytesStream: TBytesStream read GetImageBytesStream;
    property ImageBytesLength: Integer read GetImageBytesLength;
    property Format: TdxOfficeImageFormat read GetFormat;
  public
    constructor Create; overload;
    constructor Create(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader; const AImageCreator: IdxDocOfficeImageCreator); overload; virtual;
    constructor Create(AImage: TdxOfficeImageReference); overload; virtual;
    destructor Destroy; override;
    function Write(AWriter: TBinaryWriter): TdxMD4Byte16; overload; virtual;
    function GetSize: Integer; virtual;
    function CreateRecordHeader: TdxOfficeArtRecordHeader; virtual; abstract;

    property SingleMD4Message: Integer read GetSingleMD4Message;
    property DoubleMD4Message: Integer read GetDoubleMD4Message;
    property TagValue: Byte read FTagValue write FTagValue;
    property MetafileHeader: TdxDocMetafileHeader read FMetafileHeader;
    property Image: TdxOfficeImageReference read FImage write SetImage;
  end;

  TdxBlipBaseClass = class of TdxBlipBase;

  { TdxFileBlipStoreEntry }

  TdxFileBlipStoreEntry = class(TdxBlipBase)
  public const
    EmptySlotOffset     = Cardinal($ffffffff);
    HeaderVersion       = Integer($2);
    BlipStoreHeaderSize = Integer($24);
    Tag                 = SmallInt($ff);
    DefaultDelay        = Integer($44);
    DefaultNameLength   = Byte(0);
    UnusedDataSize      = Integer(24);
  strict private
    FRefCount: Integer;
    FEmbeddedBlipOffset: Cardinal;
    FBlipType: Byte;
    FName: string;
    FEmbeddedBlipSize: Integer;
    FHasDelayedStream: Boolean;
    FEmbeddedReader: TBinaryReader;
    FEmbeddedBlip: TdxBlipBase;
  protected
    function GetSingleMD4Message: Integer; override;
    function GetDoubleMD4Message: Integer; override;
    procedure Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader;
      const AImageCreator: IdxDocOfficeImageCreator); override;
    procedure ReadCore(AReader: TBinaryReader);
    procedure ReadName(AReader: TBinaryReader);
    procedure ReadEmbeddedBlip(AReader: TBinaryReader;
      const AImageCreator: IdxDocOfficeImageCreator);
    function GetBlipType(AFormat: TdxOfficeImageFormat): Byte; virtual;

    property EmbeddedReader: TBinaryReader read FEmbeddedReader;
    property EmbeddedBlipSize: Integer read FEmbeddedBlipSize;
    property HasDelayedStream: Boolean read FHasDelayedStream write FHasDelayedStream;
  public
    constructor Create(AReader: TBinaryReader; AEmbeddedReader: TBinaryReader;
      AHeader: TdxOfficeArtRecordHeader; const AImageCreator: IdxDocOfficeImageCreator); overload;
    constructor Create(AImage: TdxOfficeImageReference; AHasDelayedStream: Boolean); overload;
    destructor Destroy; override;
    function Write(AWriter: TBinaryWriter): TdxMD4Byte16; overload; override;
    procedure Write(AWriter: TBinaryWriter; AEmbeddedWriter: TBinaryWriter); overload;
    function WriteCore(AWriter: TBinaryWriter): Int64;
    procedure WriteName(AWriter: TBinaryWriter);
    function CreateRecordHeader: TdxOfficeArtRecordHeader; override;
    function GetSize: Integer; override;

    property Name: string read FName;
    property ReferenceCount: Integer read FRefCount write FRefCount;
  end;

  { TdxBlipFactory }

  TdxBlipFactory = class
  strict private
    class var
      FTypeCodeTranslationTable: TDictionary<Integer, TdxBlipBaseClass>;
      FImageFormatTranslationTable: TDictionary<TdxOfficeImageFormat, TdxBlipBaseClass>;
    class constructor Initialize;
    class destructor Finalize;
  public
    class function ReadAllBlips(AReader: TBinaryReader;
      AEndPosition: Int64; const AImageCreator: IdxDocOfficeImageCreator): TdxList<TdxBlipBase>; overload; static;
    class function ReadAllBlips(AReader: TBinaryReader; AEmbeddedReader: TBinaryReader;
      AEndPosition: Int64; const AImageCreator: IdxDocOfficeImageCreator): TdxList<TdxBlipBase>; overload; static;
    class function CreateBlipFromStream(AReader: TBinaryReader;
      AHeader: TdxOfficeArtRecordHeader; const AImageCreator: IdxDocOfficeImageCreator): TdxBlipBase; overload; static;
    class function CreateBlipFromStream(AReader: TBinaryReader; AEmbeddedReader: TBinaryReader;
      AHeader: TdxOfficeArtRecordHeader; const AImageCreator: IdxDocOfficeImageCreator): TdxBlipBase; overload; static;
    class function CreateBlipFromStreamCore(AReader: TBinaryReader; AEmbeddedReader: TBinaryReader;
      AHeader: TdxOfficeArtRecordHeader; const AImageCreator: IdxDocOfficeImageCreator): TdxBlipBase; static;
    class function CreateBlipFromImage(AImage: TdxOfficeImageReference): TdxBlipBase; static;
  end;

  { TdxBlipMetaFile }

  TdxBlipMetaFile = class abstract(TdxBlipBase)
  public const
    BufferSize          = Integer($4000);
    MaxUncompressedSize = Integer($8000);
  strict private
    FUncompressedSize: Integer;
    FMd4Digest: TdxMD4Byte16;
  protected
    procedure Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader;
      const AImageCreator: IdxDocOfficeImageCreator); override;
    function CalcMD4MessageOffset(AUidInfo: Integer): Integer; override;
    function CreateMetafileHeader: TdxDocMetafileHeader; override;
    function CreateImageBytesStream: TBytesStream; override;
  public
    function Write(AWriter: TBinaryWriter): TdxMD4Byte16; override;
    function GetSize: Integer; override;
  end;

  { TdxBlipEmf }

  TdxBlipEmf = class(TdxBlipMetaFile)
  protected
    function GetSingleMD4Message: Integer; override;
    function GetDoubleMD4Message: Integer; override;
  public
    function CreateRecordHeader: TdxOfficeArtRecordHeader; override;
  end;

  { TdxBlipWmf }

  TdxBlipWmf = class(TdxBlipMetaFile)
  protected
    function GetSingleMD4Message: Integer; override;
    function GetDoubleMD4Message: Integer; override;
  public
    function CreateRecordHeader: TdxOfficeArtRecordHeader; override;
  end;

  { TdxBlipPict }

  TdxBlipPict = class(TdxBlipMetaFile)
  protected
    function GetSingleMD4Message: Integer; override;
    function GetDoubleMD4Message: Integer; override;
    procedure Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader;
      const AImageCreator: IdxDocOfficeImageCreator); override;
  public
    function CreateRecordHeader: TdxOfficeArtRecordHeader; override;
  end;

  { TdxBlipPng }

  TdxBlipPng = class(TdxBlipBase)
  protected
    function GetSingleMD4Message: Integer; override;
    function GetDoubleMD4Message: Integer; override;
    function GetFormat: TdxOfficeImageFormat; override;
  public
    function CreateRecordHeader: TdxOfficeArtRecordHeader; override;
  end;

  { TdxBlipTiff }

  TdxBlipTiff = class(TdxBlipBase)
  protected
    function GetSingleMD4Message: Integer; override;
    function GetDoubleMD4Message: Integer; override;
  public
    function CreateRecordHeader: TdxOfficeArtRecordHeader; override;
  end;

  { TdxBlipDib }

  TdxBlipDib = class(TdxBlipBase)
  public const
    WidthPosition    = Integer(4);
    HeightPosition   = Integer(8);
    BitCountPosition = Integer(14);
    DwordSize        = Integer(32);
    HeaderInfoSize   = Integer(16);
  protected
    function GetSingleMD4Message: Integer; override;
    function GetDoubleMD4Message: Integer; override;
    function GetFormat: TdxOfficeImageFormat; override;
    procedure LoadImageFromStream(AImageStream: TdxMemoryStream; const AImageCreator: IdxDocOfficeImageCreator); override;
  public
    function CreateRecordHeader: TdxOfficeArtRecordHeader; override;
    function GetSize: Integer; override;
  end;

  { TdxBlipJpeg }

  TdxBlipJpeg = class(TdxBlipBase)
  protected
    function GetSingleMD4Message: Integer; override;
    function GetDoubleMD4Message: Integer; override;
    function CalcMD4MessageOffset(AUidInfo: Integer): Integer; override;
  public
    function CreateRecordHeader: TdxOfficeArtRecordHeader; override;
  end;

implementation

uses
  Math, Types, ZLib, Contnrs, dxZIPUtils,
  dxTypeHelpers,
  dxEncoding,
  dxRichEdit.Import.Doc.OfficeArtContent, dxRichEdit.Utils.Exceptions;

type
  { TdxDibHeader }

  TdxDibHeader = class
  public const
    DibHeaderSize = Integer($28);
    DibPlanes     = Integer($1);
  strict private
    FHeaderSize: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FPlanes: SmallInt;
    FBitCount: SmallInt;
    FCompression: Integer;
    FImageSize: Integer;
    FHorizontalPixelsPerMeter: Integer;
    FVerticalPixelsPerMeter: Integer;
    FUsedColors: Integer;
    FImportantColors: Integer;
  protected
    procedure Read(AReader: TBinaryReader);

    property HeaderSize: Integer read FHeaderSize;
    property Planes: SmallInt read FPlanes;
    property Compression: Integer read FCompression write FCompression;
    property ImageSize: Integer read FImageSize write FImageSize;
    property HorizontalPixelsPerMeter: Integer read FHorizontalPixelsPerMeter write FHorizontalPixelsPerMeter;
    property VerticalPixelsPerMeter: Integer read FVerticalPixelsPerMeter write FVerticalPixelsPerMeter;
    property UsedColors: Integer read FUsedColors write FUsedColors;
    property ImportantColors: Integer read FImportantColors write FImportantColors;
  public
    constructor Create;
    class function FromStream(AReader: TBinaryReader): TdxDibHeader; static;
    procedure Write(AWriter: TBinaryWriter);

    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property BitCount: SmallInt read FBitCount write FBitCount;
  end;

  { TdxDibHelper }

  TdxDibHelper = class
  public
    class function CreateBmpFileStreamForDib(ADibStream: TStream; ADibHeight: Integer; ABytesInLine: Integer): TdxMemoryStream; static;
  end;

{ TdxDibHeader }

constructor TdxDibHeader.Create;
begin
  FHeaderSize := DibHeaderSize;
  FPlanes := DibPlanes;
end;

class function TdxDibHeader.FromStream(AReader: TBinaryReader): TdxDibHeader;
begin
  Result := TdxDibHeader.Create;
  Result.Read(AReader);
end;

procedure TdxDibHeader.Read(AReader: TBinaryReader);
begin
  FHeaderSize := AReader.ReadInt32;
  if FHeaderSize <> DibHeaderSize then
    TdxOfficeArtExceptions.ThrowInvalidContent;
  FWidth := AReader.ReadInt32;
  FHeight := AReader.ReadInt32;
  FPlanes := AReader.ReadSmallInt;
  FBitCount := AReader.ReadSmallInt;
  FCompression := AReader.ReadInt32;
  FImageSize := AReader.ReadInt32;
  FHorizontalPixelsPerMeter := AReader.ReadInt32;
  FVerticalPixelsPerMeter := AReader.ReadInt32;
  FUsedColors := AReader.ReadInt32;
  FImportantColors := AReader.ReadInt32;
end;

procedure TdxDibHeader.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(FHeaderSize);
  AWriter.Write(FWidth);
  AWriter.Write(FHeight);
  AWriter.Write(FPlanes);
  AWriter.Write(FBitCount);
  AWriter.Write(FCompression);
  AWriter.Write(FImageSize);
  AWriter.Write(FHorizontalPixelsPerMeter);
  AWriter.Write(FVerticalPixelsPerMeter);
  AWriter.Write(FUsedColors);
  AWriter.Write(FImportantColors);
end;

{ TdxDibHelper }

class function TdxDibHelper.CreateBmpFileStreamForDib(ADibStream: TStream; ADibHeight: Integer; ABytesInLine: Integer): TdxMemoryStream;
var
  AWriter: TBinaryWriter;
  ADibDataSize, AFileSize, ABitsSize, AOffset: Integer;
begin
  Result := TdxMemoryStream.Create;
  AWriter := TBinaryWriter.Create(Result);
  try
    ADibDataSize := ADibStream.Size;
    AFileSize := TdxBitmapFileHelper.sizeofBITMAPFILEHEADER + ADibDataSize;
    ABitsSize := ADibHeight * ABytesInLine;
    AOffset := ADibDataSize - ABitsSize + TdxBitmapFileHelper.sizeofBITMAPFILEHEADER;

    TdxBitmapFileHelper.WriteBITMAPFILEHEADER(AWriter, AFileSize, AOffset);
    // DIB data
    Result.CopyFrom(ADibStream, ADibStream.Size - ADibStream.Position);
    Result.Position := 0;
  finally
    AWriter.Free;
  end;
end;


{ TdxDocMetafileHeader }

function TdxDocMetafileHeader.Clone: TdxDocMetafileHeader;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxDocMetafileHeader.Create;
  Result.FUncompressedSize := FUncompressedSize;
  Result.FLeft := FLeft;
  Result.FTop := FTop;
  Result.FRight := FRight;
  Result.FBottom := FBottom;
  Result.FWidthInEmus := FWidthInEmus;
  Result.FHeightInEmus := FHeightInEmus;
  Result.FCompressedSize := FCompressedSize;
  Result.FCompressed := FCompressed;
end;

class function TdxDocMetafileHeader.FromStream(AReader: TBinaryReader): TdxDocMetafileHeader;
begin
  Result := TdxDocMetafileHeader.Create;
  Result.Read(AReader);
end;

function TdxDocMetafileHeader.GetWidthInHundredthsOfMillimeter: Integer;
begin
  Result := FWidthInEmus div EmusPerHundredthsOfMillimeter;
end;

function TdxDocMetafileHeader.GetHeightInHundredthsOfMillimeter: Integer;
begin
  Result := FHeightInEmus div EmusPerHundredthsOfMillimeter;
end;

procedure TdxDocMetafileHeader.Read(AReader: TBinaryReader);
var
  ACompressionCode: Byte;
begin
  FUncompressedSize := AReader.ReadInt32;
  FLeft := AReader.ReadInt32;
  FTop := AReader.ReadInt32;
  FRight := AReader.ReadInt32;
  FBottom := AReader.ReadInt32;
  FWidthInEmus := AReader.ReadInt32;
  FHeightInEmus := AReader.ReadInt32;
  FCompressedSize := AReader.ReadInt32;
  ACompressionCode := AReader.ReadByte;
  if ACompressionCode = DeflateCode then
    FCompressed := True;
  if AReader.ReadByte <> $fe then
    TdxOfficeArtExceptions.ThrowInvalidContent;
end;

procedure TdxDocMetafileHeader.Write(AWriter: TBinaryWriter);
var
  ACompressed: Byte;
begin
  AWriter.Write(FUncompressedSize);
  AWriter.Write(FLeft);
  AWriter.Write(FTop);
  AWriter.Write(FRight);
  AWriter.Write(FBottom);
  AWriter.Write(FWidthInEmus);
  AWriter.Write(FHeightInEmus);
  AWriter.Write(FCompressedSize);
  if FCompressed then
    ACompressed := DeflateCode
  else
    ACompressed := UncompressedCode;
  AWriter.Write(ACompressed);
  AWriter.Write(FilterCode);
end;

{ TdxDocMetafileReader }

destructor TdxDocMetafileReader.Destroy;
begin
  FreeAndNil(FImage);
  FreeAndNil(FMetafileHeader);
  inherited Destroy;
end;

procedure TdxDocMetafileReader.Read(AReader: TBinaryReader; const AImageCreator: IdxDocOfficeImageCreator);
var
  ABuffer: TBytes;
  AMetafileStream, ACompressedStream, AUncompressedStream: TdxMemoryStream;
  AClipWidth, AClipHeight: Integer;
  ANativeMetafileImage: TdxOfficeImageReference;
begin
  FMetafileHeader.Free;
  FMetafileHeader := TdxDocMetafileHeader.FromStream(AReader);
  if not FMetafileHeader.Compressed then
  begin
    ABuffer := AReader.ReadBytes(FMetafileHeader.UncompressedSize);
    AMetafileStream := TdxMemoryStream.Create(ABuffer);
    try
      FImage := AImageCreator.CreateImage(AMetafileStream);
    finally
      AMetafileStream.Free;
    end;
  end
  else
  begin
    ABuffer := AReader.ReadBytes(FMetafileHeader.CompressedSize);
    CheckCompressedData(ABuffer);
    ACompressedStream := TdxMemoryStream.Create(ABuffer, 2, FMetafileHeader.CompressedSize - 6);
    ABuffer := nil;
    try
      AUncompressedStream := TdxMemoryStream.Create;
      try
        TdxInflateHelper.Decompress(ACompressedStream, ACompressedStream.Size, AUncompressedStream);
        Assert(AUncompressedStream.Size = FMetafileHeader.UncompressedSize);
        AUncompressedStream.Position := 0;
        AClipWidth := FMetafileHeader.Right - FMetafileHeader.Left;
        AClipHeight := FMetafileHeader.Bottom - FMetafileHeader.Top;
        ANativeMetafileImage := AImageCreator.CreateMetafile(AUncompressedStream, TdxMapMode.Anisotropic, AClipWidth, AClipHeight);

        if ANativeMetafileImage.Image is TdxOfficeMetafile then
          TdxOfficeMetafile(ANativeMetafileImage.Image).MetafileSizeInHundredthsOfMillimeter :=
            TSize.Create(FMetafileHeader.WidthInHundredthsOfMillimeter, FMetafileHeader.HeightInHundredthsOfMillimeter);

        FImage := ANativeMetafileImage;
      finally
        AUncompressedStream.Free;
      end;
    finally
      ACompressedStream.Free;
    end;
  end;
end;

procedure TdxDocMetafileReader.CheckCompressedData(const ABuffer: TBytes);
begin
  if (Length(ABuffer) < 3) or (ABuffer[0] and $0f <> 8) or (ABuffer[1] and $20 <> 0) then
    TdxOfficeArtExceptions.ThrowInvalidContent;
end;

{ TdxOfficeArtRecordHeader }

class function TdxOfficeArtRecordHeader.FromStream(AReader: TBinaryReader): TdxOfficeArtRecordHeader;
begin
  Result := TdxOfficeArtRecordHeader.Create;
  Result.Read(AReader);
end;

procedure TdxOfficeArtRecordHeader.Read(AReader: TBinaryReader);
var
  AFlags: Word;
begin
  AFlags := AReader.ReadUInt16;
  FVersion := AFlags and $000f;
  FInstanceInfo := (AFlags and $fff0) shr 4;
  FTypeCode := Integer(AReader.ReadUInt16);
  FLength := AReader.ReadInt32;
end;

procedure TdxOfficeArtRecordHeader.Write(AWriter: TBinaryWriter);
var
  AFlags: Word;
begin
  AFlags := FVersion or (FInstanceInfo shl 4);
  AWriter.Write(AFlags);
  AWriter.Write(Word(FTypeCode));
  AWriter.Write(FLength);
end;

{ TdxBlipBase }

constructor TdxBlipBase.Create(AImage: TdxOfficeImageReference);
begin
  TagValue := DefaultTagValue;
  SetImage(AImage);
end;

constructor TdxBlipBase.Create(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader; const AImageCreator: IdxDocOfficeImageCreator);
begin
  Assert(AReader <> nil);
  TagValue := DefaultTagValue;
  Read(AReader, AHeader, AImageCreator);
end;

constructor TdxBlipBase.Create;
begin
  TagValue := DefaultTagValue;
end;

destructor TdxBlipBase.Destroy;
begin
  FImage.Free;
  FImageBytesStream.Free;
  FMetafileHeader.Free;
  inherited Destroy;
end;

function TdxBlipBase.GetImageBytesStream: TBytesStream;
begin
  if FImageBytesStream = nil then
    FImageBytesStream := CreateImageBytesStream;
  Result := FImageBytesStream;
end;

function TdxBlipBase.GetImageBytesLength: Integer;
begin
  if FImageBytesStream = nil then
    FImageBytesStream := CreateImageBytesStream;

  if FImageBytesStream <> nil then
    Result := FImageBytesStream.Size
  else
    Result := 0;
end;

function TdxBlipBase.GetFormat: TdxOfficeImageFormat;
begin
  if FImage = nil then
    Result := TdxOfficeImageFormat.None
  else
    Result := FImage.RawFormat;
end;

procedure TdxBlipBase.SetImage(AImage: TdxOfficeImageReference);
begin
  if AImage = FImage then
    Exit;
  FImage.Free;
  FImage := AImage;
end;

procedure TdxBlipBase.SetImage(ABlip: TdxBlipBase);
begin
  SetImage(ABlip.Image.Clone);
  FMetafileHeader.Free;
  FMetafileHeader := ABlip.MetafileHeader.Clone;
  TagValue := ABlip.TagValue;
end;

procedure TdxBlipBase.SetImage(AReader: TdxDocMetafileReader);
begin
  SetImage(AReader.Image.Clone);
  FMetafileHeader.Free;
  FMetafileHeader := AReader.MetafileHeader.Clone;
end;

procedure TdxBlipBase.Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader;
  const AImageCreator: IdxDocOfficeImageCreator);
var
  AOffset, ASize: Integer;
  ABuffer: TBytes;
  AImageStream: TdxMemoryStream;
begin
  AOffset := CalcMD4MessageOffset(AHeader.InstanceInfo);
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soCurrent);
  ASize := AHeader.Length - AOffset;
  ABuffer := AReader.ReadBytes(ASize);
  AImageStream := TdxMemoryStream.Create(ABuffer);
  try
    LoadImageFromStream(AImageStream, AImageCreator);
  finally
    AImageStream.Free;
  end;
end;

function TdxBlipBase.Write(AWriter: TBinaryWriter): TdxMD4Byte16;
var
  AHeader: TdxOfficeArtRecordHeader;
  APosition, AEnd: Int64;
begin
  AHeader := CreateRecordHeader;
  try
    AHeader.Write(AWriter);
    APosition := AWriter.BaseStream.Position;
    AWriter.Seek(16 {MD4MessageDigest.Size}, TSeekOrigin.soCurrent);
    AWriter.Write(TagValue);
    Result := SaveImageToStream(AWriter);
    AEnd := AWriter.BaseStream.Position;
    AWriter.Seek(Integer(APosition), TSeekOrigin.soBeginning);
    AWriter.BaseStream.Write(Result, SizeOf(Result));
    AWriter.Seek(Integer(AEnd), TSeekOrigin.soBeginning);
  finally
    AHeader.Free;
  end;
end;

procedure TdxBlipBase.LoadImageFromStream(AImageStream: TdxMemoryStream; const AImageCreator: IdxDocOfficeImageCreator);
begin
  Image := AImageCreator.CreateImage(AImageStream);
end;

function TdxBlipBase.SaveImageToStream(AWriter: TBinaryWriter): TdxMD4Byte16;
var
  AStream: TBytesStream;
  ALength: Int64;
begin
  ZeroMemory(@Result, SizeOf(Result));

  AStream := ImageBytesStream;
  if AStream = nil then
    Exit;
  ALength := ImageBytesLength;

  dxMD4Calc(AStream.Memory, ALength, Result);
  AWriter.BaseStream.WriteBuffer(AStream.Memory^, ALength);
end;

function TdxBlipBase.GetSize: Integer;
begin
  Result := TdxOfficeArtRecordHeader.Size + MD4MessageSize + TagSize + ImageBytesLength;
end;

function TdxBlipBase.CreateMetafileHeader: TdxDocMetafileHeader;
var
  AImageSize, AWidthInPixels, AHeightInPixels: Integer;
begin
  Result := TdxDocMetafileHeader.Create;
  AImageSize := ImageBytesLength;
  AWidthInPixels := Image.SizeInPixels.Width;
  AHeightInPixels := Image.SizeInPixels.Height;
  Result.Compressed := False;
  Result.CompressedSize := AImageSize;
  Result.UncompressedSize := AImageSize;
  Result.Right := 0;
  Result.Left := AWidthInPixels;
  Result.Top := 0;
  Result.Bottom := AHeightInPixels;
  Result.HeightInEmus := AHeightInPixels;
  Result.WidthInEmus := AWidthInPixels;
end;

function TdxBlipBase.CalcMD4MessageOffset(AUidInfo: Integer): Integer;
begin
  if AUidInfo = SingleMD4Message then
    Exit(MD4MessageSize + TagSize);
  if AUidInfo = DoubleMD4Message then
    Exit((MD4MessageSize * 2) + TagSize);

  TdxOfficeArtExceptions.ThrowInvalidContent;
  Result := 0;
end;

function TdxBlipBase.CreateImageBytesStream: TBytesStream;
begin
  if FImage <> nil then
    Result := FImage.GetImageBytesStreamSafe(Format)
  else
    Result := nil;
end;

{ TdxFileBlipStoreEntry }

constructor TdxFileBlipStoreEntry.Create(AImage: TdxOfficeImageReference; AHasDelayedStream: Boolean);
begin
  FRefCount := 1;
  FEmbeddedBlip := TdxBlipFactory.CreateBlipFromImage(AImage);
  SetImage(FEmbeddedBlip);
  FBlipType := GetBlipType(AImage.RawFormat);
  FEmbeddedBlipSize := FEmbeddedBlip.GetSize;
  FHasDelayedStream := AHasDelayedStream;
end;

constructor TdxFileBlipStoreEntry.Create(AReader: TBinaryReader; AEmbeddedReader: TBinaryReader;
  AHeader: TdxOfficeArtRecordHeader; const AImageCreator: IdxDocOfficeImageCreator);
begin
  FRefCount := 1;
  FEmbeddedReader := AEmbeddedReader;
  Read(AReader, AHeader, AImageCreator);
end;

destructor TdxFileBlipStoreEntry.Destroy;
begin
  FEmbeddedBlip.Free;
  inherited Destroy;
end;

function TdxFileBlipStoreEntry.GetSingleMD4Message: Integer;
begin
  Result := FEmbeddedBlip.SingleMD4Message;
end;

function TdxFileBlipStoreEntry.GetDoubleMD4Message: Integer;
begin
  Result := FEmbeddedBlip.DoubleMD4Message;
end;

procedure TdxFileBlipStoreEntry.Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader;
  const AImageCreator: IdxDocOfficeImageCreator);
begin
  AReader.BaseStream.Seek(UnusedDataSize, TSeekOrigin.soCurrent);
  ReadCore(AReader);
  ReadName(AReader);
  ReadEmbeddedBlip(AReader, AImageCreator);
end;

procedure TdxFileBlipStoreEntry.ReadCore(AReader: TBinaryReader);
begin
  FRefCount := AReader.ReadInt32;
  FEmbeddedBlipOffset := AReader.ReadUInt32;
  AReader.BaseStream.Seek(1, TSeekOrigin.soCurrent);
end;

procedure TdxFileBlipStoreEntry.ReadName(AReader: TBinaryReader);
var
  ANameLength: Byte;
  ABytes: TBytes;
begin
  ANameLength := AReader.ReadByte;
  AReader.BaseStream.Seek(2, TSeekOrigin.soCurrent);
  if ANameLength <> 0 then
  begin
    ABytes := AReader.ReadBytes(ANameLength - 2);
    FName := TdxEncoding.Unicode.GetString(ABytes, 0, Length(ABytes));
    AReader.BaseStream.Seek(2, TSeekOrigin.soCurrent);
  end;
end;

procedure TdxFileBlipStoreEntry.ReadEmbeddedBlip(AReader: TBinaryReader;
  const AImageCreator: IdxDocOfficeImageCreator);
var
  AEmbeddeBlipHeader: TdxOfficeArtRecordHeader;
begin
  if (FEmbeddedBlipOffset = EmptySlotOffset) or (ReferenceCount = 0) then
    Exit;
  if EmbeddedReader <> AReader then
    EmbeddedReader.BaseStream.Seek(FEmbeddedBlipOffset, TSeekOrigin.soBeginning);
  AEmbeddeBlipHeader := TdxOfficeArtRecordHeader.FromStream(FEmbeddedReader);
  try
    FEmbeddedBlip := TdxBlipFactory.CreateBlipFromStream(FEmbeddedReader, AEmbeddeBlipHeader, AImageCreator);
  finally
    AEmbeddeBlipHeader.Free;
  end;
  if (FEmbeddedBlip <> nil) and (FEmbeddedBlip.Image <> nil) then
    SetImage(FEmbeddedBlip)
  else
    Image := AImageCreator.CreateImage(TdxUriOfficeImage.PlaceHolder.Clone);
end;

function TdxFileBlipStoreEntry.Write(AWriter: TBinaryWriter): TdxMD4Byte16;
var
  APosition, AEnd: Int64;
begin
  Assert(AWriter <> nil, 'writer');
  APosition := WriteCore(AWriter);
  AWriter.Write(DefaultDelay);
  WriteName(AWriter);
  Result := FEmbeddedBlip.Write(AWriter);
  AEnd := AWriter.BaseStream.Position;
  AWriter.Seek(APosition, TSeekOrigin.soBeginning);
  AWriter.BaseStream.Write(Result, SizeOf(Result));
  AWriter.Seek(AEnd, TSeekOrigin.soBeginning);
end;

procedure TdxFileBlipStoreEntry.Write(AWriter: TBinaryWriter; AEmbeddedWriter: TBinaryWriter);
var
  APosition, AEnd: Int64;
  ADigest: TdxMD4Byte16;
begin
  Assert(AWriter <> nil, 'writer');
  Assert(AEmbeddedWriter <> nil, 'embeddedWriter');
  APosition := WriteCore(AWriter);
  if HasDelayedStream then
    AWriter.Write(Integer(AEmbeddedWriter.BaseStream.Position))
  else
    AWriter.Write(Integer(0));
  WriteName(AWriter);
  ADigest := FEmbeddedBlip.Write(AEmbeddedWriter);
  AEnd := AWriter.BaseStream.Position;
  AWriter.Seek(APosition, TSeekOrigin.soBeginning);
  AWriter.BaseStream.Write(ADigest, SizeOf(ADigest));
  AWriter.Seek(AEnd, TSeekOrigin.soBeginning);
end;

function TdxFileBlipStoreEntry.WriteCore(AWriter: TBinaryWriter): Int64;
var
  AHeader: TdxOfficeArtRecordHeader;
begin
  AHeader := CreateRecordHeader;
  try
    AHeader.Write(AWriter);
  finally
    AHeader.Free;
  end;
  AWriter.Write(FBlipType);
  AWriter.Write(FBlipType);
  Result := AWriter.BaseStream.Position;
  AWriter.Seek(16 {MD4MessageDigest.Size}, TSeekOrigin.soCurrent);
  AWriter.Write(Tag);
  AWriter.Write(EmbeddedBlipSize);
  AWriter.Write(ReferenceCount);
end;

procedure TdxFileBlipStoreEntry.WriteName(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(1, TSeekOrigin.soCurrent);
  AWriter.Write(DefaultNameLength);
  AWriter.BaseStream.Seek(2, TSeekOrigin.soCurrent);
end;

function TdxFileBlipStoreEntry.CreateRecordHeader: TdxOfficeArtRecordHeader;
begin
  Result := TdxOfficeArtRecordHeader.Create;
  Result.Version := HeaderVersion;
  Result.InstanceInfo := FBlipType;
  Result.TypeCode := TdxBlipTypeCodes.FileBlipStoreEntry;
  if HasDelayedStream then
    Result.Length := BlipStoreHeaderSize
  else
    Result.Length := BlipStoreHeaderSize + EmbeddedBlipSize;
end;

function TdxFileBlipStoreEntry.GetBlipType(AFormat: TdxOfficeImageFormat): Byte;
begin
  case AFormat of
    TdxOfficeImageFormat.Bmp:
      Result := TdxBlipTypes.Png;
    TdxOfficeImageFormat.MemoryBmp:
      Result := TdxBlipTypes.Png;
    TdxOfficeImageFormat.Gif:
      Result := TdxBlipTypes.Png;
    TdxOfficeImageFormat.Emf:
      Result := TdxBlipTypes.Emf;
    TdxOfficeImageFormat.Jpeg:
      Result := TdxBlipTypes.Jpeg;
    TdxOfficeImageFormat.None:
      Result := TdxBlipTypes.Unknown;
    TdxOfficeImageFormat.Png:
      Result := TdxBlipTypes.Png;
    TdxOfficeImageFormat.Tiff:
      Result := TdxBlipTypes.Tiff;
    TdxOfficeImageFormat.Wmf:
      Result := TdxBlipTypes.Wmf;
    else
      Result := TdxBlipTypes.Unknown;
  end;
end;

function TdxFileBlipStoreEntry.GetSize: Integer;
begin
  if HasDelayedStream then
    Result := TdxOfficeArtRecordHeader.Size + BlipStoreHeaderSize
  else
    Result := TdxOfficeArtRecordHeader.Size + BlipStoreHeaderSize + EmbeddedBlipSize;
end;

{ TdxBlipFactory }

class constructor TdxBlipFactory.Initialize;
begin
  FTypeCodeTranslationTable := TDictionary<Integer, TdxBlipBaseClass>.Create(9);
  FTypeCodeTranslationTable.Add(TdxBlipTypeCodes.BlipEmf, TdxBlipEmf);
  FTypeCodeTranslationTable.Add(TdxBlipTypeCodes.BlipWmf, TdxBlipWmf);
  FTypeCodeTranslationTable.Add(TdxBlipTypeCodes.BlipPict, TdxBlipPict);
  FTypeCodeTranslationTable.Add(TdxBlipTypeCodes.BlipJpeg, TdxBlipJpeg);
  FTypeCodeTranslationTable.Add(TdxBlipTypeCodes.BlipPng, TdxBlipPng);
  FTypeCodeTranslationTable.Add(TdxBlipTypeCodes.BlipDib, TdxBlipDib);
  FTypeCodeTranslationTable.Add(TdxBlipTypeCodes.BlipTiff, TdxBlipTiff);
  FTypeCodeTranslationTable.Add(TdxBlipTypeCodes.BlipJpeg2, TdxBlipJpeg);

  FImageFormatTranslationTable := TDictionary<TdxOfficeImageFormat, TdxBlipBaseClass>.Create;
  FImageFormatTranslationTable.Add(TdxOfficeImageFormat.Bmp, TdxBlipDib);
  FImageFormatTranslationTable.Add(TdxOfficeImageFormat.MemoryBmp, TdxBlipDib);
  FImageFormatTranslationTable.Add(TdxOfficeImageFormat.Emf, TdxBlipEmf);
  FImageFormatTranslationTable.Add(TdxOfficeImageFormat.Jpeg, TdxBlipJpeg);
  FImageFormatTranslationTable.Add(TdxOfficeImageFormat.Png, TdxBlipPng);
  FImageFormatTranslationTable.Add(TdxOfficeImageFormat.Gif, TdxBlipPng);
  FImageFormatTranslationTable.Add(TdxOfficeImageFormat.Tiff, TdxBlipTiff);
  FImageFormatTranslationTable.Add(TdxOfficeImageFormat.Wmf, TdxBlipWmf);
end;

class destructor TdxBlipFactory.Finalize;
begin
  FTypeCodeTranslationTable.Free;
  FImageFormatTranslationTable.Free;
end;

class function TdxBlipFactory.ReadAllBlips(AReader: TBinaryReader; AEndPosition: Int64;
  const AImageCreator: IdxDocOfficeImageCreator): TdxList<TdxBlipBase>;
begin
  Result := ReadAllBlips(AReader, AReader, AEndPosition, AImageCreator);
end;

class function TdxBlipFactory.ReadAllBlips(AReader: TBinaryReader;
  AEmbeddedReader: TBinaryReader; AEndPosition: Int64; const AImageCreator: IdxDocOfficeImageCreator): TdxList<TdxBlipBase>;
var
  ABlipHeader: TdxOfficeArtRecordHeader;
  AExpectedBlipEnd: Int64;
  ABlip: TdxBlipBase;
begin
  Result := TdxObjectList<TdxBlipBase>.Create;
  while AReader.BaseStream.Position < AEndPosition do
  begin
    ABlipHeader := TdxOfficeArtRecordHeader.FromStream(AReader);
    try
      AExpectedBlipEnd := AReader.BaseStream.Position + ABlipHeader.Length;
      ABlip := CreateBlipFromStream(AReader, AEmbeddedReader, ABlipHeader, AImageCreator);
      if ABlip <> nil then
        Result.Add(ABlip);
    finally
      ABlipHeader.Free;
    end;
    AReader.BaseStream.Position := Max(AExpectedBlipEnd, AReader.BaseStream.Position);
  end;
  Assert(AReader.BaseStream.Position = AEndPosition);
end;

class function TdxBlipFactory.CreateBlipFromStream(AReader: TBinaryReader;
  AHeader: TdxOfficeArtRecordHeader; const AImageCreator: IdxDocOfficeImageCreator): TdxBlipBase;
begin
  Result := CreateBlipFromStreamCore(AReader, AReader, AHeader, AImageCreator);
end;

class function TdxBlipFactory.CreateBlipFromStream(AReader: TBinaryReader; AEmbeddedReader: TBinaryReader;
  AHeader: TdxOfficeArtRecordHeader; const AImageCreator: IdxDocOfficeImageCreator): TdxBlipBase;
begin
  Result := CreateBlipFromStreamCore(AReader, AEmbeddedReader, AHeader, AImageCreator);
end;

class function TdxBlipFactory.CreateBlipFromStreamCore(AReader: TBinaryReader;
  AEmbeddedReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader;
  const AImageCreator: IdxDocOfficeImageCreator): TdxBlipBase;
var
  ATypeCode: Integer;
  ABlipClass: TdxBlipBaseClass;
begin
  ATypeCode := AHeader.TypeCode;
  if ATypeCode = TdxBlipTypeCodes.FileBlipStoreEntry then
    Exit(TdxFileBlipStoreEntry.Create(AReader, AEmbeddedReader, AHeader, AImageCreator));

  if FTypeCodeTranslationTable.TryGetValue(ATypeCode, ABlipClass) then
    Result := ABlipClass.Create(AReader, AHeader, AImageCreator)
  else
    Result := nil;
end;

class function TdxBlipFactory.CreateBlipFromImage(AImage: TdxOfficeImageReference): TdxBlipBase;
var
  ABlipClass: TdxBlipBaseClass;
begin
  if FImageFormatTranslationTable.TryGetValue(AImage.RawFormat, ABlipClass) then
    Result := ABlipClass.Create(AImage.Clone)
  else
    Result := TdxBlipDib.Create(AImage.Clone);
end;

{ TdxBlipMetaFile }

procedure TdxBlipMetaFile.Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader;
  const AImageCreator: IdxDocOfficeImageCreator);
var
  AEnd: Int64;
  AUidOffset: Integer;
  AMetafileReader: TdxDocMetafileReader;
begin
  AEnd := AReader.BaseStream.Position + AHeader.Length;
  AUidOffset := CalcMD4MessageOffset(AHeader.InstanceInfo);
  AReader.BaseStream.Seek(AUidOffset, TSeekOrigin.soCurrent);
  AMetafileReader := TdxDocMetafileReader.Create;
  try
    AMetafileReader.Read(AReader, AImageCreator);
    if (AReader.BaseStream.Position <> AEnd) and (AReader.BaseStream.Size >= AEnd) then
      AReader.BaseStream.Seek(AEnd, TSeekOrigin.soBeginning);
    SetImage(AMetafileReader);
  finally
    AMetafileReader.Free;
  end;
end;

function TdxBlipMetaFile.Write(AWriter: TBinaryWriter): TdxMD4Byte16;
var
  AHeader: TdxOfficeArtRecordHeader;
  AMetafileHeader: TdxDocMetafileHeader;
  ABuf: TBytes;
  ABytesToWrite, ACount: Integer;
  AStream: TStream;
begin
  AHeader := CreateRecordHeader;
  try
    AHeader.Write(AWriter);
  finally
    AHeader.Free;
  end;
  AWriter.BaseStream.WriteBuffer(FMd4Digest, SizeOf(FMd4Digest));
  AMetafileHeader := CreateMetafileHeader;
  try
    AMetafileHeader.Write(AWriter);
  finally
    AMetafileHeader.Free;
  end;
  SetLength(ABuf, BufferSize);
  ABytesToWrite := ImageBytesLength;
  AStream := ImageBytesStream;
  while ABytesToWrite > 0 do
  begin
    ACount := Min(ABytesToWrite, BufferSize);
    AStream.Read(ABuf[0], ACount);
    AWriter.Write(ABuf, 0, ACount);
    Dec(ABytesToWrite, ACount);
  end;
  Result := FMd4Digest;
end;

function TdxBlipMetaFile.CalcMD4MessageOffset(AUidInfo: Integer): Integer;
begin
  if AUidInfo = SingleMD4Message then
    Exit(MD4MessageSize);
  if AUidInfo = DoubleMD4Message then
    Exit(2 * MD4MessageSize);

  TdxOfficeArtExceptions.ThrowInvalidContent;
  Result := 0;
end;

function TdxBlipMetaFile.GetSize: Integer;
begin
  Result := TdxOfficeArtRecordHeader.Size + MD4MessageSize + MetafileHeaderSize + ImageBytesLength;
end;

function TdxBlipMetaFile.CreateMetafileHeader: TdxDocMetafileHeader;
var
  AImageSize, AWidthInPixels, AHeightInPixels: Integer;
begin
  Result := TdxDocMetafileHeader.Create;
  AImageSize := ImageBytesLength;
  AWidthInPixels := Image.SizeInPixels.Width;
  AHeightInPixels := Image.SizeInPixels.Height;
  Result.Compressed := AImageSize <> FUncompressedSize;
  Result.CompressedSize := AImageSize;
  Result.UncompressedSize := FUncompressedSize;
  Result.Right := 0;
  Result.Left := AWidthInPixels;
  Result.Top := 0;
  Result.Bottom := AHeightInPixels;
  Result.HeightInEmus := AHeightInPixels;
  Result.WidthInEmus := AWidthInPixels;
end;

function TdxBlipMetaFile.CreateImageBytesStream: TBytesStream;
var
  AUncompressedStream: TBytesStream;
begin
  AUncompressedStream := inherited CreateImageBytesStream;
  FUncompressedSize := Integer(AUncompressedStream.Size);
  dxMD4Calc(AUncompressedStream.Memory, FUncompressedSize, FMd4Digest);
  if FUncompressedSize <= MaxUncompressedSize then
    Exit(AUncompressedStream);
  try
    Result := TBytesStream.Create;
    ZCompressStream(AUncompressedStream, Result, zcFastest);
    Result.Position := 0;
  finally
    AUncompressedStream.Free;
  end;
end;

{ TdxBlipEmf }

function TdxBlipEmf.GetSingleMD4Message: Integer;
begin
  Result := TdxMessageDigestCodes.EmfSingleMessage;
end;

function TdxBlipEmf.GetDoubleMD4Message: Integer;
begin
  Result := TdxMessageDigestCodes.EmfDoubleMessage;
end;

function TdxBlipEmf.CreateRecordHeader: TdxOfficeArtRecordHeader;
begin
  Result := TdxOfficeArtRecordHeader.Create;
  Result.Version := 0;
  Result.InstanceInfo := TdxMessageDigestCodes.EmfSingleMessage;
  Result.TypeCode := TdxBlipTypeCodes.BlipEmf;
  Result.Length := MD4MessageSize + MetafileHeaderSize + ImageBytesLength;
end;

{ TdxBlipWmf }

function TdxBlipWmf.GetSingleMD4Message: Integer;
begin
  Result := TdxMessageDigestCodes.WmfSingleMessage;
end;

function TdxBlipWmf.GetDoubleMD4Message: Integer;
begin
  Result := TdxMessageDigestCodes.WmfDoubleMessage;
end;

function TdxBlipWmf.CreateRecordHeader: TdxOfficeArtRecordHeader;
begin
  Result := TdxOfficeArtRecordHeader.Create;
  Result.Version := 0;
  Result.InstanceInfo := TdxMessageDigestCodes.WmfSingleMessage;
  Result.TypeCode := TdxBlipTypeCodes.BlipWmf;
  Result.Length := MD4MessageSize + MetafileHeaderSize + ImageBytesLength;
end;

{ TdxBlipPict }

function TdxBlipPict.GetSingleMD4Message: Integer;
begin
  Result := TdxMessageDigestCodes.PictSingleMessage;
end;

function TdxBlipPict.GetDoubleMD4Message: Integer;
begin
  Result := TdxMessageDigestCodes.PictDoubleMessage;
end;

procedure TdxBlipPict.Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader;
  const AImageCreator: IdxDocOfficeImageCreator);
var
  AUidOffset: Integer;
  AMetafileHeader: TdxDocMetafileHeader;
begin
  AUidOffset := CalcMD4MessageOffset(AHeader.InstanceInfo);
  AReader.BaseStream.Seek(AUidOffset, TSeekOrigin.soCurrent);
  AMetafileHeader := TdxDocMetafileHeader.FromStream(AReader);
  try
    if AMetafileHeader.Compressed then
      AReader.BaseStream.Seek(AMetafileHeader.CompressedSize, TSeekOrigin.soCurrent)
    else
      AReader.BaseStream.Seek(AMetafileHeader.UncompressedSize, TSeekOrigin.soCurrent);
  finally
    AMetafileHeader.Free;
  end;
end;

function TdxBlipPict.CreateRecordHeader: TdxOfficeArtRecordHeader;
begin
  Result := TdxOfficeArtRecordHeader.Create;
  Result.Version := 0;
  Result.InstanceInfo := TdxMessageDigestCodes.PictSingleMessage;
  Result.TypeCode := TdxBlipTypeCodes.BlipPict;
  Result.Length := MD4MessageSize + MetafileHeaderSize + ImageBytesLength;
end;

{ TdxBlipPng }

function TdxBlipPng.GetSingleMD4Message: Integer;
begin
  Result := TdxMessageDigestCodes.PngSingleMessage;
end;

function TdxBlipPng.GetDoubleMD4Message: Integer;
begin
  Result := TdxMessageDigestCodes.PngDoubleMessage;
end;

function TdxBlipPng.GetFormat: TdxOfficeImageFormat;
begin
  Result := TdxOfficeImageFormat.Png;
end;

function TdxBlipPng.CreateRecordHeader: TdxOfficeArtRecordHeader;
begin
  Result := TdxOfficeArtRecordHeader.Create;
  Result.Version := 0;
  Result.InstanceInfo := TdxMessageDigestCodes.PngSingleMessage;
  Result.TypeCode := TdxBlipTypeCodes.BlipPng;
  Result.Length := MD4MessageSize + TagSize + ImageBytesLength;
end;

{ TdxBlipTiff }

function TdxBlipTiff.GetSingleMD4Message: Integer;
begin
  Result := TdxMessageDigestCodes.TiffSingleMessage;
end;

function TdxBlipTiff.GetDoubleMD4Message: Integer;
begin
  Result := TdxMessageDigestCodes.TiffDoubleMessage;
end;

function TdxBlipTiff.CreateRecordHeader: TdxOfficeArtRecordHeader;
begin
  Result := TdxOfficeArtRecordHeader.Create;
  Result.Version := 0;
  Result.InstanceInfo := TdxMessageDigestCodes.TiffSingleMessage;
  Result.TypeCode := TdxBlipTypeCodes.BlipTiff;
  Result.Length := MD4MessageSize + TagSize + ImageBytesLength;
end;

{ TdxBlipDib }

function TdxBlipDib.GetSingleMD4Message: Integer;
begin
  Result := TdxMessageDigestCodes.DibSingleMessage;
end;

function TdxBlipDib.GetDoubleMD4Message: Integer;
begin
  Result := TdxMessageDigestCodes.DibDoubleMessage;
end;

function TdxBlipDib.GetFormat: TdxOfficeImageFormat;
begin
  Result := TdxOfficeImageFormat.Png;
end;

procedure TdxBlipDib.LoadImageFromStream(AImageStream: TdxMemoryStream;
  const AImageCreator: IdxDocOfficeImageCreator);
var
  AReader: TBinaryReader;
  AHeader: TdxDibHeader;
  ABytesInLine: Integer;
  ABMPStream: TdxMemoryStream;
begin
  AReader := TBinaryReader.Create(AImageStream);
  try
    AHeader := TdxDibHeader.FromStream(AReader);
    try
      ABytesInLine := AHeader.BitCount * AHeader.Width;

      if ABytesInLine mod DwordSize = 0 then
        ABytesInLine := ABytesInLine div 8
      else
      begin
        ABytesInLine := ABytesInLine div 32;
        Inc(ABytesInLine);
        ABytesInLine := ABytesInLine * 4;
      end;
      AImageStream.Seek(0, TSeekOrigin.soBeginning);

      ABMPStream := TdxDibHelper.CreateBmpFileStreamForDib(AImageStream, AHeader.Height, ABytesInLine);
      try
        Image := AImageCreator.CreateImage(ABMPStream);
      finally
        ABMPStream.Free;
      end;
    finally
      AHeader.Free;
    end;
  finally
    AReader.Free;
  end;
end;

function TdxBlipDib.GetSize: Integer;
begin
  Result := TdxOfficeArtRecordHeader.Size + MD4MessageSize + TagSize + ImageBytesLength;
end;

function TdxBlipDib.CreateRecordHeader: TdxOfficeArtRecordHeader;
begin
  Result := TdxOfficeArtRecordHeader.Create;
  Result.Version := 0;
  Result.InstanceInfo := TdxMessageDigestCodes.PngSingleMessage;
  Result.TypeCode := TdxBlipTypeCodes.BlipPng;
  Result.Length := MD4MessageSize + TagSize + ImageBytesLength;
end;

{ TdxBlipJpeg }

function TdxBlipJpeg.GetSingleMD4Message: Integer;
begin
  Result := TdxMessageDigestCodes.JpegRGBSingleMessage;
end;

function TdxBlipJpeg.GetDoubleMD4Message: Integer;
begin
  Result := TdxMessageDigestCodes.JpegRGBDoubleMessage;
end;

function TdxBlipJpeg.CalcMD4MessageOffset(AUidInfo: Integer): Integer;
begin
  if (AUidInfo = TdxMessageDigestCodes.JpegRGBSingleMessage) or (AUidInfo = TdxMessageDigestCodes.JpegCMYKSingleMessage) then
    Exit(MD4MessageSize + TagSize);
  if (AUidInfo = TdxMessageDigestCodes.JpegRGBDoubleMessage) or (AUidInfo = TdxMessageDigestCodes.JpegCMYKDoubleMessage) then
    Exit((MD4MessageSize * 2) + TagSize);

  TdxOfficeArtExceptions.ThrowInvalidContent;
  Result := 0;
end;

function TdxBlipJpeg.CreateRecordHeader: TdxOfficeArtRecordHeader;
begin
  Result := TdxOfficeArtRecordHeader.Create;
  Result.Version := 0;
  Result.InstanceInfo := TdxMessageDigestCodes.JpegRGBSingleMessage;
  Result.TypeCode := TdxBlipTypeCodes.BlipJpeg;
  Result.Length := MD4MessageSize + TagSize + ImageBytesLength;
end;

end.
