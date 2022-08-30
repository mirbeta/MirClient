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

unit dxRichEdit.Import.Rtf.DestinationPicture;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, Windows, SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.Utils.CheckSumStream,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Import.Rtf,
  dxRichEdit.Import.Rtf.DestinationPieceTable,
  dxRichEdit.Import.Rtf.DestinationHexContent,
  dxRichEdit.Import.Rtf.DestinationShape,
  dxRichEdit.DocumentModel.UnitConverter;

type
  TdxPictureSourceType = (Emf, Png, Jpeg, Mac, PmmMetafile, Wmf, WindowsDib, WindowsBmp);

  { TdxPictureDestinationInfo }

  TdxPictureDestinationInfo = class
  strict private
    FDataStream: TdxCrc32Stream;
    FPictureSourceType: TdxPictureSourceType;
    FPictureWidth: Integer;
    FPictureHeight: Integer;
    FDesiredPictureWidth: Integer;
    FDesiredPictureHeight: Integer;
    FPictureStream: TBytesStream;
    FProperties: TdxRtfShapeProperties;
    FBmpBitsPerPixel: Integer;
    FBmpColorPlanes: Integer;
    FBmpBytesInLine: Integer;
    FScaleX: Integer;
    FScaleY: Integer;
    FImageUri: string;
    FWmfMapMode: TdxMapMode;
  private
    function GetDataStream: TdxCrc32Stream;
    function GetDataCrc32: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ClosePictureStream;

    property BmpBitsPerPixel: Integer read FBmpBitsPerPixel write FBmpBitsPerPixel;
    property BmpColorPlanes: Integer read FBmpColorPlanes write FBmpColorPlanes;
    property BmpBytesInLine: Integer read FBmpBytesInLine write FBmpBytesInLine;
    property DataCrc32: Integer read GetDataCrc32;
    property ScaleX: Integer read FScaleX write FScaleX;
    property ScaleY: Integer read FScaleY write FScaleY;
    property ImageUri: string read FImageUri write FImageUri;
    property PictureSourceType: TdxPictureSourceType read FPictureSourceType write FPictureSourceType;
    property PictureWidth: Integer read FPictureWidth write FPictureWidth;
    property PictureHeight: Integer read FPictureHeight write FPictureHeight;
    property Properties: TdxRtfShapeProperties read FProperties;
    property DataStream: TdxCrc32Stream read GetDataStream;
    property DesiredPictureWidth: Integer read FDesiredPictureWidth write FDesiredPictureWidth;
    property DesiredPictureHeight: Integer read FDesiredPictureHeight write FDesiredPictureHeight;
    property PictureStream: TBytesStream read FPictureStream;
    property WmfMapMode: TdxMapMode read FWmfMapMode write FWmfMapMode;
  end;

  { TdxPictureDestination }

  TdxPictureDestination = class(TdxHexContentDestination)
  public type
    { TdxRtfPictureUnitsConverter }

    TdxRtfPictureUnitsConverter = class abstract
    public
      function UnitsToTwips(Value: Integer): Integer; virtual; abstract;
      function UnitsToModelUnits(Value: Integer; AUnitConverter: TdxDocumentModelUnitConverter): Integer; virtual; abstract;
    end;

    { TdxRtfPixelsToTwipsConverter }

    TdxRtfPixelsToTwipsConverter = class(TdxRtfPictureUnitsConverter)
    strict private
      FDpi: Integer;
    public
      constructor Create(ADpi: Integer);
      function UnitsToTwips(Value: Integer): Integer; override;
      function UnitsToModelUnits(Value: Integer; AUnitConverter: TdxDocumentModelUnitConverter): Integer; override;
    end;

    { TdxRtfHundredthsOfMillimeterConverter }

    TdxRtfHundredthsOfMillimeterConverter = class(TdxRtfPictureUnitsConverter)
    public
      function UnitsToTwips(Value: Integer): Integer; override;
      function UnitsToModelUnits(Value: Integer; AUnitConverter: TdxDocumentModelUnitConverter): Integer; override;
    end;

  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class var FRtfHundredthsOfMillimeterConverter: TdxRtfPictureUnitsConverter;
    class var FRtfPixelsConverter: TdxRtfPictureUnitsConverter;

    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
    function GetBmpBitsPerPixel: Integer;
    function GetBmpBytesInLine: Integer;
    function GetBmpColorPlanes: Integer;
    procedure SetBmpBitsPerPixel(const Value: Integer);
    procedure SetBmpBytesInLine(const Value: Integer);
    procedure SetBmpColorPlanes(const Value: Integer);
  strict private
    FInfo: TdxPictureDestinationInfo;
    FIsInfoOwner: Boolean;
    FOldDecoder: TdxCodePageCharacterDecoder;
    class procedure EmfFileKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PngFileKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure JpegFileKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure MacFileKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure WindowsMetafileKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DeviceIndependentBitmapFileKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DeviceDependentBitmapFileKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure BitmapBitsPerPixelKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure BitmapPlanesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure BitmapBytesInLineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PictureWidthKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure PictureHeightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure PictureGoalWidthKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PictureGoalHeightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure HorizontalScalingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure VerticalScalingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PicScaledKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure TopCropKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure BottomCropKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure LeftCropKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure RightCropKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure BitmapMetafileKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure BitsPerPixelBitmapMetafileKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DxImageUriHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapePropertiesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;

    class function FillBytesToConvertFromShortIntToLongInt(AParameterValue: Word): Integer; inline; static;
    procedure LoadBitmap(AInfo: TdxRtfImageInfo);
    procedure LoadDib(AInfo: TdxRtfImageInfo);
    function LoadPicture: TdxRtfImageInfo;
    procedure LoadMetafile(AInfo: TdxRtfImageInfo);
    procedure LoadImage(AInfo: TdxRtfImageInfo);
    procedure LoadImageInUnits(AImageInfo: TdxRtfImageInfo; AUnitsConverter: TdxRtfPictureUnitsConverter);
    procedure LoadMetafileImageInUnits(AImageInfo: TdxRtfImageInfo; AUnitsConverter: TdxRtfPictureUnitsConverter);
    procedure ValidateImageSize(AImage: TdxOfficeImageReference);

    function GetDataStream: TdxCrc32Stream;
    function GetPictureSourceType: TdxPictureSourceType;
    function GetWmfMapMode: TdxMapMode;
    procedure SetInfo(const Value: TdxPictureDestinationInfo);
    procedure SetPictureSourceType(const Value: TdxPictureSourceType); inline;
    procedure SetWmfMapMode(const Value: TdxMapMode); inline;
  protected
    procedure ProcessBinCharCore(AChar: Char); override;
    procedure LoadPictureCore(AInfo: TdxRtfImageInfo);
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;

    property BmpBitsPerPixel: Integer read GetBmpBitsPerPixel write SetBmpBitsPerPixel;
    property BmpBytesInLine: Integer read GetBmpBytesInLine write SetBmpBytesInLine;
    property BmpColorPlanes: Integer read GetBmpColorPlanes write SetBmpColorPlanes;
    property DataStream: TdxCrc32Stream read GetDataStream;
    property PictureSourceType: TdxPictureSourceType read GetPictureSourceType write SetPictureSourceType;
    property WmfMapMode: TdxMapMode read GetWmfMapMode write SetWmfMapMode;
  public
    constructor Create(AImporter: TdxRtfImporter); override;
    destructor Destroy; override;
    procedure BeforePopRtfState; override;

    function GetImageInfo: TdxRtfImageInfo;

    property Info: TdxPictureDestinationInfo read FInfo;
  end;

  { TdxDxImageUriDestination }

  TdxDxImageUriDestination = class(TdxStringValueDestination)
  strict private
    FPictureDestination: TdxPictureDestination;
    FStringBuilder: TStringBuilder;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    function CreateEmptyClone: TdxStringValueDestination; override;
    procedure ProcessCharCore(AChar: Char); override;
    function ProcessKeywordCore(const AKeyword: string;
      AParameterValue: Integer; AHasParameter: Boolean): Boolean; override;
    function GetValue: string; override;
  public
    constructor Create(AImporter: TdxRtfImporter; APictureDestination: TdxPictureDestination); reintroduce;
    destructor Destroy; override;
    procedure AfterPopRtfState; override;
  end;

implementation

uses
  Variants, Math, cxVariants, cxGeometry,
  dxRichEdit.Import.Rtf.DestinationSkip;

type
  TdxRtfFormattingInfoAccess = class(TdxRtfFormattingInfo);

{ TdxPictureDestinationInfo }

constructor TdxPictureDestinationInfo.Create;
begin
  inherited Create;
  FPictureSourceType := TdxPictureSourceType.WindowsBmp;
  FWmfMapMode := TdxMapMode.Text;
  FPictureWidth := -1;
  FPictureHeight := -1;
  FDesiredPictureWidth := -1;
  FDesiredPictureHeight := -1;
  FPictureStream := TBytesStream.Create;
  FDataStream := TdxCrc32Stream.Create(FPictureStream);
  FBmpBitsPerPixel := 1;
  FBmpColorPlanes := 1;
  FScaleX := 100;
  FScaleY := 100;
  FProperties := TdxRtfShapeProperties.Create;
end;

destructor TdxPictureDestinationInfo.Destroy;
begin
  FreeAndNil(FProperties);
  FreeAndNil(FPictureStream);
  FreeAndNil(FDataStream);
  inherited Destroy;
end;

procedure TdxPictureDestinationInfo.ClosePictureStream;
begin
  FreeAndNil(FDataStream);
  FreeAndNil(FPictureStream);
end;

function TdxPictureDestinationInfo.GetDataCrc32: Integer;
begin
  Result :=  DataStream.WriteCheckSum;
end;

function TdxPictureDestinationInfo.GetDataStream: TdxCrc32Stream;
begin
  Result := FDataStream;
end;

{ TdxPictureDestination }

constructor TdxPictureDestination.Create(
  AImporter: TdxRtfImporter);
begin
  inherited Create(AImporter);
  FInfo := TdxPictureDestinationInfo.Create;
  FIsInfoOwner := True;
  FOldDecoder := Importer.Position.RtfFormattingInfo.Decoder;
  TdxRtfFormattingInfoAccess(Importer.Position.RtfFormattingInfo).SetDecoder(TdxEmptyCharacterDecoder.Create, False);
end;

destructor TdxPictureDestination.Destroy;
begin
  if FIsInfoOwner then
    FreeAndNil(FInfo);
  FreeAndNil(FOldDecoder);
  inherited Destroy;
end;

function TdxPictureDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxPictureDestination.Create(Importer);
  TdxPictureDestination(Result).SetInfo(Info);
end;

class function TdxPictureDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class function TdxPictureDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('emfblip', EmfFileKeywordHandler);
  Result.Add('pngblip', PngFileKeywordHandler);
  Result.Add('jpegblip', JpegFileKeywordHandler);
  Result.Add('macpict', MacFileKeywordHandler);
  Result.Add('wmetafile', WindowsMetafileKeywordHandler);
  Result.Add('dibitmap', DeviceIndependentBitmapFileKeywordHandler);
  Result.Add('wbitmap', DeviceDependentBitmapFileKeywordHandler);

  Result.Add('wbmbitspixel', BitmapBitsPerPixelKeywordHandler);
  Result.Add('wbmplanes', BitmapPlanesKeywordHandler);
  Result.Add('wbmwidthbytes', BitmapBytesInLineKeywordHandler);

  Result.Add('picw', PictureWidthKeywordHandler);
  Result.Add('pich', PictureHeightKeywordHandler);
  Result.Add('picwgoal', PictureGoalWidthKeywordHandler);
  Result.Add('pichgoal', PictureGoalHeightKeywordHandler);
  Result.Add('picscalex', HorizontalScalingKeywordHandler);
  Result.Add('picscaley', VerticalScalingKeywordHandler);
  Result.Add('picscaled', PicScaledKeywordHandler);
  Result.Add('piccropt', TopCropKeywordHandler);
  Result.Add('piccropb', BottomCropKeywordHandler);
  Result.Add('piccropr', LeftCropKeywordHandler);
  Result.Add('piccropl', RightCropKeywordHandler);

  Result.Add('picbmp', BitmapMetafileKeywordHandler);
  Result.Add('picbpp', BitsPerPixelBitmapMetafileKeywordHandler);


  Result.Add('dximageuri', DxImageUriHandler);

  Result.Add('picprop', ShapePropertiesKeywordHandler);
end;

procedure TdxPictureDestination.BeforePopRtfState;
begin
  TdxRtfFormattingInfoAccess(Importer.Position.RtfFormattingInfo).SetDecoder(FOldDecoder);
  FOldDecoder := nil;
  inherited BeforePopRtfState;
end;

class procedure TdxPictureDestination.BitmapBitsPerPixelKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  AIsParameterValueCorrect: Boolean;
  ADestination: TdxPictureDestination;
begin
  if not AHasParameter then
    AParameterValue := 1;
  AIsParameterValueCorrect := (AParameterValue = 1) or (AParameterValue = 4) or
    (AParameterValue = 8) or (AParameterValue = 16) or (AParameterValue = 24) or (AParameterValue = 32);
  if not AIsParameterValueCorrect then
    TdxRtfImporter.ThrowInvalidRtfFile;
  ADestination := TdxPictureDestination(AImporter.Destination);
  ADestination.BmpBitsPerPixel := AParameterValue;
end;

class procedure TdxPictureDestination.BitmapBytesInLineKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
begin
  if not AHasParameter then
    TdxRtfImporter.ThrowInvalidRtfFile;
  ADestination := TdxPictureDestination(AImporter.Destination);
  ADestination.BmpBytesInLine := AParameterValue;
end;

class procedure TdxPictureDestination.BitmapMetafileKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
//do nothing
end;

class procedure TdxPictureDestination.BitmapPlanesKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
begin
  if not AHasParameter then
    TdxRtfImporter.ThrowInvalidRtfFile;
  if AParameterValue <> 1 then
    TdxRtfImporter.ThrowInvalidRtfFile;
  ADestination := TdxPictureDestination(AImporter.Destination);
  ADestination.BmpColorPlanes := AParameterValue;
end;

class procedure TdxPictureDestination.BitsPerPixelBitmapMetafileKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
//do nothing
end;

class procedure TdxPictureDestination.BottomCropKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
//do nothing
end;

class procedure TdxPictureDestination.DeviceDependentBitmapFileKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
begin
  if AHasParameter and (AParameterValue <> 0) then
    AImporter.ThrowInvalidRtfFile;
  ADestination := TdxPictureDestination(AImporter.Destination);
  ADestination.PictureSourceType := TdxPictureSourceType.WindowsBmp;
end;

class procedure TdxPictureDestination.DeviceIndependentBitmapFileKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
begin
  if AHasParameter and (AParameterValue <> 0) then
    AImporter.ThrowInvalidRtfFile;
  ADestination := TdxPictureDestination(AImporter.Destination);
  ADestination.PictureSourceType := TdxPictureSourceType.WindowsDib;
end;

class procedure TdxPictureDestination.DxImageUriHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Destination := TdxDxImageUriDestination.Create(AImporter, TdxPictureDestination(AImporter.Destination));
end;

class procedure TdxPictureDestination.EmfFileKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
begin
  ADestination := TdxPictureDestination(AImporter.Destination);
  ADestination.PictureSourceType := TdxPictureSourceType.Emf;
end;

class function TdxPictureDestination.FillBytesToConvertFromShortIntToLongInt(
  AParameterValue: Word): Integer;
begin
  Result := $FFFF and AParameterValue;
end;

function TdxPictureDestination.GetBmpBitsPerPixel: Integer;
begin
  Result := Info.BmpBitsPerPixel;
end;

function TdxPictureDestination.GetBmpBytesInLine: Integer;
begin
  Result := Info.BmpBytesInLine;
end;

function TdxPictureDestination.GetBmpColorPlanes: Integer;
begin
  Result := Info.BmpColorPlanes;
end;

function TdxPictureDestination.GetDataStream: TdxCrc32Stream;
begin
  Result := Info.DataStream;
end;

function TdxPictureDestination.GetImageInfo: TdxRtfImageInfo;
begin
  if Info.PictureStream.Size <= 0 then
    Result := nil
  else
  begin
    Info.PictureStream.Seek(0, soBeginning);
    Result := LoadPicture;
    if Result <> nil then
    begin
      if Info.Properties.HasBoolProperty('fPseudoInline') then
        Result.PseudoInline := Info.Properties.GetBoolPropertyValue('fPseudoInline');
    end;
    Info.ClosePictureStream;
  end;
end;

function TdxPictureDestination.GetPictureSourceType: TdxPictureSourceType;
begin
  Result := FInfo.PictureSourceType;
end;

function TdxPictureDestination.GetWmfMapMode: TdxMapMode;
begin
  Result := Info.WmfMapMode;
end;

procedure TdxPictureDestination.SetInfo(const Value: TdxPictureDestinationInfo);
begin
  if FIsInfoOwner then
    FInfo.Free;
  FInfo := Value;
  FIsInfoOwner := False;
end;

class procedure TdxPictureDestination.HorizontalScalingKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
begin
  if not AHasParameter then
    AImporter.ThrowInvalidRtfFile;
  ADestination := TdxPictureDestination(AImporter.Destination);
  ADestination.Info.ScaleX := AParameterValue;
end;

class constructor TdxPictureDestination.Initialize;
begin
  FRtfHundredthsOfMillimeterConverter := TdxRtfHundredthsOfMillimeterConverter.Create;
  FRtfPixelsConverter := TdxRtfPixelsToTwipsConverter.Create(96);
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxPictureDestination.Finalize;
begin
  FreeAndNil(FRtfHundredthsOfMillimeterConverter);
  FreeAndNil(FRtfPixelsConverter);
  FreeAndNil(FKeywordHT);
end;

class procedure TdxPictureDestination.JpegFileKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
begin
  ADestination := TdxPictureDestination(AImporter.Destination);
  ADestination.PictureSourceType := TdxPictureSourceType.Jpeg;
end;

class procedure TdxPictureDestination.LeftCropKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
//do nothing
end;

procedure TdxPictureDestination.LoadImage(AInfo: TdxRtfImageInfo);
begin
  if AInfo.RtfImage = nil then
  begin
    try
      AInfo.LoadImageFromStream(Info.PictureStream);
    except
      Exit;
    end;
  end;
  ValidateImageSize(AInfo.RtfImage);
  LoadImageInUnits(AInfo, FRtfHundredthsOfMillimeterConverter);
end;

procedure TdxPictureDestination.LoadImageInUnits(AImageInfo: TdxRtfImageInfo;
  AUnitsConverter: TdxRtfPictureUnitsConverter);
var
  AModelUnitConverter: TdxDocumentModelUnitConverter;
  AWidthInModelUnits, AHeightInModelUnits: Integer;
begin
  if AImageInfo.RtfImage.RawFormat in [TdxOfficeImageFormat.Emf, TdxOfficeImageFormat.Wmf] then
  begin
    LoadMetafileImageInUnits(AImageInfo, AUnitsConverter);
    Exit;
  end;
  Info.PictureWidth := AImageInfo.RtfImage.Image.SizeInTwips.cx;
  Info.PictureHeight := AImageInfo.RtfImage.Image.SizeInTwips.cy;
  if Info.DesiredPictureWidth <= 0 then
    Info.DesiredPictureWidth := Info.PictureWidth;
  if Info.DesiredPictureHeight <= 0 then
    Info.DesiredPictureHeight := Info.PictureHeight;
  if Info.ScaleX <= 0 then
    Info.ScaleX := 100;
  if Info.ScaleY <= 0 then
    Info.ScaleY := 100;
  if (Info.PictureWidth > 0) and (Info.DesiredPictureWidth > 0) then
    AImageInfo.ScaleX := Round(Info.ScaleX * Info.DesiredPictureWidth / Info.PictureWidth);
  if (Info.PictureHeight > 0) and (Info.DesiredPictureHeight > 0) then
    AImageInfo.ScaleY := Round(Info.ScaleY * Info.DesiredPictureHeight / Info.PictureHeight);

  AModelUnitConverter := Importer.UnitConverter;
  AWidthInModelUnits := Max(1, AModelUnitConverter.TwipsToModelUnits(Info.PictureWidth));
  AHeightInModelUnits := Max(1, AModelUnitConverter.TwipsToModelUnits(Info.PictureHeight));
  AImageInfo.SizeInModelUnits := cxSize(AWidthInModelUnits, AHeightInModelUnits);
end;

procedure TdxPictureDestination.LoadMetafile(AInfo: TdxRtfImageInfo);
var
  AActualFormat: TdxOfficeImageFormat;
begin
  if AInfo.RtfImage = nil then
  begin
    try
      AInfo.LoadMetafileFromStream(Info.PictureStream, WmfMapMode, Info.PictureWidth, Info.PictureHeight);
    except
      Exit;
    end;
  end;
  ValidateImageSize(AInfo.RtfImage);
  AActualFormat := AInfo.RtfImage.RawFormat;
  if AActualFormat in [TdxOfficeImageFormat.Wmf, TdxOfficeImageFormat.Emf] then
    LoadMetafileImageInUnits(AInfo, FRtfHundredthsOfMillimeterConverter)
  else
    LoadImageInUnits(AInfo, FRtfPixelsConverter);
end;

procedure TdxPictureDestination.LoadMetafileImageInUnits(
  AImageInfo: TdxRtfImageInfo; AUnitsConverter: TdxRtfPictureUnitsConverter);
var
  AModelUnitConverter: TdxDocumentModelUnitConverter;
  AWidthInModelUnits, AHeightInModelUnits: Integer;
begin
  if Info.DesiredPictureWidth <= 0 then
    Info.DesiredPictureWidth := AUnitsConverter.UnitsToTwips(Info.PictureWidth);
  if Info.DesiredPictureHeight <= 0 then
    Info.DesiredPictureHeight := AUnitsConverter.UnitsToTwips(Info.PictureHeight);
  if Info.ScaleX <= 0 then
    Info.ScaleX := 100;
  if Info.ScaleY <= 0 then
    Info.ScaleY := 100;
  if (Info.PictureWidth > 0) and (Info.DesiredPictureWidth > 0) then
    AImageInfo.ScaleX := Round(Info.ScaleX * Info.DesiredPictureWidth / AUnitsConverter.UnitsToTwips(Info.PictureWidth));
  if (Info.PictureHeight > 0) and (Info.DesiredPictureHeight > 0) then
    AImageInfo.ScaleY := Round(Info.ScaleY * Info.DesiredPictureHeight / AUnitsConverter.UnitsToTwips(Info.PictureHeight));

  AModelUnitConverter := Importer.UnitConverter;
  AWidthInModelUnits := Max(1, AUnitsConverter.UnitsToModelUnits(Info.PictureWidth, AModelUnitConverter));
  AHeightInModelUnits := Max(1, AUnitsConverter.UnitsToModelUnits(Info.PictureHeight, AModelUnitConverter));
  AImageInfo.SizeInModelUnits := cxSize(AWidthInModelUnits, AHeightInModelUnits);
end;

procedure TdxPictureDestination.LoadBitmap(AInfo: TdxRtfImageInfo);
begin
  if AInfo.RtfImage = nil then
  begin
    try
      AInfo.LoadBitmapFromStream(Info.PictureStream, Info.PictureWidth,
        Info.PictureHeight, Info.BmpColorPlanes, Info.BmpBitsPerPixel, Info.BmpBytesInLine);
    except
      Exit;
    end;
  end;
  LoadImageInUnits(AInfo, FRtfPixelsConverter);
end;

procedure TdxPictureDestination.LoadDib(AInfo: TdxRtfImageInfo);
begin
  if AInfo.RtfImage = nil then
  begin
    try
      AInfo.LoadDibFromStream(Info.PictureStream, Info.PictureWidth, Info.PictureHeight,
        Info.BmpBytesInLine);
    except
      Exit;
    end;
  end;
  LoadImageInUnits(AInfo, FRtfPixelsConverter);
end;

function TdxPictureDestination.LoadPicture: TdxRtfImageInfo;
begin
  Result := TdxRtfImageInfo.Create(Importer.DocumentModel);
  LoadPictureCore(Result);
  if Result.RtfImage = nil then
    FreeAndNil(Result)
  else
  begin
    if Result.RtfImage.Uri = '' then
      Result.RtfImage.Uri := Info.ImageUri;
  end;
end;

procedure TdxPictureDestination.LoadPictureCore(AInfo: TdxRtfImageInfo);
var
  AImageCache: TdxImageCache;
  ACrc32: Integer;
  ACachedImage: TdxOfficeImage;
begin
  ACrc32 := Info.DataCrc32;
  AImageCache := Importer.DocumentModel.ImageCache;
  ACachedImage := AImageCache.GetImage(ACrc32);
  if ACachedImage <> nil then
  begin
    AInfo.RtfImage := TdxOfficeImageReference.Create(Importer.DocumentModel.ImageCache, ACachedImage);
  end;

  case PictureSourceType of
    TdxPictureSourceType.Emf, TdxPictureSourceType.Wmf:
      LoadMetafile(AInfo);
    TdxPictureSourceType.WindowsBmp:
      LoadBitmap(AInfo);
    TdxPictureSourceType.WindowsDib:
      LoadDib(AInfo);
  else
    LoadImage(AInfo);
  end;
  if (ACachedImage = nil) and (AInfo.RtfImage <> nil) then
    AImageCache.AddImage(AInfo.RtfImage.Image, ACrc32);
end;

class procedure TdxPictureDestination.MacFileKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
begin
  ADestination := TdxPictureDestination(AImporter.Destination);
  ADestination.PictureSourceType := TdxPictureSourceType.Mac;
end;

class procedure TdxPictureDestination.PicScaledKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
end;

class procedure TdxPictureDestination.PictureGoalHeightKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
begin
  if not AHasParameter then
    TdxRtfImporter.ThrowInvalidRtfFile;
  ADestination := TdxPictureDestination(AImporter.Destination);
  ADestination.Info.DesiredPictureHeight := AParameterValue;
end;

class procedure TdxPictureDestination.PictureGoalWidthKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
begin
  if not AHasParameter then
    TdxRtfImporter.ThrowInvalidRtfFile;
  ADestination := TdxPictureDestination(AImporter.Destination);
  ADestination.Info.DesiredPictureWidth := AParameterValue;
end;

class procedure TdxPictureDestination.PictureHeightKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
  ACorrectedValue: Integer;
begin
  if not AHasParameter then
    TdxRtfImporter.ThrowInvalidRtfFile;
  ADestination := TdxPictureDestination(AImporter.Destination);
  ACorrectedValue := AParameterValue;
  if (AParameterValue < 0) and (AParameterValue = ShortInt(AParameterValue)) then
    ACorrectedValue := FillBytesToConvertFromShortIntToLongInt(Word(AParameterValue));
  ADestination.Info.PictureHeight := ACorrectedValue;
end;

class procedure TdxPictureDestination.PictureWidthKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
  ACorrectedValue: Integer;
begin
  if not AHasParameter then
    TdxRtfImporter.ThrowInvalidRtfFile;
  ADestination := TdxPictureDestination(AImporter.Destination);
  ACorrectedValue := AParameterValue;
  if (AParameterValue < 0) and (AParameterValue = ShortInt(AParameterValue)) then
    ACorrectedValue := FillBytesToConvertFromShortIntToLongInt(Word(AParameterValue));
  ADestination.Info.PictureWidth := ACorrectedValue;
end;

class procedure TdxPictureDestination.PngFileKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
begin
  ADestination := TdxPictureDestination(AImporter.Destination);
  ADestination.PictureSourceType := TdxPictureSourceType.Png;
end;

procedure TdxPictureDestination.ProcessBinCharCore(AChar: Char);
begin
  DataStream.WriteByte(Ord(AChar));
end;

class procedure TdxPictureDestination.RightCropKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
//do nothing
end;

procedure TdxPictureDestination.SetBmpBitsPerPixel(const Value: Integer);
begin
  Info.BmpBitsPerPixel := Value;
end;

procedure TdxPictureDestination.SetBmpBytesInLine(const Value: Integer);
begin
  Info.BmpBytesInLine := Value;
end;

procedure TdxPictureDestination.SetBmpColorPlanes(const Value: Integer);
begin
  Info.BmpColorPlanes := Value;
end;

procedure TdxPictureDestination.SetPictureSourceType(
  const Value: TdxPictureSourceType);
begin
  FInfo.PictureSourceType := Value;
end;

procedure TdxPictureDestination.SetWmfMapMode(const Value: TdxMapMode);
begin
  Info.WmfMapMode := Value;
end;

class procedure TdxPictureDestination.ShapePropertiesKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Destination := TdxShapePropertyDestination.Create(AImporter, TdxPictureDestination(AImporter.Destination).Info.Properties);
end;

class procedure TdxPictureDestination.TopCropKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
//do nothing
end;

procedure TdxPictureDestination.ValidateImageSize(AImage: TdxOfficeImageReference);
var
  AImageSize: TSize;
begin
  if (Info.PictureWidth <= 0) or (Info.PictureHeight <= 0) then
  begin
    AImageSize := AImage.Image.SizeInHundredthsOfMillimeter;
    if Info.PictureWidth <= 0 then
      Info.PictureWidth := AImageSize.cx;
    if Info.PictureHeight <= 0 then
      Info.PictureHeight := AImageSize.cy;
  end;
end;

class procedure TdxPictureDestination.VerticalScalingKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
begin
  if not AHasParameter then
    AImporter.ThrowInvalidRtfFile;
  ADestination := TdxPictureDestination(AImporter.Destination);
  ADestination.Info.ScaleY := AParameterValue;
end;

class procedure TdxPictureDestination.WindowsMetafileKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxPictureDestination;
begin
  ADestination := TdxPictureDestination(AImporter.Destination);
  ADestination.PictureSourceType := TdxPictureSourceType.Wmf;
  if AHasParameter then
    ADestination.WmfMapMode := TdxMapMode(AParameterValue);
end;

{ TdxPictureDestination.TdxRtfPixelsToTwipsConverter }

constructor TdxPictureDestination.TdxRtfPixelsToTwipsConverter.Create(ADpi: Integer);
begin
  inherited Create;
  FDpi := ADpi;
end;

function TdxPictureDestination.TdxRtfPixelsToTwipsConverter.UnitsToModelUnits(Value: Integer;
  AUnitConverter: TdxDocumentModelUnitConverter): Integer;
begin
  Result := AUnitConverter.PixelsToModelUnits(Value, FDpi);
end;

function TdxPictureDestination.TdxRtfPixelsToTwipsConverter.UnitsToTwips(Value: Integer): Integer;
begin
  Result := Round(1440 * Value / FDpi);
end;

{ TdxPictureDestination.TdxRtfHundredthsOfMillimeterConverter }

function TdxPictureDestination.TdxRtfHundredthsOfMillimeterConverter.UnitsToModelUnits(Value: Integer;
  AUnitConverter: TdxDocumentModelUnitConverter): Integer;
begin
  Result := AUnitConverter.HundredthsOfMillimeterToModelUnitsRound(Value);
end;

function TdxPictureDestination.TdxRtfHundredthsOfMillimeterConverter.UnitsToTwips(
  Value: Integer): Integer;
begin
  Result := Round(1440 * Value / 2540.0);
end;

{ TdxDxImageUriDestination }

procedure TdxDxImageUriDestination.AfterPopRtfState;
begin
  FPictureDestination.Info.ImageUri := Value;
end;

constructor TdxDxImageUriDestination.Create(AImporter: TdxRtfImporter;
  APictureDestination: TdxPictureDestination);
begin
  inherited Create(AImporter);
  FPictureDestination := APictureDestination;
  FStringBuilder := TStringBuilder.Create;
end;

function TdxDxImageUriDestination.CreateClone: TdxRichEditRtfDestinationBase;
var
  AClone: TdxDxImageUriDestination;
begin
  AClone := TdxDxImageUriDestination(CreateEmptyClone);
  AClone.FStringBuilder.Append(FStringBuilder.ToString);
  Result := AClone;
end;

function TdxDxImageUriDestination.CreateEmptyClone: TdxStringValueDestination;
begin
  Result := TdxDxImageUriDestination.Create(Importer, FPictureDestination);
end;

destructor TdxDxImageUriDestination.Destroy;
begin
  FreeAndNil(FStringBuilder);
  inherited Destroy;
end;

function TdxDxImageUriDestination.GetValue: string;
begin
  Result := FStringBuilder.ToString;
end;

procedure TdxDxImageUriDestination.ProcessCharCore(AChar: Char);
begin
  FStringBuilder.Append(AChar);
end;

function TdxDxImageUriDestination.ProcessKeywordCore(const AKeyword: string;
  AParameterValue: Integer; AHasParameter: Boolean): Boolean;
begin
  Result := False;
end;

end.
