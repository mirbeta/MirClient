{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           GDI+ Library                                             }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
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

unit dxSmartImage;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, Variants, ExtCtrls, dxCore, dxCoreGraphics, cxGeometry;

type
  TdxCustomSmartImage = class;

  TdxSmartImageCodecClass = class of TdxSmartImageCodec;
  TdxSmartImageCodec = class;

  TdxImageDataFormat = (dxImageUnknown, dxImageBitmap, dxImageJpeg, dxImagePng, dxImageTiff,
    dxImageGif, dxImageEmf, dxImageExif, dxImageIcon, dxImageMemoryBmp, dxImageWmf);

  { IdxAnimatedImage }

  IdxAnimatedImage = interface
  ['{511C08FA-ED2A-4998-95BB-CBAED1D39BA9}']
    function GetActiveFrame: Cardinal;
    function GetAnimationFrameCount: Cardinal;
    function GetAnimationFrameDelay: Integer;
    function GetAnimationLoopCount: Integer;
    procedure SetActiveFrame(AValue: Cardinal);
  end;

  { IdxImageDataFormat }

  IdxImageDataFormat = interface
  ['{19993C97-DF15-4E67-A250-77BACBF5DB64}']
    function GetImageFormat: TdxImageDataFormat;
  end;

  { IdxImageDataFormatEx }

  IdxImageDataFormatEx = interface
  ['{F54CB0CE-F15C-4EE2-9323-F95B65BB9769}']
    function GetImageFormat: TdxSmartImageCodecClass;
  end;

  { IdxVectorImage }

  IdxVectorImage = interface
  ['{F7B61E70-B4E6-4A23-862B-117530E00EB2}']
  end;

  { TdxSmartImageCustomHandle }

  TdxSmartImageCustomHandleClass = class of TdxSmartImageCustomHandle;
  TdxSmartImageCustomHandle = class abstract(TInterfacedPersistent)
  strict private
    function GetClientRect: TRect;
    function GetHeight: Integer;
    function GetWidth: Integer;
  protected
    function GetSize: TSize; virtual; abstract;
    procedure SetSize(const AValue: TSize); virtual;
  public
    function Empty: Boolean; virtual;
    function Equals(Obj: TObject): Boolean; override;
    function GetAlphaState: TdxAlphaState; virtual;
    function GetAsBitmap: TBitmap; virtual;
    function GetAsBitmapBits: TRGBColors; virtual;
    function GetHashCode: Integer; override;

    procedure Draw(DC: HDC; const ADest, ASource: TdxRectF;
      AAlpha: Byte = 255; APalette: IdxColorPalette = nil); overload; virtual;
    procedure Draw(DC: HDC; const ADest, ASource: TRect;
      AAlpha: Byte = 255; APalette: IdxColorPalette = nil); overload; virtual; abstract;

    property ClientRect: TRect read GetClientRect;
    property Height: Integer read GetHeight;
    property Size: TSize read GetSize write SetSize;
    property Width: Integer read GetWidth;
  end;

  { TdxSmartImageEmptyHandle }

  TdxSmartImageEmptyHandle = class(TdxSmartImageCustomHandle)
  protected
    function GetSize: TSize; override;
  public
    procedure Draw(DC: HDC; const ADest, ASource: TRect; AAlpha: Byte = 255; APalette: IdxColorPalette = nil); override;
  end;

  { TdxSmartImageCodec }

  TdxSmartImageCodec = class abstract
  protected
    class function ID: TdxImageDataFormat; virtual;
  public
    class function Description: string; virtual;
    class function Ext: string; virtual;
    class function GetSize(AStream: TStream; out ASize: TSize): Boolean; virtual;

    class function CanLoadFromBits: Boolean; virtual;
    class function CanLoadStream(AStream: TStream): Boolean; virtual;
    class function CanSaveImage(AHandle: TdxSmartImageCustomHandle): Boolean; virtual;
    class function Load(const ABits: TRGBColors; AAlphaFormat: TAlphaFormat;
      AWidth, AHeight: Integer; out AHandle: TdxSmartImageCustomHandle): Boolean; overload; virtual;
    class function Load(AStream: TStream; out AHandle: TdxSmartImageCustomHandle): Boolean; overload; virtual;
    class function Save(AStream: TStream; AHandle: TdxSmartImageCustomHandle): Boolean; virtual;
  end;

  { TdxSmartImageAnimationController }

  TdxSmartImageAnimationControllerClass = class of TdxSmartImageAnimationController;
  TdxSmartImageAnimationController = class
  strict private
    FImage: TdxCustomSmartImage;

    function GetActive: Boolean;
  protected
    procedure ActivateTimer; virtual;
    procedure DeactivateTimer; virtual;
    function IsTimerActive: Boolean; virtual;
    procedure TimerHandler(Sender: TObject); virtual;
  public
    constructor Create(AImage: TdxCustomSmartImage); virtual;
    destructor Destroy; override;
    procedure StartAnimation;
    procedure StopAnimation;

    property Active: Boolean read GetActive;
    property Image: TdxCustomSmartImage read FImage;
  end;

  { TdxSmartImageDataStream }

  TdxSmartImageDataStreamClass = class of TdxSmartImageDataStream;
  TdxSmartImageDataStream = class(TMemoryStream)
  public
    function Clone: TdxSmartImageDataStream;
  end;

  { TdxSmartImageData }

  TdxSmartImageData = class
  strict private
    FTransparent: Boolean;
    FTransparentColor: TColor;
    FTransparentMode: TTransparentMode;

    function GetIsEmpty: Boolean;
  protected
    FBits: TRGBColors;
    FBitsFormat: TAlphaFormat;
    FData: TdxSmartImageDataStream;
    FNewSize: TSize;
    FSize: TSize;

    procedure ApplyMask;
    procedure Assign(AData: TdxSmartImageData);
    procedure FreeImageData;
    procedure LoadFromBitmap(const ABitmap: TBitmap);
    procedure LoadFromBits(const ABits: TRGBColors; const ASize: TSize; AAlphaFormat: TAlphaFormat);
    procedure LoadFromStream(AStream: TStream);
  public
    destructor Destroy; override;
    //
    property Transparent: Boolean read FTransparent write FTransparent;
    property TransparentColor: TColor read FTransparentColor write FTransparentColor;
    property TransparentMode: TTransparentMode read FTransparentMode write FTransparentMode;
    property Empty: Boolean read GetIsEmpty;
  end;

  { TdxCustomSmartImage }

  TdxCustomSmartImageClass = class of TdxCustomSmartImage;
  TdxCustomSmartImage = class(TGraphic)
  strict private
    FAlphaState: TdxAlphaState;
    FAlphaStateAssigned: Boolean;
    FAnimationController: TdxSmartImageAnimationController;
    FAnimationFrameDelay: Integer;
    FAnimationLoop: TdxDefaultBoolean;
    FAnimationLoopIndex: Integer;
    FCache: TdxSmartImageCustomHandle;
    FCachePaletteID: TGUID;
    FHandle: TdxSmartImageCustomHandle;
    FImageData: TdxSmartImageData;
    FUseCache: Boolean;

    procedure Check(AResult: Boolean; const AMessage: string);
    function GetActiveFrame: LongWord;
    function GetAlphaState: TdxAlphaState;
    function GetAnimation: Boolean;
    function GetAnimationFrameCount: Cardinal;
    function GetAnimationFrameDelay: Integer;
    function GetAnimationLoopCount: Integer;
    function GetClientRect: TRect;
    function GetHandle: TdxSmartImageCustomHandle;
    function GetHandleAllocated: Boolean;
    function GetImageCodec: TdxSmartImageCodecClass;
    function GetImageDataFormat: TdxImageDataFormat;
    function GetIsAlphaUsed: Boolean;
    function GetSize: TSize;
    procedure SetActiveFrame(AValue: LongWord);
    procedure SetAnimation(AValue: Boolean);
    procedure SetAnimationFrameDelay(AValue: Integer);
    procedure SetHandle(const Value: TdxSmartImageCustomHandle);
    procedure SetImageCodec(const AValue: TdxSmartImageCodecClass);
    procedure SetImageDataFormat(AValue: TdxImageDataFormat);
    procedure SetUseCache(const Value: Boolean);
  protected
    function CanStartAnimation: Boolean;
    function CanCreateCache(const ASourceSize, ATargetSize: TSize): Boolean; virtual;
    function CheckCache(const ASize: TSize; const APalette: IdxColorPalette): Boolean;
    function CreateCache(const ASize: TSize): TdxSmartImageCustomHandle; virtual;
    function IsBitmapStream(AStream: TStream): Boolean; virtual;

    function AssignFromClipboard: Boolean; virtual;
    procedure AssignFromGraphic(AGraphic: TGraphic); virtual;
    procedure AssignFromSmartImage(AImage: TdxCustomSmartImage); virtual;
    procedure AssignTo(ADest: TPersistent); override;
    procedure AssignToMetaFile(ADest: TMetafile); virtual;

    procedure CreateHandleFromBitmap(ABitmap: TBitmap); virtual;
    procedure CreateHandleFromBits(AWidth, AHeight: Integer; const ABits: TRGBColors; AAlphaFormat: TAlphaFormat);
    procedure CreateHandleFromHBITMAP(ABitmap: HBITMAP; APalette: HPALETTE);
    procedure CreateHandleFromImageData;
    procedure CreateHandleFromStream(AStream: TStream);
    function ExportToStream(ACodec: TdxSmartImageCodecClass): TdxSmartImageDataStream;
    procedure FreeHandle; virtual;
    procedure PopulateImageDataFromHandle;

    procedure Changed; reintroduce; overload;
    procedure Changed(Sender: TObject); overload; override;
    procedure CheckIsImageDataValid;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    function Equals(Graphic: TGraphic): Boolean; override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetTransparent: Boolean; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;

    property AlphaState: TdxAlphaState read GetAlphaState;
    property AnimationController: TdxSmartImageAnimationController read FAnimationController;
    property Handle: TdxSmartImageCustomHandle read GetHandle write SetHandle;
    property ImageData: TdxSmartImageData read FImageData;
  protected
    property UseCache: Boolean read FUseCache write SetUseCache;
  public
    constructor Create; override;
    constructor CreateSize(const ARect: TRect; AColor: TdxAlphaColor = 0); overload;
    constructor CreateSize(const ASize: TSize; AColor: TdxAlphaColor = 0); overload;
    constructor CreateSize(AWidth, AHeight: Integer; AColor: TdxAlphaColor = 0); overload; virtual;
    constructor CreateFromBitmap(ABitmap: TBitmap); virtual;
    constructor CreateFromHBitmap(ABitmap: HBitmap); virtual;
    constructor CreateFromBits(AWidth, AHeight: Integer; const ABits: TRGBColors; AHasAlphaChannel: Boolean); overload; virtual;
    constructor CreateFromBits(AWidth, AHeight: Integer; const ABits: TRGBColors; AAlphaFormat: TAlphaFormat); overload; virtual;
    constructor CreateFromStream(AStream: TStream); virtual;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
    procedure LoadFromBits(AWidth, AHeight: Integer; const ABits: TRGBColors; AHasAlphaChannel: Boolean); overload;
    procedure LoadFromBits(AWidth, AHeight: Integer; const ABits: TRGBColors; AAlphaFormat: TAlphaFormat); overload;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); override;
    procedure LoadFromFieldValue(const AValue: Variant);
    procedure LoadFromResource(AInstance: THandle; const AResName: string; AResType: PChar);
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE); override;
    procedure SaveToFile(const Filename: string); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure SaveToStreamByCodec(AStream: TStream; ACodec: TdxSmartImageCodecClass); overload;
    procedure SaveToStreamByCodec(AStream: TStream; AFormat: TdxImageDataFormat); overload;

    class function IsSupportClipboardFormat(AFormat: Word): Boolean; virtual;
    class function HasClipboardFormat: Boolean; virtual;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;

    procedure Clear; virtual;
    function Clone: TdxCustomSmartImage;
    procedure ChangeColor(AColor: TColor); virtual;
    function Compare(AImage: TdxCustomSmartImage): Boolean; virtual;
    procedure ConvertToBitmap;
    procedure Dormant;
    function GetAsBitmap: TBitmap; virtual;
    function GetBitmapBits: TRGBColors;
    function GetHashCode: Integer; override;
    procedure HandleNeeded;
    procedure Resize(const AWidth, AHeight: Integer); overload; virtual;
    procedure Resize(const ASize: TSize); overload;
    procedure SetBitmap(ABitmap: TBitmap); virtual;
    procedure Scale(const M, D: Integer);
    procedure StartAnimation;
    procedure StopAnimation;

    procedure StretchDraw(DC: HDC; const ADest, ASource: TdxRectF;
      AAlpha: Byte = 255; APalette: IdxColorPalette = nil); overload;
    procedure StretchDraw(DC: HDC; const ADest, ASource: TRect;
      AAlpha: Byte = 255; APalette: IdxColorPalette = nil); overload;
    procedure StretchDraw(DC: HDC; const ADest: TRect;
      AAlpha: Byte = 255; APalette: IdxColorPalette = nil); overload;

    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; override;

    property ActiveFrame: LongWord read GetActiveFrame write SetActiveFrame;
    property Animation: Boolean read GetAnimation write SetAnimation;
    property AnimationFrameCount: Cardinal read GetAnimationFrameCount;
    property AnimationFrameDelay: Integer read GetAnimationFrameDelay write SetAnimationFrameDelay;
    property AnimationLoop: TdxDefaultBoolean read FAnimationLoop write FAnimationLoop default bTrue;
    property AnimationLoopIndex: Integer read FAnimationLoopIndex write FAnimationLoopIndex;
    property AnimationLoopCount: Integer read GetAnimationLoopCount;

    property ClientRect: TRect read GetClientRect;
    property HandleAllocated: Boolean read GetHandleAllocated;
    property ImageCodec: TdxSmartImageCodecClass read GetImageCodec write SetImageCodec;
    property ImageDataFormat: TdxImageDataFormat read GetImageDataFormat write SetImageDataFormat; // deprecated, use ImageCodec instead
    property IsAlphaUsed: Boolean read GetIsAlphaUsed;
    property Size: TSize read GetSize;
  end;

  { TdxSmartImageCodecsRepository }

  TdxSmartImageCodecsRepository = class
  strict private
    class var FList: TList;

    class function GetCount: Integer; static;
    class function GetItem(Index: Integer): TdxSmartImageCodecClass; static;
  public
    class function Contains(ACodec: TdxSmartImageCodecClass): Boolean;
    class function GetFormatByExt(const Ext: string): TdxSmartImageCodecClass;
    class function GetFormatByID(ID: TdxImageDataFormat): TdxSmartImageCodecClass;
    class function GetFormatFromStream(AStream: TStream): TdxSmartImageCodecClass;
    class function GetImageInfo(const AFileName: string; out ASize: TSize; out ACodec: TdxSmartImageCodecClass): Boolean; overload;
    class function GetImageInfo(const AImageData: TdxSmartImageData; out ASize: TSize; out ACodec: TdxSmartImageCodecClass): Boolean; overload;
    class function GetImageInfo(const AStream: TStream; out ASize: TSize; out ACodec: TdxSmartImageCodecClass): Boolean; overload;
    class function Load(const ABits: TRGBColors; AAlphaFormat: TAlphaFormat;
      AWidth, AHeight: Integer; out AHandle: TdxSmartImageCustomHandle): Boolean; overload;
    class function Load(const AImageData: TdxSmartImageData; out AHandle: TdxSmartImageCustomHandle): Boolean; overload;
    class function Load(const AStream: TStream; out AHandle: TdxSmartImageCustomHandle): Boolean; overload;

    class procedure Register(ACodec: TdxSmartImageCodecClass);
    class procedure Unregister(ACodec: TdxSmartImageCodecClass);

    class property Count: Integer read GetCount;
    class property Items[Index: Integer]: TdxSmartImageCodecClass read GetItem;
  end;

  { TdxSmartImageHelper }

  TdxSmartImageHelper = class
  public
    class function AllocColors(AWidth, AHeight: Integer; AColor: TdxAlphaColor = 0): TRGBColors;
    class function AreColorsPremultiplied(const ABits: TRGBColors): Boolean;
    class function HasAlpha(const ABits: TRGBColors): Boolean;
  end;

var
  ImageAnimationControllerClass: TdxSmartImageAnimationControllerClass = TdxSmartImageAnimationController;
  ImageClipboardFormats: array[TdxImageDataFormat] of Word;
  dxSmartImageCodecDescriptionSuffix: string = ' graphics from DevExpress';

implementation

uses
  SysUtils, Math, Clipbrd, dxHash, dxThreading;

const
  sdxErrorCannotExportImage = 'The %s codec failed to export the image (%d)';
  sdxErrorUnsupportedImageFormat = 'Unsupported image format.';
  sdxErrorUnsupportedOperation = 'Operation was not supported.';

  SupportedFormats: array[0..4] of TdxImageDataFormat = (
    dxImagePng, dxImageGif, dxImageTiff, dxImageJpeg, dxImageBitmap
  );

  HasAlphaChannelFormatMap: array[Boolean] of TAlphaFormat = (afIgnored, afPremultiplied);

type
  TMemoryStreamAccess = class(TMemoryStream);

{ TdxSmartImageCustomHandle }

function TdxSmartImageCustomHandle.Empty: Boolean;
begin
  Result := not cxSizeIsValid(Size);
end;

function TdxSmartImageCustomHandle.Equals(Obj: TObject): Boolean;
var
  AColors1: TRGBColors;
  AColors2: TRGBColors;
begin
  Result := False;
  if Obj is TdxSmartImageCustomHandle then
  begin
    AColors1 := nil;
    AColors2 := nil;
    Result := cxSizeIsEqual(TdxSmartImageCustomHandle(Obj).Size, Size);
    if Result and not (TdxSmartImageCustomHandle(Obj).Empty or Empty) then
    begin
      AColors1 := GetAsBitmapBits;
      AColors2 := TdxSmartImageCustomHandle(Obj).GetAsBitmapBits;
      Result := CompareMem(@AColors1[0], @AColors2[0], SizeOf(AColors1[0]) * Length(AColors1));
    end;
  end;
end;

function TdxSmartImageCustomHandle.GetAsBitmap: TBitmap;
begin
  Result := TBitmap.Create;
  try
    Result.SetSize(Width, Height);
    Draw(Result.Canvas.Handle, Rect(0, 0, Result.Width, Result.Height), ClientRect);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TdxSmartImageCustomHandle.GetAsBitmapBits: TRGBColors;
var
  ABitmap: TBitmap;
begin
  ABitmap := GetAsBitmap;
  try
    GetBitmapBits(ABitmap, Result, True);
  finally
    ABitmap.Free;
  end;
end;

function TdxSmartImageCustomHandle.GetHashCode: Integer;
var
  AColors: TRGBColors;
begin
  Result := 0;
  if not Empty then
  begin
    AColors := GetAsBitmapBits;
    Result := dxCRC32(@AColors[0], Length(AColors) * SizeOf(AColors[0]));
  end;
end;

function TdxSmartImageCustomHandle.GetAlphaState: TdxAlphaState;
begin
  if Empty then
    Result := asNoAlpha
  else
    Result := dxGetAlphaState(GetAsBitmapBits);
end;

procedure TdxSmartImageCustomHandle.Draw(DC: HDC;
  const ADest, ASource: TdxRectF; AAlpha: Byte; APalette: IdxColorPalette);
begin
  Draw(DC, cxRect(ADest), cxRect(ASource), AAlpha, APalette);
end;

procedure TdxSmartImageCustomHandle.SetSize(const AValue: TSize);
begin
  // do nothing
end;

function TdxSmartImageCustomHandle.GetClientRect: TRect;
begin
  Result := cxRect(Size);
end;

function TdxSmartImageCustomHandle.GetHeight: Integer;
begin
  Result := Size.cy;
end;

function TdxSmartImageCustomHandle.GetWidth: Integer;
begin
  Result := Size.cx;
end;

{ TdxSmartImageEmptyHandle }

procedure TdxSmartImageEmptyHandle.Draw(DC: HDC; const ADest, ASource: TRect; AAlpha: Byte; APalette: IdxColorPalette);
begin
  // do nothing
end;

function TdxSmartImageEmptyHandle.GetSize: TSize;
begin
  Result := cxNullSize;
end;

{ TdxSmartImageCodec }

class function TdxSmartImageCodec.Description: string;
begin
  Result := UpperCase(Copy(Ext, 2, MaxInt)) + dxSmartImageCodecDescriptionSuffix;
end;

class function TdxSmartImageCodec.Ext: string;
begin
  Result := '';
end;

class function TdxSmartImageCodec.ID: TdxImageDataFormat;
begin
  Result := dxImageUnknown;
end;

class function TdxSmartImageCodec.GetSize(AStream: TStream; out ASize: TSize): Boolean;
begin
  Result := False;
end;

class function TdxSmartImageCodec.CanLoadFromBits: Boolean;
begin
  Result := False;
end;

class function TdxSmartImageCodec.CanLoadStream(AStream: TStream): Boolean;
begin
  Result := False;
end;

class function TdxSmartImageCodec.CanSaveImage(AHandle: TdxSmartImageCustomHandle): Boolean;
begin
  Result := False;
end;

class function TdxSmartImageCodec.Load(const ABits: TRGBColors; AAlphaFormat: TAlphaFormat;
  AWidth, AHeight: Integer; out AHandle: TdxSmartImageCustomHandle): Boolean;
begin
  Result := False;
end;

class function TdxSmartImageCodec.Load(AStream: TStream; out AHandle: TdxSmartImageCustomHandle): Boolean;
begin
  Result := False;
end;

class function TdxSmartImageCodec.Save(AStream: TStream; AHandle: TdxSmartImageCustomHandle): Boolean;
begin
  Result := False;
end;

{ TdxSmartImageAnimationController }

constructor TdxSmartImageAnimationController.Create(AImage: TdxCustomSmartImage);
begin
  FImage := AImage;
end;

destructor TdxSmartImageAnimationController.Destroy;
begin
  StopAnimation;
  DeactivateTimer;
  inherited Destroy;
end;

procedure TdxSmartImageAnimationController.StartAnimation;
begin
  ActivateTimer;
end;

procedure TdxSmartImageAnimationController.StopAnimation;
begin
  DeactivateTimer;
end;

procedure TdxSmartImageAnimationController.ActivateTimer;
begin
  // do nothing
end;

procedure TdxSmartImageAnimationController.DeactivateTimer;
begin
  // do nothing
end;

function TdxSmartImageAnimationController.GetActive: Boolean;
begin
  Result := (Image.AnimationFrameCount > 1) and IsTimerActive;
end;

function TdxSmartImageAnimationController.IsTimerActive: Boolean;
begin
  Result := False;
end;

procedure TdxSmartImageAnimationController.TimerHandler(Sender: TObject);
var
  AIndex: LongWord;
begin
  DeactivateTimer;
  AIndex := Image.ActiveFrame;
  try
    Image.ActiveFrame := Image.ActiveFrame + 1;
  finally
    if Image.ActiveFrame = AIndex then
      Image.StopAnimation
    else
      ActivateTimer;
  end;
end;

{ TdxSmartImageDataStream }

function TdxSmartImageDataStream.Clone: TdxSmartImageDataStream;
var
  APrevStreamPosition: Int64;
begin
  if Self <> nil then
  begin
    APrevStreamPosition := Position;
    try
      Position := 0;
      Result := TdxSmartImageDataStreamClass(ClassType).Create;
      Result.Size := Size;
      Result.CopyFrom(Self, Size);
      Result.Position := 0;
    finally
      Position := APrevStreamPosition;
    end;
  end
  else
    Result := nil;
end;

{ TdxSmartImageData}

destructor TdxSmartImageData.Destroy;
begin
  FreeImageData;
  inherited Destroy;
end;

procedure TdxSmartImageData.ApplyMask;
var
  I: Integer;
  ATransparentColor: DWORD;
  APixelCount: Integer;
begin
  if FBitsFormat = afIgnored then
  begin
    APixelCount := Length(FBits);
    if FTransparentMode = tmFixed then
      ATransparentColor := dxAlphaColorToColor(FTransparentColor)
    else
      ATransparentColor := DWORD(FBits[APixelCount - FSize.cx]);

    for I := 0 to APixelCount - 1 do
      if FTransparent and (DWORD(FBits[I]) = ATransparentColor) then
        FBits[I].rgbReserved := 0
      else
        FBits[I].rgbReserved := 255;

    FBitsFormat := afPremultiplied
  end;
end;

procedure TdxSmartImageData.Assign(AData: TdxSmartImageData);
begin
  FreeImageData;
  if AData <> nil then
  begin
    FBits := AData.FBits;
    FBitsFormat := AData.FBitsFormat;
    FSize := AData.FSize;
    FNewSize := AData.FNewSize;
    FData := AData.FData.Clone;
  end;
end;

procedure TdxSmartImageData.FreeImageData;
begin
  FBits := nil;
  FSize := cxNullSize;
  FNewSize := cxNullSize;
  FreeAndNil(FData);
end;

procedure TdxSmartImageData.LoadFromBitmap(const ABitmap: TBitmap);
begin
  FreeImageData;

  Transparent := Transparent or ABitmap.Transparent;
  TransparentColor := ABitmap.TransparentColor;
  TransparentMode := ABitmap.TransparentMode;

  if dxCoreGraphics.GetBitmapBits(ABitmap, FBits, True) then
  begin
    FBitsFormat := ABitmap.AlphaFormat;
    if FBitsFormat = afIgnored then
    begin
      if (cxGetBitmapPixelFormat(ABitmap) = 32) and TdxSmartImageHelper.HasAlpha(FBits) then
      begin
        if TdxSmartImageHelper.AreColorsPremultiplied(FBits) then
          FBitsFormat := afPremultiplied
        else
          FBitsFormat := afDefined;
      end;
    end;
    FSize := cxSize(ABitmap.Width, ABitmap.Height);
    ApplyMask;
  end;
end;

procedure TdxSmartImageData.LoadFromBits(const ABits: TRGBColors; const ASize: TSize; AAlphaFormat: TAlphaFormat);
begin
  FreeImageData;
  FBits := ABits;
  FBitsFormat := AAlphaFormat;
  FSize := ASize;
  ApplyMask;
end;

procedure TdxSmartImageData.LoadFromStream(AStream: TStream);
var
  ASize: Integer;
begin
  FreeImageData;
  ASize := AStream.Size - AStream.Position;
  if ASize > 0 then
  begin
    FData := TdxSmartImageDataStream.Create;
    FData.Size := ASize;
    FData.CopyFrom(AStream, ASize);
    FData.Position := 0;
  end;
end;

function TdxSmartImageData.GetIsEmpty: Boolean;
begin
  Result := (FData = nil) and (Length(FBits) = 0);
end;

{ TdxSmartImage }

constructor TdxCustomSmartImage.Create;
begin
  inherited;
  FImageData := TdxSmartImageData.Create;
  FImageData.TransparentColor := clDefault;
  FAnimationLoop := bTrue;
  FAnimationFrameDelay := 0;
  FUseCache := False;
end;

destructor TdxCustomSmartImage.Destroy;
begin
  FreeHandle;
  FreeAndNil(FAnimationController);
  FreeAndNil(FImageData);
  FreeAndNil(FCache);
  inherited Destroy;
end;

constructor TdxCustomSmartImage.CreateFromBitmap(ABitmap: TBitmap);
begin
  Create;
  CreateHandleFromBitmap(ABitmap);
end;

constructor TdxCustomSmartImage.CreateFromBits(AWidth, AHeight: Integer; const ABits: TRGBColors; AHasAlphaChannel: Boolean);
begin
  CreateFromBits(AWidth, AHeight, ABits, HasAlphaChannelFormatMap[AHasAlphaChannel]);
end;

constructor TdxCustomSmartImage.CreateFromBits(AWidth, AHeight: Integer; const ABits: TRGBColors; AAlphaFormat: TAlphaFormat);
begin
  Create;
  CreateHandleFromBits(AWidth, AHeight, ABits, AAlphaFormat);
end;

constructor TdxCustomSmartImage.CreateFromHBitmap(ABitmap: HBitmap);
begin
  Create;
  CreateHandleFromHBITMAP(ABitmap, 0);
end;

constructor TdxCustomSmartImage.CreateFromStream(AStream: TStream);
begin
  Create;
  CreateHandleFromStream(AStream);
end;

constructor TdxCustomSmartImage.CreateSize(const ARect: TRect; AColor: TdxAlphaColor);
begin
  CreateSize(cxRectWidth(ARect), cxRectHeight(ARect), AColor);
end;

constructor TdxCustomSmartImage.CreateSize(const ASize: TSize; AColor: TdxAlphaColor);
begin
  CreateSize(ASize.cx, ASize.cy, AColor);
end;

constructor TdxCustomSmartImage.CreateSize(AWidth, AHeight: Integer; AColor: TdxAlphaColor);
begin
  CreateFromBits(AWidth, AHeight, TdxSmartImageHelper.AllocColors(AWidth, AHeight, AColor), True);
end;

procedure TdxCustomSmartImage.Assign(ASource: TPersistent);
begin
  if ASource = nil then
    Clear
  else if ASource is TdxCustomSmartImage then
    AssignFromSmartImage(TdxCustomSmartImage(ASource))
  else if ASource is TGraphic then
    AssignFromGraphic(TGraphic(ASource))
  else if ASource is TClipboard then
    PasteFromClipboard
  else
    inherited Assign(ASource);
end;

procedure TdxCustomSmartImage.ChangeColor(AColor: TColor);
var
  AColors: TRGBColors;
begin
  if (AColor <> clDefault) and (AColor <> clNone) and not Empty then
  begin
    AColors := GetBitmapBits;
    dxChangeColor(@AColors[0], Length(AColors), AColor);
    LoadFromBits(Width, Height, AColors, True);
  end;
end;

procedure TdxCustomSmartImage.Clear;
begin
  if HandleAllocated or not ImageData.Empty then
  begin
    FreeHandle;
    Changed;
  end;
end;

function TdxCustomSmartImage.Clone: TdxCustomSmartImage;
begin
  Result := TdxCustomSmartImageClass(ClassType).Create;
  Result.AssignFromSmartImage(Self);
end;

function TdxCustomSmartImage.Compare(AImage: TdxCustomSmartImage): Boolean;
begin
  Result := Handle.Equals(AImage.Handle);
end;

procedure TdxCustomSmartImage.ConvertToBitmap;
var
  ABitmap: TBitmap;
begin
  if not Empty and not (ImageDataFormat in [dxImageBitmap, dxImageMemoryBmp]) then
  begin
    ABitmap := GetAsBitmap;
    try
      SetBitmap(ABitmap);
    finally
      ABitmap.Free;
    end;
  end;
end;

procedure TdxCustomSmartImage.Dormant;
begin
  if HandleAllocated then
  begin
    if not Empty then
      PopulateImageDataFromHandle;
    FreeAndNil(FHandle);
  end;
end;

procedure TdxCustomSmartImage.CopyToClipboard;
begin
  if not Empty then
    Clipboard.Assign(Self);
end;

procedure TdxCustomSmartImage.CutToClipboard;
begin
  if not Empty then
  begin
    CopyToClipboard;
    Clear;
  end;
end;

procedure TdxCustomSmartImage.PasteFromClipboard;
begin
  Clipboard.Open;
  try
    AssignFromClipboard;
  finally
    Clipboard.Close;
  end;
end;

procedure TdxCustomSmartImage.HandleNeeded;
begin
  if not HandleAllocated then
    CreateHandleFromImageData;
end;

class function TdxCustomSmartImage.HasClipboardFormat: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(SupportedFormats) to High(SupportedFormats) do
  begin
    if IsClipboardFormatAvailable(ImageClipboardFormats[SupportedFormats[I]]) then
      Exit(True);
  end;
end;

class function TdxCustomSmartImage.IsSupportClipboardFormat(AFormat: Word): Boolean;
var
  I: TdxImageDataFormat;
begin
  Result := False;
  if AFormat <> 0 then
    for I := Low(ImageClipboardFormats) to High(ImageClipboardFormats) do
    begin
      if ImageClipboardFormats[I] = AFormat then
        Exit(True);
    end;
end;

procedure TdxCustomSmartImage.LoadFromBits(AWidth, AHeight: Integer; const ABits: TRGBColors; AHasAlphaChannel: Boolean);
begin
  LoadFromBits(AWidth, AHeight, ABits, HasAlphaChannelFormatMap[AHasAlphaChannel]);
end;

procedure TdxCustomSmartImage.LoadFromBits(AWidth, AHeight: Integer; const ABits: TRGBColors; AAlphaFormat: TAlphaFormat);
begin
  CreateHandleFromBits(AWidth, AHeight, ABits, AAlphaFormat);
  Changed;
end;

procedure TdxCustomSmartImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE);
var
  ABitmap: TBitmap;
  AStream: TMemoryStream;
  ADataPtr: PByte;
begin
  if AFormat = CF_BITMAP then
  begin
    ABitmap := TBitmap.Create;
    try
      ABitmap.HandleType := bmDIB;
      ABitmap.LoadFromClipboardFormat(AFormat, AData, APalette);
      ABitmap.PixelFormat := pf24bit;
      SetBitmap(ABitmap);
    finally
      ABitmap.Free;
    end;
  end
  else
    if IsSupportClipboardFormat(AFormat) then
    begin
      ADataPtr := GlobalLock(AData);
      try
        AStream := TMemoryStream.Create;
        try
          AStream.WriteBuffer(ADataPtr^, GlobalSize(AData));
          AStream.Position := 0;
          LoadFromStream(AStream);
        finally
          AStream.Free;
        end;
      finally
        GlobalUnlock(AData);
      end;
    end;
end;

procedure TdxCustomSmartImage.LoadFromFieldValue(const AValue: Variant);
type
  TGraphicHeader = record
    Count: Word;
    HType: Word;
    Size: Longint;
  end;
var
  AHeader: TGraphicHeader;
  ASize: Longint;
  AStream: TMemoryStream;
  AValueAsString: AnsiString;
begin
  if dxVarIsBlob(AValue) then
  begin
    AStream := TMemoryStream.Create;
    try
      AValueAsString := dxVariantToAnsiString(AValue);
      ASize := Length(AValueAsString);
      if ASize >= SizeOf(AHeader) then
      begin
        TMemoryStreamAccess(AStream).SetPointer(@AValueAsString[1], ASize);
        AStream.Position := 0;
        AStream.Read(AHeader, SizeOf(AHeader));
        if (AHeader.Count <> 1) or (AHeader.HType <> $0100) or (AHeader.Size <> ASize - SizeOf(AHeader)) then
          AStream.Position := 0;
      end;
      if AStream.Size > 0 then
        LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
  end;
end;

procedure TdxCustomSmartImage.LoadFromResource(AInstance: THandle; const AResName: string; AResType: PChar);
var
  ABitmap: TBitmap;
  AStream: TStream;
begin
  if AResType = RT_BITMAP then
  begin
    ABitmap := TBitmap.Create;
    try
      ABitmap.LoadFromResourceName(AInstance, AResName);
      CreateHandleFromBitmap(ABitmap);
      Changed;
    finally
      ABitmap.Free;
    end;
  end
  else
  begin
    AStream := TResourceStream.Create(AInstance, AResName, AResType);
    try
      LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
  end;
end;

procedure TdxCustomSmartImage.LoadFromStream(AStream: TStream);
begin
  CreateHandleFromStream(AStream);
  Changed;
end;

procedure TdxCustomSmartImage.Resize(const AWidth, AHeight: Integer);
begin
  Resize(cxSize(AWidth, AHeight));
end;

procedure TdxCustomSmartImage.Resize(const ASize: TSize);
begin
  if HandleAllocated then
  begin
    if Handle is TdxSmartImageEmptyHandle then
      CreateHandleFromBits(ASize.cx, ASize.cy, TdxSmartImageHelper.AllocColors(ASize.cx, ASize.cy), afPremultiplied)
    else
      if not cxSizeIsEqual(ASize, Handle.Size) then
      begin
        Handle.Size := ASize;
        ImageData.FreeImageData;
      end
      else
        Exit;
  end
  else
    ImageData.FNewSize := ASize;

  Changed;
end;

procedure TdxCustomSmartImage.SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE);
var
  ABitmap: TBitmap;
  AStream: TMemoryStream;
  ABuffer: Pointer;
begin
  ABitmap := TBitmap.Create;
  try
    ABitmap.Width := Width;
    ABitmap.Height := Height;
    ABitmap.Canvas.Brush.Color := clWhite;
    ABitmap.Canvas.FillRect(ClientRect);
    Draw(ABitmap.Canvas, ClientRect);
    ABitmap.SaveToClipboardFormat(AFormat, AData, APalette);
  finally
    ABitmap.Free;
  end;
  if ImageClipboardFormats[ImageDataFormat] <> 0 then
  begin
    SetClipboardData(AFormat, AData);
    AStream := TMemoryStream.Create;
    try
      SaveToStream(AStream);
      AData := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, AStream.Size);
      try
        if (AData <> 0) then
        begin
          ABuffer := GlobalLock(AData);
          try
            Move(AStream.Memory^, ABuffer^, AStream.Size);
          finally
            GlobalUnlock(AData);
          end;
        end;
      except
        GlobalFree(AData);
        raise;
      end;
      AFormat := ImageClipboardFormats[ImageDataFormat];
    finally
      AStream.Free
    end;
  end;
end;

procedure TdxCustomSmartImage.SaveToFile(const Filename: string);
var
  ACodec: TdxSmartImageCodecClass;
  AStream: TStream;
begin
  AStream := TFileStream.Create(FileName, fmCreate);
  try
    ACodec := TdxSmartImageCodecsRepository.GetFormatByExt(ExtractFileExt(Filename));
    if ACodec <> nil then
      SaveToStreamByCodec(AStream, ACodec)
    else
      SaveToStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxCustomSmartImage.SaveToStream(AStream: TStream);
begin
  if not Empty and not HandleAllocated and (ImageData.FData <> nil) then
    AStream.CopyFrom(ImageData.FData, 0)
  else
    SaveToStreamByCodec(AStream, ImageDataFormat);
end;

procedure TdxCustomSmartImage.SaveToStreamByCodec(AStream: TStream; ACodec: TdxSmartImageCodecClass);
var
  ATempStream: TdxSmartImageDataStream;
begin
  if not Empty then
  begin
    ATempStream := ExportToStream(ACodec);
    try
      AStream.CopyFrom(ATempStream, ATempStream.Size);
    finally
      ATempStream.Free;
    end;
  end;
end;

procedure TdxCustomSmartImage.SaveToStreamByCodec(AStream: TStream; AFormat: TdxImageDataFormat);
begin
  SaveToStreamByCodec(AStream, TdxSmartImageCodecsRepository.GetFormatByID(AFormat));
end;

procedure TdxCustomSmartImage.SetBitmap(ABitmap: TBitmap);
begin
  CreateHandleFromBitmap(ABitmap);
  Changed;
end;

procedure TdxCustomSmartImage.Scale(const M, D: Integer);
begin
  Resize(MulDiv(Width, M, D), MulDiv(Height, M, D));
end;

procedure TdxCustomSmartImage.StartAnimation;
begin
  if (FAnimationController <> nil) or (AnimationFrameCount <= 1) then
    Exit;
  FAnimationController := ImageAnimationControllerClass.Create(Self);
  FAnimationController.StartAnimation;
end;

procedure TdxCustomSmartImage.StopAnimation;
begin
  FreeAndNil(FAnimationController);
end;

procedure TdxCustomSmartImage.StretchDraw(DC: HDC;
  const ADest, ASource: TdxRectF; AAlpha: Byte; APalette: IdxColorPalette);
begin
  Handle.Draw(DC, ADest, ASource, AAlpha, APalette);
end;

procedure TdxCustomSmartImage.StretchDraw(DC: HDC;
  const ADest, ASource: TRect; AAlpha: Byte; APalette: IdxColorPalette);
begin
  Handle.Draw(DC, ADest, ASource, AAlpha, APalette);
end;

procedure TdxCustomSmartImage.StretchDraw(DC: HDC; const ADest: TRect; AAlpha: Byte; APalette: IdxColorPalette);
begin
  if UseCache and CheckCache(cxSize(ADest), APalette) then
    FCache.Draw(DC, ADest, FCache.ClientRect, AAlpha, APalette)
  else
    StretchDraw(DC, ADest, ClientRect, AAlpha, APalette);
end;

function TdxCustomSmartImage.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  Result := inherited QueryInterface(IID, Obj);
  if Result = E_NOINTERFACE then
    Result := Handle.QueryInterface(IID, Obj);
end;

function TdxCustomSmartImage.CanStartAnimation: Boolean;
begin
  Result := Supports(Handle, IdxAnimatedImage) and (ImageDataFormat <> dxImageTiff);
end;

function TdxCustomSmartImage.CanCreateCache(const ASourceSize, ATargetSize: TSize): Boolean;
begin
  Result :=
    (ASourceSize.cx > ATargetSize.cx) and (ASourceSize.cy > ATargetSize.cy) and
    (ASourceSize.cx * ASourceSize.cy > 4 * ATargetSize.cx * ATargetSize.cy);
end;

function TdxCustomSmartImage.CheckCache(const ASize: TSize; const APalette: IdxColorPalette): Boolean;
begin
  if IsEqualGUID(FCachePaletteID, dxGetColorPaletteID(APalette)) then
    FreeAndNil(FCache);
  if (FCache = nil) or not cxSizeIsEqual(FCache.Size, ASize) then
  begin
    FreeAndNil(FCache);
    if CanCreateCache(Handle.Size, ASize) then
    begin
      FCache := CreateCache(ASize);
      FCachePaletteID := dxGetColorPaletteID(APalette);
    end;
  end;
  Result := FCache <> nil;
end;

function TdxCustomSmartImage.CreateCache(const ASize: TSize): TdxSmartImageCustomHandle;
begin
  Result := nil;
end;

function TdxCustomSmartImage.IsBitmapStream(AStream: TStream): Boolean;
var
  AHeader: TBitmapFileHeader;
  ASavedPosition: Int64;
begin
  ASavedPosition := AStream.Position;
  try
    Result := (AStream.Read(AHeader, SizeOf(AHeader)) = SizeOf(AHeader)) and (AHeader.bfType = $4D42);
  finally
    AStream.Position := ASavedPosition;
  end;
end;

function TdxCustomSmartImage.AssignFromClipboard: Boolean;
var
  AData: THandle;
  APalette: HPALETTE;
  I: Integer;
begin
  for I := Low(SupportedFormats) to High(SupportedFormats) do
  begin
    AData := GetClipboardData(ImageClipboardFormats[SupportedFormats[I]]);
    if AData <> 0 then
    begin
      APalette := 0;
      LoadFromClipboardFormat(ImageClipboardFormats[SupportedFormats[I]], AData, APalette);
      Exit(True);
    end;
  end;
  Result := False;
end;

procedure TdxCustomSmartImage.AssignFromGraphic(AGraphic: TGraphic);
var
  ABitmap: TBitmap;
  AStream: TMemoryStream;
begin
  if AGraphic is TBitmap then
    SetBitmap(TBitmap(AGraphic))
  else
    if AGraphic is TMetafile then
    begin
      AStream := TMemoryStream.Create;
      try
        AGraphic.SaveToStream(AStream);
        AStream.Position := 0;
        LoadFromStream(AStream);
      finally
        AStream.Free;
      end;
    end
    else
    begin
      ABitmap := TBitmap.Create;
      try
        if AGraphic is TIcon then
          ABitmap.Assign(AGraphic)
        else
        begin
          ABitmap.PixelFormat := pf24bit;
          ABitmap.SetSize(AGraphic.Width, AGraphic.Height);
          ABitmap.Canvas.Draw(0, 0, AGraphic);
        end;
        SetBitmap(ABitmap);
      finally
        ABitmap.Free;
      end;
    end;
end;

procedure TdxCustomSmartImage.AssignFromSmartImage(AImage: TdxCustomSmartImage);
var
  ABitmap: TBitmap;
  AStream: TMemoryStream;
begin
  if AImage.Empty then
    Clear
  else

  if not AImage.ImageData.Empty then
  begin
    FreeHandle;
    ImageData.Assign(AImage.ImageData);
    Changed;
  end
  else

  if AImage.ImageDataFormat = dxImageBitmap then
  begin
    ABitmap := AImage.GetAsBitmap;
    try
      SetBitmap(ABitmap);
    finally
      ABitmap.Free;
    end;
  end
  else
  begin
    AStream := TMemoryStream.Create;
    try
      AImage.SaveToStream(AStream);
      AStream.Position := 0;
      LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
  end;
end;

procedure TdxCustomSmartImage.AssignTo(ADest: TPersistent);
var
  ABitmap: TBitmap;
begin
  if ADest is TdxCustomSmartImage then
    ADest.Assign(Self)
  else
    if ADest is TMetafile then
      AssignToMetaFile(TMetafile(ADest))
    else
      if ADest is TBitmap then
      begin
        ABitmap := GetAsBitmap;
        try
          ADest.Assign(ABitmap);
        finally
          ABitmap.Free;
        end;
      end
      else
        inherited AssignTo(ADest);
end;

procedure TdxCustomSmartImage.AssignToMetaFile(ADest: TMetafile);
const
  FormatMap: array[Boolean] of TdxImageDataFormat = (dxImageWmf, dxImageEmf);
var
  AStream: TMemoryStream;
begin
  AStream:= ExportToStream(TdxSmartImageCodecsRepository.GetFormatByID(FormatMap[ADest.Enhanced]));
  try
    AStream.Position := 0;
    ADest.LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxCustomSmartImage.CreateHandleFromBitmap(ABitmap: TBitmap);
begin
  FreeHandle;
  ImageData.LoadFromBitmap(ABitmap);
end;

procedure TdxCustomSmartImage.CreateHandleFromBits(AWidth, AHeight: Integer; const ABits: TRGBColors; AAlphaFormat: TAlphaFormat);
begin
  FreeHandle;
  ImageData.LoadFromBits(ABits, cxSize(AWidth, AHeight), AAlphaFormat);
end;

procedure TdxCustomSmartImage.CreateHandleFromHBITMAP(ABitmap: HBITMAP; APalette: HPALETTE);
var
  AImage: TBitmap;
begin
  AImage := TBitmap.Create;
  try
    AImage.Handle := ABitmap;
    AImage.Palette := APalette;
    CreateHandleFromBitmap(AImage);
    if ABitmap <> 0 then
      AImage.ReleaseHandle;
    if APalette <> 0 then
      AImage.ReleasePalette;
  finally
    AImage.Free;
  end;
end;

procedure TdxCustomSmartImage.CreateHandleFromImageData;
begin
  if ImageData.Empty then
    FHandle := TdxSmartImageEmptyHandle.Create
  else
    Check(TdxSmartImageCodecsRepository.Load(ImageData, FHandle), sdxErrorUnsupportedImageFormat);

  if cxSizeIsValid(ImageData.FNewSize) then
  begin
    Resize(ImageData.FNewSize);
    HandleNeeded;
  end;
end;

procedure TdxCustomSmartImage.CreateHandleFromStream(AStream: TStream);
var
  ABitmap: TBitmap;
begin
  FreeHandle;
  if IsBitmapStream(AStream) then
  begin
    ABitmap := TBitmap.Create;
    try
      ABitmap.LoadFromStream(AStream);
      CreateHandleFromBitmap(ABitmap);
    finally
      ABitmap.Free;
    end;
  end
  else
    ImageData.LoadFromStream(AStream);
end;

procedure TdxCustomSmartImage.FreeHandle;
begin
  FreeAndNil(FHandle);
  ImageData.FreeImageData;
end;

procedure TdxCustomSmartImage.PopulateImageDataFromHandle;
begin
  if ImageData.FData = nil then
    ImageData.FData := ExportToStream(ImageCodec);
end;

function TdxCustomSmartImage.ExportToStream(ACodec: TdxSmartImageCodecClass): TdxSmartImageDataStream;
begin
  Check(ACodec <> nil, sdxErrorUnsupportedOperation);
  if (ACodec.ID = ImageDataFormat) and (ImageData.FData <> nil) and (ACodec.ID <> dxImageUnknown) then
    Result := ImageData.FData.Clone
  else
  begin
    Result := TdxSmartImageDataStream.Create;
    try
      Check(ACodec.Save(Result, Handle), Format(sdxErrorCannotExportImage, [ACodec.ClassName, Ord(ImageDataFormat)]));
      Result.Position := 0;
    except
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

procedure TdxCustomSmartImage.Changed;
begin
  Changed(Self);
end;

procedure TdxCustomSmartImage.Changed(Sender: TObject);
begin
  FreeAndNil(FCache);
  FAlphaStateAssigned := False;
  inherited Changed(Sender);
end;

procedure TdxCustomSmartImage.CheckIsImageDataValid;
var
  ACodec: TdxSmartImageCodecClass;
  ASize: TSize;
begin
  if not HandleAllocated then
  begin
    if not TdxSmartImageCodecsRepository.GetImageInfo(ImageData, ASize, ACodec) then
      HandleNeeded;
  end;
end;

procedure TdxCustomSmartImage.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  StretchDraw(ACanvas.Handle, ARect);
end;

function TdxCustomSmartImage.Equals(Graphic: TGraphic): Boolean;
begin
  if Graphic is TdxCustomSmartImage then
    Result := Compare(TdxCustomSmartImage(Graphic))
  else
    Result := inherited Equals(Graphic);
end;

function TdxCustomSmartImage.GetEmpty: Boolean;
begin
  if HandleAllocated then
    Result := Handle.Empty
  else
    Result := ImageData.Empty;
end;

function TdxCustomSmartImage.GetHeight: Integer;
begin
  Result := Size.cy;
end;

function TdxCustomSmartImage.GetTransparent: Boolean;
begin
  Result := IsAlphaUsed;
end;

function TdxCustomSmartImage.GetWidth: Integer;
begin
  Result := Size.cx;
end;

procedure TdxCustomSmartImage.SetHeight(Value: Integer);
begin
  // do nothing
end;

procedure TdxCustomSmartImage.SetWidth(Value: Integer);
begin
  // do nothing
end;

procedure TdxCustomSmartImage.Check(AResult: Boolean; const AMessage: string);
begin
  if not AResult then
    raise EdxException.Create(AMessage);
end;

function TdxCustomSmartImage.GetActiveFrame: LongWord;
var
  AIntf: IdxAnimatedImage;
begin
  if Supports(Handle, IdxAnimatedImage, AIntf) then
    Result := AIntf.GetActiveFrame
  else
    Result := 0;
end;

function TdxCustomSmartImage.GetAlphaState: TdxAlphaState;
begin
  if not FAlphaStateAssigned then
  begin
    FAlphaStateAssigned := True;
    FAlphaState := Handle.GetAlphaState;
  end;
  Result := FAlphaState;
end;

function TdxCustomSmartImage.GetAnimation: Boolean;
begin
  Result := (AnimationController <> nil) and AnimationController.Active;
end;

function TdxCustomSmartImage.GetAnimationFrameCount: Cardinal;
var
  AIntf: IdxAnimatedImage;
begin
  if Supports(Handle, IdxAnimatedImage, AIntf) then
    Result := AIntf.GetAnimationFrameCount
  else
    Result := 1;
end;

function TdxCustomSmartImage.GetAnimationFrameDelay: Integer;
var
  AIntf: IdxAnimatedImage;
begin
  Result := FAnimationFrameDelay;
  if Result <> 0 then
    Exit;
  if Supports(Handle, IdxAnimatedImage, AIntf) then
    Result := AIntf.GetAnimationFrameDelay;
end;

function TdxCustomSmartImage.GetAnimationLoopCount: Integer;
var
  AIntf: IdxAnimatedImage;
begin
  if Supports(Handle, IdxAnimatedImage, AIntf) then
    Result := AIntf.GetAnimationLoopCount
  else
    Result := 0;
end;

function TdxCustomSmartImage.GetAsBitmap: TBitmap;
begin
  Result := Handle.GetAsBitmap
end;

function TdxCustomSmartImage.GetBitmapBits: TRGBColors;
var
  ABitmap: TBitmap;
begin
  ABitmap := GetAsBitmap;
  try
    dxCoreGraphics.GetBitmapBits(ABitmap, Result, True);
  finally
    ABitmap.Free;
  end;
end;

function TdxCustomSmartImage.GetClientRect: TRect;
begin
  Result := cxRect(Size);
end;

function TdxCustomSmartImage.GetHandle: TdxSmartImageCustomHandle;
begin
  HandleNeeded;
  Result := FHandle;
end;

function TdxCustomSmartImage.GetHandleAllocated: Boolean;
begin
  Result := FHandle <> nil;
end;

function TdxCustomSmartImage.GetHashCode: Integer;
begin
  Result := Handle.GetHashCode;
end;

function TdxCustomSmartImage.GetImageCodec: TdxSmartImageCodecClass;
var
  AIntf: IdxImageDataFormat;
  AIntfEx: IdxImageDataFormatEx;
begin
  if Supports(Handle, IdxImageDataFormatEx, AIntfEx) then
    Result := AIntfEx.GetImageFormat
  else
    if Supports(Handle, IdxImageDataFormat, AIntf) then
      Result := TdxSmartImageCodecsRepository.GetFormatByID(AIntf.GetImageFormat)
    else
      Result := nil;
end;

function TdxCustomSmartImage.GetImageDataFormat: TdxImageDataFormat;
var
  AIntf: IdxImageDataFormat;
begin
  if Supports(Handle, IdxImageDataFormat, AIntf) then
    Result := AIntf.GetImageFormat
  else
    Result := dxImageUnknown;
end;

function TdxCustomSmartImage.GetIsAlphaUsed: Boolean;
begin
  Result := AlphaState <> asOpaque;
end;

function TdxCustomSmartImage.GetSize: TSize;
var
  ACodec: TdxSmartImageCodecClass;
begin
  if not HandleAllocated then
  begin
    if ImageData.Empty then
      Exit(cxNullSize);
    if cxSizeIsValid(ImageData.FNewSize) then
      Exit(ImageData.FNewSize);
    if cxSizeIsValid(ImageData.FSize) then
      Exit(ImageData.FSize);
    if TdxSmartImageCodecsRepository.GetImageInfo(ImageData, Result, ACodec) then
    begin
      ImageData.FSize := Result;
      Exit;
    end;
  end;
  Result := Handle.Size
end;

procedure TdxCustomSmartImage.SetActiveFrame(AValue: LongWord);
var
  AIntf: IdxAnimatedImage;
begin
  if Supports(Handle, IdxAnimatedImage, AIntf) then
  begin
    AValue := Max(0, AValue);
    if AValue >= AnimationFrameCount then
    begin
      if (AnimationLoop = bTrue) or ((AnimationLoop = bDefault) and
        ((AnimationLoopCount = 0) or (AnimationLoopIndex < AnimationLoopCount))) then
      begin
        AValue := 0;
        if (AnimationLoopCount <> 0) and (AnimationLoop = bDefault) then
          Inc(FAnimationLoopIndex);
      end
      else
        AValue := Max(0, AnimationFrameCount - 1);
    end;

    if AValue <> ActiveFrame then
    begin
      AIntf.SetActiveFrame(AValue);
      Changed;
    end;
  end;
end;

procedure TdxCustomSmartImage.SetAnimation(AValue: Boolean);
begin
  if AValue <> Animation then
  begin
    if AValue and CanStartAnimation then
      StartAnimation
    else
      StopAnimation;
  end;
end;

procedure TdxCustomSmartImage.SetAnimationFrameDelay(AValue: Integer);
begin
  if AValue <> FAnimationFrameDelay then
  begin
    FAnimationFrameDelay := Max(0, AValue);
    if Animation then
    begin
      StopAnimation;
      StartAnimation;
    end;
  end;
end;

procedure TdxCustomSmartImage.SetHandle(const Value: TdxSmartImageCustomHandle);
begin
  if Handle <> Value then
  begin
    FreeHandle;
    FHandle := Value;
    Changed;
  end;
end;

procedure TdxCustomSmartImage.SetImageCodec(const AValue: TdxSmartImageCodecClass);
var
  AStream: TMemoryStream;
begin
  if AValue <> ImageCodec then
  begin
    AStream := TMemoryStream.Create;
    try
      SaveToStreamByCodec(AStream, AValue);
      AStream.Position := 0;
      LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
  end;
end;

procedure TdxCustomSmartImage.SetImageDataFormat(AValue: TdxImageDataFormat);
begin
  ImageCodec := TdxSmartImageCodecsRepository.GetFormatByID(AValue);
end;

procedure TdxCustomSmartImage.SetUseCache(const Value: Boolean);
begin
  if FUseCache <> Value then
  begin
    FUseCache := Value;
    FreeAndNil(FCache);
  end;
end;

{ TdxSmartImageCodecsRepository }

class function TdxSmartImageCodecsRepository.Contains(ACodec: TdxSmartImageCodecClass): Boolean;
begin
  Result := (Count > 0) and (FList.IndexOf(ACodec) >= 0);
end;

class function TdxSmartImageCodecsRepository.GetFormatFromStream(AStream: TStream): TdxSmartImageCodecClass;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.CanLoadStream(AStream) then
      Exit;
  end;
  Result := nil;
end;

class function TdxSmartImageCodecsRepository.GetImageInfo(
  const AFileName: string; out ASize: TSize; out ACodec: TdxSmartImageCodecClass): Boolean;
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := GetImageInfo(AStream, ASize, ACodec);
  finally
    AStream.Free;
  end;
end;

class function TdxSmartImageCodecsRepository.GetImageInfo(
  const AImageData: TdxSmartImageData; out ASize: TSize; out ACodec: TdxSmartImageCodecClass): Boolean;
var
  I: Integer;
begin
  Result := False;
  if not AImageData.Empty then
  begin
    if Length(AImageData.FBits) > 0 then
    begin
      for I := 0 to Count - 1 do
      begin
        Result := Items[I].CanLoadFromBits;
        if Result then
        begin
          ACodec := Items[I];
          ASize := AImageData.FSize;
          Break;
        end;
      end;
    end
    else
    begin
      AImageData.FData.Position := 0;
      Result := GetImageInfo(AImageData.FData, ASize, ACodec);
    end;
  end;
end;

class function TdxSmartImageCodecsRepository.GetImageInfo(
  const AStream: TStream; out ASize: TSize; out ACodec: TdxSmartImageCodecClass): Boolean;
var
  APosition: Int64;
  I: Integer;
begin
  APosition := AStream.Position;
  for I := 0 to Count - 1 do
  begin
    if Items[I].GetSize(AStream, ASize) then
    begin
      ACodec := Items[I];
      Exit(True);
    end;
    AStream.Position := APosition;
  end;
  Result := False;
end;

class function TdxSmartImageCodecsRepository.GetFormatByExt(const Ext: string): TdxSmartImageCodecClass;
var
  I: Integer;
begin
  if Ext <> '' then
    for I := 0 to Count - 1 do
    begin
      Result := Items[I];
      if SameText(Result.Ext, Ext) then
        Exit;
    end;
  Result := nil;
end;

class function TdxSmartImageCodecsRepository.GetFormatByID(ID: TdxImageDataFormat): TdxSmartImageCodecClass;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.ID = ID then
      Exit;
  end;
  Result := nil;
end;

class function TdxSmartImageCodecsRepository.Load(const ABits: TRGBColors; AAlphaFormat: TAlphaFormat;
  AWidth, AHeight: Integer; out AHandle: TdxSmartImageCustomHandle): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Items[I].Load(ABits, AAlphaFormat, AWidth, AHeight, AHandle) then
      Exit(True);
  end;
  Result := False;
end;

class function TdxSmartImageCodecsRepository.Load(
  const AImageData: TdxSmartImageData; out AHandle: TdxSmartImageCustomHandle): Boolean;
begin
  if Length(AImageData.FBits) > 0 then
    Result := Load(AImageData.FBits, AImageData.FBitsFormat, AImageData.FSize.cx, AImageData.FSize.cy, AHandle)
  else
  begin
    AImageData.FData.Position := 0;
    Result := Load(AImageData.FData, AHandle);
  end;
end;

class function TdxSmartImageCodecsRepository.Load(const AStream: TStream; out AHandle: TdxSmartImageCustomHandle): Boolean;
var
  ASavedPosition: Int64;
  I: Integer;
begin
  ASavedPosition := AStream.Position;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Load(AStream, AHandle) then
      Exit(True);
    AStream.Position := ASavedPosition;
  end;
  Result := False;
end;

class procedure TdxSmartImageCodecsRepository.Register(ACodec: TdxSmartImageCodecClass);
begin
  if FList = nil then
    FList := TList.Create;
  FList.Add(ACodec);
end;

class procedure TdxSmartImageCodecsRepository.Unregister(ACodec: TdxSmartImageCodecClass);
begin
  if FList <> nil then
  begin
    FList.Remove(ACodec);
    if FList.Count = 0 then
      FreeAndNil(FList);
  end;
end;

class function TdxSmartImageCodecsRepository.GetCount: Integer;
begin
  if FList <> nil then
    Result := FList.Count
  else
    Result := 0;
end;

class function TdxSmartImageCodecsRepository.GetItem(Index: Integer): TdxSmartImageCodecClass;
begin
  Result := TdxSmartImageCodecClass(FList.List[Index]);
end;

{ TdxSmartImageHelper }

class function TdxSmartImageHelper.AllocColors(AWidth, AHeight: Integer; AColor: TdxAlphaColor): TRGBColors;
var
  I: Integer;
begin
  SetLength(Result, AWidth * AHeight);
  for I := 0 to Length(Result) - 1 do
    Integer(Result[I]) := AColor;
end;

class function TdxSmartImageHelper.AreColorsPremultiplied(const ABits: TRGBColors): Boolean;
var
  Q: TRGBQuad;
  I: Integer;
begin
  for I := 0 to Length(ABits) - 1 do
  begin
    Q := ABits[I];
    if Max(Max(Q.rgbBlue, Q.rgbGreen), Q.rgbRed) > Q.rgbReserved then
      Exit(False);
  end;
  Result := True;
end;

class function TdxSmartImageHelper.HasAlpha(const ABits: TRGBColors): Boolean;

  function HasNonTransparentPixels: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Length(ABits) - 1 do
    begin
      Result := ABits[I].rgbReserved > 0;
      if Result then
        Break;
    end;
  end;

  function CheckTransparentPixels: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Length(ABits) - 1 do
    begin
      Result := DWORD(ABits[I]) = 0;
      if not Result then
        Break;
    end;
  end;

begin
  Result := HasNonTransparentPixels or CheckTransparentPixels;
end;

end.
