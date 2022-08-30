{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE       }
{   PROGRAM ONLY.                                                    }
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

unit cxImageList;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, SysUtils, Controls, Graphics, Menus, CommCtrl, ComCtrls, ImgList, ActiveX, Contnrs,
  dxCore, dxCoreGraphics, dxMessages, cxClasses, cxGeometry, dxGDIPlusClasses, dxGDIPlusApi, dxSmartImage, dxDPIAwareUtils;

type

  { TcxImageInfo }

  TcxImageInfo = class(TPersistent)
  strict private
    FImage: TGraphic;
    FIsAlphaUsed: TdxDefaultBoolean;
    FMask: TBitmap;
    FMaskColor: TColor;

    procedure AssignBitmap(ASourceBitmap, ADestBitmap: TBitmap);
    function GetImageClass: TGraphicClass;
    function GetImageType: string;
    function GetIsAlphaUsed: Boolean;
    procedure SetImage(Value: TGraphic);
    procedure SetImageClass(AValue: TGraphicClass);
    procedure SetMask(Value: TBitmap);
  private
    FInternalMask: TBitmap;

    function HasMask: Boolean;
    function HasNativeHandle: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ConvertTo32Bit;
    procedure Dormant;
    procedure FlushMask;
    procedure Resize(const ASize: TSize);

    property Image: TGraphic read FImage write SetImage;
    property ImageClass: TGraphicClass read GetImageClass write SetImageClass;
    property ImageType: string read GetImageType;
    property IsAlphaUsed: Boolean read GetIsAlphaUsed;
    property Mask: TBitmap read FMask write SetMask;
    property MaskColor: TColor read FMaskColor write FMaskColor;
  end;

  { TcxImageInfoHelper }

  TcxImageInfoHelper = class
  public
    class procedure CopyRect(ADest: TBitmap; const ADestRect: TRect; ASource: TGraphic); overload;
    class procedure CopyRect(ADest: TBitmap; const ADestRect: TRect; ASource: TGraphic; const ASourceRect: TRect); overload;
    class procedure Dormant(AGraphic: TGraphic);
    class function GetDefaultTransparentColor(AImage: TGraphic; AMask: TBitmap): TColor;
    class function GetPixel(AGraphic: TGraphic; X, Y: Integer): TColor;
    class function GetPixelFormat(AGraphic: TGraphic): TPixelFormat;
    class function IsAlphaUsed(AGraphic: TGraphic): Boolean;
    class procedure Resize(AGraphic: TGraphic; const ASize: TSize);
  end;

  { TcxBaseImageList }

  TcxBaseImageList = class(TDragImageList, IdxSourceDPI)
  strict private
    FSourceDPI: Integer;

    // IdxSourceDPI
    function GetSourceDPI: Integer;
    procedure SetSourceDPI(AValue: Integer);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property SourceDPI: Integer read FSourceDPI write SetSourceDPI;
  end;

  { TcxCustomImageList }

  TcxCustomImageList = class(TcxBaseImageList)
  strict private const
    DXILSignature: Integer = $494C4458; //DXIL
    DXILVersion: Word = 1;
  strict private
    FAlphaBlending: Boolean;
    FFormatVersion: Integer;
    FImages: TCollection;
    FLockCount: Integer;
    FSynchronizationLockCount: Integer;

    procedure ReadDesignInfo(AReader: TReader);
    procedure ReadFormatVersion(AReader: TReader);
    procedure ReadImageInfo(AReader: TReader);
    procedure WriteDesignInfo(AWriter: TWriter);
    procedure WriteFormatVersion(AWriter: TWriter);
    procedure WriteImageInfo(AWriter: TWriter);

    function IsSynchronizationLocked: Boolean;
    procedure SynchronizeImageInfo;
    procedure SynchronizeHandle;

    procedure AddToInternalCollection(AImage: TGraphic; AMask: TBitmap; AMaskColor: TColor = clNone);
    class procedure CheckImageSize(AImage: TBitmap; AWidth, AHeight: Integer);
    class procedure ReleaseImageInfo(var AImageInfo: TImageInfo);
    procedure DormantImage(AIndex: Integer);
    function GetImageCount(AImage: TGraphic; AWidth, AHeight: Integer): Integer;

    function GetCompressData: Boolean;
    function GetHandle: HImageList;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetCompressData(Value: Boolean);
    procedure SetHandle(Value: HImageList);
    procedure SetHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  protected
    function ChangeLocked: Boolean;
  {$IFNDEF DELPHI22}
    procedure Change; override;
  {$ENDIF}
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True); override;
    procedure DoDrawEx(AIndex: Integer; ACanvas: TCanvas;
      const ARect: TRect; AStyle: Cardinal; AStretch, ASmoothResize, AEnabled: Boolean);

    procedure Initialize; override;
    procedure Finalize;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Dormant;

    procedure GetImageInfoCore(AIndex: Integer; AImage, AMask: TBitmap;
      APalette: IdxColorPalette; var AIsAlphaUsed: TdxDefaultBoolean); overload;
    class procedure GetImageInfoCore(AImages: TCustomImageList; AIndex: Integer;
      AImage, AMask: TBitmap; APalette: IdxColorPalette; var AIsAlphaUsed: TdxDefaultBoolean); overload;

    // for cxImageListEditor
    function AddImageInfo(AImageInfo: TcxImageInfo): Integer;
    function CanSplitImage(AImage: TGraphic): Boolean;
    procedure ConvertTo32bit;
    procedure InternalCopyImageInfos(AImageList: TcxCustomImageList; AStartIndex, AEndIndex: Integer);
    procedure InternalCopyImages(AImageList: TCustomImageList; AStartIndex, AEndIndex: Integer);
    function GetImageInfo(AIndex: Integer): TcxImageInfo; overload;
    function HasRasterImages: Boolean;
    procedure Resize(const ASize: TSize);
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  {$IFDEF DELPHI22}
    procedure Change; override;
  {$ENDIF}

    // Base Functions
    function Add(AImage: TBitmap; AMask: TBitmap): Integer; overload;
    function Add(AImage: TdxSmartImage): Integer; overload;
    function Add(AImage: TGraphic; AMask: TBitmap): Integer; overload;
    function AddIcon(AIcon: TIcon): Integer;
    function AddMasked(AImage: TGraphic; AMaskColor: TColor): Integer;
    procedure Move(ACurIndex, ANewIndex: Integer);
    procedure Delete(AIndex: Integer);

    // Subsidiary Functions
    function AddBitmap(AImage, AMask: TBitmap; AMaskColor: TColor; AStretch, ASmooth: Boolean): Integer;
    function AddImage(AImages: TCustomImageList; AIndex: Integer): Integer;
    procedure AddImages(AImages: TCustomImageList);
    procedure CopyImages(AImages: TCustomImageList; AStartIndex: Integer = 0; AEndIndex: Integer = -1);
    procedure Clear;
    procedure Insert(AIndex: Integer; AImage: TGraphic; AMask: TBitmap);
    procedure InsertIcon(AIndex: Integer; AIcon: TIcon);
    procedure InsertMasked(AIndex: Integer; AImage: TGraphic; AMaskColor: TColor);
    procedure Replace(AIndex: Integer; AImage: TGraphic; AMask: TBitmap);
    procedure ReplaceIcon(AIndex: Integer; AIcon: TIcon);
    procedure ReplaceMasked(AIndex: Integer; AImage: TGraphic; AMaskColor: TColor);

    procedure DrawOverlay(Canvas: TCanvas; X, Y: Integer;
      ImageIndex: Integer; Overlay: TOverlay; Enabled: Boolean = True); overload;
    procedure DrawOverlay(Canvas: TCanvas; X, Y: Integer;
      ImageIndex: Integer; Overlay: TOverlay; ADrawingStyle: TDrawingStyle;
      AImageType: TImageType; Enabled: Boolean = True); overload;
    function LoadImage(AInstance: THandle; const AResourceName: string;
      AMaskColor: TColor = clDefault; AWidth: Integer = 0; AFlags: TLoadResources = []): Boolean;
    function FileLoad(AResType: TResType; const AName: string; AMaskColor: TColor): Boolean;
    function GetResource(AResType: TResType; const AName: string;
      AWidth: Integer; ALoadFlags: TLoadResources; AMaskColor: TColor): Boolean;
    function GetInstRes(AInstance: THandle; AResType: TResType; const AName: string;
      AWidth: Integer; ALoadFlags: TLoadResources; AMaskColor: TColor): Boolean; overload;
    function GetInstRes(AInstance: THandle; AResType: TResType; AResID: DWORD;
      AWidth: Integer; ALoadFlags: TLoadResources; AMaskColor: TColor): Boolean; overload;
    function ResourceLoad(AResType: TResType; const AName: string; AMaskColor: TColor): Boolean;
    function ResInstLoad(AInstance: THandle; AResType: TResType; const AName: string; AMaskColor: TColor): Boolean;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;

    procedure Draw(ACanvas: TCanvas; const ARect: TRect; AIndex: Integer;
      AStretch: Boolean = True; ASmoothResize: Boolean = False; AEnabled: Boolean = True); overload;

    procedure GetImageInfo(AIndex: Integer; AImage, AMask: TBitmap; APalette: IdxColorPalette = nil); overload;
    procedure GetImage(AIndex: Integer; AImage: TBitmap); overload;
    procedure GetImage(AIndex: Integer; AImage: TdxSmartImage); overload;
    procedure GetMask(AIndex: Integer; AMask: TBitmap);

    class procedure GetImageInfo(AImages: TCustomImageList; AIndex: Integer; AImage, AMask: TBitmap; APalette: IdxColorPalette = nil); overload;
    class procedure GetImageInfo(AHandle: HIMAGELIST; AIndex: Integer; AImage, AMask: TBitmap); overload;
    class function GetPixelFormat(AHandle: HIMAGELIST): Integer;
    class function IsEquals(AImages1, AImages2: TCustomImageList): Boolean;

    procedure SetSize(AWidth, AHeight: Integer); overload;
    procedure SetSize(const ASize: TSize); overload;

    property AlphaBlending: Boolean read FAlphaBlending write FAlphaBlending;
    property Handle: HImageList read GetHandle write SetHandle;
  published
    property BlendColor;
    property BkColor;
    property ColorDepth default cd32Bit;
    property CompressData: Boolean read GetCompressData write SetCompressData default False;
    property DrawingStyle;
    property Height: Integer read GetHeight write SetHeight default 16;
    property ImageType;
    property ShareImages;
    property Width: Integer read GetWidth write SetWidth default 16;
    property OnChange;
  end;

function cxGetImageListStyle(ADrawingStyle: TDrawingStyle; AImageType: TImageType): DWORD;
implementation

uses
  cxGraphics, Math, dxFading;

type
  TBitmapAccess = class(TBitmap);
  TcxBitmap32Access = class(TcxBitmap32);

  { TcxImageInfoItem }

  TcxImageInfoItem = class(TCollectionItem)
  strict private
    FImageInfo: TcxImageInfo;

    function GetCompressData: Boolean;
    function GetImage: TGraphic;
    function GetImageClass: string;
    function GetMask: TBitmap;
    function GetMaskColor: TColor;
    function IsImageClassStored: Boolean;
    procedure SetCompressData(Value: Boolean);
    procedure SetImage(Value: TGraphic);
    procedure SetImageClass(const Value: string);
    procedure SetMask(Value: TBitmap);
    procedure SetMaskColor(Value: TColor);
  public
    constructor Create(ACollection: TCollection); overload; override;
    constructor Create(ACollection: TCollection; const AImage: TGraphic;
      const AMask: TBitmap; AMaskColor: TColor = clNone); reintroduce; overload;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property CompressData: Boolean read GetCompressData write SetCompressData;
    property ImageInfo: TcxImageInfo read FImageInfo;
  published
    property ImageClass: string read GetImageClass write SetImageClass stored IsImageClassStored;
    property Image: TGraphic read GetImage write SetImage;
    property Mask: TBitmap read GetMask write SetMask;
    property MaskColor: TColor read GetMaskColor write SetMaskColor default clNone;
  end;

  { TcxImageInfoCollection }

  TcxImageInfoCollection = class(TCollection)
  strict private
    FCompressData: Boolean;
    FImageList: TcxCustomImageList;

    procedure SetCompressData(Value: Boolean);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AImageList: TcxCustomImageList);
    function Add(AImage: TGraphic; AMask: TBitmap; AMaskColor: TColor = clNone): TCollectionItem;
    procedure Move(ACurrentIndex, ANewIndex: Integer);
    procedure Delete(AIndex: Integer);
    function GetVectorImageCount: Integer;

    property CompressData: Boolean read FCompressData write SetCompressData;
  end;

procedure cxCopyBitmap(ADestBitmap, ASrcBitmap: TBitmap); overload;
begin
  cxDrawBitmap(ADestBitmap.Canvas.Handle, ASrcBitmap, cxGetImageClientRect(ADestBitmap), cxNullPoint);
  TBitmapAccess(ADestBitmap).Changed(ADestBitmap);
end;

function cxGetImageListStyle(ADrawingStyle: TDrawingStyle; AImageType: TImageType): DWORD;
const
  DrawingStyles: array[TDrawingStyle] of DWORD = (ILD_FOCUS, ILD_SELECTED, ILD_NORMAL, ILD_TRANSPARENT);
  ImageTypes: array[TImageType] of DWORD = (0, ILD_MASK);
begin
  Result := DrawingStyles[ADrawingStyle] or ImageTypes[AImageType];
end;

{ TcxImageInfoItem }

constructor TcxImageInfoItem.Create(ACollection: TCollection);
begin
  inherited;
  FImageInfo := TcxImageInfo.Create;
  CompressData := TcxImageInfoCollection(ACollection).CompressData;
end;

constructor TcxImageInfoItem.Create(ACollection: TCollection;
  const AImage: TGraphic; const AMask: TBitmap; AMaskColor: TColor);
begin
  Create(ACollection);
  Image := AImage;
  Mask := AMask;
  MaskColor := AMaskColor;
end;

destructor TcxImageInfoItem.Destroy;
begin
  FreeAndNil(FImageInfo);
  inherited Destroy;
end;

procedure TcxImageInfoItem.Assign(Source: TPersistent);
begin
  if Source is TcxImageInfoItem then
    FImageInfo.Assign(TcxImageInfoItem(Source).ImageInfo)
  else
    inherited;
end;

function TcxImageInfoItem.GetCompressData: Boolean;
begin
  Result := TcxBitmap(Mask).CompressData;
  if Result and (Image is TcxBitmap) then
    Result := TcxBitmap(Image).CompressData;
end;

function TcxImageInfoItem.GetImage: TGraphic;
begin
  Result := FImageInfo.Image;
end;

function TcxImageInfoItem.GetImageClass: string;
begin
  Result := Image.ClassName;
end;

function TcxImageInfoItem.GetMask: TBitmap;
begin
  Result := FImageInfo.Mask;
end;

function TcxImageInfoItem.GetMaskColor: TColor;
begin
  Result := FImageInfo.MaskColor;
end;

function TcxImageInfoItem.IsImageClassStored: Boolean;
begin
  Result := not (Image is TcxBitmap);
end;

procedure TcxImageInfoItem.SetCompressData(Value: Boolean);
begin
  if CompressData <> Value then
  begin
    if Image is TcxBitmap then
      TcxBitmap(Image).CompressData := Value;
    TcxBitmap(Mask).CompressData := Value;
  end;
end;

procedure TcxImageInfoItem.SetImage(Value: TGraphic);
begin
  FImageInfo.Image := Value;
end;

procedure TcxImageInfoItem.SetImageClass(const Value: string);
var
  AClass: TClass;
  AImage: TGraphic;
begin
  AClass := GetClass(Value);
  if (AClass = nil) or not AClass.InheritsFrom(TGraphic) then
    raise EdxException.CreateFmt('The "%s" is invalid graphic class', [Value]);

  AImage := TGraphicClass(AClass).Create;
  try
    FImageInfo.Image := AImage;
  finally
    AImage.Free;
  end;
end;

procedure TcxImageInfoItem.SetMask(Value: TBitmap);
begin
  FImageInfo.Mask := Value;
end;

procedure TcxImageInfoItem.SetMaskColor(Value: TColor);
begin
  FImageInfo.MaskColor := Value;
end;

{ TcxImageInfoCollection }

constructor TcxImageInfoCollection.Create(AImageList: TcxCustomImageList);
begin
  inherited Create(TcxImageInfoItem);
  FImageList := AImageList;
end;

function TcxImageInfoCollection.Add(AImage: TGraphic; AMask: TBitmap; AMaskColor: TColor = clNone): TCollectionItem;
begin
  Result := TcxImageInfoItem.Create(Self, AImage, AMask, AMaskColor);
end;

procedure TcxImageInfoCollection.Move(ACurrentIndex, ANewIndex: Integer);
begin
  Items[ACurrentIndex].Index := ANewIndex;
end;

procedure TcxImageInfoCollection.Delete(AIndex: Integer);
begin
  if AIndex = -1 then
    Clear
  else
    inherited Delete(AIndex);
end;

function TcxImageInfoCollection.GetVectorImageCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    if Supports(TcxImageInfoItem(Items[I]).Image, IdxVectorImage) then
      Inc(Result);
  end;
end;

function TcxImageInfoCollection.GetOwner: TPersistent;
begin
  Result := FImageList;
end;

procedure TcxImageInfoCollection.SetCompressData(Value: Boolean);
var
  I: Integer;
begin
  if CompressData <> Value then
  begin
    FCompressData := Value;
    for I := 0 to Count - 1 do
      TcxImageInfoItem(Items[I]).CompressData := Value;
  end;
end;

{ TcxImageInfo }

constructor TcxImageInfo.Create;
begin
  inherited Create;
  FImage := TcxBitmap.Create;
  FMask := TcxBitmap.Create;
  FMaskColor := clNone;
  FInternalMask := TcxBitmap.Create;
  FIsAlphaUsed := bDefault;
end;

destructor TcxImageInfo.Destroy;
begin
  FreeAndNil(FInternalMask);
  FreeAndNil(FMask);
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TcxImageInfo.Assign(Source: TPersistent);
begin
  if Source is TcxImageInfo then
  begin
    Image := TcxImageInfo(Source).Image;
    Mask := TcxImageInfo(Source).Mask;
    MaskColor := TcxImageInfo(Source).MaskColor;
  end
  else
    inherited;
end;

procedure TcxImageInfo.ConvertTo32Bit;
var
  ABuffer: TcxAlphaBitmap;
  ARegion: TcxRegionHandle;
begin
  if HasMask then
  begin
    ABuffer := TcxAlphaBitmap.CreateSize(Image.Width, Image.Height);
    try
      TcxImageInfoHelper.CopyRect(ABuffer, ABuffer.ClientRect, Image);
      if cxColorIsValid(MaskColor) then
        ABuffer.RecoverTransparency(MaskColor)
      else
      begin
        if not IsAlphaUsed then
          ABuffer.TransformBitmap(btmSetOpaque);
        if IsGlyphAssigned(Mask) then
        begin
          ARegion := cxCreateRegionFromBitmap(Mask, clBlack);
          if ARegion <> 0 then
          try
            SelectClipRgn(ABuffer.Canvas.Handle, ARegion);
            cxClearBitmap(ABuffer);
          finally
            DeleteObject(ARegion);
          end;
        end
      end;
      ImageClass := TBitmap;
      Image.Assign(ABuffer);
    finally
      ABuffer.Free;
    end;
    FIsAlphaUsed := bDefault;
  end;
  FlushMask;
end;

procedure TcxImageInfo.Dormant;
begin
  if not Mask.Empty then
    Mask.Dormant;
  if not Image.Empty then
    TcxImageInfoHelper.Dormant(Image);
end;

procedure TcxImageInfo.FlushMask;
begin
  Mask := nil;
  MaskColor := clNone;
end;

procedure TcxImageInfo.Resize(const ASize: TSize);
begin
  ConvertTo32Bit;
  TcxImageInfoHelper.Resize(Image, ASize);
end;

procedure TcxImageInfo.AssignBitmap(ASourceBitmap, ADestBitmap: TBitmap);
begin
  ADestBitmap.Assign(ASourceBitmap);
  ADestBitmap.Handle; // HandleNeeded
end;

function TcxImageInfo.HasMask: Boolean;
begin
  Result := cxColorIsValid(MaskColor) or not Mask.Empty;
end;

function TcxImageInfo.HasNativeHandle: Boolean;
begin
  Result := FImage is TBitmap;
end;

function TcxImageInfo.GetImageClass: TGraphicClass;
begin
  if Image <> nil then
    Result := TGraphicClass(Image.ClassType)
  else
    Result := nil;
end;

function TcxImageInfo.GetImageType: string;
var
  ACodec: TdxSmartImageCodecClass;
begin
  Result := 'IMG';
  if Image is TBitmap then
    Result := 'BMP'
  else
    if Image is TdxCustomSmartImage then
    begin
      ACodec := TdxCustomSmartImage(Image).ImageCodec;
      if ACodec <> nil then
        Result := UpperCase(Copy(ACodec.Ext, 2, MaxInt))
    end;
end;

function TcxImageInfo.GetIsAlphaUsed: Boolean;
begin
  if FIsAlphaUsed = bDefault then
    FIsAlphaUsed := dxBooleanToDefaultBoolean(TcxImageInfoHelper.IsAlphaUsed(Image));
  Result := FIsAlphaUsed = bTrue;
end;

procedure TcxImageInfo.SetImage(Value: TGraphic);
begin
  if Value <> nil then
    ImageClass := TGraphicClass(Value.ClassType);
  FImage.Assign(Value);
  FIsAlphaUsed := bDefault;
end;

procedure TcxImageInfo.SetImageClass(AValue: TGraphicClass);
begin
  if (AValue <> nil) and (AValue <> ImageClass) then
  begin
    FreeAndNil(FImage);
    FImage := AValue.Create;
    FIsAlphaUsed := bDefault;
  end;
end;

procedure TcxImageInfo.SetMask(Value: TBitmap);
begin
  AssignBitmap(Value, Mask);
end;

{ TcxImageInfoHelper }

class procedure TcxImageInfoHelper.CopyRect(ADest: TBitmap; const ADestRect: TRect; ASource: TGraphic);
begin
  CopyRect(ADest, ADestRect, ASource, cxGetImageClientRect(ASource));
end;

class procedure TcxImageInfoHelper.CopyRect(ADest: TBitmap;
  const ADestRect: TRect; ASource: TGraphic; const ASourceRect: TRect);
begin
  if ASource is TBitmap then
    ADest.Canvas.CopyRect(ADestRect, TBitmap(ASource).Canvas, ASourceRect)
  else
    if ASource is TdxCustomSmartImage then
      TdxCustomSmartImage(ASource).StretchDraw(ADest.Canvas.Handle, ADestRect, ASourceRect);
end;

class procedure TcxImageInfoHelper.Dormant(AGraphic: TGraphic);
begin
  if AGraphic is TBitmap then
    TBitmap(AGraphic).Dormant
  else
    if AGraphic is TdxCustomSmartImage then
      TdxCustomSmartImage(AGraphic).Dormant;
end;

class function TcxImageInfoHelper.GetDefaultTransparentColor(AImage: TGraphic; AMask: TBitmap): TColor;
begin
  if IsGlyphAssigned(AMask) or not (AImage is TBitmap) or IsAlphaUsed(AImage) then
    Result := clNone
  else
    Result := TBitmap(AImage).Canvas.Pixels[0, AImage.Height - 1];
end;

class function TcxImageInfoHelper.GetPixel(AGraphic: TGraphic; X, Y: Integer): TColor;
begin
  if AGraphic is TBitmap then
    Result := TBitmap(AGraphic).Canvas.Pixels[X, Y]
  else
    Result := clNone;
end;

class function TcxImageInfoHelper.GetPixelFormat(AGraphic: TGraphic): TPixelFormat;
begin
  if AGraphic is TBitmap then
    Result := TBitmap(AGraphic).PixelFormat
  else
    if AGraphic is TMetafile then
      Result := pf24bit
    else
      Result := pf32bit; // todo
end;

class function TcxImageInfoHelper.IsAlphaUsed(AGraphic: TGraphic): Boolean;
begin
  if AGraphic is TBitmap then
    Result := dxIsAlphaUsed(TBitmap(AGraphic))
  else if AGraphic is TdxCustomSmartImage then
    Result := TdxCustomSmartImage(AGraphic).IsAlphaUsed
  else
    Result := False;
end;

class procedure TcxImageInfoHelper.Resize(AGraphic: TGraphic; const ASize: TSize);
var
  ABitmap: TcxBitmap32;
begin
  if AGraphic is TdxCustomSmartImage then
    TdxCustomSmartImage(AGraphic).Resize(ASize)
  else
    if AGraphic is TBitmap then
    begin
      ABitmap := TcxBitmap32.CreateSize(ASize.cx, ASize.cy, True);
      try
        cxStretchGraphic(ABitmap, AGraphic, True);
        AGraphic.Assign(ABitmap);
      finally
        ABitmap.Free;
      end;
    end
    else
      raise Exception.Create('');
end;

{ TcxBaseImageList }

procedure TcxBaseImageList.AfterConstruction;
begin
  inherited AfterConstruction;
  FSourceDPI := dxDefaultDPI;
end;

procedure TcxBaseImageList.BeforeDestruction;
begin
  inherited BeforeDestruction;
  cxBroadcastRemoveNotifications(Self);
end;

function TcxBaseImageList.GetSourceDPI: Integer;
begin
  Result := FSourceDPI;
end;

procedure TcxBaseImageList.SetSourceDPI(AValue: Integer);
begin
  AValue := dxCheckDPIValue(AValue);
  if FSourceDPI <> AValue then
  begin
    FSourceDPI := AValue;
    Change;
  end;
end;

{ TcxCustomImageList }

destructor TcxCustomImageList.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

procedure TcxCustomImageList.Assign(Source: TPersistent);
var
  AImages: TCustomImageList;
begin
  if Source is TCustomImageList then
  begin
    BeginUpdate;
    try
      inherited;
      Clear;
      AImages := TCustomImageList(Source);
      if AImages is TcxCustomImageList then
        InternalCopyImageInfos(TcxCustomImageList(AImages), 0, AImages.Count - 1)
      else
        InternalCopyImages(AImages, 0, AImages.Count - 1);
    finally
      EndUpdate;
    end;
  end;
end;

function TcxCustomImageList.Add(AImage: TBitmap; AMask: TBitmap): Integer;
begin
  Result := Add(TGraphic(AImage), AMask);
end;

function TcxCustomImageList.Add(AImage: TdxSmartImage): Integer;
begin
  Result := Add(TGraphic(AImage), nil);
end;

function TcxCustomImageList.Add(AImage: TGraphic; AMask: TBitmap): Integer;

  function AddToImageList(AImage, AMask: TBitmap): Integer;
  var
    AImageHandle: HBITMAP;
    AMaskBits: TBytes;
    AMaskHandle: HBITMAP;
  begin
    if AImage <> nil then
      AImageHandle := AImage.Handle
    else
      AImageHandle := 0;

    if AMask = nil then
    begin
      SetLength(AMaskBits, AImage.Width * AImage.Height);
      AMaskHandle := CreateBitmap(Width, Height, 1, 1, AMaskBits);
    end
    else
      AMaskHandle := AMask.Handle;

    Result := ImageList_Add(Handle, AImageHandle, AMaskHandle);
    if AMask = nil then
      DeleteObject(AMaskHandle);
  end;


  function ConvertToBitmap(AImage: TGraphic): TBitmap;
  begin
    if Supports(AImage, IdxVectorImage) then
    begin
      Result := TcxBitmap32.CreateSize(Width, Height, True);
      Result.Canvas.StretchDraw(cxGetImageClientRect(Result), AImage);
    end
    else
      Result := cxGetAsBitmap(AImage);
  end;

var
  ABitmap: TBitmap;
begin
  if (AImage = nil) or (AImage is TBitmap) then
    Result := AddToImageList(TBitmap(AImage), AMask)
  else
  begin
    ABitmap := ConvertToBitmap(AImage);
    try
      Result := AddToImageList(ABitmap, AMask);
    finally
      ABitmap.Free;
    end;
  end;

  if not IsSynchronizationLocked and (Result <> -1) then
    AddToInternalCollection(AImage, AMask);
  Change;
end;

function TcxCustomImageList.AddIcon(AIcon: TIcon): Integer;
var
  AImage, AMask: TBitmap;
begin
  BeginUpdate;
  try
    Result := inherited AddIcon(AIcon);
    if not IsSynchronizationLocked and (Result <> -1) then
    begin
      AImage := cxCreateBitmap(Width, Height, pf32bit);
      AMask := cxCreateBitmap(Width, Height, pf1bit);
      try
        GetImageInfo(Handle, Count - 1, AImage, AMask);
        AddToInternalCollection(AImage, AMask);
      finally
        AMask.Free;
        AImage.Free
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function TcxCustomImageList.AddMasked(AImage: TGraphic; AMaskColor: TColor): Integer;
var
  ACloneImage: TBitmap;
begin
  if AMaskColor = clNone then
    Result := Add(AImage, nil)
  else
  begin
    BeginUpdate;
    try
      ACloneImage := cxGetAsBitmap(AImage);
      try
        Result := ImageList_AddMasked(Handle, ACloneImage.Handle, ColorToRGB(AMaskColor));
        if Result >= Count then
          Result := -1;
      finally
        ACloneImage.Free;
      end;
      if not IsSynchronizationLocked and (Result <> -1) then
        AddToInternalCollection(AImage, nil, AMaskColor);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcxCustomImageList.Move(ACurIndex, ANewIndex: Integer);
var
  AStep, AIndex: Integer;
begin
  BeginUpdate;
  try
    AStep := cxSign(ANewIndex - ACurIndex);
    AIndex := ACurIndex;
    while AIndex <> ANewIndex do
    begin
      ImageList_Copy(Handle, AIndex + AStep, Handle, AIndex, ILCF_SWAP);
      Inc(AIndex, AStep);
    end;
    if not IsSynchronizationLocked then
      TcxImageInfoCollection(FImages).Move(ACurIndex, ANewIndex);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomImageList.Delete(AIndex: Integer);
begin
  BeginUpdate;
  try
    inherited;
    if not IsSynchronizationLocked then
      TcxImageInfoCollection(FImages).Delete(AIndex);
  finally
    EndUpdate;
  end;
end;

function TcxCustomImageList.AddBitmap(AImage, AMask: TBitmap; AMaskColor: TColor; AStretch, ASmooth: Boolean): Integer;
var
  ASizedImage, ASizedMask: TBitmap;
begin
  if (AImage.Width <> Width) or (AImage.Height <> Height) then
  begin
    ASmooth := ASmooth and AStretch and (AImage.PixelFormat = pf32bit);
    ASizedImage := TcxBitmap32.CreateSize(AImage.Width, AImage.Height);
    TcxBitmap32Access(ASizedImage).CopyBitmap(AImage);
    TcxBitmap32Access(ASizedImage).Resize(Width, Height, AStretch, ASmooth, AMaskColor);
  end
  else
    ASizedImage := AImage;

  if (AMask <> nil) and ((AMask.Width <> Width) or (AMask.Height <> Height)) then
  begin
    ASizedMask := TcxBitmap32.CreateSize(AMask.Width, AMask.Height);
    TcxBitmap32Access(ASizedMask).CopyBitmap(AMask);
    TcxBitmap32Access(ASizedMask).Resize(Width, Height, AStretch, False);
  end
  else
    ASizedMask := AMask;

  try
    if ASizedMask <> nil then
      Result := Add(ASizedImage, ASizedMask)
    else
      Result := AddMasked(ASizedImage, AMaskColor);
  finally
    if ASizedMask <> AMask then
     ASizedMask.Free;
    if ASizedImage <> AImage then
     ASizedImage.Free;
  end;
end;

function TcxCustomImageList.AddImage(AImages: TCustomImageList; AIndex: Integer): Integer;
begin
  if (AImages <> nil) and (AIndex < AImages.Count) then
  begin
    Result := Count;
    CopyImages(AImages, AIndex, AIndex);
  end
  else
    Result := -1;
end;

procedure TcxCustomImageList.AddImages(AImages: TCustomImageList);
begin
  if AImages <> nil then
    CopyImages(AImages);
end;

procedure TcxCustomImageList.CopyImages(AImages: TCustomImageList; AStartIndex, AEndIndex: Integer);

  procedure InternalCopy(AcxImages: TcxCustomImageList);
  begin
    if AEndIndex < 0 then
      AEndIndex := AcxImages.Count - 1
    else
      AEndIndex := Min(AcxImages.Count - 1, AEndIndex);
    InternalCopyImageInfos(AcxImages, AStartIndex, AEndIndex);
  end;

var
  AcxImages: TcxCustomImageList;
begin
  BeginUpdate;
  try
    if AImages is TcxCustomImageList then
      InternalCopy(TcxCustomImageList(AImages))
    else
    begin
      AcxImages := TcxCustomImageList.Create(nil);
      try
        AcxImages.Assign(AImages);
        InternalCopy(AcxImages);
      finally
        AcxImages.Free;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomImageList.Clear;
begin
  Delete(-1);
end;

procedure TcxCustomImageList.Insert(AIndex: Integer; AImage: TGraphic; AMask: TBitmap);
var
  I, ACurIndex: Integer;
begin
  if InRange(AIndex, 0, Count) then
  begin
    BeginUpdate;
    try
      ACurIndex := Add(AImage, AMask);
      for I := 0 to GetImageCount(AImage, Width, Height) - 1 do
        Move(ACurIndex + I, AIndex + I);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcxCustomImageList.InsertIcon(AIndex: Integer; AIcon: TIcon);
begin
  if InRange(AIndex, 0, Count) then
    Move(AddIcon(AIcon), AIndex);
end;

procedure TcxCustomImageList.InsertMasked(AIndex: Integer; AImage: TGraphic; AMaskColor: TColor);
var
  I, ACurIndex: Integer;
begin
  if InRange(AIndex, 0, Count) then
  begin
    BeginUpdate;
    try
      ACurIndex := AddMasked(AImage, AMaskColor);
      for I := 0 to GetImageCount(AImage, Width, Height) - 1 do
        Move(ACurIndex + I, AIndex + I);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcxCustomImageList.Replace(AIndex: Integer; AImage: TGraphic; AMask: TBitmap);
begin
  BeginUpdate;
  try
    Delete(AIndex);
    Insert(AIndex, AImage, AMask);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomImageList.ReplaceIcon(AIndex: Integer; AIcon: TIcon);
begin
  BeginUpdate;
  try
    Delete(AIndex);
    InsertIcon(AIndex, AIcon);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomImageList.ReplaceMasked(AIndex: Integer; AImage: TGraphic; AMaskColor: TColor);
begin
  BeginUpdate;
  try
    Delete(AIndex);
    InsertMasked(AIndex, AImage, AMaskColor);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomImageList.DrawOverlay(Canvas: TCanvas; X, Y: Integer;
  ImageIndex: Integer; Overlay: TOverlay; Enabled: Boolean = True);
begin
  DrawOverlay(Canvas, X, Y, ImageIndex, Overlay, dsNormal, itImage, Enabled);
end;

procedure TcxCustomImageList.DrawOverlay(Canvas: TCanvas; X, Y: Integer;
  ImageIndex: Integer; Overlay: TOverlay; ADrawingStyle: TDrawingStyle;
  AImageType: TImageType; Enabled: Boolean = True);
var
  Index: Cardinal;
begin
  if HandleAllocated then
  begin
    Index := IndexToOverlayMask(Overlay + 1);
    inherited DoDraw(ImageIndex, Canvas, X, Y, cxGetImageListStyle(ADrawingStyle, AImageType) or ILD_OVERLAYMASK and Index, Enabled);
  end;
end;

function TcxCustomImageList.LoadImage(AInstance: THandle; const AResourceName: string;
  AMaskColor: TColor = clDefault; AWidth: Integer = 0; AFlags: TLoadResources = []): Boolean;
const
  FlagMap: array [TLoadResource] of DWORD = (
    LR_DEFAULTCOLOR, LR_DEFAULTSIZE, LR_LOADFROMFILE, LR_LOADMAP3DCOLORS, LR_LOADTRANSPARENT, LR_MONOCHROME
  );
var
  I: TLoadResource;
  ALoadFlags: DWORD;
  AHandle: HImageList;
  ARGBColor: DWORD;
  AImageList: TImageList;
begin
  if AMaskColor = clNone then
    ARGBColor := CLR_NONE
  else if AMaskColor = clDefault then
    ARGBColor := CLR_DEFAULT
  else
    ARGBColor := ColorToRGB(AMaskColor);

  ALoadFlags := LR_CREATEDIBSECTION;
  for I := Low(TLoadResource) to High(TLoadResource) do
  begin
    if I in AFlags then
      ALoadFlags := ALoadFlags or FlagMap[I];
  end;

  AHandle := ImageList_LoadImage(AInstance, PChar(AResourceName), AWidth, AllocBy, ARGBColor, IMAGE_BITMAP, ALoadFlags);
  Result := AHandle <> 0;
  if Result then
  begin
    AImageList := TImageList.Create(Self);
    try
      AImageList.Handle := AHandle;
      CopyImages(AImageList);
    finally
      AImageList.Free;
    end;
  end;
end;

function TcxCustomImageList.FileLoad(AResType: TResType; const AName: string; AMaskColor: TColor): Boolean;
begin
  BeginUpdate;
  try
    Result := inherited FileLoad(AResType, AName, AMaskColor);
    SynchronizeImageInfo;
  finally
    EndUpdate;
  end;
end;

function TcxCustomImageList.GetResource(AResType: TResType; const AName: string;
  AWidth: Integer; ALoadFlags: TLoadResources; AMaskColor: TColor): Boolean;
begin
  BeginUpdate;
  try
    Result := inherited GetResource(AResType, AName, AWidth, ALoadFlags, AMaskColor);
    SynchronizeImageInfo;
  finally
    EndUpdate;
  end;
end;

function TcxCustomImageList.GetInstRes(AInstance: THandle; AResType: TResType; const AName: string;
  AWidth: Integer; ALoadFlags: TLoadResources; AMaskColor: TColor): Boolean;
begin
  BeginUpdate;
  try
    Result := inherited GetInstRes(AInstance, AResType, AName, AWidth, ALoadFlags, AMaskColor);
    SynchronizeImageInfo;
  finally
    EndUpdate;
  end;
end;

function TcxCustomImageList.GetInstRes(AInstance: THandle; AResType: TResType; AResID: DWORD;
  AWidth: Integer; ALoadFlags: TLoadResources; AMaskColor: TColor): Boolean;
begin
  BeginUpdate;
  try
    Result := inherited GetInstRes(AInstance, AResType, AResID, AWidth, ALoadFlags, AMaskColor);
    SynchronizeImageInfo;
  finally
    EndUpdate;
  end;
end;

function TcxCustomImageList.ResourceLoad(AResType: TResType; const AName: string; AMaskColor: TColor): Boolean;
begin
  BeginUpdate;
  try
    Result := inherited ResourceLoad(AResType, AName, AMaskColor);
    SynchronizeImageInfo;
  finally
    EndUpdate;
  end;
end;

function TcxCustomImageList.ResInstLoad(AInstance: THandle; AResType: TResType;
  const AName: string; AMaskColor: TColor): Boolean;
begin
  BeginUpdate;
  try
    Result := inherited ResInstLoad(AInstance, AResType, AName, AMaskColor);
    SynchronizeImageInfo;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomImageList.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TcxCustomImageList.CancelUpdate;
begin
  Dec(FLockCount);
end;

procedure TcxCustomImageList.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    Change;
end;

procedure TcxCustomImageList.Draw(ACanvas: TCanvas; const ARect: TRect; AIndex: Integer;
  AStretch: Boolean = True; ASmoothResize: Boolean = False; AEnabled: Boolean = True);
begin
  DoDrawEx(AIndex, ACanvas, ARect, cxGetImageListStyle(DrawingStyle, ImageType), AStretch, ASmoothResize, AEnabled);
end;

procedure TcxCustomImageList.GetImageInfo(AIndex: Integer; AImage, AMask: TBitmap; APalette: IdxColorPalette = nil);
var
  AIsAlphaUsed: TdxDefaultBoolean;
begin
  AIsAlphaUsed := bFalse;
  GetImageInfoCore(AIndex, AImage, AMask, APalette, AIsAlphaUsed);
end;

procedure TcxCustomImageList.GetImage(AIndex: Integer; AImage: TBitmap);
begin
  GetImageInfo(AIndex, AImage, nil);
end;

procedure TcxCustomImageList.GetImage(AIndex: Integer; AImage: TdxSmartImage);
var
  ABitmap: TBitmap;
  AInfo: TcxImageInfo;
begin
  if InRange(AIndex, 0, Count - 1) then
  begin
    AInfo := GetImageInfo(AIndex);
    if TcxImageInfoHelper.GetPixelFormat(AInfo.Image) = pf32bit then
      AImage.Assign(AInfo.Image)
    else
    begin
      ABitmap := TcxBitmap.Create;
      try
        GetImageInfo(AIndex, ABitmap, nil);
        AImage.Assign(ABitmap);
      finally
        ABitmap.Free;
      end;
    end;
  end;
end;

procedure TcxCustomImageList.GetMask(AIndex: Integer; AMask: TBitmap);
begin
  GetImageInfo(AIndex, nil, AMask);
end;

class procedure TcxCustomImageList.GetImageInfo(AImages: TCustomImageList;
  AIndex: Integer; AImage, AMask: TBitmap; APalette: IdxColorPalette = nil);
var
  AIsAlphaUsed: TdxDefaultBoolean;
begin
  AIsAlphaUsed := bFalse;
  GetImageInfoCore(AImages, AIndex, AImage, AMask, APalette, AIsAlphaUsed);
end;

class procedure TcxCustomImageList.GetImageInfo(AHandle: HIMAGELIST; AIndex: Integer; AImage, AMask: TBitmap);

  procedure GetBitmap(ASrcHandle: HBITMAP; ADestBitmap: TBitmap; ACopyAll, ASmoothStretch: Boolean; const ARect: TRect);
  var
    ASrcBitmap: TBitmap;
  begin
    if ACopyAll then
      ADestBitmap.Handle := cxCopyImage(ASrcHandle)
    else
    begin
      ASrcBitmap := TBitmap.Create;
      try
        ASrcBitmap.Handle := cxCopyImage(ASrcHandle);
        CheckImageSize(ADestBitmap, cxRectWidth(ARect), cxRectHeight(ARect));
        cxStretchGraphic(ADestBitmap, ASrcBitmap, cxGetImageClientRect(ADestBitmap), ARect, ASmoothStretch);
        if dxIsAlphaUsed(ADestBitmap) then
          TdxFadingHelper.CorrectAlphaChannel(ADestBitmap);
      finally
        ASrcBitmap.Free;
      end;
    end;
  end;

var
  ACopyAll: Boolean;
  AImageInfo: TImageInfo;
begin
  ACopyAll := AIndex = -1;
  if ACopyAll then
    AIndex := 0;
  if ImageList_GetImageInfo(AHandle, AIndex, AImageInfo) then
  try
    if AImage <> nil then
      GetBitmap(AImageInfo.hbmImage, AImage, ACopyAll, AImageInfo.hbmMask = 0, AImageInfo.rcImage);
    if AMask <> nil then
    begin
      if AImageInfo.hbmMask <> 0 then
        GetBitmap(AImageInfo.hbmMask, AMask, ACopyAll, False, AImageInfo.rcImage)
      else
        cxClearBitmap(AMask);
    end;
  finally
    ReleaseImageInfo(AImageInfo);
  end;
end;

class function TcxCustomImageList.GetPixelFormat(AHandle: HIMAGELIST): Integer;
var
  ABitmapInfo: Windows.TBitmap;
  AImageInfo: TImageInfo;
begin
  Result := 0;
  if ImageList_GetImageInfo(AHandle, 0, AImageInfo) then
  try
    cxGetBitmapData(AImageInfo.hbmImage, ABitmapInfo);
    Result := ABitmapInfo.bmBitsPixel;
  finally
    ReleaseImageInfo(AImageInfo);
  end;
end;

class function TcxCustomImageList.IsEquals(AImages1, AImages2: TCustomImageList): Boolean;
var
  AAdapter1: TStreamAdapter;
  AAdapter2: TStreamAdapter;
  AStream1: TMemoryStream;
  AStream2: TMemoryStream;
begin
  if AImages1.Count <> AImages2.Count then
    Result := False
  else
    if AImages1.Count = 0 then
      Result := True
    else
    begin
      AStream1 := TMemoryStream.Create;
      AStream2 := TMemoryStream.Create;
      AAdapter1 := TStreamAdapter.Create(AStream1);
      AAdapter2 := TStreamAdapter.Create(AStream2);
      try
        ImageList_Write(AImages1.Handle, AAdapter1);
        ImageList_Write(AImages2.Handle, AAdapter2);
        Result := (AStream1.Size = AStream2.Size) and CompareMem(AStream1.Memory, AStream2.Memory, AStream1.Size);
      finally
        AAdapter2.Free;
        AAdapter1.Free;
        AStream2.Free;
        AStream1.Free;
      end;
    end;
end;

procedure TcxCustomImageList.SetSize(AWidth, AHeight: Integer);
begin
  AWidth := Max(AWidth, 1);
  AHeight := Max(AHeight, 1);
  if (AWidth <> Width) or (AHeight <> Height) then
  begin
    BeginUpdate;
    try
      Clear;
      inherited SetSize(AWidth, AHeight);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcxCustomImageList.SetSize(const ASize: TSize);
begin
  SetSize(ASize.cx, ASize.cy);
end;

function TcxCustomImageList.ChangeLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TcxCustomImageList.Change;
begin
  if not ChangeLocked and ([csLoading, csReading, csDestroying] * ComponentState = []) then
  begin
    TdxImageListPaintCache.InvalidateImageList(Self);
    if Count <> FImages.Count then
      SynchronizeImageInfo
    else
      inherited Change;
  end;
end;

procedure TcxCustomImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True);
begin
  DoDrawEx(Index, Canvas, cxRectBounds(X, Y, Width, Height), Style, False, False, Enabled);
end;

procedure TcxCustomImageList.DoDrawEx(AIndex: Integer; ACanvas: TCanvas;
  const ARect: TRect; AStyle: Cardinal; AStretch, ASmoothResize, AEnabled: Boolean);
var
  AGlyphRect: TRect;
  ADrawBitmap: TBitmap;
begin
  if (cxRectWidth(ARect) = Width) and (cxRectHeight(ARect) = Height) then
    AStretch := False;
  if AStretch then
    AGlyphRect := ARect
  else
    AGlyphRect := cxRectCenter(ARect, Width, Height);

  if AlphaBlending then
    cxDrawImage(ACanvas.Handle, AGlyphRect, ARect, nil, Self, AIndex, EnabledImageDrawModeMap[AEnabled], ASmoothResize)
  else
    if AStretch then
    begin
      ADrawBitmap := cxCreateBitmap(Width, Height, pfDevice);
      try
        inherited DoDraw(AIndex, ADrawBitmap.Canvas, 0, 0, AStyle, AEnabled);
        cxDrawImage(ACanvas.Handle, AGlyphRect, ARect, ADrawBitmap, nil, 0, EnabledImageDrawModeMap[AEnabled], ASmoothResize);
      finally
        ADrawBitmap.Free;
      end;
    end
    else
      inherited DoDraw(AIndex, ACanvas, AGlyphRect.Left, AGlyphRect.Top, AStyle, AEnabled);
end;

procedure TcxCustomImageList.Initialize;
begin
  BeginUpdate;
  try
    inherited Initialize;
    FImages := TcxImageInfoCollection.Create(Self);
    FAlphaBlending := True;
    ColorDepth := cd32Bit;
  finally
    CancelUpdate;
  end;
end;

procedure TcxCustomImageList.Finalize;
begin
  FreeAndNil(FImages);
end;

procedure TcxCustomImageList.DefineProperties(Filer: TFiler);

  function NeedWriteImageInfo: Boolean;
  begin
    if (Filer.Ancestor <> nil) and (Filer.Ancestor is TCustomImageList) then
      Result := not IsEquals(TCustomImageList(Filer.Ancestor), Self)
    else
      Result := Count > 0;
  end;

  function NeedWriteDesignInfo: Boolean;
  begin
    Result := (Filer.Ancestor = nil) or not (Filer.Ancestor is TCustomImageList) or
      (TCustomImageList(Filer.Ancestor).DesignInfo <> DesignInfo);
  end;

  function NeedWriteRasterImageList: Boolean;
  begin
    Result := TcxImageInfoCollection(FImages).GetVectorImageCount > MulDiv(Count, 1, 3);
  end;

begin
  Filer.DefineProperty('FormatVersion', ReadFormatVersion, WriteFormatVersion, True);

  if csReading in ComponentState then
  begin
    inherited;
    if FFormatVersion = 0 then
      SynchronizeImageInfo;
  end
  else
    if NeedWriteRasterImageList then
      inherited;

  Filer.DefineProperty('DesignInfo', ReadDesignInfo, WriteDesignInfo, NeedWriteDesignInfo);
  Filer.DefineProperty('ImageInfo', ReadImageInfo, WriteImageInfo, NeedWriteImageInfo);
end;

procedure TcxCustomImageList.Dormant;
var
  I: Integer;
begin
  for I := 0 to FImages.Count - 1 do
    DormantImage(I);
end;

procedure TcxCustomImageList.GetImageInfoCore(AIndex: Integer;
  AImage, AMask: TBitmap; APalette: IdxColorPalette; var AIsAlphaUsed: TdxDefaultBoolean);

  procedure GetBitmap(ADestBitmap: TBitmap; ASourceImage: TGraphic; ASmoothStretch: Boolean);
  begin
    CheckImageSize(ADestBitmap, Width, Height);
    cxStretchGraphic(ADestBitmap, ASourceImage, ASmoothStretch, APalette);
  end;

var
  AInfo: TcxImageInfo;
begin
  if AIndex = -1 then
    GetImageInfo(Handle, AIndex, AImage, AMask)
  else
    if InRange(AIndex, 0, Count - 1) then
    begin
      AInfo := GetImageInfo(AIndex);
      if (TcxImageInfoHelper.GetPixelFormat(AInfo.Image) = pf32bit) or IsWin9X then
      begin
        if AImage <> nil then
        begin
          GetBitmap(AImage, AInfo.Image, AInfo.Mask.Empty);
          if AIsAlphaUsed = bDefault then
            AIsAlphaUsed := dxBooleanToDefaultBoolean(AInfo.IsAlphaUsed);
        end;
        if AMask <> nil then
        begin
          if AInfo.Mask.Empty then
          begin
            if AInfo.FInternalMask.Empty then
            begin
              AInfo.FInternalMask.SetSize(Width, Height);
              GetImageInfo(Handle, AIndex, nil, AInfo.FInternalMask);
            end;
            GetBitmap(AMask, AInfo.FInternalMask, False);
          end
          else
            GetBitmap(AMask, AInfo.Mask, False);
        end;
      end
      else
        GetImageInfo(Handle, AIndex, AImage, AMask);

      if AInfo.HasNativeHandle then
        AInfo.Dormant;
    end;
end;

class procedure TcxCustomImageList.GetImageInfoCore(
  AImages: TCustomImageList; AIndex: Integer; AImage, AMask: TBitmap;
  APalette: IdxColorPalette; var AIsAlphaUsed: TdxDefaultBoolean);
begin
  if AImages is TcxCustomImageList then
    TcxCustomImageList(AImages).GetImageInfoCore(AIndex, AImage, AMask, APalette, AIsAlphaUsed)
  else
    TcxCustomImageList.GetImageInfo(AImages.Handle, AIndex, AImage, AMask);
end;

function TcxCustomImageList.AddImageInfo(AImageInfo: TcxImageInfo): Integer;
begin
  if IsGlyphAssigned(AImageInfo.Mask) then
    Result := Add(AImageInfo.Image, AImageInfo.Mask)
  else
    Result := AddMasked(AImageInfo.Image, AImageInfo.MaskColor);
end;

function TcxCustomImageList.CanSplitImage(AImage: TGraphic): Boolean;
begin
  Result := ((AImage.Width <> Width) or (AImage.Height <> Height)) and
    (AImage.Width mod Width + AImage.Height mod Height = 0) and not Supports(AImage, IdxVectorImage);
end;

procedure TcxCustomImageList.ConvertTo32bit;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      GetImageInfo(I).ConvertTo32Bit;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomImageList.InternalCopyImageInfos(AImageList: TcxCustomImageList; AStartIndex, AEndIndex: Integer);
var
  I: Integer;
begin
  for I := Max(AStartIndex, 0) to AEndIndex do
    AddImageInfo(TcxImageInfoItem(AImageList.FImages.Items[I]).ImageInfo);
end;

procedure TcxCustomImageList.InternalCopyImages(AImageList: TCustomImageList; AStartIndex, AEndIndex: Integer);
var
  I: Integer;
  AImage, AMask: TBitmap;
begin
  AImage := cxCreateBitmap(Width, Height, pf32bit);
  AMask := cxCreateBitmap(Width, Height, pf1bit);
  try
    for I := Max(AStartIndex, 0) to AEndIndex do
    begin
      GetImageInfo(AImageList.Handle, I, AImage, AMask);
      Add(AImage, AMask);
    end;
  finally
    AImage.Free;
    AMask.Free;
  end;
end;

function TcxCustomImageList.GetImageInfo(AIndex: Integer): TcxImageInfo;
begin
  Result := TcxImageInfoItem(FImages.Items[AIndex]).ImageInfo;
end;

function TcxCustomImageList.HasRasterImages: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FImages.Count - 1 do
  begin
    if not Supports(GetImageInfo(I).Image, IdxVectorImage) then
      Exit(True);
  end;
end;

procedure TcxCustomImageList.Resize(const ASize: TSize);
var
  I: Integer;
begin
  BeginUpdate;
  try
    Inc(FSynchronizationLockCount);
    try
      for I := 0 to FImages.Count - 1 do
        GetImageInfo(I).Resize(ASize);
      inherited SetSize(ASize.cx, ASize.cy);
      SynchronizeHandle;
    finally
      Dec(FSynchronizationLockCount);
    end;
  finally
    EndUpdate;
  end;
end;

function TcxCustomImageList.GetCompressData: Boolean;
begin
  Result := TcxImageInfoCollection(FImages).CompressData;
end;

procedure TcxCustomImageList.SetCompressData(Value: Boolean);
begin
  TcxImageInfoCollection(FImages).CompressData := Value;
end;

function TcxCustomImageList.GetHandle: HImageList;
begin
  Result := inherited Handle;
end;

procedure TcxCustomImageList.SetHandle(Value: HImageList);
var
  AImageList: TCustomImageList;
begin
  AImageList := TCustomImageList.Create(Self);
  try
    AImageList.Handle := Value;
    Assign(AImageList);
    ImageList_Destroy(Value);
  finally
    AImageList.Free;
  end;
end;

procedure TcxCustomImageList.ReadFormatVersion(AReader: TReader);
begin
  FFormatVersion := AReader.ReadInteger;
end;

procedure TcxCustomImageList.WriteFormatVersion(AWriter: TWriter);
begin
  FFormatVersion := DXILVersion;
  AWriter.WriteInteger(FFormatVersion);
end;

procedure TcxCustomImageList.ReadImageInfo(AReader: TReader);
begin
  FImages.Clear;
  AReader.ReadValue;
  AReader.ReadCollection(FImages);
  if FImages.Count <> Count then
    SynchronizeHandle;
end;

procedure TcxCustomImageList.WriteImageInfo(AWriter: TWriter);
begin
  AWriter.WriteCollection(FImages);
end;

procedure TcxCustomImageList.ReadDesignInfo(AReader: TReader);
begin
  DesignInfo := AReader.ReadInteger;
end;

procedure TcxCustomImageList.WriteDesignInfo(AWriter: TWriter);
begin
  AWriter.WriteInteger(DesignInfo);
end;

function TcxCustomImageList.IsSynchronizationLocked: Boolean;
begin
  Result := FSynchronizationLockCount > 0;
end;

procedure TcxCustomImageList.SynchronizeImageInfo;
var
  I: Integer;
  AImage, AMask: TBitmap;
begin
  FImages.BeginUpdate;
  try
    FImages.Clear;
    if Count > 0 then
    begin
      AImage := cxCreateBitmap(Width, Height, pf32bit);
      AMask := cxCreateBitmap(Width, Height, pf1bit);
      try
        for I := 0 to Count - 1 do
        begin
          GetImageInfo(Handle, I, AImage, AMask);
          TcxImageInfoCollection(FImages).Add(AImage, AMask);
          DormantImage(I);
        end;
      finally
        AMask.Free;
        AImage.Free;
      end;
    end;
  finally
    FImages.EndUpdate;
  end;
end;

procedure TcxCustomImageList.SynchronizeHandle;
var
  I: Integer;
  AImageInfoItem: TcxImageInfoItem;
begin
  BeginUpdate;
  try
    Inc(FSynchronizationLockCount);
    try
      Clear;
      I := 0;
      while I < FImages.Count do
      begin
        AImageInfoItem := TcxImageInfoItem(FImages.Items[I]);
        if AddImageInfo(AImageInfoItem.ImageInfo) <> -1 then
        begin
          DormantImage(I);
          Inc(I);
        end
        else
          FImages.Delete(I);
      end;
    finally
      Dec(FSynchronizationLockCount);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomImageList.AddToInternalCollection(AImage: TGraphic; AMask: TBitmap; AMaskColor: TColor);

  procedure InternalAddToInternalCollection(AImage: TGraphic; AMask: TBitmap; AMaskColor: TColor);
  begin
    DormantImage(TcxImageInfoCollection(FImages).Add(AImage, AMask, AMaskColor).Index);
  end;

var
  AColCount: Integer;
  AColIndex: Integer;
  ADestBitmap: TcxBitmap;
  ADestMask: TcxBitmap;
  ARowCount: Integer;
  ARowIndex: Integer;
  ASourceRect: TRect;
begin
  if CanSplitImage(AImage) then
  begin
    AColCount := AImage.Width div Width;
    ARowCount := AImage.Height div Height;

    ADestBitmap := TcxBitmap.CreateSize(Width, Height, TcxImageInfoHelper.GetPixelFormat(AImage));
    if IsGlyphAssigned(AMask) then
      ADestMask := TcxBitmap.CreateSize(Width, Height, AMask.PixelFormat)
    else
      ADestMask := nil;
    try
      for ARowIndex := 0 to ARowCount - 1 do
        for AColIndex := 0 to AColCount - 1 do
        begin
          cxClearBitmap(ADestBitmap);
          ASourceRect := Rect(AColIndex * Width, ARowIndex * Height, (AColIndex + 1) * Width, (ARowIndex + 1) * Height);
          TcxImageInfoHelper.CopyRect(ADestBitmap, ADestBitmap.ClientRect, AImage, ASourceRect);
          if IsGlyphAssigned(AMask) then
            ADestMask.Canvas.CopyRect(ADestMask.ClientRect, AMask.Canvas, ASourceRect);
          InternalAddToInternalCollection(ADestBitmap, ADestMask, AMaskColor);
        end;
    finally
      ADestMask.Free;
      ADestBitmap.Free;
    end;
  end
  else
    InternalAddToInternalCollection(AImage, AMask, AMaskColor);
end;

class procedure TcxCustomImageList.CheckImageSize(AImage: TBitmap; AWidth, AHeight: Integer);
begin
  if (AImage.Width = 0) or (AImage.Height = 0) then
    AImage.SetSize(AWidth, AHeight);
end;

class procedure TcxCustomImageList.ReleaseImageInfo(var AImageInfo: TImageInfo);
begin
  DeleteObject(AImageInfo.hbmImage);
  DeleteObject(AImageInfo.hbmMask);
end;

procedure TcxCustomImageList.DormantImage(AIndex: Integer);
begin
  GetImageInfo(AIndex).Dormant;
end;

function TcxCustomImageList.GetImageCount(AImage: TGraphic; AWidth, AHeight: Integer): Integer;
begin
  if CanSplitImage(AImage) then
    Result := (AImage.Width div AWidth) * (AImage.Height div AHeight)
  else
    Result := 1;
end;

function TcxCustomImageList.GetHeight: Integer;
begin
  Result := inherited Height;
end;

procedure TcxCustomImageList.SetHeight(AValue: Integer);
begin
  SetSize(Width, AValue);
end;

function TcxCustomImageList.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TcxCustomImageList.SetWidth(AValue: Integer);
begin
  SetSize(AValue, Height);
end;

end.
