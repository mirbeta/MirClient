{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPDFViewer                                         }
{                                                                    }
{           Copyright (c) 2015-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL              }
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

unit dxPDFColorSpace;

{$I cxVer.inc}

interface

uses
  SysUtils, Types, Generics.Defaults, Generics.Collections, Math, dxPDFCore, dxPDFFunction, dxPDFBase, dxPDFTypes;

type
  { TdxPDFGrayColorSpaceTransformation }

  TdxPDFGrayColorSpaceTransformation = class(TdxPDFCustomColorSpaceTransformation)
  protected
    function GetComponentCount: Integer; override;
  public
    function Transform(const AData: TBytes): TdxPDFColorSpaceTransformResult; override;
  end;

  { TdxPDFRGBColorSpaceTransformation }

  TdxPDFRGBColorSpaceTransformation = class(TdxPDFCustomColorSpaceTransformation)
  protected
    function GetComponentCount: Integer; override;
  public
    function Transform(const AData: TBytes): TdxPDFColorSpaceTransformResult; override;
  end;

  { TdxPDFCMYKColorSpaceTransformation }

  TdxPDFCMYKColorSpaceTransformation = class(TdxPDFCustomColorSpaceTransformation)
  strict private
    function CalculateCMYKTransformationResult(const AData: TBytes; ALength: Integer): TdxPDFColorSpaceTransformResult;
  protected
    function GetComponentCount: Integer; override;
  public
    function Transform(const AData: TBytes): TdxPDFColorSpaceTransformResult; override;
  end;

  { TdxPDFIndexedColorSpaceCustomTransformation }

  TdxPDFIndexedColorSpaceCustomTransformation = class(TdxPDFCustomColorSpaceTransformation)
  strict private
    FComponentCount: Integer;
    FLookupTable: TBytes;
    FMaskData: TBytes;
    FMaxIndex: Integer;
    FNeedPopulateMaskData: Boolean;
    FTransparentRange: TdxPDFRange;
  protected
    function GetComponentCount: Integer; override;

    function TransformData(const AData: TBytes): TBytes; virtual;

    procedure PopulateData(var AData: TBytes; AValue: Byte; var ADestinationIndex, APosition: Integer);
    procedure PopulateMaskData(AValue: Byte; AIndex: Integer);

    property LookupTable: TBytes read FLookupTable;
    property MaxIndex: Integer read FMaxIndex;
    property TransparentRange: TdxPDFRange read FTransparentRange;
  public
    constructor Create(AColorSpace: TdxPDFReferencedObject; const AInfo: TdxPDFImageInfo);
    function Transform(const AData: TBytes): TdxPDFColorSpaceTransformResult; override;

    property MaskData: TBytes read FMaskData;
  end;

  { TdxPDFIndexedColorSpaceOneTwoBitTransformation }

  TdxPDFIndexedColorSpaceOneTwoBitTransformation = class(TdxPDFIndexedColorSpaceCustomTransformation)
  protected
    function TransformData(const AData: TBytes): TBytes; override;
  end;

  { TdxPDFIndexedColorSpaceEightBitTransformation }

  TdxPDFIndexedColorSpaceEightBitTransformation = class(TdxPDFIndexedColorSpaceCustomTransformation)
  protected
    function TransformData(const AData: TBytes): TBytes; override;
  end;

  { TdxPDFIndexedColorSpaceFourBitTransformation }

  TdxPDFIndexedColorSpaceFourBitTransformation = class(TdxPDFIndexedColorSpaceCustomTransformation)
  protected
    function TransformData(const AData: TBytes): TBytes; override;
  end;

  { TdxPDFICCBasedColorSpace }

  TdxPDFICCBasedColorSpace = class(TdxPDFCustomColorSpace)
  strict private
    FRanges: TdxPDFRanges;

    function CreateDefaultColorSpace: TdxPDFCustomColorSpace;
    function GetDictionary(AObject: TdxPDFBase): TdxPDFReaderDictionary;
    procedure InitializeRanges;
    procedure DoReadRanges(AArray: TdxPDFArray);
    procedure ReadAlternativeColorSpace(ADictionary: TdxPDFReaderDictionary);
    procedure ReadComponentCount(ADictionary: TdxPDFDictionary);
    procedure ReadRanges(ADictionary: TdxPDFDictionary);
  protected
    class function GetTypeName: string; override;
    function CreateDefaultDecodeArray(ABitsPerComponent: Integer): TdxPDFRanges; override;
    procedure DoRead(AArray: TdxPDFArray); override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); overload; override;
  public
    function Transform(const AComponents: TDoubleDynArray): TDoubleDynArray; override;
    function Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult; override;
  end;

  { TdxPDFCustomDeviceColorSpace }

  TdxPDFCustomDeviceColorSpace = class(TdxPDFCustomColorSpace)
  public
    function Transform(const AComponents: TDoubleDynArray): TDoubleDynArray; override;
    function Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult; override;
  end;

  { TdxPDFRGBDeviceColorSpace }

  TdxPDFRGBDeviceColorSpace = class(TdxPDFCustomDeviceColorSpace)
  protected
    class function GetTypeName: string; override;
    function GetComponentCount: Integer; override;
    function GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass; override;
  end;

  { TdxPDFGrayDeviceColorSpace }

  TdxPDFGrayDeviceColorSpace = class(TdxPDFCustomDeviceColorSpace)
  protected
    class function GetTypeName: string; override;
    function GetComponentCount: Integer; override;
    function GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass; override;
  end;

  { TdxPDFCMYKDeviceColorSpace }

  TdxPDFCMYKDeviceColorSpace = class(TdxPDFCustomDeviceColorSpace)
  protected
    class function GetTypeName: string; override;
    function GetComponentCount: Integer; override;
    function GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass; override;
  end;

  { TdxPDFIndexedColorSpace }

  TdxPDFIndexedColorSpace = class(TdxPDFCustomColorSpace)
  strict private
    FLookupTable: TBytes;
    FMaxIndex: Integer;

    function CreateTransformation(const AInfo: TdxPDFImageInfo): TdxPDFIndexedColorSpaceCustomTransformation;
    procedure ReadLookupTable(AObject: TdxPDFBase);
    procedure ReadMaxIndex(AObject: TdxPDFBase);
  protected
    class function GetTypeName: string; override;
    function CanRead(ASize: Integer): Boolean; override;
    function CreateDefaultDecodeArray(ABitsPerComponent: Integer): TdxPDFRanges; override;
    function GetComponentCount: Integer; override;
    function GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass; override;
    procedure DoRead(AArray: TdxPDFArray); override;
    procedure Initialize; override;
  public
    function Transform(const AComponents: TDoubleDynArray): TDoubleDynArray; override;
    function Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult; override;

    property LookupTable: TBytes read FLookupTable;
    property MaxIndex: Integer read FMaxIndex;
  end;

  { TdxPDFSpecialColorSpace }

  TdxPDFSpecialColorSpace = class(TdxPDFCustomColorSpace)
  strict private
    FTintTransform: TdxPDFCustomFunction;
    procedure SetTintTransform(const AValue: TdxPDFCustomFunction);
  protected
    procedure DestroySubClasses; override;
    procedure DoRead(AArray: TdxPDFArray); override;
    procedure Initialize; override;

    property TintTransform: TdxPDFCustomFunction read FTintTransform write SetTintTransform;
  public
    function Transform(const AComponents: TDoubleDynArray): TDoubleDynArray; override;
    function Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult; override;
  end;

  { TdxPDFSeparationColorSpace }

  TdxPDFSeparationColorSpace = class(TdxPDFSpecialColorSpace)
  strict private
    procedure ReadName(AObject: TdxPDFBase);
  protected
    class function GetTypeName: string; override;
    function CanRead(ASize: Integer): Boolean; override;
    function GetComponentCount: Integer; override;
    procedure DoRead(AArray: TdxPDFArray); override;
  end;

  { TdxPDFDeviceNColorSpace }

  TdxPDFDeviceNColorSpace = class(TdxPDFSpecialColorSpace)
  strict private
    FNames: TList<string>;

    function IsValidNames(ANames: TList<string>): Boolean;
    procedure ReadNames(AObject: TdxPDFBase);
  protected
    class function GetTypeName: string; override;
    function CanRead(ASize: Integer): Boolean; override;
    function GetComponentCount: Integer; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoRead(AArray: TdxPDFArray); override;

    property Names: TList<string> read FNames;
  end;

  { TdxPDFNChannelColorSpace }

  TdxPDFNChannelColorSpace = class(TdxPDFDeviceNColorSpace)
  strict private
    FColorants: TObjectList<TdxPDFSeparationColorSpace>;

    function GetColorSpaceDictionary(AObject: TdxPDFBase): TdxPDFReaderDictionary;
    procedure ReadColorants(ADictionary: TdxPDFReaderDictionary; AActualNames: TList<string>);
    procedure ReadComponentNames(ADictionary: TdxPDFReaderDictionary; AActualNames: TList<string>);
  protected
    class function GetTypeName: string; override;
    function CanRead(ASize: Integer): Boolean; override;
    procedure CheckComponentCount; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoRead(AArray: TdxPDFArray); overload; override;
  end;

  { TdxPDFPatternColorSpace }

  TdxPDFPatternColorSpace = class(TdxPDFCustomColorSpace)
  protected
    class function GetTypeName: string; override;
    procedure Initialize; override;
    procedure DoRead(AArray: TdxPDFArray); override;
  public
    function Transform(const AComponents: TDoubleDynArray): TDoubleDynArray; override;
    function Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult; override;
  end;

  { TdxPDFCIEColor }

  TdxPDFCIEColor = record
  public
    X: Double;
    Y: Double;
    Z: Double;

    function Create(const AArray: TdxPDFArray): TdxPDFCIEColor;
    function IsEmpty: Boolean;
  end;

  { TdxPDFGamma }

  TdxPDFGamma = class
  strict private
    FDefault: Double;
    FRed: Double;
    FGreen: Double;
    FBlue: Double;
  public
    constructor Create; overload;
    constructor Create(AArray: TdxPDFArray); overload;

    function IsDefault: Boolean;

    property DefaultValue: Double read FDefault;
    property Red: Double read FRed;
    property Green: Double read FGreen;
    property Blue: Double read FBlue;
  end;

  { TdxPDFColorSpaceMatrix }

  TdxPDFColorSpaceMatrix = class
  strict private
    FXA: Double;
    FYA: Double;
    FZA: Double;
    FXB: Double;
    FYB: Double;
    FZB: Double;
    FXC: Double;
    FYC: Double;
    FZC: Double;
  public
    constructor Create; overload;
    constructor Create(AArray: TdxPDFArray); overload;

    function IsIdentity: Boolean;

    property XA: Double read FXA;
    property YA: Double read FYA;
    property ZA: Double read FZA;
    property XB: Double read FXB;
    property YB: Double read FYB;
    property ZB: Double read FZB;
    property XC: Double read FXC;
    property YC: Double read FYC;
    property ZC: Double read FZC;
  end;

  { TdxPDFCIEBasedColorSpace }

  TdxPDFCIEBasedColorSpace = class(TdxPDFCustomColorSpace)
  strict private
    FBlackPoint: TdxPDFCIEColor;
    FWhitePoint: TdxPDFCIEColor;

    procedure ReadBlackPoint(AArray: TdxPDFArray);
    procedure ReadWhitePoint(AArray: TdxPDFArray);
  protected
    function CanRead(ASize: Integer): Boolean; override;
    procedure DoRead(AArray: TdxPDFArray); override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    function DoTransform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult;
    function FillResult(const AResult: TBytes; const AComponents: TDoubleDynArray; APosition: Integer): Integer;
    function GetColorSpaceDictionary(AObject: TdxPDFBase): TdxPDFReaderDictionary;

    property BlackPoint: TdxPDFCIEColor read FBlackPoint;
    property WhitePoint: TdxPDFCIEColor read FWhitePoint;
  end;

  { TdxPDFCalRGBColorSpace }

  TdxPDFCalRGBColorSpace = class(TdxPDFCIEBasedColorSpace)
  strict private
    FGamma: TdxPDFGamma;
    FMatrix: TdxPDFColorSpaceMatrix;

    function ColorComponentTransferFunction(AComponent: Double): Double;
  protected
    class function GetTypeName: string; override;
    function GetComponentCount: Integer; override;
    function GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); overload; override;
  public
    function Transform(const AComponents: TDoubleDynArray): TDoubleDynArray; override;
    function Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult; override;
  end;

  { TdxPDFCalGrayColorSpace }

  TdxPDFCalGrayColorSpace = class(TdxPDFCIEBasedColorSpace)
  strict private
    FGammaValue: Double;

    procedure CreateBlackWhiteComponents(out ABlackComponents, AWhiteComponents: TDoubleDynArray);
    procedure PopulateGray1BitImageData(const AInfo: TdxPDFImageInfo; const ATransformedData: TBytes; out AData: TBytes);
    procedure PopulateImageData(const ATransformedData: TBytes; out AData: TBytes);
  protected
    class function GetTypeName: string; override;
    function GetComponentCount: Integer; override;
    function GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); overload; override;
  public
    function Transform(const AColorComponents: TDoubleDynArray): TDoubleDynArray; override;
    function Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult; override;
  end;

  { TdxPDFLabColorSpace }

  TdxPDFLabColorSpace = class(TdxPDFCIEBasedColorSpace)
  strict private
    FRangeA: TdxPDFRange;
    FRangeB: TdxPDFRange;

    function CorrectRange(const ARange: TdxPDFRange; AValue: Double): Double;
    function GammaFunction(X: Double): Double;
  protected
    class function GetTypeName: string; override;
    function CanRead(ASize: Integer): Boolean; override;
    function CreateDefaultDecodeArray(ABitsPerComponent: Integer): TdxPDFRanges; override;
    function GetComponentCount: Integer; override;
    function GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass; override;
    procedure Initialize; override;
    procedure DoRead(AArray: TdxPDFArray); override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); overload; override;
  public
    function Transform(const AComponents: TDoubleDynArray): TDoubleDynArray; override;
    function Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult; override;
  end;

implementation

uses
  dxCore, dxPDFUtils;

type
  TdxPDFCustomColorSpaceAccess = class(TdxPDFCustomColorSpace);
  TdxPDFNumericObjectAccess = class(TdxPDFNumericObject);
  TdxPDFDictionaryAccess = class(TdxPDFDictionary);

{ TdxPDFGrayColorSpaceTransformation }

function TdxPDFGrayColorSpaceTransformation.Transform(const AData: TBytes): TdxPDFColorSpaceTransformResult;
begin
  case Info.BitsPerComponent of
    1:
      Result := Result.Create(AData, pfGray1bit);
    2, 4, 16:
      Result := Result.Create(UnpackData(AData), pfGray8bit);
    8:
      Result := Result.Create(AData, pfGray8bit);
  else
    Result.IsInvalid := True;
  end;
end;

function TdxPDFGrayColorSpaceTransformation.GetComponentCount: Integer;
begin
  Result := 1;
end;

{ TdxPDFRGBColorSpaceTransformation }

function TdxPDFRGBColorSpaceTransformation.Transform(const AData: TBytes): TdxPDFColorSpaceTransformResult;
var
  ARed, AGreen, ABlue: Byte;
  ATempData, AMaskData: TBytes;
  I, ASourceIndex, ARedMin, ARedMax, AGreenMin, AGreenMax, ABlueMin, ABlueMax, ALength: Integer;
begin
  ATempData := UnpackData(AData);
  if (Info.ColorKeyMask = nil) or (Length(Info.ColorKeyMask) < 3) then
    Exit(Result.Create(ATempData));
  ARedMin := Round(Info.ColorKeyMask[0].Min);
  ARedMax := Round(Info.ColorKeyMask[0].Max);
  AGreenMin := Round(Info.ColorKeyMask[1].Min);
  AGreenMax := Round(Info.ColorKeyMask[1].Max);
  ABlueMin := Round(Info.ColorKeyMask[2].Min);
  ABlueMax := Round(Info.ColorKeyMask[2].Max);
  ALength := Length(ATempData) div 3;
  ASourceIndex := 0;
  SetLength(AMaskData, ALength);
  for I := 0 to ALength - 1 do
  begin
    ARed := ATempData[ASourceIndex];
    AGreen := ATempData[ASourceIndex + 1];
    ABlue := ATempData[ASourceIndex + 2];
    Inc(ASourceIndex, 3);
    AMaskData[I] := IfThen((ARed >= ARedMin) and (ARed <= ARedMax) and (AGreen >= AGreenMin) and (AGreen <= AGreenMax)
      and (ABlue >= ABlueMin) and (ABlue <= ABlueMax), 0, 255);
  end;
  Result := Result.Create(ATempData, AMaskData);
end;

function TdxPDFRGBColorSpaceTransformation.GetComponentCount: Integer;
begin
  Result := 3;
end;

{ TdxPDFCMYKColorSpaceTransformation }

function TdxPDFCMYKColorSpaceTransformation.GetComponentCount: Integer;
begin
  Result := 4;
end;

function TdxPDFCMYKColorSpaceTransformation.Transform(const AData: TBytes): TdxPDFColorSpaceTransformResult;
var
  ATempData, R: TBytes;
  ALength, I, ASourceIndex, ADestinationIndex: Integer;
begin
  if Info.BitsPerComponent <> 8 then
  begin
    Result.IsInvalid := True;
    Exit;
  end;
  ALength := Length(AData) div 4 * 3;
  if (Info.ColorKeyMask = nil) or (Length(Info.ColorKeyMask) = 0) then
  begin
    SetLength(ATempData, ALength);
    ASourceIndex := 0;
    ADestinationIndex := 0;
    for I := 0 to ALength div 3 - 1 do
    begin
      R := TdxPDFARGBColor.ConvertToBytes(AData[ASourceIndex], AData[ASourceIndex + 1], AData[ASourceIndex + 2], AData[ASourceIndex + 3]);
      Inc(ASourceIndex, 4);
      ATempData[ADestinationIndex] := R[0];
      ATempData[ADestinationIndex + 1] := R[1];
      ATempData[ADestinationIndex + 2] := R[2];
      Inc(ADestinationIndex, 3)
    end;
    Result := Result.Create(ATempData);
  end
  else
    Result := CalculateCMYKTransformationResult(AData, ALength);
end;

function TdxPDFCMYKColorSpaceTransformation.CalculateCMYKTransformationResult(const AData: TBytes;
  ALength: Integer): TdxPDFColorSpaceTransformResult;
var
  ARgbData, R, AMaskData: TBytes;
  ACyan, AMagenta, AYellow, ABlack: Byte;
  I, ASourceIndex, ADestinationIndex: Integer;
  ACyanMin, ACyanMax, AMagentaMin, AMagentaMax, AYellowMin, AYellowMax, ABlackMin, ABlackMax, AMask: Integer;
begin
  SetLength(ARgbData, ALength);
  ACyanMin := TdxPDFUtils.ConvertToByte(Info.ColorKeyMask[0].Min);
  ACyanMax := TdxPDFUtils.ConvertToByte(Info.ColorKeyMask[0].Max);
  AMagentaMin := TdxPDFUtils.ConvertToByte(Info.ColorKeyMask[1].Min);
  AMagentaMax := TdxPDFUtils.ConvertToByte(Info.ColorKeyMask[1].Max);
  AYellowMin := TdxPDFUtils.ConvertToByte(Info.ColorKeyMask[2].Min);
  AYellowMax := TdxPDFUtils.ConvertToByte(Info.ColorKeyMask[2].Max);
  ABlackMin := TdxPDFUtils.ConvertToByte(Info.ColorKeyMask[3].Min);
  ABlackMax := TdxPDFUtils.ConvertToByte(Info.ColorKeyMask[3].Max);
  SetLength(AMaskData, Length(AData) div 4);
  ASourceIndex := 0;
  ADestinationIndex := 0;
  AMask := 0;
  for I := 0 to ALength div 3 - 1 do
  begin
    ACyan := AData[ASourceIndex];
    AMagenta := AData[ASourceIndex + 1];
    AYellow := AData[ASourceIndex + 2];
    ABlack := AData[ASourceIndex + 3];
    Inc(ASourceIndex, 4);
    R := TdxPDFARGBColor.ConvertToBytes(ACyan, AMagenta, AYellow, ABlack);
    TdxPDFUtils.CopyData(R, 0, ARgbData, ADestinationIndex, 3);
    Inc(ADestinationIndex, 3);
    AMaskData[AMask] := IfThen((ACyan >= ACyanMin) and (ACyan <= ACyanMax) and (AMagenta >= AMagentaMin) and
      (AMagenta <= AMagentaMax) and (AYellow >= AYellowMin) and (AYellow <= AYellowMax) and (ABlack >= ABlackMin) and
      (ABlack <= ABlackMax), 0, 255);
    Inc(AMask);
  end;
  Result := Result.Create(ARgbData, AMaskData);
end;

{ TdxPDFIndexedColorSpaceCustomTransformation }

constructor TdxPDFIndexedColorSpaceCustomTransformation.Create(AColorSpace: TdxPDFReferencedObject; const AInfo: TdxPDFImageInfo);
var
  ASpace: TdxPDFIndexedColorSpace;
begin
  inherited Create(AInfo);
  ASpace := AColorSpace as TdxPDFIndexedColorSpace;
  FMaxIndex := ASpace.MaxIndex;
  FLookupTable := ASpace.LookupTable;
  FComponentCount := ASpace.AlternateColorSpace.ComponentCount;
  if (Info.ColorKeyMask <> nil) and (Length(Info.ColorKeyMask) = 1) then
  begin
    FTransparentRange := Info.ColorKeyMask[0];
    FNeedPopulateMaskData := not FTransparentRange.IsSame(TdxPDFRange.Invalid);
    SetLength(FMaskData, Info.Width * Info.Height);
  end;
end;

function TdxPDFIndexedColorSpaceCustomTransformation.Transform(const AData: TBytes): TdxPDFColorSpaceTransformResult;
begin
  Result := Result.Create(TransformData(AData));
end;

function TdxPDFIndexedColorSpaceCustomTransformation.TransformData(const AData: TBytes): TBytes;
begin
  SetLength(Result, Info.Width * Info.Height * FComponentCount);
end;

procedure TdxPDFIndexedColorSpaceCustomTransformation.PopulateData(var AData: TBytes; AValue: Byte;
  var ADestinationIndex, APosition: Integer);
begin
  if AValue <= MaxIndex then
  begin
    APosition := AValue * FComponentCount;
    TdxPDFUtils.CopyData(LookupTable, APosition, AData, ADestinationIndex, FComponentCount);
    Inc(ADestinationIndex, FComponentCount);
    Inc(APosition, FComponentCount);
  end
  else
    Inc(ADestinationIndex, FComponentCount);
end;

procedure TdxPDFIndexedColorSpaceCustomTransformation.PopulateMaskData(AValue: Byte; AIndex: Integer);
begin
  if FNeedPopulateMaskData then
    MaskData[AIndex] := IfThen((AValue >= TransparentRange.Min) and (AValue <= TransparentRange.Max), 0, 255);
end;

function TdxPDFIndexedColorSpaceCustomTransformation.GetComponentCount: Integer;
begin
  Result := FComponentCount;
end;

{ TdxPDFIndexedColorSpaceOneTwoBitTransformation }

function TdxPDFIndexedColorSpaceOneTwoBitTransformation.TransformData(const AData: TBytes): TBytes;
var
  B, AMask, AValue, AStartMask: Byte;
  X, Y, APosition, ASourceIndex, ADestinationIndex, AMaskIndex, AShift, AStartShift: Integer;
begin
  Result := inherited TransformData(AData);
  ASourceIndex := 0;
  ADestinationIndex := 0;
  AMaskIndex := 0;
  AStartMask := IfThen(Info.BitsPerComponent = 1, $80, $C0);
  AStartShift := 8 - Info.BitsPerComponent;
  for Y := 0 to Info.Height - 1 do
  begin
    B := 0;
    AMask := AStartMask;
    AShift := AStartShift;
    for X := 0 to Info.Width - 1 do
    begin
      if AShift = AStartShift then
      begin
        B := AData[ASourceIndex];
        Inc(ASourceIndex);
      end;
      AValue := B and AMask shr AShift;
      PopulateData(Result, AValue, ADestinationIndex, APosition);
      AMask := AMask shr Info.BitsPerComponent;
      Dec(AShift, Info.BitsPerComponent);
      if AShift < 0 then
      begin
        AMask := AStartMask;
        AShift := AStartShift;
      end;
      PopulateMaskData(AValue, AMaskIndex);
      Inc(AMaskIndex);
    end;
  end;
end;

{ TdxPDFIndexedColorSpaceEightBitTransformation }

function TdxPDFIndexedColorSpaceEightBitTransformation.TransformData(const AData: TBytes): TBytes;
var
  I, ADestinationIndex, APosition: Integer;
begin
  Result := inherited TransformData(AData);
  ADestinationIndex := 0;
  for I := 0 to Info.Width * Info.Height - 1 do
  begin
    PopulateData(Result, AData[I], ADestinationIndex, APosition);
    PopulateMaskData(AData[I], I);
  end;
end;

{ TdxPDFIndexedColorSpaceFourBitTransformation }

function TdxPDFIndexedColorSpaceFourBitTransformation.TransformData(const AData: TBytes): TBytes;
var
  X, Y, ASourceIndex, ADestinationIndex, AMaskIndex, APosition: Integer;
  B, AValue: Byte;
  AHighBits: Boolean;
begin
  Result := inherited TransformData(AData);
  ASourceIndex := 0;
  ADestinationIndex := 0;
  AMaskIndex := 0;
  for Y := 0 to Info.Height - 1 do
  begin
    B := 0;
    AHighBits := True;
    for X := 0 to Info.Width - 1 do
    begin
      if AHighBits then
      begin
        B := AData[ASourceIndex];
        Inc(ASourceIndex);
        AValue := B and $F0 shr 4;
      end
      else
        AValue := B and $0F;
      PopulateData(Result, AValue, ADestinationIndex, APosition);
      AHighBits := not AHighBits;
      PopulateMaskData(AValue, AMaskIndex);
      Inc(AMaskIndex);
    end;
  end;
end;

{ TdxPDFICCBasedColorSpace }

function TdxPDFICCBasedColorSpace.Transform(const AComponents: TDoubleDynArray): TDoubleDynArray;
begin
  Result := AlternateColorSpace.Transform(AComponents);
end;

function TdxPDFICCBasedColorSpace.Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult;
begin
  Result := AlternateColorSpace.Transform(AInfo);
end;

class function TdxPDFICCBasedColorSpace.GetTypeName: string;
begin
  Result := 'ICCBased';
end;

function TdxPDFICCBasedColorSpace.CreateDefaultDecodeArray(ABitsPerComponent: Integer): TdxPDFRanges;
begin
  SetLength(Result, 0);
  TdxPDFUtils.AddData(FRanges, Result);
end;

procedure TdxPDFICCBasedColorSpace.DoRead(AArray: TdxPDFArray);
var
  ADictionary: TdxPDFReaderDictionary;
begin
  inherited DoRead(AArray);
  if (AArray <> nil) and (AArray.Count = 2) then
  begin
    ADictionary := GetDictionary(AArray[1]);
    ReadComponentCount(ADictionary);
    InitializeRanges;
    ReadAlternativeColorSpace(ADictionary);
    ReadRanges(ADictionary);
  end
  else
    TdxPDFUtils.Abort;
end;

procedure TdxPDFICCBasedColorSpace.Initialize;
begin
  InitializeRanges;
end;

procedure TdxPDFICCBasedColorSpace.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  ReadRanges(ADictionary);
end;

function TdxPDFICCBasedColorSpace.CreateDefaultColorSpace: TdxPDFCustomColorSpace;
begin
  case ComponentCount of
    1:
      Result := TdxPDFGrayDeviceColorSpace.Create(Self);
    4:
      Result := TdxPDFCMYKDeviceColorSpace.Create(Self);
  else
    Result := TdxPDFRGBDeviceColorSpace.Create(Self);
  end;
end;

function TdxPDFICCBasedColorSpace.GetDictionary(AObject: TdxPDFBase): TdxPDFReaderDictionary;
begin
  case AObject.ObjectType of
    otIndirectReference:
      begin
        Result := Repository.GetStream(TdxPDFReference(AObject).Number).Dictionary as TdxPDFReaderDictionary;
        Result.Number := TdxPDFReference(AObject).Number;
      end;
    otStream:
      Result := TdxPDFStream(AObject).Dictionary as TdxPDFReaderDictionary;
  else
    Result := nil;
  end;
end;

procedure TdxPDFICCBasedColorSpace.InitializeRanges;
var
  I: Integer;
begin
  SetLength(FRanges, ComponentCount);
  for I := 0 to ComponentCount - 1 do
    FRanges[I] := TdxPDFRange.Create(0, 1);
end;

procedure TdxPDFICCBasedColorSpace.DoReadRanges(AArray: TdxPDFArray);

  function CalculateRange(AArray: TdxPDFArray; AIndex: Integer): TdxPDFRange;
  var
    AMin, AMax: Double;
  begin
    AMax := TdxPDFNumericObjectAccess(AArray[AIndex]).InternalValue;
    AMin := TdxPDFNumericObjectAccess(AArray[AIndex + 1]).InternalValue;
    Result := TdxPDFRange.Create(AMin, AMax)
  end;

var
  I, AIndex, ALength: Integer;
begin
  ALength := AArray.Count div 2;
  SetLength(FRanges, ALength);
  AIndex := 0;
  for I := 0 to ALength - 1 do
  begin
    FRanges[I] := CalculateRange(AArray, AIndex);
    Inc(AIndex, 2);
  end;
end;

procedure TdxPDFICCBasedColorSpace.ReadAlternativeColorSpace(ADictionary: TdxPDFReaderDictionary);
var
  AObject: TdxPDFBase;
begin
  if (ADictionary <> nil) and ADictionary.TryGetObject(TdxPDFKeywords.Alternate, AObject) then
    AlternateColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AObject)
  else
    AlternateColorSpace := CreateDefaultColorSpace;
end;

procedure TdxPDFICCBasedColorSpace.ReadComponentCount(ADictionary: TdxPDFDictionary);
begin
  if ADictionary <> nil then
  begin
    ComponentCount := ADictionary.GetInteger(TdxPDFKeywords.Count);
    if not TdxPDFUtils.IsIntegerValid(ComponentCount) then
      TdxPDFUtils.Abort;
  end;
end;

procedure TdxPDFICCBasedColorSpace.ReadRanges(ADictionary: TdxPDFDictionary);
var
  AArray: TdxPDFArray;
begin
  AArray := ADictionary.GetArray(TdxPDFKeywords.Range);
  if AArray <> nil then
    if AArray.Count <> ComponentCount * 2 then
      TdxPDFUtils.Abort
    else
      DoReadRanges(AArray);
end;

{ TdxPDFCustomDeviceColorSpace }

function TdxPDFCustomDeviceColorSpace.Transform(const AComponents: TDoubleDynArray): TDoubleDynArray;
var
  I: Integer;
  AComponent: Double;
begin
  Result := inherited Transform(AComponents);
  if Length(AComponents) > 0 then
  begin
    for AComponent in AComponents do
      if not InRange(AComponent, 0, 1) then
      begin
        SetLength(Result, Length(AComponents));
        for I := 0 to Length(AComponents) - 1 do
          if AComponents[I] < 0 then
            Result[I] := 0
          else
            if AComponents[I] > 1 then
              Result[I] := 1
            else
              Result[I] := AComponents[I];
        Break;
      end;
  end
end;

function TdxPDFCustomDeviceColorSpace.Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult;
var
  ATransformation: TdxPDFCustomColorSpaceTransformation;
begin
  ATransformation := GetTransformationClass.Create(AInfo);
  try
    Result := ATransformation.Transform(AInfo.Data);
  finally
    ATransformation.Free;
  end;
end;

{ TdxPDFRGBDeviceColorSpace }

class function TdxPDFRGBDeviceColorSpace.GetTypeName: string;
begin
  Result := 'DeviceRGB';
end;

function TdxPDFRGBDeviceColorSpace.GetComponentCount: Integer;
begin
  Result := 3;
end;

function TdxPDFRGBDeviceColorSpace.GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass;
begin
  Result := TdxPDFRGBColorSpaceTransformation;
end;

{ TdxPDFGrayDeviceColorSpace }

class function TdxPDFGrayDeviceColorSpace.GetTypeName: string;
begin
  Result := 'DeviceGray';
end;

function TdxPDFGrayDeviceColorSpace.GetComponentCount: Integer;
begin
  Result := 1;
end;

function TdxPDFGrayDeviceColorSpace.GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass;
begin
  Result := TdxPDFGrayColorSpaceTransformation;
end;

{ TdxPDFCMYKDeviceColorSpace }

class function TdxPDFCMYKDeviceColorSpace.GetTypeName: string;
begin
  Result := 'DeviceCMYK';
end;

function TdxPDFCMYKDeviceColorSpace.GetComponentCount: Integer;
begin
  Result := 4;
end;

function TdxPDFCMYKDeviceColorSpace.GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass;
begin
  Result := TdxPDFCMYKColorSpaceTransformation;
end;

{ TdxPDFIndexedColorSpace }

function TdxPDFIndexedColorSpace.Transform(const AComponents: TDoubleDynArray): TDoubleDynArray;

  function CreateTransformedComponents(const AColorComponents: TDoubleDynArray): TDoubleDynArray;
  var
    I, AIndex, AComponentCount: Integer;
  begin
    AComponentCount := AlternateColorSpace.ComponentCount;
    SetLength(Result, AComponentCount);
    AIndex := Round(AColorComponents[0]) * AComponentCount;
    for I  := 0 to AComponentCount - 1 do
    begin
      Result[I] := FLookupTable[AIndex] / 255;
      Inc(AIndex);
    end;
  end;

begin
  if Length(AComponents) = 1 then
    Result := AlternateColorSpace.Transform(CreateTransformedComponents(AComponents))
  else
    Result := inherited Transform(AComponents);
end;

function TdxPDFIndexedColorSpace.Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult;
var
  AMaskData: TBytes;
  ATransformation: TdxPDFIndexedColorSpaceCustomTransformation;
begin
  Result.IsInvalid := True;
  ATransformation := CreateTransformation(AInfo);
  if ATransformation <> nil then
    try
      Result := ATransformation.Transform(AInfo.Data);
      AMaskData := Result.MaskData;
      Result := AlternateColorSpace.Transform(dxPDFImageInfo(Result.Data, AInfo.Width, AInfo.Height, 8, nil));
      if not Result.IsInvalid then
        Result.MaskData := ATransformation.MaskData;
    finally
      ATransformation.Free;
    end;
end;

class function TdxPDFIndexedColorSpace.GetTypeName: string;
begin
  Result := 'Indexed';
end;

function TdxPDFIndexedColorSpace.CanRead(ASize: Integer): Boolean;
begin
  Result := ASize = 4;
end;

function TdxPDFIndexedColorSpace.CreateDefaultDecodeArray(ABitsPerComponent: Integer): TdxPDFRanges;
begin
  SetLength(Result, 1);
  Result[0] := TdxPDFRange.Create(0, 1 shl ABitsPerComponent - 1);
end;

function TdxPDFIndexedColorSpace.GetComponentCount: Integer;
begin
  Result := 1;
end;

function TdxPDFIndexedColorSpace.GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass;
begin
  Result:= nil;
end;

procedure TdxPDFIndexedColorSpace.DoRead(AArray: TdxPDFArray);
begin
  inherited DoRead(AArray);
  AlternateColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AArray[1]);
  ReadMaxIndex(AArray[2]);
  ReadLookupTable(AArray[3]);
end;

procedure TdxPDFIndexedColorSpace.Initialize;
begin
  inherited Initialize;
  FMaxIndex := dxPDFInvalidValue;
  SetLength(FLookupTable, 0);
end;

function TdxPDFIndexedColorSpace.CreateTransformation(const AInfo: TdxPDFImageInfo): TdxPDFIndexedColorSpaceCustomTransformation;
begin
  case AInfo.BitsPerComponent of
    1, 2:
      Result := TdxPDFIndexedColorSpaceOneTwoBitTransformation.Create(Self, AInfo);
    8:
      Result := TdxPDFIndexedColorSpaceEightBitTransformation.Create(Self, AInfo);
    4:
      Result := TdxPDFIndexedColorSpaceFourBitTransformation.Create(Self, AInfo);
  else
    Result := nil;
  end;
end;

procedure TdxPDFIndexedColorSpace.ReadLookupTable(AObject: TdxPDFBase);
var
  AValue: TdxPDFReferencedObject;
  AExpectedLength: Integer;
begin
  if AObject.ObjectType = otString then
    FLookupTable := TdxPDFUtils.StrToByteArray(TdxPDFString(AObject).Value)
  else
  begin
    if AObject.ObjectType = otIndirectReference then
      AValue := Repository.GetObject(TdxPDFReference(AObject).Number)
    else
      AValue := AObject as TdxPDFStream;
    SetLength(FLookupTable, 0);
    FLookupTable := (AValue as TdxPDFStream).UncompressedData;
  end;
  AExpectedLength := AlternateColorSpace.ComponentCount * (FMaxIndex + 1);
  if Length(FLookupTable) <> AExpectedLength then
    SetLength(FLookupTable, AExpectedLength)
end;

procedure TdxPDFIndexedColorSpace.ReadMaxIndex(AObject: TdxPDFBase);
begin
  if AObject.ObjectType = otInteger then
    FMaxIndex := Min(Max(TdxPDFInteger(AObject).Value, 0), 255);
end;

{ TdxPDFSpecialColorSpace }

function TdxPDFSpecialColorSpace.Transform(const AComponents: TDoubleDynArray): TDoubleDynArray;
begin
  Result := AlternateColorSpace.Transform(TintTransform.CreateTransformedComponents(AComponents));
end;

function TdxPDFSpecialColorSpace.Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult;
var
  ATempData: TBytes;
  AArguments, AComponents: TDoubleDynArray;
  I, J, ALength, ATempIndex: Integer;
begin
  if AInfo.BitsPerComponent <> 8 then
  begin
    Result.IsInvalid := True;
    Exit;
  end;
  ALength := Length(AInfo.Data);
  I := 0;
  ATempIndex := 0;

  SetLength(AArguments, ComponentCount);
  SetLength(ATempData, ALength * AlternateColorSpace.ComponentCount);
  while I < ALength do
  begin
    for J := 0 to ComponentCount - 1 do
    begin
      AArguments[J] := AInfo.Data[I] / 255;
      Inc(I);
    end;
    AComponents := TintTransform.CreateTransformedComponents(AArguments);

    if Length(AComponents) < AlternateColorSpace.ComponentCount then
    begin
      Result.IsInvalid := True;
      Exit;
    end;

    for J := 0 to AlternateColorSpace.ComponentCount - 1 do
    begin
      ATempData[ATempIndex] := TdxPDFUtils.ConvertToByte(AComponents[J] * 255);
      Inc(ATempIndex);
    end;
  end;
  Result := AlternateColorSpace.Transform(dxPDFImageInfo(ATempData, AInfo.Width, AInfo.Height, 8, nil));
end;

procedure TdxPDFSpecialColorSpace.DestroySubClasses;
begin
  TintTransform := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFSpecialColorSpace.DoRead(AArray: TdxPDFArray);
begin
  inherited DoRead(AArray);
  AlternateColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AArray[2]);
  TintTransform := TdxPDFCustomFunction.Parse(Repository, AArray[3]);
  if TintTransform.RangeCount <> AlternateColorSpace.ComponentCount then
    TdxPDFUtils.Abort;
end;

procedure TdxPDFSpecialColorSpace.Initialize;
begin
  inherited Initialize;
  TintTransform := nil;
end;

procedure TdxPDFSpecialColorSpace.SetTintTransform(const AValue: TdxPDFCustomFunction);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FTintTransform));
end;

{ TdxPDFSeparationColorSpace }

class function TdxPDFSeparationColorSpace.GetTypeName: string;
begin
  Result := 'Separation';
end;

function TdxPDFSeparationColorSpace.CanRead(ASize: Integer): Boolean;
begin
  Result := ASize = 4;
end;

function TdxPDFSeparationColorSpace.GetComponentCount: Integer;
begin
  Result := 1;
end;

procedure TdxPDFSeparationColorSpace.DoRead(AArray: TdxPDFArray);
begin
  inherited DoRead(AArray);
  ReadName(AArray[1]);
end;

procedure TdxPDFSeparationColorSpace.ReadName(AObject: TdxPDFBase);
begin
  if not (AObject.ObjectType in [otName, otString]) or (Length(TintTransform.Domain) <> 1) then
    TdxPDFUtils.Abort
  else
    Name := (AObject as TdxPDFName).Value;
end;

{ TdxPDFDeviceNColorSpace }

class function TdxPDFDeviceNColorSpace.GetTypeName: string;
begin
  Result := 'DeviceN';
end;

function TdxPDFDeviceNColorSpace.CanRead(ASize: Integer): Boolean;
begin
  Result := (ASize = 4) or (ASize = 5);
end;

function TdxPDFDeviceNColorSpace.GetComponentCount: Integer;
begin
  Result := FNames.Count;
end;

procedure TdxPDFDeviceNColorSpace.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FNames := TList<string>.Create;
end;

procedure TdxPDFDeviceNColorSpace.DestroySubClasses;
begin
  FreeAndNil(FNames);
  inherited DestroySubClasses;
end;

procedure TdxPDFDeviceNColorSpace.DoRead(AArray: TdxPDFArray);
begin
  inherited DoRead(AArray);
  if (AlternateColorSpace is TdxPDFRGBDeviceColorSpace) or (AlternateColorSpace is TdxPDFCMYKDeviceColorSpace) or
    (AlternateColorSpace is TdxPDFGrayDeviceColorSpace) or (AlternateColorSpace is TdxPDFICCBasedColorSpace) or
    (AlternateColorSpace is TdxPDFCIEBasedColorSpace) then
    ReadNames(AArray[1])
  else
    TdxPDFUtils.Abort;
end;

function TdxPDFDeviceNColorSpace.IsValidNames(ANames: TList<string>): Boolean;
var
  I, J: Integer;
begin
  Result := True;
  if ANames.Count > 1 then
    for I := 1 to ANames.Count - 1 do
      if ANames[I] <> 'None' then
        for J := 0 to I - 1 do
          if ANames[I] = ANames[J] then
            Exit(False);
end;

procedure TdxPDFDeviceNColorSpace.ReadNames(AObject: TdxPDFBase);
var
  I: Integer;
  AArray: TdxPDFArray;
  AName: TdxPDFName;
begin
  if AObject.ObjectType = otArray then
  begin
    AArray := AObject as TdxPDFArray;
    for I := 0 to AArray.Count - 1 do
    begin
      AName := AArray[I] as TdxPDFName;
      if AName.Value <> '' then
        FNames.Add(AName.Value);
    end;
    IsValidNames(FNames);
  end;
end;

{ TdxPDFNChannelColorSpace }

class function TdxPDFNChannelColorSpace.GetTypeName: string;
begin
  Result := 'NChannel';
end;

function TdxPDFNChannelColorSpace.CanRead(ASize: Integer): Boolean;
begin
  Result := ASize = 5;
end;

procedure TdxPDFNChannelColorSpace.CheckComponentCount;
begin
// do nothing
end;

procedure TdxPDFNChannelColorSpace.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FColorants := TObjectList<TdxPDFSeparationColorSpace>.Create;
end;

procedure TdxPDFNChannelColorSpace.DestroySubClasses;
begin
  FreeAndNil(FColorants);
  inherited DestroySubClasses;
end;

procedure TdxPDFNChannelColorSpace.DoRead(AArray: TdxPDFArray);
var
  AActualNames: TList<string>;
  ADictionary: TdxPDFDictionary;
begin
  inherited DoRead(AArray);
  ADictionary := GetColorSpaceDictionary(AArray[4]);
  if ADictionary <> nil then
  begin
    AActualNames := TList<string>.Create;
    try
      ReadColorants(ADictionary.GetDictionary('Colorants') as TdxPDFReaderDictionary, AActualNames);
      ReadComponentNames(ADictionary.GetDictionary('Process') as TdxPDFReaderDictionary, AActualNames);
      ComponentCount := Names.Count;
    finally
      AActualNames.Free;
    end;
  end;
end;

function TdxPDFNChannelColorSpace.GetColorSpaceDictionary(AObject: TdxPDFBase): TdxPDFReaderDictionary;
begin
  if AObject.ObjectType <> otDictionary then
  begin
    Result := Repository.GetDictionary((AObject as TdxPDFReference).Number) as TdxPDFReaderDictionary;
    if Result = nil then
      TdxPDFUtils.Abort;
  end
  else
    Result := AObject as TdxPDFReaderDictionary;
end;

procedure TdxPDFNChannelColorSpace.ReadColorants(ADictionary: TdxPDFReaderDictionary;
  AActualNames: TList<string>);
var
  AColorSpace: TdxPDFSeparationColorSpace;
  AKey: string;
begin
  FColorants.Clear;
  if ADictionary <> nil then
    for AKey in TdxPDFDictionaryAccess(ADictionary).Items.Keys do
    begin
      AColorSpace := TdxPDFCustomColorSpace.Parse(Repository, ADictionary.GetObject(AKey)) as TdxPDFSeparationColorSpace;
      if (AColorSpace = nil) or (AColorSpace.Name <> AKey) or (AColorSpace.ComponentCount <> 1) then
        TdxPDFUtils.Abort;
      FColorants.Add(AColorSpace);
      AActualNames.Add(AKey);
    end;
end;

procedure TdxPDFNChannelColorSpace.ReadComponentNames(ADictionary: TdxPDFReaderDictionary; AActualNames: TList<string>);
var
  I: Integer;
  AComponents: TdxPDFArray;
  AObject: TdxPDFBase;
  ATempNames: TList<string>;
begin
  if ADictionary <> nil then
  begin
    AComponents := ADictionary.GetArray(TdxPDFKeywords.Components);
    if (AComponents = nil) or not ADictionary.TryGetObject(TdxPDFKeywords.ColorSpace, AObject) then
      TdxPDFUtils.Abort;
    AlternateColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AObject);
    if AComponents.Count <> AlternateColorSpace.ComponentCount then
      TdxPDFUtils.Abort;
    ATempNames := TList<string>.Create;
    try
      for I := 0 to AComponents.Count - 1 do
        if AComponents[I].ObjectType = otName then
          ATempNames.Add(TdxPDFName(AComponents[I]).Value)
        else
          TdxPDFUtils.Abort;
      AActualNames.AddRange(ATempNames);
    finally
      ATempNames.Free;
    end;
  end;
end;

{ TdxPDFPatternColorSpace }

function TdxPDFPatternColorSpace.Transform(const AComponents: TDoubleDynArray): TDoubleDynArray;
begin
  Result := AlternateColorSpace.Transform(AComponents);
end;

function TdxPDFPatternColorSpace.Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult;
begin
  Result := AlternateColorSpace.Transform(AInfo);
end;

class function TdxPDFPatternColorSpace.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Pattern;
end;

procedure TdxPDFPatternColorSpace.Initialize;
begin
  inherited Initialize;
  AlternateColorSpace := TdxPDFRGBDeviceColorSpace.Create(Self);
end;

procedure TdxPDFPatternColorSpace.DoRead(AArray: TdxPDFArray);
begin
  inherited DoRead(AArray);
  if AArray.Count = 2 then
    AlternateColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AArray[1]);
end;

{ TdxPDFCIEColor }

function TdxPDFCIEColor.Create(const AArray: TdxPDFArray): TdxPDFCIEColor;
begin
  if AArray.Count = 3 then
  begin
    Result.X := Max(TdxPDFUtils.ConvertToDouble(AArray[0]), 0);
    Result.Y := Max(TdxPDFUtils.ConvertToDouble(AArray[1]), 0);
    Result.Z := Max(TdxPDFUtils.ConvertToDouble(AArray[2]), 0);
  end;
end;

function TdxPDFCIEColor.IsEmpty: Boolean;
begin
  Result := (X = 0) and (Y = 0) and (Z = 0);
end;

{ TdxPDFGamma }

constructor TdxPDFGamma.Create;
begin
  inherited Create;
  FDefault := 1.0;
  FRed := FDefault;
  FGreen := FDefault;
  FBlue := FDefault;
end;

constructor TdxPDFGamma.Create(AArray: TdxPDFArray);
begin
  Create;
  if AArray.Count = 3 then
  begin
    FRed := TdxPDFUtils.ConvertToDouble(AArray[0]);
    FGreen := TdxPDFUtils.ConvertToDouble(AArray[1]);
    FBlue := TdxPDFUtils.ConvertToDouble(AArray[2]);
  end;
end;

function TdxPDFGamma.IsDefault: Boolean;
begin
  Result := (FRed = FDefault) and (FGreen = FDefault) and (FBlue = FDefault);
end;

{ TdxPDFColorSpaceMatrix }

constructor TdxPDFColorSpaceMatrix.Create;
begin
  inherited Create;
  FXA := 1;
  FYA := 0;
  FZA := 0;
  FXB := 0;
  FYB := 1;
  FZB := 0;
  FXC := 0;
  FYC := 0;
  FZC := 1;
end;

constructor TdxPDFColorSpaceMatrix.Create(AArray: TdxPDFArray);
begin
  Create;
  if AArray.Count = 9 then
  begin
    FXA := TdxPDFUtils.ConvertToDouble(AArray[0]);
    FYA := TdxPDFUtils.ConvertToDouble(AArray[1]);
    FZA := TdxPDFUtils.ConvertToDouble(AArray[2]);
    FXB := TdxPDFUtils.ConvertToDouble(AArray[3]);
    FYB := TdxPDFUtils.ConvertToDouble(AArray[4]);
    FZB := TdxPDFUtils.ConvertToDouble(AArray[5]);
    FXC := TdxPDFUtils.ConvertToDouble(AArray[6]);
    FYC := TdxPDFUtils.ConvertToDouble(AArray[7]);
    FZC := TdxPDFUtils.ConvertToDouble(AArray[8]);
  end;
end;

function TdxPDFColorSpaceMatrix.IsIdentity: Boolean;
begin
  Result :=
    (FXA = 1) and (FYA = 0) and (FZA = 0) and
    (FXB = 0) and (FYB = 1) and (FZB = 0) and
    (FXC = 0) and (FYC = 0) and (FZC = 1);
end;

{ TdxPDFCIEBasedColorSpace }

function TdxPDFCIEBasedColorSpace.CanRead(ASize: Integer): Boolean;
begin
  Result := ASize = 2;
end;

procedure TdxPDFCIEBasedColorSpace.DoRead(AArray: TdxPDFArray);
begin
  inherited DoRead(AArray);
  if CanRead(AArray.Count) then
    Read(GetColorSpaceDictionary(AArray[1]) as TdxPDFReaderDictionary)
  else
    TdxPDFUtils.Abort;
end;

procedure TdxPDFCIEBasedColorSpace.Read(ADictionary: TdxPDFReaderDictionary);
begin
  if ADictionary <> nil then
  begin
    ReadProperties(ADictionary);
    ReadWhitePoint(ADictionary.GetArray(TdxPDFKeywords.WhitePoint));
    ReadBlackPoint(ADictionary.GetArray(TdxPDFKeywords.BlackPoint));
  end;
end;

function TdxPDFCIEBasedColorSpace.DoTransform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult;
var
  ATransformation: TdxPDFCustomColorSpaceTransformation;
begin
  ATransformation := GetTransformationClass.Create(AInfo);
  try
    Result := ATransformation.Transform(AInfo.Data);
  finally
    ATransformation.Free;
  end;
end;

function TdxPDFCIEBasedColorSpace.FillResult(const AResult: TBytes; const AComponents: TDoubleDynArray; APosition: Integer): Integer;
begin
  AResult[APosition] := TdxPDFUtils.ConvertToByte(AComponents[0] * 255.0);
  AResult[APosition + 1] := TdxPDFUtils.ConvertToByte(AComponents[1] * 255.0);
  AResult[APosition + 2] := TdxPDFUtils.ConvertToByte(AComponents[2] * 255.0);
  Inc(APosition, 3);
  Result := APosition;
end;

function TdxPDFCIEBasedColorSpace.GetColorSpaceDictionary(AObject: TdxPDFBase): TdxPDFReaderDictionary;
begin
  case AObject.ObjectType of
    otDictionary:
      Result := AObject as TdxPDFReaderDictionary;
    otIndirectReference:
      begin
        Result := Repository.GetDictionary(TdxPDFReference(AObject).Number) as TdxPDFReaderDictionary;
        if Result = nil then
          TdxPDFUtils.Abort;
      end;
    otArray:
      begin
        if TdxPDFArray(AObject).Count <> 2 then
          TdxPDFUtils.RaiseTestException('Incorrect array value count');
        Result := GetColorSpaceDictionary(TdxPDFArray(AObject)[1]) as TdxPDFReaderDictionary
      end
  else
    Result := nil;
  end;
end;

procedure TdxPDFCIEBasedColorSpace.ReadBlackPoint(AArray: TdxPDFArray);
begin
  if AArray <> nil then
    FBlackPoint := FBlackPoint.Create(AArray);
end;

procedure TdxPDFCIEBasedColorSpace.ReadWhitePoint(AArray: TdxPDFArray);
begin
  if AArray <> nil then
  begin
    FWhitePoint := FWhitePoint.Create(AArray);
    if (FWhitePoint.X <= 0) or (FWhitePoint.Y <> 1) or (FWhitePoint.Z <= 0) then
      TdxPDFUtils.Abort;
  end
  else
    TdxPDFUtils.Abort;
end;

{ TdxPDFCalRGBColorSpace }

function TdxPDFCalRGBColorSpace.Transform(const AComponents: TDoubleDynArray): TDoubleDynArray;
var
  ARed, AGreen, ABlue, X, Y, Z: Double;
begin
  ARed := Power(AComponents[0], FGamma.Red);
  AGreen := Power(AComponents[1], FGamma.Green);
  ABlue := Power(AComponents[2], FGamma.Blue);
  if FMatrix.IsIdentity then
  begin
    ARed := ColorComponentTransferFunction(ARed);
    AGreen := ColorComponentTransferFunction(AGreen);
    ABlue := ColorComponentTransferFunction(ABlue);
    X := TdxPDFColor.ClipColorComponent((ARed * 0.4124 + AGreen * 0.3576 + ABlue * 0.1805 - BlackPoint.X) / (WhitePoint.X - BlackPoint.X));
    Y := TdxPDFColor.ClipColorComponent((ARed * 0.2126 + AGreen * 0.7152 + ABlue * 0.0722 - BlackPoint.Y) / (WhitePoint.Y - BlackPoint.Y));
    Z := TdxPDFColor.ClipColorComponent((ARed * 0.0193 + AGreen * 0.1192 + ABlue * 0.9505 - BlackPoint.Z) / (WhitePoint.Z - BlackPoint.Z));
  end
  else
  begin
    X := FMatrix.XA * ARed + FMatrix.XB * AGreen + FMatrix.XC * ABlue;
    Y := FMatrix.YA * ARed + FMatrix.YB * AGreen + FMatrix.YC * ABlue;
    Z := FMatrix.ZA * ARed + FMatrix.ZB * AGreen + FMatrix.ZC * ABlue;
  end;
  Result := TdxPDFColor.GetComponents(X, Y, Z, WhitePoint.Z);
end;

function TdxPDFCalRGBColorSpace.Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult;
var
  I, ASourceIndex, ADestinationIndex, ALength: Integer;
  ATempData: TBytes;
  ATemp: TdxPDFColorSpaceTransformResult;
  AColorComponents: TDoubleDynArray;
begin
  ATemp := DoTransform(AInfo);
  ASourceIndex := 0;
  ADestinationIndex := 0;
  ALength := Length(ATemp.Data);
  SetLength(ATempData, ALength);
  ALength := ALength div 3;

  SetLength(AColorComponents, 3);
  for  I := 0 to ALength - 1 do
  begin
    AColorComponents[0] := ATemp.Data[ASourceIndex] / 255.0;
    AColorComponents[1] := ATemp.Data[ASourceIndex + 1] / 255.0;
    AColorComponents[2] := ATemp.Data[ASourceIndex + 2] / 255.0;
    Inc(ASourceIndex, 3);
    ADestinationIndex := FillResult(ATempData, AColorComponents, ADestinationIndex);
  end;
  Result := Result.Create(ATempData, ATemp.MaskData);
end;

class function TdxPDFCalRGBColorSpace.GetTypeName: string;
begin
  Result := 'CalRGB';
end;

function TdxPDFCalRGBColorSpace.GetComponentCount: Integer;
begin
  Result := 3;
end;

function TdxPDFCalRGBColorSpace.GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass;
begin
  Result := TdxPDFRGBColorSpaceTransformation;
end;

procedure TdxPDFCalRGBColorSpace.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FGamma := TdxPDFGamma.Create;
  FMatrix := TdxPDFColorSpaceMatrix.Create;
end;

procedure TdxPDFCalRGBColorSpace.DestroySubClasses;
begin
  FreeAndNil(FGamma);
  FreeAndNil(FMatrix);
  inherited DestroySubClasses;
end;

procedure TdxPDFCalRGBColorSpace.Read(ADictionary: TdxPDFReaderDictionary);
var
  AArray: TdxPDFArray;
begin
  inherited Read(ADictionary);
  AArray := ADictionary.GetArray(TdxPDFKeywords.Gamma);
  if AArray <> nil then
  begin
    FGamma.Free;
    FGamma := TdxPDFGamma.Create(AArray);
  end;
  AArray := ADictionary.GetArray(TdxPDFKeywords.Matrix);
  if AArray <> nil then
  begin
    FMatrix.Free;
    FMatrix := TdxPDFColorSpaceMatrix.Create(AArray);
  end;
end;

function TdxPDFCalRGBColorSpace.ColorComponentTransferFunction(AComponent: Double): Double;
begin
  if AComponent > 0.04045 then
    Result := Power((AComponent + 0.055) / 1.055, 2.4)
  else
    Result := AComponent / 12.92;
end;

{ TdxPDFCalGrayColorSpace }

function TdxPDFCalGrayColorSpace.Transform(const AColorComponents: TDoubleDynArray): TDoubleDynArray;
var
  ALuminosity: Double;
begin
  ALuminosity := TdxPDFUtils.Max(116 * Power(BlackPoint.Y + (WhitePoint.Y - BlackPoint.Y) * Power(AColorComponents[0],
    FGammaValue), 1.0 / 3.0) - 16, 0) / 100.0;
  SetLength(Result, 3);
  Result[0] := ALuminosity;
  Result[1] := ALuminosity;
  Result[2] := ALuminosity;
end;

function TdxPDFCalGrayColorSpace.Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult;
var
  AData: TBytes;
begin
  Result := DoTransform(AInfo);
  if Result.PixelFormat = pfGray1bit then
    PopulateGray1BitImageData(AInfo, Result.Data, AData)
  else
    PopulateImageData(Result.Data, AData);
  Result := Result.Create(AData);
end;

class function TdxPDFCalGrayColorSpace.GetTypeName: string;
begin
  Result := 'CalGray';
end;

function TdxPDFCalGrayColorSpace.GetComponentCount: Integer;
begin
  Result := 1;
end;

function TdxPDFCalGrayColorSpace.GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass;
begin
  Result := TdxPDFGrayColorSpaceTransformation;
end;

procedure TdxPDFCalGrayColorSpace.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  if ADictionary.Contains(TdxPDFKeywords.Gamma) then
    FGammaValue := Max(ADictionary.GetDouble(TdxPDFKeywords.Gamma), 0)
  else
    FGammaValue := 1;
end;

procedure TdxPDFCalGrayColorSpace.CreateBlackWhiteComponents(out ABlackComponents, AWhiteComponents: TDoubleDynArray);
var
  AComponents: TDoubleDynArray;
begin
  SetLength(AComponents, 1);
  AComponents[0] := 1;
  TdxPDFUtils.AddData(Transform(AComponents), AWhiteComponents);
  AComponents[0] := 0;
  TdxPDFUtils.AddData(Transform(AComponents), ABlackComponents);
end;

procedure TdxPDFCalGrayColorSpace.PopulateGray1BitImageData(const AInfo: TdxPDFImageInfo; const ATransformedData: TBytes;
  out AData: TBytes);
var
  B: Byte;
  X, Y, ASourceIndex, ADestinationIndex, AMask: Integer;
  AWhiteComponents, ABlackComponents: TDoubleDynArray;
begin
  CreateBlackWhiteComponents(ABlackComponents, AWhiteComponents);

  SetLength(AData, AInfo.Width * AInfo.Height * 3);
  ASourceIndex := 0;
  ADestinationIndex := 0;
  for Y := 0 to AInfo.Height - 1 do
  begin
    B := 0;
    AMask := 0;
    for X := 0 to AInfo.Width - 1 do
    begin
      if AMask = 0 then
      begin
        AMask := 128;
        B := ATransformedData[ASourceIndex];
        Inc(ASourceIndex);
      end;
      if (B and AMask) = 0 then
        ADestinationIndex := FillResult(AData, ABlackComponents, ADestinationIndex)
      else
        ADestinationIndex := FillResult(AData, AWhiteComponents, ADestinationIndex);
      AMask := AMask shr 1;
    end;
  end;
end;

procedure TdxPDFCalGrayColorSpace.PopulateImageData(const ATransformedData: TBytes; out AData: TBytes);
var
  I, AIndex: Integer;
  AComponents: TDoubleDynArray;
begin
  AIndex := 0;
  SetLength(AData, Length(ATransformedData) * 3);
  for I := 0 to Length(ATransformedData) - 1 do
  begin
    SetLength(AComponents, 1);
    AComponents[0] := ATransformedData[I] / 255.0;
    AIndex := FillResult(AData, Transform(AComponents), AIndex);
  end;
end;

{ TdxPDFLabColorSpace }

function TdxPDFLabColorSpace.Transform(const AComponents: TDoubleDynArray): TDoubleDynArray;
var
  M: Double;
begin
  M := (AComponents[0] + 16) / 116;
  Result := TdxPDFColor.GetComponents(
    BlackPoint.X + (WhitePoint.X - BlackPoint.X) * GammaFunction(M + CorrectRange(FRangeA, AComponents[1]) / 500),
    BlackPoint.Y + (WhitePoint.Y - BlackPoint.Y) * GammaFunction(M),
    BlackPoint.Z + (WhitePoint.Z - BlackPoint.Z) * GammaFunction(M - CorrectRange(FRangeB, AComponents[2]) / 200),
    WhitePoint.Z);
end;

function TdxPDFLabColorSpace.Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult;
var
  I, ASourceIndex, ADestinationIndex, ALength: Integer;
  AResult: TBytes;
  ARangeALength, ARangeBLength: Double;
  AComponents: TDoubleDynArray;
begin
  ASourceIndex := 0;
  ADestinationIndex := 0;
  ARangeALength := FRangeA.Max - FRangeA.Min;
  ARangeBLength := FRangeB.Max - FRangeB.Min;
  ALength := Length(AInfo.Data);
  SetLength(AResult, ALength);
  SetLength(AComponents, 3);
  for I := 0 to ALength div 3 - 1 do
  begin
    AComponents[0] := AInfo.Data[ASourceIndex] / 2.55;
    AComponents[1] := FRangeA.Min + AInfo.Data[ASourceIndex + 1] / 255.0 * ARangeALength;
    AComponents[2] := FRangeB.Min + AInfo.Data[ASourceIndex + 2] / 255.0 * ARangeBLength;
    ADestinationIndex := FillResult(AResult, Transform(AComponents), ADestinationIndex);
    Inc(ASourceIndex, 3);
  end;
  Result := Result.Create(AResult);
end;

class function TdxPDFLabColorSpace.GetTypeName: string;
begin
  Result := 'Lab';
end;

function TdxPDFLabColorSpace.CanRead(ASize: Integer): Boolean;
begin
  Result := ASize = 4;
end;

function TdxPDFLabColorSpace.CreateDefaultDecodeArray(ABitsPerComponent: Integer): TdxPDFRanges;
begin
  SetLength(Result, 3);
  Result[0] := TdxPDFRange.Create(0, 100);
  Result[1] := TdxPDFRange.Create(FRangeA.Min, FRangeA.Max);
  Result[2] := TdxPDFRange.Create(FRangeB.Min, FRangeB.Max);
end;

function TdxPDFLabColorSpace.GetComponentCount: Integer;
begin
  Result := 3;
end;

function TdxPDFLabColorSpace.GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass;
begin
  Result := nil;
end;

procedure TdxPDFLabColorSpace.Initialize;
begin
  inherited Initialize;
  FRangeA := TdxPDFRange.Create(-100, 100);
  FRangeB := TdxPDFRange.Create(-100, 100);
end;

procedure TdxPDFLabColorSpace.DoRead(AArray: TdxPDFArray);
begin
  Read(GetColorSpaceDictionary(AArray));
end;

procedure TdxPDFLabColorSpace.Read(ADictionary: TdxPDFReaderDictionary);
var
  AArray: TdxPDFArray;
  AMinA, AMaxA, AMinB, AMaxB: Double;
begin
  inherited Read(ADictionary);
  AArray := ADictionary.GetArray(TdxPDFKeywords.Range);
  if (AArray <> nil) and CanRead(AArray.Count) then
  begin
    AMinA := TdxPDFUtils.ConvertToDouble(AArray[0]);
    AMaxA := TdxPDFUtils.ConvertToDouble(AArray[1]);
    AMinB := TdxPDFUtils.ConvertToDouble(AArray[2]);
    AMaxB := TdxPDFUtils.ConvertToDouble(AArray[3]);
    if (AMaxA < AMinA) or (AMaxB < AMinB) then
      TdxPDFUtils.Abort;
    FRangeA := TdxPDFRange.Create(AMinA, AMaxA);
    FRangeB := TdxPDFRange.Create(AMinB, AMaxB);
  end;
end;

function TdxPDFLabColorSpace.CorrectRange(const ARange: TdxPDFRange; AValue: Double): Double;
begin
  Result := (AValue - ARange.Min) / (ARange.Max - ARange.Min) * 200 - 100;
end;

function TdxPDFLabColorSpace.GammaFunction(X: Double): Double;
begin
  if X >= 6.0 / 29.0 then
    Result := X * X * X
  else
    Result := X - 4.0 / 29.0 * 108.0 / 841.0;
end;

initialization
  dxPDFRegisterDocumentObjectClass(TdxPDFICCBasedColorSpace);
  dxPDFRegisterDocumentObjectClass(TdxPDFCMYKDeviceColorSpace);
  dxPDFRegisterDocumentObjectClass(TdxPDFRGBDeviceColorSpace);
  dxPDFRegisterDocumentObjectClass(TdxPDFGrayDeviceColorSpace);
  dxPDFRegisterDocumentObjectClass(TdxPDFPatternColorSpace);
  dxPDFRegisterDocumentObjectClass(TdxPDFSeparationColorSpace);
  dxPDFRegisterDocumentObjectClass(TdxPDFDeviceNColorSpace);
  dxPDFRegisterDocumentObjectClass(TdxPDFNChannelColorSpace);
  dxPDFRegisterDocumentObjectClass(TdxPDFIndexedColorSpace);
  dxPDFRegisterDocumentObjectClass('I', TdxPDFIndexedColorSpace);
  dxPDFRegisterDocumentObjectClass(TdxPDFCalRGBColorSpace);
  dxPDFRegisterDocumentObjectClass(TdxPDFCalGrayColorSpace);
  dxPDFRegisterDocumentObjectClass(TdxPDFLabColorSpace);
  dxPDFRegisterDocumentObjectClass('CMYK', TdxPDFCMYKDeviceColorSpace);
  dxPDFRegisterDocumentObjectClass('G', TdxPDFGrayDeviceColorSpace);
  dxPDFRegisterDocumentObjectClass('RGB', TdxPDFRGBDeviceColorSpace);

finalization
  dxPDFUnregisterDocumentObjectClass(TdxPDFLabColorSpace);
  dxPDFUnregisterDocumentObjectClass('RGB', TdxPDFRGBDeviceColorSpace);
  dxPDFUnregisterDocumentObjectClass('G', TdxPDFGrayDeviceColorSpace);
  dxPDFUnregisterDocumentObjectClass('CMYK', TdxPDFCMYKDeviceColorSpace);
  dxPDFUnregisterDocumentObjectClass(TdxPDFCalGrayColorSpace);
  dxPDFUnregisterDocumentObjectClass(TdxPDFCalRGBColorSpace);
  dxPDFUnregisterDocumentObjectClass('I', TdxPDFIndexedColorSpace);
  dxPDFUnregisterDocumentObjectClass(TdxPDFIndexedColorSpace);
  dxPDFUnregisterDocumentObjectClass(TdxPDFNChannelColorSpace);
  dxPDFUnregisterDocumentObjectClass(TdxPDFDeviceNColorSpace);
  dxPDFUnregisterDocumentObjectClass(TdxPDFSeparationColorSpace);
  dxPDFUnregisterDocumentObjectClass(TdxPDFPatternColorSpace);
  dxPDFUnregisterDocumentObjectClass(TdxPDFGrayDeviceColorSpace);
  dxPDFUnregisterDocumentObjectClass(TdxPDFRGBDeviceColorSpace);
  dxPDFUnregisterDocumentObjectClass(TdxPDFGrayDeviceColorSpace);
  dxPDFUnregisterDocumentObjectClass(TdxPDFCMYKDeviceColorSpace);
  dxPDFUnregisterDocumentObjectClass(TdxPDFICCBasedColorSpace);

end.

