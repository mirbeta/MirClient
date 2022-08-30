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

unit dxPDFUtils;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  SysUtils, Types, Math, Classes, Windows, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, cxClasses,
  cxGraphics, dxCoreGraphics, cxGeometry, dxGDIPlusClasses, dxPDFBase, dxPDFTypes;

type
  { TdxPDFUtils }

  TdxPDFUtils = class // for internal use
  public
    class procedure AddByte(B: Byte; var ADestinationData: TBytes);  static; inline;
    class procedure AddChar(AChar: Char; var ADestinationData: TCharArray); static; inline;
    class procedure AddData(const AData: TBytes; var ADestinationData: TBytes); overload; static; inline;
    class procedure AddData(const AData: TDoubleDynArray; var ADestinationData: TDoubleDynArray); overload; static; inline;
    class procedure AddData(const AData: TdxPDFRanges; var ADestinationData: TdxPDFRanges); overload; static; inline;
    class procedure AddRange(const ARange: TdxPDFRange; var ARanges: TdxPDFRanges); static;
    class procedure AddValue(AValue: Double; var AData: TDoubleDynArray); overload; static;
    class procedure AddValue(AValue: Integer; var AData: TIntegerDynArray); overload; static;
    class procedure AddValue(AValue: Single; var AData: TSingleDynArray); overload; static;
    class procedure ClearData(var AData: TBytes); static;
    class procedure CopyData(const AData: TDoubleDynArray; AIndex: Integer; var ADestinationData: TDoubleDynArray;
      ADestinationIndex, ACount: Integer); overload; static; inline;
    class procedure CopyData(const AData: TBytes; AIndex: Integer; var ADestinationData: TBytes;
      ADestinationIndex, ACount: Integer); overload; static; inline;
    class procedure CopyData(const AData: TIntegerDynArray; AIndex: Integer; var ADestinationData: TIntegerDynArray;
      ADestinationIndex, ACount: Integer); overload; static; inline;
    class procedure DeleteData(AIndex, ACount: Integer; var ASourceData: TBytes); static; inline;
    class procedure InsertFirst(AValue: Double; var AData: TDoubleDynArray); static;

    class function ArrayToRectF(AArray: TdxPDFArray): TdxRectF; static;
    class function ArrayToPointF(AArray: TdxPDFArray; AIndex: Integer): TdxPointF; static; inline;
    class function ArrayToMatrix(AArray: TdxPDFArray): TdxPDFTransformationMatrix; static; inline;
    class function CreateBoundingRectangle(const APoints: TdxPDFPoints): TdxRectF;
    class function Distance(const AStart, AEnd: TdxPointF): Single; static;
    class function DistanceToRect(const P: TPoint; const R: TdxRectF): Single; overload; static;
    class function DistanceToRect(const P: TdxPointF; const R: TdxRectF): Single; overload; static;
    class function DistanceToSegment(const APoint: TdxPointF; AStartX, AStartY, AEndX, AEndY: Single): Single; static;
    class function GenerateGUID: string; static;
    class function IsRectEmpty(const R: TdxRectF): Boolean; static; inline;
    class function Max(AValue1, AValue2: Double): Double; static; inline;
    class function LoadGlyph(const AResourceName, AResourceType: string): TdxSmartGlyph;
    class function Min(AValue1, AValue2: Double): Double; static; inline;
    class function MinMax(AValue, AMinValue, AMaxValue: Integer): Integer; static;
    class function NormalizeAngle(AAngle: Double): Double; static; inline;
    class function NormalizeRectF(const R: TdxRectF): TdxRectF; static; inline;
    class function NormalizeRotate(ARotate: Integer): Integer; static;
    class function OrientedDistance(const AStart, AFinish: TdxPointF; AAngle: Single): Single; static;
    class function RectContain(const ABounds, AInner: TdxRectF): Boolean; static; inline;
    class function RectIsEqual(const R1, R2: TdxRectF; ADelta: Single): Boolean; static;
    class function RectRotate(const R: TdxRectF): TdxRectF; static;
    class function RotatePoint(APoint: TdxPointF; AAngle: Single): TdxPointF; static;

    class function EndsWith(const S, AEndsWith: string): Boolean; static;
    class function StartsWith(const S, AStartsWith: string): Boolean; static;

    class function BytesToStr(const AData: TBytes): string; static; inline;
    class function ByteToHexDigit(AByte: Byte): Byte; static;

    class function FormatFileSize(AFileSize: Int64; AAuto: Boolean = True): string; static;

    class function IsDigit(AByte: Byte): Boolean; static; inline;
    class function IsDoubleValid(AValue: Double): Boolean; static; inline;
    class function IsIntegerValid(AValue: Integer): Boolean; static; inline;
    class function IsHexDigit(AByte: Byte): Boolean; static; inline;
    class function IsSpace(AByte: Byte): Boolean; static; inline;
    class function IsUnicode(const AData: TBytes): Boolean; static; inline;
    class function IsWhiteSpace(AByte: Byte): Boolean; static; inline;

    class function StrToByteArray(const S: string): TBytes; static;

    class function Contains(const R1, R2: TdxRectF): Boolean; static; inline;
    class function Intersects(const R1, R2: TdxRectF): Boolean; overload; static;
    class function Intersects(out AIntersection: TdxRectF; const R1, R2: TdxRectF): Boolean; overload; static;
    class function PtInRect(const R: TdxRectF; const P: TdxPointF): Boolean; static; inline;
    class function Subtract(const P1, P2: TdxPointF): TdxPointF; static;
    class function TrimRect(const R1, R2: TdxRectF): TdxRectF; static;

    class function ConvertToAlphaColor(AColor: TdxPDFColor; AAlpha: Double): TdxAlphaColor; static; inline;
    class function ConvertToByte(AValue: Double): Byte; static;
    class function ConvertToBoolean(AValue: TdxPDFReferencedObject; ADefaultValue: Boolean): Boolean; static;
    class function ConvertToDigit(AValue: Byte): Byte; inline; static;
    class function ConvertToDouble(AValue: TdxPDFBase): Double; overload; static; inline;
    class function ConvertToDouble(AValue: TdxPDFReferencedObject; ADefaultValue: Double): Double; overload; static; inline;
    class function ConvertToInt(AValue: Double): Integer; overload; static; inline;
    class function ConvertToInt(AValue: TdxPDFReferencedObject; ADefaultValue: Integer): Integer; overload; static; inline;
    class function ConvertToIntEx(AValue: TcxRotationAngle): Integer; static;
    class function ConvertToGpMatrix(AMatrix: TdxPDFTransformationMatrix): TdxGPMatrix; static;
    class function ConvertToGpPixelFormat(AFormat: TdxPDFPixelFormat): Integer; static;
    class function ConvertToPageRanges(const APageIndexes: TIntegerDynArray): string; static;
    class function ConvertToSingle(AValue: TdxPDFBase): Single; static;
    class function ConvertToStr(const AData: TBytes): string; overload; static;
    class function ConvertToStr(const AData: TBytes; ALength: Integer): string; overload; static;
    class function ConvertToStr(AValue: TdxPDFReferencedObject): string; overload; static;
    class function ConvertToStr(AValue: TdxPDFBase): string; overload; static;
    class function ConvertToText(const AData: TBytes): string; static;
    class function ConvertToUnicode(const AData: TBytes): string; static;
    class function Split(const S: string; const ADelimiters: array of string): TArray<string>; static;

    class function ConvertToASCIIString(const AData: TBytes): string; static;
    class function ConvertToUTF8String(const AData: TBytes): string; static;
    class function ConvertToBigEndianUnicode(const AData: TBytes; AByteIndex, ACharCount: Integer): string; static;

    class procedure Abort; static;
    class procedure SaveToFile(const AFileName: string; const AData: TBytes); static;
    class procedure RaiseException(const AMessage: string = ''; AExceptionClass: EdxPDFExceptionClass = nil); static;
    class procedure RaiseTestException(const AMessage: string = ''); static;
  end;

implementation

uses
  SysConst, Character, dxStringHelper, dxPDFDocumentStrs, dxPDFViewerDialogsStrs, dxJPX;

const
  dxsHexUnicodeStringPrefix = 'FEFF';

{ TdxPDFUtils }

class procedure TdxPDFUtils.AddByte(B: Byte; var ADestinationData: TBytes);
var
  L: Integer;
begin
  L := Length(ADestinationData);
  SetLength(ADestinationData, L + 1);
  ADestinationData[L] := B;
end;

class procedure TdxPDFUtils.AddChar(AChar: Char; var ADestinationData: TCharArray);
var
  L: Integer;
begin
  L := Length(ADestinationData);
  SetLength(ADestinationData, L + 1);
  ADestinationData[L] := AChar;
end;

class procedure TdxPDFUtils.AddRange(const ARange: TdxPDFRange; var ARanges: TdxPDFRanges);
var
  L: Integer;
begin
  L := Length(ARanges);
  SetLength(ARanges, L + 1);
  ARanges[L] := ARange;
end;

class procedure TdxPDFUtils.AddData(const AData: TBytes; var ADestinationData: TBytes);
var
  ADestLength, ASrcLength: Integer;
begin
  ASrcLength := Length(AData);
  ADestLength := Length(ADestinationData);
  SetLength(ADestinationData, ASrcLength + ADestLength);
  cxCopyData(@AData[0], @ADestinationData[0], 0, ADestLength, ASrcLength * SizeOf(Byte));
end;

class procedure TdxPDFUtils.AddData(const AData: TDoubleDynArray; var ADestinationData: TDoubleDynArray);
var
  ADestLength, ASrcLength: Integer;
begin
  ASrcLength := Length(AData);
  ADestLength := Length(ADestinationData);
  SetLength(ADestinationData, ASrcLength + ADestLength);
  cxCopyData(@AData[0], @ADestinationData[0], 0, ADestLength, ASrcLength * SizeOf(Double));
end;

class procedure TdxPDFUtils.AddData(const AData: TdxPDFRanges; var ADestinationData: TdxPDFRanges);
var
  ADestLength, ASrcLength: Integer;
begin
  ASrcLength := Length(AData);
  ADestLength := Length(ADestinationData);
  SetLength(ADestinationData, ASrcLength + ADestLength);
  cxCopyData(@AData[0], @ADestinationData[0], 0, ADestLength, ASrcLength * SizeOf(TdxPDFRange));
end;

class procedure TdxPDFUtils.AddValue(AValue: Double; var AData: TDoubleDynArray);
var
  L: Integer;
begin
  L := Length(AData);
  SetLength(AData, L + 1);
  AData[L] := AValue;
end;

class procedure TdxPDFUtils.CopyData(const AData: TDoubleDynArray; AIndex: Integer;
  var ADestinationData: TDoubleDynArray; ADestinationIndex, ACount: Integer);
begin
  cxCopyData(@AData[0], @ADestinationData[0], AIndex, ADestinationIndex * SizeOf(Double), ACount * SizeOf(Double));
end;

class procedure TdxPDFUtils.AddValue(AValue: Integer; var AData: TIntegerDynArray);
var
  L: Integer;
begin
  L := Length(AData);
  SetLength(AData, L + 1);
  AData[L] := AValue;
end;

class procedure TdxPDFUtils.AddValue(AValue: Single; var AData: TSingleDynArray);
var
  L: Integer;
begin
  L := Length(AData);
  SetLength(AData, L + 1);
  AData[L] := AValue;
end;

class procedure TdxPDFUtils.ClearData(var AData: TBytes);
begin
  FillChar(AData[0], Length(AData), 0);
end;

class procedure TdxPDFUtils.CopyData(const AData: TBytes; AIndex: Integer;
  var ADestinationData: TBytes; ADestinationIndex, ACount: Integer);
begin
  cxCopyData(@AData[0], @ADestinationData[0], AIndex, ADestinationIndex, ACount);
end;

class procedure TdxPDFUtils.CopyData(const AData: TIntegerDynArray; AIndex: Integer;
  var ADestinationData: TIntegerDynArray; ADestinationIndex, ACount: Integer);
begin
  cxCopyData(@AData[0], @ADestinationData[0], AIndex, ADestinationIndex * SizeOf(Integer), ACount * SizeOf(Integer));
end;

class procedure TdxPDFUtils.DeleteData(AIndex, ACount: Integer; var ASourceData: TBytes);
var
  I, L: Integer;
begin
  L := Length(ASourceData);
  if (L > 0) and (ACount > 0) and (ACount <= L - AIndex) and (AIndex < L) then
    for I := AIndex + ACount to L - 1 do
      ASourceData[I - ACount] := ASourceData[I];
  SetLength(ASourceData, L - ACount);
end;

class procedure TdxPDFUtils.InsertFirst(AValue: Double; var AData: TDoubleDynArray);
var
  L: Integer;
begin
  L := Length(AData);
  SetLength(AData, L + 1);
  TdxPDFUtils.CopyData(AData, 0, AData, 1, L);
  AData[0] := AValue;
end;

class function TdxPDFUtils.ArrayToRectF(AArray: TdxPDFArray): TdxRectF;
begin
  if (AArray <> nil) and (AArray.Count >= 4) then
  begin
    Result.Left := TdxPDFDouble(AArray[0]).Value;
    Result.Bottom := TdxPDFDouble(AArray[1]).Value;
    Result.Right := TdxPDFDouble(AArray[2]).Value;
    Result.Top := TdxPDFDouble(AArray[3]).Value;
  end
  else
    Result := dxRectF(cxNullRect);
end;

class function TdxPDFUtils.ArrayToPointF(AArray: TdxPDFArray; AIndex: Integer): TdxPointF;
begin
  if AArray <> nil then
  begin
    Result.X := ConvertToDouble(AArray[AIndex]);
    Result.Y := ConvertToDouble(AArray[AIndex + 1]);
  end
  else
    Result := dxPointF(cxNullPoint);
end;

class function TdxPDFUtils.ArrayToMatrix(AArray: TdxPDFArray): TdxPDFTransformationMatrix;
begin
  Result := TdxPDFTransformationMatrix.Create;
  if AArray <> nil then
  begin
    if AArray.Count <> 6 then
      RaiseException('Incorrect matrix array size');
    Result.Assign(ConvertToDouble(AArray[0]), ConvertToDouble(AArray[1]),
      ConvertToDouble(AArray[2]), ConvertToDouble(AArray[3]),
      ConvertToDouble(AArray[4]), ConvertToDouble(AArray[5]));
  end;
end;

class function TdxPDFUtils.CreateBoundingRectangle(const APoints: TdxPDFPoints): TdxRectF;
var
  ACount, I: Integer;
  APoint: TdxPointF;
  AXMin, AXMax, AYMin, AYMax, X, Y: Double;
begin
  ACount := Length(APoints);
  if ACount = 0 then
    Exit(dxRectF(cxNullRect));
  APoint := APoints[0];
  AXMin := APoint.X;
  AXMax := AXMin;
  AYMin := APoint.Y;
  AYMax := AYMin;
  for I := 1 to ACount - 1 do
  begin
    APoint := APoints[I];
    X := APoint.X;
    if X < AXMin then
      AXMin := X
    else
      if X > AXMax then
        AXMax := X;
    Y := APoint.Y;
    if Y < AYMin then
      AYMin := Y
    else
      if Y > AYMax then
        AYMax := Y;
  end;
  Result := dxRectF(AXMin, AYMax, AXMax, AYMin);
end;

class function TdxPDFUtils.Distance(const AStart, AEnd: TdxPointF): Single;
var
  ADx, ADy: Single;
begin
  ADx := AEnd.X - AStart.X;
  ADy := AEnd.Y - AStart.Y;
  Result := Sqrt(ADx * ADx + ADy * ADy);
end;

class function TdxPDFUtils.DistanceToRect(const P: TPoint; const R: TdxRectF): Single;
begin
  Result := DistanceToRect(dxPointF(P), R);
end;

class function TdxPDFUtils.DistanceToRect(const P: TdxPointF; const R: TdxRectF): Single;
var
  ALeft, ABottom, ARight, ATop: Double;
begin
  ALeft := R.Left;
  ABottom := R.Bottom;
  ARight := R.Right;
  ATop := R.Top;
  Result := Math.Min(
    Math.Min(DistanceToSegment(P, ALeft, ATop, ARight, ATop), DistanceToSegment(P, ARight, ATop, ARight, ABottom)),
    Math.Min(DistanceToSegment(P, ARight, ABottom, ALeft, ABottom), DistanceToSegment(P, ALeft, ABottom, ALeft, ATop)));
end;

class function TdxPDFUtils.DistanceToSegment(const APoint: TdxPointF; AStartX, AStartY, AEndX, AEndY: Single): Single;
var
  ADx, ADy, AC1, AC2, B: Double;
begin
  ADx := AEndX - AStartX;
  ADy := AEndY - AStartY;
  AC1 := ADx * (APoint.X - AStartX) + ADy * (APoint.Y - AStartY);
  if AC1 <= 0 then
    Exit(Distance(APoint, dxPointF(AStartX, AStartY)));
  AC2 := ADx * ADx + ADy * ADy;
  if AC2 <= AC1 then
    Exit(Distance(APoint, dxPointF(AEndX, AEndY)));
  B := AC1 / AC2;
  Result := Distance(APoint, dxPointF(AStartX + ADx * B, AStartY + ADy * B));
end;

class function TdxPDFUtils.GenerateGUID: string;
begin
  Result := dxGenerateGUID;
  Result := Copy(Result, 2, Length(Result) - 2);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
  Result := Copy(Result, 1, 31);
end;

class function TdxPDFUtils.IsRectEmpty(const R: TdxRectF): Boolean;
begin
  Result := R = cxRectF(cxNullRect);
end;

class function TdxPDFUtils.Max(AValue1, AValue2: Double): Double;
begin
  Result := IfThen(AValue1 > AValue2, AValue1, AValue2);
end;

class function TdxPDFUtils.LoadGlyph(const AResourceName, AResourceType: string): TdxSmartGlyph;
begin
  Result := TdxSmartGlyph.Create;
  Result.LoadFromResource(HInstance, AResourceName, PWideChar(AResourceType));
  Result.SourceDPI := 96;
end;

class function TdxPDFUtils.Min(AValue1, AValue2: Double): Double;
begin
  Result := IfThen(AValue1 < AValue2, AValue1, AValue2);
end;

class function TdxPDFUtils.MinMax(AValue, AMinValue, AMaxValue: Integer): Integer;
begin
  if AMaxValue >= AMinValue then
    Result := Math.Min(AMaxValue, Math.Max(AValue, AMinValue))
  else
    Result := AMaxValue;
end;

class function TdxPDFUtils.NormalizeAngle(AAngle: Double): Double;
var
  AResult: Double;
begin
  AResult := AAngle;
  while AResult < 0 do
		AResult := AResult + PI * 2;
  while AResult >= PI * 2 do
		AResult := AResult - PI * 2;
  Result := AResult;
end;

class function TdxPDFUtils.NormalizeRectF(const R: TdxRectF): TdxRectF;
begin
  Result.Left := R.Left;
  Result.Top :=  R.Bottom;
  Result.Right := R.Right;
  Result.Bottom := R.Top;
end;

class function TdxPDFUtils.NormalizeRotate(ARotate: Integer): Integer;
var
  AStep: Integer;
begin
  AStep := -360;
  if ARotate < 0 then
    AStep := 360;
  while (((ARotate < 0) and (AStep > 0))) or (((ARotate > 270) and (AStep < 0))) do
    Inc(ARotate, AStep);
  Result := ARotate;
end;

class function TdxPDFUtils.OrientedDistance(const AStart, AFinish: TdxPointF; AAngle: Single): Single;
begin
  Result := AFinish.X - AStart.X;
  if AAngle <> 0 then
    Result := Result * Cos(-AAngle) - (AFinish.Y - AStart.Y) * Sin(-AAngle);
end;

class function TdxPDFUtils.RectContain(const ABounds, AInner: TdxRectF): Boolean;
begin
  with ABounds do
    Result := (Left <= AInner.Left) and (Right >= AInner.Right) and
      (Top <= AInner.Top) and (Bottom >= AInner.Bottom);
end;

class function TdxPDFUtils.RectIsEqual(const R1, R2: TdxRectF; ADelta: Single): Boolean;

  function CheckValue(A, B, ADelta: Single): Boolean;
  begin
    Result := Abs(A - b) <= ADelta;
  end;

begin
  Result := CheckValue(R1.Left, R2.Left, ADelta) and CheckValue(R1.Right, R2.Right, ADelta) and
    CheckValue(R1.Top, R2.Top, ADelta) and CheckValue(R1.Bottom, R2.Bottom, ADelta);
end;

class function TdxPDFUtils.RectRotate(const R: TdxRectF): TdxRectF;
begin
  Result := dxRectF(R.Top, R.Left, R.Bottom, R.Right);
end;

class function TdxPDFUtils.RotatePoint(APoint: TdxPointF; AAngle: Single): TdxPointF;
var
  ASin, ACos: Single;
begin
  if AAngle <> 0 then
  begin
    ASin := Sin(AAngle);
    ACos := Cos(AAngle);
    Result := dxPointF(APoint.X * ACos - APoint.Y * ASin, APoint.X * ASin + APoint.Y * ACos);
  end
  else
    Result := APoint;
end;

class function TdxPDFUtils.EndsWith(const S, AEndsWith: string): Boolean;
var
  L: Integer;
begin
  L := Length(AEndsWith);
  Result := (AEndsWith <> '') and (Length(S) >= L) and (Copy(S, Length(S) - L + 1, L) = AEndsWith);
end;

class function TdxPDFUtils.StartsWith(const S, AStartsWith: string): Boolean;
var
  L: Integer;
begin
  L := Length(AStartsWith);
  Result := (AStartsWith <> '') and (Length(S) >= L) and (Copy(S, 1, L) = AStartsWith);
end;

class function TdxPDFUtils.BytesToStr(const AData: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(AData) to High(AData) do
    Result := Result + Char(AData[I]);
end;

class function TdxPDFUtils.ByteToHexDigit(AByte: Byte): Byte;
begin
  if (AByte >= TdxPDFDefinedSymbols.DigitStart) and (AByte <= TdxPDFDefinedSymbols.DigitEnd) then
    Exit(Byte((AByte - TdxPDFDefinedSymbols.DigitStart)));
  if (AByte >= TdxPDFDefinedSymbols.HexDigitStart) and (AByte <= TdxPDFDefinedSymbols.HexDigitEnd) then
    Exit(Byte((AByte - TdxPDFDefinedSymbols.HexDigitStart + 10)));
  if (AByte < TdxPDFDefinedSymbols.LowercaseHexDigitStart) or (AByte > TdxPDFDefinedSymbols.LowercaseHexDigitEnd) then
    TdxPDFUtils.Abort;
  Result := Byte((AByte - TdxPDFDefinedSymbols.LowercaseHexDigitStart + 10));
end;

class function TdxPDFUtils.FormatFileSize(AFileSize: Int64; AAuto: Boolean = True): string;

  function GetInBytes(const ATemplate: string): string;
  begin
    Result := FormatFloat(ATemplate, AFileSize) + ' ' + cxGetResourceString(@sdxPDFViewerBytes);
  end;

const
  FormatTemplate = '##0.##';
  KiloByte = 1024;
  MegaByte = KiloByte * KiloByte;
  GigaByte = KiloByte * MegaByte;
begin
  if AAuto then
  begin
    if AFileSize > GigaByte then
      Result := FormatFloat(FormatTemplate, AFileSize / GigaByte) + ' ' +
        cxGetResourceString(@sdxPDFViewerGigaBytes)
    else
      if AFileSize > MegaByte then
        Result := FormatFloat(FormatTemplate, AFileSize / MegaByte) + ' ' +
          cxGetResourceString(@sdxPDFViewerMegaBytes)
      else
        if AFileSize > KiloByte then
          Result := FormatFloat(FormatTemplate, AFileSize / KiloByte) + ' ' +
          cxGetResourceString(@sdxPDFViewerKiloBytes)
        else
          Result := GetInBytes(FormatTemplate)
  end
  else
    Result := GetInBytes('##0,###');
end;

class function TdxPDFUtils.IsDigit(AByte: Byte): Boolean;
begin
  Result := (AByte >= TdxPDFDefinedSymbols.DigitStart) and (AByte <= TdxPDFDefinedSymbols.DigitEnd);
end;

class function TdxPDFUtils.IsDoubleValid(AValue: Double): Boolean;
begin
  Result := dxPDFIsDoubleValid(AValue);
end;

class function TdxPDFUtils.IsIntegerValid(AValue: Integer): Boolean;
begin
  Result := IsDoubleValid(AValue);
end;

class function TdxPDFUtils.IsHexDigit(AByte: Byte): Boolean;
begin
  Result := ((AByte >= TdxPDFDefinedSymbols.DigitStart) and (AByte <= TdxPDFDefinedSymbols.DigitEnd)) or
    ((AByte >= TdxPDFDefinedSymbols.HexDigitStart) and (AByte <= TdxPDFDefinedSymbols.HexDigitEnd)) or
    ((AByte >= TdxPDFDefinedSymbols.LowercaseHexDigitStart) and (AByte <= TdxPDFDefinedSymbols.LowercaseHexDigitEnd));
end;

class function TdxPDFUtils.IsSpace(AByte: Byte): Boolean;
begin
  Result := AByte = TdxPDFDefinedSymbols.Space;
end;

class function TdxPDFUtils.IsUnicode(const AData: TBytes): Boolean;
begin
  Result := (Length(AData) >= 2) and (AData[0] = 254) and (AData[1] = 255);
end;

class function TdxPDFUtils.IsWhiteSpace(AByte: Byte): Boolean;
begin
  Result := IsSpace(AByte) or (AByte in [TdxPDFDefinedSymbols.CarriageReturn, TdxPDFDefinedSymbols.Null,
    TdxPDFDefinedSymbols.LineFeed, TdxPDFDefinedSymbols.FormFeed, TdxPDFDefinedSymbols.HorizontalTab]);
end;

class function TdxPDFUtils.StrToByteArray(const S: string): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
    Result[I - 1] := Byte(S[I]);
end;

class function TdxPDFUtils.Contains(const R1, R2: TdxRectF): Boolean;
begin
  Result := (R1.Left <= R2.left) and (R1.Right >= R2.Right) and (R1.Bottom <= R2.Bottom) and (R1.Top >= R2.Top);
end;

class function TdxPDFUtils.Intersects(const R1, R2: TdxRectF): Boolean;
var
  R: TdxRectF;
begin
  Result := Intersects(R, R1, R2);
end;

class function TdxPDFUtils.Intersects(out AIntersection: TdxRectF; const R1, R2: TdxRectF): Boolean;
begin
  AIntersection.Left := Max(R2.Left, R1.Left);
  AIntersection.Top := Max(R2.Top, R1.Top);
  AIntersection.Right := Min(R2.Right, R1.Right);
  AIntersection.Bottom := Min(R2.Bottom, R1.Bottom);
  Result := not ((AIntersection.Right <= AIntersection.Left) or (AIntersection.Bottom <= AIntersection.Top));
end;

class function TdxPDFUtils.PtInRect(const R: TdxRectF; const P: TdxPointF): Boolean;
begin
  Result := (P.X >= R.Left) and (P.X < R.Right) and (P.Y >= R.Top) and (P.Y < R.Bottom);
end;

class function TdxPDFUtils.Subtract(const P1, P2: TdxPointF): TdxPointF;
begin
  Result := cxPointOffset(P1, P2);
end;

class function TdxPDFUtils.TrimRect(const R1, R2: TdxRectF): TdxRectF;
begin
  Result.Left := Max(R1.Left, R2.left);
  Result.Bottom := Max(R1.Bottom, R2.Bottom);
  Result.Right := Min(R1.Right, R2.Right);
  Result.Top := Min(R1.Top, R2.Top);
end;

class function TdxPDFUtils.ConvertToByte(AValue: Double): Byte;
begin
  Result := dxDoubleToByte(AValue);
end;

class function TdxPDFUtils.ConvertToBoolean(AValue: TdxPDFReferencedObject; ADefaultValue: Boolean): Boolean;
begin
  Result := ADefaultValue;
  if (AValue is TdxPDFBase) and (AValue is TdxPDFBoolean) then
    Result := TdxPDFBoolean(AValue).Value;
end;

class function TdxPDFUtils.ConvertToDigit(AValue: Byte): Byte;
begin
  Result := 0;
  if InRange(AValue, TdxPDFDefinedSymbols.DigitStart, TdxPDFDefinedSymbols.DigitEnd) then
    Result := Byte(AValue - TdxPDFDefinedSymbols.DigitStart)
  else
    RaiseTestException('ConvertToDigit');
end;

class function TdxPDFUtils.ConvertToDouble(AValue: TdxPDFBase): Double;
begin
  case AValue.ObjectType of
    otDouble:
      Result := TdxPDFDouble(AValue).Value;
    otInteger:
      Result := TdxPDFInteger(AValue).Value;
  else
    begin
      RaiseTestException('ConvertToDouble');
      Result := dxPDFInvalidValue;
    end;
  end;
end;

class function TdxPDFUtils.ConvertToDouble(AValue: TdxPDFReferencedObject; ADefaultValue: Double): Double;
begin
  Result := ADefaultValue;
  if AValue is TdxPDFBase then
    Result := ConvertToDouble(TdxPDFBase(AValue));
end;

class function TdxPDFUtils.ConvertToInt(AValue: Double): Integer;
begin
  Result := Round(AValue + 0.5);
end;

class function TdxPDFUtils.ConvertToInt(AValue: TdxPDFReferencedObject; ADefaultValue: Integer): Integer;
begin
  Result := ADefaultValue;
  if (AValue is TdxPDFBase) and (AValue is TdxPDFInteger) then
    Result := TdxPDFInteger(AValue).Value;
end;

class function TdxPDFUtils.ConvertToIntEx(AValue: TcxRotationAngle): Integer;
const
  AngleMap: array[TcxRotationAngle] of Integer = (0, 90, -90, 180);
begin
  Result := AngleMap[AValue];
end;

class function TdxPDFUtils.ConvertToGpMatrix(AMatrix: TdxPDFTransformationMatrix): TdxGPMatrix;
begin
  Result := TdxGPMatrix.CreateEx(AMatrix.A, AMatrix.B, AMatrix.C, AMatrix.D, AMatrix.E, AMatrix.F);
end;

class function TdxPDFUtils.ConvertToGpPixelFormat(AFormat: TdxPDFPixelFormat): Integer;
begin
  case AFormat of
    pfGray1bit:
      Result := 196865;
    pfGray8bit:
      Result := 198659;
    pfArgb24bpp:
      Result := 137224;
    pfArgb32bpp:
      Result := 2498570
  else
    Result := 137224;
  end;
end;

class function TdxPDFUtils.ConvertToPageRanges(const APageIndexes: TIntegerDynArray): string;
var
  I, APageCount, AIndex, APrevIndex, ADifference: Integer;
  AStringBuilder: TStringBuilder;
begin
  APageCount := Length(APageIndexes);
  if APageCount > 0 then
  begin
    AStringBuilder := TStringBuilder.Create;
    try
      APrevIndex := APageIndexes[0];
      AStringBuilder.Append(APrevIndex);
      I := 1;
      while I < APageCount do
      begin
        AIndex := APageIndexes[I];
        ADifference := AIndex - APrevIndex;
        if Abs(ADifference) = 1 then
        begin
          APrevIndex := AIndex;
          Inc(I);
          while I < APageCount do
          begin
            AIndex := APageIndexes[I];
            if AIndex - APrevIndex <> ADifference then
              Break;
            APrevIndex := AIndex;
            Inc(I);
          end;
          AStringBuilder.Append('-');
          AStringBuilder.Append(APrevIndex);
          if I = APageCount then
            Break;
        end;
        AStringBuilder.Append(',');
        AStringBuilder.Append(AIndex);
        APrevIndex := AIndex;
        Inc(I);
      end;
      Result := AStringBuilder.ToString;
    finally
      AStringBuilder.Free;
    end;
  end;
end;

class function TdxPDFUtils.ConvertToSingle(AValue: TdxPDFBase): Single;
begin
  if (AValue = nil) or (AValue.ObjectType = otNull) then
    Result := dxPDFInvalidValue
  else
   Result := ConvertToDouble(AValue);
end;

class function TdxPDFUtils.ConvertToStr(const AData: TBytes): string;
var
  AEncoding: TEncoding;
begin
  if IsUnicode(AData) then
    Result := ConvertToUnicode(AData)
  else
  begin
    AEncoding := TEncoding.GetEncoding(1252);
    try
      Result := AEncoding.GetString(AData);
    finally
      AEncoding.Free;
    end;
  end;
end;

class function TdxPDFUtils.ConvertToStr(const AData: TBytes; ALength: Integer): string;
begin
  try
    Result := TdxStringHelper.Substring(ConvertToBigEndianUnicode(AData, 0 , ALength), 1);
  except
    Result := '';
  end;
end;

class function TdxPDFUtils.ConvertToStr(AValue: TdxPDFReferencedObject): string;
begin
  Result := ConvertToStr(AValue as TdxPDFBase);
end;

class function TdxPDFUtils.ConvertToStr(AValue: TdxPDFBase): string;
begin
  Result := '';
  if (AValue is TdxPDFBase) and (AValue is TdxPDFString) then
    Result := TdxPDFString(AValue).Value;
end;

class function TdxPDFUtils.ConvertToText(const AData: TBytes): string;

  function GetString(const AData: TBytes): string;
  const
    Map: array[0..255] of Char = (#$0000, #$0001, #$0002, #$0003, #$0004, #$0005, #$0006, #$0007, #$0008, #$0009,
      #$000A, #$000B, #$000C, #$000D, #$000E, #$000F, #$0010, #$0011, #$0012, #$0013, #$0014, #$0015, #$0017, #$0017,
      #$02D8, #$02C7, #$02C6, #$02D9, #$02DD, #$02DB, #$02DA, #$02DC, #$0020, #$0021, #$0022, #$0023, #$0024, #$0025,
      #$0026, #$0027, #$0028, #$0029, #$002A, #$002B, #$002C, #$002D, #$002E, #$002F, #$0030, #$0031, #$0032, #$0033,
      #$0034, #$0035, #$0036, #$0037, #$0038, #$0039, #$003A, #$003B, #$003C, #$003D, #$003E, #$003F, #$0040, #$0041,
      #$0042, #$0043, #$0044, #$0045, #$0046, #$0047, #$0048, #$0049, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F,
      #$0050, #$0051, #$0052, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058, #$0059, #$005A, #$005B, #$005C, #$005D,
      #$005E, #$005F, #$0060, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, #$0068, #$0069, #$006A, #$006B,
      #$006C, #$006D, #$006E, #$006F, #$0070, #$0071, #$0072, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078, #$0079,
      #$007A, #$007B, #$007C, #$007D, #$007E, #$002D, #$2022, #$2020, #$2021, #$2026, #$2014, #$2013, #$0192, #$2044,
      #$2039, #$203A, #$2212, #$2030, #$201E, #$201C, #$201D, #$2018, #$2019, #$201A, #$2122, #$FB01, #$FB02, #$0141,
      #$0152, #$0160, #$0178, #$017D, #$0131, #$0142, #$0153, #$0161, #$017E, #$002D, #$20AC, #$00A1, #$00A2, #$00A3,
      #$00A4, #$00A5, #$00A6, #$00A7, #$00A8, #$00A9, #$00AA, #$00AB, #$00AC, #$002D, #$00AE, #$00AF, #$00B0, #$00B1,
      #$00B2, #$00B3, #$00B4, #$00B5, #$00B6, #$00B7, #$00B8, #$00B9, #$00BA, #$00BB, #$00BC, #$00BD, #$00BE, #$00BF,
      #$00C0, #$00C1, #$00C2, #$00C3, #$00C4, #$00C5, #$00C6, #$00C7, #$00C8, #$00C9, #$00CA, #$00CB, #$00CC, #$00CD,
      #$00CE, #$00CF, #$00D0, #$00D1, #$00D2, #$00D3, #$00D4, #$00D5, #$00D6, #$00D7, #$00D8, #$00D9, #$00DA, #$00DB,
      #$00DC, #$00DD, #$00DE, #$00DF, #$00E0, #$00E1, #$00E2, #$00E3, #$00E4, #$00E5, #$00E6, #$00E7, #$00E8, #$00E9,
      #$00EA, #$00EB, #$00EC, #$00ED, #$00EE, #$00EF, #$00F0, #$00F1, #$00F2, #$00F3, #$00F4, #$00F5, #$00F6, #$00F7,
      #$00F8, #$00F9, #$00FA, #$00FB, #$00FC, #$00FD, #$00FE, #$00FF);
  var
    I: Integer;
    ABuilder: TStringBuilder;
  begin
    ABuilder := TStringBuilder.Create;
    try
      for I := 0 to Length(AData) - 1 do
        ABuilder.Append(Map[AData[I]]);
      Result := ABuilder.ToString;
    finally
      ABuilder.Free;
    end;
  end;

begin
  if IsUnicode(AData) then
    Result := ConvertToUnicode(AData)
  else
    Result := GetString(AData);
end;

class function TdxPDFUtils.ConvertToUnicode(const AData: TBytes): string;
begin
  Result := ConvertToStr(AData, Length(AData));
end;

class function TdxPDFUtils.Split(const S: string; const ADelimiters: array of string): TArray<string>;
begin
  Result := TdxStringHelper.Split(S, ADelimiters);
end;

class function TdxPDFUtils.ConvertToASCIIString(const AData: TBytes): string;
begin
  try
    Result := TEncoding.ASCII.GetString(AData);
  except
    Result := '';
  end;
end;

class function TdxPDFUtils.ConvertToUTF8String(const AData: TBytes): string;
begin
  try
    Result := TEncoding.UTF8.GetString(AData);
  except
    Result := '';
  end;
end;

class function TdxPDFUtils.ConvertToBigEndianUnicode(const AData: TBytes; AByteIndex, ACharCount: Integer): string;
begin
  try
    Result := TEncoding.BigEndianUnicode.GetString(AData, AByteIndex, ACharCount);
  except
    Result := '';
  end;
end;

class function TdxPDFUtils.ConvertToAlphaColor(AColor: TdxPDFColor; AAlpha: Double): TdxAlphaColor;
var
  ARGBColor: TdxPDFARGBColor;
  A, R, G, B: Byte;
begin
  ARGBColor := TdxPDFARGBColor.Create(AColor);
  try
    if AColor = nil then
      A := 255
    else
      A := Trunc(AAlpha * 255);
    R := ConvertToByte(ARGBColor.Red * 255);
    G := ConvertToByte(ARGBColor.Green * 255);
    B := ConvertToByte(ARGBColor.Blue * 255);
    Result := TdxAlphaColors.FromArgb(A, R, G, B);
  finally
    ARGBColor.Free;
  end;
end;

class procedure TdxPDFUtils.SaveToFile(const AFileName: string; const AData: TBytes);
var
  AFileStream: TFileStream;
begin
  if FileExists(AFileName) then
    DeleteFile(PWideChar(AFileName));
  AFileStream := TFileStream.Create(AFileName, fmCreate);
  try
    AFileStream.WriteBuffer(AData[0], Length(AData));
  finally
    AFileStream.Free;
  end;
end;

class procedure TdxPDFUtils.Abort;
begin
  raise EdxPDFAbortException.Create(cxGetResourceString(@sdxPDFDocumentInvalidFormatMessage));
end;

class procedure TdxPDFUtils.RaiseException(const AMessage: string = ''; AExceptionClass: EdxPDFExceptionClass = nil);
var
  S: string;
begin
  if SameText(AMessage, '') then
    S := cxGetResourceString(@sdxPDFDocumentInvalidFormatMessage)
  else
    S := AMessage;
  if AExceptionClass = nil then
    raise EdxPDFException.Create(S)
  else
    raise AExceptionClass.Create(S);
end;

class procedure TdxPDFUtils.RaiseTestException(const AMessage: string = '');
begin
  dxTestCheck(False, AMessage);
end;

end.

