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
unit dxRichEdit.Import.Doc.BorderDescriptor;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.Doc.Utils;

type

  TdxDocBorderLineStyle = (
    None = $00,
    Single = $01,
    Double = $03,
    ThinSingle = $05,
    Dotted = $06,
    Dashed = $07,
    DotDash = $08,
    DotDotDash = $09,
    Triple = $0A,
    ThinThickSmallGap = $0B,
    ThickThinSmallGap = $0C,
    ThinThickThinSmallGap = $0D,
    ThinThickMediumGap = $0E,
    ThickThinMediumGap = $0F,
    ThinThickThinMediumGap = $10,
    ThinThickLargeGap = $11,
    ThickThinLargeGap = $12,
    ThinThickThinLargeGap = $13,
    Wave = $14,
    DoubleWave = $15);

  { TdxBorderDescriptor97 }

  TdxBorderDescriptor97 = class
  public const
    BorderDescriptorSize: Byte = 4;
    EmptyBorder: Cardinal = $FFFFFFFF;
  strict private
    FWidth: Byte;
    FStyle: TdxDocBorderLineStyle;
    FBorderColor: TdxAlphaColor;
    FDistance: Integer;
    FNotUseBorder: Boolean;
  protected
    procedure Read(AReader: TBinaryReader); overload;
    procedure Read(const AData: TBytes; AStartIndex: Integer); overload;
  public
    class function FromStream(AReader: TBinaryReader): TdxBorderDescriptor97; static;
    class function FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxBorderDescriptor97; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure ConvertFromBorderInfo(AInfo: TdxBorderInfo; AUnitConverter: TdxDocumentModelUnitConverter);
    procedure ApplyProperties(ADestination: TdxBorderBase; AUnitConverter: TdxDocumentModelUnitConverter);
    function Clone: TdxBorderDescriptor97;

    property Width: Byte read FWidth;
    property Style: TdxDocBorderLineStyle read FStyle;
    property BorderColor: TdxAlphaColor read FBorderColor;
    property Distance: Integer read FDistance;
    property NotUseBorder: Boolean read FNotUseBorder write FNotUseBorder;
  end;

  { TdxBorderDescriptor }

  TdxBorderDescriptor = class
  public const
    EmptyBorder: Cardinal = $ffffffff;
    BorderDescriptorSize = 8;
    ReturnOffset = -8;
  strict private
    function GetColor: TdxAlphaColor;
    procedure SetColor(const AValue: TdxAlphaColor);
  protected
    procedure Read(AReader: TBinaryReader); overload;
    procedure Read(const AData: TBytes; AStartIndex: Integer); overload;
  public
    FColorReference: TdxDocColorReference;
    FFrame: Boolean;
    FOffset: Byte;
    FShadow: Boolean;
    FStyle: TdxDocBorderLineStyle;
    FWidth: Byte;
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader): TdxBorderDescriptor; static;
    class function FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxBorderDescriptor; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure ConvertFromBorderInfo(AInfo: TdxBorderInfo; AUnitConverter: TdxDocumentModelUnitConverter);
    procedure ApplyProperties(ADestination: TdxBorderBase; AUnitConverter: TdxDocumentModelUnitConverter);
    function Clone: TdxBorderDescriptor;

    property Color: TdxAlphaColor read GetColor write SetColor;
    property Frame: Boolean read FFrame write FFrame;
    property Offset: Byte read FOffset write FOffset;
    property Shadow: Boolean read FShadow write FShadow;
    property Style: TdxDocBorderLineStyle read FStyle write FStyle;
    property Width: Byte read FWidth write FWidth;
  end;

  { TdxDocBorderCalculator }

  TdxDocBorderCalculator = class
  public
    class function MapToDocBorderLineStyle(ALineStyleType: TdxBorderLineStyle): TdxDocBorderLineStyle; static;
    class function MapToBorderLineStyle(ALineStyleType: TdxDocBorderLineStyle): TdxBorderLineStyle; static;
  end;

implementation

{ TdxBorderDescriptor97 }

class function TdxBorderDescriptor97.FromStream(AReader: TBinaryReader): TdxBorderDescriptor97;
begin
  Result := TdxBorderDescriptor97.Create;
  Result.Read(AReader);
end;

class function TdxBorderDescriptor97.FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxBorderDescriptor97;
begin
  Result := TdxBorderDescriptor97.Create;
  Result.Read(AData, AStartIndex);
end;

procedure TdxBorderDescriptor97.Read(AReader: TBinaryReader);
var
  AData: TBytes;
begin
  Assert(AReader <> nil, 'reader');
  AData := AReader.ReadBytes(BorderDescriptorSize);
  Read(AData, 0);
end;

procedure TdxBorderDescriptor97.Read(const AData: TBytes; AStartIndex: Integer);
var
  ABorder: Cardinal;
  AColorIndex: Byte;
begin
  ABorder := PCardinal(@AData[AStartIndex])^;
  if ABorder = EmptyBorder then
    Exit;

  if ABorder = 0 then
  begin
    FNotUseBorder := True;
    Exit;
  end;

  FWidth := AData[AStartIndex];
  if FWidth < 2 then
    FWidth := 2;
  FStyle := TdxDocBorderLineStyle(AData[AStartIndex + 1]);
  AColorIndex := AData[AStartIndex + 2];
  if AColorIndex < Length(TdxDocConstants.DefaultMSWordColor) then
    FBorderColor := TdxDocConstants.DefaultMSWordColor[AColorIndex];
  FDistance := AData[AStartIndex + 3] and $E0;
end;

procedure TdxBorderDescriptor97.Write(AWriter: TBinaryWriter);
var
  AColorIndex: Byte;
  AIndex: Integer;
begin
  Assert(AWriter <> nil, 'writer');
  if (FStyle = TdxDocBorderLineStyle.None) and (FWidth = 0) then
  begin
    AWriter.Write(EmptyBorder);
    Exit;
  end;
  AWriter.Write(FWidth);
  AWriter.Write(Byte(FStyle));
  AIndex := TdxDocConstants.GetColorIndex(FBorderColor);
  if AIndex < 0 then
    AColorIndex := $FF
  else
    AColorIndex := AIndex;
  AWriter.Write(AColorIndex);
  AWriter.Write(Byte(FDistance));
end;

procedure TdxBorderDescriptor97.ConvertFromBorderInfo(AInfo: TdxBorderInfo; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  FBorderColor := AInfo.Color;
  FStyle := TdxDocBorderCalculator.MapToDocBorderLineStyle(AInfo.Style);
  FWidth := Byte(Trunc(AUnitConverter.ModelUnitsToTwips(AInfo.Width) / 2.5));
end;

procedure TdxBorderDescriptor97.ApplyProperties(ADestination: TdxBorderBase; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  if FNotUseBorder then
    Exit;
  ADestination.Color := BorderColor;
  ADestination.Style := TdxDocBorderCalculator.MapToBorderLineStyle(FStyle);
  ADestination.Width := AUnitConverter.TwipsToModelUnits(FWidth * 5 div 2);
end;

function TdxBorderDescriptor97.Clone: TdxBorderDescriptor97;
begin
  Result := TdxBorderDescriptor97.Create;
  Result.FBorderColor := FBorderColor;
  Result.FStyle := Style;
  Result.FDistance := FDistance;
  Result.FWidth := FWidth;
  Result.FNotUseBorder := FNotUseBorder;
end;

{ TdxBorderDescriptor }

constructor TdxBorderDescriptor.Create;
begin
  inherited Create;
  FColorReference := TdxDocColorReference.Create;
end;

destructor TdxBorderDescriptor.Destroy;
begin
  FColorReference.Free;
  inherited Destroy;
end;

class function TdxBorderDescriptor.FromStream(AReader: TBinaryReader): TdxBorderDescriptor;
begin
  Result := TdxBorderDescriptor.Create;
  Result.Read(AReader);
end;

class function TdxBorderDescriptor.FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxBorderDescriptor;
begin
  Result := TdxBorderDescriptor.Create;
  Result.Read(AData, AStartIndex);
end;

function TdxBorderDescriptor.GetColor: TdxAlphaColor;
begin
  Result := FColorReference.Color;
end;

procedure TdxBorderDescriptor.SetColor(const AValue: TdxAlphaColor);
begin
  FColorReference.Color := AValue;
end;

procedure TdxBorderDescriptor.Read(AReader: TBinaryReader);
var
  ABitwiseField: Byte;
begin
  Assert(AReader <> nil, 'reader');
  if AReader.ReadInt64 = EmptyBorder then
    Exit;
  AReader.BaseStream.Seek(ReturnOffset, TSeekOrigin.soCurrent);
  FColorReference.Free;
  FColorReference := TdxDocColorReference.FromByteArray(AReader.ReadBytes(TdxDocColorReference.ColorReferenceSize), 0);
  FWidth := AReader.ReadByte;
  FStyle := TdxDocBorderLineStyle(AReader.ReadByte);
  ABitwiseField := AReader.ReadByte;
  FOffset := Byte(ABitwiseField and $1F);
  FShadow := (ABitwiseField and $20) <> 0;
  FFrame := (ABitwiseField and $40) <> 0;
  AReader.ReadByte;
end;

procedure TdxBorderDescriptor.Read(const AData: TBytes; AStartIndex: Integer);
var
  ABitwiseField: Byte;
begin
  FColorReference.Free;
  FColorReference := TdxDocColorReference.FromByteArray(AData, AStartIndex);
  if (PCardinal(@AData[AStartIndex])^ = EmptyBorder) or (PCardinal(@AData[AStartIndex + 4])^ = EmptyBorder) then
    Exit;
  FWidth := AData[AStartIndex + 4];
  FStyle := TdxDocBorderLineStyle(AData[AStartIndex + 5]);
  ABitwiseField := AData[AStartIndex + 6];
  FOffset := Byte((ABitwiseField and $1f));
  FShadow := (ABitwiseField and $20) <> 0;
  FFrame := (ABitwiseField and $40) <> 0;
end;

procedure TdxBorderDescriptor.Write(AWriter: TBinaryWriter);
var
  ABitwiseField: Byte;
begin
  Assert(AWriter <> nil, 'writer');
  AWriter.Write(FColorReference.GetBytes);
  if (FStyle = TdxDocBorderLineStyle.None) and (FWidth = 0) then
  begin
    AWriter.Write(EmptyBorder);
    Exit;
  end;
  AWriter.Write(FWidth);
  AWriter.Write(Byte(FStyle));
  ABitwiseField := FOffset;
  if FShadow then
    ABitwiseField := ABitwiseField or $20;
  if FFrame then
    ABitwiseField := ABitwiseField or $40;
  AWriter.Write(ABitwiseField);
  AWriter.Write(Byte(0));
end;

procedure TdxBorderDescriptor.ConvertFromBorderInfo(AInfo: TdxBorderInfo; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  Color := AInfo.Color;
  FFrame := AInfo.Frame;
  FOffset := Byte(AInfo.Offset);
  FShadow := AInfo.Shadow;
  FStyle := TdxDocBorderCalculator.MapToDocBorderLineStyle(AInfo.Style);
  FWidth := Byte(Trunc(AUnitConverter.ModelUnitsToTwips(AInfo.Width) / 2.5));
end;

procedure TdxBorderDescriptor.ApplyProperties(ADestination: TdxBorderBase; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  ADestination.Color := Color;
  ADestination.Frame := FFrame;
  ADestination.Offset := FOffset;
  ADestination.Shadow := FShadow;
  ADestination.Style := TdxDocBorderCalculator.MapToBorderLineStyle(FStyle);
  ADestination.Width := AUnitConverter.TwipsToModelUnits(FWidth * 5 div 2);
end;

function TdxBorderDescriptor.Clone: TdxBorderDescriptor;
begin
  Result := TdxBorderDescriptor.Create;
  Result.Color := Color;
  Result.frame := FFrame;
  Result.offset := FOffset;
  Result.shadow := FShadow;
  Result.style := FStyle;
  Result.width := FWidth;
end;

{ TdxDocBorderCalculator }

class function TdxDocBorderCalculator.MapToDocBorderLineStyle(ALineStyleType: TdxBorderLineStyle): TdxDocBorderLineStyle;
begin
  case ALineStyleType of
    TdxBorderLineStyle.None:
      Result := TdxDocBorderLineStyle.None;
    TdxBorderLineStyle.Single:
      Result := TdxDocBorderLineStyle.Single;
    TdxBorderLineStyle.Double:
      Result := TdxDocBorderLineStyle.Double;
    TdxBorderLineStyle.Dotted:
      Result := TdxDocBorderLineStyle.Dotted;
    TdxBorderLineStyle.Dashed:
      Result := TdxDocBorderLineStyle.Dashed;
    TdxBorderLineStyle.DotDash:
      Result := TdxDocBorderLineStyle.DotDash;
    TdxBorderLineStyle.DotDotDash:
      Result := TdxDocBorderLineStyle.DotDotDash;
    TdxBorderLineStyle.Triple:
      Result := TdxDocBorderLineStyle.Triple;
    TdxBorderLineStyle.ThinThickSmallGap:
      Result := TdxDocBorderLineStyle.ThinThickSmallGap;
    TdxBorderLineStyle.ThickThinSmallGap:
      Result := TdxDocBorderLineStyle.ThickThinSmallGap;
    TdxBorderLineStyle.ThinThickThinSmallGap:
      Result := TdxDocBorderLineStyle.ThinThickThinSmallGap;
    TdxBorderLineStyle.ThinThickMediumGap:
      Result := TdxDocBorderLineStyle.ThinThickMediumGap;
    TdxBorderLineStyle.ThickThinMediumGap:
      Result := TdxDocBorderLineStyle.ThickThinMediumGap;
    TdxBorderLineStyle.ThinThickThinMediumGap:
      Result := TdxDocBorderLineStyle.ThinThickThinMediumGap;
    TdxBorderLineStyle.ThinThickLargeGap:
      Result := TdxDocBorderLineStyle.ThinThickLargeGap;
    TdxBorderLineStyle.ThickThinLargeGap:
      Result := TdxDocBorderLineStyle.ThickThinLargeGap;
    TdxBorderLineStyle.ThinThickThinLargeGap:
      Result := TdxDocBorderLineStyle.ThinThickThinLargeGap;
    TdxBorderLineStyle.Wave:
      Result := TdxDocBorderLineStyle.Wave;
    TdxBorderLineStyle.DoubleWave:
      Result := TdxDocBorderLineStyle.DoubleWave;
    else
      Result := TdxDocBorderLineStyle.None;
  end;
end;

class function TdxDocBorderCalculator.MapToBorderLineStyle(ALineStyleType: TdxDocBorderLineStyle): TdxBorderLineStyle;
begin
  case ALineStyleType of
    TdxDocBorderLineStyle.None:
      Result := TdxBorderLineStyle.None;
    TdxDocBorderLineStyle.Single:
      Result := TdxBorderLineStyle.Single;
    TdxDocBorderLineStyle.Double:
      Result := TdxBorderLineStyle.Double;
    TdxDocBorderLineStyle.Dotted:
      Result := TdxBorderLineStyle.Dotted;
    TdxDocBorderLineStyle.Dashed:
      Result := TdxBorderLineStyle.Dashed;
    TdxDocBorderLineStyle.DotDash:
      Result := TdxBorderLineStyle.DotDash;
    TdxDocBorderLineStyle.DotDotDash:
      Result := TdxBorderLineStyle.DotDotDash;
    TdxDocBorderLineStyle.Triple:
      Result := TdxBorderLineStyle.Triple;
    TdxDocBorderLineStyle.ThinThickSmallGap:
      Result := TdxBorderLineStyle.ThinThickSmallGap;
    TdxDocBorderLineStyle.ThickThinSmallGap:
      Result := TdxBorderLineStyle.ThickThinSmallGap;
    TdxDocBorderLineStyle.ThinThickThinSmallGap:
      Result := TdxBorderLineStyle.ThinThickThinSmallGap;
    TdxDocBorderLineStyle.ThinThickMediumGap:
      Result := TdxBorderLineStyle.ThinThickMediumGap;
    TdxDocBorderLineStyle.ThickThinMediumGap:
      Result := TdxBorderLineStyle.ThickThinMediumGap;
    TdxDocBorderLineStyle.ThinThickThinMediumGap:
      Result := TdxBorderLineStyle.ThinThickThinMediumGap;
    TdxDocBorderLineStyle.ThinThickLargeGap:
      Result := TdxBorderLineStyle.ThinThickLargeGap;
    TdxDocBorderLineStyle.ThickThinLargeGap:
      Result := TdxBorderLineStyle.ThickThinLargeGap;
    TdxDocBorderLineStyle.ThinThickThinLargeGap:
      Result := TdxBorderLineStyle.ThinThickThinLargeGap;
    TdxDocBorderLineStyle.Wave:
      Result := TdxBorderLineStyle.Wave;
    TdxDocBorderLineStyle.DoubleWave:
      Result := TdxBorderLineStyle.DoubleWave;
    else
      Result := TdxBorderLineStyle.Single;
  end;
end;

end.
