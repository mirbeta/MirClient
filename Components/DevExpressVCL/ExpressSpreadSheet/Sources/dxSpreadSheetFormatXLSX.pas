{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSpreadSheetFormatXLSX;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, dxSpreadSheetCore, dxSpreadSheetTypes, dxSpreadSheetClasses, dxCore;

const
  dxXLSXMaxColumnIndex = dxSpreadSheetMaxColumnCount - 1;
  dxXLSXMaxRowIndex = dxSpreadSheetMaxRowCount - 1;

type

  { TdxSpreadSheetXLSXFormat }

  TdxSpreadSheetXLSXFormat = class(TdxSpreadSheetCustomFormat)
  public
    class function CanReadFromStream(AStream: TStream): Boolean; override;
    class function CreateFormatSettings: TdxSpreadSheetFormatSettings; override;
    class function CreateReader(ASpreadSheet: TdxCustomSpreadSheet; AStream: TStream): TdxSpreadSheetCustomReader; override;
    class function GetDescription: string; override;
    class function GetExt: string; override;
    class function GetReader: TdxSpreadSheetCustomReaderClass; override;
    class function GetWriter: TdxSpreadSheetCustomWriterClass; override;
  end;

  { TdxSpreadSheetXLTXFormat }

  TdxSpreadSheetXLTXFormat = class(TdxSpreadSheetXLSXFormat)
  public
    class function GetDescription: string; override;
    class function GetExt: string; override;
    class function GetWriter: TdxSpreadSheetCustomWriterClass; override;
  end;

  { TdxSpreadSheetXLSXUtils }

  TdxSpreadSheetXLSXUtils = class
  private
    const PercentsResolution = 1000;
    const PositiveFixedAngleResolution = 60000;
  private
    class function DecodeValue(AValue: Integer): Integer; inline;
    class function EncodeValue(AValue: Integer): Integer; inline;
  public
    // Color Alpha
    class function DecodeColorAlpha(const AValue: Integer): Byte; inline;
    class function EncodeColorAlpha(const AValue: Byte): Integer; inline;
    // Percents
    class function DecodePercents(const AValue: Integer): Double;
    class function EncodePercents(const AValue: Double): Integer;
    // PositiveFixedAngle
    class function DecodePositiveFixedAngle(const AValue: Integer): Integer;
    class function EncodePositiveFixedAngle(const AValue: Integer): Integer;
    // Source Rect
    class function DecodeSourceRect(const R: TRect): TRect;
    class function EncodeSourceRect(const R: TRect): TRect;

    class function GetRelsFileNameForFile(const AFileName: AnsiString): AnsiString;
  end;

implementation

uses
  AnsiStrings, cxGraphics, cxGeometry, dxZIPUtils, dxOLECryptoContainer,
  dxSpreadSheetFormatXLSXReader, dxSpreadSheetFormatXLSXWriter, dxSpreadSheetUtils, dxSpreadSheetFormatUtils,
  dxDPIAwareUtils;

type
  TdxSpreadSheetAccess = class(TdxCustomSpreadSheet);

{ TdxSpreadSheetXLSXFormat }

class function TdxSpreadSheetXLSXFormat.CanReadFromStream(AStream: TStream): Boolean;
var
  AFileName: AnsiString;
  AReader: TdxSpreadSheetXLSXReader;
  ASavedPosition: Int64;
begin
  ASavedPosition := AStream.Position;
  try
    Result := TdxOLECryptoContainer.IsOurStream(AStream);
    if not Result then
    try
      AReader := TdxSpreadSheetXLSXReader.Create(nil, AStream);
      try
        AReader.IgnoreMessages := [Low(TdxSpreadSheetMessageType)..High(TdxSpreadSheetMessageType)];
        Result := AReader.GetWorkbookFileName(AFileName);
      finally
        AReader.Free;
      end;
    except
      Result := False;
    end;
  finally
    AStream.Position := ASavedPosition;
  end;
end;

class function TdxSpreadSheetXLSXFormat.CreateFormatSettings: TdxSpreadSheetFormatSettings;
begin
  Result := TdxSpreadSheetFormatSettings.Create;
end;

class function TdxSpreadSheetXLSXFormat.CreateReader(
  ASpreadSheet: TdxCustomSpreadSheet; AStream: TStream): TdxSpreadSheetCustomReader;
var
  APassword: string;
begin
  APassword := ASpreadSheet.Password;
  if TdxOLECryptoContainer.Decrypt(AStream, AStream, APassword,
    function (var APassword: string): Boolean
    begin
      if APassword <> dxSpreadSheetDefaultPassword then
      begin
        APassword := dxSpreadSheetDefaultPassword;
        Result := True;
      end
      else
        Result := TdxSpreadSheetAccess(ASpreadSheet).DoGetPassword(APassword);
    end, 2)
  then
    begin
      Result := GetReader.Create(ASpreadSheet, AStream);
      Result.StreamAutoFree := True;
    end
    else
      Result := inherited CreateReader(ASpreadSheet, AStream);
end;

class function TdxSpreadSheetXLSXFormat.GetDescription: string;
begin
  Result := 'Excel Workbook';
end;

class function TdxSpreadSheetXLSXFormat.GetExt: string;
begin
  Result := '.xlsx';
end;

class function TdxSpreadSheetXLSXFormat.GetReader: TdxSpreadSheetCustomReaderClass;
begin
  Result := TdxSpreadSheetXLSXReader;
end;

class function TdxSpreadSheetXLSXFormat.GetWriter: TdxSpreadSheetCustomWriterClass;
begin
  Result := TdxSpreadSheetXLSXWriter;
end;

{ TdxSpreadSheetXLTXFormat }

class function TdxSpreadSheetXLTXFormat.GetDescription: string;
begin
  Result := 'Excel Template';
end;

class function TdxSpreadSheetXLTXFormat.GetExt: string;
begin
  Result := '.xltx';
end;

class function TdxSpreadSheetXLTXFormat.GetWriter: TdxSpreadSheetCustomWriterClass;
begin
  Result := TdxSpreadSheetXLTXWriter;
end;

{ TdxSpreadSheetXLSXUtils }

class function TdxSpreadSheetXLSXUtils.DecodeColorAlpha(const AValue: Integer): Byte;
begin
  Result := MulDiv(AValue, MaxByte, 100 * PercentsResolution)
end;

class function TdxSpreadSheetXLSXUtils.EncodeColorAlpha(const AValue: Byte): Integer;
begin
  Result := MulDiv(AValue, 100 * PercentsResolution, MaxByte);
end;

class function TdxSpreadSheetXLSXUtils.DecodePercents(const AValue: Integer): Double;
begin
  Result := AValue / PercentsResolution;
end;

class function TdxSpreadSheetXLSXUtils.EncodePercents(const AValue: Double): Integer;
begin
  Result := Trunc(AValue * PercentsResolution);
end;

class function TdxSpreadSheetXLSXUtils.DecodePositiveFixedAngle(const AValue: Integer): Integer;
begin
  Result := (AValue div PositiveFixedAngleResolution) mod 360;
end;

class function TdxSpreadSheetXLSXUtils.EncodePositiveFixedAngle(const AValue: Integer): Integer;
begin
  Result := AValue mod 360;
  if Result < 0 then
    Inc(Result, 360);
  Result := Result * PositiveFixedAngleResolution;
end;

class function TdxSpreadSheetXLSXUtils.DecodeSourceRect(const R: TRect): TRect;
begin
  Result := cxRect(DecodeValue(R.Left), DecodeValue(R.Top), DecodeValue(R.Right), DecodeValue(R.Bottom));
end;

class function TdxSpreadSheetXLSXUtils.DecodeValue(AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, dxDefaultDPI, 21333);
end;

class function TdxSpreadSheetXLSXUtils.EncodeSourceRect(const R: TRect): TRect;
begin
  Result := cxRect(EncodeValue(R.Left), EncodeValue(R.Top), EncodeValue(R.Right), EncodeValue(R.Bottom));
end;

class function TdxSpreadSheetXLSXUtils.EncodeValue(AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 21333, dxDefaultDPI);
end;

class function TdxSpreadSheetXLSXUtils.GetRelsFileNameForFile(const AFileName: AnsiString): AnsiString;
begin
  Result := TdxZIPPathHelper.DecodePath(AFileName);
  Result := ExtractFilePath(Result) + '_rels' + PathDelim + ExtractFileName(Result) + '.rels';
  Result := TdxZIPPathHelper.EncodePath(Result);
end;

initialization
  TdxSpreadSheetXLSXFormat.Register;
  TdxSpreadSheetXLTXFormat.Register;

finalization
  TdxSpreadSheetXLTXFormat.Unregister;
  TdxSpreadSheetXLSXFormat.Unregister;
end.
