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

unit dxSpreadSheetFormatXLS;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  RTLConsts, Windows, Generics.Defaults, Generics.Collections, Classes, Types, SysUtils, Math, Graphics, dxCore,
  dxSpreadSheetFormulas, dxSpreadSheetCore, dxSpreadSheetUtils, dxSpreadSheetTypes;

resourcestring
  sdxInvalidStreamFormat = 'Unsupported Stream Format';
  sdxInvalidStreamVersion = 'Unsupported Stream Version';
  sdxUnsupportedEncryption = 'Unsupported Encryption Algorithm';

const
  ErrorCodeToErrorIndex: array[TdxSpreadSheetFormulaErrorCode] of Byte = (0, 0, 7, 15, 23, 29, 36, 42);

type

  { TdxSpreadSheetXLSFormat }

  TdxSpreadSheetXLSFormat = class(TdxSpreadSheetCustomFormat)
  public
    class function CanReadFromStream(AStream: TStream): Boolean; override;
    class function GetDescription: string; override;
    class function GetExt: string; override;
    class function GetReader: TdxSpreadSheetCustomReaderClass; override;
    class function GetWriter: TdxSpreadSheetCustomWriterClass; override;
  end;

  { TdxSpreadSheetXLTFormat }

  TdxSpreadSheetXLTFormat = class(TdxSpreadSheetXLSFormat)
  public
    class function GetDescription: string; override;
    class function GetExt: string; override;
  end;

function CheckColumn(AIndex: Integer): Integer; inline;
function CheckRow(AIndex: Integer): Integer; inline;

function XLSErrorToErrorCode(ACode: Byte): TdxSpreadSheetFormulaErrorCode;
implementation

uses
  dxSpreadSheetFormatXLSReader, dxSpreadSheetFormatXLSWriter, dxSpreadSheetFormatXLSTypes, dxOLEDocument;

function CheckColumn(AIndex: Integer): Integer; inline;
begin
  if AIndex > dxMaxColumn then
    Result := dxMaxColumn
  else
    Result := AIndex;
end;

function CheckRow(AIndex: Integer): Integer; inline;
begin
  if AIndex > dxMaxRow then
    Result := dxMaxRow
  else
    Result := AIndex;
end;

function XLSErrorToErrorCode(ACode: Byte): TdxSpreadSheetFormulaErrorCode;
begin
  Result := ecNone;
  case ACode of
    0:
      Result := ecNull;
    7:
      Result := ecDivByZero;
    15:
      Result := ecValue;
    23:
      Result := ecRefErr;
    29:
      Result := ecName;
    36:
      Result := ecNum;
    42:
      Result := ecNA;
  end;
end;

{ TdxSpreadSheetXLSFormat }

class function TdxSpreadSheetXLSFormat.CanReadFromStream(AStream: TStream): Boolean;
begin
  Result := dxIsOLEStream(AStream);
end;

class function TdxSpreadSheetXLSFormat.GetDescription: string;
begin
  Result := 'Excel 97-2003 Workbook';
end;

class function TdxSpreadSheetXLSFormat.GetExt: string;
begin
  Result := '.xls';
end;

class function TdxSpreadSheetXLSFormat.GetReader: TdxSpreadSheetCustomReaderClass;
begin
  Result := TdxSpreadSheetXLSReader;
end;

class function TdxSpreadSheetXLSFormat.GetWriter: TdxSpreadSheetCustomWriterClass;
begin
  Result := TdxSpreadSheetXLSWriter;
end;

{ TdxSpreadSheetXLTFormat }

class function TdxSpreadSheetXLTFormat.GetDescription: string;
begin
  Result := 'Excel 97-2003 Template';
end;

class function TdxSpreadSheetXLTFormat.GetExt: string;
begin
  Result := '.xlt';
end;

initialization
  TdxSpreadSheetXLSFormat.Register;
  TdxSpreadSheetXLTFormat.Register;

finalization
  TdxSpreadSheetXLTFormat.Unregister;
  TdxSpreadSheetXLSFormat.Unregister;
end.
