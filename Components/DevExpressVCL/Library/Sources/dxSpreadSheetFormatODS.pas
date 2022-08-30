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

unit dxSpreadSheetFormatODS;

{$I cxVer.Inc}

interface

uses
  SysUtils, Classes, dxSpreadSheetCore, dxSpreadSheetClasses, dxSpreadSheetPackedFileFormatCore, dxXMLDoc, dxCustomTree;

type

  { TdxSpreadSheetODSFormat }

  TdxSpreadSheetODSFormat = class(TdxSpreadSheetCustomFormat)
  public
    class function CanReadFromStream(AStream: TStream): Boolean; override;
    class function CreateFormatSettings: TdxSpreadSheetFormatSettings; override;
    class function GetDescription: string; override;
    class function GetExt: string; override;
    class function GetReader: TdxSpreadSheetCustomReaderClass; override;
    class function GetWriter: TdxSpreadSheetCustomWriterClass; override;
  end;

  { TdxSpreadSheetODSFormatSettings }

  TdxSpreadSheetODSFormatSettings = class(TdxSpreadSheetFormatSettings)
  public
    procedure UpdateSettings; override;
  end;

implementation

uses
  dxCore, dxSpreadSheetStrs, dxSpreadSheetTypes, dxSpreadSheetFormatODSReader;

{ TdxSpreadSheetODSFormat }

class function TdxSpreadSheetODSFormat.CanReadFromStream(AStream: TStream): Boolean;
var
  AReader: TdxSpreadSheetODSReader;
  ASavedPosition: Int64;
begin
  ASavedPosition := AStream.Position;
  try
    try
      AReader := TdxSpreadSheetODSReader.Create(nil, AStream);
      try
        Result := AReader.CheckMimeType;
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

class function TdxSpreadSheetODSFormat.CreateFormatSettings: TdxSpreadSheetFormatSettings;
begin
  Result := TdxSpreadSheetODSFormatSettings.Create;
end;

class function TdxSpreadSheetODSFormat.GetDescription: string;
begin
  Result := 'OpenDocument SpreadSheet';
end;

class function TdxSpreadSheetODSFormat.GetExt: string;
begin
  Result := '.ods';
end;

class function TdxSpreadSheetODSFormat.GetReader: TdxSpreadSheetCustomReaderClass;
begin
  Result := TdxSpreadSheetODSReader;
end;

class function TdxSpreadSheetODSFormat.GetWriter: TdxSpreadSheetCustomWriterClass;
begin
  Result := nil;
end;

{ TdxSpreadSheetODSFormatSettings }

procedure TdxSpreadSheetODSFormatSettings.UpdateSettings;
begin
  inherited UpdateSettings;
  ArraySeparator := ':';
  ListSeparator := ';';
  DecimalSeparator := '.';
end;

initialization
  TdxSpreadSheetODSFormat.Register;

finalization
  TdxSpreadSheetODSFormat.Unregister;
end.
