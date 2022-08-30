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

unit dxSpreadSheetFormatTXT;

{$I cxVer.Inc}

interface

uses
  Types, SysUtils, Classes, dxHashUtils,
  dxSpreadSheetCore, dxSpreadSheetClasses, dxSpreadSheetUtils, dxSpreadSheetTextFileFormatCore;

type

  { TdxSpreadSheetTXTFormat }

  TdxSpreadSheetTXTFormat = class(TdxSpreadSheetCustomFormat)
  public
    class function CanReadFromStream(AStream: TStream): Boolean; override;
    class function GetDescription: string; override;
    class function GetExt: string; override;
    class function GetReader: TdxSpreadSheetCustomReaderClass; override;
    class function GetWriter: TdxSpreadSheetCustomWriterClass; override;
  end;

  { TdxSpreadSheetTXTFormatSettings }

  TdxSpreadSheetTXTFormatSettings = record
    Encoding: TEncoding;
    BeginString: string;
    EndString: string;
    Separator: string;
  end;

  { TdxSpreadSheetTXTWriter }

  TdxSpreadSheetTXTWriter = class(TdxSpreadSheetCustomTextFormatWriter)
  protected
    function CreateTableViewWriter(AView: TdxSpreadSheetTableView; const AArea: TRect): TdxSpreadSheetCustomFilerSubTask; override;
    function GetEncoding: TEncoding; override;
  end;

  { TdxSpreadSheetTXTTableViewWriter }

  TdxSpreadSheetTXTTableViewWriter = class(TdxSpreadSheetCustomTextFormatTableViewWriter)
  protected
    FColumnWidths: array of Integer;
    FSettings: TdxSpreadSheetTXTFormatSettings;

    procedure CalculateColumnWidths; virtual;
    procedure WriteCell(ARowIndex: Integer; AColumnIndex: Integer; ACell: TdxSpreadSheetCell); override;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomTextFormatWriter; AView: TdxSpreadSheetTableView; const AArea: TRect); override;
    procedure Execute; override;
  end;

var
  dxSpreadSheetTXTFormatSettings: TdxSpreadSheetTXTFormatSettings;

implementation

uses
  Math, StrUtils;

type
  TdxSpreadSheetCellAccess = class(TdxSpreadSheetCell);

{ TdxSpreadSheetTXTFormat }

class function TdxSpreadSheetTXTFormat.CanReadFromStream(AStream: TStream): Boolean;
begin
  Result := False;
end;

class function TdxSpreadSheetTXTFormat.GetDescription: string;
begin
  Result := 'Text Document';
end;

class function TdxSpreadSheetTXTFormat.GetExt: string;
begin
  Result := '.txt';
end;

class function TdxSpreadSheetTXTFormat.GetReader: TdxSpreadSheetCustomReaderClass;
begin
  Result := nil;
end;

class function TdxSpreadSheetTXTFormat.GetWriter: TdxSpreadSheetCustomWriterClass;
begin
  Result := TdxSpreadSheetTXTWriter;
end;

{ TdxSpreadSheetTXTWriter }

function TdxSpreadSheetTXTWriter.CreateTableViewWriter(
  AView: TdxSpreadSheetTableView; const AArea: TRect): TdxSpreadSheetCustomFilerSubTask;
begin
  Result := TdxSpreadSheetTXTTableViewWriter.Create(Self, AView, AArea);
end;

function TdxSpreadSheetTXTWriter.GetEncoding: TEncoding;
begin
  Result := dxSpreadSheetTXTFormatSettings.Encoding;
  if Result = nil then
    Result := TEncoding.Unicode;
end;

{ TdxSpreadSheetTXTTableViewWriter }

constructor TdxSpreadSheetTXTTableViewWriter.Create(
  AOwner: TdxSpreadSheetCustomTextFormatWriter; AView: TdxSpreadSheetTableView; const AArea: TRect);
begin
  inherited Create(AOwner, AView, AArea);
  FSettings := dxSpreadSheetTXTFormatSettings;
  FEnumBlankCellsInTheEndOfLine := True;
end;

procedure TdxSpreadSheetTXTTableViewWriter.Execute;
begin
  CalculateColumnWidths;
  inherited Execute;
end;

procedure TdxSpreadSheetTXTTableViewWriter.CalculateColumnWidths;
begin
  SetLength(FColumnWidths, 0);
  SetLength(FColumnWidths, View.Dimensions.Right + 1);

  View.ForEachCell(View.Dimensions,
    procedure (ACell: TdxSpreadSheetCell)
    var
      ACellWidth: Integer;
      AMergedCell: TdxSpreadSheetMergedCell;
      I: Integer;
    begin
      AMergedCell := View.MergedCells.FindCell(ACell.RowIndex, ACell.ColumnIndex);
      if AMergedCell <> nil then
      begin
        if AMergedCell.ActiveCell = ACell then
        begin
          ACellWidth := Length(ACell.DisplayText) div dxSpreadSheetAreaWidth(AMergedCell.Area);
          for I := AMergedCell.Area.Left to AMergedCell.Area.Right do
            FColumnWidths[I] := Max(FColumnWidths[I], ACellWidth);
        end;
      end
      else
        FColumnWidths[ACell.ColumnIndex] := Max(FColumnWidths[ACell.ColumnIndex], Length(ACell.DisplayText));

      if MemorySavingMode then
      begin
        TdxSpreadSheetCellAccess(ACell).ReleaseDisplayValue;
        TdxSpreadSheetCellAccess(ACell).ReleaseWrappers;
      end;
    end);
end;

procedure TdxSpreadSheetTXTTableViewWriter.WriteCell(ARowIndex, AColumnIndex: Integer; ACell: TdxSpreadSheetCell);

  function GetCellDataLength(ACell: TdxSpreadSheetCell): Integer;
  begin
    if ACell <> nil then
      Result := Length(ACell.DisplayText)
    else
      Result := 0;
  end;

begin
  if FSettings.BeginString <> '' then
    StreamWriter.Write(FSettings.BeginString);
  if ACell <> nil then
    StreamWriter.Write(ACell.DisplayText);
  if FSettings.EndString <> '' then
    StreamWriter.Write(FSettings.EndString);
  if AColumnIndex < Area.Right then
    StreamWriter.Write(FSettings.Separator);
  if FSettings.Separator = '' then
    StreamWriter.Write('  ' + DupeString(' ', FColumnWidths[AColumnIndex] - GetCellDataLength(ACell)));
end;

initialization
  TdxSpreadSheetTXTFormat.Register;

finalization
  TdxSpreadSheetTXTFormat.Unregister;
end.
