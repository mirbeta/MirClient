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

unit dxSpreadSheetTextFileFormatCore;

{$I cxVer.Inc}

interface

uses
  Windows, SysUtils, Classes, dxHashUtils, dxSpreadSheetCore, dxSpreadSheetClasses, dxSpreadSheetTypes;

type

  { TdxSpreadSheetStreamWriter }

  TdxSpreadSheetStreamWriter = class(TStreamWriter)
  public
    constructor Create(Stream: TStream; Encoding: TEncoding; WriteEncodingPreamble: Boolean = True); reintroduce;
  end;

  { TdxSpreadSheetCustomTextFormatWriter }

  TdxSpreadSheetCustomTextFormatWriter = class(TdxSpreadSheetCustomWriter)
  strict private
    FWriteEncodingPreamble: Boolean;
    FStreamWriter: TStreamWriter;
  protected
    function CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper; override;
    function CreateTableViewWriter(AView: TdxSpreadSheetTableView; const AArea: TRect): TdxSpreadSheetCustomFilerSubTask; virtual; abstract;
    function GetEncoding: TEncoding; virtual;
    //
    procedure WriteDocumentContent; virtual;
    procedure WriteDocumentFooter; virtual;
    procedure WriteDocumentHeader; virtual;
    procedure WriteTableView(AView: TdxSpreadSheetTableView); overload;
    procedure WriteTableView(AView: TdxSpreadSheetTableView; const AArea: TRect); overload;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); override;
    procedure WriteData; override;
    //
    property Encoding: TEncoding read GetEncoding;
    property StreamWriter: TStreamWriter read FStreamWriter;
    property WriteEncodingPreamble: Boolean read FWriteEncodingPreamble write FWriteEncodingPreamble;
  end;

  { TdxSpreadSheetCustomTextFormatTableViewWriter }

  TdxSpreadSheetCustomTextFormatTableViewWriter = class(TdxSpreadSheetCustomFilerSubTask)
  strict private
    FMemorySavingMode: Boolean;
    FProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
    FStreamWriter: TStreamWriter;
    FView: TdxSpreadSheetTableView;
  protected
    FArea: TRect;
    FEnumBlankCellsInTheEndOfLine: Boolean;

    procedure WriteCell(ARowIndex, AColumnIndex: Integer; ACell: TdxSpreadSheetCell); virtual; abstract;
    procedure WriteRow(ARowIndex: Integer; ARow: TdxSpreadSheetTableRow); virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomTextFormatWriter;
      AView: TdxSpreadSheetTableView; const AArea: TRect); virtual;
    procedure Execute; override;
    //
    property Area: TRect read FArea;
    property EnumBlankCellsInTheEndOfLine: Boolean read FEnumBlankCellsInTheEndOfLine;
    property MemorySavingMode: Boolean read FMemorySavingMode;
    property ProgressHelper: TdxSpreadSheetCustomFilerProgressHelper read FProgressHelper;
    property StreamWriter: TStreamWriter read FStreamWriter;
    property View: TdxSpreadSheetTableView read FView;
  end;

implementation

uses
  Math;

type
  TdxSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetCellAccess = class(TdxSpreadSheetCell);
  TdxSpreadSheetTableRowAccess = class(TdxSpreadSheetTableRow);
  TdxSpreadSheetTableRowCellsAccess = class(TdxSpreadSheetTableRowCells);
  TdxSpreadSheetTableRowsAccess = class(TdxSpreadSheetTableRows);

{ TdxSpreadSheetStreamWriter }

constructor TdxSpreadSheetStreamWriter.Create(Stream: TStream; Encoding: TEncoding; WriteEncodingPreamble: Boolean);
var
  APosition: Int64;
begin
  APosition := Stream.Position;
  inherited Create(Stream, Encoding);
  if not WriteEncodingPreamble then
  begin
    if (APosition = 0) and (Stream.Position = Length(Encoding.GetPreamble)) then
      Stream.Size := 0;
  end;
end;

{ TdxSpreadSheetCustomTextFormatWriter }

constructor TdxSpreadSheetCustomTextFormatWriter.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
begin
  inherited Create(AOwner, AStream);
  WriteEncodingPreamble := True;
end;

procedure TdxSpreadSheetCustomTextFormatWriter.WriteData;
begin
  FStreamWriter := TdxSpreadSheetStreamWriter.Create(Stream, Encoding, WriteEncodingPreamble);
  try
    WriteDocumentHeader;
    WriteDocumentContent;
    WriteDocumentFooter;
  finally
    FStreamWriter.Free;
  end;
end;

function TdxSpreadSheetCustomTextFormatWriter.CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
begin
  Result := TdxSpreadSheetCustomFilerProgressHelper.Create(Self, 1);
end;

function TdxSpreadSheetCustomTextFormatWriter.GetEncoding: TEncoding;
begin
  Result := TEncoding.Default;
end;

procedure TdxSpreadSheetCustomTextFormatWriter.WriteDocumentContent;
begin
  WriteTableView(SpreadSheet.ActiveSheetAsTable);
end;

procedure TdxSpreadSheetCustomTextFormatWriter.WriteDocumentFooter;
begin
  // do nothing
end;

procedure TdxSpreadSheetCustomTextFormatWriter.WriteDocumentHeader;
begin
  // do nothing
end;

procedure TdxSpreadSheetCustomTextFormatWriter.WriteTableView(AView: TdxSpreadSheetTableView);
begin
  WriteTableView(AView, Rect(0, 0, AView.Dimensions.Right, AView.Dimensions.Bottom));
end;

procedure TdxSpreadSheetCustomTextFormatWriter.WriteTableView(AView: TdxSpreadSheetTableView; const AArea: TRect);
begin
  ExecuteSubTask(CreateTableViewWriter(AView, AArea));
end;

{ TdxSpreadSheetCustomTextFormatTableViewWriter }

constructor TdxSpreadSheetCustomTextFormatTableViewWriter.Create(
  AOwner: TdxSpreadSheetCustomTextFormatWriter; AView: TdxSpreadSheetTableView; const AArea: TRect);
begin
  inherited Create(AOwner);
  FView := AView;
  FStreamWriter := AOwner.StreamWriter;
  FProgressHelper := AOwner.ProgressHelper;
  FArea := AArea;
end;

procedure TdxSpreadSheetCustomTextFormatTableViewWriter.Execute;
var
  ARowIndex: Integer;
begin
  FMemorySavingMode := sssExporting in TdxSpreadSheetAccess(SpreadSheet).State;

  ProgressHelper.BeginStage(View.Rows.Count);
  try
    ARowIndex := Area.Top;
    TdxSpreadSheetTableRowsAccess(View.Rows).ForEach(
      procedure (AItem: TdxDynamicListItem)
      var
        ARow: TdxSpreadSheetTableRow;
      begin
        ARow := TdxSpreadSheetTableRow(AItem);
        while ARowIndex < ARow.Index do
        begin
          if ARowIndex > Area.Top then
            StreamWriter.WriteLine;
          WriteRow(ARowIndex, nil);
          ProgressHelper.NextTask;
          Inc(ARowIndex);
        end;
        if ARowIndex > Area.Top then
          StreamWriter.WriteLine;
        WriteRow(ARowIndex, ARow);
        ProgressHelper.NextTask;
        Inc(ARowIndex);
      end,
      Area.Top, Area.Bottom);
  finally
    ProgressHelper.EndStage;
  end;
end;

procedure TdxSpreadSheetCustomTextFormatTableViewWriter.WriteRow(ARowIndex: Integer; ARow: TdxSpreadSheetTableRow);
var
  AColumnIndex: Integer;
begin
  AColumnIndex := Area.Left;
  if ARow <> nil then
    TdxSpreadSheetTableRowCellsAccess(TdxSpreadSheetTableRowAccess(ARow).RowCells).ForEach(
      procedure (AItem: TdxDynamicListItem)
      var
        ACell: TdxSpreadSheetCell;
      begin
        ACell := TdxSpreadSheetCell(AItem);
        while AColumnIndex < ACell.ColumnIndex do
        begin
          WriteCell(ARowIndex, AColumnIndex, nil);
          Inc(AColumnIndex);
        end;
        WriteCell(ARowIndex, AColumnIndex, ACell);
        if MemorySavingMode then
        begin
          TdxSpreadSheetCellAccess(ACell).ReleaseDisplayValue;
          TdxSpreadSheetCellAccess(ACell).ReleaseWrappers;
        end;
        Inc(AColumnIndex);
      end,
      Area.Left, Area.Right);

  if EnumBlankCellsInTheEndOfLine then
    while AColumnIndex <= Area.Right do
    begin
      WriteCell(ARowIndex, AColumnIndex, nil);
      Inc(AColumnIndex);
    end;
end;

end.
