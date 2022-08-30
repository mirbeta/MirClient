{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars ImageCombo item editor                       }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxBarImageComboEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, dxCore, dxBarExtItems;

type
  TdxBarImageComboEditorForm = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ButtonAdd: TButton;
    ButtonDelete: TButton;
    Grid: TStringGrid;
    ButtonInsert: TButton;
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonInsertClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; Col, Row: Integer; Rect: TRect; State: TGridDrawState);
    procedure GridSelectCell(Sender: TObject; Col, Row: Integer; var CanSelect: Boolean);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure GridGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: String);
    procedure FormShow(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FCombo: TdxBarImageCombo;
    FPrevCellText: string;
  end;

function dxBarImageComboEdit(ACombo: TdxBarImageCombo): Boolean;

implementation

uses
  cxClasses, dxBarStrs;

{$R *.DFM}

const
  DXBAR_IMAGEINDEX = 39456;  // from dxBarExtItems.res
  DXBAR_IMAGETEXT = 39457;

type
  TDummyGrid = class(TStringGrid);

function dxBarImageComboEdit(ACombo: TdxBarImageCombo): Boolean;
var
  I: Integer;
begin
  with TdxBarImageComboEditorForm.Create(nil) do
  begin
    FCombo := ACombo;
    with Grid do
    begin
      if ACombo.Images <> nil then ColWidths[0] := ACombo.Images.Width;
      ColWidths[2] := ClientWidth - GetSystemMetrics(SM_CXVSCROLL) -
        (ColWidths[0] + 1 + ColWidths[1] + 1);
      if ACombo.Images <> nil then
        DefaultRowHeight := ACombo.Images.Height
      else
        DefaultRowHeight := 20;
      RowHeights[0] := 20;
      if ACombo.Items.Count = 0 then
      begin
        RowCount := 2;
        Cells[1, 1] := '-1';
      end
      else RowCount := 1 + ACombo.Items.Count;
      Cells[1, 0] := cxGetResourceString(@dxSBAR_IMAGEINDEX);
      Cells[2, 0] := cxGetResourceString(@dxSBAR_IMAGETEXT);
      for I := 0 to ACombo.Items.Count - 1 do
      begin
        Cells[1, I + 1] := IntToStr(ACombo.ImageIndexes[I]);
        Cells[2, I + 1] := ACombo.Items[I];
      end;
    end;
    Result := ShowModal = mrOk;
    if Result then
      with Grid do
      begin
        ACombo.Items.Clear;
        if (RowCount > 2) or (Cells[2, 1] <> '') then
          for I := 1 to RowCount - 1 do
          begin
            ACombo.Items.Add(Cells[2, I]);
            try
              ACombo.ImageIndexes[I - 1] := StrToInt(Cells[1, I]);
            except
            end;
          end;
      end;
    Free;
  end;
end;

procedure TdxBarImageComboEditorForm.ButtonAddClick(Sender: TObject);
var
  AImageIndex: Integer;
begin
  with TDummyGrid(Grid) do
  begin
    RowCount := RowCount + 1;
    try
      AImageIndex := StrToInt(Cells[1, RowCount - 2]);
      if AImageIndex > -1 then Inc(AImageIndex);
    except
      AImageIndex := -1;
    end;
    Cells[1, RowCount - 1] := IntToStr(AImageIndex);
    Cells[2, RowCount - 1] := '';
    InvalidateCell(0, RowCount - 1);
    Row := RowCount - 1;
  end;
end;

procedure TdxBarImageComboEditorForm.ButtonInsertClick(Sender: TObject);
var
  PrevRow, I: Integer;
begin
  if Grid.Row < 1 then Exit;
  with TDummyGrid(Grid) do
  begin
    PrevRow := Row;
    RowCount := RowCount + 1;
    for I := RowCount - 2 downto Row do MoveRow(I, I + 1);
    Cells[1, PrevRow] := '-1';
    Cells[2, PrevRow] := '';
    InvalidateCell(0, PrevRow);
    Row := PrevRow;
  end;
end;

procedure TdxBarImageComboEditorForm.ButtonDeleteClick(Sender: TObject);
var
  PrevRow: Integer;
begin
  with TDummyGrid(Grid) do
    if Row > 0 then
    begin
      PrevRow := Row;
      if RowCount > 2 then DeleteRow(Row)
      else
      begin
        Cells[1, 1] := '-1';
        Cells[2, 1] := '';
        InvalidateCell(0, 1);
      end;
      if PrevRow <= RowCount - 1 then Row := PrevRow
      else Row := RowCount - 1;
    end;
end;

procedure TdxBarImageComboEditorForm.GridDrawCell(Sender: TObject;
  Col, Row: Integer; Rect: TRect; State: TGridDrawState);
var
  AImageIndex: Integer;
begin
  if (Col = 0) and (Row > 0) and (FCombo.Images <> nil) then
  begin
    try
      AImageIndex := StrToInt(Grid.Cells[1, Row]);
    except
      AImageIndex := -1;
    end;
    if (0 <= AImageIndex) and (AImageIndex < FCombo.Images.Count) then
      FCombo.Images.Draw(Grid.Canvas, Rect.Left, Rect.Top, AImageIndex)
    else
      Grid.Canvas.FillRect(Rect);
  end;
end;

procedure TdxBarImageComboEditorForm.GridSelectCell(Sender: TObject;
  Col, Row: Integer; var CanSelect: Boolean);
begin
  if Col = 0 then CanSelect := False;
end;

procedure TdxBarImageComboEditorForm.FormShow(Sender: TObject);
begin
  Grid.Col := 1;
end;

procedure TdxBarImageComboEditorForm.GridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  TDummyGrid(Grid).InvalidateCell(0, ARow);
end;

procedure TdxBarImageComboEditorForm.GridKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      with Grid do
        if EditorMode then
          if Cells[Col, Row] <> FPrevCellText then
            Cells[Col, Row] := FPrevCellText
          else
            EditorMode := False;
    VK_INSERT:
      if not Grid.EditorMode then ButtonAdd.Click;
    VK_DELETE:
      if not Grid.EditorMode then ButtonDelete.Click;
  end;
end;

procedure TdxBarImageComboEditorForm.GridGetEditText(Sender: TObject;
  ACol, ARow: Integer; var Value: String);
begin
  FPrevCellText := Grid.Cells[ACol, ARow];
end;

end.
