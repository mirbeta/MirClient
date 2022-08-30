{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressVerticalGrid                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSVERTICALGRID AND ALL           }
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
unit cxVGridLayoutEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Menus,
  dxCore, cxButtons, cxVGrid, cxLookAndFeelPainters, cxGraphics, cxLookAndFeels, cxControls, dxLayoutControlAdapters,
  dxLayoutContainer, cxClasses, dxLayoutControl, dxLayoutLookAndFeels, dxForms;

type
  TfmvgLayoutEditor = class(TdxForm)
    btCancel: TcxButton;
    btCustomize: TcxButton;
    btOk: TcxButton;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    pnlVGPlace: TdxLayoutItem;

    procedure btCustomizeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FVerticalGrid: TcxCustomVerticalGrid;
  protected
    procedure ApplyUpdates(Dest: TcxCustomVerticalGrid); virtual;
    procedure CreateClone(Source: TcxCustomVerticalGrid); virtual;
    procedure PrepareLayoutStyle(Source: TcxCustomVerticalGrid); virtual;

    property VerticalGrid: TcxCustomVerticalGrid read FVerticalGrid;
  end;

var
  fmvgLayoutEditor: TfmvgLayoutEditor;

procedure ShowVerticalGridLayoutEditor(AVerticalGrid: TcxCustomVerticalGrid; ACaption: string = '');

implementation

uses
  cxInplaceContainer, cxVGridConsts;

{$R *.dfm}

const
  SizeStore: TRect = (Left: -1; Top: -1; Right: -1; Bottom: -1);

type
  TcxCustomVerticalGridClass = class of TcxCustomVerticalGrid;
  TcxCustomVerticalGridAccess = class(TcxCustomVerticalGrid);
  TcxUnboundVerticalGridAccess = class(TcxUnboundVerticalGrid);
  TcxVerticalGridCustomizingAccess = class(TcxVerticalGridCustomizing);
  TcxCustomRowAccess = class(TcxCustomRow);
  TcxCustomMultiEditorRowAccess = class(TcxCustomMultiEditorRow);
  TcxVerticalGridRowsAccess = class(TcxVerticalGridRows);

procedure ShowVerticalGridLayoutEditor(AVerticalGrid: TcxCustomVerticalGrid; ACaption: string = '');
var
  AForm: TfmvgLayoutEditor;
begin
  if AVerticalGrid = nil then Exit;
  AForm := TfmvgLayoutEditor.Create(nil);
  with AForm do
  try
    if ACaption <> '' then
      Caption := ACaption
    else
      Caption := cxGetResourceString(@cxSvgLayoutEditorCaption);
    FVerticalGrid := TcxCustomVerticalGridClass(AVerticalGrid.ClassType).Create(nil);
    try
      CreateClone(AVerticalGrid);
      pnlVGPlace.Control := FVerticalGrid;
      if ShowModal = mrOk then
        ApplyUpdates(AVerticalGrid);
    finally
      FVerticalGrid.Free;
    end;
  finally
    Release;
  end;
end;

{ TfmvgLayoutEditor }

procedure TfmvgLayoutEditor.ApplyUpdates(Dest: TcxCustomVerticalGrid);
var
  I: Integer;
  ASourceRow: TcxCustomRowAccess;
  ADestRow: TcxCustomRow;
  ADestRows: TcxVerticalGridRowsAccess;
begin
  with Dest do
  try
    BeginUpdate;
    ADestRows := TcxVerticalGridRowsAccess(Rows);
    for I := 0 to VerticalGrid.Rows.Count - 1 do
    begin
      ASourceRow := TcxCustomRowAccess(VerticalGrid.Rows[I]);
      ADestRow := ADestRows.FindRowByID(ASourceRow.ID);
      if ADestRow = nil then continue;
      if ASourceRow.Parent = nil then
        ADestRow.Parent := nil
      else
        ADestRow.Parent := ADestRows.FindRowByID(TcxCustomRowAccess(ASourceRow.Parent).ID);
      ADestRow.Index := ASourceRow.Index;
      if ASourceRow.IsHeightAssigned then
        ADestRow.Height := ASourceRow.Height;
      ADestRow.Expanded := ASourceRow.Expanded;
      ADestRow.Visible := ASourceRow.Visible;
    end;
    OptionsView.Assign(VerticalGrid.OptionsView);
  finally
    EndUpdate;
    TcxCustomVerticalGridAccess(Dest).Modified;
  end;
end;

procedure TfmvgLayoutEditor.CreateClone(Source: TcxCustomVerticalGrid);

  procedure CorrectRowsNames;
  var
    I, J: Integer;
    ASourceRow, ADestRow: TcxCustomRow;
  begin
    for I := 0 to VerticalGrid.Rows.Count - 1 do
    begin
      ASourceRow := Source.Rows[I];
      ADestRow := VerticalGrid.Rows[I];
      if ADestRow is TcxCustomMultiEditorRow then
        with TcxCustomMultiEditorRowAccess(ADestRow) do
          for J := 0 to Properties.Editors.Count - 1 do
            Properties.Editors[J].Options.Focusing := True;
      ADestRow.Options.Focusing := True;
      if ASourceRow.Name <> '' then
      try
        ADestRow.Name := ASourceRow.Name;
      except
      end;
    end;
  end;

begin
  dxLayoutCxLookAndFeel1.LookAndFeel.MasterLookAndFeel := Source.LookAndFeel;
  with VerticalGrid do
  try
    BeginUpdate;
    TcxCustomVerticalGridAccess(VerticalGrid).DataController.Assign(TcxCustomVerticalGridAccess(Source).DataController);
    AssignRows(Source);
    CorrectRowsNames;
    OptionsData.Assign(Source.OptionsData);
    OptionsView.Assign(Source.OptionsView);
    Images := Source.Images;
    PrepareLayoutStyle(Source);
    OptionsBehavior.Assign(Source.OptionsBehavior);
    Styles.Assign(Source.Styles);
    TcxCustomVerticalGridAccess(VerticalGrid).DragMode := dmAutomatic;
    OptionsBehavior.RowSizing := True;
    OptionsBehavior.CellHints := False;
    OptionsData.Editing := False;
    if OptionsData is TcxvgMultiRecordsOptionsData then
      with TcxvgMultiRecordsOptionsData(OptionsData) do
      begin
        Deleting := False;
        Inserting := False;
      end;
  finally
    EndUpdate;
  end;
end;

procedure TfmvgLayoutEditor.PrepareLayoutStyle(Source: TcxCustomVerticalGrid);
begin
  if VerticalGrid is TcxUnboundVerticalGrid then
    TcxUnboundVerticalGridAccess(VerticalGrid).LayoutStyle := TcxUnboundVerticalGridAccess(Source).LayoutStyle
  else
    if VerticalGrid is TcxVirtualVerticalGrid then
      TcxVirtualVerticalGrid(VerticalGrid).LayoutStyle := TcxVirtualVerticalGrid(Source).LayoutStyle;
end;

procedure TfmvgLayoutEditor.btCustomizeClick(Sender: TObject);
begin
  with TcxCustomVerticalGridAccess(VerticalGrid) do
  begin
    TcxVerticalGridCustomizingAccess(Customizing).ShowCategoryButtons := False;
    Customizing.Visible := True;
  end;
end;

procedure TfmvgLayoutEditor.FormCreate(Sender: TObject);
begin
  btOk.Caption := cxGetResourceString(@cxSvgOKCaption);
  btCancel.Caption := cxGetResourceString(@cxSvgCancelCaption);
  btCustomize.Caption := cxGetResourceString(@cxSvgLayoutEditorCustomize);
end;

procedure TfmvgLayoutEditor.FormShow(Sender: TObject);
begin
  if SizeStore.Right <> -1 then
  begin
    Left := SizeStore.Left;
    Top := SizeStore.Top;
    Width := SizeStore.Right;
    Height := SizeStore.Bottom;
  end;
end;

procedure TfmvgLayoutEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SizeStore.Left := Left;
  SizeStore.Top := Top;
  SizeStore.Right := Width;
  SizeStore.Bottom := Height;
end;

end.
