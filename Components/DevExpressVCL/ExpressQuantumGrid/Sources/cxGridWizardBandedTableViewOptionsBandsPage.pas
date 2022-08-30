{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridWizardBandedTableViewOptionsBandsPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  ActnList, StdCtrls, Math,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutContainer, dxLayoutControlAdapters,
  cxButtons, cxContainer, cxEdit, cxListBox, dxLayoutControl, cxGridBandedTableView, cxClasses, cxGeometry,
  cxGridWizardCustomPage, cxGridWizardStrs, cxGridWizardCustomHelper, dxLayoutLookAndFeels,
  cxGridWizardBandedTableViewHelper;

type
  { TcxGridWizardBandedTableViewOptionsBandsPageFrame }

  TcxGridWizardBandedTableViewOptionsBandsPageFrame = class(TcxGridWizardCustomPageFrame)
    acAddBand: TAction;
    acDeleteAllBands: TAction;
    acDeleteSelectedBands: TAction;
    acEditBand: TAction;
    aclBands: TActionList;
    acMoveDown: TAction;
    acMoveUp: TAction;
    Add: TMenuItem;
    btnAddBand: TcxButton;
    btnDeleteAllBands: TcxButton;
    btnDeleteSelectedBands: TcxButton;
    btnEditBand: TcxButton;
    btnMoveDown: TcxButton;
    btnMoveUp: TcxButton;
    Deleteall: TMenuItem;
    Deleteselected: TMenuItem;
    dxLayoutGroup2: TdxLayoutGroup;
    Edit: TMenuItem;
    lbBands: TcxListBox;
    lciAddBand: TdxLayoutItem;
    lciBands: TdxLayoutItem;
    lciDeleteAllBands: TdxLayoutItem;
    lciDeleteSelectedBands: TdxLayoutItem;
    lciEditBand: TdxLayoutItem;
    lciMoveDown: TdxLayoutItem;
    lciMoveUp: TdxLayoutItem;
    lcMainGroup1: TdxLayoutGroup;
    lcMainGroup2: TdxLayoutGroup;
    MoveDown: TMenuItem;
    MoveUp: TMenuItem;
    pupmBands: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    procedure acAddBandExecute(Sender: TObject);
    procedure acEditBandExecute(Sender: TObject);
    procedure acDeleteAllBandsExecute(Sender: TObject);
    procedure acDeleteSelectedBandsExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure lbBandsClick(Sender: TObject);
    procedure lbBandsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbBandsEndDrag(Sender, Target: TObject; X, Y: Integer);
  private
    procedure AddBand(const ACaption: string);
    procedure CheckBandsCount;
    procedure DeleteBand(const AIndex: Integer);
    function GetBandedTableViewHelper: IcxGridWizardBandedTableView;
    function GetBands: TcxGridBands;
    function GetDefaultBandCaption: string;
    procedure ReorderGridViewBands;
    procedure SwapBands(const AIndex1, AIndex2: Integer);
  protected
    function GetCanJumpToNextPage: Boolean; override;
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;

    property BandedTableViewHelper: IcxGridWizardBandedTableView read GetBandedTableViewHelper;
    property Bands: TcxGridBands read GetBands;
  public
    procedure ApplyLocalization; override;
    procedure LoadSettings; override;
  end;

implementation

uses
  cxGridWizardUnboundViewsEditItemDialog;

{$R *.dfm}

{ TcxGridWizardBandedTableViewOptionsBandsPageFrame }

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.ApplyLocalization;
begin
  acAddBand.Caption := cxGetResourceString(@scxgwCommonAdd);
  acDeleteAllBands.Caption := cxGetResourceString(@scxgwCommonDeleteAll);
  acDeleteSelectedBands.Caption := cxGetResourceString(@scxgwCommonDeleteSelected);
  acEditBand.Caption := cxGetResourceString(@scxgwCommonEdit);
  acMoveDown.Caption := cxGetResourceString(@scxgwCommonMoveDown);
  acMoveUp.Caption := cxGetResourceString(@scxgwCommonMoveUp);
end;

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.LoadSettings;
var
  I: Integer;
begin
  lbBands.Items.Clear;
  for I := 0 to Bands.Count - 1 do
    lbBands.AddItem(Bands[I].Caption, TObject(I));
  CheckBandsCount;
end;

function TcxGridWizardBandedTableViewOptionsBandsPageFrame.GetCanJumpToNextPage: Boolean;
begin
  Result := Bands.Count > 0;
end;

function TcxGridWizardBandedTableViewOptionsBandsPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwBandsPageDescription);
end;

function TcxGridWizardBandedTableViewOptionsBandsPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwBandsPageTitle);
end;

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.AddBand(const ACaption: string);
begin
  Bands.Add.Caption := ACaption;
end;

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.CheckBandsCount;
begin
  UpdateOwnerButtonsState;
  acDeleteAllBands.Enabled := Bands.Count > 0;
  acDeleteSelectedBands.Enabled := lbBands.SelCount > 0;
  acEditBand.Enabled := lbBands.SelCount = 1;
  acMoveDown.Enabled := (lbBands.SelCount > 0) and not lbBands.Selected[lbBands.Count - 1];
  acMoveUp.Enabled := (lbBands.SelCount > 0) and not lbBands.Selected[0];
end;

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.DeleteBand(const AIndex: Integer);
var
  ABand: TcxGridBand;
  I: Integer;
begin
  ABand := Bands[AIndex];
  for I := 0 to ABand.ColumnCount - 1 do
    ABand.Columns[I].Visible := False;
  Bands.Delete(AIndex);
end;

function TcxGridWizardBandedTableViewOptionsBandsPageFrame.GetBandedTableViewHelper: IcxGridWizardBandedTableView;
begin
  Result := inherited Helper as IcxGridWizardBandedTableView;
end;

function TcxGridWizardBandedTableViewOptionsBandsPageFrame.GetBands: TcxGridBands;
begin
  Result := BandedTableViewHelper.GetBands;
end;

function TcxGridWizardBandedTableViewOptionsBandsPageFrame.GetDefaultBandCaption: string;
begin
  Result := 'Band';
end;

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.ReorderGridViewBands;
var
  I, AOldIndex: Integer;
begin
  for I := 0 to lbBands.Count - 1 do
  begin
    AOldIndex := Integer(lbBands.Items.Objects[I]);
    if I < AOldIndex then
      Bands[AOldIndex].Index := I;
  end;
  for I := 0 to lbBands.Count - 1 do
    lbBands.Items.Objects[I] := TObject(I);
end;

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.SwapBands(const AIndex1, AIndex2: Integer);
var
  ATempSelected: Boolean;
begin
  ATempSelected := lbBands.Selected[AIndex1];
  lbBands.Items.Exchange(AIndex1, AIndex2);
  lbBands.Selected[AIndex1] := lbBands.Selected[AIndex2];
  lbBands.Selected[AIndex2] := ATempSelected;
end;

{ Events }

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.acAddBandExecute(Sender: TObject);
const
  FormatStr = '%s %d';
var
  ABandCaption: string;
begin
  ABandCaption := Format(FormatStr, [GetDefaultBandCaption, lbBands.Count + 1]);
  if cxgwExecuteEditItemDialog(Self, cxGetResourceString(@scxgwBandsPageInputQueryCaptionAdd), ABandCaption) then
  begin
    AddBand(ABandCaption);
    LoadSettings;
  end;
end;

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.acEditBandExecute(Sender: TObject);
var
  ANewCaption: string;
begin
  ANewCaption := lbBands.Items.Strings[lbBands.ItemIndex];
  if cxgwExecuteEditItemDialog(Self, cxGetResourceString(@scxgwBandsPageInputQueryCaptionEdit), ANewCaption) then
  begin
    Bands[lbBands.ItemIndex].Caption := ANewCaption;
    LoadSettings;
  end;
end;

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.acDeleteAllBandsExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := lbBands.Count - 1 downto 0 do
    DeleteBand(I);
  LoadSettings;
end;

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.acDeleteSelectedBandsExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := lbBands.Count - 1 downto 0 do
    if lbBands.Selected[I] then
      DeleteBand(I);
  LoadSettings;
end;

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.acMoveDownExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := lbBands.Count - 2 downto 0 do
    if lbBands.Selected[I] then
      SwapBands(I, I + 1);
  ReorderGridViewBands;
  CheckBandsCount;
end;

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.acMoveUpExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 1 to lbBands.Count - 1 do
    if lbBands.Selected[I] then
      SwapBands(I, I - 1);
  ReorderGridViewBands;
  CheckBandsCount;
end;

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.lbBandsClick(Sender: TObject);
begin
  CheckBandsCount;
end;

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.lbBandsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TcxGridWizardBandedTableViewOptionsBandsPageFrame.lbBandsEndDrag(Sender, Target: TObject; X, Y: Integer);
var
  AAboveTargetIndex: Integer;
  ACount: Integer;
  ASelCount: Integer;
  ATargetIndex: Integer;
  I: Integer;
begin
  ATargetIndex := lbBands.ItemAtPos(Point(X, Y), True);
  if ATargetIndex >= 0 then
  begin
    lbBands.Items.BeginUpdate;
    try
      ACount := lbBands.Items.Count;
      ASelCount := lbBands.SelCount;

      if Y > cxRectCenter(lbBands.ItemRect(ATargetIndex)).Y then
        Inc(ATargetIndex);

      AAboveTargetIndex := 0;
      for I := 0 to ACount - 1 do
        if lbBands.Selected[I] then
        begin
          if I < ATargetIndex then
            Inc(AAboveTargetIndex);
          lbBands.Items.AddObject(lbBands.Items[I], lbBands.Items.Objects[I]);
        end;
      Dec(ATargetIndex, AAboveTargetIndex);

      for I := ACount - 1 downto 0 do
        if lbBands.Selected[I] then
          lbBands.Items.Delete(I);

      for I := ATargetIndex to (ACount - ASelCount) - 1 do
        lbBands.Items.AddObject(lbBands.Items[I], lbBands.Items.Objects[I]);

      for I := (ACount - ASelCount) - 1 downto ATargetIndex do
        lbBands.Items.Delete(I);
    finally
      lbBands.Items.EndUpdate;
    end;
    ATargetIndex := Min(ATargetIndex, lbBands.Count - ASelCount);
    lbBands.ItemIndex := ATargetIndex;
    for I := 0 to ASelCount - 1 do
      lbBands.Selected[ATargetIndex + I] := True;

    ReorderGridViewBands;
  end;
  CheckBandsCount;
end;

end.
