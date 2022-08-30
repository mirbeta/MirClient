{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSfmCompositionAdd;

interface

{$I cxVer.inc}

uses
  Windows, Classes, Messages, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, IniFiles,
  dxCore, dxPSForm, dxPSCore, cxControls, cxContainer, cxListView, cxEdit,
  cxCheckBox, Menus, cxLookAndFeelPainters, cxButtons, cxPC, cxLabel,
  cxGraphics, cxLookAndFeels, cxGroupBox, cxClasses, dxLayoutContainer, dxLayoutControl,
  dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutLookAndFeels;

type
  TdxCompositionAddItemsDlgOption = (caiShowDescription);
  TdxCompositionAddItemsDlgOptions = set of TdxCompositionAddItemsDlgOption;

  TdxfmCompositionAddItems = class(TCustomdxPSForm)
    btnOK: TcxButton;
    btnCancel: TcxButton;
    btnHelp: TcxButton;
    chbxHideIncludedItems: TcxCheckBox;
    lvItems: TcxListView;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutGroup1: TdxLayoutGroup;
    tshItems: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    lbbtnHelp: TdxLayoutItem;
    pnlNoItems: TcxLabel;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    procedure chbxHideIncludedItemsClick(Sender: TObject);
    procedure lvItemsDblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FComposition: TdxCompositionReportLink;
    FOptions: TdxCompositionAddItemsDlgOptions;
    FNoItemsState: Boolean;
    FSavedHideIncludedItems: Boolean;

    function GetActiveItem: TBasedxReportLink;
    function GetItem(Index: Integer): TBasedxReportLink;
    function GetItemCount: Integer;
    function GetSelected(Index: Integer): Boolean;
    function GetSelectedCount: Integer;
    procedure SetSelected(Index: Integer; Value: Boolean);

    procedure GetSelection(AList: TList);
    procedure Initialize;
    procedure LoadStrings;
    procedure RefreshColumns;
    procedure RefreshList;
    procedure UpdateNoItems;

    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure BeforeConstruction; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;

    property ActiveItem: TBasedxReportLink read GetActiveItem;
    property Composition: TdxCompositionReportLink read FComposition write FComposition;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TBasedxReportLink read GetItem;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property SelectedCount: Integer read GetSelectedCount;
  end;

  PdxAddItemsToCompositionDlgData = ^TdxAddItemsToCompositionDlgData;
  TdxAddItemsToCompositionDlgData = record
    Composition: TdxCompositionReportLink;
    Items: TList;
    Options: TdxCompositionAddItemsDlgOptions;
  end;

function dxShowAddItemsToCompositionDlg(var AData: TdxAddItemsToCompositionDlgData): Boolean;

implementation

{$R *.DFM}

uses
  Graphics, Registry, Math, dxPSGlbl, dxPSUtl, dxPSRes, dxPSPopupMan;

const
  sdxHideIncludedItems = 'HideIncludedItems'; // Don't Localize

function dxShowAddItemsToCompositionDlg(var AData: TdxAddItemsToCompositionDlgData): Boolean;
var
  Dialog: TdxfmCompositionAddItems;
begin
  Result := False;
  if (AData.Composition = nil) or (AData.Items = nil) then Exit;

  Dialog := TdxfmCompositionAddItems.Create(nil);
  try
    Dialog.Composition := AData.Composition;
    Dialog.FOptions := AData.Options;
    Result := Dialog.Execute;
    if Result then
      Dialog.GetSelection(AData.Items);
  finally
    Dialog.Free;
  end;
end;

{ TdxfmCompositionAddItems }

procedure TdxfmCompositionAddItems.chbxHideIncludedItemsClick(Sender: TObject);
begin
  RefreshList;
end;

procedure TdxfmCompositionAddItems.lvItemsDblClick(Sender: TObject);
begin
  if SelectedCount = 1 then ModalResult := mrOK;
end;

procedure TdxfmCompositionAddItems.FormResize(Sender: TObject);
begin
  UpdateNoItems;
end;

constructor TdxfmCompositionAddItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HelpContext := dxPSGlbl.dxhcAddItemsToCompositionDlg;
  RefreshColumns;
  dxPSPopupMenuController.RegisterControl(lvItems);
end;

destructor TdxfmCompositionAddItems.Destroy;
begin
  dxPSPopupMenuController.UnregisterControl(lvItems);
  inherited Destroy;
end;

function TdxfmCompositionAddItems.Execute: Boolean;
begin
  Initialize;
  Result := (ShowModal = mrOk) and (SelectedCount <> 0);
end;

procedure TdxfmCompositionAddItems.LoadFromIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited LoadFromIniFile(AIniFile, ASectionName);
  dxLoadListViewColumns(AIniFile, ASectionName, lvItems.InnerListView);
  FSavedHideIncludedItems := AIniFile.ReadBool(
    ASectionName, sdxHideIncludedItems, FSavedHideIncludedItems);
end;

procedure TdxfmCompositionAddItems.SaveToIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited SaveToIniFile(AIniFile, ASectionName);
  dxSaveListViewColumns(AIniFile, ASectionName, lvItems.InnerListView);
  AIniFile.WriteBool(ASectionName, sdxHideIncludedItems, chbxHideIncludedItems.Checked);
end;

procedure TdxfmCompositionAddItems.BeforeConstruction;
begin
  inherited BeforeConstruction;
  Options := Options + [foSizeableDialog];
end;

function TdxfmCompositionAddItems.GetActiveItem: TBasedxReportLink;
begin
  if SelectedCount = 1 then
    Result := TBasedxReportLink(lvItems.Selected.Data)
  else
    Result := nil;
end;

function TdxfmCompositionAddItems.GetItem(Index: Integer): TBasedxReportLink;
begin
  Result := TBasedxReportLink(lvItems.Items[Index].Data);
end;

function TdxfmCompositionAddItems.GetItemCount: Integer;
begin
  Result := lvItems.Items.Count;
end;

function TdxfmCompositionAddItems.GetSelected(Index: Integer): Boolean;
begin
  Result := lvItems.Items[Index].Selected;
end;

function TdxfmCompositionAddItems.GetSelectedCount: Integer;
begin
  Result := lvItems.SelCount;
end;

procedure TdxfmCompositionAddItems.SetSelected(Index: Integer; Value: Boolean);
begin
  lvItems.Items[Index].Selected := Value;
end;

procedure TdxfmCompositionAddItems.GetSelection(AList: TList);
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    if Selected[I] then AList.Add(Items[I]);
end;

procedure TdxfmCompositionAddItems.Initialize;
begin
  CheckDialogFormHelpContext(Self, lbbtnHelp);

  pnlNoItems.Parent := lvItems;
  chbxHideIncludedItems.Checked := FSavedHideIncludedItems;
  LoadStrings;
  RefreshColumns;
  RefreshList;

  FormResize(nil);
end;

procedure TdxfmCompositionAddItems.LoadStrings;
begin
  Caption := cxGetResourceString(@sdxAddItemsToComposition);
  btnOK.Caption := cxGetResourceString(@sdxBtnOK);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnHelp.Caption := cxGetResourceString(@sdxBtnHelp);
  tshItems.Caption := cxGetResourceString(@sdxAvailableItems);
  pnlNoItems.Caption := cxGetResourceString(@sdxThereAreNowItemsForShow);
  chbxHideIncludedItems.Caption := cxGetResourceString(@sdxHideAlreadyIncludedItems);
end;

procedure TdxfmCompositionAddItems.RefreshColumns;
var
  ColumnWidths: array of Integer;
  I: Integer;
begin
  lvItems.Columns.BeginUpdate;
  try
    SetLength(ColumnWidths, lvItems.Columns.Count);
    for I := 0 to lvItems.Columns.Count - 1 do
      ColumnWidths[I] := lvItems.Columns[I].Width;

    lvItems.Columns.Clear;
    with lvItems.Columns.Add do
    begin
      Width := 2 * (lvItems.Width - GetSystemMetrics(SM_CXHSCROLL) - 2) div 3;
      Caption := cxGetResourceString(@sdxItemName);
    end;

    if caiShowDescription in FOptions then
      with lvItems.Columns.Add do
      begin
        Width := (lvItems.Width - GetSystemMetrics(SM_CXHSCROLL) - 2) div 3;
        Caption := cxGetResourceString(@sdxItemDescription);
      end;

    for I := 0 to Min(lvItems.Columns.Count - 1, Length(ColumnWidths) - 1) do
      lvItems.Columns[I].Width := ColumnWidths[I];
  finally
    lvItems.Columns.EndUpdate;
  end;
end;

procedure TdxfmCompositionAddItems.RefreshList;
const
  Styles: array[Boolean] of TViewStyle = (vsReport, vsList);
var
  Selection: TList;
  Strings: TStrings;
  I, J: Integer;
begin
  Selection := TList.Create;
  try
    GetSelection(Selection);
    lvItems.Items.BeginUpdate;
    try
      Strings := TStringList.Create;
      try
        lvItems.Items.Clear;
        if (Composition <> nil) and (Composition.ComponentPrinter <> nil) then
          Composition.ComponentPrinter.GetItems(Composition, Strings, chbxHideIncludedItems.Checked);
        for I := 0 to Strings.Count - 1 do
          with lvItems.Items.Add do
          begin
            Caption := Strings[I];
            Data := Strings.Objects[I];
            SubItems.Add(TBasedxReportLink(Data).Description);
          end;

        for I := 0 to Selection.Count - 1 do
          for J := 0 to lvItems.Items.Count - 1 do
            if lvItems.Items[J].Data = Selection[I] then
            begin
              lvItems.Items[J].Selected := True;
              Break;
            end;
      finally
        Strings.Free;
      end;

      FNoItemsState := lvItems.Items.Count = 0;
      lvItems.ViewStyle := Styles[FNoItemsState];
      lvItems.Enabled := not FNoItemsState;
      lvItems.HideSelection := FNoItemsState;
      if (lvItems.SelCount = 0) and (lvItems.Items.Count <> 0) then
        lvItems.Selected := lvItems.Items[0];
    finally
      lvItems.Items.EndUpdate;
    end;
  finally
    Selection.Free;
  end;
  UpdateNoItems;
end;

procedure TdxfmCompositionAddItems.UpdateNoItems;
begin
  with pnlNoItems do
  begin
    SetBounds(3, Height, lvItems.ClientWidth - 2 * 3, Height);
    Visible := FNoItemsState;
  end;
end;

procedure TdxfmCompositionAddItems.CMDialogChar(var Msg: TCMDialogChar);
begin
  inherited;
  if IsAccel(Msg.CharCode, tshItems.Caption) then
  begin
    Msg.Result := 1;
    if lvItems.CanFocus then
      ActiveControl := lvItems;
  end;
end;

end.
