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

unit cxGridWizardCardViewHelper;

{$I cxVer.inc}

interface

uses
  cxGridWizardCustomHelper, cxGridCustomView, cxGridCardView, cxGridDBCardView, cxGridLevel, SysUtils, cxGraphics,
  dxCustomWizardControl, dxWizardControl, cxGridDBDataDefinitions, cxDBData, Forms, cxEdit, ExtCtrls, cxPC, Controls,
  cxGridWizardCardStructureEditor;

type

  { TcxGridWizardCustomCardViewHelper }

  TcxGridWizardCustomCardViewHelper = class(TcxGridWizardCustomTableLikeViewHelper)
  private
    FSavedRowMovingOptionValue: Boolean;
    FTreeViewFrame: TcxGridWizardCardStructureEditorFrame;
    FTreeViewPanel: TPanel;

    function GetGridView: TcxGridCardView;
    function GetItem(Index: Integer): TcxGridCardViewRow;
  protected
    procedure CheckComponentNames(AView: TcxCustomGridView); override;

    property TreeViewFrame: TcxGridWizardCardStructureEditorFrame read FTreeViewFrame;
    property TreeViewPanel: TPanel read FTreeViewPanel;
  public
    procedure CorrectCustomizationFormContent(ACustomizationForm: TForm); override;
    function GetDefaultItemCaption: string; override;

    procedure PrepareForCustomization; override;
    procedure RestoreAfterCustomization; override;
    procedure RestoreGridViewOptionsData; override;
    procedure SaveGridViewOptionsData; override;

    property Items[Index: Integer]: TcxGridCardViewRow read GetItem;
    property GridView: TcxGridCardView read GetGridView;
  end;

  { TcxGridWizardCardViewHelper }

  TcxGridWizardCardViewHelper = class(TcxGridWizardCustomCardViewHelper)
  private
    FSavedNavigatorVisibleOptionValue: Boolean;
  protected
    function CanSaveData: Boolean; override;
    function GetPageClassCount: Integer; override;
    function GetPageClasses(Index: Integer): TClass; override;
  public
    class function GetGridViewClass: TcxCustomGridViewClass; override;

    procedure PrepareForCustomization; override;
    procedure RestoreAfterCustomization; override;
  end;

  { TcxGridWizardDBCardViewHelper }

  TcxGridWizardDBCardViewHelper = class(TcxGridWizardCustomCardViewHelper, IcxGridWizardHelperDBDataControllerSupport)
  private
    function GetGridView: TcxGridDBCardView;
    function GetItem(Index: Integer): TcxGridDBCardViewRow;
  protected
    function GetItemCaption(Index: Integer): string; override;
    function GetItemFieldName(Index: Integer): string; override;
    function GetPageClassCount: Integer; override;
    function GetPageClasses(Index: Integer): TClass; override;
    procedure SetItemFieldName(Index: Integer; const AValue: string); override;
    // IcxGridWizardHelperDBDataControllerSupport
    function GetDataController: TcxDBDataController;
  public
    procedure DeleteItem(const AFieldName: string); overload; override;
    function GetItemIndexByFieldName(const AFieldName: string): Integer; override;
    class function GetGridViewClass: TcxCustomGridViewClass; override;
    //
    property GridView: TcxGridDBCardView read GetGridView;
    property Items[Index: Integer]: TcxGridDBCardViewRow read GetItem;
  end;

  TcxGridCardViewCustomizationFormAccess = class(TcxGridCardViewCustomizationForm);

implementation

uses
  cxClasses, cxGridWizardCustomPage, cxGridWizardDBViewsDataSourcePage, cxGridWizardDBViewsSelectItemsForDisplayPage,
  cxGridWizardCommonCustomizeItemsPage, cxGridWizardDBViewsDataLoadingSettingsPage, cxGridWizardCardViewOptionsInterfaceElementsPage,
  cxGridWizardCommonOptionsFilteringSortingPage, cxGridWizardCardViewOptionsBehaviorPage,
  cxGridWizardCardViewOptionsSizingPage, cxGridWizardUnboundViewsSelectItemsForDisplayPage, cxGridCommon, dxCore,
  cxGridWizardStrs, cxGrid;

const
  cxGridWizardCardViewPages: array [0..5] of TcxGridWizardCustomPageFrameClass = (
    TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame, TcxGridWizardCommonCustomizeItemsPageFrame,
    TcxGridWizardCardViewOptionsInterfaceElementsPageFrame,
    TcxGridWizardCommonOptionsFilteringSortingPageFrame, TcxGridWizardCardViewOptionsBehaviorPageFrame,
    TcxGridWizardCardViewOptionsSizingPageFrame);

  cxGridWizardDBCardViewPages: array [0..7] of TcxGridWizardCustomPageFrameClass = (
    TcxGridWizardDBViewsDataSourcePageFrame, TcxGridWizardDBViewsSelectItemsForDisplayPageFrame,
    TcxGridWizardCommonCustomizeItemsPageFrame,
    TcxGridWizardDBViewsDataLoadingSettingsPageFrame, TcxGridWizardCardViewOptionsInterfaceElementsPageFrame,
    TcxGridWizardCommonOptionsFilteringSortingPageFrame, TcxGridWizardCardViewOptionsBehaviorPageFrame,
    TcxGridWizardCardViewOptionsSizingPageFrame);

{$R cxGridWizardCardViewHelper.res}

{ TcxGridWizardCustomCardViewHelper }

procedure TcxGridWizardCustomCardViewHelper.CorrectCustomizationFormContent(ACustomizationForm: TForm);
var
  ATreeViewPage: TcxTabSheet;
  ACustomizationFormAccess: TcxGridCardViewCustomizationFormAccess;
begin
  inherited CorrectCustomizationFormContent(ACustomizationForm);

  ACustomizationFormAccess := TcxGridCardViewCustomizationFormAccess(ACustomizationForm);

  ACustomizationFormAccess.ItemsPage.Caption := cxGetResourceString(@scxgwCustomizationFormRowsTab);

  ATreeViewPage := ACustomizationFormAccess.CreatePage(cxGetResourceString(@scxgwCustomizationFormCardTreeViewTab), True);

  FTreeViewPanel := TPanel.Create(ATreeViewPage);
  TreeViewPanel.Align := alClient;
  TreeViewPanel.BevelOuter := bvNone;
  TreeViewPanel.Parent := ATreeViewPage;

  FTreeViewFrame := TcxGridWizardCardStructureEditorFrame.Create(TreeViewPanel);
  TreeViewFrame.Align := alClient;
  TreeViewFrame.Parent := TreeViewPanel;
  TreeViewFrame.LoadSettings(TcxCustomGrid(ACustomizationFormAccess.Controller.GridView.Control));
end;

function TcxGridWizardCustomCardViewHelper.GetDefaultItemCaption: string;
begin
  Result := 'Row';
end;

procedure TcxGridWizardCustomCardViewHelper.PrepareForCustomization;
begin
  FSavedRowMovingOptionValue := GridView.OptionsCustomize.RowMoving;
  GridView.OptionsCustomize.RowMoving := True;
end;

procedure TcxGridWizardCustomCardViewHelper.RestoreAfterCustomization;
begin
  GridView.OptionsCustomize.RowMoving := FSavedRowMovingOptionValue;
end;

procedure TcxGridWizardCustomCardViewHelper.RestoreGridViewOptionsData;
begin
  GridView.OptionsData.Appending := FOriginalGridViewOptionsData.Appending;
  GridView.OptionsData.Deleting := FOriginalGridViewOptionsData.Deleting;
  GridView.OptionsData.Editing := FOriginalGridViewOptionsData.Editing;
  GridView.OptionsData.Inserting := FOriginalGridViewOptionsData.Inserting;
end;

procedure TcxGridWizardCustomCardViewHelper.SaveGridViewOptionsData;
begin
  FOriginalGridViewOptionsData.Appending := GridView.OptionsData.Appending;
  FOriginalGridViewOptionsData.Deleting := GridView.OptionsData.Deleting;
  FOriginalGridViewOptionsData.Editing := GridView.OptionsData.Editing;
  FOriginalGridViewOptionsData.Inserting := GridView.OptionsData.Inserting;
  GridView.OptionsData.Appending := False;
  GridView.OptionsData.Deleting := False;
  GridView.OptionsData.Editing := False;
  GridView.OptionsData.Inserting := False;
end;

procedure TcxGridWizardCustomCardViewHelper.CheckComponentNames(AView: TcxCustomGridView);
var
  ACardView: TcxGridCardView;
  I: Integer;
begin
  ACardView := AView as TcxGridCardView;
  for I := 0 to ACardView.RowCount - 1 do
    ACardView.Rows[I].Name := CreateUniqueName(GetParentForm(ACardView.Control),
      ACardView, ACardView.Rows[I], ScxGridPrefixName, '');
end;

function TcxGridWizardCustomCardViewHelper.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridWizardCustomCardViewHelper.GetItem(Index: Integer): TcxGridCardViewRow;
begin
  Result := GridView.Rows[Index];
end;

{ TcxGridWizardCardViewHelper }

class function TcxGridWizardCardViewHelper.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridCardView;
end;

procedure TcxGridWizardCardViewHelper.PrepareForCustomization;
begin
  inherited PrepareForCustomization;
  FSavedNavigatorVisibleOptionValue := GridView.Navigator.Visible;
  GridView.Navigator.Visible := True;
  GridView.OptionsData.Appending := True;
  GridView.OptionsData.Deleting := True;
  GridView.OptionsData.Editing := True;
  GridView.OptionsData.Inserting := True;
end;

procedure TcxGridWizardCardViewHelper.RestoreAfterCustomization;
begin
  inherited RestoreAfterCustomization;
  GridView.Navigator.Visible := FSavedNavigatorVisibleOptionValue;
  GridView.OptionsData.Appending := False;
  GridView.OptionsData.Deleting := False;
  GridView.OptionsData.Editing := False;
  GridView.OptionsData.Inserting := False;
end;

function TcxGridWizardCardViewHelper.CanSaveData: Boolean;
begin
  Result := True;
end;

function TcxGridWizardCardViewHelper.GetPageClassCount: Integer;
begin
  Result := Length(cxGridWizardCardViewPages);
end;

function TcxGridWizardCardViewHelper.GetPageClasses(Index: Integer): TClass;
begin
  Result := cxGridWizardCardViewPages[Index];
end;

{ TcxGridWizardDBCardViewHelper }

procedure TcxGridWizardDBCardViewHelper.DeleteItem(const AFieldName: string);
begin
  GridView.GetRowByFieldName(AFieldName).Free;
end;

function TcxGridWizardDBCardViewHelper.GetDataController: TcxDBDataController;
begin
  Result := GridView.DataController;
end;

function TcxGridWizardDBCardViewHelper.GetItemIndexByFieldName(const AFieldName: string): Integer;
var
  ARow: TcxGridDBCardViewRow;
begin
  ARow := GridView.GetRowByFieldName(AFieldName);
  if ARow <> nil then
    Result := ARow.Index
  else
    Result := -1;
end;

class function TcxGridWizardDBCardViewHelper.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridDBCardView;
end;

function TcxGridWizardDBCardViewHelper.GetItem(Index: Integer): TcxGridDBCardViewRow;
begin
  Result := GridView.Rows[Index];
end;

function TcxGridWizardDBCardViewHelper.GetItemCaption(Index: Integer): string;
begin
  Result := inherited GetItemCaption(Index);
  if (Result = '') and (ItemFieldName[Index] <> '') then
    Result := ItemFieldName[Index];
end;

function TcxGridWizardDBCardViewHelper.GetItemFieldName(Index: Integer): string;
begin
  Result := Items[Index].DataBinding.FieldName;
end;

function TcxGridWizardDBCardViewHelper.GetPageClassCount: Integer;
begin
  Result := Length(cxGridWizardDBCardViewPages);
end;

function TcxGridWizardDBCardViewHelper.GetPageClasses(Index: Integer): TClass;
begin
  Result := cxGridWizardDBCardViewPages[Index]
end;

procedure TcxGridWizardDBCardViewHelper.SetItemFieldName(Index: Integer; const AValue: string);
begin
  Items[Index].DataBinding.FieldName := AValue;
end;

function TcxGridWizardDBCardViewHelper.GetGridView: TcxGridDBCardView;
begin
  Result := TcxGridDBCardView(inherited GridView);
end;

initialization
  cxGridWizardHelperInfoList.Add(TcxGridWizardCardViewHelper, HInstance);
  cxGridWizardHelperInfoList.Add(TcxGridWizardDBCardViewHelper, HInstance);
end.

